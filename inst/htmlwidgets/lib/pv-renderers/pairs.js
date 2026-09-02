/* The scatterplot-matrix renderer: a grid of small panels - scatters
   below the diagonal, histograms on it, correlation values above -
   drawn directly into one svg. Every statistic (bins, coefficients,
   pairwise counts) arrives pre-computed from R; the cells are simple
   enough that drawing them here beats routing through the other
   renderers the way the facet grid does. */
(function () {

  pvRenderers.pairs = function (ctx) {
    var vars = ctx.x.variables || [];
    var pts = ctx.x.data || [];
    var corr = ctx.x.cor || [];
    var nMat = ctx.x.n || [];
    var nv = vars.length;
    if (nv < 2) return;

    var hasSeries = pts.length && pts[0].series !== undefined;
    var seriesNames = hasSeries ?
      pv.uniq(pts.map(function (d) { return d.series; })) : [];
    var color = d3.scaleOrdinal().domain(seriesNames)
      .range(ctx.theme.palette);
    if (hasSeries) {
      pv.buildLegend(ctx.header, seriesNames, color, ctx.theme);
      ctx.height = Math.max(120, ctx.height - 26);
    }

    /* The correlation method's name appears exactly once, as a quiet
       line in the header - never per cell, where it would repeat
       n-squared times. */
    if (ctx.x.methodLabel) {
      var note = document.createElement("div");
      note.textContent = ctx.x.methodLabel;
      note.style.cssText = "font-size:11px;margin-top:3px;color:" +
        ctx.theme.ink.muted + ";";
      ctx.header.appendChild(note);
      ctx.height = Math.max(120, ctx.height - note.offsetHeight - 3);
    }

    /* Outer margins hold the only axis ticks the matrix has - tiny 9px
       ones on the left column and the bottom row. Inner cells carry no
       ticks at all; their hairline frames do the organising. */
    var m = { top: 6, right: 10, bottom: 26, left: 40 };
    var iw = ctx.width - m.left - m.right,
        ih = ctx.height - m.top - m.bottom;
    var cw = iw / nv, chh = ih / nv;
    var cellMin = Math.min(cw, chh);
    var pad = 3;      /* breathing room inside each cell */
    var nameH = 15;   /* strip a diagonal cell reserves for its name */

    var svg = pv.baseSvg(ctx);
    var g = svg.append("g").attr("transform",
      "translate(" + m.left + "," + m.top + ")");

    /* One scale per variable per direction. The domains are the
       R-computed bin edges, which cover the data - so the diagonal
       histogram, the scatters in that variable's row and column, and
       the outer ticks all agree on where a value sits. */
    var xs = vars.map(function (v) {
      return d3.scaleLinear().domain(v.lim).range([pad, cw - pad]);
    });
    var ys = vars.map(function (v) {
      return d3.scaleLinear().domain(v.lim).range([chh - pad, pad]);
    });

    /* The scatter chart's crowding rule, rescaled to a small cell:
       full ink up to 80 points, then fading with the square root of
       the count, floored so even a dense cell keeps a visible cloud. */
    function dotOpacity(n) {
      return n <= 80 ? 0.62 : Math.max(0.16, 0.62 * Math.sqrt(80 / n));
    }
    var rPt = cellMin < 90 ? 1.4 : cellMin < 150 ? 1.8 : 2.3;

    /* Tiny outer ticks: two per cell, three when the cells are roomy. */
    function tickCount(px) { return px >= 110 ? 3 : 2; }
    function tinyAxis(sel) {
      pv.styleAxis(sel, ctx.theme, false);
      sel.selectAll("text").style("font-size", "9px");
    }
    /* A tick landing at a cell's edge would print half of its label
       inside the neighbouring cell and collide with that cell's own
       edge tick ("300" meeting "0" as "3000"). Edge labels therefore
       anchor inward - along the axis for bottom ticks, up or down a
       shade for left ones - so every label stays inside its own cell. */
    function insetTicksX(sel, scale, size) {
      sel.selectAll(".tick text").each(function (d) {
        var p = scale(d);
        if (p < 12) {
          d3.select(this).attr("text-anchor", "start").attr("x", 0);
        } else if (p > size - 12) {
          d3.select(this).attr("text-anchor", "end").attr("x", 0);
        }
      });
    }
    function insetTicksY(sel, scale, size) {
      sel.selectAll(".tick text").each(function (d) {
        var p = scale(d);
        if (p < 8) {
          d3.select(this).attr("dy", "0.85em");
        } else if (p > size - 8) {
          d3.select(this).attr("dy", "0em");
        }
      });
    }

    /* Left-edge ticks label each row's y variable. Row 0 is skipped on
       purpose: its leftmost cell is the diagonal histogram, whose
       vertical extent is a count - a value tick there would lie. */
    for (var ri = 1; ri < nv; ri++) {
      g.append("g")
        .attr("transform", "translate(0," + (ri * chh) + ")")
        .call(d3.axisLeft(ys[ri]).ticks(tickCount(chh))
          .tickFormat(pv.fmtTick).tickSize(3).tickPadding(2))
        .call(tinyAxis)
        .call(function (s) { insetTicksY(s, ys[ri], chh); });
    }
    /* Bottom-edge ticks label each column's x variable - the scatters
       above, and in the last column the diagonal histogram itself. */
    for (var ci = 0; ci < nv; ci++) {
      g.append("g")
        .attr("transform", "translate(" + (ci * cw) + "," + ih + ")")
        .call(d3.axisBottom(xs[ci]).ticks(tickCount(cw))
          .tickFormat(pv.fmtTick).tickSize(3).tickPadding(2))
        .call(tinyAxis)
        .call(function (s) { insetTicksX(s, xs[ci], cw); });
    }

    var fmt2 = function (v) {
      return (v < 0 ? "−" : "") + Math.abs(v).toFixed(2);
    };
    var fmt3 = function (v) {
      return (v < 0 ? "−" : "") + Math.abs(v).toFixed(3);
    };

    vars.forEach(function (rowVar, i) {
      vars.forEach(function (colVar, j) {
        var cell = g.append("g").attr("transform",
          "translate(" + (j * cw) + "," + (i * chh) + ")");
        /* The hairline frame every cell wears - the grid's only
           chrome. */
        cell.append("rect")
          .attr("x", 0).attr("y", 0)
          .attr("width", cw).attr("height", chh)
          .attr("fill", "none")
          .attr("stroke", ctx.theme.ink.grid);
        var delay = Math.min((i + j) * 40, 300);

        if (i > j) {
          /* Below the diagonal: the scatter of (column j, row i). Each
             cell keeps every row complete for its own pair - the
             pairwise rule the R side promised. */
          var xk = "p" + (j + 1), yk = "p" + (i + 1);
          var mine = pts.filter(function (d) {
            return d[xk] != null && d[yk] != null;
          });
          var op = dotOpacity(mine.length);
          var dots = cell.selectAll("circle").data(mine).enter()
            .append("circle")
            .attr("cx", function (d) { return xs[j](d[xk]); })
            .attr("cy", function (d) { return ys[i](d[yk]); })
            .attr("r", 0)
            .attr("fill", function (d) {
              return hasSeries ? color(d.series) : ctx.theme.palette[0];
            })
            .attr("fill-opacity", op);
          dots.transition().duration(ctx.duration).delay(delay)
            .attr("r", rPt);
          dots
            .on("pointerenter pointermove", function (event, d) {
              d3.select(this).attr("fill-opacity", 1).attr("r", rPt * 1.8);
              var rows = [];
              if (d.label != null) {
                rows.push("<b>" + pv.esc(d.label) + "</b>");
              }
              if (hasSeries) {
                rows.push(pv.swatchRow(color(d.series), "group",
                  pv.esc(d.series)));
              }
              rows.push(pv.esc(colVar.name) + ": <b>" +
                ctx.fmt(d[xk]) + "</b>");
              rows.push(pv.esc(rowVar.name) + ": <b>" +
                ctx.fmt(d[yk]) + "</b>");
              pv.showTip(ctx, event, rows.join("<br>"));
            })
            .on("pointerleave", function () {
              d3.select(this).attr("fill-opacity", op).attr("r", rPt);
              pv.hideTip(ctx);
            })
            .on("click", function (event, d) {
              ctx.emit("click", {
                x: colVar.name, y: rowVar.name,
                xvalue: d[xk], yvalue: d[yk],
                label: d.label != null ? d.label : null
              });
            });

        } else if (i === j) {
          /* The diagonal: the variable's own histogram, in neutral ink
             rather than a palette hue - colour here would falsely read
             as a group - with the variable's name in the strip above
             the bars, playing the role a facet panel's name does. */
          var bins = rowVar.bins || [];
          var hy = d3.scaleLinear()
            .domain([0, d3.max(bins, function (b) { return b.count; }) || 1])
            .range([chh - pad, nameH + 3]);
          var hx = xs[i];
          var bars = cell.selectAll("rect.bin").data(bins).enter()
            .append("rect").attr("class", "bin")
            .attr("x", function (b) { return hx(b.x0) + 0.5; })
            .attr("width", function (b) {
              return Math.max(0.5, hx(b.x1) - hx(b.x0) - 1);
            })
            .attr("y", chh - pad).attr("height", 0)
            .attr("fill", ctx.theme.ink.muted)
            .attr("fill-opacity", 0.45);
          bars.transition().duration(ctx.duration).delay(delay)
            .attr("y", function (b) { return hy(b.count); })
            .attr("height", function (b) {
              return (chh - pad) - hy(b.count);
            });
          bars
            .on("pointerenter pointermove", function (event, b) {
              d3.select(this).attr("fill-opacity", 0.8);
              pv.showTip(ctx, event,
                "<b>" + pv.esc(rowVar.name) + "</b><br>" +
                ctx.fmt(b.x0) + " – " + ctx.fmt(b.x1) + "<br>" +
                "count: <b>" + ctx.fmt(b.count) + "</b>");
            })
            .on("pointerleave", function () {
              d3.select(this).attr("fill-opacity", 0.45);
              pv.hideTip(ctx);
            });
          /* Long names truncate to the cell; the svg title and the bin
             tooltip keep the full text a hover away. */
          var nameChars = Math.max(3, Math.floor((cw - 8) / (12 * 0.62)));
          cell.append("text")
            .attr("x", cw / 2).attr("y", 12)
            .attr("text-anchor", "middle")
            .attr("fill", ctx.theme.ink.secondary)
            .attr("stroke", ctx.theme.ink.surface)
            .attr("stroke-width", 3)
            .style("paint-order", "stroke")
            .style("font-size", "12px")
            .style("font-weight", "700")
            .text(pv.truncate(rowVar.name, nameChars))
            .append("title").text(rowVar.name);

        } else {
          /* Above the diagonal: the pair's correlation, the number set
             large. Its ink slides from the muted grey at zero to the
             diverging ramp's pole at one - blue negative, red positive,
             the same poles the heatmap uses - so sign and strength read
             at a glance. The whole ramp passes large-text contrast on
             both surfaces (measured floor 3.4:1, at the grey end on
             paper; every actual colour sits above 4:1). The background
             takes only a whisper of the pole colour; the number is the
             mark here, not the cell. */
          var r = corr[i] ? corr[i][j] : null;
          var np = nMat[i] ? nMat[i][j] : null;
          var has = r != null && isFinite(r);
          var pole = has && r < 0 ?
            ctx.theme.diverging.low : ctx.theme.diverging.high;
          if (has && Math.abs(r) > 0.005) {
            cell.append("rect")
              .attr("x", 0.5).attr("y", 0.5)
              .attr("width", cw - 1).attr("height", chh - 1)
              .attr("fill", pole)
              .attr("fill-opacity", 0.08 * Math.abs(r));
          }
          var fs = Math.max(11, Math.min(24, Math.round(cellMin * 0.28)));
          var inkC = has ?
            d3.interpolateRgb(ctx.theme.ink.muted, pole)(Math.abs(r)) :
            ctx.theme.ink.muted;
          var num = cell.append("text")
            .attr("x", cw / 2).attr("y", chh / 2)
            .attr("text-anchor", "middle")
            .attr("dominant-baseline", "central")
            .attr("fill", inkC)
            .style("font-size", fs + "px")
            .style("font-weight", "600")
            .style("font-variant-numeric", "tabular-nums")
            .text(has ? fmt2(r) : "–");
          if (ctx.duration > 0) {
            num.attr("opacity", 0)
              .transition().duration(ctx.duration).delay(delay)
              .attr("opacity", 1);
          }
          /* The whole cell is the hover target; the tooltip restates
             the coefficient precisely, with the pairs behind it. */
          cell.append("rect")
            .attr("width", cw).attr("height", chh)
            .attr("fill", "transparent")
            .on("pointerenter pointermove", function (event) {
              var rows = ["<b>" + pv.esc(rowVar.name) + " × " +
                pv.esc(colVar.name) + "</b>"];
              rows.push(pv.esc(ctx.x.methodLabel || "correlation") +
                ": <b>" + (has ? fmt3(r) : "not defined") + "</b>");
              if (np != null) {
                rows.push("n = <b>" + ctx.fmt(np) + "</b> pairs");
              }
              pv.showTip(ctx, event, rows.join("<br>"));
            })
            .on("pointerleave", function () { pv.hideTip(ctx); })
            .on("click", function () {
              ctx.emit("click", {
                x: colVar.name, y: rowVar.name,
                r: has ? r : null, n: np
              });
            });
        }
      });
    });
  };

})();
