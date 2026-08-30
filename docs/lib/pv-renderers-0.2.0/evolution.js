/*
 * Evolution and matrix renderers: the area chart family (stacked, percent,
 * stream) and the categorical heatmap. See basic.js for the ctx contract.
 */
(function () {

  /* ---------- area ---------- */

  pvRenderers.area = function (ctx) {
    var xtype = ctx.x.xtype;
    var offset = ctx.x.offset;
    var parse = xtype === "date" ? d3.timeParse("%Y-%m-%d") : null;
    /* A single series arrives from R as a bare string, not an array. */
    var seriesNames = [].concat(ctx.x.series);
    var color = d3.scaleOrdinal().domain(seriesNames)
      .range(ctx.theme.palette);
    if (ctx.x.showLegend && seriesNames.length > 1) {
      pv.buildLegend(ctx.header, seriesNames, color, ctx.theme);
      ctx.height = Math.max(120, ctx.height - 26);
    }

    /* Pivot the long rows into one object per x position holding a value
       for every series. A series with no row at some x stays at zero -
       stacks can't have holes. */
    var xKeys = pv.uniq(ctx.x.data.map(function (d) { return d.x; }));
    if (xtype !== "category") {
      xKeys.sort(function (a, b) { return d3.ascending(a, b); });
    }
    var byKey = {};
    var pivot = xKeys.map(function (k) {
      var row = { key: k, x: xtype === "date" ? parse(k) : k };
      seriesNames.forEach(function (nm) { row[nm] = 0; });
      byKey[String(k)] = row;
      return row;
    });
    ctx.x.data.forEach(function (d) {
      if (d.y != null) { byKey[String(d.x)][d.series] = +d.y; }
    });

    /* Streams read best with the big series in the middle and a soft
       curve; the other modes keep the given order and run the curve
       through every data point exactly. */
    var stack = d3.stack().keys(seriesNames)
      .offset(offset === "percent" ? d3.stackOffsetExpand :
              offset === "stream" ? d3.stackOffsetSilhouette :
              d3.stackOffsetNone)
      .order(offset === "stream" ? d3.stackOrderInsideOut :
             d3.stackOrderNone);
    var layers = stack(pivot);

    /* A stream has no y axis (its heights are only meaningful relative to
       each other), so it doesn't need room for one on the left. */
    var m = { top: 12, right: 24, bottom: 52,
              left: offset === "stream" ? 24 : 58 };
    var iw = ctx.width - m.left - m.right,
        ih = ctx.height - m.top - m.bottom;
    var svg = pv.baseSvg(ctx);
    var g = svg.append("g").attr("transform",
      "translate(" + m.left + "," + m.top + ")");

    /* No nice() on the x domain: an area should fill the plot edge to
       edge, not trail off into rounded-out empty space. */
    var xScale;
    if (xtype === "category") {
      xScale = d3.scalePoint().domain(xKeys).range([0, iw]);
    } else if (xtype === "date") {
      xScale = d3.scaleTime()
        .domain(d3.extent(pivot, function (d) { return d.x; }))
        .range([0, iw]);
    } else {
      xScale = d3.scaleLinear()
        .domain(d3.extent(pivot, function (d) { return d.x; }))
        .range([0, iw]);
    }
    var y = d3.scaleLinear().range([ih, 0]);
    if (offset === "percent") {
      y.domain([0, 1]);
    } else if (offset === "stream") {
      y.domain([
        d3.min(layers, function (l) {
          return d3.min(l, function (p) { return p[0]; }); }),
        d3.max(layers, function (l) {
          return d3.max(l, function (p) { return p[1]; }); })
      ]);
    } else {
      y.domain([0, d3.max(layers, function (l) {
        return d3.max(l, function (p) { return p[1]; }); }) || 1]).nice();
    }

    if (offset !== "stream") { pv.yGrid(g, y, iw, ctx.theme); }
    var xAxis = d3.axisBottom(xScale).tickSizeOuter(0);
    if (xtype === "category") {
      /* A long category axis can't show every label - keep roughly one per
         80px and let the tooltip carry the exact values. */
      var maxTicks = Math.max(2, Math.floor(iw / 80));
      var step = Math.ceil(xKeys.length / maxTicks);
      xAxis.tickValues(xKeys.filter(function (d, i) {
        return i % step === 0;
      }));
    } else {
      xAxis.ticks(Math.min(8, Math.floor(iw / 80)));
      if (xtype === "number") {
        /* Plain digits for numeric x - the usual case is years, and
           "1,930" with a thousands comma reads as a quantity, not a year. */
        xAxis.tickFormat(function (v) {
          return Math.abs(v) >= 10000 ?
            d3.format("~s")(v) : d3.format("~f")(v);
        });
      }
    }
    g.append("g").attr("transform", "translate(0," + ih + ")")
      .call(xAxis).call(function (s) { pv.styleAxis(s, ctx.theme, true); });
    if (offset !== "stream") {
      g.append("g").call(d3.axisLeft(y).ticks(5)
          .tickFormat(offset === "percent" ? d3.format(".0%") : pv.fmtTick))
        .call(function (s) { pv.styleAxis(s, ctx.theme, false); });
    }
    pv.axisLabels(svg, ctx, m, iw, ih, ctx.x.xlab,
      offset === "stacked" ? ctx.x.ylab : null);

    var area = d3.area()
      .x(function (p) { return xScale(p.data.x); })
      .y0(function (p) { return y(p[0]); })
      .y1(function (p) { return y(p[1]); })
      .curve(offset === "stream" ? d3.curveBasis : d3.curveMonotoneX);

    /* The 1.5px surface-coloured stroke draws a seam between layers so
       neighbouring bands never touch. */
    var paths = g.selectAll("path.layer").data(layers).enter()
      .append("path")
      .attr("class", "layer")
      .attr("fill", function (l) { return color(l.key); })
      .attr("fill-opacity", 0.85)
      .attr("stroke", ctx.theme.ink.surface)
      .attr("stroke-width", 1.5)
      .attr("d", area)
      .attr("opacity", 0);

    /* Layers fade in from the bottom of the stack upward. */
    paths.transition().duration(ctx.duration)
      .delay(function (l) { return Math.min(l.index * 90, 500); })
      .ease(d3.easeCubicOut)
      .attr("opacity", 1);

    /* The crosshair: an invisible rectangle covers the plot, finds the
       nearest x position under the pointer, and reads every series out in
       one tooltip. The band under the pointer is lifted out of the stack;
       the dimming is instant on purpose. */
    var cross = g.append("line")
      .attr("y1", 0).attr("y2", ih)
      .attr("stroke", ctx.theme.ink.baseline)
      .attr("stroke-dasharray", "3,3").attr("opacity", 0);
    var dots = g.append("g");
    var xPos = pivot.map(function (r) { return xScale(r.x); });

    g.append("rect")
      .attr("width", iw).attr("height", ih)
      .attr("fill", "transparent")
      .on("pointermove", function (event) {
        var p = d3.pointer(event, this);
        var idx = 0, best = Infinity;
        xPos.forEach(function (px, i) {
          var dd = Math.abs(px - p[0]);
          if (dd < best) { best = dd; idx = i; }
        });
        cross.attr("x1", xPos[idx]).attr("x2", xPos[idx]).attr("opacity", 1);

        var hoverKey = null;
        layers.forEach(function (l) {
          var top = y(l[idx][1]), bot = y(l[idx][0]);
          if (p[1] >= Math.min(top, bot) && p[1] <= Math.max(top, bot)) {
            hoverKey = l.key;
          }
        });
        paths.attr("fill-opacity", function (l) {
          if (hoverKey === null) return 0.85;
          return l.key === hoverKey ? 1 : 0.35;
        });

        /* Stream curves are smoothed and don't pass through the data
           points, so the marker dots only appear for the exact modes. */
        if (offset !== "stream") {
          var sel = dots.selectAll("circle").data(layers);
          sel.enter().append("circle").attr("r", 4)
            .attr("stroke", ctx.theme.ink.surface).attr("stroke-width", 2)
            .merge(sel)
            .attr("cx", xPos[idx])
            .attr("cy", function (l) { return y(l[idx][1]); })
            .attr("fill", function (l) { return color(l.key); });
          sel.exit().remove();
        }

        var row = pivot[idx];
        var total = d3.sum(seriesNames, function (nm) { return row[nm]; });
        var xLabel = xtype === "date" ?
          d3.timeFormat("%b %e, %Y")(row.x) : row.key;
        var tipRows = seriesNames.map(function (nm) {
          var val = ctx.fmt(row[nm]);
          if (offset === "percent") {
            val += " (" + d3.format(".1%")(total ? row[nm] / total : 0) + ")";
          }
          return pv.swatchRow(color(nm), nm, val);
        });
        if (offset === "stacked" && seriesNames.length > 1) {
          tipRows.push("total: <b>" + ctx.fmt(total) + "</b>");
        }
        pv.showTip(ctx, event, "<b>" + pv.esc(xLabel) + "</b><br>" +
          tipRows.join("<br>"));
      })
      .on("pointerleave", function () {
        cross.attr("opacity", 0);
        dots.selectAll("circle").remove();
        paths.attr("fill-opacity", 0.85);
        pv.hideTip(ctx);
      });
  };

  /* ---------- heatmap ---------- */

  pvRenderers.heatmap = function (ctx) {
    var data = ctx.x.data;
    var domain = ctx.x.domain;
    var diverging = ctx.x.palette === "diverging";

    /* The colour of a cell. Sequential glides through the theme's 11-step
       ramp; diverging pins its neutral midpoint to zero (the R side made
       the domain symmetric, so the poles carry equal weight). */
    var colorOf;
    if (diverging) {
      var dv = ctx.theme.diverging;
      colorOf = d3.scaleDiverging(
        d3.piecewise(d3.interpolateRgb, [dv.low, dv.mid, dv.high]))
        .domain([domain[0], 0, domain[1]]).clamp(true);
    } else {
      var ramp = d3.interpolateRgbBasis(ctx.theme.sequential);
      var t = d3.scaleLinear().domain(domain).range([0, 1]).clamp(true);
      colorOf = function (v) { return ramp(t(v)); };
    }

    /* Compact numbers for the legend ends and the in-cell labels: SI
       units past 10k, at most two decimals below. */
    var cellFmt = function (v) {
      return Math.abs(v) >= 10000 ?
        d3.format("~s")(v) : d3.format(",.2~f")(v);
    };

    /* The heatmap's legend is its colour scale: a small gradient bar in
       the header with the domain ends labelled. */
    var scaleRow = document.createElement("div");
    scaleRow.style.cssText =
      "display:flex;align-items:center;gap:7px;margin-top:7px;" +
      "font-size:11px;font-variant-numeric:tabular-nums;color:" +
      ctx.theme.ink.muted + ";";
    var stops = [];
    for (var i = 0; i <= 10; i++) {
      stops.push(colorOf(domain[0] + (domain[1] - domain[0]) * i / 10) +
        " " + (i * 10) + "%");
    }
    var bar = document.createElement("span");
    bar.style.cssText = "width:140px;height:8px;border-radius:4px;" +
      "background:linear-gradient(90deg," + stops.join(",") + ");";
    var lo = document.createElement("span");
    lo.textContent = cellFmt(domain[0]);
    var hi = document.createElement("span");
    hi.textContent = cellFmt(domain[1]);
    scaleRow.appendChild(lo);
    scaleRow.appendChild(bar);
    scaleRow.appendChild(hi);
    ctx.header.appendChild(scaleRow);
    ctx.height = Math.max(120, ctx.height - 26);

    var xCats = pv.uniq(data.map(function (d) { return d.x; }));
    var yCats = pv.uniq(data.map(function (d) { return d.y; }));
    var longest = d3.max(yCats, function (d) { return d.length; }) || 4;
    var m = { top: 8, right: 14, bottom: 34,
              left: Math.min(190, 22 + longest * 6.6) };
    var iw = ctx.width - m.left - m.right,
        ih = ctx.height - m.top - m.bottom;
    var svg = pv.baseSvg(ctx);
    var g = svg.append("g").attr("transform",
      "translate(" + m.left + "," + m.top + ")");

    var xb = d3.scaleBand().domain(xCats).range([0, iw]);
    var yb = d3.scaleBand().domain(yCats).range([0, ih]);

    /* Crowded column labels thin out to roughly one per 80px, same as the
       category axis on a line chart. */
    var maxTicks = Math.max(2, Math.floor(iw / 80));
    var step = Math.ceil(xCats.length / maxTicks);
    var xAxis = g.append("g").attr("transform", "translate(0," + ih + ")")
      .call(d3.axisBottom(xb).tickSizeOuter(0)
        .tickValues(xCats.filter(function (d, i) { return i % step === 0; })));
    pv.styleAxis(xAxis, ctx.theme, true);

    var yAxis = g.append("g").call(d3.axisLeft(yb).tickSize(0));
    pv.styleAxis(yAxis, ctx.theme, false);
    /* Row labels that outgrow the margin get an ellipsis; the tooltip
       carries the full name. */
    var maxChars = Math.max(3, Math.floor((m.left - 14) / 6.2));
    yAxis.selectAll("text").text(function (d) {
      return d.length > maxChars ? d.slice(0, maxChars - 1) + "…" : d;
    });

    /* Pick whichever of the two ink extremes reads better on a given cell
       colour, by WCAG contrast ratio from relative luminance. */
    function lum(c) {
      var rgb = d3.rgb(c);
      function chan(u) {
        u /= 255;
        return u <= 0.04045 ? u / 12.92 : Math.pow((u + 0.055) / 1.055, 2.4);
      }
      return 0.2126 * chan(rgb.r) + 0.7152 * chan(rgb.g) +
        0.0722 * chan(rgb.b);
    }
    var lumPrimary = lum(ctx.theme.ink.primary);
    var lumSurface = lum(ctx.theme.ink.surface);
    function inkFor(fill) {
      var L = lum(fill);
      var vsPrimary = (Math.max(L, lumPrimary) + 0.05) /
        (Math.min(L, lumPrimary) + 0.05);
      var vsSurface = (Math.max(L, lumSurface) + 0.05) /
        (Math.min(L, lumSurface) + 0.05);
      return vsPrimary >= vsSurface ?
        ctx.theme.ink.primary : ctx.theme.ink.surface;
    }

    var xIndex = {};
    xCats.forEach(function (c, i) { xIndex[c] = i; });

    /* Cells live in their own layer so a raised (hovered) cell can't cover
       the value labels drawn after them. The 2px surface stroke is what
       makes the gaps between cells. */
    var cellLayer = g.append("g");
    var cells = cellLayer.selectAll("rect.cell").data(data).enter()
      .append("rect")
      .attr("class", "cell")
      .attr("x", function (d) { return xb(d.x); })
      .attr("y", function (d) { return yb(d.y); })
      .attr("width", xb.bandwidth())
      .attr("height", yb.bandwidth())
      .attr("fill", function (d) { return colorOf(d.value); })
      .attr("stroke", ctx.theme.ink.surface)
      .attr("stroke-width", 2)
      .attr("opacity", 0);

    /* Cells sweep in column by column. */
    cells.transition().duration(ctx.duration)
      .delay(function (d) { return Math.min(xIndex[d.x] * 40, 500); })
      .ease(d3.easeCubicOut)
      .attr("opacity", 1);

    /* Print values in the cells only when they genuinely fit. */
    if (xb.bandwidth() > 40 && yb.bandwidth() > 40) {
      g.selectAll("text.cellval").data(data).enter().append("text")
        .attr("class", "cellval")
        .attr("x", function (d) { return xb(d.x) + xb.bandwidth() / 2; })
        .attr("y", function (d) { return yb(d.y) + yb.bandwidth() / 2; })
        .attr("text-anchor", "middle")
        .attr("dominant-baseline", "middle")
        .attr("fill", function (d) { return inkFor(colorOf(d.value)); })
        .style("font-size", "11px")
        .style("font-variant-numeric", "tabular-nums")
        .style("pointer-events", "none")
        .style("opacity", 0)
        .text(function (d) { return cellFmt(d.value); })
        .transition().delay(ctx.duration).duration(200)
        .style("opacity", 1);
    }

    /* Hovering rings the cell in primary ink (raised so the ring isn't
       hidden under its neighbours' strokes). */
    cells
      .on("pointerenter pointermove", function (event, d) {
        d3.select(this).raise().attr("stroke", ctx.theme.ink.primary);
        pv.showTip(ctx, event,
          "<b>" + pv.esc(d.x) + " · " + pv.esc(d.y) + "</b><br>" +
          pv.esc(ctx.x.vlab || "value") + ": <b>" + ctx.fmt(d.value) +
          "</b>");
      })
      .on("pointerleave", function () {
        d3.select(this).attr("stroke", ctx.theme.ink.surface);
        pv.hideTip(ctx);
      });
  };

})();
