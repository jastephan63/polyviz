/*
 * Core chart renderers: bar, line, scatter.
 * Each renderer receives a ctx object prepared by pvchart.js:
 *   ctx.el      - the container element
 *   ctx.x       - the payload sent from R (data, labels, options)
 *   ctx.theme   - resolved colours for the current light/dark mode
 *   ctx.tip     - the tooltip element (use pv.showTip / pv.hideTip)
 *   ctx.header  - the title block (legends get appended here)
 *   ctx.width / ctx.height - pixel size available for the SVG
 *   ctx.duration - entrance transition length in ms
 *   ctx.fmt     - number formatter for tooltips
 */
(function () {

  /* Resolves a TRUE/FALSE/"auto" option sent from R. Explicit values win;
     "auto" (or an old payload with no value at all) takes the decision the
     renderer computed from the real data and pixel sizes - which is the
     point of "auto": only this side knows the actual container. */
  function opt(v, autoDecision) {
    return v === "auto" || v == null ? autoDecision : !!v;
  }

  /* Bar value labels want to be short: three significant digits, with an
     SI suffix for big numbers (4681280 reads "4.68M", not "4.68128M").
     The tooltip still shows the exact value. */
  function fmtVal(v) {
    return Math.abs(v) >= 10000 ?
      d3.format(".3~s")(v) : d3.format(".3~r")(v);
  }

  /* ---------- bar ---------- */

  pvRenderers.bar = function (ctx) {
    var data = ctx.x.data;
    var grouped = data.length && data[0].series !== undefined;
    var cats = pv.uniq(data.map(function (d) { return d.x; }));

    /* "auto" flips to horizontal bars when this is a single series whose
       category labels cannot sit side by side under vertical bars: their
       total width would overflow the plot, or one label alone is far
       wider than its band. 72px is the vertical layout's side margins. */
    var estW = Math.max(50, ctx.width - 72);
    var step = estW / Math.max(1, cats.length);
    var widest = d3.max(cats, function (c) {
      return pv.textWidth(c, 11); }) || 0;
    var total = d3.sum(cats, function (c) { return pv.textWidth(c, 11); });
    var horizontal = opt(ctx.x.horizontal,
      !grouped && (total > 0.85 * estW || widest > 1.6 * step));
    if (horizontal && !grouped) { return renderBarH(ctx); }
    var seriesNames = grouped ?
      pv.uniq(data.map(function (d) { return d.series; })) : [];
    var color = d3.scaleOrdinal()
      .domain(grouped ? seriesNames : ["value"])
      .range(ctx.theme.palette);
    if (grouped) {
      pv.buildLegend(ctx.header, seriesNames, color, ctx.theme);
      ctx.height = Math.max(120, ctx.height - 26);
    }

    var m = { top: 12, right: 14, bottom: 52, left: 58 };
    var iw = ctx.width - m.left - m.right,
        ih = ctx.height - m.top - m.bottom;
    var svg = pv.baseSvg(ctx);
    var g = svg.append("g").attr("transform",
      "translate(" + m.left + "," + m.top + ")");

    var x0 = d3.scaleBand().domain(cats).range([0, iw]).paddingInner(0.28)
      .paddingOuter(0.12);
    var x1 = grouped ?
      d3.scaleBand().domain(seriesNames).range([0, x0.bandwidth()])
        .paddingInner(Math.min(0.35, 2 / Math.max(1, x0.bandwidth() /
          seriesNames.length))) : null;
    var yMax = d3.max(data, function (d) { return d.y; }) || 1;
    var y = d3.scaleLinear().domain([0, yMax]).nice().range([ih, 0]);

    pv.yGrid(g, y, iw, ctx.theme);
    g.append("g").attr("transform", "translate(0," + ih + ")")
      .call(d3.axisBottom(x0).tickSizeOuter(0))
      .call(function (s) { pv.styleAxis(s, ctx.theme, true); });
    g.append("g").call(d3.axisLeft(y).ticks(5).tickFormat(pv.fmtTick))
      .call(function (s) { pv.styleAxis(s, ctx.theme, false); });
    pv.axisLabels(svg, ctx, m, iw, ih, ctx.x.xlab, ctx.x.ylab);

    /* Each bar starts at zero height and grows up to its value, with a
       small stagger from left to right so the chart builds across. */
    var bars = g.selectAll("path.bar").data(data).enter().append("path")
      .attr("class", "bar")
      .attr("fill", function (d) { return color(grouped ? d.series : "value"); })
      .attr("d", function (d) {
        var bx = grouped ? x0(d.x) + x1(d.series) : x0(d.x);
        var bw = grouped ? x1.bandwidth() : x0.bandwidth();
        return pv.topRoundedBar(bx, ih, bw, 0, 4);
      });

    bars.transition().duration(ctx.duration)
      .delay(function (d, i) { return Math.min(i * 24, 600); })
      .ease(d3.easeCubicOut)
      .attrTween("d", function (d) {
        var bx = grouped ? x0(d.x) + x1(d.series) : x0(d.x);
        var bw = grouped ? x1.bandwidth() : x0.bandwidth();
        var hFinal = ih - y(d.y);
        return function (t) {
          return pv.topRoundedBar(bx, y(d.y) + (1 - t) * hFinal, bw,
            t * hFinal, 4);
        };
      });

    /* Value labels above the bars. "auto" shows them only when a single
       series has few enough bars, each wide enough, for the numbers to
       read cleanly; TRUE forces them on regardless. They fade in after
       the bars have finished growing. */
    var showVals = opt(ctx.x.valueLabels,
      !grouped && data.length <= 12 && x0.bandwidth() >= 34);
    if (showVals) {
      g.selectAll("text.val").data(data).enter().append("text")
        .attr("class", "val")
        .attr("x", function (d) {
          var bx = grouped ? x0(d.x) + x1(d.series) : x0(d.x);
          var bw = grouped ? x1.bandwidth() : x0.bandwidth();
          return bx + bw / 2;
        })
        .attr("y", function (d) { return y(d.y) - 5; })
        .attr("text-anchor", "middle")
        .attr("fill", ctx.theme.ink.secondary)
        .style("font-size", "11px")
        .style("font-variant-numeric", "tabular-nums")
        .style("opacity", 0)
        .text(function (d) { return fmtVal(d.y); })
        .transition().delay(ctx.duration).duration(200)
        .style("opacity", 1);
    }

    /* Hovering a bar dims the others and shows its exact value. */
    bars
      .on("pointerenter pointermove", function (event, d) {
        bars.attr("opacity", function (b) { return b === d ? 1 : 0.45; });
        var head = grouped ?
          pv.esc(d.x) + " &middot; " + pv.esc(d.series) : pv.esc(d.x);
        pv.showTip(ctx, event, head + "<br>" +
          pv.swatchRow(color(grouped ? d.series : "value"),
                       ctx.x.ylab || "value", ctx.fmt(d.y)));
      })
      .on("pointerleave", function () {
        bars.attr("opacity", 1);
        pv.hideTip(ctx);
      });
  };

  /* Horizontal bars: the answer to long category names. Categories run
     down the left in plain horizontal text, bars grow rightward from a
     zero baseline. Single-series only (the R side enforces it). */
  function renderBarH(ctx) {
    var data = ctx.x.data;
    /* The label margin grows with the longest category name, but it must
       never eat the plot: cap it at 45% of the chart width - and tighter
       still where needed so the bars keep at least half the container -
       then truncate the labels to what actually fits. The tooltip always
       carries the full name, so a shortened label loses nothing. */
    var longest = d3.max(data, function (d) {
      return pv.textWidth(d.x, 11); }) || 30;
    var m = { top: 8, right: 46, bottom: 34,
              left: Math.max(70, Math.min(190, 0.45 * ctx.width,
                0.5 * ctx.width - 46, 22 + longest)) };
    var maxChars = Math.max(4, Math.floor((m.left - 12) / 6.9));
    var iw = ctx.width - m.left - m.right,
        ih = ctx.height - m.top - m.bottom;
    var svg = pv.baseSvg(ctx);
    var g = svg.append("g").attr("transform",
      "translate(" + m.left + "," + m.top + ")");

    var yBand = d3.scaleBand()
      .domain(data.map(function (d) { return d.x; }))
      .range([0, ih]).paddingInner(0.3).paddingOuter(0.1);
    var x = d3.scaleLinear()
      .domain([0, d3.max(data, function (d) { return d.y; }) || 1])
      .nice().range([0, iw]);

    var xAxis = g.append("g").attr("transform", "translate(0," + ih + ")")
      .call(d3.axisBottom(x).ticks(Math.min(6, Math.floor(iw / 80)))
        .tickFormat(pv.fmtTick).tickSizeOuter(0));
    pv.styleAxis(xAxis, ctx.theme, true);
    var yAxis = g.append("g").call(d3.axisLeft(yBand).tickSize(0)
      .tickFormat(function (d) { return pv.truncate(d, maxChars); }));
    pv.styleAxis(yAxis, ctx.theme, false);

    var bars = g.selectAll("path.bar").data(data).enter().append("path")
      .attr("class", "bar")
      .attr("fill", ctx.theme.palette[0])
      .attr("d", function (d) {
        return pv.rightRoundedBar(0, yBand(d.x), 0, yBand.bandwidth(), 4);
      });

    bars.transition().duration(ctx.duration)
      .delay(function (d, i) { return Math.min(i * 22, 500); })
      .ease(d3.easeCubicOut)
      .attrTween("d", function (d) {
        var w = x(d.y);
        return function (t) {
          return pv.rightRoundedBar(0, yBand(d.x), t * w,
            yBand.bandwidth(), 4);
        };
      });

    /* Value labels at the bar ends - horizontal bars have the room, and a
       labelled bar needs no gridlines at all. On by default; only an
       explicit value_labels = FALSE from R switches them off. */
    if (opt(ctx.x.valueLabels, true)) {
      g.selectAll("text.val").data(data).enter().append("text")
        .attr("class", "val")
        .attr("x", function (d) { return x(d.y) + 6; })
        .attr("y", function (d) { return yBand(d.x) + yBand.bandwidth() / 2; })
        .attr("dominant-baseline", "middle")
        .attr("fill", ctx.theme.ink.secondary)
        .style("font-size", "11px")
        .style("font-variant-numeric", "tabular-nums")
        .style("opacity", 0)
        .text(function (d) { return fmtVal(d.y); })
        .transition().delay(ctx.duration).duration(200)
        .style("opacity", 1);
    }

    bars
      .on("pointerenter pointermove", function (event, d) {
        bars.attr("opacity", function (b) { return b === d ? 1 : 0.45; });
        pv.showTip(ctx, event, pv.esc(d.x) + "<br>" +
          pv.swatchRow(ctx.theme.palette[0], ctx.x.ylab || "value",
                       ctx.fmt(d.y)));
      })
      .on("pointerleave", function () {
        bars.attr("opacity", 1);
        pv.hideTip(ctx);
      });
  }

  /* ---------- line ---------- */

  pvRenderers.line = function (ctx) {
    var xtype = ctx.x.xtype;
    var parse = xtype === "date" ? d3.timeParse("%Y-%m-%d") : null;
    var data = ctx.x.data.map(function (d) {
      return {
        x: xtype === "date" ? parse(d.x) : d.x,
        y: d.y, series: d.series
      };
    });
    var seriesNames = pv.uniq(data.map(function (d) { return d.series; }));
    var color = d3.scaleOrdinal().domain(seriesNames)
      .range(ctx.theme.palette);
    /* "auto" keeps the old rule - a legend only when a real series
       mapping produced more than one line; TRUE and FALSE override it. */
    var showLegend = opt(ctx.x.legend,
      ctx.x.showLegend && seriesNames.length > 1);
    if (showLegend && seriesNames.length) {
      pv.buildLegend(ctx.header, seriesNames, color, ctx.theme);
      ctx.height = Math.max(120, ctx.height - 26);
    }

    var directLabels = seriesNames.length > 1 && seriesNames.length <= 4;
    var m = { top: 12, right: directLabels ? 90 : 24, bottom: 52, left: 58 };
    var iw = ctx.width - m.left - m.right,
        ih = ctx.height - m.top - m.bottom;
    var svg = pv.baseSvg(ctx);
    var g = svg.append("g").attr("transform",
      "translate(" + m.left + "," + m.top + ")");

    var xScale;
    if (xtype === "category") {
      xScale = d3.scalePoint()
        .domain(pv.uniq(data.map(function (d) { return d.x; })))
        .range([0, iw]).padding(0.5);
    } else if (xtype === "date") {
      xScale = d3.scaleTime()
        .domain(d3.extent(data, function (d) { return d.x; })).range([0, iw]);
    } else {
      xScale = d3.scaleLinear()
        .domain(d3.extent(data, function (d) { return d.x; })).nice()
        .range([0, iw]);
    }
    var y = d3.scaleLinear()
      .domain([Math.min(0, d3.min(data, function (d) { return d.y; })),
               d3.max(data, function (d) { return d.y; })])
      .nice().range([ih, 0]);

    pv.yGrid(g, y, iw, ctx.theme);
    var xAxis = d3.axisBottom(xScale).tickSizeOuter(0);
    if (xtype === "category") {
      /* A long category axis can't show every label - keep roughly one per
         80px and let the tooltip carry the exact values. */
      var domain = xScale.domain();
      var maxTicks = Math.max(2, Math.floor(iw / 80));
      var step = Math.ceil(domain.length / maxTicks);
      xAxis.tickValues(domain.filter(function (d, i) {
        return i % step === 0;
      }));
    } else {
      xAxis.ticks(Math.min(8, Math.floor(iw / 80)));
      /* Numeric axes must not comma-group - years would render "1,940". */
      if (xtype === "number") { xAxis.tickFormat(pv.fmtTick); }
    }
    g.append("g").attr("transform", "translate(0," + ih + ")")
      .call(xAxis).call(function (s) { pv.styleAxis(s, ctx.theme, true); });
    g.append("g").call(d3.axisLeft(y).ticks(5).tickFormat(pv.fmtTick))
      .call(function (s) { pv.styleAxis(s, ctx.theme, false); });
    pv.axisLabels(svg, ctx, m, iw, ih, ctx.x.xlab, ctx.x.ylab);

    var bySeries = seriesNames.map(function (nm) {
      return { name: nm, points: data.filter(function (d) {
        return d.series === nm; }) };
    });
    var line = d3.line()
      .x(function (d) { return xScale(d.x); })
      .y(function (d) { return y(d.y); });

    bySeries.forEach(function (s) {
      var path = g.append("path").datum(s.points)
        .attr("fill", "none")
        .attr("stroke", color(s.name))
        .attr("stroke-width", 2)
        .attr("stroke-linejoin", "round")
        .attr("d", line);
      /* The draw-in effect: dash the line with one dash as long as the whole
         path, start fully offset (invisible), then animate the offset to
         zero so the line appears to be drawn left to right. */
      var len = path.node().getTotalLength();
      path.attr("stroke-dasharray", len + " " + len)
        .attr("stroke-dashoffset", len)
        .transition().duration(ctx.duration).ease(d3.easeCubicInOut)
        .attr("stroke-dashoffset", 0)
        .on("end", function () { path.attr("stroke-dasharray", null); });

      });

    /* Direct labels at the line ends. Series that finish near the same
       value would overlap, so sort the label positions and push any pair
       closer than one line of text apart. */
    if (directLabels) {
      var ends = bySeries
        .filter(function (s) { return s.points.length; })
        .map(function (s) {
          var last = s.points[s.points.length - 1];
          return { name: s.name, x: xScale(last.x), y: y(last.y),
                   ly: y(last.y) };
        })
        .sort(function (a, b) { return a.ly - b.ly; });
      var minGap = 13;
      for (var i = 1; i < ends.length; i++) {
        if (ends[i].ly - ends[i - 1].ly < minGap) {
          ends[i].ly = ends[i - 1].ly + minGap;
        }
      }
      ends.forEach(function (e) {
        g.append("circle")
          .attr("cx", e.x).attr("cy", e.y).attr("r", 3.5)
          .attr("fill", color(e.name))
          .attr("stroke", ctx.theme.ink.surface).attr("stroke-width", 2);
        g.append("text")
          .attr("x", e.x + 8).attr("y", e.ly)
          .attr("dominant-baseline", "middle")
          .attr("fill", ctx.theme.ink.secondary).style("font-size", "11px")
          .text(e.name);
      });
    }

    /* The crosshair: an invisible rectangle covers the plot and tracks the
       pointer. On every move we find the nearest x position, drop a dashed
       vertical line there, mark each series with a dot, and list all their
       values in one tooltip. */
    var cross = g.append("line")
      .attr("y1", 0).attr("y2", ih)
      .attr("stroke", ctx.theme.ink.baseline)
      .attr("stroke-dasharray", "3,3").attr("opacity", 0);
    var dots = g.append("g");
    var xVals = pv.uniq(data.map(function (d) { return +xScale(d.x); }))
      .sort(function (a, b) { return a - b; });

    g.append("rect")
      .attr("width", iw).attr("height", ih)
      .attr("fill", "transparent")
      .on("pointermove", function (event) {
        var px = d3.pointer(event, this)[0];
        var nearest = xVals.reduce(function (a, b) {
          return Math.abs(b - px) < Math.abs(a - px) ? b : a;
        });
        var hits = data.filter(function (d) {
          return Math.abs(xScale(d.x) - nearest) < 0.5;
        });
        if (!hits.length) return;
        cross.attr("x1", nearest).attr("x2", nearest).attr("opacity", 1);
        var sel = dots.selectAll("circle").data(hits);
        sel.enter().append("circle").attr("r", 4)
          .attr("stroke", ctx.theme.ink.surface).attr("stroke-width", 2)
          .merge(sel)
          .attr("cx", function (d) { return xScale(d.x); })
          .attr("cy", function (d) { return y(d.y); })
          .attr("fill", function (d) { return color(d.series); });
        sel.exit().remove();
        var xLabel = xtype === "date" ?
          d3.timeFormat("%b %e, %Y")(hits[0].x) : hits[0].x;
        var rows = hits.map(function (d) {
          return pv.swatchRow(color(d.series), d.series, ctx.fmt(d.y));
        });
        pv.showTip(ctx, event, "<b>" + pv.esc(xLabel) + "</b><br>" +
          rows.join("<br>"));
      })
      .on("pointerleave", function () {
        cross.attr("opacity", 0);
        dots.selectAll("circle").remove();
        pv.hideTip(ctx);
      });
  };

  /* ---------- scatter ---------- */

  pvRenderers.scatter = function (ctx) {
    var data = ctx.x.data;
    var hasSeries = data.length && data[0].series !== undefined;
    var hasSize = data.length && data[0].size !== undefined;
    var seriesNames = hasSeries ?
      pv.uniq(data.map(function (d) { return d.series; })) : [];
    var color = d3.scaleOrdinal().domain(seriesNames)
      .range(ctx.theme.palette);
    /* "auto" keeps the old rule - a legend exactly when a colour mapping
       exists; TRUE and FALSE override it. */
    var showLegend = opt(ctx.x.legend, hasSeries);
    if (showLegend && seriesNames.length) {
      pv.buildLegend(ctx.header, seriesNames, color, ctx.theme);
      ctx.height = Math.max(120, ctx.height - 26);
    }

    /* Dense clouds need lighter ink: up to 150 points keep the usual
       opacity, beyond that fade smoothly toward a floor of 0.3 so
       overplotted regions still show their structure. */
    var baseOpacity = data.length <= 150 ? 0.62 :
      Math.max(0.3, 0.62 * Math.sqrt(150 / data.length));

    var m = { top: 12, right: 24, bottom: 52, left: 58 };
    var iw = ctx.width - m.left - m.right,
        ih = ctx.height - m.top - m.bottom;
    var svg = pv.baseSvg(ctx);
    var g = svg.append("g").attr("transform",
      "translate(" + m.left + "," + m.top + ")");

    var xScale = d3.scaleLinear()
      .domain(d3.extent(data, function (d) { return d.x; })).nice()
      .range([0, iw]);
    var y = d3.scaleLinear()
      .domain(d3.extent(data, function (d) { return d.y; })).nice()
      .range([ih, 0]);
    var r = hasSize ?
      d3.scaleSqrt()
        .domain(d3.extent(data, function (d) { return d.size; }))
        .range([3.5, 13]) :
      function () { return 4.5; };

    pv.yGrid(g, y, iw, ctx.theme);
    g.append("g").attr("transform", "translate(0," + ih + ")")
      .call(d3.axisBottom(xScale).ticks(Math.min(8, Math.floor(iw / 80)))
        .tickFormat(pv.fmtTick).tickSizeOuter(0))
      .call(function (s) { pv.styleAxis(s, ctx.theme, true); });
    g.append("g").call(d3.axisLeft(y).ticks(5).tickFormat(pv.fmtTick))
      .call(function (s) { pv.styleAxis(s, ctx.theme, false); });
    pv.axisLabels(svg, ctx, m, iw, ih, ctx.x.xlab, ctx.x.ylab);

    var pts = g.selectAll("circle.pt").data(data).enter().append("circle")
      .attr("class", "pt")
      .attr("cx", function (d) { return xScale(d.x); })
      .attr("cy", function (d) { return y(d.y); })
      .attr("r", 0)
      .attr("fill", function (d) {
        return hasSeries ? color(d.series) : ctx.theme.palette[0];
      })
      .attr("fill-opacity", baseOpacity)
      .attr("stroke", ctx.theme.ink.surface).attr("stroke-width", 1);

    pts.transition().duration(ctx.duration)
      .delay(function (d, i) { return Math.min(i * 6, 400); })
      .attr("r", function (d) { return hasSize ? r(d.size) : r(); });

    pts
      .on("pointerenter pointermove", function (event, d) {
        /* Lift the hovered point to full opacity so it reads clearly
           even inside a dense, faded cloud. */
        d3.select(this)
          .attr("fill-opacity", 1)
          .attr("stroke-width", 2)
          .attr("r", (hasSize ? r(d.size) : r()) * 1.35);
        var rows = [];
        if (d.label !== undefined) rows.push("<b>" + pv.esc(d.label) + "</b>");
        if (hasSeries) {
          rows.push(pv.swatchRow(color(d.series), "group", pv.esc(d.series)));
        }
        rows.push(pv.esc(ctx.x.xlab || "x") + ": <b>" + ctx.fmt(d.x) + "</b>");
        rows.push(pv.esc(ctx.x.ylab || "y") + ": <b>" + ctx.fmt(d.y) + "</b>");
        if (hasSize) {
          rows.push(pv.esc(ctx.x.sizelab || "size") + ": <b>" +
            ctx.fmt(d.size) + "</b>");
        }
        pv.showTip(ctx, event, rows.join("<br>"));
      })
      .on("pointerleave", function (event, d) {
        d3.select(this)
          .attr("fill-opacity", baseOpacity)
          .attr("stroke-width", 1)
          .attr("r", hasSize ? r(d.size) : r());
        pv.hideTip(ctx);
      });
  };

})();
