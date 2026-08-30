/*
 * Distribution renderers: histogram, boxplot, ridgeline. Every statistic
 * (bins, quartiles, densities) arrives pre-computed from R, so the code
 * here only draws geometry. See basic.js for the ctx contract.
 */
(function () {

  /* Deterministic pseudo-random in [0, 1) from an index, so jittered
     points land in the same spots on every redraw and resize. */
  function hash(i) {
    var t = Math.sin((i + 1) * 12.9898) * 43758.5453;
    return t - Math.floor(t);
  }

  /* ---------- histogram ---------- */

  pvRenderers.histogram = function (ctx) {
    var bins = ctx.x.data;
    var curve = ctx.x.density || [];

    var m = { top: 12, right: 24, bottom: 52, left: 58 };
    var iw = ctx.width - m.left - m.right,
        ih = ctx.height - m.top - m.bottom;
    var svg = pv.baseSvg(ctx);
    var g = svg.append("g").attr("transform",
      "translate(" + m.left + "," + m.top + ")");

    var x = d3.scaleLinear()
      .domain([d3.min(bins, function (d) { return d.x0; }),
               d3.max(bins, function (d) { return d.x1; })])
      .range([0, iw]);
    /* The density curve lives in count space too, so let whichever is
       taller set the y scale. */
    var yMax = Math.max(
      d3.max(bins, function (d) { return d.count; }) || 1,
      d3.max(curve, function (d) { return d.y; }) || 0);
    var y = d3.scaleLinear().domain([0, yMax]).nice().range([ih, 0]);

    pv.yGrid(g, y, iw, ctx.theme);
    g.append("g").attr("transform", "translate(0," + ih + ")")
      .call(d3.axisBottom(x).ticks(Math.min(8, Math.floor(iw / 80)))
        .tickFormat(pv.fmtTick).tickSizeOuter(0))
      .call(function (s) { pv.styleAxis(s, ctx.theme, true); });
    g.append("g").call(d3.axisLeft(y).ticks(5).tickFormat(pv.fmtTick))
      .call(function (s) { pv.styleAxis(s, ctx.theme, false); });
    pv.axisLabels(svg, ctx, m, iw, ih, ctx.x.xlab, ctx.x.ylab);

    var fill = ctx.theme.palette[0];
    /* Adjacent bins touch on the x scale, so inset each bar one pixel per
       side for a 2px breathing gap between neighbours. */
    var barX = function (d) { return x(d.x0) + 1; };
    var barW = function (d) { return Math.max(1, x(d.x1) - x(d.x0) - 2); };

    var bars = g.selectAll("path.bar").data(bins).enter().append("path")
      .attr("class", "bar")
      .attr("fill", fill)
      .attr("d", function (d) {
        return pv.topRoundedBar(barX(d), ih, barW(d), 0, 4);
      });

    bars.transition().duration(ctx.duration)
      .delay(function (d, i) { return Math.min(i * 24, 500); })
      .ease(d3.easeCubicOut)
      .attrTween("d", function (d) {
        var bx = barX(d), bwd = barW(d);
        var hFinal = ih - y(d.count);
        return function (t) {
          return pv.topRoundedBar(bx, y(d.count) + (1 - t) * hFinal, bwd,
            t * hFinal, 4);
        };
      });

    bars
      .on("pointerenter pointermove", function (event, d) {
        bars.attr("opacity", function (b) { return b === d ? 1 : 0.45; });
        pv.showTip(ctx, event,
          "<b>" + ctx.fmt(d.x0) + " – " + ctx.fmt(d.x1) + "</b><br>" +
          pv.swatchRow(fill, ctx.x.ylab || "count", ctx.fmt(d.count)));
      })
      .on("pointerleave", function () {
        bars.attr("opacity", 1);
        pv.hideTip(ctx);
      });

    /* The optional density curve, drawn in with the same stroke-dash trick
       as the line chart, once the bars are mostly up. It ignores the
       pointer so bar tooltips keep working underneath it. */
    if (curve.length > 1) {
      var line = d3.line()
        .x(function (d) { return x(d.x); })
        .y(function (d) { return y(d.y); });
      var path = g.append("path").datum(curve)
        .attr("fill", "none")
        .attr("stroke", ctx.theme.palette[1])
        .attr("stroke-width", 2)
        .attr("stroke-linejoin", "round")
        .attr("pointer-events", "none")
        .attr("d", line);
      var len = path.node().getTotalLength();
      path.attr("stroke-dasharray", len + " " + len)
        .attr("stroke-dashoffset", len)
        .transition().delay(ctx.duration * 0.4).duration(ctx.duration)
        .ease(d3.easeCubicInOut)
        .attr("stroke-dashoffset", 0)
        .on("end", function () { path.attr("stroke-dasharray", null); });
    }
  };

  /* ---------- boxplot ---------- */

  pvRenderers.boxplot = function (ctx) {
    var boxes = ctx.x.boxes;
    var raw = ctx.x.points || [];
    var groups = boxes.map(function (b) { return b.group; });
    var color = d3.scaleOrdinal().domain(groups).range(ctx.theme.palette);

    var m = { top: 12, right: 24, bottom: 52, left: 58 };
    var iw = ctx.width - m.left - m.right,
        ih = ctx.height - m.top - m.bottom;
    var svg = pv.baseSvg(ctx);
    var g = svg.append("g").attr("transform",
      "translate(" + m.left + "," + m.top + ")");

    var xBand = d3.scaleBand().domain(groups).range([0, iw])
      .paddingInner(0.35).paddingOuter(0.15);
    /* A lone box on a wide chart would balloon; cap its width and centre
       it in its band. */
    var bw = Math.min(xBand.bandwidth(), 70);
    var ox = (xBand.bandwidth() - bw) / 2;

    /* Whiskers plus outliers already bound every raw value, so they set
       the y extent. */
    var vmin = d3.min(boxes, function (b) {
      return b.outliers.length ? Math.min(b.lo, d3.min(b.outliers)) : b.lo;
    });
    var vmax = d3.max(boxes, function (b) {
      return b.outliers.length ? Math.max(b.hi, d3.max(b.outliers)) : b.hi;
    });
    var y = d3.scaleLinear().domain([vmin, vmax]).nice().range([ih, 0]);

    pv.yGrid(g, y, iw, ctx.theme);
    g.append("g").attr("transform", "translate(0," + ih + ")")
      .call(d3.axisBottom(xBand).tickSizeOuter(0))
      .call(function (s) { pv.styleAxis(s, ctx.theme, true); });
    g.append("g").call(d3.axisLeft(y).ticks(5).tickFormat(pv.fmtTick))
      .call(function (s) { pv.styleAxis(s, ctx.theme, false); });
    pv.axisLabels(svg, ctx, m, iw, ih, ctx.x.xlab, ctx.x.ylab);

    function fadeIn(sel, delay, to) {
      sel.attr("opacity", 0)
        .transition().delay(delay).duration(ctx.duration)
        .ease(d3.easeCubicOut).attr("opacity", to == null ? 1 : to);
      return sel;
    }

    var groupSel = [];
    boxes.forEach(function (b, i) {
      var gx = xBand(b.group) + ox;
      var cx = gx + bw / 2;
      var c = color(b.group);
      var grp = g.append("g");
      groupSel.push(grp);
      var delay = Math.min(i * 60, 500);

      /* Jittered raw values sit behind the semi-transparent box, so the
         middle of the distribution ghosts through it. */
      var mine = raw.filter(function (p) { return p.group === b.group; });
      fadeIn(grp.selectAll("circle.raw").data(mine).enter()
        .append("circle")
        .attr("class", "raw")
        .attr("cx", function (p, j) { return cx + (hash(j) - 0.5) * bw * 0.8; })
        .attr("cy", function (p) { return y(p.value); })
        .attr("r", 4)
        .attr("fill", c)
        .attr("stroke", ctx.theme.ink.surface).attr("stroke-width", 1),
        delay, 0.35);

      /* Whisker stems and caps. */
      [[b.lo, b.q1], [b.q3, b.hi]].forEach(function (seg) {
        fadeIn(grp.append("line")
          .attr("x1", cx).attr("x2", cx)
          .attr("y1", y(seg[0])).attr("y2", y(seg[1]))
          .attr("stroke", c).attr("stroke-width", 1.5), delay);
      });
      [b.lo, b.hi].forEach(function (v) {
        fadeIn(grp.append("line")
          .attr("x1", cx - bw * 0.25).attr("x2", cx + bw * 0.25)
          .attr("y1", y(v)).attr("y2", y(v))
          .attr("stroke", c).attr("stroke-width", 1.5), delay);
      });

      /* The box grows outward from the median line to the quartiles. */
      grp.append("rect")
        .attr("x", gx).attr("width", bw).attr("rx", 3)
        .attr("fill", c).attr("fill-opacity", 0.75)
        .attr("y", y(b.median)).attr("height", 0)
        .transition().delay(delay).duration(ctx.duration)
        .ease(d3.easeCubicOut)
        .attr("y", y(b.q3))
        .attr("height", Math.max(1, y(b.q1) - y(b.q3)));

      fadeIn(grp.append("line")
        .attr("x1", gx).attr("x2", gx + bw)
        .attr("y1", y(b.median)).attr("y2", y(b.median))
        .attr("stroke", ctx.theme.ink.primary).attr("stroke-width", 2),
        delay);

      fadeIn(grp.selectAll("circle.out").data(b.outliers).enter()
        .append("circle")
        .attr("class", "out")
        .attr("cx", cx).attr("cy", function (v) { return y(v); })
        .attr("r", 4)
        .attr("fill", c).attr("fill-opacity", 0.9)
        .attr("stroke", ctx.theme.ink.surface).attr("stroke-width", 1),
        delay);

      /* An invisible band-wide rectangle makes the whole column
         hoverable, not just the thin whisker lines. */
      grp.append("rect")
        .attr("x", xBand(b.group)).attr("width", xBand.bandwidth())
        .attr("y", 0).attr("height", ih)
        .attr("fill", "transparent")
        .on("pointerenter pointermove", function (event) {
          groupSel.forEach(function (other, j) {
            other.attr("opacity", j === i ? 1 : 0.3);
          });
          var rows = [
            pv.swatchRow(c, b.group, "n = " + b.n),
            "upper whisker: <b>" + ctx.fmt(b.hi) + "</b>",
            "upper quartile: <b>" + ctx.fmt(b.q3) + "</b>",
            "median: <b>" + ctx.fmt(b.median) + "</b>",
            "lower quartile: <b>" + ctx.fmt(b.q1) + "</b>",
            "lower whisker: <b>" + ctx.fmt(b.lo) + "</b>"
          ];
          if (b.outliers.length) {
            rows.push("outliers: <b>" + b.outliers.length + "</b>");
          }
          pv.showTip(ctx, event, rows.join("<br>"));
        })
        .on("pointerleave", function () {
          groupSel.forEach(function (other) { other.attr("opacity", 1); });
          pv.hideTip(ctx);
        });
    });
  };

  /* ---------- ridgeline ---------- */

  pvRenderers.ridgeline = function (ctx) {
    var ridges = ctx.x.ridges;
    /* Each ridge may rise 2.2 band-heights above its own baseline; that
       overlap is what makes shifts between the distributions readable. */
    var overlap = 2.2;

    var longest = d3.max(ridges, function (r) {
      return String(r.group).length; }) || 4;
    var m = { top: 12, right: 24, bottom: 40,
              left: Math.min(160, 24 + longest * 6.6) };
    var iw = ctx.width - m.left - m.right,
        ih = ctx.height - m.top - m.bottom;
    var svg = pv.baseSvg(ctx);
    var g = svg.append("g").attr("transform",
      "translate(" + m.left + "," + m.top + ")");

    var allX = [], maxY = 0;
    ridges.forEach(function (r) {
      r.points.forEach(function (p) {
        allX.push(p.x);
        if (p.y > maxY) maxY = p.y;
      });
    });
    var x = d3.scaleLinear().domain(d3.extent(allX)).nice().range([0, iw]);
    var color = d3.scaleOrdinal()
      .domain(ridges.map(function (r) { return r.group; }))
      .range(ctx.theme.palette);

    /* Divide the height so the last baseline lands exactly on the x axis
       and the first ridge still has headroom for its full peak. */
    var step = ih / (ridges.length + overlap - 1);
    var amp = d3.scaleLinear().domain([0, maxY]).range([0, step * overlap]);
    var baseY = function (i) { return (overlap - 1) * step + (i + 1) * step; };

    /* The payload arrives ordered by median, top ridge first, so drawing
       in order paints nearer (lower) ridges over farther ones. */
    var ridgeGroups = [], areaPaths = [];
    ridges.forEach(function (r, i) {
      var b = baseY(i);
      var c = color(r.group);
      var area = d3.area()
        .x(function (p) { return x(p.x); })
        .y0(b)
        .y1(function (p) { return b - amp(p.y); });
      var top = d3.line()
        .x(function (p) { return x(p.x); })
        .y(function (p) { return b - amp(p.y); });

      var grp = g.append("g");
      ridgeGroups.push(grp);

      grp.append("line")
        .attr("x1", 0).attr("x2", iw).attr("y1", b).attr("y2", b)
        .attr("stroke", ctx.theme.ink.grid);
      var areaPath = grp.append("path").datum(r.points)
        .attr("fill", c).attr("fill-opacity", 0.75)
        .attr("d", area);
      areaPaths.push(areaPath);
      /* Stroke only the top edge - outlining the closed area would draw
         hard vertical walls at both ends of the density. */
      grp.append("path").datum(r.points)
        .attr("fill", "none")
        .attr("stroke", d3.color(c).darker(0.7))
        .attr("stroke-width", 1.5)
        .attr("stroke-linejoin", "round")
        .attr("pointer-events", "none")
        .attr("d", top);
      grp.append("text")
        .attr("x", -10).attr("y", b - 4)
        .attr("text-anchor", "end")
        .attr("fill", ctx.theme.ink.secondary)
        .style("font-size", "12px")
        .text(r.group);

      /* Ridges rise into place from slightly below, top to bottom. */
      grp.attr("opacity", 0)
        .attr("transform", "translate(0," + step * 0.6 + ")")
        .transition().delay(Math.min(i * 70, 500)).duration(ctx.duration)
        .ease(d3.easeCubicOut)
        .attr("opacity", 1)
        .attr("transform", "translate(0,0)");

      areaPath
        .on("pointerenter pointermove", function (event) {
          ridgeGroups.forEach(function (other, j) {
            other.attr("opacity", j === i ? 1 : 0.3);
          });
          areaPath.attr("fill-opacity", 1);
          pv.showTip(ctx, event,
            "<b>" + pv.esc(r.group) + "</b><br>" +
            pv.swatchRow(c, "median", ctx.fmt(r.median)) + "<br>" +
            "n = <b>" + r.n + "</b>");
        })
        .on("pointerleave", function () {
          ridgeGroups.forEach(function (other) {
            other.attr("opacity", 1); });
          areaPath.attr("fill-opacity", 0.75);
          pv.hideTip(ctx);
        });
    });

    /* The shared x axis goes on last so it paints over the bottom ridge's
       baseline. */
    g.append("g").attr("transform", "translate(0," + ih + ")")
      .call(d3.axisBottom(x).ticks(Math.min(8, Math.floor(iw / 80)))
        .tickFormat(pv.fmtTick).tickSizeOuter(0))
      .call(function (s) { pv.styleAxis(s, ctx.theme, true); });
    pv.axisLabels(svg, ctx, m, iw, ih, ctx.x.xlab, null);
  };

})();
