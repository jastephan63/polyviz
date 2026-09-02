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
    /* Stacked modes draw one bar per category out of the segment bounds
       the R side pre-computed; everything below is the side-by-side
       layout, untouched. */
    if (ctx.x.stack === "stack" || ctx.x.stack === "percent") {
      return renderBarStacked(ctx);
    }
    var data = ctx.x.data;
    var grouped = data.length && data[0].series !== undefined;
    var cats = pv.uniq(data.map(function (d) { return d.x; }));

    /* Linked selection (pv_link): ctKeys is a row key per data row. Tag
       each bar's datum with its key, and let pv.keyOpacity dim the bars
       whose key the group-wide selection leaves out. */
    var keys = ctx.x.ctKeys || null;
    if (keys) { data.forEach(function (d, i) { d.key = String(keys[i]); }); }
    function baseOp(d) { return keys ? pv.keyOpacity(ctx, d.key, 1) : 1; }

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
      pv.buildLegend(ctx.header, seriesNames, color, ctx.theme,
        pv.textureLegend(ctx, seriesNames, color));
      ctx.height = Math.max(120, ctx.height - 26);
    }

    var m = { top: 12, right: 14, bottom: 52, left: pv.leftMargin(ctx) };
    var iw = ctx.width - m.left - m.right,
        ih = ctx.height - m.top - m.bottom;
    var svg = pv.baseSvg(ctx);
    /* Texture fills (pv_textures): hatch patterns in the plot's own
       defs replace the solid fills; tooltips keep the solid swatch. */
    var tex = pv.textureFill(ctx, svg,
      grouped ? seriesNames : ["value"], color);
    function fillOf(d) {
      var nm = grouped ? d.series : "value";
      return tex ? tex(nm) : color(nm);
    }
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
    /* An explicit ylim (the facet renderer sends one to share scales
       across panels) replaces the computed domain verbatim - no nice().
       The x axis is categorical, so there is no xlim to honour. */
    if (ctx.x.ylim) { y.domain(ctx.x.ylim); }

    pv.yGrid(g, y, iw, ctx.theme);
    g.append("g").attr("transform", "translate(0," + ih + ")")
      .call(d3.axisBottom(x0).tickSizeOuter(0))
      .call(function (s) { pv.styleAxis(s, ctx.theme, true); });
    g.append("g").call(d3.axisLeft(y).ticks(5).tickFormat(pv.fmtTick))
      .call(function (s) { pv.styleAxis(s, ctx.theme, false); });
    pv.axisLabels(svg, ctx, m, iw, ih, ctx.x.xlab, ctx.x.ylab);

    /* Annotation bands go under the bars, reference lines and labels over
       them. Neither layer may steal the bars' pointer events, so both are
       transparent to the pointer. gOver is appended after the bars exist,
       further down. */
    var gUnder = g.append("g").attr("pointer-events", "none");

    /* Each bar starts at zero height and grows up to its value, with a
       small stagger from left to right so the chart builds across. */
    var bars = g.selectAll("path.bar").data(data).enter().append("path")
      .attr("class", "bar")
      .attr("fill", fillOf)
      .attr("opacity", baseOp)
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

    /* A bar's datum as a plain object, for the Shiny round-trip. */
    function barDatum(d) {
      var out = { x: d.x, y: d.y };
      if (grouped) { out.series = d.series; }
      return out;
    }

    /* Hovering a bar dims the others and shows its exact value. Dimming
       never lifts a bar above its linked-selection opacity. */
    bars
      .on("pointerenter pointermove", function (event, d) {
        bars.attr("opacity", function (b) {
          return b === d ? baseOp(b) : Math.min(0.45, baseOp(b));
        });
        if (event.type === "pointerenter") {
          ctx.emit("hover", barDatum(d));
        }
        var head = grouped ?
          pv.esc(d.x) + " &middot; " + pv.esc(d.series) : pv.esc(d.x);
        pv.showTip(ctx, event, head + "<br>" +
          pv.swatchRow(color(grouped ? d.series : "value"),
                       ctx.x.ylab || "value", ctx.fmt(d.y)));
      })
      .on("pointerleave", function () {
        bars.attr("opacity", baseOp);
        pv.hideTip(ctx);
      })
      .on("click", function (event, d) {
        ctx.emit("click", barDatum(d));
      });

    /* Reference lines and annotation labels sit above the bars; the
       bands went into gUnder earlier. Trend fits are skipped here - a
       categorical x axis has no numeric positions to fit along. */
    var gOver = g.append("g").attr("pointer-events", "none");
    pv.drawAnnotations(ctx, gUnder, gOver, x0, y, iw, ih);
  };

  /* Horizontal bars: the answer to long category names. Categories run
     down the left in plain horizontal text, bars grow rightward from a
     zero baseline. Single-series only (the R side enforces it). */
  function renderBarH(ctx) {
    var data = ctx.x.data;
    /* Linked selection works here exactly as on vertical bars: tag each
       row with its ctKey and dim bars a selection leaves out. */
    var keys = ctx.x.ctKeys || null;
    if (keys) { data.forEach(function (d, i) { d.key = String(keys[i]); }); }
    function baseOp(d) { return keys ? pv.keyOpacity(ctx, d.key, 1) : 1; }
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
    /* Texture fill (pv_textures): the single series is slot 0. */
    var tex = pv.textureFill(ctx, svg, ["value"],
      function () { return ctx.theme.palette[0]; });
    var barFill = tex ? tex("value") : ctx.theme.palette[0];
    var g = svg.append("g").attr("transform",
      "translate(" + m.left + "," + m.top + ")");

    var yBand = d3.scaleBand()
      .domain(data.map(function (d) { return d.x; }))
      .range([0, ih]).paddingInner(0.3).paddingOuter(0.1);
    var x = d3.scaleLinear()
      .domain([0, d3.max(data, function (d) { return d.y; }) || 1])
      .nice().range([0, iw]);
    /* Horizontal bars draw the value along x, so an explicit ylim (the
       value limits) overrides this scale verbatim - no nice(). */
    if (ctx.x.ylim) { x.domain(ctx.x.ylim); }

    var xAxis = g.append("g").attr("transform", "translate(0," + ih + ")")
      .call(d3.axisBottom(x).ticks(Math.min(6, Math.floor(iw / 80)))
        .tickFormat(pv.fmtTick).tickSizeOuter(0));
    pv.styleAxis(xAxis, ctx.theme, true);
    var yAxis = g.append("g").call(d3.axisLeft(yBand).tickSize(0)
      .tickFormat(function (d) { return pv.truncate(d, maxChars); }));
    pv.styleAxis(yAxis, ctx.theme, false);

    /* Annotations are skipped on horizontal bars: the axes are swapped
       relative to every other cartesian chart, so a shared hline/vline
       vocabulary would land on the wrong axis and mislead. */
    var bars = g.selectAll("path.bar").data(data).enter().append("path")
      .attr("class", "bar")
      .attr("fill", barFill)
      .attr("opacity", baseOp)
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
        bars.attr("opacity", function (b) {
          return b === d ? baseOp(b) : Math.min(0.45, baseOp(b));
        });
        if (event.type === "pointerenter") {
          ctx.emit("hover", { x: d.x, y: d.y });
        }
        pv.showTip(ctx, event, pv.esc(d.x) + "<br>" +
          pv.swatchRow(ctx.theme.palette[0], ctx.x.ylab || "value",
                       ctx.fmt(d.y)));
      })
      .on("pointerleave", function () {
        bars.attr("opacity", baseOp);
        pv.hideTip(ctx);
      })
      .on("click", function (event, d) {
        ctx.emit("click", { x: d.x, y: d.y });
      });
  }

  /* ---------- stacked bars ---------- */

  /* What a stacked segment's label says: the share of its bar in
     percent mode, the value itself otherwise. */
  function segText(percent, d) {
    return percent ? d3.format(".0%")(d.share) : fmtVal(d.y);
  }

  /* Ink for text sitting inside a coloured segment: whichever of the
     theme's two extremes - primary ink or the surface - reads better on
     that fill, by WCAG contrast ratio from relative luminance. A fixed
     ink can't stay readable across eight hues in two modes. */
  function stackInkPicker(ctx) {
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
    return function (fill) {
      var L = lum(fill);
      var vsPrimary = (Math.max(L, lumPrimary) + 0.05) /
        (Math.min(L, lumPrimary) + 0.05);
      var vsSurface = (Math.max(L, lumSurface) + 0.05) /
        (Math.min(L, lumSurface) + 0.05);
      return vsPrimary >= vsSurface ?
        ctx.theme.ink.primary : ctx.theme.ink.surface;
    };
  }

  /* One segment's tooltip: the category, the series swatch with the
     exact value (plus its share in percent mode), and the bar's total -
     always the exact numbers, whatever the chart rounds. */
  function stackTip(ctx, color, percent, d) {
    var val = ctx.fmt(d.y);
    if (percent) {
      val += " (" + d3.format(".1%")(d.share) + ")";
    }
    return pv.esc(d.x) + "<br>" +
      pv.swatchRow(color(d.series), d.series, val) + "<br>" +
      "total: <b>" + ctx.fmt(d.total) + "</b>";
  }

  /* Shared setup for both stacked layouts: colours and legend as for
     grouped bars, then the vertical/horizontal split. The R side sent
     each row with its segment bounds (y0 to y1) already accumulated in
     stacking order, so both layouts only place rectangles. */
  function renderBarStacked(ctx) {
    var percent = ctx.x.stack === "percent";
    var data = ctx.x.data;
    var cats = pv.uniq(data.map(function (d) { return d.x; }));
    var seriesNames = pv.uniq(data.map(function (d) { return d.series; }));
    var color = d3.scaleOrdinal().domain(seriesNames)
      .range(ctx.theme.palette);
    pv.buildLegend(ctx.header, seriesNames, color, ctx.theme,
      pv.textureLegend(ctx, seriesNames, color));
    ctx.height = Math.max(120, ctx.height - 26);

    /* Linked selection (pv_link) works exactly as on grouped bars: tag
       each row with its ctKey and dim segments a selection leaves out. */
    var keys = ctx.x.ctKeys || null;
    if (keys) { data.forEach(function (d, i) { d.key = String(keys[i]); }); }

    /* A stack is one bar per category, so "auto" flips to horizontal on
       the same label-width test the single-series layout uses. */
    var estW = Math.max(50, ctx.width - 72);
    var step = estW / Math.max(1, cats.length);
    var widest = d3.max(cats, function (c) {
      return pv.textWidth(c, 11); }) || 0;
    var total = d3.sum(cats, function (c) { return pv.textWidth(c, 11); });
    var horizontal = opt(ctx.x.horizontal,
      total > 0.85 * estW || widest > 1.6 * step);

    /* The rounded data end belongs only to the outermost visible segment
       of each bar. The rows arrive in stacking order, so that is the
       last row of a category with any height. */
    var outermost = {};
    data.forEach(function (d) {
      if (d.y1 > d.y0) { outermost[d.x] = d; }
    });

    var shared = { percent: percent, data: data, cats: cats, color: color,
                   outermost: outermost, keys: keys };
    if (horizontal) { renderBarStackedH(ctx, shared); }
    else { renderBarStackedV(ctx, shared); }
  }

  function renderBarStackedV(ctx, s) {
    var data = s.data, cats = s.cats, color = s.color, percent = s.percent;
    function baseOp(d) { return s.keys ? pv.keyOpacity(ctx, d.key, 1) : 1; }

    var m = { top: 12, right: 14, bottom: 52, left: pv.leftMargin(ctx) };
    var iw = ctx.width - m.left - m.right,
        ih = ctx.height - m.top - m.bottom;
    var svg = pv.baseSvg(ctx);
    /* Texture fills (pv_textures), one hatch per series slot. */
    var tex = pv.textureFill(ctx, svg, color.domain(), color);
    function fillOf(d) { return tex ? tex(d.series) : color(d.series); }
    var g = svg.append("g").attr("transform",
      "translate(" + m.left + "," + m.top + ")");

    var x0 = d3.scaleBand().domain(cats).range([0, iw]).paddingInner(0.28)
      .paddingOuter(0.12);
    var y = d3.scaleLinear().range([ih, 0]);
    if (percent) {
      y.domain([0, 1]);
    } else {
      y.domain([0, d3.max(data, function (d) { return d.y1; }) || 1]).nice();
    }
    /* An explicit ylim (the facet renderer sends one to share scales
       across panels) replaces the computed domain verbatim - no nice(). */
    if (ctx.x.ylim) { y.domain(ctx.x.ylim); }

    pv.yGrid(g, y, iw, ctx.theme);
    g.append("g").attr("transform", "translate(0," + ih + ")")
      .call(d3.axisBottom(x0).tickSizeOuter(0))
      .call(function (sel) { pv.styleAxis(sel, ctx.theme, true); });
    g.append("g").call(d3.axisLeft(y).ticks(5)
        .tickFormat(percent ? d3.format(".0%") : pv.fmtTick))
      .call(function (sel) { pv.styleAxis(sel, ctx.theme, false); });
    pv.axisLabels(svg, ctx, m, iw, ih, ctx.x.xlab, ctx.x.ylab);

    /* Annotation bands go under the bars, reference lines and labels
       over them; gOver is appended after the bars exist, further down. */
    var gUnder = g.append("g").attr("pointer-events", "none");

    /* A segment's final pixels. Every segment below another gives up
       2px at its top - the surface-coloured gap between segments; the
       outermost keeps its exact top and the rounded data end. */
    function geom(d) {
      var isOuter = s.outermost[d.x] === d;
      var top = y(d.y1) + (isOuter ? 0 : 2);
      return { top: top, h: Math.max(0, y(d.y0) - top), r: isOuter ? 4 : 0 };
    }

    var bars = g.selectAll("path.bar").data(data).enter().append("path")
      .attr("class", "bar")
      .attr("fill", fillOf)
      .attr("opacity", baseOp)
      .attr("d", function (d) {
        return pv.topRoundedBar(x0(d.x), ih, x0.bandwidth(), 0, geom(d).r);
      });

    /* The whole stack grows out of the baseline together - every edge
       scaled toward the axis - with the same left-to-right stagger the
       side-by-side bars use. */
    bars.transition().duration(ctx.duration)
      .delay(function (d) { return Math.min(cats.indexOf(d.x) * 24, 600); })
      .ease(d3.easeCubicOut)
      .attrTween("d", function (d) {
        var gm = geom(d);
        var bx = x0(d.x), bw = x0.bandwidth();
        return function (t) {
          return pv.topRoundedBar(bx, ih - t * (ih - gm.top), bw,
            t * gm.h, gm.r);
        };
      });

    /* Value labels sit inside each segment big enough to carry its text
       (the share, in percent mode), and the category total goes over the
       bar's end when the stack shows raw values. "auto" matches the
       single-series rule: few enough bars, each wide enough. */
    var showVals = opt(ctx.x.valueLabels,
      cats.length <= 12 && x0.bandwidth() >= 34);
    if (showVals) {
      var inkFor = stackInkPicker(ctx);
      /* On a textured segment the text mostly sits on the lightened
         ground, so that is the colour the ink contrast is judged on. */
      var segInk = function (d) {
        return inkFor(tex ?
          pv.textureGround(color(d.series), ctx.theme) : color(d.series));
      };
      var labelled = data.filter(function (d) {
        return geom(d).h >= 15 &&
          pv.textWidth(segText(percent, d), 11) <= x0.bandwidth() - 6;
      });
      g.selectAll("text.val").data(labelled).enter().append("text")
        .attr("class", "val")
        .attr("x", function (d) { return x0(d.x) + x0.bandwidth() / 2; })
        .attr("y", function (d) {
          var gm = geom(d);
          return gm.top + gm.h / 2;
        })
        .attr("text-anchor", "middle")
        .attr("dominant-baseline", "middle")
        .attr("fill", segInk)
        .style("font-size", "11px")
        .style("font-variant-numeric", "tabular-nums")
        .style("pointer-events", "none")
        .style("opacity", 0)
        .text(function (d) { return segText(percent, d); })
        .transition().delay(ctx.duration).duration(200)
        .style("opacity", 1);
      if (!percent) {
        var tops = cats.filter(function (c) { return s.outermost[c]; })
          .map(function (c) { return s.outermost[c]; });
        g.selectAll("text.total").data(tops).enter().append("text")
          .attr("class", "total")
          .attr("x", function (d) { return x0(d.x) + x0.bandwidth() / 2; })
          .attr("y", function (d) { return y(d.total) - 5; })
          .attr("text-anchor", "middle")
          .attr("fill", ctx.theme.ink.secondary)
          .style("font-size", "11px")
          .style("font-variant-numeric", "tabular-nums")
          .style("opacity", 0)
          .text(function (d) { return fmtVal(d.total); })
          .transition().delay(ctx.duration).duration(200)
          .style("opacity", 1);
      }
    }

    /* A segment's datum as a plain object, for the Shiny round-trip. */
    function barDatum(d) { return { x: d.x, y: d.y, series: d.series }; }

    bars
      .on("pointerenter pointermove", function (event, d) {
        bars.attr("opacity", function (b) {
          return b === d ? baseOp(b) : Math.min(0.45, baseOp(b));
        });
        if (event.type === "pointerenter") {
          ctx.emit("hover", barDatum(d));
        }
        pv.showTip(ctx, event, stackTip(ctx, color, percent, d));
      })
      .on("pointerleave", function () {
        bars.attr("opacity", baseOp);
        pv.hideTip(ctx);
      })
      .on("click", function (event, d) {
        ctx.emit("click", barDatum(d));
      });

    var gOver = g.append("g").attr("pointer-events", "none");
    pv.drawAnnotations(ctx, gUnder, gOver, x0, y, iw, ih);
  }

  /* Horizontal stacked bars: categories down the left, segments growing
     rightward. Annotations are skipped, exactly as on the single-series
     horizontal layout - the axes are swapped relative to every other
     cartesian chart. */
  function renderBarStackedH(ctx, s) {
    var data = s.data, cats = s.cats, color = s.color, percent = s.percent;
    function baseOp(d) { return s.keys ? pv.keyOpacity(ctx, d.key, 1) : 1; }

    /* The label margin logic of the single-series horizontal layout:
       grow with the longest category name, never eat the plot. The
       right margin holds the totals in raw mode; percent bars all end
       at 100%, which only needs room for the last tick label. */
    var longest = d3.max(cats, function (c) {
      return pv.textWidth(c, 11); }) || 30;
    var m = { top: 8, right: percent ? 24 : 46, bottom: 34,
              left: Math.max(70, Math.min(190, 0.45 * ctx.width,
                0.5 * ctx.width - 46, 22 + longest)) };
    var maxChars = Math.max(4, Math.floor((m.left - 12) / 6.9));
    var iw = ctx.width - m.left - m.right,
        ih = ctx.height - m.top - m.bottom;
    var svg = pv.baseSvg(ctx);
    /* Texture fills (pv_textures), one hatch per series slot. */
    var tex = pv.textureFill(ctx, svg, color.domain(), color);
    function fillOf(d) { return tex ? tex(d.series) : color(d.series); }
    var g = svg.append("g").attr("transform",
      "translate(" + m.left + "," + m.top + ")");

    var yBand = d3.scaleBand().domain(cats).range([0, ih])
      .paddingInner(0.3).paddingOuter(0.1);
    var x = d3.scaleLinear().range([0, iw]);
    if (percent) {
      x.domain([0, 1]);
    } else {
      x.domain([0, d3.max(data, function (d) { return d.y1; }) || 1]).nice();
    }
    /* Horizontal bars draw the value along x, so an explicit ylim (the
       value limits) overrides this scale verbatim - no nice(). */
    if (ctx.x.ylim) { x.domain(ctx.x.ylim); }

    var xAxis = g.append("g").attr("transform", "translate(0," + ih + ")")
      .call(d3.axisBottom(x).ticks(Math.min(6, Math.floor(iw / 80)))
        .tickFormat(percent ? d3.format(".0%") : pv.fmtTick)
        .tickSizeOuter(0));
    pv.styleAxis(xAxis, ctx.theme, true);
    var yAxis = g.append("g").call(d3.axisLeft(yBand).tickSize(0)
      .tickFormat(function (d) { return pv.truncate(d, maxChars); }));
    pv.styleAxis(yAxis, ctx.theme, false);

    /* A segment's final pixels: every segment with another after it
       gives up 2px at its right edge - the surface-coloured gap - and
       only the outermost keeps the rounded data end. */
    function geom(d) {
      var isOuter = s.outermost[d.x] === d;
      var left = x(d.y0);
      return { left: left,
               w: Math.max(0, x(d.y1) - (isOuter ? 0 : 2) - left),
               r: isOuter ? 4 : 0 };
    }

    var bars = g.selectAll("path.bar").data(data).enter().append("path")
      .attr("class", "bar")
      .attr("fill", fillOf)
      .attr("opacity", baseOp)
      .attr("d", function (d) {
        return pv.rightRoundedBar(0, yBand(d.x), 0, yBand.bandwidth(),
          geom(d).r);
      });

    /* The whole bar stretches out of the axis together, top to bottom. */
    bars.transition().duration(ctx.duration)
      .delay(function (d) { return Math.min(cats.indexOf(d.x) * 22, 500); })
      .ease(d3.easeCubicOut)
      .attrTween("d", function (d) {
        var gm = geom(d);
        var by = yBand(d.x), bh = yBand.bandwidth();
        return function (t) {
          return pv.rightRoundedBar(t * gm.left, by, t * gm.w, bh, gm.r);
        };
      });

    /* Segment labels where they fit, and the category total at the
       bar's end when the stack shows raw values. Labels default on, as
       on every horizontal bar - only an explicit FALSE turns them off. */
    if (opt(ctx.x.valueLabels, true)) {
      var inkFor = stackInkPicker(ctx);
      /* On a textured segment the text mostly sits on the lightened
         ground, so that is the colour the ink contrast is judged on. */
      var segInk = function (d) {
        return inkFor(tex ?
          pv.textureGround(color(d.series), ctx.theme) : color(d.series));
      };
      var labelled = data.filter(function (d) {
        return yBand.bandwidth() >= 12 &&
          pv.textWidth(segText(percent, d), 11) <= geom(d).w - 8;
      });
      g.selectAll("text.val").data(labelled).enter().append("text")
        .attr("class", "val")
        .attr("x", function (d) {
          var gm = geom(d);
          return gm.left + gm.w / 2;
        })
        .attr("y", function (d) {
          return yBand(d.x) + yBand.bandwidth() / 2;
        })
        .attr("text-anchor", "middle")
        .attr("dominant-baseline", "middle")
        .attr("fill", segInk)
        .style("font-size", "11px")
        .style("font-variant-numeric", "tabular-nums")
        .style("pointer-events", "none")
        .style("opacity", 0)
        .text(function (d) { return segText(percent, d); })
        .transition().delay(ctx.duration).duration(200)
        .style("opacity", 1);
      if (!percent) {
        var ends = cats.filter(function (c) { return s.outermost[c]; })
          .map(function (c) { return s.outermost[c]; });
        g.selectAll("text.total").data(ends).enter().append("text")
          .attr("class", "total")
          .attr("x", function (d) { return x(d.total) + 6; })
          .attr("y", function (d) {
            return yBand(d.x) + yBand.bandwidth() / 2;
          })
          .attr("dominant-baseline", "middle")
          .attr("fill", ctx.theme.ink.secondary)
          .style("font-size", "11px")
          .style("font-variant-numeric", "tabular-nums")
          .style("opacity", 0)
          .text(function (d) { return fmtVal(d.total); })
          .transition().delay(ctx.duration).duration(200)
          .style("opacity", 1);
      }
    }

    /* A segment's datum as a plain object, for the Shiny round-trip. */
    function barDatum(d) { return { x: d.x, y: d.y, series: d.series }; }

    bars
      .on("pointerenter pointermove", function (event, d) {
        bars.attr("opacity", function (b) {
          return b === d ? baseOp(b) : Math.min(0.45, baseOp(b));
        });
        if (event.type === "pointerenter") {
          ctx.emit("hover", barDatum(d));
        }
        pv.showTip(ctx, event, stackTip(ctx, color, percent, d));
      })
      .on("pointerleave", function () {
        bars.attr("opacity", baseOp);
        pv.hideTip(ctx);
      })
      .on("click", function (event, d) {
        ctx.emit("click", barDatum(d));
      });
  }

  /* ---------- line ---------- */

  pvRenderers.line = function (ctx) {
    var xtype = ctx.x.xtype;
    var parse = xtype === "date" ? d3.timeParse("%Y-%m-%d") : null;
    /* xkey keeps the value exactly as R sent it (dates become Date
       objects in x), so interaction payloads can report the original. */
    var data = ctx.x.data.map(function (d) {
      return {
        x: xtype === "date" ? parse(d.x) : d.x,
        xkey: d.x, y: d.y, series: d.series
      };
    });
    /* Linked selection is skipped on lines: a path has no per-row
       identity, so there is nothing for a row-keyed selection to dim. */
    var seriesNames = pv.uniq(data.map(function (d) { return d.series; }));
    var color = d3.scaleOrdinal().domain(seriesNames)
      .range(ctx.theme.palette);
    /* More series than the palette has hues would silently recycle
       colours, so identity by colour is lost anyway. Such charts switch
       to a spaghetti treatment instead: every line in the muted ink at
       low opacity, and hover lifts one line at a time in the accent
       colour with its name in the tooltip. */
    var spaghetti = seriesNames.length > ctx.theme.palette.length;
    var accent = ctx.theme.palette[0];
    /* "auto" keeps the old rule - a legend only when a real series
       mapping produced more than one line; TRUE and FALSE override it.
       A spaghetti chart never gets one: with every line in the same ink
       there is no colour for a legend to explain. */
    var showLegend = !spaghetti && opt(ctx.x.legend,
      ctx.x.showLegend && seriesNames.length > 1);
    if (showLegend && seriesNames.length) {
      pv.buildLegend(ctx.header, seriesNames, color, ctx.theme);
      ctx.height = Math.max(120, ctx.height - 26);
    }

    /* Brush-to-zoom (zoom = TRUE from R, continuous x only). The strip
       gets its room at the bottom before the layout is measured, and an
       active brush window - kept on the widget element itself, so it
       survives the full re-renders a resize or theme flip triggers -
       narrows the x domain further down through the same
       explicit-domain path a facet xlim uses. Inside a facet panel (the
       payload clone carries the grid's subtype) the strip is dropped
       quietly: the panels share axes, which per-panel brushes would
       break. */
    var zoomOn = ctx.x.zoom === true && xtype !== "category" &&
      ctx.x.subtype == null;
    var zoomDomain = zoomOn ? (ctx.el.__pvZoomX || null) : null;
    if (zoomOn) {
      ctx.height = Math.max(120,
        ctx.height - pv.ZOOM_STRIP_H - pv.ZOOM_STRIP_GAP);
    }

    var directLabels = seriesNames.length > 1 && seriesNames.length <= 4;
    var m = { top: 12, right: directLabels ? 90 : 24, bottom: 52, left: pv.leftMargin(ctx) };
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
    /* Explicit limits (the facet renderer shares scales this way)
       replace the computed domains exactly as given - no nice(). Dates
       arrive as ISO strings and go through the same parser as the data. */
    if (ctx.x.ylim) { y.domain(ctx.x.ylim); }
    if (ctx.x.xlim && xtype !== "category") {
      xScale.domain(xtype === "date" ?
        ctx.x.xlim.map(function (v) { return parse(v); }) : ctx.x.xlim);
    }
    /* The zoom strip needs the full extent even while the main panel
       shows a window, so remember the domain before the brush narrows
       it. Date windows travel as millisecond numbers. */
    var fullX = zoomOn ? xScale.domain() : null;
    if (zoomDomain) {
      xScale.domain(xtype === "date" ?
        zoomDomain.map(function (v) { return new Date(v); }) : zoomDomain);
    }

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

    /* Annotation bands go under the lines; gOver (reference lines,
       labels, trend fits) is appended once the lines exist, further
       down. Both ignore the pointer so the crosshair keeps working. */
    var gUnder = g.append("g").attr("pointer-events", "none");

    var bySeries = seriesNames.map(function (nm) {
      return { name: nm, points: data.filter(function (d) {
        return d.series === nm; }) };
    });
    /* R validated `curve` to one of three names; the default "linear"
       is exactly what d3.line() draws on its own, so old payloads
       without the field come out unchanged. Monotone smooths without
       overshooting a data point; step holds each value until the next. */
    var curveFactory = ctx.x.curve === "monotone" ? d3.curveMonotoneX :
      ctx.x.curve === "step" ? d3.curveStep : d3.curveLinear;
    var line = d3.line()
      .x(function (d) { return xScale(d.x); })
      .y(function (d) { return y(d.y); })
      .curve(curveFactory);

    /* A brushed window shows only a slice of the data, so the drawn
       lines and markers are clipped to the plot; without zoom the
       domain covers everything and no clip exists at all. */
    var drawLayer = g;
    if (zoomOn) {
      var zoomClip = "pv-zoom-clip-" + Math.floor(Math.random() * 1e9);
      svg.append("clipPath").attr("id", zoomClip)
        .append("rect").attr("width", iw).attr("height", ih);
      drawLayer = g.append("g").attr("clip-path", "url(#" + zoomClip + ")");
    }
    /* Spaghetti lines get a layer of their own, so hover can raise one
       line above its siblings without lifting it over the crosshair, the
       marker dots, or the pointer surface drawn later. */
    var lineLayer = spaghetti ? drawLayer.append("g") : drawLayer;
    var pathBySeries = {};
    bySeries.forEach(function (s) {
      var path = lineLayer.append("path").datum(s.points)
        .attr("fill", "none")
        .attr("stroke", spaghetti ? ctx.theme.ink.muted : color(s.name))
        .attr("stroke-width", spaghetti ? 1.5 : 2)
        .attr("stroke-linejoin", "round")
        .attr("d", line);
      if (spaghetti) {
        path.attr("stroke-opacity", 0.4);
        pathBySeries[s.name] = path;
      }
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

    /* Observation markers (show_points): the connected-scatter look.
       FALSE is the shipped default, so a payload from before the flag
       existed stays markerless; "auto" turns the dots on only when each
       observation deserves to be seen on its own - no series longer
       than 30 points and roughly 12px between neighbours. Spaghetti
       charts never draw them: their lines share one muted ink, so dots
       would only add noise. The dots sit under the crosshair overlay,
       which keeps the tooltip and its snapping exactly as before. */
    var maxLen = d3.max(bySeries, function (s) {
      return s.points.length; }) || 0;
    var showPoints = !spaghetti && (ctx.x.showPoints == null ? false :
      opt(ctx.x.showPoints,
        maxLen <= 30 && iw / Math.max(1, maxLen - 1) >= 12));
    if (showPoints) {
      var marks = drawLayer.selectAll("circle.obs").data(data).enter()
        .append("circle")
        .attr("class", "obs")
        .attr("cx", function (d) { return xScale(d.x); })
        .attr("cy", function (d) { return y(d.y); })
        .attr("r", 3.5)
        .attr("fill", function (d) { return color(d.series); })
        .attr("stroke", ctx.theme.ink.surface).attr("stroke-width", 2);
      /* The dots appear left to right, keeping pace with the draw-in. */
      if (ctx.duration > 0) {
        marks.attr("opacity", 0)
          .transition().duration(150)
          .delay(function (d) {
            return ctx.duration * (xScale(d.x) / Math.max(1, iw));
          })
          .attr("opacity", 1);
      }
    }

    /* The one spaghetti line the pointer currently singles out. Moving to
       another settles the old line back into the grey bundle and brings
       the new one forward in the accent colour. */
    var hotSeries = null;
    function setHot(nm) {
      if (nm === hotSeries) return;
      if (hotSeries != null) {
        pathBySeries[hotSeries]
          .attr("stroke", ctx.theme.ink.muted)
          .attr("stroke-width", 1.5)
          .attr("stroke-opacity", 0.4);
      }
      hotSeries = nm;
      if (nm != null) {
        pathBySeries[nm].raise()
          .attr("stroke", accent)
          .attr("stroke-width", 2)
          .attr("stroke-opacity", 1);
      }
    }

    /* Direct labels at the line ends. Series that finish near the same
       value would overlap, so sort the label positions and push any pair
       closer than one line of text apart. */
    if (directLabels) {
      var ends = bySeries
        .map(function (s) {
          /* Under a brushed window a series' true end can sit out of
             view, so the label follows the last visible point; with the
             full domain every point is visible and nothing changes. */
          var pts = s.points.filter(function (d) {
            var px = xScale(d.x);
            return px >= -0.5 && px <= iw + 0.5;
          });
          if (!pts.length) return null;
          var last = pts[pts.length - 1];
          return { name: s.name, x: xScale(last.x), y: y(last.y),
                   ly: y(last.y) };
        })
        .filter(function (e) { return e; })
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

    /* Reference lines and annotation labels sit above the lines. Trend
       fits (pv_trend) go into the same pointer-transparent group, so the
       confidence ribbon can never swallow pointer events - and only on a
       truly numeric x axis, because R fits over numbers; category and
       date axes have no numeric positions for the fitted points. The
       trend layer is clipped to the plot: a wide ribbon would otherwise
       spill past the axes at the chart's edges. */
    var gOver = g.append("g").attr("pointer-events", "none");
    pv.drawAnnotations(ctx, gUnder, gOver, xScale, y, iw, ih);
    if (xtype === "number") {
      var clipId = "pv-trend-clip-" + Math.floor(Math.random() * 1e9);
      svg.append("clipPath").attr("id", clipId)
        .append("rect").attr("width", iw).attr("height", ih);
      pv.drawTrends(ctx,
        gOver.append("g").attr("clip-path", "url(#" + clipId + ")"),
        xScale, y);
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
    /* Positions a brushed window pushed out of the plot can't be
       crosshair targets; with the full domain everything is in range. */
    var xVals = pv.uniq(data.map(function (d) { return +xScale(d.x); }))
      .filter(function (v) { return v >= -0.5 && v <= iw + 0.5; })
      .sort(function (a, b) { return a - b; });

    /* Every point whose x position is nearest the pointer - the same
       lookup feeds the crosshair tooltip and the click payload. */
    function hitsAt(px) {
      if (!xVals.length) return [];
      var nearest = xVals.reduce(function (a, b) {
        return Math.abs(b - px) < Math.abs(a - px) ? b : a;
      });
      return data.filter(function (d) {
        return Math.abs(xScale(d.x) - nearest) < 0.5;
      });
    }

    g.append("rect")
      .attr("width", iw).attr("height", ih)
      .attr("fill", "transparent")
      .on("pointermove", function (event) {
        var p = d3.pointer(event, this);
        var hits = hitsAt(p[0]);
        if (!hits.length) return;
        var nearest = xScale(hits[0].x);
        cross.attr("x1", nearest).attr("x2", nearest).attr("opacity", 1);
        if (spaghetti) {
          /* Dozens of tooltip rows would be noise. Pick the one line
             nearest the pointer vertically, lift it, and report it
             alone - name, dot, and value. */
          var best = hits[0];
          hits.forEach(function (d) {
            if (Math.abs(y(d.y) - p[1]) < Math.abs(y(best.y) - p[1])) {
              best = d;
            }
          });
          setHot(best.series);
          var dotSel = dots.selectAll("circle").data([best]);
          dotSel.enter().append("circle").attr("r", 4)
            .attr("stroke", ctx.theme.ink.surface).attr("stroke-width", 2)
            .merge(dotSel)
            .attr("cx", xScale(best.x)).attr("cy", y(best.y))
            .attr("fill", accent);
          dotSel.exit().remove();
          var bestLabel = xtype === "date" ?
            d3.timeFormat("%b %e, %Y")(best.x) : best.x;
          pv.showTip(ctx, event, "<b>" + pv.esc(bestLabel) + "</b><br>" +
            pv.swatchRow(accent, best.series, ctx.fmt(best.y)));
          return;
        }
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
        if (spaghetti) { setHot(null); }
        pv.hideTip(ctx);
      })
      .on("click", function (event) {
        /* A click reports the crosshair position: the x value under the
           pointer and every series' value there. */
        var hits = hitsAt(d3.pointer(event, this)[0]);
        if (!hits.length) return;
        ctx.emit("click", {
          x: hits[0].xkey,
          values: hits.map(function (d) {
            return { series: d.series, y: d.y };
          })
        });
      });

    /* The context strip, below everything else and clear of the source
       line (ctx.height already excludes the footer). Its margins match
       the main panel's, so the brush lines up under the plot. */
    if (zoomOn) {
      pv.zoomStrip(ctx, {
        left: m.left, right: m.right, xtype: xtype,
        extent: fullX, zoom: zoomDomain,
        draw: function (sg, sx, siw, sih) {
          /* The miniature: every series as a 1px muted line over the
             full range - enough shape to aim a brush at, no more. */
          var sy = d3.scaleLinear().domain(y.domain()).range([sih, 0]);
          var mini = d3.line()
            .x(function (d) { return sx(d.x); })
            .y(function (d) { return sy(d.y); })
            .curve(curveFactory);
          bySeries.forEach(function (s) {
            sg.append("path").datum(s.points)
              .attr("fill", "none")
              .attr("stroke", ctx.theme.ink.muted)
              .attr("stroke-width", 1)
              .attr("stroke-opacity", 0.55)
              .attr("d", mini);
          });
        }
      });
    }
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
       opacity, beyond that fade with the square root of the count, and
       past ~640 points (where that curve reaches 0.3) keep easing down
       on a gentler curve - a 50k cloud lands near 0.08 - so heavily
       overplotted cores still show their density structure. */
    var nPts = data.length;
    var baseOpacity = nPts <= 150 ? 0.62 :
      nPts <= 640 ? 0.62 * Math.sqrt(150 / nPts) :
      0.3 * Math.pow(640 / nPts, 0.3);

    /* Linked selection (pv_link): ctKeys is a row key per point. Points a
       group-wide selection leaves out drop to a faint ghost opacity. */
    var keys = ctx.x.ctKeys || null;
    if (keys) { data.forEach(function (d, i) { d.key = String(keys[i]); }); }
    function ptOp(d) {
      return keys ? pv.keyOpacity(ctx, d.key, baseOpacity) : baseOpacity;
    }

    var m = { top: 12, right: 24, bottom: 52, left: pv.leftMargin(ctx) };
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
    /* Explicit limits (the facet renderer shares scales this way)
       replace the computed domains exactly as given - no nice(). */
    if (ctx.x.xlim) { xScale.domain(ctx.x.xlim); }
    if (ctx.x.ylim) { y.domain(ctx.x.ylim); }
    /* The default dot slims down in huge clouds too: past ~10k points it
       shrinks smoothly toward 2.5px at 50k, so each mark stops being
       mostly overlap. Its surface-coloured halo thins away on the same
       curve - an opaque ring would paint over the neighbours the faded
       fill is meant to let through. An explicit size mapping is the
       user's own choice of radius and keeps its scale untouched. */
    var autoR = nPts <= 10000 ? 4.5 :
      Math.max(2.5, 4.5 * Math.pow(10000 / nPts, 0.37));
    var ptStroke = Math.max(0, (autoR - 2.5) / 2);
    var r = hasSize ?
      d3.scaleSqrt()
        .domain(d3.extent(data, function (d) { return d.size; }))
        .range([3.5, 13]) :
      function () { return autoR; };

    pv.yGrid(g, y, iw, ctx.theme);
    g.append("g").attr("transform", "translate(0," + ih + ")")
      .call(d3.axisBottom(xScale).ticks(Math.min(8, Math.floor(iw / 80)))
        .tickFormat(pv.fmtTick).tickSizeOuter(0))
      .call(function (s) { pv.styleAxis(s, ctx.theme, true); });
    g.append("g").call(d3.axisLeft(y).ticks(5).tickFormat(pv.fmtTick))
      .call(function (s) { pv.styleAxis(s, ctx.theme, false); });
    pv.axisLabels(svg, ctx, m, iw, ih, ctx.x.xlab, ctx.x.ylab);

    /* Annotation bands go under the points; gOver (reference lines,
       labels, trend fits) is appended after the points, further down. */
    var gUnder = g.append("g").attr("pointer-events", "none");

    /* Drag-to-select, when the chart carries row keys: the brush layer
       goes in BEFORE the points, so the points stay on top and keep
       their hover events, while drags started on empty plot still reach
       the brush surface underneath. */
    if (keys) {
      var brush = d3.brush().extent([[0, 0], [iw, ih]])
        .on("end", function (event) {
          /* A click (or an emptied brush) clears the selection. */
          if (!event.selection) { ctx.select([]); return; }
          var s = event.selection;
          var inside = [];
          data.forEach(function (d) {
            var px = xScale(d.x), py = y(d.y);
            if (px >= s[0][0] && px <= s[1][0] &&
                py >= s[0][1] && py <= s[1][1]) {
              inside.push(d.key);
            }
          });
          ctx.select(inside);
        });
      g.append("g").attr("class", "brush").call(brush);
    }

    var pts = g.selectAll("circle.pt").data(data).enter().append("circle")
      .attr("class", "pt")
      .attr("cx", function (d) { return xScale(d.x); })
      .attr("cy", function (d) { return y(d.y); })
      .attr("r", 0)
      .attr("fill", function (d) {
        return hasSeries ? color(d.series) : ctx.theme.palette[0];
      })
      .attr("fill-opacity", ptOp)
      .attr("stroke", ctx.theme.ink.surface).attr("stroke-width", ptStroke);

    pts.transition().duration(ctx.duration)
      .delay(function (d, i) { return Math.min(i * 6, 400); })
      .attr("r", function (d) { return hasSize ? r(d.size) : r(); });

    /* A point's datum as a plain object, for the Shiny round-trip. */
    function ptDatum(d) {
      var out = { x: d.x, y: d.y };
      if (hasSeries) { out.series = d.series; }
      if (hasSize) { out.size = d.size; }
      if (d.label !== undefined) { out.label = d.label; }
      if (keys) { out.key = d.key; }
      return out;
    }

    pts
      .on("pointerenter pointermove", function (event, d) {
        /* Lift the hovered point to full opacity so it reads clearly
           even inside a dense, faded cloud. */
        d3.select(this)
          .attr("fill-opacity", 1)
          .attr("stroke-width", 2)
          .attr("r", (hasSize ? r(d.size) : r()) * 1.35);
        if (event.type === "pointerenter") {
          ctx.emit("hover", ptDatum(d));
        }
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
          .attr("fill-opacity", ptOp(d))
          .attr("stroke-width", ptStroke)
          .attr("r", hasSize ? r(d.size) : r());
        pv.hideTip(ctx);
      })
      .on("click", function (event, d) {
        ctx.emit("click", ptDatum(d));
      });

    /* Reference lines and annotation labels above the points, and trend
       fits (pv_trend) into the same pointer-transparent group so the
       confidence ribbon can never swallow point hovers. Both scatter
       axes are numeric, so trends always apply here. The trend layer is
       clipped to the plot: a wide ribbon would otherwise spill past the
       axes at the chart's edges. */
    var gOver = g.append("g").attr("pointer-events", "none");
    pv.drawAnnotations(ctx, gUnder, gOver, xScale, y, iw, ih);
    var clipId = "pv-trend-clip-" + Math.floor(Math.random() * 1e9);
    svg.append("clipPath").attr("id", clipId)
      .append("rect").attr("width", iw).attr("height", ih);
    pv.drawTrends(ctx,
      gOver.append("g").attr("clip-path", "url(#" + clipId + ")"),
      xScale, y);
  };

})();
