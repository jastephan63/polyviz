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

    /* The spaghetti hover singles one line out of the bundle, and that
       pick is true 2D nearest-neighbour (d3.Delaunay) over every
       visible point: the pointer lifts the line whose observation is
       genuinely closest, instead of first snapping to an x column and
       only then comparing heights - which is what makes a dense
       crosshair feel precise. The index is built once per render,
       never per pointer move. */
    var hoverPts = null, hoverDelaunay = null;
    if (spaghetti) {
      hoverPts = data.filter(function (d) {
        var px = xScale(d.x);
        return px >= -0.5 && px <= iw + 0.5;
      });
      hoverDelaunay = hoverPts.length ? d3.Delaunay.from(hoverPts,
        function (d) { return xScale(d.x); },
        function (d) { return y(d.y); }) : null;
    }

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
        if (spaghetti) {
          /* Dozens of tooltip rows would be noise. The Delaunay index
             hands over the one nearest observation; lift its line and
             report it alone - name, dot, and value - with the
             crosshair snapped to that observation's x. */
          if (!hoverDelaunay) return;
          var best = hoverPts[hoverDelaunay.find(p[0], p[1])];
          var bx = xScale(best.x);
          cross.attr("x1", bx).attr("x2", bx).attr("opacity", 1);
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
        var hits = hitsAt(p[0]);
        if (!hits.length) return;
        var nearest = xScale(hits[0].x);
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
    /* The density treatments (density = TRUE / "contours" / "hex" from
       R) replace the point marks entirely, and they are already the
       aggregate view - so they also win over any canvas request. R
       normalises "contours" to TRUE, but accept the spelling here too.
       Everything below draws points. */
    if (ctx.x.density === true || ctx.x.density === "contours") {
      return renderScatterDensity(ctx);
    }
    if (ctx.x.density === "hex") { return renderScatterHex(ctx); }
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

    /* A point's datum as a plain object, for the Shiny round-trip. */
    function ptDatum(d) {
      var out = { x: d.x, y: d.y };
      if (hasSeries) { out.series = d.series; }
      if (hasSize) { out.size = d.size; }
      if (d.label !== undefined) { out.label = d.label; }
      if (keys) { out.key = d.key; }
      return out;
    }

    /* One point's tooltip rows - shared by the SVG and canvas marks, so
       the two modes always read the same on hover. */
    function tipHtml(d) {
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
      return rows.join("<br>");
    }

    /* Canvas rendering (#9). "auto" moves the point marks onto a
       <canvas> past 8000 points - about where one SVG node per point
       starts to drag on mid-range hardware (tens of thousands of DOM
       elements to build, style, and hit-test), while a canvas draws the
       same cloud in a single pass. Everything else - axes, grid,
       labels, annotations, trends, brush, chrome - stays SVG. TRUE and
       FALSE from R override the threshold. */
    var useCanvas = opt(ctx.x.canvas, nPts > 8000);
    if (useCanvas) {
      renderScatterCanvas(ctx, {
        data: data, hasSize: hasSize, keys: keys, ptOp: ptOp,
        m: m, iw: iw, ih: ih, svg: svg, g: g, gUnder: gUnder,
        xScale: xScale, y: y, autoR: autoR, ptStroke: ptStroke, r: r,
        ptDatum: ptDatum, tipHtml: tipHtml,
        fillOf: function (d) {
          return hasSeries ? color(d.series) : ctx.theme.palette[0];
        }
      });
      return;
    }

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

    /* Hover targeting is nearest-neighbour (d3.Delaunay): wherever the
       pointer sits in the plot, the nearest mark owns it - no more
       pixel-perfect aim on sparse clouds. The index is built once per
       render, never per pointer move; the marks themselves carry no
       listeners, and nothing visible changes - the hover lift and the
       tooltip are exactly the ones the per-mark listeners used to run. */
    var delaunay = d3.Delaunay.from(data,
      function (d) { return xScale(d.x); },
      function (d) { return y(d.y); });
    var ptNodes = pts.nodes();
    var hoveredIdx = -1;

    /* The nearest mark's index, or -1 when the pointer is outside the
       plot panel (bubbled events can arrive from the axes below it). */
    function nearestIdx(p) {
      if (p[0] < 0 || p[0] > iw || p[1] < 0 || p[1] > ih) { return -1; }
      var i = delaunay.find(p[0], p[1]);
      return i == null || i < 0 ? -1 : i;
    }

    /* Settles the old mark back into the cloud and lifts the new one to
       full opacity so it reads clearly even inside a dense, faded
       cloud - the same lift as ever, just driven from one place. */
    function setHover(i) {
      if (i === hoveredIdx) { return; }
      if (hoveredIdx >= 0) {
        var od = data[hoveredIdx];
        d3.select(ptNodes[hoveredIdx])
          .attr("fill-opacity", ptOp(od))
          .attr("stroke-width", ptStroke)
          .attr("r", hasSize ? r(od.size) : r());
      }
      hoveredIdx = i;
      if (i >= 0) {
        var d = data[i];
        d3.select(ptNodes[i])
          .attr("fill-opacity", 1)
          .attr("stroke-width", 2)
          .attr("r", (hasSize ? r(d.size) : r()) * 1.35);
        ctx.emit("hover", ptDatum(d));
      }
    }

    function onMove(event) {
      var i = nearestIdx(d3.pointer(event, g.node()));
      setHover(i);
      if (i >= 0) { pv.showTip(ctx, event, tipHtml(data[i])); }
      else { pv.hideTip(ctx); }
    }
    function onLeave() {
      setHover(-1);
      pv.hideTip(ctx);
    }
    function onClick(event) {
      var i = nearestIdx(d3.pointer(event, g.node()));
      if (i >= 0) { ctx.emit("click", ptDatum(data[i])); }
    }

    if (keys) {
      /* The brush overlay (drawn before the marks) already blankets the
         plot and must keep owning drags, so the hover handlers listen
         on the plot group and catch the bubbled events instead of
         putting a surface of their own above the brush. The brush
         suppresses the click a finished drag would leave behind, so a
         plain click still both clears the selection and reports the
         nearest mark. */
      g.on("pointermove", onMove)
        .on("pointerleave", onLeave)
        .on("click", onClick);
    } else {
      /* No brush to keep clear of: a transparent surface over the plot
         does the listening, exactly like the line's crosshair rect. */
      g.append("rect")
        .attr("width", iw).attr("height", ih)
        .attr("fill", "transparent")
        .on("pointermove", onMove)
        .on("pointerleave", onLeave)
        .on("click", onClick);
    }

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

  /* The canvas marks layer of the scatter. The caller (the scatter
     renderer above) has already drawn the whole SVG substrate - grid,
     axes, labels, and the under-annotation bands - and hands over its
     scales and point styling. This helper adds two layers on top of
     that svg, in stacking order:

       1. a <canvas> pinned exactly over the plot area, carrying every
          point mark, drawn at devicePixelRatio resolution so retina
          screens stay crisp;
       2. an overlay <svg> of the same footprint as the main one,
          holding everything that belongs above the points - the brush,
          the hover marker, reference lines, annotation labels, and
          trend fits - so a trend still draws over the cloud exactly as
          it does over SVG points.

     Hovering has no per-point elements to listen on, so the points go
     into a d3.quadtree keyed by their pixel positions: the tooltip
     finds the nearest point within a small radius, and the brush reads
     the same tree to collect the keys inside its rectangle. Crosstalk
     dimming needs no special path at all - a selection re-renders the
     widget, and every redraw already paints each point at its
     pv.keyOpacity. */
  function renderScatterCanvas(ctx, s) {
    var data = s.data, m = s.m, iw = s.iw, ih = s.ih;
    var xScale = s.xScale, y = s.y;

    /* The main svg sits in normal flow under the header; the canvas and
       the overlay are positioned absolutely inside the widget, so both
       need the svg's offset within it. In a container that is not laid
       out yet the rects read zero and everything lands at the widget's
       corner - the resize observer re-renders as soon as real geometry
       exists. */
    var elRect = ctx.el.getBoundingClientRect();
    var svgRect = s.svg.node().getBoundingClientRect();
    var offLeft = svgRect.left - elRect.left;
    var offTop = svgRect.top - elRect.top;

    var dpr = window.devicePixelRatio || 1;
    var canvas = document.createElement("canvas");
    canvas.className = "pv-canvas";
    canvas.width = Math.max(1, Math.round(iw * dpr));
    canvas.height = Math.max(1, Math.round(ih * dpr));
    canvas.style.cssText = "position:absolute;" +
      "left:" + (offLeft + m.left) + "px;" +
      "top:" + (offTop + m.top) + "px;" +
      "width:" + iw + "px;height:" + ih + "px;pointer-events:none;";
    ctx.el.appendChild(canvas);
    var c2 = canvas.getContext("2d");

    function radiusOf(d) { return s.hasSize ? s.r(d.size) : s.r(); }

    /* One full paint of the cloud, at `grow` times the final radius (the
       entrance animation drives grow from 0 to 1). Fill opacity is the
       same per-point pv.keyOpacity the SVG marks use, and the
       surface-coloured ring stays opaque like an SVG stroke. */
    function draw(grow) {
      c2.setTransform(dpr, 0, 0, dpr, 0, 0);
      c2.clearRect(0, 0, iw, ih);
      c2.lineWidth = s.ptStroke;
      c2.strokeStyle = ctx.theme.ink.surface;
      for (var i = 0; i < data.length; i++) {
        var d = data[i];
        var pr = radiusOf(d) * grow;
        if (pr <= 0) { continue; }
        c2.beginPath();
        c2.arc(xScale(d.x), y(d.y), pr, 0, 2 * Math.PI);
        c2.globalAlpha = s.ptOp(d);
        c2.fillStyle = s.fillOf(d);
        c2.fill();
        if (s.ptStroke > 0) {
          c2.globalAlpha = 1;
          c2.stroke();
        }
      }
      c2.globalAlpha = 1;
    }

    /* The entrance: the whole cloud grows out of nothing together. The
       per-point stagger of the SVG marks would be invisible at canvas
       point counts anyway. A re-render mid-animation detaches the
       canvas, and the timer notices and stops. */
    if (ctx.duration > 0) {
      var timer = d3.timer(function (elapsed) {
        if (!canvas.isConnected) { timer.stop(); return; }
        var t = Math.min(1, elapsed / ctx.duration);
        draw(d3.easeCubicOut(t));
        if (t >= 1) { timer.stop(); }
      });
    } else {
      draw(1);
    }

    /* Every point, keyed by its pixel position - the one lookup both
       the hover tooltip and the brush use. */
    var quad = d3.quadtree()
      .x(function (d) { return xScale(d.x); })
      .y(function (d) { return y(d.y); })
      .addAll(data);
    var hitR = Math.max(12, (s.hasSize ? 13 : s.autoR) + 4);

    var overlay = d3.select(ctx.el).append("svg")
      .attr("width", ctx.width).attr("height", ctx.height)
      .style("position", "absolute")
      .style("left", offLeft + "px").style("top", offTop + "px")
      .style("font-family", "inherit");
    var overlayG = overlay.append("g").attr("transform",
      "translate(" + m.left + "," + m.top + ")");

    /* Drag-to-select, when the chart carries row keys. The brush's own
       surface doubles as the hover surface below, and its end handler
       walks the quadtree, pruning every subtree that lies wholly
       outside the brushed rectangle. */
    if (s.keys) {
      var brush = d3.brush().extent([[0, 0], [iw, ih]])
        .on("end", function (event) {
          /* A click (or an emptied brush) clears the selection. */
          if (!event.selection) { ctx.select([]); return; }
          var b = event.selection;
          var inside = [];
          quad.visit(function (node, x0, y0, x1, y1) {
            if (!node.length) {
              do {
                var d = node.data;
                var px = xScale(d.x), py = y(d.y);
                if (px >= b[0][0] && px <= b[1][0] &&
                    py >= b[0][1] && py <= b[1][1]) {
                  inside.push(d.key);
                }
              } while ((node = node.next));
            }
            return x0 > b[1][0] || y0 > b[1][1] ||
              x1 < b[0][0] || y1 < b[0][1];
          });
          ctx.select(inside);
        });
      overlayG.append("g").attr("class", "brush").call(brush);
    }

    /* The hover marker: the nearest point re-drawn at hover size in
       SVG, exactly the lift the SVG marks perform on themselves. It
       lives in its own layer so it stays under the reference lines and
       trends appended after it, matching the SVG stacking. */
    var hoverLayer = overlayG.append("g").attr("pointer-events", "none");
    var hoverDot = null;
    var hovered = null;

    function hitAt(event) {
      var p = d3.pointer(event, overlayG.node());
      return quad.find(p[0], p[1], hitR) || null;
    }
    function onMove(event) {
      var d = hitAt(event);
      if (!d) { onLeave(); return; }
      if (d !== hovered) {
        hovered = d;
        ctx.emit("hover", s.ptDatum(d));
      }
      if (!hoverDot) {
        hoverDot = hoverLayer.append("circle")
          .attr("stroke", ctx.theme.ink.surface).attr("stroke-width", 2);
      }
      hoverDot
        .attr("cx", xScale(d.x)).attr("cy", y(d.y))
        .attr("r", radiusOf(d) * 1.35)
        .attr("fill", s.fillOf(d))
        .style("display", null);
      pv.showTip(ctx, event, s.tipHtml(d));
    }
    function onLeave() {
      hovered = null;
      if (hoverDot) { hoverDot.style("display", "none"); }
      pv.hideTip(ctx);
    }
    function onClick(event) {
      var d = hitAt(event);
      if (d) { ctx.emit("click", s.ptDatum(d)); }
    }

    /* With a brush in place its surface owns the pointer, so the hover
       listeners sit on the overlay svg and catch the bubbled events;
       without one a plain transparent rectangle over the plot does the
       listening, as on every other chart. */
    if (s.keys) {
      overlay
        .on("pointermove", onMove)
        .on("pointerleave", onLeave)
        .on("click", onClick);
    } else {
      overlayG.append("rect")
        .attr("width", iw).attr("height", ih)
        .attr("fill", "transparent")
        .on("pointermove", onMove)
        .on("pointerleave", onLeave)
        .on("click", onClick);
    }

    /* Reference lines, annotation labels, and trend fits go into the
       overlay, above the canvas, so they draw over the points exactly
       as they do over SVG marks. The under-bands already sit in the
       main svg's gUnder, below the canvas. */
    var gOver = overlayG.append("g").attr("pointer-events", "none");
    pv.drawAnnotations(ctx, s.gUnder, gOver, xScale, y, iw, ih);
    var clipId = "pv-trend-clip-" + Math.floor(Math.random() * 1e9);
    overlay.append("clipPath").attr("id", clipId)
      .append("rect").attr("width", iw).attr("height", ih);
    pv.drawTrends(ctx,
      gOver.append("g").attr("clip-path", "url(#" + clipId + ")"),
      xScale, y);
  }

  /* ---------- scatter density contours ---------- */

  /* The density treatment (density = TRUE from R): the cloud aggregated
     into filled contour bands - d3.contourDensity's 2D kernel estimate
     over the projected points, filled along the theme's sequential ramp
     from light (sparse) to dark (the peak), with thin surface-coloured
     separators so neighbouring bands never fuse. The points themselves
     are not drawn, so there is nothing for the drag-to-select brush or
     a crosstalk selection to pick out - both stay off here - while
     annotation and trend layers draw over the contours exactly as they
     draw over points. */
  function renderScatterDensity(ctx) {
    var data = ctx.x.data;
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

    pv.yGrid(g, y, iw, ctx.theme);
    g.append("g").attr("transform", "translate(0," + ih + ")")
      .call(d3.axisBottom(xScale).ticks(Math.min(8, Math.floor(iw / 80)))
        .tickFormat(pv.fmtTick).tickSizeOuter(0))
      .call(function (sel) { pv.styleAxis(sel, ctx.theme, true); });
    g.append("g").call(d3.axisLeft(y).ticks(5).tickFormat(pv.fmtTick))
      .call(function (sel) { pv.styleAxis(sel, ctx.theme, false); });
    pv.axisLabels(svg, ctx, m, iw, ih, ctx.x.xlab, ctx.x.ylab);

    /* Annotation bands still go under the data layer. */
    var gUnder = g.append("g").attr("pointer-events", "none");

    /* The estimator's two knobs, picked from the data and the plot
       rather than hard-coded. Bandwidth follows Scott's rule scaled to
       pixel space: the kernel width shrinks with n^(-1/6) - more
       points justify finer structure - around a base of a quarter of
       the plot's short side, roughly the spread of a cloud that fills
       the panel; clamped so tiny plots never blur into one blob and
       huge ones never dissolve into speckle. The band count grows with
       the plot's short side (about one band per 28px of it, within 8
       to 14): enough steps to show the gradation, few enough that
       neighbouring fills on the sequential ramp stay tellable apart. */
    var n = data.length;
    var bw = Math.max(6, Math.min(40,
      0.25 * Math.min(iw, ih) * Math.pow(n, -1 / 6)));
    var bands = Math.max(8, Math.min(14, Math.round(Math.min(iw, ih) / 28)));
    var contours = d3.contourDensity()
      .x(function (d) { return xScale(d.x); })
      .y(function (d) { return y(d.y); })
      .size([iw, ih])
      .bandwidth(bw)
      .thresholds(bands)(data);

    var ramp = d3.interpolateRgbBasis(ctx.theme.sequential);
    var maxV = contours.length ?
      contours[contours.length - 1].value : 1;
    var geo = d3.geoPath();
    var gBands = g.append("g");
    var paths = gBands.selectAll("path").data(contours).enter()
      .append("path")
      .attr("d", geo)
      .attr("fill", function (d, i) {
        return ramp(contours.length > 1 ? i / (contours.length - 1) : 1);
      })
      .attr("stroke", ctx.theme.ink.surface)
      .attr("stroke-width", 0.7);

    /* The whole surface fades in as one layer - bands have no
       per-mark entrance to stagger. */
    if (ctx.duration > 0) {
      gBands.attr("opacity", 0)
        .transition().duration(ctx.duration)
        .attr("opacity", 1);
    }

    /* The tooltip reports the band under the cursor. SVG hit-testing
       hands the event to the topmost band containing the pointer -
       exactly the highest density level reached there. The level is
       read out relative to the darkest band's threshold: the absolute
       estimate is in points per square pixel, a unit no reader should
       have to think in. */
    paths
      .on("pointerenter pointermove", function (event, d) {
        var i = contours.indexOf(d);
        pv.showTip(ctx, event,
          "density band <b>" + (i + 1) + " of " + contours.length +
          "</b><br>at least <b>" + d3.format(".0%")(d.value / maxV) +
          "</b> of the peak level");
      })
      .on("pointerleave", function () { pv.hideTip(ctx); });

    /* Reference lines, annotation labels, and trend fits over the
       contours, clipped like every scatter trend layer. */
    var gOver = g.append("g").attr("pointer-events", "none");
    pv.drawAnnotations(ctx, gUnder, gOver, xScale, y, iw, ih);
    var clipId = "pv-trend-clip-" + Math.floor(Math.random() * 1e9);
    svg.append("clipPath").attr("id", clipId)
      .append("rect").attr("width", iw).attr("height", ih);
    pv.drawTrends(ctx,
      gOver.append("g").attr("clip-path", "url(#" + clipId + ")"),
      xScale, y);
  }

  /* ---------- scatter hexagonal binning ---------- */

  /* The hex treatment (density = "hex" from R): the cloud binned into
     hexagons (d3.hexbin), each filled on the theme's sequential ramp by
     how many points it holds, with the same thin surface-coloured
     separators the contour bands use. Where the contours smooth the
     cloud into an estimate, the hexes stay honest counts - the tooltip
     gives each cell's exact tally and centre. Like the contours, the
     hexes are the aggregate view: no per-point aesthetics, nothing for
     the brush or a crosstalk selection to pick out, and annotation and
     trend layers draw over the cells exactly as they draw over points. */
  function renderScatterHex(ctx) {
    var data = ctx.x.data;
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

    pv.yGrid(g, y, iw, ctx.theme);
    g.append("g").attr("transform", "translate(0," + ih + ")")
      .call(d3.axisBottom(xScale).ticks(Math.min(8, Math.floor(iw / 80)))
        .tickFormat(pv.fmtTick).tickSizeOuter(0))
      .call(function (sel) { pv.styleAxis(sel, ctx.theme, true); });
    g.append("g").call(d3.axisLeft(y).ticks(5).tickFormat(pv.fmtTick))
      .call(function (sel) { pv.styleAxis(sel, ctx.theme, false); });
    pv.axisLabels(svg, ctx, m, iw, ih, ctx.x.xlab, ctx.x.ylab);

    /* Annotation bands still go under the data layer. */
    var gUnder = g.append("g").attr("pointer-events", "none");

    /* The one knob, picked from the data and the plot rather than
       hard-coded (the same spirit as the contour bandwidth): the radius
       targets about 2 * cbrt(n) hexes across the plot's short side -
       more points earn finer bins - clamped to [8, 32]px so a small
       sample never coarsens into a handful of blobs and a huge cloud
       never dissolves into speckle. A thousand points on a 360px panel
       get an 18px radius; twenty thousand reach the 8px floor. */
    var n = data.length;
    var side = Math.max(1, Math.min(iw, ih));
    var radius = Math.max(8, Math.min(32,
      side / (2 * Math.cbrt(Math.max(1, n)))));

    var hexbin = d3.hexbin()
      .x(function (d) { return xScale(d.x); })
      .y(function (d) { return y(d.y); })
      .extent([[0, 0], [iw, ih]])
      .radius(radius);
    var bins = hexbin(data);

    /* Fills ride the sequential ramp on the square root of the count:
       most clouds pile heavily into a few peak cells, and a linear
       mapping would wash everything else out to the lightest end. The
       root keeps the peak darkest while mid-density structure stays
       tellable apart; the tooltip always has the exact count. */
    var ramp = d3.interpolateRgbBasis(ctx.theme.sequential);
    var maxCount = d3.max(bins, function (b) { return b.length; }) || 1;
    function fillOf(b) { return ramp(Math.sqrt(b.length / maxCount)); }

    /* Cells whose centres sit near the plot's edge overhang it by up to
       one radius, so the layer is clipped to the panel - edge hexes end
       flush with the axes instead of spilling into the margins. */
    var hexClip = "pv-hex-clip-" + Math.floor(Math.random() * 1e9);
    svg.append("clipPath").attr("id", hexClip)
      .append("rect").attr("width", iw).attr("height", ih);
    var gHex = g.append("g").attr("clip-path", "url(#" + hexClip + ")");
    var hexes = gHex.selectAll("path").data(bins).enter().append("path")
      .attr("transform", function (b) {
        return "translate(" + b.x + "," + b.y + ")";
      })
      .attr("d", hexbin.hexagon())
      .attr("fill", fillOf)
      .attr("stroke", ctx.theme.ink.surface)
      .attr("stroke-width", 0.7);

    /* The whole mosaic fades in as one layer, like the contour bands -
       hundreds of cells have no per-mark entrance worth staggering. */
    if (ctx.duration > 0) {
      gHex.attr("opacity", 0)
        .transition().duration(ctx.duration)
        .attr("opacity", 1);
    }

    /* A cell's tooltip and click payload: the exact count and the cell
       centre read back through the scales into data units. */
    function binDatum(b) {
      return { x: xScale.invert(b.x), y: y.invert(b.y), count: b.length };
    }
    function tipHtml(b) {
      return "<b>" + ctx.fmt(b.length) +
        (b.length === 1 ? " point" : " points") + "</b><br>" +
        pv.esc(ctx.x.xlab || "x") + " &#8776; <b>" +
        ctx.fmt(xScale.invert(b.x)) + "</b><br>" +
        pv.esc(ctx.x.ylab || "y") + " &#8776; <b>" +
        ctx.fmt(y.invert(b.y)) + "</b>";
    }

    hexes
      .on("pointerenter pointermove", function (event, b) {
        /* The hovered cell lifts its hairline border to a full ring so
           the eye can hold it while reading the tooltip. */
        d3.select(this).raise()
          .attr("stroke-width", 1.6);
        if (event.type === "pointerenter") {
          ctx.emit("hover", binDatum(b));
        }
        pv.showTip(ctx, event, tipHtml(b));
      })
      .on("pointerleave", function () {
        d3.select(this).attr("stroke-width", 0.7);
        pv.hideTip(ctx);
      })
      .on("click", function (event, b) {
        ctx.emit("click", binDatum(b));
      });

    /* Reference lines, annotation labels, and trend fits over the
       hexes, clipped like every scatter trend layer. */
    var gOver = g.append("g").attr("pointer-events", "none");
    pv.drawAnnotations(ctx, gUnder, gOver, xScale, y, iw, ih);
    var clipId = "pv-trend-clip-" + Math.floor(Math.random() * 1e9);
    svg.append("clipPath").attr("id", clipId)
      .append("rect").attr("width", iw).attr("height", ih);
    pv.drawTrends(ctx,
      gOver.append("g").attr("clip-path", "url(#" + clipId + ")"),
      xScale, y);
  }

})();
