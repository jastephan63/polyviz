/*
 * Evolution and matrix renderers: the area chart family (stacked, percent,
 * stream), the categorical heatmap, and the calendar heatmap. See basic.js
 * for the ctx contract.
 */
(function () {

  /* Resolve a TRUE/FALSE/"auto" flag sent from R. "auto" (or a missing
     value, for payloads saved before the flag existed) takes the
     data-driven decision worked out here, where the real pixel sizes are
     known; anything else is the user's explicit choice. */
  function opt(v, autoDecision) {
    return v === "auto" || v == null ? autoDecision : !!v;
  }

  /* ---------- area ---------- */

  pvRenderers.area = function (ctx) {
    var xtype = ctx.x.xtype;
    var offset = ctx.x.offset;
    var parse = xtype === "date" ? d3.timeParse("%Y-%m-%d") : null;
    /* A single series arrives from R as a bare string, not an array. */
    var seriesNames = [].concat(ctx.x.series);
    var color = d3.scaleOrdinal().domain(seriesNames)
      .range(ctx.theme.palette);
    /* legend = "auto" shows the legend exactly when there is something to
       tell apart; TRUE/FALSE from R override that. The row can wrap on
       narrow charts, so measure what it really used instead of assuming
       one line - otherwise the plot draws over the source credit. */
    if (opt(ctx.x.legend, seriesNames.length > 1)) {
      var legendRow = pv.buildLegend(ctx.header, seriesNames, color, ctx.theme);
      ctx.height = Math.max(120, ctx.height - legendRow.offsetHeight - 7);
    }

    /* Brush-to-zoom (zoom = TRUE from R, continuous x only), exactly as
       on the line chart: the strip gets its room at the bottom before
       the layout is measured, and an active brush window - kept on the
       widget element itself, so it survives full re-renders - narrows
       the x domain further down through the same explicit-domain path a
       facet xlim uses. Inside a facet panel (the payload clone carries
       the grid's subtype) the strip is dropped quietly: the panels
       share axes, which per-panel brushes would break. */
    var zoomOn = ctx.x.zoom === true && xtype !== "category" &&
      ctx.x.subtype == null;
    var zoomDomain = zoomOn ? (ctx.el.__pvZoomX || null) : null;
    if (zoomOn) {
      ctx.height = Math.max(120,
        ctx.height - pv.ZOOM_STRIP_H - pv.ZOOM_STRIP_GAP);
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
       each other), so it doesn't need room for one on the left. Axis
       titles the R side suppressed (empty strings) give their room back
       to the plot. */
    var yTitle = offset === "stream" ? "" : (ctx.x.ylab || "");
    var m = { top: 12, right: 24,
              bottom: ctx.x.xlab ? 52 : 36,
              left: offset === "stream" ? 24 : (yTitle ? 58 : 46) };
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
    pv.axisLabels(svg, ctx, m, iw, ih, ctx.x.xlab, yTitle);

    /* Annotation bands go under the layers; gOver (reference lines and
       labels) is appended after them, further down. Both ignore the
       pointer so the crosshair overlay keeps working. */
    var gUnder = g.append("g").attr("pointer-events", "none");

    var area = d3.area()
      .x(function (p) { return xScale(p.data.x); })
      .y0(function (p) { return y(p[0]); })
      .y1(function (p) { return y(p[1]); })
      .curve(offset === "stream" ? d3.curveBasis : d3.curveMonotoneX);

    /* A brushed window shows only a slice of the data, so the layers
       are clipped to the plot; without zoom no clip exists at all. */
    var layerHost = g;
    if (zoomOn) {
      var zoomClip = "pv-zoom-clip-" + Math.floor(Math.random() * 1e9);
      svg.append("clipPath").attr("id", zoomClip)
        .append("rect").attr("width", iw).attr("height", ih);
      layerHost = g.append("g").attr("clip-path", "url(#" + zoomClip + ")");
    }

    /* The 1.5px surface-coloured stroke draws a seam between layers so
       neighbouring bands never touch. */
    var paths = layerHost.selectAll("path.layer").data(layers).enter()
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

    /* Reference lines and annotation labels sit above the layers; the
       bands went into gUnder earlier. Trend fits are skipped here - the
       layers are stacked, so a trend fitted to the raw values would
       float free of what the eye actually sees. */
    var gOver = g.append("g").attr("pointer-events", "none");
    pv.drawAnnotations(ctx, gUnder, gOver, xScale, y, iw, ih);

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
    /* Positions a brushed window pushed out of the plot can't be
       crosshair targets; with the full domain everything is in range. */
    var visIdx = [];
    xPos.forEach(function (xp, i) {
      if (xp >= -0.5 && xp <= iw + 0.5) visIdx.push(i);
    });

    /* Which pivot row sits nearest the pointer's x, and which layer the
       pointer is inside vertically - shared by hover and click. Returns
       -1 when nothing is in view. */
    function nearestIdx(px) {
      var idx = -1, best = Infinity;
      visIdx.forEach(function (i) {
        var dd = Math.abs(xPos[i] - px);
        if (dd < best) { best = dd; idx = i; }
      });
      return idx;
    }
    function layerUnder(py, idx) {
      var found = null;
      layers.forEach(function (l) {
        var top = y(l[idx][1]), bot = y(l[idx][0]);
        if (py >= Math.min(top, bot) && py <= Math.max(top, bot)) {
          found = l.key;
        }
      });
      return found;
    }

    g.append("rect")
      .attr("width", iw).attr("height", ih)
      .attr("fill", "transparent")
      .on("pointermove", function (event) {
        var p = d3.pointer(event, this);
        var idx = nearestIdx(p[0]);
        if (idx < 0) return;
        cross.attr("x1", xPos[idx]).attr("x2", xPos[idx]).attr("opacity", 1);

        var hoverKey = layerUnder(p[1], idx);
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
      })
      .on("click", function (event) {
        /* A click reports the x position under the pointer and, when the
           pointer sits inside a layer, that layer and its value there. */
        var p = d3.pointer(event, this);
        var idx = nearestIdx(p[0]);
        if (idx < 0) return;
        var key = layerUnder(p[1], idx);
        var payload = { x: pivot[idx].key };
        if (key !== null) {
          payload.series = key;
          payload.y = pivot[idx][key];
        }
        ctx.emit("click", payload);
      });

    /* The context strip, below everything else and clear of the source
       line (ctx.height already excludes the footer). Its margins match
       the main panel's, so the brush lines up under the plot. */
    if (zoomOn) {
      pv.zoomStrip(ctx, {
        left: m.left, right: m.right, xtype: xtype,
        extent: fullX, zoom: zoomDomain,
        draw: function (sg, sx, siw, sih) {
          /* The miniature: the stack's outer envelope as one muted
             silhouette - enough shape to aim a brush at, no more. */
          var sy = d3.scaleLinear().domain(y.domain()).range([sih, 0]);
          var env = pivot.map(function (row, i) {
            return {
              x: row.x,
              y0: d3.min(layers, function (l) { return l[i][0]; }),
              y1: d3.max(layers, function (l) { return l[i][1]; })
            };
          });
          sg.append("path").datum(env)
            .attr("fill", ctx.theme.ink.muted)
            .attr("fill-opacity", 0.3)
            .attr("d", d3.area()
              .x(function (d) { return sx(d.x); })
              .y0(function (d) { return sy(d.y0); })
              .y1(function (d) { return sy(d.y1); })
              .curve(offset === "stream" ? d3.curveBasis :
                     d3.curveMonotoneX));
        }
      });
    }
  };

  /* ---------- heatmap ---------- */

  pvRenderers.heatmap = function (ctx) {
    /* Annotations and trends are skipped on heatmaps: both axes are
       categorical bands, so lines and fits have no position to live at. */
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
    ctx.height = Math.max(120, ctx.height - scaleRow.offsetHeight - 7);

    var xCats = pv.uniq(data.map(function (d) { return d.x; }));
    var yCats = pv.uniq(data.map(function (d) { return d.y; }));

    /* Row labels get a margin sized to the longest label after the
       character budget (truncate_labels in R) is applied - but never more
       than 40% of the chart, because at phone widths the cells matter
       more than full row names. Anything shortened keeps its full text in
       the cell tooltip. Axis titles only exist when the R side sent them
       (the user set xlab/ylab), and take their room here too. */
    var truncN = ctx.x.truncateLabels == null ? 24 : ctx.x.truncateLabels;
    var xTitle = ctx.x.xtitle || "";
    var yTitle = ctx.x.ytitle || "";
    var widest = d3.max(yCats, function (d) {
      return pv.textWidth(pv.truncate(d, truncN), 11);
    }) || 26;
    var m = { top: 8, right: 14, bottom: xTitle ? 48 : 34,
              left: Math.min(Math.round(ctx.width * 0.4), 190, 16 + widest) };
    if (yTitle) { m.left += 16; }
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
    xAxis.selectAll("text").text(function (d) {
      return pv.truncate(d, truncN);
    });

    /* Crowded row labels thin out the same way: 11px type needs about
       14px of band to stay separate, so keep roughly one label per 14px
       of height and let the cell tooltip carry the exact rows. */
    var yMaxTicks = Math.max(2, Math.floor(ih / 14));
    var yStep = Math.ceil(yCats.length / yMaxTicks);
    var yAxis = g.append("g").call(d3.axisLeft(yb).tickSize(0)
      .tickValues(yCats.filter(function (d, i) { return i % yStep === 0; })));
    pv.styleAxis(yAxis, ctx.theme, false);
    /* Row labels shorten to the character budget, and further to whatever
       the (possibly width-capped) margin really fits; the tooltip carries
       the full name. */
    var fitChars = Math.max(3, Math.floor(
      (m.left - (yTitle ? 16 : 0) - 12) / (11 * 0.62)));
    yAxis.selectAll("text").text(function (d) {
      return pv.truncate(d, Math.min(truncN, fitChars));
    });
    pv.axisLabels(svg, ctx, m, iw, ih, xTitle, yTitle);

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

    /* The surface-coloured stroke on every cell is what makes the gaps
       between cells. It scales with the cells themselves: the full 2px
       on roomy grids, proportionally less as the bands shrink, and none
       at all below 4px bands - there the gap would erase the fill, and a
       dense grid reads best as a continuous colour field. */
    var minBand = Math.min(xb.bandwidth(), yb.bandwidth());
    var gapW = minBand < 4 ? 0 : Math.min(2, minBand / 6);

    /* Cells live in their own layer so a raised (hovered) cell can't cover
       the value labels drawn after them. */
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
      .attr("stroke-width", gapW)
      .attr("opacity", 0);

    /* Cells sweep in column by column. */
    cells.transition().duration(ctx.duration)
      .delay(function (d) { return Math.min(xIndex[d.x] * 40, 500); })
      .ease(d3.easeCubicOut)
      .attr("opacity", 1);

    /* Print values in the cells when there is room. "auto" keeps the
       comfortable rule (both cell dimensions over 40px); cell_values =
       TRUE squeezes down to a 9px font for 24-40px cells, but below 24px
       nothing fits at any honest size, so even TRUE prints nothing.
       FALSE never prints. */
    var showVals = opt(ctx.x.cellValues,
      xb.bandwidth() > 40 && yb.bandwidth() > 40);
    if (showVals && minBand >= 24) {
      var valPx = minBand > 40 ? 11 : 9;
      g.selectAll("text.cellval").data(data).enter().append("text")
        .attr("class", "cellval")
        .attr("x", function (d) { return xb(d.x) + xb.bandwidth() / 2; })
        .attr("y", function (d) { return yb(d.y) + yb.bandwidth() / 2; })
        .attr("text-anchor", "middle")
        .attr("dominant-baseline", "middle")
        .attr("fill", function (d) { return inkFor(colorOf(d.value)); })
        .style("font-size", valPx + "px")
        .style("font-variant-numeric", "tabular-nums")
        .style("pointer-events", "none")
        .style("opacity", 0)
        .text(function (d) { return cellFmt(d.value); })
        .transition().delay(ctx.duration).duration(200)
        .style("opacity", 1);
    }

    /* Hovering rings the cell in primary ink (raised so the ring isn't
       hidden under its neighbours' strokes). The ring keeps a visible
       width of its own - on dense grids the gap stroke is zero. */
    cells
      .on("pointerenter pointermove", function (event, d) {
        d3.select(this).raise()
          .attr("stroke", ctx.theme.ink.primary)
          .attr("stroke-width", Math.max(gapW, 1.25));
        pv.showTip(ctx, event,
          "<b>" + pv.esc(d.x) + " · " + pv.esc(d.y) + "</b><br>" +
          pv.esc(ctx.x.vlab || "value") + ": <b>" + ctx.fmt(d.value) +
          "</b>");
      })
      .on("pointerleave", function () {
        d3.select(this).attr("stroke", ctx.theme.ink.surface)
          .attr("stroke-width", gapW);
        pv.hideTip(ctx);
      });
  };

  /* ---------- calendar ---------- */

  pvRenderers.calendar = function (ctx) {
    /* Annotations and trends are skipped on calendars: the grid layout
       has no continuous axes for a line or band to attach to. */
    /* A single year arrives from R as a bare number, not an array. */
    var years = [].concat(ctx.x.years);
    var domain = ctx.x.domain;

    /* Day colours glide through the theme's 11-step sequential ramp over
       the observed [min, max] the R side sent. (A diverging variant, for
       values spanning zero, is future work.) */
    var ramp = d3.interpolateRgbBasis(ctx.theme.sequential);
    var t = d3.scaleLinear().domain(domain).range([0, 1]).clamp(true);
    var colorOf = function (v) { return ramp(t(v)); };

    /* Compact numbers for the legend ends: SI units past 10k, at most
       two decimals below - same as the heatmap. */
    var legFmt = function (v) {
      return Math.abs(v) >= 10000 ?
        d3.format("~s")(v) : d3.format(",.2~f")(v);
    };

    /* The calendar's legend is its colour scale: a small gradient bar in
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
    lo.textContent = legFmt(domain[0]);
    var hi = document.createElement("span");
    hi.textContent = legFmt(domain[1]);
    scaleRow.appendChild(lo);
    scaleRow.appendChild(bar);
    scaleRow.appendChild(hi);
    ctx.header.appendChild(scaleRow);
    ctx.height = Math.max(120, ctx.height - scaleRow.offsetHeight - 7);

    var valueOf = {};
    ctx.x.data.forEach(function (d) { valueOf[d.date] = d.value; });
    var iso = d3.timeFormat("%Y-%m-%d");

    /* Every day of every shown year becomes a cell, whether the data has
       a row for it or not - absence should be visible, not skipped.
       Columns are Monday-started weeks counted from Jan 1; rows are
       weekdays with Monday on top. */
    var blocks = years.map(function (y) {
      var start = new Date(y, 0, 1);
      var days = d3.timeDays(start, new Date(y + 1, 0, 1)).map(function (day) {
        var key = iso(day);
        return {
          date: day,
          col: d3.timeMonday.count(start, day),
          row: (day.getDay() + 6) % 7,
          value: valueOf.hasOwnProperty(key) ? valueOf[key] : null
        };
      });
      return { year: y, start: start, days: days,
               cols: days[days.length - 1].col + 1 };
    });
    /* 53 columns is the usual year; a leap year starting on Sunday needs
       54. Size the grid for whichever these years actually need. */
    var cols = Math.max(53, d3.max(blocks, function (b) { return b.cols; }));

    /* The cell step (cell + gap) is whatever lets all the columns fit the
       width AND all the year blocks fit the height, capped so one lone
       year on a huge canvas doesn't balloon. Weekday hints next to the
       first column only appear when the cells are big enough to line up
       with 9px type - so try with their margin first, and reclaim it for
       the cells if they won't be shown. */
    var n = blocks.length;
    var padL = 8, padR = 10, monthH = 14, gapY = 16;
    var yearW = Math.ceil(pv.textWidth(String(d3.max(years)), 12)) + 10;
    function fit(wdW) {
      var availW = ctx.width - padL - yearW - wdW - padR;
      var availH = ctx.height - n * monthH - (n - 1) * gapY - 12;
      return Math.max(3, Math.min(17,
        Math.floor(availW / cols), Math.floor(availH / (7 * n))));
    }
    var step = fit(16);
    var showWd = step >= 9;
    if (!showWd) { step = fit(0); }
    var wdW = showWd ? 16 : 0;
    /* Tiny cells trade their 2px gap for a 1px one - at phone widths the
       gap would otherwise eat more pixels than the cell. */
    var gap = step >= 6 ? 2 : 1;
    var cell = step - gap;
    var rad = Math.min(2, cell / 2);
    /* When the height (not the width) decided the cell size, the grid
       won't reach the right edge - shift the whole composition, labels
       and all, to centre it rather than leave all the slack on one side. */
    var offX = Math.max(0, Math.floor(
      (ctx.width - padR - padL - yearW - wdW - cols * step) / 2));
    var gridX = padL + offX + yearW + wdW;
    var blockH = monthH + 7 * step;
    var totalH = n * blockH + (n - 1) * gapY;
    /* Calendars are wide and shallow; centre the blocks vertically so a
       tall container doesn't leave them huddled at the top. */
    var offY = Math.max(6, (ctx.height - totalH) / 2);

    var svg = pv.baseSvg(ctx);
    var fmtDate = d3.timeFormat("%A, %b %e, %Y");
    var monthInitials = "JFMAMJJASOND";

    blocks.forEach(function (b, bi) {
      var top = offY + bi * (blockH + gapY);
      var g = svg.append("g").attr("transform",
        "translate(" + gridX + "," + (top + monthH) + ")");

      /* The year, left of its block, centred on the 7 weekday rows. */
      svg.append("text")
        .attr("x", padL + offX).attr("y", top + monthH + 3.5 * step)
        .attr("dominant-baseline", "middle")
        .attr("fill", ctx.theme.ink.secondary)
        .style("font-size", "12px").style("font-weight", 600)
        .text(b.year);

      /* Month initials along the top, each over its first week. */
      for (var mi = 0; mi < 12; mi++) {
        g.append("text")
          .attr("x", d3.timeMonday.count(b.start, new Date(b.year, mi, 1)) *
            step)
          .attr("y", -4)
          .attr("fill", ctx.theme.ink.muted)
          .style("font-size", "9.5px")
          .text(monthInitials.charAt(mi));
      }

      /* Weekday hints on alternate rows, when the cells can carry them. */
      if (showWd) {
        ["M", "W", "F"].forEach(function (wd, wi) {
          g.append("text")
            .attr("x", -5).attr("y", wi * 2 * step + cell / 2)
            .attr("text-anchor", "end")
            .attr("dominant-baseline", "middle")
            .attr("fill", ctx.theme.ink.muted)
            .style("font-size", "9px")
            .text(wd);
        });
      }

      /* The day cells. Days the data never mentions keep a faint
         grid-coloured cell - visibly absent, not painted as zero. */
      var cells = g.selectAll("rect.day").data(b.days).enter()
        .append("rect")
        .attr("class", "day")
        .attr("x", function (d) { return d.col * step; })
        .attr("y", function (d) { return d.row * step; })
        .attr("width", cell).attr("height", cell)
        .attr("rx", rad).attr("ry", rad)
        .attr("fill", function (d) {
          return d.value == null ? ctx.theme.ink.grid : colorOf(d.value);
        });

      /* Cells fade in week by week, left to right, all blocks together.
         With duration 0 they are simply drawn - no transition is even
         scheduled, so the final state exists synchronously. */
      if (ctx.duration > 0) {
        cells.attr("opacity", 0)
          .transition().duration(Math.max(180, ctx.duration * 0.4))
          .delay(function (d) {
            return Math.min(d.col * ctx.duration / 70, ctx.duration * 0.7);
          })
          .ease(d3.easeCubicOut)
          .attr("opacity", 1);
      }

      /* Hovering rings the day in primary ink (raised so the ring isn't
         clipped by its neighbours) and reads out the full date. */
      cells
        .on("pointerenter pointermove", function (event, d) {
          d3.select(this).raise()
            .attr("stroke", ctx.theme.ink.primary)
            .attr("stroke-width", 1.5);
          var row = d.value == null ?
            '<span style="opacity:0.65">no data</span>' :
            pv.swatchRow(colorOf(d.value), ctx.x.vlab || "value",
                         ctx.fmt(d.value));
          pv.showTip(ctx, event,
            "<b>" + fmtDate(d.date) + "</b><br>" + row);
        })
        .on("pointerleave", function () {
          d3.select(this).attr("stroke", "none");
          pv.hideTip(ctx);
        });
    });
  };

})();
