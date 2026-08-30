/*
 * Shared chart chrome for every polyviz renderer: the header, legend,
 * tooltip, axis styling, and a few small drawing helpers. Everything is
 * exposed on one global object, `pv`, that the renderer files use.
 *
 * The renderers themselves live in ../pv-renderers/, one file per chart
 * family, and register themselves on the `pvRenderers` object. The
 * htmlwidgets binding (pvchart.js) looks the chart type up there.
 */
window.pvRenderers = window.pvRenderers || {};
window.pv = (function () {
  var pv = {};

  /* ---------- text & numbers ---------- */

  pv.esc = function (s) {
    return String(s).replace(/&/g, "&amp;").replace(/</g, "&lt;")
      .replace(/>/g, "&gt;");
  };

  /* Compact tick labels: 6M instead of 6,000,000; plain numbers below 10k.
     No thousands separator under 10k - otherwise years render as "2,020".
     Three significant digits ("23.3M", not "23.2787M") - charts round,
     tooltips carry the precision. */
  pv.fmtTick = function (v) {
    return Math.abs(v) >= 10000 ? d3.format(".3~s")(v) : String(v);
  };

  /* Shorten a label to fit, with an ellipsis. Charts that truncate must
     keep the full text available in their tooltip. */
  pv.truncate = function (s, maxChars) {
    s = String(s);
    return s.length <= maxChars ? s : s.slice(0, Math.max(1, maxChars - 1)) + "…";
  };

  /* How many pixels a label roughly needs at the given font size - for
     fit decisions before anything is drawn. Slightly generous on purpose:
     a label that just fits usually reads as cramped. */
  pv.textWidth = function (s, fontSize) {
    return String(s).length * (fontSize || 11) * 0.62;
  };

  pv.uniq = function (arr) {
    var seen = {}, out = [];
    arr.forEach(function (v) {
      var k = String(v);
      if (!seen[k]) { seen[k] = true; out.push(v); }
    });
    return out;
  };

  /* ---------- header, legend, tooltip ---------- */

  pv.buildHeader = function (el, x, theme) {
    var header = document.createElement("div");
    header.style.cssText = "padding:14px 16px 8px 16px;";
    if (x.title) {
      var t = document.createElement("div");
      t.textContent = x.title;
      t.style.cssText = "font-size:16px;font-weight:700;letter-spacing:-0.01em;color:" +
        theme.ink.primary + ";";
      header.appendChild(t);
    }
    if (x.subtitle) {
      var s = document.createElement("div");
      s.textContent = x.subtitle;
      s.style.cssText = "font-size:12.5px;margin-top:3px;color:" +
        theme.ink.secondary + ";";
      header.appendChild(s);
    }
    el.appendChild(header);
    return header;
  };

  pv.buildLegend = function (header, names, colorOf, theme) {
    var row = document.createElement("div");
    row.style.cssText =
      "display:flex;flex-wrap:wrap;gap:4px 14px;margin-top:7px;";
    names.forEach(function (nm) {
      var item = document.createElement("span");
      item.style.cssText =
        "display:inline-flex;align-items:center;gap:5px;font-size:12px;color:" +
        theme.ink.secondary + ";";
      var sw = document.createElement("span");
      sw.style.cssText =
        "width:10px;height:10px;border-radius:3px;background:" + colorOf(nm) + ";";
      item.appendChild(sw);
      item.appendChild(document.createTextNode(nm));
      row.appendChild(item);
    });
    header.appendChild(row);
    return row;
  };

  pv.buildTooltip = function (el, theme) {
    var tip = document.createElement("div");
    tip.style.cssText =
      "position:absolute;pointer-events:none;opacity:0;z-index:10;" +
      "background:" + theme.ink.tooltipBg + ";color:" + theme.ink.tooltipText + ";" +
      "border:1px solid " + theme.ink.tooltipBorder + ";border-radius:8px;" +
      "padding:7px 10px;font-size:12px;line-height:1.55;max-width:280px;" +
      "box-shadow:0 4px 16px rgba(0,0,0,0.16);transition:opacity 130ms ease;";
    el.appendChild(tip);
    return tip;
  };

  pv.showTip = function (ctx, event, html) {
    var tip = ctx.tip;
    tip.innerHTML = html;
    tip.style.opacity = 1;
    /* Place the tooltip just above-right of the pointer, then flip it to
       the other side if it would poke out of the chart. */
    var r = ctx.el.getBoundingClientRect();
    var px = event.clientX - r.left, py = event.clientY - r.top;
    var tw = tip.offsetWidth, th = tip.offsetHeight;
    var lx = px + 14, ly = py - th - 10;
    if (lx + tw > ctx.width - 6) lx = px - tw - 14;
    if (ly < 4) ly = py + 16;
    tip.style.left = Math.max(4, lx) + "px";
    tip.style.top = ly + "px";
  };

  pv.hideTip = function (ctx) { ctx.tip.style.opacity = 0; };

  pv.swatchRow = function (color, name, value) {
    return '<span style="display:inline-block;width:9px;height:9px;' +
      'border-radius:2.5px;margin-right:5px;background:' + color + ';"></span>' +
      pv.esc(name) + ': <b>' + value + "</b>";
  };

  /* ---------- svg scaffolding ---------- */

  pv.baseSvg = function (ctx) {
    return d3.select(ctx.el).append("svg")
      .attr("width", ctx.width).attr("height", ctx.height)
      .style("display", "block")
      .style("font-family", "inherit");
  };

  pv.styleAxis = function (sel, theme, keepDomain) {
    sel.selectAll(".domain")
      .attr("stroke", keepDomain ? theme.ink.baseline : "none");
    sel.selectAll("line").attr("stroke", theme.ink.baseline);
    sel.selectAll("text")
      .attr("fill", theme.ink.muted)
      .style("font-size", "11px")
      .style("font-family", "inherit")
      .style("font-variant-numeric", "tabular-nums");
  };

  pv.yGrid = function (g, yScale, innerW, theme) {
    var grid = g.append("g")
      .call(d3.axisLeft(yScale).ticks(5).tickSize(-innerW).tickFormat(""));
    grid.selectAll("line").attr("stroke", theme.ink.grid);
    grid.selectAll(".domain").remove();
    return grid;
  };

  pv.axisLabels = function (svg, ctx, m, innerW, innerH, xlab, ylab) {
    if (xlab) {
      svg.append("text")
        .attr("x", m.left + innerW / 2).attr("y", ctx.height - 6)
        .attr("text-anchor", "middle")
        .attr("fill", ctx.theme.ink.secondary).style("font-size", "12px")
        .text(xlab);
    }
    if (ylab) {
      svg.append("text")
        .attr("transform", "rotate(-90)")
        .attr("x", -(m.top + innerH / 2)).attr("y", 14)
        .attr("text-anchor", "middle")
        .attr("fill", ctx.theme.ink.secondary).style("font-size", "12px")
        .text(ylab);
    }
  };

  /* Draws a bar as an SVG path where only the top two corners are rounded.
     A plain <rect> with rounded corners would round the bottom too, and the
     bottom edge should sit flat on the axis baseline. */
  pv.topRoundedBar = function (x0, y0, w, h, r) {
    r = Math.max(0, Math.min(r, w / 2, h));
    return "M" + x0 + "," + (y0 + h) +
      "v" + (-(h - r)) +
      "a" + r + "," + r + " 0 0 1 " + r + "," + (-r) +
      "h" + (w - 2 * r) +
      "a" + r + "," + r + " 0 0 1 " + r + "," + r +
      "v" + (h - r) + "z";
  };

  /* Left-rounded horizontal bar variant (data end on the right). */
  pv.rightRoundedBar = function (x0, y0, w, h, r) {
    r = Math.max(0, Math.min(r, h / 2, w));
    return "M" + x0 + "," + y0 +
      "h" + (w - r) +
      "a" + r + "," + r + " 0 0 1 " + r + "," + r +
      "v" + (h - 2 * r) +
      "a" + r + "," + r + " 0 0 1 " + (-r) + "," + r +
      "h" + (-(w - r)) + "z";
  };

  /* ---------- annotation, trend, and selection layers ----------
     Cartesian renderers call these after building their scales, so the
     modifier functions on the R side (pv_annotate, pv_trend, pv_link)
     work on every chart without the chart knowing anything about them. */

  /* Draws the chart's annotation list: reference lines, shaded bands,
     and text labels with optional leader lines. Bands go under the data
     (the gUnder group), lines and labels above it (gOver). Values on a
     category axis pass through the scale, so "Luzern" works as well
     as 100. */
  pv.drawAnnotations = function (ctx, gUnder, gOver, xs, ys, iw, ih) {
    var anns = ctx.x.annotations;
    if (!anns || !anns.length) return;
    var px = function (v) { return +xs(v); };
    var py = function (v) { return +ys(v); };
    anns.forEach(function (a) {
      if (a.type === "band") {
        var horiz = a.y0 != null;
        var r = gUnder.append("rect")
          .attr("fill", ctx.theme.ink.grid).attr("fill-opacity", 0.55);
        if (horiz) {
          var y1 = py(a.y0), y2 = py(a.y1);
          r.attr("x", 0).attr("width", iw)
            .attr("y", Math.min(y1, y2))
            .attr("height", Math.abs(y2 - y1));
        } else {
          var x1 = px(a.x0), x2 = px(a.x1);
          r.attr("y", 0).attr("height", ih)
            .attr("x", Math.min(x1, x2))
            .attr("width", Math.abs(x2 - x1));
        }
        if (a.label) {
          gUnder.append("text")
            .attr("x", horiz ? 6 : Math.min(px(a.x0), px(a.x1)) + 5)
            .attr("y", horiz ? Math.min(py(a.y0), py(a.y1)) + 13 : 13)
            .attr("fill", ctx.theme.ink.muted)
            .style("font-size", "10.5px")
            .text(a.label);
        }
      } else if (a.type === "hline" || a.type === "vline") {
        var line = gOver.append("line")
          .attr("stroke", a.color || ctx.theme.ink.secondary)
          .attr("stroke-width", 1)
          .attr("stroke-dasharray", "4,3");
        if (a.type === "hline") {
          line.attr("x1", 0).attr("x2", iw)
            .attr("y1", py(a.at)).attr("y2", py(a.at));
        } else {
          line.attr("y1", 0).attr("y2", ih)
            .attr("x1", px(a.at)).attr("x2", px(a.at));
        }
        if (a.label) {
          gOver.append("text")
            .attr("x", a.type === "hline" ? iw - 4 : px(a.at) + 5)
            .attr("y", a.type === "hline" ? py(a.at) - 5 : 12)
            .attr("text-anchor", a.type === "hline" ? "end" : "start")
            .attr("fill", a.color || ctx.theme.ink.secondary)
            .style("font-size", "11px")
            .style("paint-order", "stroke")
            .style("stroke", ctx.theme.ink.surface)
            .style("stroke-width", 3)
            .text(a.label);
        }
      } else if (a.type === "label") {
        var lx = px(a.x), ly = py(a.y);
        var tx = lx + (a.dx || 0), ty = ly + (a.dy || 0);
        if (a.dx || a.dy) {
          gOver.append("line")
            .attr("x1", lx).attr("y1", ly).attr("x2", tx).attr("y2", ty)
            .attr("stroke", ctx.theme.ink.muted)
            .attr("stroke-width", 0.75);
        }
        gOver.append("text")
          .attr("x", tx + ((a.dx || 0) >= 0 ? 4 : -4))
          .attr("y", ty)
          .attr("text-anchor", (a.dx || 0) >= 0 ? "start" : "end")
          .attr("dominant-baseline", "middle")
          .attr("fill", ctx.theme.ink.secondary)
          .style("font-size", "11px")
          .style("paint-order", "stroke")
          .style("stroke", ctx.theme.ink.surface)
          .style("stroke-width", 3)
          .text(a.text);
      }
    });
  };

  /* Draws R-fitted trend curves: a confidence ribbon (when the fit
     carries one) under a 2px line, both in the requested palette slot.
     The points arrive already ordered by x. */
  pv.drawTrends = function (ctx, g, xs, ys) {
    var trends = ctx.x.trends;
    if (!trends || !trends.length) return;
    trends.forEach(function (t) {
      var color = ctx.theme.palette[(t.slot || 2) - 1] ||
        ctx.theme.palette[1];
      if (t.points.length && t.points[0].lo != null) {
        g.append("path").datum(t.points)
          .attr("fill", color).attr("fill-opacity", 0.14)
          .attr("d", d3.area()
            .x(function (d) { return xs(d.x); })
            .y0(function (d) { return ys(d.lo); })
            .y1(function (d) { return ys(d.hi); })
            .curve(d3.curveMonotoneX));
      }
      g.append("path").datum(t.points)
        .attr("fill", "none")
        .attr("stroke", color).attr("stroke-width", 2)
        .attr("stroke-dasharray", t.dash ? "5,3" : null)
        .attr("d", d3.line()
          .x(function (d) { return xs(d.x); })
          .y(function (d) { return ys(d.y); })
          .curve(d3.curveMonotoneX));
    });
  };

  /* Linked-selection opacity: full strength for selected keys (or when
     nothing is selected anywhere), faded for the rest. Renderers use it
     wherever they set mark opacity. */
  pv.keyOpacity = function (ctx, key, base, dim) {
    if (!ctx.selected) return base;
    return ctx.selected.indexOf(String(key)) >= 0 ? base :
      (dim == null ? 0.12 : dim);
  };

  return pv;
})();
