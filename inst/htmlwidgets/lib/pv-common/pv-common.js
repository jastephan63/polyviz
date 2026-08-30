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

  /* Compact tick labels: 6M instead of 6,000,000; plain numbers below 10k. */
  pv.fmtTick = function (v) {
    return Math.abs(v) >= 10000 ? d3.format("~s")(v) : d3.format(",")(v);
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

  return pv;
})();
