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

  /* The active locale for the formatters below. pvchart.js sets it
     before each render from the payload (pv_locale on the R side):
     the built d3 locale instances plus the decimal mark, or null for
     the stock US-style output. The derived tick formatters are cached
     on the locale object itself, which pvchart.js keeps one of per
     locale tag, so they are built once per locale, not per render. */
  pv.locale = null;
  pv.setLocale = function (locale) {
    if (locale && !locale.tickBig) {
      /* Grouped whole numbers with 3 significant digits for the
         10k-to-1M tick range, and the compact form (locale decimal
         mark included) for the millions. */
      locale.tickBig = locale.number.format(",.3~r");
      locale.tickCompact = locale.number.format(".3~s");
    }
    pv.locale = locale || null;
  };

  /* Compact tick labels: 6M instead of 6,000,000; plain numbers below 10k.
     No thousands separator under 10k - otherwise years render as "2,020".
     Three significant digits ("23.3M", not "23.2787M") - charts round,
     tooltips carry the precision.

     An active locale keeps those rules but writes ticks the way Swiss
     print does: 10'000 up to a million appears grouped in full with the
     locale's own marks ("10'000", not "10k" - the grouping mark is what
     makes the full form compact enough), the millions stay in the short
     form so the fixed axis margins still fit them, and below 10k only
     the decimal mark changes, so year axes keep reading "2020". */
  pv.fmtTick = function (v) {
    var loc = pv.locale;
    if (loc) {
      if (Math.abs(v) >= 1e6) return loc.tickCompact(v);
      if (Math.abs(v) >= 10000) return loc.tickBig(v);
      var s = String(v);
      return loc.decimal === "." ? s : s.replace(".", loc.decimal);
    }
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

  /* `textureOf` is optional (pv_textures): a lookup handing back the CSS
     hatch for a series, or nothing - then the swatch stays a solid, as
     every legend was before textures existed. */
  pv.buildLegend = function (header, names, colorOf, theme, textureOf) {
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
      var tex = textureOf && textureOf(nm);
      if (tex) {
        /* The swatch wears the same texture as the marks: the lightened
           ground as its background colour, the hatch as a repeating
           gradient on top of it. */
        sw.style.background = tex.color;
        sw.style.backgroundImage = tex.image;
      }
      item.appendChild(sw);
      item.appendChild(document.createTextNode(nm));
      row.appendChild(item);
    });
    header.appendChild(row);
    return row;
  };

  pv.buildTooltip = function (el, theme) {
    var tip = document.createElement("div");
    /* The class lets the svg exporter recognise the tooltip and leave
       it out of exported charts, wherever in the widget it currently
       lives (the facet renderer moves it between panels). */
    tip.className = "pv-tooltip";
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

  /* ---------- standalone svg export ----------
     A rendered widget is an HTML sandwich: the title, subtitle, legend,
     and source line are divs sitting around the plot's <svg>, so saving
     just the svg would lose all of them. pv.toStandaloneSvg rebuilds the
     whole chart as one self-contained SVG document by walking the live
     DOM: text nodes become <text> at their measured positions, coloured
     boxes (legend swatches, gradient scale bars) become <rect>, and each
     plot <svg> is embedded where it sits. Both the R export function and
     the in-browser download button go through here. */

  var SVG_NS = "http://www.w3.org/2000/svg";

  function svgNode(name) {
    return document.createElementNS(SVG_NS, name);
  }

  function round2(v) {
    return Math.round(v * 100) / 100;
  }

  /* An element's computed background-color is "rgba(0, 0, 0, 0)" when
     nothing was set - only a real colour earns a rect. */
  function realBg(c) {
    return c && c !== "transparent" &&
      c.indexOf("rgba(0, 0, 0, 0)") !== 0 ? c : null;
  }

  /* Split a CSS argument list on the commas between arguments, not the
     ones inside colour functions like rgb(90, 166, 221). */
  function splitCssArgs(s) {
    var out = [], depth = 0, cur = "";
    for (var i = 0; i < s.length; i++) {
      var c = s.charAt(i);
      if (c === "(") depth++;
      else if (c === ")") depth--;
      if (c === "," && depth === 0) { out.push(cur.trim()); cur = ""; }
      else cur += c;
    }
    if (cur.trim()) out.push(cur.trim());
    return out;
  }

  function ensureDefs(state) {
    if (!state.defs) {
      state.defs = svgNode("defs");
      state.root.insertBefore(state.defs, state.root.firstChild);
    }
    return state.defs;
  }

  /* Turn a computed linear-gradient background (the heatmap and map
     colour-scale bars) into an SVG <linearGradient> in the document's
     defs, and hand back a url(#...) fill for it. Every polyviz gradient
     bar runs left to right, so the direction is fixed horizontal.
     Returns null when the value isn't a stop list we understand. */
  function gradientFill(state, backgroundImage) {
    var m = backgroundImage.match(/linear-gradient\((.*)\)/);
    if (!m) return null;
    var args = splitCssArgs(m[1]);
    if (args.length && /^(-?[\d.]+(deg|rad|turn)$|to\s)/.test(args[0])) {
      args.shift();
    }
    var stops = [];
    args.forEach(function (a) {
      var sm = a.match(/^(.*?)\s+([\d.]+)%$/);
      if (sm) stops.push({ color: sm[1], offset: sm[2] + "%" });
    });
    if (stops.length < 2) return null;
    ensureDefs(state);
    var id = "pv-export-grad-" + (++state.gradients);
    var grad = svgNode("linearGradient");
    grad.setAttribute("id", id);
    grad.setAttribute("x1", "0");
    grad.setAttribute("y1", "0");
    grad.setAttribute("x2", "1");
    grad.setAttribute("y2", "0");
    stops.forEach(function (st) {
      var stop = svgNode("stop");
      stop.setAttribute("offset", st.offset);
      stop.setAttribute("stop-color", st.color);
      grad.appendChild(stop);
    });
    state.defs.appendChild(grad);
    return "url(#" + id + ")";
  }

  /* Rebuild a legend swatch's CSS hatch (the repeating-linear-gradient
     pv.textureSwatchCss paints when textures are on) as an SVG pattern
     in the export's defs, over the swatch's own background colour, and
     hand back a url(#...) fill. The computed style arrives with the
     angle in degrees and the stop offsets in px, which pin down the
     stripe geometry exactly; a value shaped any other way returns null
     and the swatch falls back to its solid ground. */
  function hatchFill(state, backgroundImage, ground) {
    var m = backgroundImage.match(/repeating-linear-gradient\((.*)\)/);
    if (!m) return null;
    var args = splitCssArgs(m[1]);
    var am = args.length ? args[0].match(/^(-?[\d.]+)deg$/) : null;
    if (!am) return null;
    var stops = [];
    for (var i = 1; i < args.length; i++) {
      var sm = args[i].match(/^(.*?)\s+(-?[\d.]+)px$/);
      if (!sm) return null;
      stops.push({ color: sm[1].trim(), offset: parseFloat(sm[2]) });
    }
    if (stops.length < 2) return null;
    /* The tile repeats every `period` px along the gradient axis; the
       stripe is the run of the final (non-transparent) colour, from the
       first stop that wears it to the period's end. */
    var period = stops[stops.length - 1].offset;
    var line = stops[stops.length - 1].color;
    var start = period;
    for (var j = 0; j < stops.length; j++) {
      if (stops[j].color === line) { start = stops[j].offset; break; }
    }
    if (!(period > 0) || start <= 0 || start >= period) return null;
    ensureDefs(state);
    var id = "pv-export-tex-" + (++state.textures);
    var pat = svgNode("pattern");
    pat.setAttribute("id", id);
    pat.setAttribute("patternUnits", "userSpaceOnUse");
    pat.setAttribute("width", period);
    pat.setAttribute("height", period);
    /* A CSS gradient angle points along the gradient axis measured from
       "up"; the SVG tile draws its stripe along y and offsets it along
       x, so rotating by (angle - 90) puts the stripes on the same
       diagonal the CSS renders. */
    pat.setAttribute("patternTransform", "rotate(" + (am[1] - 90) + ")");
    if (ground) {
      var bg = svgNode("rect");
      bg.setAttribute("width", period);
      bg.setAttribute("height", period);
      bg.setAttribute("fill", ground);
      pat.appendChild(bg);
    }
    var stripe = svgNode("rect");
    stripe.setAttribute("x", start);
    stripe.setAttribute("width", period - start);
    stripe.setAttribute("height", period);
    stripe.setAttribute("fill", line);
    pat.appendChild(stripe);
    state.defs.appendChild(pat);
    return "url(#" + id + ")";
  }

  /* One rendered line box per entry: {text, rect}. Header text is a
     single line, but the source line can wrap on narrow charts, and one
     <text> per line is the only way SVG can reproduce that. The wrapped
     case grows a range one character at a time and cuts a line every
     time the browser starts a new line box. */
  function textLines(node) {
    var s = String(node.nodeValue);
    var range = document.createRange();
    range.selectNodeContents(node);
    var whole = range.getBoundingClientRect();
    if (!whole.width && !whole.height) return [];
    if (range.getClientRects().length <= 1 || s.length > 400) {
      return [{ text: s, rect: whole }];
    }
    var out = [], lineStart = 0;
    for (var i = 1; i <= s.length; i++) {
      range.setStart(node, lineStart);
      range.setEnd(node, i);
      if (range.getClientRects().length > 1) {
        range.setEnd(node, i - 1);
        out.push({ text: s.slice(lineStart, i - 1),
          rect: range.getBoundingClientRect() });
        lineStart = i - 1;
      }
    }
    range.setStart(node, lineStart);
    range.setEnd(node, s.length);
    out.push({ text: s.slice(lineStart),
      rect: range.getBoundingClientRect() });
    return out;
  }

  function exportText(state, node) {
    var parent = node.parentElement;
    if (!parent || !String(node.nodeValue).trim()) return;
    var cs = getComputedStyle(parent);
    textLines(node).forEach(function (line) {
      var text = line.text.replace(/\s+/g, " ").trim();
      if (!text) return;
      var t = svgNode("text");
      t.setAttribute("x", round2(line.rect.left - state.base.left));
      /* SVG places text by its baseline. For the fonts in play the
         baseline sits at about 80% of the measured line box, and an
         explicit y stays portable - dominant-baseline support is patchy
         outside browsers. */
      t.setAttribute("y", round2(line.rect.top - state.base.top +
        line.rect.height * 0.8));
      t.setAttribute("fill", cs.color);
      t.setAttribute("font-size", cs.fontSize);
      if (cs.fontWeight !== "400" && cs.fontWeight !== "normal") {
        t.setAttribute("font-weight", cs.fontWeight);
      }
      if (cs.fontStyle && cs.fontStyle !== "normal") {
        t.setAttribute("font-style", cs.fontStyle);
      }
      if (cs.letterSpacing && cs.letterSpacing !== "normal") {
        t.setAttribute("letter-spacing", cs.letterSpacing);
      }
      if (cs.fontVariantNumeric && cs.fontVariantNumeric !== "normal") {
        t.setAttribute("style",
          "font-variant-numeric:" + cs.fontVariantNumeric + ";");
      }
      t.textContent = text;
      state.root.appendChild(t);
    });
  }

  /* A plot svg goes in whole, as a nested <svg> pinned to the spot it
     occupies in the widget - that keeps its own coordinate system and
     clip paths intact without touching any of its content. */
  function exportPlotSvg(state, el) {
    var r = el.getBoundingClientRect();
    var clone = el.cloneNode(true);
    clone.setAttribute("x", round2(r.left - state.base.left));
    clone.setAttribute("y", round2(r.top - state.base.top));
    if (!clone.getAttribute("width")) {
      clone.setAttribute("width", round2(r.width));
    }
    if (!clone.getAttribute("height")) {
      clone.setAttribute("height", round2(r.height));
    }
    state.root.appendChild(clone);
  }

  function exportWalk(state, node) {
    if (node.nodeType === 3) { exportText(state, node); return; }
    if (node.nodeType !== 1) return;
    var tag = node.tagName.toLowerCase();
    if (tag === "svg") { exportPlotSvg(state, node); return; }
    /* Interactive controls make no sense in a static file, and the
       tooltip is chrome for the pointer, not part of the chart. */
    if (tag === "button" || tag === "input" || tag === "select" ||
        tag === "canvas" || tag === "script" || tag === "style") return;
    if (node.classList && node.classList.contains("pv-tooltip")) return;
    var cs = getComputedStyle(node);
    if (cs.display === "none" || cs.visibility === "hidden" ||
        parseFloat(cs.opacity) === 0) return;
    var r = node.getBoundingClientRect();
    if (r.width > 0 && r.height > 0) {
      var fill = null;
      if (cs.backgroundImage && cs.backgroundImage.indexOf("gradient") >= 0) {
        /* A repeating gradient is a textured legend swatch; letting the
           plain-gradient path at it would flatten the hatch into one
           horizontal fade, so it gets its own rebuilder (which falls
           back to the solid ground below when it cannot parse). */
        fill = cs.backgroundImage.indexOf("repeating-linear-gradient") >= 0 ?
          hatchFill(state, cs.backgroundImage, realBg(cs.backgroundColor)) :
          gradientFill(state, cs.backgroundImage);
      }
      if (!fill) fill = realBg(cs.backgroundColor);
      if (fill) {
        var rect = svgNode("rect");
        rect.setAttribute("x", round2(r.left - state.base.left));
        rect.setAttribute("y", round2(r.top - state.base.top));
        rect.setAttribute("width", round2(r.width));
        rect.setAttribute("height", round2(r.height));
        var radius = parseFloat(cs.borderTopLeftRadius) || 0;
        if (radius) rect.setAttribute("rx", round2(radius));
        rect.setAttribute("fill", fill);
        state.root.appendChild(rect);
      }
    }
    for (var i = 0; i < node.childNodes.length; i++) {
      /* A node the walk cannot handle is dropped, never fatal - an
         export with one oddity missing beats no export. */
      try { exportWalk(state, node.childNodes[i]); } catch (e) {}
    }
  }

  /* Serialise a rendered widget (the root element htmlwidgets renders
     into) as one standalone SVG document string: header, legend, plot,
     and source line together, ready to drop into a paper. `x` is the
     payload the widget was rendered from (falls back to the copy the
     binding stores on the element) and `theme` the resolved colour set;
     both may be null, the DOM itself carries everything essential. */
  pv.toStandaloneSvg = function (el, x, theme) {
    try {
      x = x || el.__pvLastX || {};
      var base = el.getBoundingClientRect();
      var w = Math.round(base.width || el.offsetWidth || 640);
      var h = Math.round(base.height || el.offsetHeight || 400);
      var root = svgNode("svg");
      root.setAttributeNS("http://www.w3.org/2000/xmlns/", "xmlns:xlink",
        "http://www.w3.org/1999/xlink");
      root.setAttribute("width", w);
      root.setAttribute("height", h);
      root.setAttribute("viewBox", "0 0 " + w + " " + h);
      var font = (x.theme && x.theme.font) ||
        '"InterVariable", "Inter", system-ui, -apple-system, ' +
        '"Segoe UI", sans-serif';
      root.setAttribute("style", "font-family:" + font + ";");
      var surface = (theme && theme.ink && theme.ink.surface) ||
        realBg(getComputedStyle(el).backgroundColor) || "#ffffff";
      var bg = svgNode("rect");
      bg.setAttribute("width", w);
      bg.setAttribute("height", h);
      bg.setAttribute("fill", surface);
      root.appendChild(bg);
      var state = { root: root, base: base, defs: null, gradients: 0,
        textures: 0 };
      for (var i = 0; i < el.childNodes.length; i++) {
        try { exportWalk(state, el.childNodes[i]); } catch (e) {}
      }
      return '<?xml version="1.0" encoding="UTF-8"?>\n' +
        new XMLSerializer().serializeToString(root);
    } catch (e) {
      /* Whatever went wrong, hand back a valid (if empty) document. */
      return '<?xml version="1.0" encoding="UTF-8"?>\n' +
        '<svg xmlns="' + SVG_NS + '" width="640" height="400"></svg>';
    }
  };

  /* Rasterise a standalone SVG string to a PNG blob, `scale` times the
     given pixel size, and pass the blob to `callback` (null when the
     browser refuses). The svg loads through an <img>, which renders it
     in an isolated document - page web fonts don't reach it, so the
     font stack's system fallbacks carry the raster. */
  pv.svgToPngBlob = function (svgString, width, height, scale, callback) {
    scale = scale || 2;
    var svgBlob = new Blob([svgString],
      { type: "image/svg+xml;charset=utf-8" });
    var url = URL.createObjectURL(svgBlob);
    var img = new Image();
    img.onload = function () {
      try {
        var canvas = document.createElement("canvas");
        canvas.width = Math.round(width * scale);
        canvas.height = Math.round(height * scale);
        canvas.getContext("2d")
          .drawImage(img, 0, 0, canvas.width, canvas.height);
        URL.revokeObjectURL(url);
        canvas.toBlob(function (png) { callback(png); }, "image/png");
      } catch (e) {
        URL.revokeObjectURL(url);
        callback(null);
      }
    };
    img.onerror = function () {
      URL.revokeObjectURL(url);
      callback(null);
    };
    img.src = url;
  };

  /* Hand a file to the browser's save machinery: a temporary anchor
     pointing at an object URL, clicked, then cleaned up. Text content
     is wrapped in a blob of the given mime type first. */
  pv.triggerDownload = function (data, filename, mimeType) {
    var blob = data instanceof Blob ? data :
      new Blob([data], { type: mimeType || "application/octet-stream" });
    var url = URL.createObjectURL(blob);
    var a = document.createElement("a");
    a.href = url;
    a.download = filename;
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    /* The revoke waits a beat: some browsers read the URL after the
       click returns. */
    setTimeout(function () { URL.revokeObjectURL(url); }, 1000);
  };

  /* ---------- download control ----------
     A chart's in-page save UI: a small arrow button in the top-right
     corner opening a two-entry menu, "Download SVG" and "Download PNG
     (2x)", wired to the export helpers above. pvchart.js builds one per
     chart unless the R side turned downloads off. Every painted surface
     of the control - the button face, each menu entry - is a <button>,
     which the SVG exporter skips by tag, and the wrappers around them
     are transparent; on top of that the control hides itself for the
     moment of serialisation. So it never shows up inside a saved
     chart. */

  /* Turns a chart title into a safe filename: lower-case ascii with
     hyphens between words. Diacritics are stripped rather than dropped,
     so "Bevölkerung" becomes "bevolkerung". Returns "" when nothing
     usable is left. */
  pv.slugify = function (s) {
    s = String(s == null ? "" : s);
    if (s.normalize) {
      s = s.normalize("NFKD").replace(/[\u0300-\u036f]/g, "");
    }
    return s.toLowerCase().replace(/[^a-z0-9]+/g, "-")
      .replace(/^-+|-+$/g, "");
  };

  pv.buildDownloadControl = function (el, x, theme) {
    var ink = theme.ink;
    var reducedMotion = window.matchMedia &&
      window.matchMedia("(prefers-reduced-motion: reduce)").matches;
    /* Touch screens have no hover to reveal the control with, so on
       coarse pointers it simply stays visible. */
    var coarse = window.matchMedia &&
      window.matchMedia("(pointer: coarse)").matches;

    var box = document.createElement("div");
    box.className = "pv-download";
    box.style.cssText =
      "position:absolute;top:8px;right:8px;z-index:9;opacity:0;" +
      (reducedMotion ? "" : "transition:opacity 140ms ease;");

    var btn = document.createElement("button");
    btn.type = "button";
    btn.title = "Download this chart";
    btn.setAttribute("aria-label", "Download this chart");
    btn.setAttribute("aria-haspopup", "true");
    btn.setAttribute("aria-expanded", "false");
    btn.style.cssText =
      "display:flex;align-items:center;justify-content:center;" +
      "width:26px;height:24px;padding:0;border-radius:6px;" +
      "border:1px solid " + ink.baseline + ";" +
      "background:" + ink.surface + ";color:" + ink.secondary + ";" +
      "cursor:pointer;";
    btn.innerHTML =
      '<svg width="12" height="12" viewBox="0 0 12 12" aria-hidden="true">' +
      '<path d="M6 1.2v6M3.3 4.9 6 7.6l2.7-2.7M1.9 10.4h8.2" ' +
      'fill="none" stroke="currentColor" stroke-width="1.4" ' +
      'stroke-linecap="round" stroke-linejoin="round"/></svg>';
    box.appendChild(btn);

    /* The menu wrapper only positions; each entry paints its own card.
       That keeps every coloured surface on a <button>. */
    var menu = document.createElement("div");
    menu.style.cssText =
      "position:absolute;top:100%;right:0;margin-top:4px;display:none;";
    box.appendChild(menu);

    function setOpen(open) {
      menu.style.display = open ? "block" : "none";
      btn.setAttribute("aria-expanded", open ? "true" : "false");
    }
    btn.addEventListener("click", function (ev) {
      ev.stopPropagation();
      setOpen(menu.style.display === "none");
    });
    btn.addEventListener("mouseenter", function () {
      btn.style.background = ink.grid;
    });
    btn.addEventListener("mouseleave", function () {
      btn.style.background = ink.surface;
    });

    /* Base of the saved file's name: the chart's title when it has one,
       the element id otherwise, a generic fallback last. */
    function filename(ext) {
      var base = pv.slugify(x.title) || pv.slugify(el.id) ||
        "polyviz-chart";
      return base + ext;
    }

    /* The control must never appear in its own export, so it steps out
       of the DOM walk (which is synchronous) and back in right after. */
    function snapshot() {
      var prev = box.style.display;
      box.style.display = "none";
      var svg = pv.toStandaloneSvg(el, x, theme);
      box.style.display = prev;
      return svg;
    }

    function item(label, onPick) {
      var it = document.createElement("button");
      it.type = "button";
      it.textContent = label;
      it.style.cssText =
        "display:block;width:100%;box-sizing:border-box;margin:0 0 3px 0;" +
        "padding:6px 11px;border:1px solid " + ink.tooltipBorder + ";" +
        "border-radius:7px;background:" + ink.tooltipBg + ";" +
        "color:" + ink.tooltipText + ";font:inherit;font-size:12px;" +
        "text-align:left;cursor:pointer;white-space:nowrap;" +
        "box-shadow:0 4px 16px rgba(0,0,0,0.16);";
      it.addEventListener("mouseenter", function () {
        it.style.background = ink.grid;
      });
      it.addEventListener("mouseleave", function () {
        it.style.background = ink.tooltipBg;
      });
      it.addEventListener("click", function (ev) {
        ev.stopPropagation();
        setOpen(false);
        onPick();
      });
      menu.appendChild(it);
    }

    item("Download SVG", function () {
      pv.triggerDownload(snapshot(), filename(".svg"),
        "image/svg+xml;charset=utf-8");
    });
    item("Download PNG (2x)", function () {
      var r = el.getBoundingClientRect();
      var w = Math.round(r.width || el.offsetWidth || 640);
      var h = Math.round(r.height || el.offsetHeight || 400);
      pv.svgToPngBlob(snapshot(), w, h, 2, function (png) {
        if (png) pv.triggerDownload(png, filename(".png"));
      });
    });

    /* Reveal on hover or keyboard focus. The listeners go on the widget
       element once - it survives re-renders, the control does not - and
       find the current control through el.__pvDlBox each time. */
    el.__pvDlBox = box;
    if (coarse) {
      box.style.opacity = 1;
    } else if (!el.__pvDlWired) {
      el.__pvDlWired = true;
      var setVisible = function (on) {
        var b = el.__pvDlBox;
        if (!b || !b.parentNode) return;
        b.style.opacity = on ? 1 : 0;
        if (!on) {
          var m = b.lastChild;
          if (m) m.style.display = "none";
          b.firstChild.setAttribute("aria-expanded", "false");
        }
      };
      el.addEventListener("mouseenter", function () { setVisible(true); });
      el.addEventListener("mouseleave", function () { setVisible(false); });
      el.addEventListener("focusin", function () { setVisible(true); });
      el.addEventListener("focusout", function (ev) {
        /* Focus hopping between the button and a menu entry stays
           inside the widget; only leaving it hides the control. */
        if (!el.contains(ev.relatedTarget)) setVisible(false);
      });
    }
    el.appendChild(box);
    return box;
  };

  /* ---------- texture patterns ----------
     A second identity channel for filled marks (pv_textures on the R
     side): hand-drawn-feel diagonal hatching in the slot's own colour
     over a ground of that colour faded toward the surface. Colour still
     reads at a glance on screen; the hatch angle and spacing survive
     greyscale printing and colour-vision deficiency, where solid hues
     collapse into each other. The SVG patterns are registered inside
     each plot's own <defs>, so the standalone-svg exporter (which clones
     the plot svg whole) carries them along for free. Legend swatches are
     HTML spans, so they wear the matching CSS repeating-gradient
     instead - and the exporter rebuilds that as an SVG pattern. */

  /* A slot's texture is a pure function of its number, so a series
     always wears the same hatch: neighbouring slots alternate between
     the two diagonals - adjacent stacked segments never share an angle -
     and each pair of slots opens the line spacing a step, so slots two
     apart differ as well. */
  function texAngle(slot) { return slot % 2 ? -45 : 45; }
  function texSpacing(slot) { return 5.5 + 1.5 * Math.floor(slot / 2); }
  var TEX_STROKE = 1.2;   /* hatch line weight, px */
  var TEX_WAVE = 12;      /* wave period along the line, px */
  var TEX_AMP = 0.7;      /* wave amplitude - the hand-drawn wobble */
  var TEX_GROUND = 0.68;  /* how far the ground fades toward the surface */

  /* The solid the hatching sits on. Fading the slot colour toward the
     surface lightens it in light mode and darkens it in dark mode - the
     right move in both - and keeps even marks too small to catch a hatch
     line (thin stacked segments, slim donut slices) carrying colour. */
  pv.textureGround = function (color, theme) {
    return d3.interpolateRgb(color, theme.ink.surface)(TEX_GROUND);
  };

  /* Registers slot `slotIndex`'s hatch pattern in the given <defs>
     selection - once; repeat calls reuse it - and returns the url(#...)
     fill string for it. The tile is the ground plus one gently waving
     line, drawn vertically and rotated onto the slot's diagonal. The
     wave's two ends share a tangent, so the line stays smooth where
     tiles join. Ids carry a random base per defs, so two charts on one
     page can never capture each other's patterns. */
  pv.texturePattern = function (defs, slotIndex, color, theme) {
    var node = defs.node();
    if (!node.__pvTexBase) {
      node.__pvTexBase = "pv-tex-" + Math.floor(Math.random() * 1e9);
    }
    var id = node.__pvTexBase + "-" + slotIndex;
    if (!node.querySelector("#" + id)) {
      var s = texSpacing(slotIndex);
      var p = defs.append("pattern")
        .attr("id", id)
        .attr("patternUnits", "userSpaceOnUse")
        .attr("width", s).attr("height", TEX_WAVE)
        .attr("patternTransform", "rotate(" + texAngle(slotIndex) + ")");
      p.append("rect")
        .attr("width", s).attr("height", TEX_WAVE)
        .attr("fill", pv.textureGround(color, theme));
      var x = s / 2;
      p.append("path")
        .attr("d", "M" + x + ",0" +
          "C" + (x + TEX_AMP) + "," + (TEX_WAVE / 3) + " " +
          (x - TEX_AMP) + "," + (2 * TEX_WAVE / 3) + " " +
          x + "," + TEX_WAVE)
        .attr("fill", "none")
        .attr("stroke", color)
        .attr("stroke-width", TEX_STROKE);
    }
    return "url(#" + id + ")";
  };

  /* The same texture as CSS, for the HTML legend swatches: the ground as
     the background colour, the hatch as a repeating gradient over it. A
     CSS gradient angle of svg+90 puts the stripes on the same diagonal
     the rotated SVG tile draws; the stripes are straight, because at
     swatch size the wave would not show anyway. */
  pv.textureSwatchCss = function (slotIndex, color, theme) {
    var s = texSpacing(slotIndex);
    return {
      color: pv.textureGround(color, theme),
      image: "repeating-linear-gradient(" + (texAngle(slotIndex) + 90) +
        "deg,transparent 0,transparent " + (s - TEX_STROKE) + "px," +
        color + " " + (s - TEX_STROKE) + "px," + color + " " + s + "px)"
    };
  };

  /* Renderer conveniences. With textures off - the default, and any
     payload built before the flag existed - both return null, and the
     renderer's fill logic stays exactly what it always was. With them
     on, textureFill hands back a name-to-fill lookup of pattern urls
     (building the plot's defs as it goes) and textureLegend the
     matching swatch-css lookup for pv.buildLegend. Both take the colour
     scale's FULL domain and recycle slots the way the scale recycles
     hues, so texture and colour always travel together. */
  pv.textureFill = function (ctx, svg, names, colorOf) {
    if (ctx.x.textures !== true) return null;
    var defs = svg.append("defs");
    var map = {};
    names.forEach(function (nm, i) {
      map[nm] = pv.texturePattern(defs, i % ctx.theme.palette.length,
        colorOf(nm), ctx.theme);
    });
    return function (nm) { return map[nm]; };
  };

  pv.textureLegend = function (ctx, names, colorOf) {
    if (ctx.x.textures !== true) return null;
    var map = {};
    names.forEach(function (nm, i) {
      map[nm] = pv.textureSwatchCss(i % ctx.theme.palette.length,
        colorOf(nm), ctx.theme);
    });
    return function (nm) { return map[nm]; };
  };

  return pv;
})();

/*
 * Brush-to-zoom context strip, shared by the line and area renderers.
 * The strip is a second, subdued drawing of the whole series below the
 * main panel, wearing a d3.brushX: dragging across it narrows the main
 * panel's x domain (through the same explicit-domain override the facet
 * renderer's xlim uses), and double-clicking it - or clicking outside
 * the brushed window - restores the full range. The active window lives
 * on the widget element itself (el.__pvZoomX), so it survives the full
 * re-renders that resizes and theme changes trigger.
 */
(function () {

  /* The strip is pointer chrome, like the tooltip and the download
     control: it must never appear inside a saved chart. The exporter
     walk skips hidden elements before it descends into them, so every
     .pv-zoom wrapper steps out of the DOM for the (synchronous)
     serialisation and back in right after - the same trick the download
     control plays on itself. */
  var serialise = pv.toStandaloneSvg;
  pv.toStandaloneSvg = function (el, x, theme) {
    var strips = el && el.querySelectorAll ?
      el.querySelectorAll(".pv-zoom") : [];
    var shown = [];
    for (var i = 0; i < strips.length; i++) {
      shown.push(strips[i].style.display);
      strips[i].style.display = "none";
    }
    var out = serialise(el, x, theme);
    for (var j = 0; j < strips.length; j++) {
      strips[j].style.display = shown[j];
    }
    return out;
  };

  /* The pixel height a renderer must reserve for the strip: a small gap
     to sit clear of the main panel's x axis title, then the strip. */
  pv.ZOOM_STRIP_H = 46;
  pv.ZOOM_STRIP_GAP = 4;

  /* Builds the strip below whatever the renderer has drawn. The caller
     hands over its own margins (so the strip's x range lines up exactly
     under the main plot), the axis type with the FULL x extent (the
     domain before any brush narrowed it), the currently active window,
     and a draw callback that paints the chart-specific muted miniature.
     Only date and number axes ever get here - a category axis has no
     continuous window to brush, and the R side refuses it. */
  pv.zoomStrip = function (ctx, o) {
    var box = document.createElement("div");
    box.className = "pv-zoom";
    box.style.cssText = "margin-top:" + pv.ZOOM_STRIP_GAP + "px;";
    var svg = d3.select(box).append("svg")
      .attr("width", ctx.width).attr("height", pv.ZOOM_STRIP_H)
      .style("display", "block");
    var iw = Math.max(10, ctx.width - o.left - o.right);
    var ih = pv.ZOOM_STRIP_H - 8;
    var g = svg.append("g")
      .attr("transform", "translate(" + o.left + ",4)");

    var sx = (o.xtype === "date" ? d3.scaleTime() : d3.scaleLinear())
      .domain(o.extent).range([0, iw]);
    /* Date windows travel as millisecond numbers (JSON-safe, and finer
       than the day-level ISO strings the data itself uses). */
    var toX = o.xtype === "date" ?
      function (v) { return new Date(v); } : Number;

    o.draw(g, sx, iw, ih);

    function redraw() {
      /* The whole widget re-renders, exactly as a crosstalk selection
         or a resize would - that is what routes the new window through
         the renderer's explicit-domain path. A brush update must not
         replay the entry animation, so the payload's duration is
         silenced for just this synchronous render. */
      var x = ctx.el.__pvLastX || ctx.x;
      var keep = x.duration;
      x.duration = 0;
      window.pvRender(ctx.el, x,
        ctx.el.offsetWidth || ctx.width, ctx.el.offsetHeight,
        window.matchMedia ?
          window.matchMedia("(prefers-color-scheme: dark)") : null);
      x.duration = keep;
    }

    function onEnd(event) {
      /* brush.move (restoring the window below) fires this too, with no
         sourceEvent; reacting to it would loop the re-render. */
      if (!event.sourceEvent) return;
      var z = null;
      /* A window under 2px is a click, not a selection: reset. */
      if (event.selection &&
          event.selection[1] - event.selection[0] >= 2) {
        var lo = sx.invert(event.selection[0]);
        var hi = sx.invert(event.selection[1]);
        z = o.xtype === "date" ? [+lo, +hi] : [lo, hi];
      }
      var had = ctx.el.__pvZoomX;
      ctx.el.__pvZoomX = z;
      /* Only a changed window earns a re-render: a zero-move click on
         the selection hands back the same window, and re-rendering then
         would tear the strip out from under a double-click. */
      if (JSON.stringify(z) !== JSON.stringify(had || null)) redraw();
    }

    var brush = d3.brushX().extent([[0, 0], [iw, ih]]).on("end", onEnd);
    var gb = g.append("g").attr("class", "brush").call(brush);
    /* d3's stock brush chrome is loud; restyle it to the quiet ink. */
    gb.select(".selection")
      .attr("fill", ctx.theme.ink.muted).attr("fill-opacity", 0.18)
      .attr("stroke", ctx.theme.ink.baseline);
    gb.on("dblclick.pvzoom", function () {
      if (ctx.el.__pvZoomX) {
        ctx.el.__pvZoomX = null;
        redraw();
      }
    });
    /* A re-render (resize, theme flip, the brush itself) rebuilds the
       strip from scratch, so the active window is painted back on. */
    if (o.zoom) {
      gb.call(brush.move, [sx(toX(o.zoom[0])), sx(toX(o.zoom[1]))]);
    }

    ctx.el.appendChild(box);
    return box;
  };

})();
