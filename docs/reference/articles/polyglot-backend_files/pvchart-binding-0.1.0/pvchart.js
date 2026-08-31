/*
 * This file is the JavaScript half of polyviz. You never call it directly:
 * the R functions (pv_bar, pv_line, ...) bundle up your data plus a "type"
 * label, htmlwidgets ships it over, and the code below hands it to the
 * matching d3 renderer.
 *
 * Layout of the file:
 *   - the widget wiring (renders on load, re-renders on resize and when
 *     the system switches between light and dark mode)
 *   - shared chrome: header, legend, tooltip, axis styling
 *   - one render function per chart type
 */
HTMLWidgets.widget({
  name: "pvchart",
  type: "output",

  factory: function (el, width, height) {
    var lastX = null;
    var mq = window.matchMedia ?
      window.matchMedia("(prefers-color-scheme: dark)") : null;

    function draw(w, h) {
      if (!lastX) return;
      render(el, lastX, w || el.offsetWidth || width,
             h || el.offsetHeight || height, mq);
    }
    if (mq && mq.addEventListener) {
      mq.addEventListener("change", function () { draw(); });
    }

    return {
      renderValue: function (x) { lastX = x; draw(); },
      resize: function (w, h) { draw(w, h); }
    };
  }
});

function render(el, x, width, height, mq) {
  /* Pick the light or dark colour set. "auto" follows the viewer's own
     system preference; the R side can also force one mode. */
  var mode = x.mode === "auto" ? (mq && mq.matches ? "dark" : "light") : x.mode;
  var theme = {
    mode: mode,
    palette: x.theme.categorical[mode],
    ink: x.theme.ink[mode]
  };

  /* Start from a blank container every time - re-rendering is cheaper to
     reason about than patching an existing drawing. */
  el.innerHTML = "";
  el.style.position = "relative";
  el.style.background = theme.ink.surface;
  el.style.fontFamily = 'system-ui, -apple-system, "Segoe UI", sans-serif';

  var header = buildHeader(el, x, theme);
  var innerH = Math.max(120, height - header.offsetHeight);
  var tip = buildTooltip(el, theme);

  var ctx = {
    el: el, x: x, theme: theme, tip: tip, header: header,
    width: width, height: innerH,
    duration: x.duration == null ? 700 : x.duration,
    fmt: d3.format(",.2~f")
  };

  var renderers = {
    bar: renderBar, line: renderLine, scatter: renderScatter,
    force: renderForce, chord: renderChord, sunburst: renderSunburst
  };
  renderers[x.type](ctx);
}

/* ---------- shared chrome ----------
 * The title, legend and tooltip are ordinary HTML sitting above and over
 * the SVG. HTML text is easier to style and wraps on its own. */

function buildHeader(el, x, theme) {
  var header = document.createElement("div");
  header.style.cssText = "padding:10px 12px 6px 12px;";
  if (x.title) {
    var t = document.createElement("div");
    t.textContent = x.title;
    t.style.cssText = "font-size:15px;font-weight:700;color:" +
      theme.ink.primary + ";";
    header.appendChild(t);
  }
  if (x.subtitle) {
    var s = document.createElement("div");
    s.textContent = x.subtitle;
    s.style.cssText = "font-size:12px;margin-top:2px;color:" +
      theme.ink.secondary + ";";
    header.appendChild(s);
  }
  el.appendChild(header);
  return header;
}

function buildLegend(header, names, colorOf, theme) {
  var row = document.createElement("div");
  row.style.cssText =
    "display:flex;flex-wrap:wrap;gap:4px 14px;margin-top:6px;";
  names.forEach(function (nm) {
    var item = document.createElement("span");
    item.style.cssText =
      "display:inline-flex;align-items:center;gap:5px;font-size:12px;color:" +
      theme.ink.secondary + ";";
    var sw = document.createElement("span");
    sw.style.cssText =
      "width:10px;height:10px;border-radius:2px;background:" + colorOf(nm) + ";";
    item.appendChild(sw);
    item.appendChild(document.createTextNode(nm));
    row.appendChild(item);
  });
  header.appendChild(row);
}

function buildTooltip(el, theme) {
  var tip = document.createElement("div");
  tip.style.cssText =
    "position:absolute;pointer-events:none;opacity:0;z-index:10;" +
    "background:" + theme.ink.surface + ";color:" + theme.ink.primary + ";" +
    "border:1px solid " + theme.ink.baseline + ";border-radius:6px;" +
    "padding:6px 9px;font-size:12px;line-height:1.5;max-width:260px;" +
    "box-shadow:0 2px 10px rgba(0,0,0,0.18);transition:opacity 120ms;";
  el.appendChild(tip);
  return tip;
}

function showTip(ctx, event, html) {
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
}

function hideTip(ctx) { ctx.tip.style.opacity = 0; }

function swatchRow(color, name, value) {
  return '<span style="display:inline-block;width:9px;height:9px;' +
    'border-radius:2px;margin-right:5px;background:' + color + ';"></span>' +
    esc(name) + ': <b>' + value + "</b>";
}

function esc(s) {
  return String(s).replace(/&/g, "&amp;").replace(/</g, "&lt;")
    .replace(/>/g, "&gt;");
}

function baseSvg(ctx) {
  return d3.select(ctx.el).append("svg")
    .attr("width", ctx.width).attr("height", ctx.height)
    .style("display", "block");
}

function styleAxis(sel, theme, keepDomain) {
  sel.selectAll(".domain")
    .attr("stroke", keepDomain ? theme.ink.baseline : "none");
  sel.selectAll("line").attr("stroke", theme.ink.baseline);
  sel.selectAll("text")
    .attr("fill", theme.ink.muted)
    .style("font-size", "11px");
}

function yGrid(g, yScale, innerW, theme) {
  var grid = g.append("g")
    .call(d3.axisLeft(yScale).ticks(5).tickSize(-innerW).tickFormat(""));
  grid.selectAll("line").attr("stroke", theme.ink.grid);
  grid.selectAll(".domain").remove();
  return grid;
}

/* Compact tick labels: 6M instead of 6,000,000; plain numbers below 10k. */
function fmtTick(v) {
  return Math.abs(v) >= 10000 ? d3.format("~s")(v) : d3.format(",")(v);
}

function axisLabels(svg, ctx, m, innerW, innerH, xlab, ylab) {
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
}

/* Draws a bar as an SVG path where only the top two corners are rounded.
   A plain <rect> with rounded corners would round the bottom too, and the
   bottom edge should sit flat on the axis baseline. */
function topRoundedBar(x0, y0, w, h, r) {
  r = Math.max(0, Math.min(r, w / 2, h));
  return "M" + x0 + "," + (y0 + h) +
    "v" + (-(h - r)) +
    "a" + r + "," + r + " 0 0 1 " + r + "," + (-r) +
    "h" + (w - 2 * r) +
    "a" + r + "," + r + " 0 0 1 " + r + "," + r +
    "v" + (h - r) + "z";
}

/* ---------- bar ---------- */

function renderBar(ctx) {
  var data = ctx.x.data;
  var grouped = data.length && data[0].series !== undefined;
  var cats = uniq(data.map(function (d) { return d.x; }));
  var seriesNames = grouped ?
    uniq(data.map(function (d) { return d.series; })) : [];
  var color = d3.scaleOrdinal()
    .domain(grouped ? seriesNames : ["value"])
    .range(ctx.theme.palette);
  if (grouped) {
    buildLegend(ctx.header, seriesNames, color, ctx.theme);
    ctx.height = Math.max(120, ctx.height - 26);
  }

  var m = { top: 12, right: 14, bottom: 52, left: 58 };
  var iw = ctx.width - m.left - m.right,
      ih = ctx.height - m.top - m.bottom;
  var svg = baseSvg(ctx);
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

  yGrid(g, y, iw, ctx.theme);
  g.append("g").attr("transform", "translate(0," + ih + ")")
    .call(d3.axisBottom(x0).tickSizeOuter(0))
    .call(function (s) { styleAxis(s, ctx.theme, true); });
  g.append("g").call(d3.axisLeft(y).ticks(5).tickFormat(fmtTick))
    .call(function (s) { styleAxis(s, ctx.theme, false); });
  axisLabels(svg, ctx, m, iw, ih, ctx.x.xlab, ctx.x.ylab);

  /* Each bar starts at zero height and grows up to its value, with a
     small stagger from left to right so the chart builds across. */
  var bars = g.selectAll("path.bar").data(data).enter().append("path")
    .attr("class", "bar")
    .attr("fill", function (d) { return color(grouped ? d.series : "value"); })
    .attr("d", function (d) {
      var bx = grouped ? x0(d.x) + x1(d.series) : x0(d.x);
      var bw = grouped ? x1.bandwidth() : x0.bandwidth();
      return topRoundedBar(bx, ih, bw, 0, 4);
    });

  bars.transition().duration(ctx.duration)
    .delay(function (d, i) { return Math.min(i * 24, 600); })
    .ease(d3.easeCubicOut)
    .attrTween("d", function (d) {
      var bx = grouped ? x0(d.x) + x1(d.series) : x0(d.x);
      var bw = grouped ? x1.bandwidth() : x0.bandwidth();
      var hFinal = ih - y(d.y);
      return function (t) {
        return topRoundedBar(bx, y(d.y) + (1 - t) * hFinal, bw, t * hFinal, 4);
      };
    });

  /* Hovering a bar dims the others and shows its exact value. */
  bars
    .on("pointerenter pointermove", function (event, d) {
      bars.attr("opacity", function (b) { return b === d ? 1 : 0.55; });
      var head = grouped ?
        esc(d.x) + " &middot; " + esc(d.series) : esc(d.x);
      showTip(ctx, event, head + "<br>" +
        swatchRow(color(grouped ? d.series : "value"),
                  ctx.x.ylab || "value", ctx.fmt(d.y)));
    })
    .on("pointerleave", function () {
      bars.attr("opacity", 1);
      hideTip(ctx);
    });
}

/* ---------- line ---------- */

function renderLine(ctx) {
  var xtype = ctx.x.xtype;
  var parse = xtype === "date" ? d3.timeParse("%Y-%m-%d") : null;
  var data = ctx.x.data.map(function (d) {
    return {
      x: xtype === "date" ? parse(d.x) : d.x,
      y: d.y, series: d.series
    };
  });
  var seriesNames = uniq(data.map(function (d) { return d.series; }));
  var color = d3.scaleOrdinal().domain(seriesNames).range(ctx.theme.palette);
  if (ctx.x.showLegend && seriesNames.length > 1) {
    buildLegend(ctx.header, seriesNames, color, ctx.theme);
    ctx.height = Math.max(120, ctx.height - 26);
  }

  var directLabels = seriesNames.length > 1 && seriesNames.length <= 4;
  var m = { top: 12, right: directLabels ? 90 : 24, bottom: 52, left: 58 };
  var iw = ctx.width - m.left - m.right,
      ih = ctx.height - m.top - m.bottom;
  var svg = baseSvg(ctx);
  var g = svg.append("g").attr("transform",
    "translate(" + m.left + "," + m.top + ")");

  var xScale;
  if (xtype === "category") {
    xScale = d3.scalePoint()
      .domain(uniq(data.map(function (d) { return d.x; })))
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

  yGrid(g, y, iw, ctx.theme);
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
  }
  g.append("g").attr("transform", "translate(0," + ih + ")")
    .call(xAxis).call(function (s) { styleAxis(s, ctx.theme, true); });
  g.append("g").call(d3.axisLeft(y).ticks(5).tickFormat(fmtTick))
    .call(function (s) { styleAxis(s, ctx.theme, false); });
  axisLabels(svg, ctx, m, iw, ih, ctx.x.xlab, ctx.x.ylab);

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

    if (directLabels && s.points.length) {
      var last = s.points[s.points.length - 1];
      g.append("circle")
        .attr("cx", xScale(last.x)).attr("cy", y(last.y)).attr("r", 3.5)
        .attr("fill", color(s.name))
        .attr("stroke", ctx.theme.ink.surface).attr("stroke-width", 2);
      g.append("text")
        .attr("x", xScale(last.x) + 8).attr("y", y(last.y))
        .attr("dominant-baseline", "middle")
        .attr("fill", ctx.theme.ink.secondary).style("font-size", "11px")
        .text(s.name);
    }
  });

  /* The crosshair: an invisible rectangle covers the plot and tracks the
     pointer. On every move we find the nearest x position, drop a dashed
     vertical line there, mark each series with a dot, and list all their
     values in one tooltip. */
  var cross = g.append("line")
    .attr("y1", 0).attr("y2", ih)
    .attr("stroke", ctx.theme.ink.baseline)
    .attr("stroke-dasharray", "3,3").attr("opacity", 0);
  var dots = g.append("g");
  var xVals = uniq(data.map(function (d) { return +xScale(d.x); }))
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
        return swatchRow(color(d.series), d.series, ctx.fmt(d.y));
      });
      showTip(ctx, event, "<b>" + esc(xLabel) + "</b><br>" + rows.join("<br>"));
    })
    .on("pointerleave", function () {
      cross.attr("opacity", 0);
      dots.selectAll("circle").remove();
      hideTip(ctx);
    });
}

/* ---------- scatter ---------- */

function renderScatter(ctx) {
  var data = ctx.x.data;
  var hasSeries = data.length && data[0].series !== undefined;
  var hasSize = data.length && data[0].size !== undefined;
  var seriesNames = hasSeries ?
    uniq(data.map(function (d) { return d.series; })) : [];
  var color = d3.scaleOrdinal().domain(seriesNames).range(ctx.theme.palette);
  if (hasSeries) {
    buildLegend(ctx.header, seriesNames, color, ctx.theme);
    ctx.height = Math.max(120, ctx.height - 26);
  }

  var m = { top: 12, right: 24, bottom: 52, left: 58 };
  var iw = ctx.width - m.left - m.right,
      ih = ctx.height - m.top - m.bottom;
  var svg = baseSvg(ctx);
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

  yGrid(g, y, iw, ctx.theme);
  g.append("g").attr("transform", "translate(0," + ih + ")")
    .call(d3.axisBottom(xScale).ticks(Math.min(8, Math.floor(iw / 80)))
      .tickFormat(fmtTick).tickSizeOuter(0))
    .call(function (s) { styleAxis(s, ctx.theme, true); });
  g.append("g").call(d3.axisLeft(y).ticks(5).tickFormat(fmtTick))
    .call(function (s) { styleAxis(s, ctx.theme, false); });
  axisLabels(svg, ctx, m, iw, ih, ctx.x.xlab, ctx.x.ylab);

  var pts = g.selectAll("circle.pt").data(data).enter().append("circle")
    .attr("class", "pt")
    .attr("cx", function (d) { return xScale(d.x); })
    .attr("cy", function (d) { return y(d.y); })
    .attr("r", 0)
    .attr("fill", function (d) {
      return hasSeries ? color(d.series) : ctx.theme.palette[0];
    })
    .attr("fill-opacity", 0.8)
    .attr("stroke", ctx.theme.ink.surface).attr("stroke-width", 1);

  pts.transition().duration(ctx.duration)
    .delay(function (d, i) { return Math.min(i * 6, 400); })
    .attr("r", function (d) { return hasSize ? r(d.size) : r(); });

  pts
    .on("pointerenter pointermove", function (event, d) {
      d3.select(this)
        .attr("stroke-width", 2)
        .attr("r", (hasSize ? r(d.size) : r()) * 1.35);
      var rows = [];
      if (d.label !== undefined) rows.push("<b>" + esc(d.label) + "</b>");
      if (hasSeries) rows.push(swatchRow(color(d.series), "group", esc(d.series)));
      rows.push(esc(ctx.x.xlab || "x") + ": <b>" + ctx.fmt(d.x) + "</b>");
      rows.push(esc(ctx.x.ylab || "y") + ": <b>" + ctx.fmt(d.y) + "</b>");
      if (hasSize) {
        rows.push(esc(ctx.x.sizelab || "size") + ": <b>" +
          ctx.fmt(d.size) + "</b>");
      }
      showTip(ctx, event, rows.join("<br>"));
    })
    .on("pointerleave", function (event, d) {
      d3.select(this)
        .attr("stroke-width", 1)
        .attr("r", hasSize ? r(d.size) : r());
      hideTip(ctx);
    });
}

/* ---------- force-directed network ---------- */

function renderForce(ctx) {
  var nodes = ctx.x.nodes.map(function (d) { return Object.assign({}, d); });
  var links = ctx.x.links.map(function (d) { return Object.assign({}, d); });
  var hasGroup = nodes.length && nodes[0].group !== undefined;
  var groups = hasGroup ?
    uniq(nodes.map(function (d) { return d.group; })) : [];
  var color = d3.scaleOrdinal().domain(groups).range(ctx.theme.palette);
  if (hasGroup) {
    buildLegend(ctx.header, groups, color, ctx.theme);
    ctx.height = Math.max(120, ctx.height - 26);
  }

  /* Count each node's connections (its size on screen) and remember who
     is linked to whom (for the hover highlight). This runs before d3
     replaces the source/target strings with node objects. */
  var degree = {};
  links.forEach(function (l) {
    degree[l.source] = (degree[l.source] || 0) + 1;
    degree[l.target] = (degree[l.target] || 0) + 1;
  });
  var adjacent = {};
  links.forEach(function (l) {
    (adjacent[l.source] = adjacent[l.source] || {})[l.target] = true;
    (adjacent[l.target] = adjacent[l.target] || {})[l.source] = true;
  });

  var svg = baseSvg(ctx);
  var g = svg.append("g");
  var w = ctx.width, h = ctx.height;
  var lw = d3.scaleSqrt()
    .domain([0, d3.max(links, function (l) { return l.value; }) || 1])
    .range([0.6, 4]);
  var nr = function (d) { return 6 + 1.8 * Math.sqrt(degree[d.id] || 0); };

  /* The physics: links pull connected nodes together, "charge" pushes all
     nodes apart, "center" keeps the whole thing in the middle, and
     "collide" stops nodes overlapping. d3 runs this simulation and calls
     our "tick" handler below on every step. */
  var sim = d3.forceSimulation(nodes)
    .force("link", d3.forceLink(links)
      .id(function (d) { return d.id; }).distance(70))
    .force("charge", d3.forceManyBody().strength(-220))
    .force("center", d3.forceCenter(w / 2, h / 2))
    .force("collide", d3.forceCollide().radius(function (d) {
      return nr(d) + 6; }));

  var link = g.selectAll("line").data(links).enter().append("line")
    .attr("stroke", ctx.theme.ink.baseline)
    .attr("stroke-opacity", 0.8)
    .attr("stroke-width", function (l) { return lw(l.value); });

  var node = g.selectAll("g.node").data(nodes).enter().append("g")
    .attr("class", "node").style("cursor", "grab");

  node.append("circle")
    .attr("r", nr)
    .attr("fill", function (d) {
      return hasGroup ? color(d.group) : ctx.theme.palette[0];
    })
    .attr("stroke", ctx.theme.ink.surface).attr("stroke-width", 1.5);

  node.append("text")
    .attr("dx", function (d) { return nr(d) + 4; })
    .attr("dominant-baseline", "middle")
    .attr("fill", ctx.theme.ink.secondary)
    .style("font-size", "11px")
    .style("pointer-events", "none")
    .text(function (d) { return d.label; });

  /* Dragging: while a node is held, pin it to the pointer (fx/fy) and
     warm the simulation back up so the rest of the graph reacts. On
     release, unpin so it can settle naturally again. */
  node.call(d3.drag()
    .on("start", function (event, d) {
      if (!event.active) sim.alphaTarget(0.3).restart();
      d.fx = d.x; d.fy = d.y;
    })
    .on("drag", function (event, d) { d.fx = event.x; d.fy = event.y; })
    .on("end", function (event, d) {
      if (!event.active) sim.alphaTarget(0);
      d.fx = null; d.fy = null;
    }));

  node
    .on("pointerenter pointermove", function (event, d) {
      node.attr("opacity", function (n) {
        return n.id === d.id || (adjacent[d.id] && adjacent[d.id][n.id]) ?
          1 : 0.18;
      });
      link.attr("stroke-opacity", function (l) {
        return l.source.id === d.id || l.target.id === d.id ? 1 : 0.08;
      });
      var rows = ["<b>" + esc(d.label) + "</b>"];
      if (hasGroup) rows.push(swatchRow(color(d.group), "group", esc(d.group)));
      rows.push("connections: <b>" + (degree[d.id] || 0) + "</b>");
      showTip(ctx, event, rows.join("<br>"));
    })
    .on("pointerleave", function () {
      node.attr("opacity", 1);
      link.attr("stroke-opacity", 0.8);
      hideTip(ctx);
    });

  sim.on("tick", function () {
    link
      .attr("x1", function (l) { return l.source.x; })
      .attr("y1", function (l) { return l.source.y; })
      .attr("x2", function (l) { return l.target.x; })
      .attr("y2", function (l) { return l.target.y; });
    node.attr("transform", function (d) {
      /* Keep nodes inside the frame, with extra room on the right so the
         text labels beside each node don't get cut off. */
      d.x = Math.max(14, Math.min(w - 85, d.x));
      d.y = Math.max(14, Math.min(h - 14, d.y));
      return "translate(" + d.x + "," + d.y + ")";
    });
  });
}

/* ---------- chord ---------- */

function renderChord(ctx) {
  var matrix = ctx.x.matrix;
  var labels = ctx.x.labels;
  var color = d3.scaleOrdinal().domain(labels).range(ctx.theme.palette);
  buildLegend(ctx.header, labels, color, ctx.theme);
  ctx.height = Math.max(120, ctx.height - 26);

  var w = ctx.width, h = ctx.height;
  var outer = Math.min(w, h) / 2 - 34;
  var inner = outer - 14;
  if (inner < 40) inner = 40, outer = 54;

  var svg = baseSvg(ctx);
  var g = svg.append("g")
    .attr("transform", "translate(" + w / 2 + "," + h / 2 + ")");

  /* d3.chord turns the flow matrix into geometry: an arc segment around
     the circle for each entity, and a ribbon between each pair that has
     flow. We just draw what it hands back. */
  var chords = d3.chord().padAngle(0.045)
    .sortSubgroups(d3.descending)(matrix);
  var arc = d3.arc().innerRadius(inner).outerRadius(outer);
  var ribbon = d3.ribbon().radius(inner - 2);

  var groupG = g.selectAll("g.group").data(chords.groups).enter()
    .append("g").attr("class", "group");

  groupG.append("path")
    .attr("d", arc)
    .attr("fill", function (d) { return color(labels[d.index]); })
    .attr("stroke", ctx.theme.ink.surface).attr("stroke-width", 1.5);

  /* Labels sit just outside each arc, rotated to face outward. Labels on
     the left half get flipped 180 degrees so they read the right way up. */
  groupG.append("text")
    .each(function (d) { d.angle = (d.startAngle + d.endAngle) / 2; })
    .attr("transform", function (d) {
      return "rotate(" + (d.angle * 180 / Math.PI - 90) + ")" +
        "translate(" + (outer + 8) + ")" +
        (d.angle > Math.PI ? "rotate(180)" : "");
    })
    .attr("text-anchor", function (d) {
      return d.angle > Math.PI ? "end" : "start";
    })
    .attr("dominant-baseline", "middle")
    .attr("fill", ctx.theme.ink.secondary)
    .style("font-size", "11px")
    .text(function (d) { return labels[d.index]; });

  var ribbons = g.selectAll("path.ribbon").data(chords).enter().append("path")
    .attr("class", "ribbon")
    .attr("d", ribbon)
    .attr("fill", function (d) { return color(labels[d.source.index]); })
    .attr("fill-opacity", 0)
    .attr("stroke", ctx.theme.ink.surface).attr("stroke-width", 0.75);

  ribbons.transition().duration(ctx.duration)
    .delay(function (d, i) { return i * 30; })
    .attr("fill-opacity", 0.72);

  function focus(idx) {
    ribbons.attr("fill-opacity", function (d) {
      return d.source.index === idx || d.target.index === idx ? 0.85 : 0.07;
    });
  }
  function unfocus() { ribbons.attr("fill-opacity", 0.72); }

  groupG
    .on("pointerenter pointermove", function (event, d) {
      focus(d.index);
      var total = d3.sum(matrix[d.index]);
      showTip(ctx, event, "<b>" + esc(labels[d.index]) + "</b><br>" +
        "outbound total: <b>" + ctx.fmt(total) + "</b>");
    })
    .on("pointerleave", function () { unfocus(); hideTip(ctx); });

  ribbons
    .on("pointerenter pointermove", function (event, d) {
      d3.select(this).attr("fill-opacity", 0.95);
      var a = labels[d.source.index], b = labels[d.target.index];
      var rows = [esc(a) + " &rarr; " + esc(b) + ": <b>" +
        ctx.fmt(matrix[d.source.index][d.target.index]) + "</b>"];
      if (d.source.index !== d.target.index) {
        rows.push(esc(b) + " &rarr; " + esc(a) + ": <b>" +
          ctx.fmt(matrix[d.target.index][d.source.index]) + "</b>");
      }
      showTip(ctx, event, rows.join("<br>"));
    })
    .on("pointerleave", function () { unfocus(); hideTip(ctx); });
}

/* ---------- zoomable sunburst ---------- */

function renderSunburst(ctx) {
  var w = ctx.width, h = ctx.height;
  var radius = Math.min(w, h) / 2 - 10;

  /* The R side sends a nested {name, children/value} tree. d3.hierarchy
     wraps it, sum() totals every branch from its leaves, and partition()
     assigns each node an angular slice (x0-x1) and a ring (y0-y1). */
  var root = d3.hierarchy(ctx.x.root)
    .sum(function (d) { return d.value || 0; })
    .sort(function (a, b) { return b.value - a.value; });
  d3.partition().size([2 * Math.PI, root.height + 1])(root);
  root.each(function (d) { d.current = d; });

  var top = root.children || [];
  var color = d3.scaleOrdinal()
    .domain(top.map(function (d) { return d.data.name; }))
    .range(ctx.theme.palette);
  buildLegend(ctx.header,
    top.map(function (d) { return d.data.name; }), color, ctx.theme);
  ctx.height = Math.max(120, ctx.height - 26);
  h = ctx.height;
  radius = Math.min(w, h) / 2 - 10;
  var ringR = radius / (root.height + 1);

  /* Every segment inherits the colour of its top-level ancestor, faded a
     step further toward the background for each ring outward - so a whole
     branch reads as one family. */
  function fillOf(d) {
    var anc = d;
    while (anc.depth > 1) anc = anc.parent;
    var base = color(anc.data.name);
    return d3.interpolate(base, ctx.theme.ink.surface)(0.22 * (d.depth - 1));
  }

  var svg = baseSvg(ctx);
  var g = svg.append("g")
    .attr("transform", "translate(" + w / 2 + "," + h / 2 + ")");

  var arc = d3.arc()
    .startAngle(function (d) { return d.x0; })
    .endAngle(function (d) { return d.x1; })
    .padAngle(function (d) { return Math.min((d.x1 - d.x0) / 2, 0.004); })
    .padRadius(radius * 1.5)
    .innerRadius(function (d) { return d.y0 * ringR; })
    .outerRadius(function (d) {
      return Math.max(d.y0 * ringR, d.y1 * ringR - 1.5);
    });

  function visible(d) {
    return d.y1 <= root.height + 1 && d.y0 >= 1 && d.x1 > d.x0;
  }
  function labelFits(d) {
    return visible(d) && (d.y1 - d.y0) * (d.x1 - d.x0) > 0.05;
  }
  function labelTransform(d) {
    var a = (d.x0 + d.x1) / 2 * 180 / Math.PI;
    var r = (d.y0 + d.y1) / 2 * ringR;
    return "rotate(" + (a - 90) + ") translate(" + r + ",0) rotate(" +
      (a < 180 ? 0 : 180) + ")";
  }

  var descendants = root.descendants().slice(1);
  var path = g.append("g").selectAll("path").data(descendants).enter()
    .append("path")
    .attr("fill", fillOf)
    .attr("fill-opacity", function (d) {
      return visible(d.current) ? (d.children ? 0.9 : 0.65) : 0;
    })
    .attr("pointer-events", function (d) {
      return visible(d.current) ? "auto" : "none";
    })
    .attr("d", function (d) { return arc(d.current); })
    .style("cursor", function (d) { return d.children ? "pointer" : "default"; });

  var label = g.append("g")
    .attr("pointer-events", "none")
    .attr("text-anchor", "middle")
    .selectAll("text").data(descendants).enter().append("text")
    .attr("dy", "0.35em")
    .attr("fill", ctx.theme.ink.primary)
    .attr("fill-opacity", function (d) { return +labelFits(d.current); })
    .attr("transform", function (d) { return labelTransform(d.current); })
    .style("font-size", "11px")
    .text(function (d) { return d.data.name; });

  var parentCircle = g.append("circle")
    .datum(root)
    .attr("r", ringR)
    .attr("fill", "none")
    .attr("pointer-events", "all")
    .style("cursor", "pointer");
  var centerLabel = g.append("text")
    .attr("text-anchor", "middle").attr("dy", "0.35em")
    .attr("fill", ctx.theme.ink.muted)
    .style("font-size", "11px")
    .style("pointer-events", "none")
    .text("");

  /* The zoom. Clicking a branch makes it the new centre: every node gets
     a "target" position rescaled so the clicked branch spans the full
     circle, then all arcs glide from where they are to where they belong.
     Clicking the centre circle zooms back out one level. */
  function clicked(event, p) {
    var target = p.children ? p : p.parent;
    if (!target) return;
    parentCircle.datum(target.parent || root);
    centerLabel.text(target === root ? "" : esc(target.data.name));

    root.each(function (d) {
      d.target = {
        x0: Math.max(0, Math.min(1,
          (d.x0 - target.x0) / (target.x1 - target.x0))) * 2 * Math.PI,
        x1: Math.max(0, Math.min(1,
          (d.x1 - target.x0) / (target.x1 - target.x0))) * 2 * Math.PI,
        y0: Math.max(0, d.y0 - target.depth),
        y1: Math.max(0, d.y1 - target.depth)
      };
    });

    var t = g.transition().duration(ctx.duration || 750);
    path.transition(t)
      .tween("data", function (d) {
        var i = d3.interpolate(d.current, d.target);
        return function (tt) { d.current = i(tt); };
      })
      .filter(function (d) {
        return +this.getAttribute("fill-opacity") || visible(d.target);
      })
      .attr("fill-opacity", function (d) {
        return visible(d.target) ? (d.children ? 0.9 : 0.65) : 0;
      })
      .attr("pointer-events", function (d) {
        return visible(d.target) ? "auto" : "none";
      })
      .attrTween("d", function (d) {
        var self = this;
        return function () { return arc(d.current); };
      });
    label.filter(function (d) {
      return +this.getAttribute("fill-opacity") || labelFits(d.target);
    }).transition(t)
      .attr("fill-opacity", function (d) { return +labelFits(d.target); })
      .attrTween("transform", function (d) {
        return function () { return labelTransform(d.current); };
      });
  }

  path.filter(function (d) { return d.children; }).on("click", clicked);
  parentCircle.on("click", clicked);

  var rootTotal = root.value || 1;
  path
    .on("pointerenter pointermove", function (event, d) {
      var trail = d.ancestors().reverse().slice(1)
        .map(function (a) { return esc(a.data.name); }).join(" / ");
      showTip(ctx, event, "<b>" + trail + "</b><br>" +
        ctx.fmt(d.value) + " (" +
        d3.format(".1%")(d.value / rootTotal) + " of total)");
    })
    .on("pointerleave", function () { hideTip(ctx); });
}

/* ---------- utils ---------- */

function uniq(arr) {
  var seen = {}, out = [];
  arr.forEach(function (v) {
    var k = String(v);
    if (!seen[k]) { seen[k] = true; out.push(v); }
  });
  return out;
}
