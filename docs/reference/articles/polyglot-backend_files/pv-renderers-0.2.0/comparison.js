/* The comparison family: slope charts, dumbbells, waterfalls, and
   bullet charts - the forms whose whole job is putting two or more
   values side by side so the gap is the message. Each registers on
   pvRenderers like every other chart family. */
(function () {

  /* Resolves a TRUE/FALSE/"auto" option sent from R. Explicit values
     win; "auto" (or an old payload with no value at all) takes the
     decision the renderer computed from the real data and pixel
     sizes - only this side knows the actual container. */
  function opt(v, autoDecision) {
    return v === "auto" || v == null ? autoDecision : !!v;
  }

  /* Compact on-chart numbers: three significant digits, an SI suffix
     past 10k (4681280 reads "4.68M"). The tooltip keeps the exact
     value. d3.format follows the active locale, so a Swiss chart gets
     its own decimal mark for free. */
  function fmtVal(v) {
    return Math.abs(v) >= 10000 ?
      d3.format(".3~s")(v) : d3.format(".3~r")(v);
  }

  /* The same, wearing its sign - the honest label for a change. */
  function fmtSigned(v) {
    return (v > 0 ? "+" : "") + fmtVal(v);
  }

  /* The change between two values, spelled for a tooltip: absolute
     plus percent when the base can carry one (a percent of zero is
     noise, not information). */
  function changeText(ctx, from, to) {
    var out = (to - from > 0 ? "+" : "") + ctx.fmt(to - from);
    if (from !== 0 && isFinite(to / from - 1)) {
      out += " (" + d3.format("+.1%")(to / from - 1) + ")";
    }
    return out;
  }

  /* Spread a sorted stack of label positions so no two sit closer than
     one line of text, then push the whole tail back up if the last one
     ran off the plot - the line chart's end-label nudge, made safe for
     the taller stacks a slope chart can produce. Entries need a `ly`
     field and arrive sorted by it. */
  function nudgeStack(ends, ih) {
    var minGap = 13;
    var i;
    for (i = 1; i < ends.length; i++) {
      if (ends[i].ly - ends[i - 1].ly < minGap) {
        ends[i].ly = ends[i - 1].ly + minGap;
      }
    }
    if (ends.length && ends[ends.length - 1].ly > ih) {
      ends[ends.length - 1].ly = ih;
      for (i = ends.length - 2; i >= 0; i--) {
        if (ends[i + 1].ly - ends[i].ly < minGap) {
          ends[i].ly = ends[i + 1].ly - minGap;
        }
      }
    }
  }

  /* ---------- slope ---------- */

  pvRenderers.slope = function (ctx) {
    var data = ctx.x.data;
    /* R validated every highlight name; a single one may arrive as a
       bare string after JSON's auto-unboxing. */
    var hl = ctx.x.highlight == null ? [] : [].concat(ctx.x.highlight);
    var accent = ctx.theme.palette[0];
    var xlev = ctx.x.xlevels;

    /* Ink is the direction channel: rising (or flat) lines keep the
       full secondary ink, falling ones step back into the muted grey.
       Dashes would be the other option, but this package reserves
       dashed strokes for projections and reference lines - a real
       measured decline deserves a solid line. Highlighted groups wear
       the accent at full weight whatever their direction. */
    function lineColor(d) {
      if (hl.indexOf(d.group) >= 0) return accent;
      return d.y2 < d.y1 ? ctx.theme.ink.muted : ctx.theme.ink.secondary;
    }
    function lineWidth(d) {
      if (hl.indexOf(d.group) >= 0) return 2.5;
      return d.y2 < d.y1 ? 1.5 : 2;
    }

    /* The margins hold the direct labels: "name value" on the left,
       the final value on the right. The left column may never eat more
       than 38% of the chart; names beyond that room are truncated (the
       tooltip keeps the full text). */
    var valW = d3.max(data, function (d) {
      return pv.textWidth(fmtVal(d.y1), 11); }) || 24;
    var nameW = d3.max(data, function (d) {
      return pv.textWidth(d.group, 11); }) || 30;
    var leftFull = 14 + nameW + 7 + valW;
    var left = Math.min(leftFull, Math.floor(ctx.width * 0.38));
    var nameRoom = left - 14 - 7 - valW;
    var maxChars = Math.max(4, Math.floor(nameRoom / pv.textWidth("x", 11) +
      0.01));
    var right = Math.max(40, 12 + (d3.max(data, function (d) {
      return pv.textWidth(fmtVal(d.y2), 11); }) || 24));
    var hasYlab = !!ctx.x.ylab;
    var m = { top: 10, right: right, bottom: ctx.x.xlab ? 46 : 30,
              left: left + (hasYlab ? 18 : 0) };
    var iw = Math.max(40, ctx.width - m.left - m.right),
        ih = Math.max(60, ctx.height - m.top - m.bottom);
    var svg = pv.baseSvg(ctx);
    var g = svg.append("g").attr("transform",
      "translate(" + m.left + "," + m.top + ")");

    var lo = d3.min(data, function (d) { return Math.min(d.y1, d.y2); });
    var hi = d3.max(data, function (d) { return Math.max(d.y1, d.y2); });
    if (lo === hi) { lo -= 1; hi += 1; }
    /* A little headroom keeps dots and labels off the plot edges; a
       slope chart has no ticks, so nothing needs a nice() domain. */
    var pad = (hi - lo) * 0.04;
    var y = d3.scaleLinear().domain([lo - pad, hi + pad]).range([ih, 0]);

    /* The frame: two hairline verticals with their position labels
       underneath - the entire axis apparatus this form needs. */
    [0, iw].forEach(function (vx, i) {
      g.append("line")
        .attr("x1", vx).attr("x2", vx).attr("y1", 0).attr("y2", ih)
        .attr("stroke", ctx.theme.ink.baseline);
      g.append("text")
        .attr("x", vx).attr("y", ih + 18)
        .attr("text-anchor", "middle")
        .attr("fill", ctx.theme.ink.muted)
        .style("font-size", "11px")
        .style("font-variant-numeric", "tabular-nums")
        .text(xlev[i]);
    });
    pv.axisLabels(svg, ctx, m, iw, ih, ctx.x.xlab, ctx.x.ylab);

    var rows = g.selectAll("g.sl").data(data).enter().append("g")
      .attr("class", "sl");
    /* Highlighted groups sit above the grey bundle, whatever their
       position in the data. */
    rows.filter(function (d) { return hl.indexOf(d.group) >= 0; }).raise();

    var lines = rows.append("line")
      .attr("x1", 0).attr("y1", function (d) { return y(d.y1); })
      .attr("x2", 0).attr("y2", function (d) { return y(d.y1); })
      .attr("stroke", lineColor)
      .attr("stroke-width", lineWidth)
      .attr("stroke-linecap", "round");
    if (ctx.duration > 0) {
      lines.transition().duration(ctx.duration).ease(d3.easeCubicInOut)
        .attr("x2", iw).attr("y2", function (d) { return y(d.y2); });
    } else {
      lines.attr("x2", iw).attr("y2", function (d) { return y(d.y2); });
    }

    /* End dots with the usual 2px surface ring. The left ones are
       there from the start; the right ones wait for the line to
       arrive. */
    rows.append("circle")
      .attr("cx", 0).attr("cy", function (d) { return y(d.y1); })
      .attr("r", 3.5).attr("fill", lineColor)
      .attr("stroke", ctx.theme.ink.surface).attr("stroke-width", 2);
    var dotsR = rows.append("circle")
      .attr("cx", iw).attr("cy", function (d) { return y(d.y2); })
      .attr("r", 3.5).attr("fill", lineColor)
      .attr("stroke", ctx.theme.ink.surface).attr("stroke-width", 2);

    /* Direct labels: the group's name and starting value on the left,
       the final value on the right, each stack nudged apart where
       lines end close together. */
    var leftEnds = data.map(function (d) {
      return { d: d, ly: y(d.y1) };
    }).sort(function (a, b) { return a.ly - b.ly; });
    nudgeStack(leftEnds, ih);
    var rightEnds = data.map(function (d) {
      return { d: d, ly: y(d.y2) };
    }).sort(function (a, b) { return a.ly - b.ly; });
    nudgeStack(rightEnds, ih);

    var leftLabels = g.selectAll("text.lab-l").data(leftEnds).enter()
      .append("text")
      .attr("class", "lab-l")
      .attr("x", -8).attr("y", function (e) { return e.ly; })
      .attr("text-anchor", "end")
      .attr("dominant-baseline", "middle")
      .style("font-size", "11px");
    leftLabels.append("tspan")
      .attr("fill", function (e) { return lineColor(e.d); })
      .text(function (e) { return pv.truncate(e.d.group, maxChars) + " "; });
    leftLabels.append("tspan")
      .attr("fill", ctx.theme.ink.muted)
      .style("font-variant-numeric", "tabular-nums")
      .text(function (e) { return fmtVal(e.d.y1); });

    var rightLabels = g.selectAll("text.lab-r").data(rightEnds).enter()
      .append("text")
      .attr("class", "lab-r")
      .attr("x", iw + 8).attr("y", function (e) { return e.ly; })
      .attr("text-anchor", "start")
      .attr("dominant-baseline", "middle")
      .attr("fill", ctx.theme.ink.secondary)
      .style("font-size", "11px")
      .style("font-variant-numeric", "tabular-nums")
      .text(function (e) { return fmtVal(e.d.y2); });
    if (ctx.duration > 0) {
      rightLabels.style("opacity", 0)
        .transition().delay(ctx.duration).duration(200)
        .style("opacity", 1);
      dotsR.attr("opacity", 0)
        .transition().delay(ctx.duration).duration(150)
        .attr("opacity", 1);
    }

    /* A 1.5px line is a mean hover target, so each row also carries an
       invisible fat twin that catches the pointer for it. */
    rows.append("line")
      .attr("x1", 0).attr("y1", function (d) { return y(d.y1); })
      .attr("x2", iw).attr("y2", function (d) { return y(d.y2); })
      .attr("stroke", "transparent").attr("stroke-width", 13);

    rows
      .on("pointerenter pointermove", function (event, d) {
        rows.attr("opacity", function (r) { return r === d ? 1 : 0.25; });
        d3.select(this).raise();
        if (event.type === "pointerenter") {
          ctx.emit("hover", { group: d.group, from: d.y1, to: d.y2 });
        }
        pv.showTip(ctx, event, "<b>" + pv.esc(d.group) + "</b><br>" +
          pv.esc(xlev[0]) + ": <b>" + ctx.fmt(d.y1) + "</b><br>" +
          pv.esc(xlev[1]) + ": <b>" + ctx.fmt(d.y2) + "</b><br>" +
          "change: <b>" + changeText(ctx, d.y1, d.y2) + "</b>");
      })
      .on("pointerleave", function () {
        rows.attr("opacity", 1);
        pv.hideTip(ctx);
      })
      .on("click", function (event, d) {
        ctx.emit("click", { group: d.group, from: d.y1, to: d.y2 });
      });
  };

  /* ---------- dumbbell ---------- */

  pvRenderers.dumbbell = function (ctx) {
    var data = ctx.x.data;
    var labels = ctx.x.labels;
    var accent = ctx.theme.palette[0];
    /* The first value is the quiet grey "before" dot, the second the
       accent-coloured "after" - the eye lands on where things ended
       up. The legend explains the pairing once, up top. */
    var color = d3.scaleOrdinal().domain(labels)
      .range([ctx.theme.ink.muted, accent]);
    pv.buildLegend(ctx.header, labels, color, ctx.theme);
    ctx.height = Math.max(120, ctx.height - 26);

    /* The category margin logic of every horizontal form: grow with
       the longest name, never eat the plot, truncate what remains
       (the tooltip keeps the full text). */
    var perChar = pv.textWidth("x", 11);
    var longest = d3.max(data, function (d) {
      return pv.textWidth(d.y, 11); }) || perChar * 4;
    var left = Math.max(70, Math.min(190, Math.ceil(22 + longest),
      Math.floor(ctx.width * 0.45)));
    var maxChars = Math.max(4, Math.floor((left - 12) / perChar + 0.01));
    var m = { top: 8, right: 20, bottom: ctx.x.xlab ? 48 : 34, left: left };
    var iw = ctx.width - m.left - m.right,
        ih = ctx.height - m.top - m.bottom;
    var svg = pv.baseSvg(ctx);
    var g = svg.append("g").attr("transform",
      "translate(" + m.left + "," + m.top + ")");

    var yBand = d3.scalePoint()
      .domain(data.map(function (d) { return d.y; }))
      .range([0, ih]).padding(0.5);
    /* Both dot sets share the value scale. It is not forced through
       zero: a dumbbell encodes by position, not length, and an index
       hovering around 100 would waste most of the plot on empty
       floor. */
    var x = d3.scaleLinear()
      .domain([d3.min(data, function (d) { return Math.min(d.x1, d.x2); }),
               d3.max(data, function (d) { return Math.max(d.x1, d.x2); })])
      .nice().range([0, iw]);

    var xAxis = g.append("g").attr("transform", "translate(0," + ih + ")")
      .call(d3.axisBottom(x).ticks(Math.min(6, Math.floor(iw / 80)))
        .tickFormat(pv.fmtTick).tickSizeOuter(0));
    pv.styleAxis(xAxis, ctx.theme, true);
    var yAxis = g.append("g").call(d3.axisLeft(yBand).tickSize(0)
      .tickFormat(function (d) { return pv.truncate(d, maxChars); }));
    pv.styleAxis(yAxis, ctx.theme, false);
    pv.axisLabels(svg, ctx, m, iw, ih, ctx.x.xlab, "");

    var rows = g.selectAll("g.row").data(data).enter().append("g")
      .attr("class", "row");

    /* The connector: a hairline joining the pair. It grows out of the
       first dot; the accent dot rides its tip. */
    var links = rows.append("line")
      .attr("x1", function (d) { return x(d.x1); })
      .attr("x2", function (d) { return x(d.x1); })
      .attr("y1", function (d) { return yBand(d.y); })
      .attr("y2", function (d) { return yBand(d.y); })
      .attr("stroke", ctx.theme.ink.baseline)
      .attr("stroke-width", 2);

    var dots1 = rows.append("circle")
      .attr("cx", function (d) { return x(d.x1); })
      .attr("cy", function (d) { return yBand(d.y); })
      .attr("r", ctx.duration > 0 ? 0 : 4.5)
      .attr("fill", color(labels[0]))
      .attr("stroke", ctx.theme.ink.surface).attr("stroke-width", 2);
    var dots2 = rows.append("circle")
      .attr("cx", function (d) { return x(d.x1); })
      .attr("cy", function (d) { return yBand(d.y); })
      .attr("r", ctx.duration > 0 ? 0 : 4.5)
      .attr("fill", color(labels[1]))
      .attr("stroke", ctx.theme.ink.surface).attr("stroke-width", 2);

    if (ctx.duration > 0) {
      var delayOf = function (d, i) { return Math.min(i * 18, 480); };
      dots1.transition().duration(200).delay(delayOf).attr("r", 4.5);
      links.transition().duration(ctx.duration).delay(delayOf)
        .ease(d3.easeCubicOut)
        .attr("x2", function (d) { return x(d.x2); });
      dots2.transition().duration(ctx.duration).delay(delayOf)
        .ease(d3.easeCubicOut)
        .attr("cx", function (d) { return x(d.x2); })
        .attr("r", 4.5);
    } else {
      links.attr("x2", function (d) { return x(d.x2); });
      dots2.attr("cx", function (d) { return x(d.x2); });
    }

    /* The gap, written at the connector's midpoint - but only where
       the connector honestly has the room; a label wider than its
       line would smear across the dots. */
    var gapLabels = rows.filter(function (d) {
      return pv.textWidth(fmtSigned(d.x2 - d.x1), 10.5) <=
        Math.abs(x(d.x2) - x(d.x1)) - 16;
    }).append("text")
      .attr("x", function (d) { return (x(d.x1) + x(d.x2)) / 2; })
      .attr("y", function (d) { return yBand(d.y) - 8; })
      .attr("text-anchor", "middle")
      .attr("fill", ctx.theme.ink.secondary)
      .style("font-size", "10.5px")
      .style("font-variant-numeric", "tabular-nums")
      .text(function (d) { return fmtSigned(d.x2 - d.x1); });
    if (ctx.duration > 0) {
      gapLabels.style("opacity", 0)
        .transition().delay(ctx.duration).duration(200)
        .style("opacity", 1);
    }

    /* An invisible strip per row makes the whole line hoverable. */
    rows.append("rect")
      .attr("x", 0)
      .attr("y", function (d) { return yBand(d.y) - yBand.step() / 2; })
      .attr("width", iw).attr("height", yBand.step())
      .attr("fill", "transparent");

    rows
      .on("pointerenter pointermove", function (event, d) {
        rows.attr("opacity", function (r) { return r === d ? 1 : 0.3; });
        if (event.type === "pointerenter") {
          ctx.emit("hover", { category: d.y, from: d.x1, to: d.x2 });
        }
        pv.showTip(ctx, event, "<b>" + pv.esc(d.y) + "</b><br>" +
          pv.swatchRow(color(labels[0]), labels[0], ctx.fmt(d.x1)) +
          "<br>" +
          pv.swatchRow(color(labels[1]), labels[1], ctx.fmt(d.x2)) +
          "<br>change: <b>" + changeText(ctx, d.x1, d.x2) + "</b>");
      })
      .on("pointerleave", function () {
        rows.attr("opacity", 1);
        pv.hideTip(ctx);
      })
      .on("click", function (event, d) {
        ctx.emit("click", { category: d.y, from: d.x1, to: d.x2 });
      });
  };

  /* ---------- waterfall ---------- */

  pvRenderers.waterfall = function (ctx) {
    var data = ctx.x.data;
    /* Colour says what each bar is: gains in the palette's first blue,
       losses in the theme's diverging red pole (the pole the
       choropleth hands to values past its midpoint - the diverging
       LOW pole is the same blue as the first palette slot, so it
       cannot mark the other sign), and the start/total amounts in the
       secondary text ink, which reads as "sum, not change". */
    function kindOf(d) {
      if (d.kind === "delta") return d.y < 0 ? "decrease" : "increase";
      return "total";
    }
    var kindColor = {
      increase: ctx.theme.palette[0],
      decrease: ctx.theme.diverging.high,
      total: ctx.theme.ink.secondary
    };

    var m = { top: 12, right: 14, bottom: 52, left: pv.leftMargin(ctx) };
    var iw = ctx.width - m.left - m.right,
        ih = ctx.height - m.top - m.bottom;
    var svg = pv.baseSvg(ctx);
    /* Texture fills (pv_textures): one hatch per role, so gains,
       losses, and totals survive greyscale printing apart. */
    var tex = pv.textureFill(ctx, svg, ["increase", "decrease", "total"],
      function (nm) { return kindColor[nm]; });
    function fillOf(d) {
      var nm = kindOf(d);
      return tex ? tex(nm) : kindColor[nm];
    }
    var g = svg.append("g").attr("transform",
      "translate(" + m.left + "," + m.top + ")");

    var x0 = d3.scaleBand()
      .domain(data.map(function (d) { return d.x; }))
      .range([0, iw]).paddingInner(0.32).paddingOuter(0.12);
    /* The start and total bars anchor at zero, so zero belongs in the
       domain whenever they exist; the running levels set the rest. */
    var yLo = Math.min(0, d3.min(data, function (d) {
      return Math.min(d.y0, d.y1); }));
    var yHi = Math.max(0, d3.max(data, function (d) {
      return Math.max(d.y0, d.y1); }));
    var y = d3.scaleLinear().domain([yLo, yHi]).nice().range([ih, 0]);

    pv.yGrid(g, y, iw, ctx.theme);
    g.append("g").attr("transform", "translate(0," + ih + ")")
      .call(d3.axisBottom(x0).tickSizeOuter(0))
      .call(function (s) { pv.styleAxis(s, ctx.theme, true); });
    g.append("g").call(d3.axisLeft(y).ticks(5).tickFormat(pv.fmtTick))
      .call(function (s) { pv.styleAxis(s, ctx.theme, false); });
    pv.axisLabels(svg, ctx, m, iw, ih, ctx.x.xlab, ctx.x.ylab);

    /* Annotation bands go under the bars, reference lines and labels
       over them - the same layering as the bar chart, so pv_annotate
       works here unchanged. */
    var gUnder = g.append("g").attr("pointer-events", "none");

    /* A bar floats between its two running levels; a zero-sized
       contribution keeps a 1px sliver so it stays findable. */
    function geom(d) {
      var top = y(Math.max(d.y0, d.y1));
      return { top: top,
               h: Math.max(1, y(Math.min(d.y0, d.y1)) - top) };
    }

    var bars = g.selectAll("rect.bar").data(data).enter().append("rect")
      .attr("class", "bar")
      .attr("x", function (d) { return x0(d.x); })
      .attr("width", x0.bandwidth())
      .attr("rx", 2)
      .attr("fill", fillOf)
      .attr("y", function (d) { return y(d.y0); })
      .attr("height", 0);

    /* Each bar grows out of the level the previous one left behind
       (the start and total bars out of the baseline), left to right
       with the family stagger. */
    var delayOf = function (d, i) { return Math.min(i * 40, 600); };
    if (ctx.duration > 0) {
      bars.transition().duration(ctx.duration).delay(delayOf)
        .ease(d3.easeCubicOut)
        .attr("y", function (d) { return geom(d).top; })
        .attr("height", function (d) { return geom(d).h; });
    } else {
      bars.attr("y", function (d) { return geom(d).top; })
        .attr("height", function (d) { return geom(d).h; });
    }

    /* Thin connectors carry each running level across the gap to the
       next bar, so the eye never loses the ledge a bar floats from. */
    var connectors = g.append("g").attr("pointer-events", "none")
      .selectAll("line").data(data.slice(0, -1)).enter().append("line")
      .attr("x1", function (d) { return x0(d.x) + x0.bandwidth(); })
      .attr("x2", function (d, i) { return x0(data[i + 1].x); })
      .attr("y1", function (d) { return y(d.y1); })
      .attr("y2", function (d) { return y(d.y1); })
      .attr("stroke", ctx.theme.ink.baseline)
      .attr("stroke-width", 1);
    if (ctx.duration > 0) {
      connectors.attr("opacity", 0)
        .transition().delay(function (d, i) {
          return delayOf(d, i) + ctx.duration * 0.7; })
        .duration(150).attr("opacity", 1);
    }

    /* Value labels ride just past each bar's far end - above rises,
       below falls - signed for contributions, plain for amounts.
       "auto" uses the bar chart's rule: few enough bars, each wide
       enough to carry a number. */
    var showVals = opt(ctx.x.valueLabels,
      data.length <= 12 && x0.bandwidth() >= 34);
    if (showVals) {
      var vals = g.selectAll("text.val").data(data).enter().append("text")
        .attr("class", "val")
        .attr("x", function (d) { return x0(d.x) + x0.bandwidth() / 2; })
        .attr("y", function (d) {
          var gm = geom(d);
          var up = d.kind === "delta" ? d.y >= 0 : d.y1 >= 0;
          return up ? gm.top - 5 : gm.top + gm.h + 13;
        })
        .attr("text-anchor", "middle")
        .attr("fill", ctx.theme.ink.secondary)
        .style("font-size", "11px")
        .style("font-variant-numeric", "tabular-nums")
        .text(function (d) {
          return d.kind === "delta" ? fmtSigned(d.y) : fmtVal(d.y);
        });
      if (ctx.duration > 0) {
        vals.style("opacity", 0)
          .transition().delay(ctx.duration).duration(200)
          .style("opacity", 1);
      }
    }

    function barDatum(d) {
      return { x: d.x, y: d.y, cumulative: d.y1, kind: d.kind };
    }

    bars
      .on("pointerenter pointermove", function (event, d) {
        bars.attr("opacity", function (b) { return b === d ? 1 : 0.45; });
        if (event.type === "pointerenter") {
          ctx.emit("hover", barDatum(d));
        }
        var body = d.kind === "delta" ?
          pv.swatchRow(kindColor[kindOf(d)], "contribution",
            (d.y > 0 ? "+" : "") + ctx.fmt(d.y)) +
          "<br>running total: <b>" + ctx.fmt(d.y1) + "</b>" :
          pv.swatchRow(kindColor.total,
            d.kind === "start" ? "start" : "total", ctx.fmt(d.y));
        pv.showTip(ctx, event, "<b>" + pv.esc(d.x) + "</b><br>" + body);
      })
      .on("pointerleave", function () {
        bars.attr("opacity", 1);
        pv.hideTip(ctx);
      })
      .on("click", function (event, d) {
        ctx.emit("click", barDatum(d));
      });

    var gOver = g.append("g").attr("pointer-events", "none");
    pv.drawAnnotations(ctx, gUnder, gOver, x0, y, iw, ih);
  };

  /* ---------- bullet ---------- */

  pvRenderers.bullet = function (ctx) {
    var data = ctx.x.data;
    var shared = ctx.x.shared !== false;
    var accent = ctx.theme.palette[0];
    var k = ctx.x.bandCount || 0;

    /* A row's band thresholds, ascending: the shared vector, or its
       own b1..bk columns. */
    function bandsOf(d) {
      if (!k) return [];
      if (ctx.x.bands) return [].concat(ctx.x.bands);
      var out = [];
      for (var i = 1; i <= k; i++) out.push(d["b" + i]);
      return out;
    }
    /* Few's graded greys off the ink ramp: the lowest band darkest
       (baseline grey), the highest fading toward the grid hairline.
       No hue anywhere in the background - colour is reserved for the
       value bar and the target tick. */
    var shades = k <= 1 ? [ctx.theme.ink.grid] :
      d3.quantize(d3.interpolateRgb(ctx.theme.ink.baseline,
        ctx.theme.ink.grid), k);

    /* Category margin as in every horizontal form; the right margin
       holds the value figures, dropped only when the chart is too
       narrow to spare the column. */
    var perChar = pv.textWidth("x", 11);
    var longest = d3.max(data, function (d) {
      return pv.textWidth(d.label, 11); }) || perChar * 4;
    var left = Math.max(70, Math.min(190, Math.ceil(22 + longest),
      Math.floor(ctx.width * 0.4)));
    var maxChars = Math.max(4, Math.floor((left - 12) / perChar + 0.01));
    var showVals = ctx.width - left >= 260;
    var m = { top: 8, right: showVals ? 52 : 16,
              bottom: (shared ? 24 : 4) + (ctx.x.xlab ? 16 : 0),
              left: left };
    var iw = ctx.width - m.left - m.right,
        ih = ctx.height - m.top - m.bottom;
    var svg = pv.baseSvg(ctx);
    var g = svg.append("g").attr("transform",
      "translate(" + m.left + "," + m.top + ")");

    /* Row slots. Each row centres its band in its slot; per-row mode
       reserves a strip at the slot's bottom for the row's own axis. */
    var n = data.length;
    var axisH = shared ? 0 : 16;
    var slotH = ih / n;
    var bandTh = Math.max(10, Math.min(30, (slotH - axisH) * 0.55));

    /* One scale for everyone, or one per row. Every scale starts at
       zero - the bar is a length. */
    function domainMax(d) {
      return d3.max([d.value, d.target].concat(bandsOf(d))) || 1;
    }
    var xShared = d3.scaleLinear()
      .domain([0, d3.max(data, domainMax)]).nice().range([0, iw]);
    function xOf(d) {
      if (shared) return xShared;
      return d3.scaleLinear().domain([0, domainMax(d)]).nice()
        .range([0, iw]);
    }

    if (shared) {
      var xAxis = g.append("g")
        .attr("transform", "translate(0," + ih + ")")
        .call(d3.axisBottom(xShared)
          .ticks(Math.min(6, Math.floor(iw / 80)))
          .tickFormat(pv.fmtTick).tickSizeOuter(0));
      pv.styleAxis(xAxis, ctx.theme, true);
    }
    pv.axisLabels(svg, ctx, m, iw, ih, ctx.x.xlab, "");

    var rows = g.selectAll("g.row").data(data).enter().append("g")
      .attr("class", "row")
      .attr("transform", function (d, i) {
        return "translate(0," + (i * slotH) + ")";
      });

    var midY = (slotH - axisH) / 2;

    /* Per-row axes, small and muted, when every row owns its scale -
       silent numbers on differing scales would mislead. */
    if (!shared) {
      rows.append("g")
        .attr("transform", "translate(0," + (midY + bandTh / 2 + 3) + ")")
        .each(function (d) {
          var ax = d3.select(this).call(d3.axisBottom(xOf(d)).ticks(3)
            .tickFormat(pv.fmtTick).tickSize(3).tickSizeOuter(0));
          pv.styleAxis(ax, ctx.theme, true);
          ax.selectAll("text").style("font-size", "9px");
        });
    }

    /* Background bands, widest first so each darker, lower band paints
       over the lighter one behind it. */
    rows.each(function (d) {
      var row = d3.select(this);
      var x = xOf(d);
      var bs = bandsOf(d);
      for (var i = bs.length - 1; i >= 0; i--) {
        row.append("rect")
          .attr("x", 0).attr("y", midY - bandTh / 2)
          .attr("width", Math.max(0, x(bs[i])))
          .attr("height", bandTh)
          .attr("fill", shades[i]);
      }
    });

    /* Row labels down the left, like every horizontal form. */
    rows.append("text")
      .attr("x", -10).attr("y", midY)
      .attr("text-anchor", "end")
      .attr("dominant-baseline", "middle")
      .attr("fill", ctx.theme.ink.muted)
      .style("font-size", "11px")
      .text(function (d) { return pv.truncate(d.label, maxChars); });

    /* The value bar: thin, in the accent, growing from zero. */
    var barH = Math.max(4, bandTh / 3);
    var bars = rows.append("path")
      .attr("fill", accent)
      .attr("d", function (d) {
        return pv.rightRoundedBar(0, midY - barH / 2, 0, barH, 2);
      });
    if (ctx.duration > 0) {
      bars.transition().duration(ctx.duration)
        .delay(function (d, i) { return Math.min(i * 30, 400); })
        .ease(d3.easeCubicOut)
        .attrTween("d", function (d) {
          var w = xOf(d)(d.value);
          return function (t) {
            return pv.rightRoundedBar(0, midY - barH / 2, t * w, barH, 2);
          };
        });
    } else {
      bars.attr("d", function (d) {
        return pv.rightRoundedBar(0, midY - barH / 2, xOf(d)(d.value),
          barH, 2);
      });
    }

    /* The target: a near-black tick standing taller than the bar. */
    var ticks = rows.append("line")
      .attr("x1", function (d) { return xOf(d)(d.target); })
      .attr("x2", function (d) { return xOf(d)(d.target); })
      .attr("y1", midY - bandTh * 0.42)
      .attr("y2", midY + bandTh * 0.42)
      .attr("stroke", ctx.theme.ink.primary)
      .attr("stroke-width", 2);
    if (ctx.duration > 0) {
      ticks.attr("opacity", 0)
        .transition().delay(ctx.duration * 0.6).duration(200)
        .attr("opacity", 1);
    }

    /* The exact value, right-aligned in its own quiet column - the
       one number each row is really about. */
    if (showVals) {
      var vals = rows.append("text")
        .attr("x", iw + 44).attr("y", midY)
        .attr("text-anchor", "end")
        .attr("dominant-baseline", "middle")
        .attr("fill", ctx.theme.ink.secondary)
        .style("font-size", "11px")
        .style("font-variant-numeric", "tabular-nums")
        .text(function (d) { return fmtVal(d.value); });
      if (ctx.duration > 0) {
        vals.style("opacity", 0)
          .transition().delay(ctx.duration).duration(200)
          .style("opacity", 1);
      }
    }

    /* An invisible strip per row makes the whole row hoverable. */
    rows.append("rect")
      .attr("x", 0).attr("y", 0)
      .attr("width", iw).attr("height", Math.max(1, slotH - axisH))
      .attr("fill", "transparent");

    rows
      .on("pointerenter pointermove", function (event, d) {
        rows.attr("opacity", function (r) { return r === d ? 1 : 0.35; });
        if (event.type === "pointerenter") {
          ctx.emit("hover", { label: d.label, value: d.value,
            target: d.target });
        }
        var vs = pv.swatchRow(accent, ctx.x.vlab || "value",
          ctx.fmt(d.value));
        var ts = pv.swatchRow(ctx.theme.ink.primary,
          ctx.x.tlab || "target", ctx.fmt(d.target));
        var rel = d.target !== 0 ?
          "<br>" + d3.format(".0%")(d.value / d.target) + " of target" : "";
        pv.showTip(ctx, event, "<b>" + pv.esc(d.label) + "</b><br>" +
          vs + "<br>" + ts + rel);
      })
      .on("pointerleave", function () {
        rows.attr("opacity", 1);
        pv.hideTip(ctx);
      })
      .on("click", function (event, d) {
        ctx.emit("click", { label: d.label, value: d.value,
          target: d.target });
      });
  };

})();
