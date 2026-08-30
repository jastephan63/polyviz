/*
 * Flow and multivariate renderers: sankey diagram, parallel coordinates.
 * See basic.js for the ctx contract. The sankey layout itself comes from
 * the bundled d3-sankey plugin, which attaches to the global d3 object.
 */
(function () {

  /* ---------- sankey ---------- */

  pvRenderers.sankey = function (ctx) {
    /* d3-sankey writes layout positions onto the node and link objects,
       so work on copies and leave the payload clean for re-renders. */
    var nodes = ctx.x.nodes.map(function (d) { return Object.assign({}, d); });
    var links = ctx.x.links.map(function (d) { return Object.assign({}, d); });
    var alignFn = {
      left: d3.sankeyLeft, right: d3.sankeyRight,
      center: d3.sankeyCenter, justify: d3.sankeyJustify
    }[ctx.x.align] || d3.sankeyJustify;

    var m = { top: 6, right: 10, bottom: 10, left: 10 };
    var svg = pv.baseSvg(ctx);
    var graph = d3.sankey()
      .nodeWidth(14)
      .nodePadding(10)
      .nodeAlign(alignFn)
      .extent([[m.left, m.top],
               [ctx.width - m.right, ctx.height - m.bottom]])(
      { nodes: nodes, links: links });

    /* Node colours follow first-appearance order, one palette slot each.
       The slot is keyed on the TRIMMED name, so the common trick of
       duplicating an entity on both sides of the diagram with a trailing
       space ("Luzern" and "Luzern ") keeps one colour per entity. Flows
       legitimately carry more entities than the palette's 8 slots, so
       past the end we reuse the slots faded toward the surface - a repeat
       reads as a dimmer cousin rather than a twin of an unrelated node,
       which is fine here because position and labels do the identifying. */
    var slotOf = {}, nextSlot = 0;
    nodes.forEach(function (n) {
      var key = String(n.name).trim();
      if (!(key in slotOf)) slotOf[key] = nextSlot++;
    });
    function nodeColor(node) {
      var pal = ctx.theme.palette;
      var i = slotOf[String(node.name).trim()] || 0;
      var lap = Math.floor(i / pal.length);
      var base = pal[i % pal.length];
      if (!lap) return base;
      return d3.interpolate(base, ctx.theme.ink.surface)(
        Math.min(0.62, 0.28 * lap));
    }

    /* Links first so nodes sit on top of the ribbon ends. Each ribbon
       takes the colour halfway between its two nodes, so it visibly
       belongs to both. */
    var link = svg.append("g").attr("fill", "none")
      .selectAll("path").data(graph.links).enter().append("path")
      .attr("d", d3.sankeyLinkHorizontal())
      .attr("stroke", function (l) {
        return d3.interpolate(nodeColor(l.source),
                              nodeColor(l.target))(0.5);
      })
      .attr("stroke-width", function (l) { return Math.max(1, l.width); })
      .attr("stroke-opacity", 0);

    link.transition().duration(ctx.duration)
      .delay(function (l, i) { return Math.min(i * 18, 450); })
      .ease(d3.easeCubicOut)
      .attr("stroke-opacity", 0.45);

    var node = svg.append("g")
      .selectAll("rect").data(graph.nodes).enter().append("rect")
      .attr("x", function (d) { return d.x0; })
      .attr("y", function (d) { return d.y0; })
      .attr("width", function (d) { return d.x1 - d.x0; })
      .attr("height", function (d) { return Math.max(1, d.y1 - d.y0); })
      .attr("rx", 3).attr("ry", 3)
      .attr("fill", function (d) { return nodeColor(d); })
      .attr("fill-opacity", 0);

    node.transition().duration(Math.min(300, ctx.duration))
      .ease(d3.easeCubicOut)
      .attr("fill-opacity", 1);

    /* Labels sit beside their node, pointing inward: nodes on the left
       half label to the right, nodes on the right half to the left, so
       nothing pokes out of the frame. */
    var label = svg.append("g")
      .attr("pointer-events", "none")
      .selectAll("text").data(graph.nodes).enter().append("text")
      .attr("x", function (d) {
        return d.x0 < ctx.width / 2 ? d.x1 + 6 : d.x0 - 6;
      })
      .attr("y", function (d) { return (d.y0 + d.y1) / 2; })
      .attr("text-anchor", function (d) {
        return d.x0 < ctx.width / 2 ? "start" : "end";
      })
      .attr("dominant-baseline", "middle")
      .attr("fill", ctx.theme.ink.secondary)
      .style("font-size", "11px")
      .attr("opacity", 0)
      .text(function (d) { return d.name; });

    label.transition().duration(Math.min(300, ctx.duration))
      .attr("opacity", 1);

    /* Hovering a ribbon raises it and reads out the flow. */
    link
      .on("pointerenter pointermove", function (event, l) {
        d3.select(this).interrupt().attr("stroke-opacity", 0.8).raise();
        pv.showTip(ctx, event, pv.esc(l.source.name) + " &rarr; " +
          pv.esc(l.target.name) + ": <b>" + ctx.fmt(l.value) + "</b>");
      })
      .on("pointerleave", function () {
        d3.select(this).attr("stroke-opacity", 0.45);
        pv.hideTip(ctx);
      });

    /* Hovering a node isolates everything that touches it and totals the
       traffic through it in both directions. */
    node
      .on("pointerenter pointermove", function (event, d) {
        link.interrupt().attr("stroke-opacity", function (l) {
          return l.source === d || l.target === d ? 0.8 : 0.08;
        });
        var tin = d3.sum(d.targetLinks, function (l) { return l.value; });
        var tout = d3.sum(d.sourceLinks, function (l) { return l.value; });
        pv.showTip(ctx, event, "<b>" + pv.esc(d.name) + "</b><br>" +
          "in: <b>" + ctx.fmt(tin) + "</b><br>" +
          "out: <b>" + ctx.fmt(tout) + "</b>");
      })
      .on("pointerleave", function () {
        link.attr("stroke-opacity", 0.45);
        pv.hideTip(ctx);
      });
  };

  /* ---------- parallel coordinates ---------- */

  pvRenderers.parallel = function (ctx) {
    var cols = ctx.x.columns;
    var data = ctx.x.data;
    var hasSeries = data.length && data[0].series !== undefined;
    var seriesNames = hasSeries ?
      pv.uniq(data.map(function (d) { return d.series; })) : [];
    var color = d3.scaleOrdinal().domain(seriesNames)
      .range(ctx.theme.palette);
    if (hasSeries && ctx.x.showLegend) {
      pv.buildLegend(ctx.header, seriesNames, color, ctx.theme);
      ctx.height = Math.max(120, ctx.height - 26);
    }

    /* Extra headroom at the top for the axis titles. */
    var m = { top: 34, right: 26, bottom: 12, left: 52 };
    var iw = ctx.width - m.left - m.right,
        ih = ctx.height - m.top - m.bottom;
    var svg = pv.baseSvg(ctx);
    var g = svg.append("g").attr("transform",
      "translate(" + m.left + "," + m.top + ")");

    /* One vertical axis per column, evenly spaced, each with its own
       independent scale - parallel coordinates compare shapes of rows,
       not absolute positions across axes. */
    var xPos = d3.scalePoint()
      .domain(d3.range(cols.length)).range([0, iw]);
    var yScales = cols.map(function (c, i) {
      return d3.scaleLinear()
        .domain(d3.extent(data, function (d) { return d["v" + (i + 1)]; }))
        .nice().range([ih, 0]);
    });

    function pathOf(d) {
      return d3.line()(cols.map(function (c, i) {
        return [xPos(i), yScales[i](d["v" + (i + 1)])];
      }));
    }
    function strokeOf(d) {
      return hasSeries ? color(d.series) : ctx.theme.palette[0];
    }

    /* With a handful of rows each line can stand alone; with hundreds
       the picture is about density, so opacity scales down with the row
       count to keep the bundle readable instead of a solid mass. */
    var baseOp = Math.max(0.14, Math.min(0.55, 40 / data.length));

    /* Any axis can carry a brush; a row survives only if it passes every
       active brush. Selections are kept in pixel space so the check is a
       plain range test. */
    var brushRanges = cols.map(function () { return null; });
    function rowVisible(d) {
      for (var i = 0; i < cols.length; i++) {
        var r = brushRanges[i];
        if (!r) continue;
        var py = yScales[i](d["v" + (i + 1)]);
        if (py < r[0] || py > r[1]) return false;
      }
      return true;
    }
    function applyBrushes() {
      lines.interrupt().attr("stroke-opacity", function (d) {
        return rowVisible(d) ? baseOp : 0.06;
      });
    }

    /* Lines go in before the axes so tick labels and brushes stay on top. */
    var linesG = g.append("g");
    var lines = linesG.selectAll("path.row").data(data).enter()
      .append("path")
      .attr("class", "row")
      .attr("fill", "none")
      .attr("stroke", strokeOf)
      .attr("stroke-width", 1.5)
      .attr("stroke-opacity", 0)
      .attr("pointer-events", "none")
      .attr("d", pathOf);

    lines.transition().duration(ctx.duration)
      .delay(function (d, i) { return Math.min(i * 4, 400); })
      .ease(d3.easeCubicOut)
      .attr("stroke-opacity", baseOp);

    /* A 1.5px line is a mean hover target, so an invisible 9px twin of
       each line does the pointer work. */
    var hits = linesG.selectAll("path.hit").data(data).enter()
      .append("path")
      .attr("class", "hit")
      .attr("fill", "none")
      .attr("stroke", "transparent")
      .attr("stroke-width", 9)
      .style("pointer-events", "stroke")
      .attr("d", pathOf);

    var axisG = g.selectAll("g.axis").data(cols).enter().append("g")
      .attr("class", "axis")
      .attr("transform", function (c, i) {
        return "translate(" + xPos(i) + ",0)";
      });

    axisG.each(function (c, i) {
      var sel = d3.select(this).call(
        d3.axisLeft(yScales[i]).ticks(4).tickFormat(pv.fmtTick)
          .tickSizeOuter(0));
      pv.styleAxis(sel, ctx.theme, true);
    });

    /* Titles centre on their axis, except at the bookends: the first
       starts at its axis and the last ends at its axis, so a long name
       spreads inward instead of clipping at the edge of the frame. */
    axisG.append("text")
      .attr("y", -16)
      .attr("x", function (c, i) {
        return i === 0 ? -6 : i === cols.length - 1 ? 6 : 0;
      })
      .attr("text-anchor", function (c, i) {
        return i === 0 ? "start" : i === cols.length - 1 ? "end" : "middle";
      })
      .attr("fill", ctx.theme.ink.secondary)
      .style("font-size", "11px")
      .text(function (c) { return c; });

    /* Each axis gets its own brush (a shared behaviour would share one
       event handler and every axis would report as the last one). All
       ranges are re-checked on every brush event, so several brushes
       compose; double-clicking an axis clears its brush. */
    axisG.append("g").attr("class", "brush")
      .each(function (c, i) {
        var b = d3.brushY()
          .extent([[-9, 0], [9, ih]])
          .on("start brush end", function (event) {
            brushRanges[i] = event.selection;
            applyBrushes();
          });
        var bg = d3.select(this).call(b);
        bg.selectAll(".selection")
          .attr("fill", ctx.theme.ink.primary)
          .attr("fill-opacity", 0.14)
          .attr("stroke", ctx.theme.ink.baseline);
        bg.on("dblclick.pvclear", function () { b.move(bg, null); });
      });

    /* Hovering a line lifts it above the crowd and reads the row out in
       full - the label first, then every axis value in order. */
    hits
      .on("pointerenter pointermove", function (event, d) {
        lines.filter(function (l) { return l === d; })
          .interrupt()
          .attr("stroke-opacity", 1)
          .attr("stroke-width", 2.5)
          .raise();
        var rows = [];
        if (d.label !== undefined) rows.push("<b>" + pv.esc(d.label) + "</b>");
        if (hasSeries) {
          rows.push(pv.swatchRow(color(d.series), "group", pv.esc(d.series)));
        }
        cols.forEach(function (c, i) {
          rows.push(pv.esc(c) + ": <b>" + ctx.fmt(d["v" + (i + 1)]) + "</b>");
        });
        pv.showTip(ctx, event, rows.join("<br>"));
      })
      .on("pointerleave", function (event, d) {
        lines.filter(function (l) { return l === d; })
          .attr("stroke-width", 1.5)
          .attr("stroke-opacity", rowVisible(d) ? baseOp : 0.06);
        pv.hideTip(ctx);
      });
  };

})();
