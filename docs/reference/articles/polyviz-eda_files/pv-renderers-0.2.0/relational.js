/*
 * Relational and hierarchical renderers: force-directed network, arc
 * diagram, chord diagram, zoomable sunburst. See basic.js for the ctx
 * contract.
 */
(function () {

  /* ---------- force-directed network ---------- */

  pvRenderers.force = function (ctx) {
    var nodes = ctx.x.nodes.map(function (d) { return Object.assign({}, d); });
    var links = ctx.x.links.map(function (d) { return Object.assign({}, d); });
    var hasGroup = nodes.length && nodes[0].group !== undefined;
    var groups = hasGroup ?
      pv.uniq(nodes.map(function (d) { return d.group; })) : [];
    var color = d3.scaleOrdinal().domain(groups).range(ctx.theme.palette);
    if (hasGroup) {
      pv.buildLegend(ctx.header, groups, color, ctx.theme);
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

    var svg = pv.baseSvg(ctx);
    var g = svg.append("g");
    var w = ctx.width, h = ctx.height;
    var lw = d3.scaleSqrt()
      .domain([0, d3.max(links, function (l) { return l.value; }) || 1])
      .range([0.6, 4]);
    var nr = function (d) { return 6 + 1.8 * Math.sqrt(degree[d.id] || 0); };

    /* The physics: links pull connected nodes together, "charge" pushes all
       nodes apart, "center" keeps the whole thing in the middle, and
       "collide" stops nodes overlapping. d3 runs this simulation and calls
       our "tick" handler below on every step. Repulsion and spring length
       scale mildly with graph size - small graphs spread out to fill the
       frame, large graphs pull tighter so they still fit - and a weak pull
       toward the middle stops disconnected components drifting into the
       corners and out of view. */
    var sizeK = Math.max(0.6, Math.min(1.6,
      Math.pow(30 / Math.max(1, nodes.length), 0.25)));
    var sim = d3.forceSimulation(nodes)
      .force("link", d3.forceLink(links)
        .id(function (d) { return d.id; }).distance(70 * sizeK))
      .force("charge", d3.forceManyBody().strength(-220 * sizeK))
      .force("center", d3.forceCenter(w / 2, h / 2))
      .force("x", d3.forceX(w / 2).strength(0.06))
      .force("y", d3.forceY(h / 2).strength(0.06))
      .force("collide", d3.forceCollide().radius(function (d) {
        return nr(d) + 6; }));

    /* Linked selection (pv_link): while a selection exists anywhere in
       the group, nodes outside it fade, and so do links that touch no
       selected node. These are the resting opacities that every hover
       restores - with no selection they are simply 1 and 0.75. */
    function nodeOp(d) { return pv.keyOpacity(ctx, d.id, 1, 0.15); }
    function linkOp(l) {
      if (!ctx.selected) return 0.75;
      /* The simulation swaps the id strings for node objects; accept
         either form so this works whenever it is called. */
      var s = l.source.id !== undefined ? l.source.id : l.source;
      var t = l.target.id !== undefined ? l.target.id : l.target;
      return ctx.selected.indexOf(String(s)) >= 0 ||
             ctx.selected.indexOf(String(t)) >= 0 ? 0.75 : 0.1;
    }

    var link = g.selectAll("line").data(links).enter().append("line")
      .attr("stroke", ctx.theme.ink.muted)
      .attr("stroke-opacity", linkOp)
      .attr("stroke-width", function (l) { return lw(l.value); });

    var node = g.selectAll("g.node").data(nodes).enter().append("g")
      .attr("class", "node").style("cursor", "grab")
      .attr("opacity", nodeOp);

    node.append("circle")
      .attr("r", nr)
      .attr("fill", function (d) {
        return hasGroup ? color(d.group) : ctx.theme.palette[0];
      })
      .attr("stroke", ctx.theme.ink.surface).attr("stroke-width", 1.5);

    /* Labels get a halo the colour of the chart surface (painted under
       the letters via paint-order) so they stay readable when they cross
       a link or another node. */
    node.append("text")
      .attr("dx", function (d) { return nr(d) + 4; })
      .attr("dominant-baseline", "middle")
      .attr("fill", ctx.theme.ink.secondary)
      .attr("stroke", ctx.theme.ink.surface)
      .attr("stroke-width", 3)
      .style("paint-order", "stroke")
      .style("stroke-linejoin", "round")
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
        var rows = ["<b>" + pv.esc(d.label) + "</b>"];
        if (hasGroup) {
          rows.push(pv.swatchRow(color(d.group), "group", pv.esc(d.group)));
        }
        rows.push("connections: <b>" + (degree[d.id] || 0) + "</b>");
        pv.showTip(ctx, event, rows.join("<br>"));
      })
      .on("pointerleave", function () {
        node.attr("opacity", nodeOp);
        link.attr("stroke-opacity", linkOp);
        pv.hideTip(ctx);
      })
      /* In Shiny, a click reports the node as input$<id>_click. A drag
         that actually moved suppresses its trailing click, so this only
         fires for a genuine click in place. */
      .on("click", function (event, d) {
        if (event.defaultPrevented) return;
        ctx.emit("click", {
          id: d.id, label: d.label,
          group: hasGroup ? d.group : null,
          connections: degree[d.id] || 0
        });
      });

    /* Decide, for the current node positions, whether each label fits in
       its usual spot beside the node or has to drop below it. Walk the
       nodes keeping the boxes of labels already placed; when the beside
       position would overlap one of them, move the label under its node
       instead. Runs every tick, so labels dodge each other live as the
       graph settles or is dragged around. */
    function placeLabels() {
      var placed = [];
      node.each(function (d) {
        var tw = pv.textWidth(d.label, 11);
        var beside = { x0: d.x + nr(d) + 4, x1: d.x + nr(d) + 4 + tw,
                       y0: d.y - 7, y1: d.y + 7 };
        var collides = placed.some(function (b) {
          return beside.x0 < b.x1 && beside.x1 > b.x0 &&
                 beside.y0 < b.y1 && beside.y1 > b.y0;
        });
        placed.push(collides ?
          { x0: d.x - tw / 2, x1: d.x + tw / 2,
            y0: d.y + nr(d) + 6, y1: d.y + nr(d) + 20 } : beside);
        d3.select(this).select("text")
          .attr("dx", collides ? 0 : nr(d) + 4)
          .attr("dy", collides ? nr(d) + 13 : 0)
          .attr("text-anchor", collides ? "middle" : "start");
      });
    }

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
      placeLabels();
    });
  };

  /* ---------- arc diagram ---------- */

  pvRenderers.arc = function (ctx) {
    var nodes = ctx.x.nodes.map(function (d) { return Object.assign({}, d); });
    var links = ctx.x.links.map(function (d) { return Object.assign({}, d); });
    var hasGroup = nodes.length && nodes[0].group !== undefined;
    /* The R side ships the group levels in the caller's original node
       order, so colours stay put whichever node `order` was chosen. */
    var groups = hasGroup ?
      (ctx.x.groups || pv.uniq(nodes.map(function (d) { return d.group; }))) :
      [];
    var color = d3.scaleOrdinal().domain(groups).range(ctx.theme.palette);
    if (hasGroup) {
      pv.buildLegend(ctx.header, groups, color, ctx.theme);
      ctx.height = Math.max(120, ctx.height - 26);
    }

    /* Each node's connection count sizes its dot; the adjacency map
       drives the hover highlight. Links here keep their id strings -
       there is no simulation to swap them for objects. */
    var degree = {}, adjacent = {}, groupOf = {}, labelOf = {};
    links.forEach(function (l) {
      degree[l.source] = (degree[l.source] || 0) + 1;
      degree[l.target] = (degree[l.target] || 0) + 1;
      (adjacent[l.source] = adjacent[l.source] || {})[l.target] = true;
      (adjacent[l.target] = adjacent[l.target] || {})[l.source] = true;
    });
    nodes.forEach(function (d) {
      groupOf[d.id] = d.group;
      labelOf[d.id] = d.label;
    });

    var w = ctx.width, h = ctx.height;
    var nr = function (d) {
      return 3.5 + 1.6 * Math.sqrt(degree[d.id] || 0);
    };
    var maxR = d3.max(nodes, nr) || 5;

    /* The line of nodes. scalePoint spaces them evenly; the half-step
       outer padding keeps the first and last labels inside the frame. */
    var xs = d3.scalePoint()
      .domain(nodes.map(function (d) { return d.id; }))
      .range([18, w - 18]).padding(0.5);
    var step = xs.step();

    /* Labels sit horizontally under the dots. When the widest one no
       longer fits its slot the labels stagger into two rows, and if even
       two slots are too narrow they are shortened with an ellipsis - the
       tooltip always carries the full name. */
    var maxLabelW = d3.max(nodes, function (d) {
      return pv.textWidth(d.label, 11);
    }) || 0;
    var staggered = nodes.length > 1 && maxLabelW > step - 6;
    var labelChars = Math.max(4, Math.floor(
      ((staggered ? 2 * step : step) - 8) / (11 * 0.62)));
    var baseY = h - maxR - (staggered ? 32 : 19) - 4;

    /* Arc width by the square root of the flow, and a resting opacity
       that steps down as the picture fills up - a dense weave stays
       readable only when each thread is faint. */
    var lw = d3.scaleSqrt()
      .domain([0, d3.max(links, function (l) { return l.value; }) || 1])
      .range([0.7, 5]);
    var baseOp = links.length <= 12 ? 0.7 :
                 links.length <= 40 ? 0.55 :
                 links.length <= 100 ? 0.42 : 0.3;

    /* Linked selection (pv_link): the resting opacities every hover
       restores - full strength with no selection anywhere, faded for
       nodes outside it and links touching no selected node. */
    function nodeOp(d) { return pv.keyOpacity(ctx, d.id, 1, 0.15); }
    function linkOp(l) {
      if (!ctx.selected) return baseOp;
      return ctx.selected.indexOf(String(l.source)) >= 0 ||
             ctx.selected.indexOf(String(l.target)) >= 0 ? baseOp : 0.06;
    }

    /* A link is half an ellipse over the baseline: full semicircles
       until the chart is too short for the widest span, then flattened
       just enough to stay inside the frame. */
    function arcPath(l) {
      var x1 = xs(l.source), x2 = xs(l.target);
      var lo = Math.min(x1, x2), hi = Math.max(x1, x2);
      var rx = (hi - lo) / 2;
      var ry = Math.min(rx, baseY - 10);
      return "M" + lo + "," + baseY +
        " A" + rx + "," + ry + " 0 0,1 " + hi + "," + baseY;
    }

    var svg = pv.baseSvg(ctx);
    var g = svg.append("g");

    /* The line itself: one hairline under the dots. */
    if (nodes.length > 1) {
      g.append("line")
        .attr("x1", xs(nodes[0].id) - 10)
        .attr("x2", xs(nodes[nodes.length - 1].id) + 10)
        .attr("y1", baseY).attr("y2", baseY)
        .attr("stroke", ctx.theme.ink.baseline);
    }

    var link = g.selectAll("path.arc").data(links).enter().append("path")
      .attr("class", "arc")
      .attr("fill", "none")
      .attr("d", arcPath)
      .attr("stroke", function (l) {
        return hasGroup ? color(groupOf[l.source]) : ctx.theme.ink.muted;
      })
      .attr("stroke-width", function (l) { return lw(l.value); })
      .attr("stroke-linecap", "round")
      .attr("stroke-opacity", ctx.duration > 0 ? 0 : linkOp);

    /* Entrance: arcs fade in one after the other. Skipped entirely in
       instant mode - the final opacity is already set above. */
    if (ctx.duration > 0) {
      link.transition().duration(ctx.duration)
        .delay(function (d, i) { return i * 25; })
        .attr("stroke-opacity", linkOp);
    }

    /* A thin arc is a hard hover target, so an invisible wider twin of
       each one catches the pointer. Both selections are bound to the
       same link objects, which is how a hit path finds its arc. */
    var hit = g.selectAll("path.hit").data(links).enter().append("path")
      .attr("class", "hit")
      .attr("fill", "none")
      .attr("d", arcPath)
      .attr("stroke", "transparent")
      .attr("stroke-width", function (l) { return Math.max(9, lw(l.value)); })
      .style("cursor", "pointer");

    var node = g.selectAll("g.node").data(nodes).enter().append("g")
      .attr("class", "node").style("cursor", "pointer")
      .attr("transform", function (d) {
        return "translate(" + xs(d.id) + "," + baseY + ")";
      })
      .attr("opacity", nodeOp);

    node.append("circle")
      .attr("r", nr)
      .attr("fill", function (d) {
        return hasGroup ? color(d.group) : ctx.theme.palette[0];
      })
      .attr("stroke", ctx.theme.ink.surface).attr("stroke-width", 2);

    node.append("text")
      .attr("text-anchor", "middle")
      .attr("y", function (d, i) {
        return maxR + (staggered && i % 2 ? 26 : 13);
      })
      .attr("fill", ctx.theme.ink.secondary)
      .style("font-size", "11px")
      .style("pointer-events", "none")
      .text(function (d) { return pv.truncate(d.label, labelChars); });

    node
      .on("pointerenter pointermove", function (event, d) {
        node.attr("opacity", function (n) {
          return n.id === d.id || (adjacent[d.id] && adjacent[d.id][n.id]) ?
            1 : 0.25;
        });
        link.attr("stroke-opacity", function (l) {
          return l.source === d.id || l.target === d.id ?
            Math.min(1, baseOp + 0.35) : 0.06;
        });
        var rows = ["<b>" + pv.esc(d.label) + "</b>"];
        if (hasGroup) {
          rows.push(pv.swatchRow(color(d.group), "group", pv.esc(d.group)));
        }
        rows.push("connections: <b>" + (degree[d.id] || 0) + "</b>");
        pv.showTip(ctx, event, rows.join("<br>"));
      })
      .on("pointerleave", function () {
        node.attr("opacity", nodeOp);
        link.attr("stroke-opacity", linkOp);
        pv.hideTip(ctx);
      })
      /* In Shiny, clicking a node reports it as input$<id>_click. */
      .on("click", function (event, d) {
        ctx.emit("click", {
          part: "node", id: d.id, label: d.label,
          group: hasGroup ? d.group : null,
          connections: degree[d.id] || 0
        });
      });

    hit
      .on("pointerenter pointermove", function (event, l) {
        link.filter(function (x) { return x === l; })
          .attr("stroke-opacity", 0.95);
        pv.showTip(ctx, event,
          pv.esc(labelOf[l.source]) + " &rarr; " + pv.esc(labelOf[l.target]) +
          ": <b>" + ctx.fmt(l.value) + "</b>");
      })
      .on("pointerleave", function () {
        link.attr("stroke-opacity", linkOp);
        pv.hideTip(ctx);
      })
      /* In Shiny, clicking an arc reports the flow it carries. */
      .on("click", function (event, l) {
        ctx.emit("click", {
          part: "link", source: l.source, target: l.target, value: l.value
        });
      });
  };

  /* ---------- chord ---------- */

  pvRenderers.chord = function (ctx) {
    var matrix = ctx.x.matrix;
    var labels = ctx.x.labels;
    var color = d3.scaleOrdinal().domain(labels).range(ctx.theme.palette);
    pv.buildLegend(ctx.header, labels, color, ctx.theme);
    ctx.height = Math.max(120, ctx.height - 26);

    var w = ctx.width, h = ctx.height;
    var outer = Math.min(w, h) / 2 - 34;
    var inner = outer - 14;
    if (inner < 40) { inner = 40; outer = 54; }

    var svg = pv.baseSvg(ctx);
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
       the left half get flipped 180 degrees so they read the right way up.
       Each label is truncated to the room actually left between the circle
       and the chart edge along its own direction, so nothing runs off the
       frame on narrow charts - the legend and tooltip carry full names. */
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
      .text(function (d) { return labels[d.index]; })
      .each(function (d) {
        /* How far this label may run before hitting the chart edge,
           along its own outward direction. */
        var ux = Math.sin(d.angle), uy = -Math.cos(d.angle);
        var px = (outer + 8) * ux, py = (outer + 8) * uy;
        var tx = ux > 0 ? (w / 2 - px) / ux :
                 ux < 0 ? (-w / 2 - px) / ux : Infinity;
        var ty = uy > 0 ? (h / 2 - py) / uy :
                 uy < 0 ? (-h / 2 - py) / uy : Infinity;
        var room = Math.min(tx, ty);
        /* Measure the real rendered width; only truncate labels that
           genuinely overflow (a few clipped pixels beat an ellipsis),
           proportionally to the overshoot. */
        var len = this.getComputedTextLength();
        if (len > room + 5) {
          var name = labels[d.index];
          var keep = Math.max(4, Math.floor(name.length * room / len) - 1);
          d3.select(this).text(pv.truncate(name, keep));
        }
      });

    var ribbons = g.selectAll("path.ribbon").data(chords).enter()
      .append("path")
      .attr("class", "ribbon")
      .attr("d", ribbon)
      .attr("fill", function (d) { return color(labels[d.source.index]); })
      .attr("fill-opacity", ctx.duration > 0 ? 0 : 0.72)
      .attr("stroke", ctx.theme.ink.surface).attr("stroke-width", 0.75);

    /* Entrance: ribbons fade in one after the other. Skipped entirely
       in instant mode - the final opacity is already set above. */
    if (ctx.duration > 0) {
      ribbons.transition().duration(ctx.duration)
        .delay(function (d, i) { return i * 30; })
        .attr("fill-opacity", 0.72);
    }

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
        pv.showTip(ctx, event, "<b>" + pv.esc(labels[d.index]) + "</b><br>" +
          "outbound total: <b>" + ctx.fmt(total) + "</b>");
      })
      .on("pointerleave", function () { unfocus(); pv.hideTip(ctx); })
      /* In Shiny, clicking a group arc reports it as input$<id>_click. */
      .on("click", function (event, d) {
        ctx.emit("click", {
          part: "group", label: labels[d.index],
          total: d3.sum(matrix[d.index])
        });
      });

    ribbons
      .on("pointerenter pointermove", function (event, d) {
        d3.select(this).attr("fill-opacity", 0.95);
        var a = labels[d.source.index], b = labels[d.target.index];
        var rows = [pv.esc(a) + " &rarr; " + pv.esc(b) + ": <b>" +
          ctx.fmt(matrix[d.source.index][d.target.index]) + "</b>"];
        if (d.source.index !== d.target.index) {
          rows.push(pv.esc(b) + " &rarr; " + pv.esc(a) + ": <b>" +
            ctx.fmt(matrix[d.target.index][d.source.index]) + "</b>");
        }
        pv.showTip(ctx, event, rows.join("<br>"));
      })
      .on("pointerleave", function () { unfocus(); pv.hideTip(ctx); })
      /* In Shiny, clicking a ribbon reports the flow it carries. */
      .on("click", function (event, d) {
        ctx.emit("click", {
          part: "ribbon",
          source: labels[d.source.index],
          target: labels[d.target.index],
          value: matrix[d.source.index][d.target.index]
        });
      });
  };

  /* ---------- zoomable sunburst ---------- */

  pvRenderers.sunburst = function (ctx) {
    var w = ctx.width, h = ctx.height;

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
    pv.buildLegend(ctx.header,
      top.map(function (d) { return d.data.name; }), color, ctx.theme);
    ctx.height = Math.max(120, ctx.height - 26);
    h = ctx.height;
    var radius = Math.min(w, h) / 2 - 10;
    var ringR = radius / (root.height + 1);

    /* Every segment inherits the colour of its top-level ancestor, faded a
       step further toward the background for each ring outward - so a whole
       branch reads as one family. Light surfaces wash colours out faster
       than dark ones, so fade a smaller step per ring in light mode. */
    var fadeStep = ctx.theme.mode === "light" ? 0.15 : 0.22;
    function fillOf(d) {
      var anc = d;
      while (anc.depth > 1) anc = anc.parent;
      var base = color(anc.data.name);
      return d3.interpolate(base, ctx.theme.ink.surface)(
        fadeStep * (d.depth - 1));
    }

    var svg = pv.baseSvg(ctx);
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
    /* A label appears only when it truly fits its segment: the arc at
       the label's radius must be at least as long as the rendered text,
       and the ring thick enough for a line of 11px type. `pos` is the
       segment's geometry (its current or zoom-target coordinates) and
       `d` the hierarchy node that knows the name. Dropped labels lose
       nothing - the tooltip always carries the full trail. */
    function labelFits(d, pos) {
      if (!visible(pos)) return false;
      var arcLen = (pos.x1 - pos.x0) * ((pos.y0 + pos.y1) / 2) * ringR;
      var ring = (pos.y1 - pos.y0) * ringR;
      return ring >= 13 && arcLen >= pv.textWidth(d.data.name, 11) + 6;
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
      .style("cursor", function (d) {
        return d.children ? "pointer" : "default"; });

    var label = g.append("g")
      .attr("pointer-events", "none")
      .attr("text-anchor", "middle")
      .selectAll("text").data(descendants).enter().append("text")
      .attr("dy", "0.35em")
      .attr("fill", ctx.theme.ink.primary)
      .attr("fill-opacity", function (d) { return +labelFits(d, d.current); })
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
      centerLabel.text(target === root ? "" : pv.esc(target.data.name));

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
          return function () { return arc(d.current); };
        });
      label.filter(function (d) {
        return +this.getAttribute("fill-opacity") || labelFits(d, d.target);
      }).transition(t)
        .attr("fill-opacity", function (d) { return +labelFits(d, d.target); })
        .attrTween("transform", function (d) {
          return function () { return labelTransform(d.current); };
        });
    }

    /* In Shiny, clicking any segment reports its path from the top ring
       down - e.g. ["West", "Widget"] - as input$<id>_click. Branch
       segments also zoom; the centre circle only zooms back out. */
    function emitPath(d) {
      ctx.emit("click", d.ancestors().reverse().slice(1)
        .map(function (a) { return a.data.name; }));
    }
    path.filter(function (d) { return d.children; })
      .on("click", function (event, d) {
        emitPath(d);
        clicked(event, d);
      });
    path.filter(function (d) { return !d.children; })
      .on("click", function (event, d) { emitPath(d); });
    parentCircle.on("click", clicked);

    var rootTotal = root.value || 1;
    path
      .on("pointerenter pointermove", function (event, d) {
        var trail = d.ancestors().reverse().slice(1)
          .map(function (a) { return pv.esc(a.data.name); }).join(" / ");
        pv.showTip(ctx, event, "<b>" + trail + "</b><br>" +
          ctx.fmt(d.value) + " (" +
          d3.format(".1%")(d.value / rootTotal) + " of total)");
      })
      .on("pointerleave", function () { pv.hideTip(ctx); });
  };

})();
