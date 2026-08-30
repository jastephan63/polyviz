/*
 * Point-cloud renderers: beeswarm. The layout is a d3 force simulation
 * run to completion synchronously before anything is drawn, so the final
 * positions exist up front - nothing jiggles on screen, and instant mode
 * (duration 0) gets its finished state for free. See basic.js for the
 * ctx contract.
 */
(function () {

  /* Deterministic pseudo-random in [0, 1) from an index - the same trick
     as distribution.js - so the simulation's seed positions, and with
     them the settled swarm, are identical on every redraw and resize. */
  function hash(i) {
    var t = Math.sin((i + 1) * 12.9898) * 43758.5453;
    return t - Math.floor(t);
  }

  /* ---------- beeswarm ---------- */

  pvRenderers.beeswarm = function (ctx) {
    var data = ctx.x.data;
    var hasGroup = data.length && data[0].group !== undefined;
    var groups = hasGroup ?
      pv.uniq(data.map(function (d) { return d.group; })) : [];
    var color = d3.scaleOrdinal().domain(groups).range(ctx.theme.palette);

    /* Dot size adapts to the crowd: 5px up to 150 dots, shrinking
       linearly to 3px by 500. The R side refuses more than 800, so the
       radius never has to go below 3. */
    var n = data.length;
    var r = n <= 150 ? 5 : Math.max(3, 5 - 2 * (n - 150) / 350);

    /* Lane labels sit to the left when groups exist. Their margin grows
       with the longest name but never past 45% of the chart width;
       names that still don't fit are truncated, and the tooltip always
       carries the full text. */
    var longest = hasGroup ? (d3.max(groups, function (gg) {
      return pv.textWidth(gg, 12); }) || 30) : 0;
    var m = { top: 14, right: 30, bottom: ctx.x.xlab ? 48 : 34,
              left: hasGroup ?
                Math.max(60, Math.min(190, 0.45 * ctx.width,
                  22 + longest)) : 30 };
    var maxChars = Math.max(3, Math.floor((m.left - 14) / 6.9));
    var iw = ctx.width - m.left - m.right,
        ih = ctx.height - m.top - m.bottom;
    var svg = pv.baseSvg(ctx);
    var g = svg.append("g").attr("transform",
      "translate(" + m.left + "," + m.top + ")");

    var x = d3.scaleLinear()
      .domain(d3.extent(data, function (d) { return d.value; })).nice()
      .range([0, iw]);
    var lane = d3.scaleBand().domain(groups).range([0, ih]);
    function laneMid(d) {
      return hasGroup ? lane(d.group) + lane.bandwidth() / 2 : ih / 2;
    }

    /* A faint hairline anchors each lane, with the group's name at its
       left end. Ungrouped swarms need neither. */
    if (hasGroup) {
      groups.forEach(function (gname) {
        var cy = lane(gname) + lane.bandwidth() / 2;
        g.append("line")
          .attr("x1", 0).attr("x2", iw).attr("y1", cy).attr("y2", cy)
          .attr("stroke", ctx.theme.ink.grid);
        g.append("text")
          .attr("x", -10).attr("y", cy)
          .attr("text-anchor", "end")
          .attr("dominant-baseline", "middle")
          .attr("fill", ctx.theme.ink.secondary)
          .style("font-size", "12px")
          .text(pv.truncate(gname, maxChars));
      });
    }

    /* The value axis along the bottom: baseline and ticks only, no
       gridlines - a swarm's vertical spread carries no value to grid
       against. */
    /* At least 2 ticks even on the narrowest lane-labelled swarm - a
       value axis with a single number barely functions as an axis. */
    g.append("g").attr("transform", "translate(0," + ih + ")")
      .call(d3.axisBottom(x)
        .ticks(Math.max(2, Math.min(8, Math.floor(iw / 80))))
        .tickFormat(pv.fmtTick).tickSizeOuter(0))
      .call(function (s) { pv.styleAxis(s, ctx.theme, true); });

    /* The value-axis title, drawn by hand rather than via pv.axisLabels:
       the lane-label margin pushes the plot right, and a title centred
       under the squeezed plot can poke past the chart's edge on narrow
       screens. Centre it under the plot, but slide it back inside the
       chart when it would overflow - and only truncate if even the full
       chart width cannot hold it. */
    if (ctx.x.xlab) {
      var xlab = ctx.x.xlab;
      if (pv.textWidth(xlab, 12) > ctx.width - 16) {
        xlab = pv.truncate(xlab, Math.floor((ctx.width - 16) / 7.44));
      }
      var halfLab = pv.textWidth(xlab, 12) / 2;
      var cxLab = Math.max(8 + halfLab,
        Math.min(m.left + iw / 2, ctx.width - 8 - halfLab));
      svg.append("text")
        .attr("x", cxLab).attr("y", ctx.height - 6)
        .attr("text-anchor", "middle")
        .attr("fill", ctx.theme.ink.secondary).style("font-size", "12px")
        .text(xlab);
    }

    /* Place the dots: forceX pins each one to its value (strength 1),
       forceY pulls it gently toward its lane centre, and the collision
       force nudges overlapping dots apart into the classic swarm. The
       whole simulation runs here, in a plain loop, before any drawing. */
    var nodes = data.map(function (d, i) {
      return { d: d, x: x(d.value), y: laneMid(d) + (hash(i) - 0.5) };
    });
    var sim = d3.forceSimulation(nodes)
      .force("x", d3.forceX(function (nd) {
        return x(nd.d.value); }).strength(1))
      .force("y", d3.forceY(function (nd) {
        return laneMid(nd.d); }).strength(0.08))
      .force("collide", d3.forceCollide(r + 0.8))
      .stop();
    for (var t = 0; t < 120; t++) {
      sim.tick();
      /* Keep every dot inside the plot, and inside its own lane. */
      nodes.forEach(function (nd) {
        var lo = hasGroup ? lane(nd.d.group) : 0;
        var hi = hasGroup ? lo + lane.bandwidth() : ih;
        nd.x = Math.max(r, Math.min(iw - r, nd.x));
        nd.y = Math.max(lo + r, Math.min(hi - r, nd.y));
      });
    }
    sim.stop();

    var dots = g.selectAll("circle.dot").data(nodes).enter()
      .append("circle")
      .attr("class", "dot")
      .attr("cx", function (nd) { return nd.x; })
      .attr("cy", function (nd) { return nd.y; })
      .attr("fill", function (nd) {
        return hasGroup ? color(nd.d.group) : ctx.theme.palette[0];
      })
      .attr("stroke", ctx.theme.ink.surface).attr("stroke-width", 1);

    /* Dots pop in with a tiny stagger. Instant mode skips the transition
       machinery entirely: the simulation already ran to completion, so
       the finished chart just needs its radii set. */
    if (ctx.duration > 0) {
      dots.attr("r", 0)
        .transition().duration(ctx.duration)
        .delay(function (nd, i) { return Math.min(i * 4, 400); })
        .ease(d3.easeBackOut)
        .attr("r", r);
    } else {
      dots.attr("r", r);
    }

    /* Hover grows the dot 1.4x on the spot - no transition, feedback
       should be instant - and reads out who it is and its exact value. */
    dots
      .on("pointerenter pointermove", function (event, nd) {
        d3.select(this).attr("r", r * 1.4).attr("stroke-width", 1.5);
        var rows = [];
        if (nd.d.label !== undefined) {
          rows.push("<b>" + pv.esc(nd.d.label) + "</b>");
        }
        if (hasGroup) {
          rows.push(pv.swatchRow(color(nd.d.group), "group",
            pv.esc(nd.d.group)));
        }
        rows.push(pv.esc(ctx.x.xlab || "value") + ": <b>" +
          ctx.fmt(nd.d.value) + "</b>");
        pv.showTip(ctx, event, rows.join("<br>"));
      })
      .on("pointerleave", function () {
        d3.select(this).attr("r", r).attr("stroke-width", 1);
        pv.hideTip(ctx);
      });
  };

})();
