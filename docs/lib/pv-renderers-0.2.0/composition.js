/*
 * Composition renderers: donut, treemap, lollipop.
 * Part-of-whole and ranking charts. See basic.js for the ctx contract.
 */
(function () {

  /* Resolve a tri-state option sent from R: TRUE/FALSE force the look,
     "auto" (or a missing field from an older payload) takes the
     decision computed here from the real data and pixel sizes. */
  function opt(v, autoDecision) {
    return v === "auto" || v == null ? autoDecision : !!v;
  }

  /* ---------- donut ---------- */

  pvRenderers.donut = function (ctx) {
    var data = ctx.x.data;
    var total = d3.sum(data, function (d) { return d.value; }) || 1;
    var color = d3.scaleOrdinal()
      .domain(data.map(function (d) { return d.category; }))
      .range(ctx.theme.palette);

    /* Slices big enough to carry their own label do; the rest go to a
       legend instead so no label ever fights for space. The `labels`
       flag steers the split: "auto" labels slices of 5%+ but falls back
       to legend-only under ~480px, where outside labels collide; TRUE
       labels everything down to 2%; FALSE sends it all to the legend. */
    var showLabels = opt(ctx.x.labels, ctx.width >= 480);
    var minShare = ctx.x.labels === true ? 0.02 : 0.05;
    function carriesLabel(d) {
      return showLabels && d.value / total >= minShare;
    }
    var small = data.filter(function (d) { return !carriesLabel(d); });
    var labelled = data.filter(carriesLabel);
    if (small.length) {
      pv.buildLegend(ctx.header,
        small.map(function (d) { return d.category; }), color, ctx.theme);
      ctx.height = Math.max(120, ctx.height - 26);
    }

    var w = ctx.width, h = ctx.height;
    /* Leave room outside the ring for the direct labels - but never
       more than 45% of the width in total (22.5% per side), so labels
       can't squeeze the ring itself. Names longer than the room allows
       are truncated; the tooltip keeps the full text. */
    var longestPx = d3.max(labelled, function (d) {
      return pv.textWidth(d.category, 11); }) || 0;
    var pad = labelled.length ?
      Math.min(150, Math.ceil(26 + longestPx), Math.floor(w * 0.225)) : 16;
    /* The nudge keeps float rounding from eating the last character of
       a name that exactly fits. */
    var maxChars = Math.max(3,
      Math.floor((pad - 26) / pv.textWidth("x", 11) + 0.01));
    var radius = Math.max(50, Math.min(w / 2 - pad, h / 2 - 28));
    var innerR = radius * ctx.x.innerRadius;

    var svg = pv.baseSvg(ctx);
    var g = svg.append("g")
      .attr("transform", "translate(" + w / 2 + "," + h / 2 + ")");

    var pie = d3.pie()
      .value(function (d) { return d.value; })
      .sortValues(d3.descending)
      .padAngle(0.008);
    var arcs = pie(data);
    var arc = d3.arc().innerRadius(innerR).outerRadius(radius)
      .cornerRadius(3);
    var labelArc = d3.arc().innerRadius(radius + 10)
      .outerRadius(radius + 10);

    var paths = g.selectAll("path.slice").data(arcs).enter().append("path")
      .attr("class", "slice")
      .attr("fill", function (d) { return color(d.data.category); })
      .attr("stroke", ctx.theme.ink.surface).attr("stroke-width", 1.5);

    /* Each slice sweeps open from its own start angle, one after the
       other, so the ring appears to draw itself clockwise. d.index is
       the pie's angular order, which can differ from data order because
       the pie sorts slices largest-first. */
    paths.transition().duration(ctx.duration)
      .delay(function (d) { return Math.min(d.index * 60, 420); })
      .ease(d3.easeCubicOut)
      .attrTween("d", function (d) {
        var sweep = d3.interpolate(d.startAngle, d.endAngle);
        return function (t) {
          return arc({ startAngle: d.startAngle, endAngle: sweep(t),
                       padAngle: d.padAngle });
        };
      });

    /* Direct labels for the big slices: name and rounded share just
       outside the arc, anchored away from the circle. Neighbouring
       slices can land their labels too close together, so on each side
       we sort the labels by height and push any overlapping pair apart
       - the same trick the line chart uses for its end labels. */
    var labData = arcs.filter(function (d) {
      return carriesLabel(d.data);
    }).map(function (d) {
      var p = labelArc.centroid(d);
      return { d: d, x: p[0], y: p[1],
               right: (d.startAngle + d.endAngle) / 2 < Math.PI };
    });
    [true, false].forEach(function (side) {
      var col = labData.filter(function (l) { return l.right === side; })
        .sort(function (a, b) { return a.y - b.y; });
      for (var i = 1; i < col.length; i++) {
        if (col[i].y - col[i - 1].y < 28) {
          col[i].y = col[i - 1].y + 28;
        }
      }
    });

    var lab = g.selectAll("text.lab").data(labData).enter().append("text")
      .attr("class", "lab")
      .attr("transform", function (l) {
        return "translate(" + l.x + "," + l.y + ")";
      })
      .attr("text-anchor", function (l) { return l.right ? "start" : "end"; })
      .style("font-size", "11px")
      .style("opacity", 0);
    lab.append("tspan")
      .attr("fill", ctx.theme.ink.secondary)
      .text(function (l) { return pv.truncate(l.d.data.category, maxChars); });
    lab.append("tspan")
      .attr("x", 0).attr("dy", 13)
      .attr("fill", ctx.theme.ink.muted)
      .style("font-variant-numeric", "tabular-nums")
      .text(function (l) {
        return d3.format(".0%")(l.d.data.value / total);
      });
    lab.transition().delay(ctx.duration * 0.8).duration(250)
      .style("opacity", 1);

    /* A donut's hole earns its keep: show the grand total in the middle
       (skipped for pies and holes too small to hold the number). */
    if (innerR >= 34) {
      var center = g.append("text")
        .attr("text-anchor", "middle")
        .style("opacity", 0);
      center.append("tspan")
        .attr("x", 0).attr("dy", "-0.1em")
        .attr("fill", ctx.theme.ink.primary)
        .style("font-size", "17px").style("font-weight", "700")
        .style("font-variant-numeric", "tabular-nums")
        .text(ctx.fmt(total));
      center.append("tspan")
        .attr("x", 0).attr("dy", 15)
        .attr("fill", ctx.theme.ink.muted)
        .style("font-size", "10.5px")
        .text("total");
      center.transition().delay(ctx.duration * 0.6).duration(250)
        .style("opacity", 1);
    }

    /* Hovering a slice pops it 6px outward along its own centroid, dims
       the rest, and reads out the exact value and share. */
    paths
      .on("pointerenter pointermove", function (event, d) {
        paths
          .attr("opacity", function (p) { return p === d ? 1 : 0.35; })
          .attr("transform", function (p) {
            if (p !== d) return null;
            var c = arc.centroid(p);
            var len = Math.sqrt(c[0] * c[0] + c[1] * c[1]) || 1;
            return "translate(" + (6 * c[0] / len) + "," +
              (6 * c[1] / len) + ")";
          });
        pv.showTip(ctx, event, "<b>" + pv.esc(d.data.category) + "</b><br>" +
          pv.swatchRow(color(d.data.category), ctx.x.vlab || "value",
                       ctx.fmt(d.data.value)) +
          " &middot; " + d3.format(".1%")(d.data.value / total) +
          " of total");
      })
      .on("pointerleave", function () {
        paths.attr("opacity", 1).attr("transform", null);
        pv.hideTip(ctx);
      });
  };

  /* ---------- treemap ---------- */

  pvRenderers.treemap = function (ctx) {
    /* The R side sends the same nested {name, children/value} tree the
       sunburst uses; sum() totals every branch from its leaves. */
    var root = d3.hierarchy(ctx.x.root)
      .sum(function (d) { return d.value || 0; })
      .sort(function (a, b) { return b.value - a.value; });

    var top = root.children || [];
    var color = d3.scaleOrdinal()
      .domain(top.map(function (d) { return d.data.name; }))
      .range(ctx.theme.palette);
    /* With more than one level the cells are the leaves, so the top-level
       grouping only shows through hue - give it a legend. */
    if (root.height > 1) {
      pv.buildLegend(ctx.header,
        top.map(function (d) { return d.data.name; }), color, ctx.theme);
      ctx.height = Math.max(120, ctx.height - 26);
    }

    var m = { top: 4, right: 16, bottom: 10, left: 16 };
    var iw = ctx.width - m.left - m.right,
        ih = ctx.height - m.top - m.bottom;
    d3.treemap().tile(d3.treemapSquarify).size([iw, ih])
      .paddingInner(2)(root);

    /* Same colouring idea as the sunburst: every cell wears its top-level
       branch's colour, faded a step toward the background per depth, so a
       whole branch reads as one family. */
    function topOf(d) {
      var anc = d;
      while (anc.depth > 1) anc = anc.parent;
      return anc;
    }
    function fillOf(d) {
      var base = color(topOf(d).data.name);
      return d3.interpolate(base, ctx.theme.ink.surface)(0.22 * (d.depth - 1));
    }

    var svg = pv.baseSvg(ctx);
    var g = svg.append("g").attr("transform",
      "translate(" + m.left + "," + m.top + ")");

    var leaves = root.leaves();
    var total = root.value || 1;

    /* One group per cell, positioned at its centre, so the entrance can
       fade and scale each cell up around its own middle. */
    var cell = g.selectAll("g.cell").data(leaves).enter().append("g")
      .attr("class", "cell")
      .attr("transform", function (d) {
        return "translate(" + (d.x0 + d.x1) / 2 + "," +
          (d.y0 + d.y1) / 2 + ") scale(0.8)";
      })
      .attr("opacity", 0);

    cell.append("rect")
      .attr("x", function (d) { return -(d.x1 - d.x0) / 2; })
      .attr("y", function (d) { return -(d.y1 - d.y0) / 2; })
      .attr("width", function (d) { return d.x1 - d.x0; })
      .attr("height", function (d) { return d.y1 - d.y0; })
      .attr("rx", 2)
      .attr("fill", fillOf);

    /* Labels only where they honestly fit, 6px in from the corner: the
       name, and the value under it when there's room for a second line.
       Everything else stays quiet and lives in the tooltip. The
       `labels` flag steers how eager this is: "auto" keeps the honest
       fit, TRUE squeezes labels into cells with 20% less room (they
       must still fit at all - slivers stay blank), FALSE writes no
       cell text whatsoever. */
    var showLabels = opt(ctx.x.labels, true);
    var relax = ctx.x.labels === true ? 0.8 : 1;
    function fitsName(d) {
      return showLabels &&
        (d.x1 - d.x0) - 12 >= d.data.name.length * 6.3 * relax &&
        (d.y1 - d.y0) - 12 >= 11 * relax;
    }
    function fitsValue(d) {
      return fitsName(d) && (d.y1 - d.y0) - 12 >= 26 * relax &&
        (d.x1 - d.x0) - 12 >= ctx.fmt(d.value).length * 6 * relax;
    }
    /* Label ink is chosen per cell: light text on dark fills, dark text
       on light fills, judged by the fill's actual luminance - a fixed
       ink colour can't stay readable across eight hues. */
    function cellInk(d) {
      var c = d3.color(fillOf(d)).rgb();
      var lin = [c.r, c.g, c.b].map(function (v) {
        v /= 255;
        return v <= 0.04045 ? v / 12.92 : Math.pow((v + 0.055) / 1.055, 2.4);
      });
      var lum = 0.2126 * lin[0] + 0.7152 * lin[1] + 0.0722 * lin[2];
      return lum > 0.4 ? "#1c1b17" : "#ffffff";
    }
    cell.filter(fitsName).append("text")
      .attr("x", function (d) { return -(d.x1 - d.x0) / 2 + 6; })
      .attr("y", function (d) { return -(d.y1 - d.y0) / 2 + 15; })
      .attr("fill", cellInk)
      .style("font-size", "11px")
      .style("pointer-events", "none")
      .text(function (d) { return d.data.name; });
    cell.filter(fitsValue).append("text")
      .attr("x", function (d) { return -(d.x1 - d.x0) / 2 + 6; })
      .attr("y", function (d) { return -(d.y1 - d.y0) / 2 + 28; })
      .attr("fill", cellInk)
      .attr("fill-opacity", 0.75)
      .style("font-size", "10px")
      .style("font-variant-numeric", "tabular-nums")
      .style("pointer-events", "none")
      .text(function (d) { return ctx.fmt(d.value); });

    cell.transition().duration(Math.max(200, ctx.duration * 0.7))
      .delay(function (d, i) { return Math.min(i * 20, 500); })
      .ease(d3.easeCubicOut)
      .attr("opacity", 1)
      .attr("transform", function (d) {
        return "translate(" + (d.x0 + d.x1) / 2 + "," +
          (d.y0 + d.y1) / 2 + ") scale(1)";
      });

    /* Hovering a cell keeps its whole top-level branch lit, dims the
       other branches, and shows the full path and share of the total. */
    cell
      .on("pointerenter pointermove", function (event, d) {
        var mine = topOf(d);
        cell.attr("opacity", function (c) {
          return topOf(c) === mine ? 1 : 0.25;
        });
        var trail = d.ancestors().reverse().slice(1)
          .map(function (a) { return pv.esc(a.data.name); }).join(" / ");
        pv.showTip(ctx, event, "<b>" + trail + "</b><br>" +
          pv.swatchRow(color(mine.data.name), ctx.x.vlab || "value",
                       ctx.fmt(d.value)) +
          " &middot; " + d3.format(".1%")(d.value / total) + " of total");
      })
      .on("pointerleave", function () {
        cell.attr("opacity", 1);
        pv.hideTip(ctx);
      });
  };

  /* ---------- lollipop ---------- */

  pvRenderers.lollipop = function (ctx) {
    var data = ctx.x.data;
    /* The category margin grows with the longest name but may never eat
       more than 45% of the width - a margin that swallows the plot
       ranks nothing. Names beyond the room the margin gives are
       truncated; the tooltip keeps the full text. */
    var perChar = pv.textWidth("x", 11);
    var longestPx = d3.max(data, function (d) {
      return pv.textWidth(d.x, 11); }) || perChar * 4;
    var left = Math.min(190, Math.ceil(22 + longestPx),
                        Math.floor(ctx.width * 0.45));
    /* The nudge keeps float rounding from eating the last character of
       a name that exactly fits. */
    var maxChars = Math.max(4, Math.floor((left - 22) / perChar + 0.01));
    /* End-value labels need right-hand room. "auto" shows them only
       while the plot stays at least 200px wide; once they're dropped
       the right margin shrinks too, giving the plot the space back. */
    var showValues = opt(ctx.x.valueLabels, ctx.width - left - 56 >= 200);
    var m = { top: 8, right: showValues ? 56 : 18, bottom: 34, left: left };
    var iw = ctx.width - m.left - m.right,
        ih = ctx.height - m.top - m.bottom;
    var svg = pv.baseSvg(ctx);
    var g = svg.append("g").attr("transform",
      "translate(" + m.left + "," + m.top + ")");

    var yBand = d3.scalePoint()
      .domain(data.map(function (d) { return d.x; }))
      .range([0, ih]).padding(0.5);
    var x = d3.scaleLinear()
      .domain([0, d3.max(data, function (d) { return d.y; }) || 1])
      .nice().range([0, iw]);

    var xAxis = g.append("g").attr("transform", "translate(0," + ih + ")")
      .call(d3.axisBottom(x).ticks(Math.min(6, Math.floor(iw / 80)))
        .tickFormat(pv.fmtTick).tickSizeOuter(0));
    pv.styleAxis(xAxis, ctx.theme, true);
    var yAxis = g.append("g").call(d3.axisLeft(yBand).tickSize(0)
      .tickFormat(function (d) { return pv.truncate(d, maxChars); }));
    pv.styleAxis(yAxis, ctx.theme, false);

    var row = g.selectAll("g.row").data(data).enter().append("g")
      .attr("class", "row");

    var stems = row.append("line")
      .attr("x1", 0).attr("x2", 0)
      .attr("y1", function (d) { return yBand(d.x); })
      .attr("y2", function (d) { return yBand(d.x); })
      .attr("stroke", ctx.theme.ink.baseline)
      .attr("stroke-width", 1);

    var heads = row.append("circle")
      .attr("cx", 0).attr("cy", function (d) { return yBand(d.x); })
      .attr("r", 0)
      .attr("fill", ctx.theme.palette[0])
      .attr("stroke", ctx.theme.ink.surface).attr("stroke-width", 1.5);

    /* Stems grow out from zero with each head riding its stem tip,
       staggered down the chart. */
    var delayOf = function (d, i) { return Math.min(i * 16, 480); };
    stems.transition().duration(ctx.duration).delay(delayOf)
      .ease(d3.easeCubicOut)
      .attr("x2", function (d) { return x(d.y); });
    heads.transition().duration(ctx.duration).delay(delayOf)
      .ease(d3.easeCubicOut)
      .attr("cx", function (d) { return x(d.y); })
      .attr("r", 5.5);

    /* Compact value at each head - a labelled ranking needs no
       gridlines; the tooltip carries full precision. Skipped entirely
       when the flag resolved to off (no room, or FALSE from R). */
    if (showValues) {
      row.append("text")
        .attr("x", function (d) { return x(d.y) + 10; })
        .attr("y", function (d) { return yBand(d.x); })
        .attr("dominant-baseline", "middle")
        .attr("fill", ctx.theme.ink.secondary)
        .style("font-size", "11px")
        .style("font-variant-numeric", "tabular-nums")
        .style("opacity", 0)
        .text(function (d) { return pv.fmtTick(d.y); })
        .transition().delay(ctx.duration).duration(200)
        .style("opacity", 1);
    }

    /* An invisible strip per row makes the whole line hoverable, not
       just the small head. */
    row.append("rect")
      .attr("x", 0)
      .attr("y", function (d) { return yBand(d.x) - yBand.step() / 2; })
      .attr("width", iw).attr("height", yBand.step())
      .attr("fill", "transparent");

    row
      .on("pointerenter pointermove", function (event, d) {
        row.attr("opacity", function (r) { return r === d ? 1 : 0.3; });
        pv.showTip(ctx, event, pv.esc(d.x) + "<br>" +
          pv.swatchRow(ctx.theme.palette[0], ctx.x.ylab || "value",
                       ctx.fmt(d.y)));
      })
      .on("pointerleave", function () {
        row.attr("opacity", 1);
        pv.hideTip(ctx);
      });
  };

})();
