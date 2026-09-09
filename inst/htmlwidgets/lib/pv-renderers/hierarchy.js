/*
 * Hierarchy renderers: zoomable circle packing, dendrogram, icicle.
 * See basic.js for the ctx contract.
 */
(function () {

  /* Resolve a tri-state option sent from R: TRUE/FALSE force the look,
     "auto" (or a missing field from an older payload) takes the
     decision computed here from the real data and pixel sizes. */
  function opt(v, autoDecision) {
    return v === "auto" || v == null ? autoDecision : !!v;
  }

  /* Label ink chosen per fill: light text on dark fills, dark text on
     light fills, judged by the fill's actual luminance - a fixed ink
     colour can't stay readable across eight hues. */
  function inkOn(fill) {
    var c = d3.color(fill).rgb();
    var lin = [c.r, c.g, c.b].map(function (v) {
      v /= 255;
      return v <= 0.04045 ? v / 12.92 : Math.pow((v + 0.055) / 1.055, 2.4);
    });
    var lum = 0.2126 * lin[0] + 0.7152 * lin[1] + 0.0722 * lin[2];
    return lum > 0.4 ? "#1c1b17" : "#ffffff";
  }

  /* ---------- zoomable circle packing ---------- */

  pvRenderers.pack = function (ctx) {
    var w = ctx.width, h = ctx.height;

    /* The R side sends the same nested {name, children/value} tree the
       treemap uses; sum() totals every branch from its leaves. */
    var root = d3.hierarchy(ctx.x.root)
      .sum(function (d) { return d.value || 0; })
      .sort(function (a, b) { return b.value - a.value; });
    var diameter = Math.max(80, Math.min(w, h) - 8);
    d3.pack().size([diameter, diameter]).padding(3)(root);

    var top = root.children || [];
    var color = d3.scaleOrdinal()
      .domain(top.map(function (d) { return d.data.name; }))
      .range(ctx.theme.palette);

    /* Same colouring idea as the sunburst and treemap: every circle wears
       its top-level branch's colour, faded a step toward the background
       per depth, so a whole branch reads as one family. Light surfaces
       wash colours out faster than dark ones, so fade a smaller step per
       depth in light mode. */
    var fadeStep = ctx.theme.mode === "light" ? 0.15 : 0.22;
    function topOf(d) {
      var anc = d;
      while (anc.depth > 1) anc = anc.parent;
      return anc;
    }
    function fillOf(d) {
      var base = color(topOf(d).data.name);
      return d3.interpolate(base, ctx.theme.ink.surface)(
        fadeStep * (d.depth - 1));
    }

    var svg = pv.baseSvg(ctx);
    var g = svg.append("g")
      .attr("transform", "translate(" + w / 2 + "," + h / 2 + ")");

    /* Everything is drawn relative to the pack's own coordinates and
       re-projected through the current view [cx, cy, span]: the point
       the camera looks at and how much of the pack it sees. Zooming is
       just gliding that view to a new circle. */
    var focus = root;
    var view;
    function viewOf(f) { return [f.x, f.y, f.r * 2 + 6]; }

    var nodes = root.descendants().slice(1);
    var circle = g.append("g").selectAll("circle").data(nodes).enter()
      .append("circle")
      .attr("fill", fillOf)
      .attr("fill-opacity", function (d) { return d.children ? 0.9 : 0.7; })
      .style("cursor", function (d) {
        return d.children ? "pointer" : "default";
      });

    /* The `labels` flag steers eagerness: "auto" labels exactly the
       circles the name honestly fits into, TRUE squeezes labels into
       circles with 20% less room (they must still fit at all - tiny
       circles stay blank), FALSE writes no circle text whatsoever. */
    var showLabels = opt(ctx.x.labels, true);
    var relax = ctx.x.labels === true ? 0.8 : 1;
    /* A label shows only on direct children of the focused circle, and
       only when the name fits the circle's on-screen size at the current
       zoom - everything else stays quiet and lives in the tooltip. */
    function labelShows(d, v) {
      if (!showLabels || d.parent !== focus) return false;
      var k = diameter / v[2];
      return d.r * k >= 9 &&
        2 * d.r * k - 10 >= pv.textWidth(d.data.name, 11) * relax;
    }

    var label = g.append("g")
      .style("pointer-events", "none")
      .selectAll("text").data(nodes).enter().append("text")
      .attr("text-anchor", "middle")
      .attr("dy", "0.35em")
      .attr("fill", function (d) { return inkOn(fillOf(d)); })
      .style("font-size", "11px")
      .text(function (d) { return d.data.name; });

    /* Re-project every circle and label through the view `v`. */
    function zoomTo(v) {
      view = v;
      var k = diameter / v[2];
      function place(d) {
        return "translate(" + (d.x - v[0]) * k + "," +
          (d.y - v[1]) * k + ")";
      }
      circle.attr("transform", place)
        .attr("r", function (d) { return d.r * k; });
      label.attr("transform", place);
    }

    /* Glide the camera to `target` along d3.interpolateZoom's arc - the
       classic pack fly-through - and cross-fade the labels to the set
       that fits at the destination. Clicks always animate, even in
       instant mode: this is user-triggered, not an entrance. */
    function zoomInto(target) {
      focus = target;
      var end = viewOf(focus);
      var i = d3.interpolateZoom(view, end);
      svg.transition().duration(700)
        .tween("zoom", function () {
          return function (t) { zoomTo(i(t)); };
        });
      label.transition().duration(700)
        .attr("fill-opacity", function (d) {
          return labelShows(d, end) ? 1 : 0;
        });
    }

    /* In Shiny, clicking any circle reports its path down the tree -
       e.g. name "Forest", path ["Natural", "Forest"] - as
       input$<id>_click, alongside the zoom. */
    function emitNode(d) {
      ctx.emit("click", {
        name: d.data.name,
        path: d.ancestors().reverse().slice(1)
          .map(function (a) { return a.data.name; }),
        value: d.value
      });
    }
    circle.filter(function (d) { return !!d.children; })
      .on("click", function (event, d) {
        event.stopPropagation();
        emitNode(d);
        /* Clicking the circle already in focus steps back out instead
           of zooming to where we already are. */
        zoomInto(d === focus ? (d.parent || root) : d);
      });
    /* Leaves report their click too, but swallow it so a stray click
       inside a branch doesn't bounce the view around; the background
       steps out one level per click. */
    circle.filter(function (d) { return !d.children; })
      .on("click", function (event, d) {
        event.stopPropagation();
        emitNode(d);
      });
    svg.on("click", function () {
      if (focus.parent) zoomInto(focus.parent);
    });

    /* Initial view: the whole pack. This runs synchronously, so at
       duration 0 the chart is already complete and static. */
    zoomTo(viewOf(root));
    label.attr("fill-opacity", function (d) {
      return labelShows(d, view) ? 1 : 0;
    });

    /* Entrance: circles surface level by level, branches before their
       children, then the labels fade in. Skipped entirely in instant
       mode - everything above already sits in its final state. */
    if (ctx.duration > 0) {
      circle.attr("opacity", 0)
        .transition().duration(ctx.duration)
        .delay(function (d, i) {
          return (d.depth - 1) * 120 + Math.min(i * 8, 240);
        })
        .ease(d3.easeCubicOut)
        .attr("opacity", 1);
      label.each(function (d) { d.finalOp = +this.getAttribute("fill-opacity"); })
        .attr("fill-opacity", 0)
        .transition().delay(ctx.duration * 0.7).duration(250)
        .attr("fill-opacity", function (d) { return d.finalOp; });
    }

    /* Hovering any circle outlines it and shows its full path, exact
       value, and share - the labels only ever carry the short name. */
    var total = root.value || 1;
    circle
      .on("pointerenter pointermove", function (event, d) {
        d3.select(this).attr("stroke", ctx.theme.ink.primary)
          .attr("stroke-width", 1.5);
        var trail = d.ancestors().reverse().slice(1)
          .map(function (a) { return pv.esc(a.data.name); }).join(" / ");
        pv.showTip(ctx, event, "<b>" + trail + "</b><br>" +
          pv.swatchRow(color(topOf(d).data.name), ctx.x.vlab || "value",
                       ctx.fmt(d.value)) +
          " &middot; " + d3.format(".1%")(d.value / total) + " of total");
      })
      .on("pointerleave", function () {
        d3.select(this).attr("stroke", "none");
        pv.hideTip(ctx);
      });
  };

  /* ---------- zoomable icicle ---------- */

  pvRenderers.icicle = function (ctx) {
    /* The rectangular sunburst: same nested {name, children/value} tree,
       same d3.partition, but the rings become columns - the root band at
       the left edge, depth growing rightward, each segment's height its
       share. The zoom below mirrors the sunburst's move for move, with
       the angular coordinate traded for a vertical one. */
    var root = d3.hierarchy(ctx.x.root)
      .sum(function (d) { return d.value || 0; })
      .sort(function (a, b) { return b.value - a.value; });

    var top = root.children || [];
    var color = d3.scaleOrdinal()
      .domain(top.map(function (d) { return d.data.name; }))
      .range(ctx.theme.palette);

    var m = { top: 4, right: 16, bottom: 10, left: 16 };
    var iw = ctx.width - m.left - m.right,
        ih = ctx.height - m.top - m.bottom;
    /* Partition in abstract units: x runs 0..ih down the chart (the
       sunburst's angle), y counts depth bands 0..height+1 (its rings).
       Band 0 is the root's own - drawn, unlike the sunburst's hole,
       because it doubles as the zoom-out control, but squeezed to a
       narrow spine at the left edge so the data columns keep the room. */
    d3.partition().size([ih, root.height + 1])(root);
    root.each(function (d) { d.current = d; });
    var navW = 26;
    var bandW = (iw - navW) / Math.max(1, root.height);

    /* Same colouring idea as the sunburst: every segment wears its
       top-level branch's colour, faded a step toward the background per
       column, so a whole branch reads as one family. */
    var fadeStep = ctx.theme.mode === "light" ? 0.15 : 0.22;
    function topOf(d) {
      var anc = d;
      while (anc.depth > 1) anc = anc.parent;
      return anc;
    }
    function fillOf(d) {
      var base = color(topOf(d).data.name);
      return d3.interpolate(base, ctx.theme.ink.surface)(
        fadeStep * (d.depth - 1));
    }

    var svg = pv.baseSvg(ctx);
    var g = svg.append("g").attr("transform",
      "translate(" + m.left + "," + m.top + ")");

    /* Texture fills (pv_textures): every segment of a branch wears that
       branch's hatch slot in its own depth-faded colour. Unlike the
       treemap, segments of one branch sit at several depths, and
       pv.texturePattern registers one colour per slot per <defs> - so
       each depth gets its own defs, and the slot/colour pairing stays
       honest at every level. */
    var texDefs = ctx.x.textures === true ? {} : null;
    var texSlot = {};
    top.forEach(function (d, i) {
      texSlot[d.data.name] = i % ctx.theme.palette.length;
    });
    function cellFill(d) {
      if (!texDefs) return fillOf(d);
      if (!texDefs[d.depth]) texDefs[d.depth] = svg.append("defs");
      return pv.texturePattern(texDefs[d.depth],
        texSlot[topOf(d).data.name], fillOf(d), ctx.theme);
    }
    /* Label ink is judged against what the eye actually sees: a
       textured segment is mostly its lightened ground, so that is what
       gets judged - the same rule the treemap applies. */
    function labelInk(d) {
      return inkOn(texDefs ?
        pv.textureGround(fillOf(d), ctx.theme) : fillOf(d));
    }

    /* A segment is on screen when its band lies right of the root
       column and it still has height. Every rect keeps a 2px surface
       gap from its neighbours on both axes. */
    function visible(pos) {
      return pos.y1 <= root.height + 1 && pos.y0 >= 1 && pos.x1 > pos.x0;
    }
    function rectX(pos) { return navW + (pos.y0 - 1) * bandW + 1; }
    function rectY(pos) { return pos.x0 + 1; }
    function rectH(pos) { return Math.max(0, pos.x1 - pos.x0 - 2); }
    var rectW = Math.max(0, bandW - 2);

    /* The icicle's whole advantage over the sunburst: labels lie flat.
       One is written on every segment tall enough for a line of text
       (and wide enough for at least a few characters), truncated to the
       column's width - the tooltip always carries the full trail. The
       `labels` flag steers eagerness the way the pack's does: "auto"
       wants a comfortable line, TRUE accepts segments about 20%
       shorter, FALSE writes nothing at all. */
    var showLabels = opt(ctx.x.labels, true);
    var relax = ctx.x.labels === true ? 0.8 : 1;
    /* The nudge keeps float rounding from eating the last character of
       a name that exactly fits. */
    var maxChars = Math.max(3,
      Math.floor((rectW - 12) / pv.textWidth("x", 11) + 0.01));
    function labelFits(pos) {
      return showLabels && visible(pos) &&
        rectH(pos) >= 14 * relax && rectW >= 30;
    }
    function labelTransform(pos) {
      return "translate(" + (rectX(pos) + 6) + "," +
        (rectY(pos) + rectH(pos) / 2) + ")";
    }

    var descendants = root.descendants().slice(1);
    var cell = g.append("g").selectAll("rect").data(descendants).enter()
      .append("rect")
      .attr("x", function (d) { return rectX(d.current); })
      .attr("y", function (d) { return rectY(d.current); })
      .attr("width", rectW)
      .attr("height", function (d) { return rectH(d.current); })
      .attr("rx", 2)
      .attr("fill", cellFill)
      .attr("fill-opacity", function (d) {
        return visible(d.current) ? (d.children ? 0.9 : 0.65) : 0;
      })
      .attr("pointer-events", function (d) {
        return visible(d.current) ? "auto" : "none";
      })
      .style("cursor", function (d) {
        return d.children ? "pointer" : "default";
      });

    var label = g.append("g")
      .attr("pointer-events", "none")
      .selectAll("text").data(descendants).enter().append("text")
      .attr("dy", "0.35em")
      .attr("fill", labelInk)
      .attr("fill-opacity", function (d) { return +labelFits(d.current); })
      .attr("transform", function (d) { return labelTransform(d.current); })
      .style("font-size", "11px")
      .text(function (d) { return pv.truncate(d.data.name, maxChars); });

    /* The root band: a neutral spine down the left edge. Zoomed in, it
       names the branch in focus - written upward, like a book spine -
       and one click steps back out: the sunburst's centre circle,
       squared off. */
    var parentRect = g.append("rect")
      .datum(root)
      .attr("x", 1).attr("y", 1)
      .attr("width", Math.max(0, navW - 3))
      .attr("height", Math.max(0, ih - 2))
      .attr("rx", 2)
      .attr("fill", ctx.theme.ink.grid)
      .attr("fill-opacity", 0.6)
      .style("cursor", "pointer");
    var parentLabel = g.append("text")
      .attr("dy", "0.35em")
      .attr("text-anchor", "middle")
      .attr("transform",
        "translate(" + (navW / 2 - 1) + "," + ih / 2 + ") rotate(-90)")
      .attr("fill", ctx.theme.ink.secondary)
      .style("font-size", "11px")
      .style("pointer-events", "none")
      .text("");

    /* The zoom. Clicking a branch makes it the new left edge: every node
       gets a "target" position rescaled so the branch spans the full
       height, then all rects glide from where they are to where they
       belong. Clicking the root band zooms back out one level. */
    function clicked(event, p) {
      var target = p.children ? p : p.parent;
      if (!target) return;
      /* A click can land mid-entrance; scheduling the zoom would cancel
         that transition and freeze cells half-faded, so the entrance is
         finished instantly first. */
      cell.interrupt().attr("opacity", 1);
      parentRect.datum(target.parent || root);
      /* The spine runs the chart's full height, so the name can afford
         to be long - truncated only against that height. */
      parentLabel.text(target === root ? "" :
        pv.truncate(target.data.name,
          Math.max(4, Math.floor((ih - 16) / pv.textWidth("x", 11)))));

      root.each(function (d) {
        d.target = {
          x0: Math.max(0, Math.min(1,
            (d.x0 - target.x0) / (target.x1 - target.x0))) * ih,
          x1: Math.max(0, Math.min(1,
            (d.x1 - target.x0) / (target.x1 - target.x0))) * ih,
          y0: Math.max(0, d.y0 - target.depth),
          y1: Math.max(0, d.y1 - target.depth)
        };
      });

      var t = g.transition().duration(ctx.duration || 750);
      cell.transition(t)
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
        .attrTween("x", function (d) {
          return function () { return rectX(d.current); };
        })
        .attrTween("y", function (d) {
          return function () { return rectY(d.current); };
        })
        .attrTween("height", function (d) {
          return function () { return rectH(d.current); };
        });
      label.filter(function (d) {
        return +this.getAttribute("fill-opacity") || labelFits(d.target);
      }).transition(t)
        .attr("fill-opacity", function (d) { return +labelFits(d.target); })
        .attrTween("transform", function (d) {
          return function () { return labelTransform(d.current); };
        });
    }

    /* In Shiny, clicking any segment reports its path down the tree -
       e.g. name "Forest", path ["Natural", "Forest"] - as
       input$<id>_click. Branch segments also zoom; the root band only
       zooms back out. */
    function emitNode(d) {
      ctx.emit("click", {
        name: d.data.name,
        path: d.ancestors().reverse().slice(1)
          .map(function (a) { return a.data.name; }),
        value: d.value
      });
    }
    cell.filter(function (d) { return !!d.children; })
      .on("click", function (event, d) {
        emitNode(d);
        clicked(event, d);
      });
    cell.filter(function (d) { return !d.children; })
      .on("click", function (event, d) { emitNode(d); });
    parentRect.on("click", clicked);

    /* Entrance: columns surface left to right, each fading in as it
       arrives, then the labels. Skipped entirely in instant mode -
       everything above is already drawn in its final state. */
    if (ctx.duration > 0) {
      cell.attr("opacity", 0)
        .transition().duration(Math.max(200, ctx.duration * 0.6))
        .delay(function (d) { return (d.depth - 1) * 150; })
        .ease(d3.easeCubicOut)
        .attr("opacity", 1);
      label.each(function (d) {
        d.finalOp = +this.getAttribute("fill-opacity");
      })
        .attr("fill-opacity", 0)
        .transition().delay(ctx.duration * 0.7).duration(250)
        .attr("fill-opacity", function (d) { return d.finalOp; });
    }

    /* Hovering any segment shows its full path, exact value, and share
       - the flat labels only ever carry the (possibly truncated) name. */
    var total = root.value || 1;
    cell
      .on("pointerenter pointermove", function (event, d) {
        d3.select(this).attr("stroke", ctx.theme.ink.primary)
          .attr("stroke-width", 1.5);
        var trail = d.ancestors().reverse().slice(1)
          .map(function (a) { return pv.esc(a.data.name); }).join(" / ");
        pv.showTip(ctx, event, "<b>" + trail + "</b><br>" +
          pv.swatchRow(color(topOf(d).data.name), ctx.x.vlab || "value",
                       ctx.fmt(d.value)) +
          " &middot; " + d3.format(".1%")(d.value / total) + " of total");
      })
      .on("pointerleave", function () {
        d3.select(this).attr("stroke", "none");
        pv.hideTip(ctx);
      });
  };

  /* ---------- dendrogram ---------- */

  pvRenderers.dendrogram = function (ctx) {
    /* The R side unfolds the hclust object into a nested
       {name, height, children} tree; every internal node carries the
       height its two pieces merged at, leaves sit at height 0. */
    var root = d3.hierarchy(ctx.x.tree);
    var leaves = root.leaves();
    var maxH = d3.max(root.descendants(), function (d) {
      return d.data.height || 0; }) || 1;

    /* With a cut (k from R), every leaf carries its cluster id. A branch
       whose leaves all share one cluster wears that cluster's colour;
       branches above the cut - mixed clusters - keep the neutral
       baseline ink, so the cut is visible as the point where colour
       drains out of the tree. Without a cut the whole tree draws in one
       structural ink. */
    var hasK = ctx.x.k != null;
    root.eachAfter(function (d) {
      if (!d.children) {
        d.cluster = hasK ? d.data.cluster : null;
      } else {
        var c = d.children[0].cluster;
        for (var i = 1; i < d.children.length; i++) {
          if (d.children[i].cluster !== c) c = null;
        }
        d.cluster = c;
      }
    });
    function strokeOf(d) {
      if (d.cluster != null) {
        return ctx.theme.palette[(d.cluster - 1) % ctx.theme.palette.length];
      }
      return hasK ? ctx.theme.ink.baseline : ctx.theme.ink.secondary;
    }

    /* Leaf labels live in a right margin sized to the longest name but
       never more than 45% of the width - names beyond that are
       truncated, and hovering a leaf gives the full text. */
    var longest = d3.max(leaves, function (d) {
      return pv.textWidth(d.data.name, 11); }) || 30;
    var m = { top: 40, bottom: 10, left: 10,
              right: Math.max(46, Math.min(Math.ceil(longest + 16),
                Math.floor(0.45 * ctx.width))) };
    /* The nudge keeps float rounding from eating the last character of
       a name that exactly fits. */
    var maxChars = Math.max(4,
      Math.floor((m.right - 16) / pv.textWidth("x", 11) + 0.01));
    var iw = ctx.width - m.left - m.right,
        ih = ctx.height - m.top - m.bottom;
    var svg = pv.baseSvg(ctx);
    var g = svg.append("g").attr("transform",
      "translate(" + m.left + "," + m.top + ")");

    /* Horizontal layout: root on the left, leaves on the right so their
       labels read normally. d3.cluster spaces the leaves evenly down the
       side (that's all it is used for); the horizontal position of every
       node is its actual merge height on a linear scale, which is the
       whole point of a dendrogram - tall elbows mean dissimilar pieces. */
    d3.cluster().size([ih, iw])
      .separation(function () { return 1; })(root);
    var xScale = d3.scaleLinear().domain([0, maxH]).range([iw, 0]);
    root.each(function (d, i) {
      d.hx = xScale(d.data.height || 0);
      d.pvId = i;
    });

    var axis = g.append("g").call(
      d3.axisTop(xScale).ticks(Math.min(6, Math.max(2, Math.floor(iw / 70))))
        .tickFormat(pv.fmtTick).tickSizeOuter(0));
    pv.styleAxis(axis, ctx.theme, true);
    /* Name the scale right above its ticks - the one number a reader
       must not misread as a data value. */
    g.append("text")
      .attr("x", 0).attr("y", -26)
      .attr("fill", ctx.theme.ink.muted)
      .style("font-size", "10.5px")
      .text("height");

    /* Each link is a right-angle elbow from the child across to the
       parent's merge height, then up or down to the parent's spine. */
    function elbow(d) {
      return "M" + d.hx + "," + d.x +
        "H" + d.parent.hx + "V" + d.parent.x;
    }
    var link = g.append("g").attr("fill", "none")
      .selectAll("path").data(root.descendants().slice(1)).enter()
      .append("path")
      .attr("stroke", strokeOf)
      .attr("stroke-width", 1.5)
      .attr("stroke-linecap", "round")
      .attr("d", elbow);

    var leafLab = g.append("g")
      .selectAll("text").data(leaves).enter().append("text")
      .attr("x", iw + 8)
      .attr("y", function (d) { return d.x; })
      .attr("dominant-baseline", "middle")
      .attr("fill", ctx.theme.ink.secondary)
      .style("font-size", "11px")
      .text(function (d) { return pv.truncate(d.data.name, maxChars); });

    /* A small dot marks every junction; an invisible fatter twin on top
       does the pointer work, because a 2.5px dot is a mean hover target. */
    var internals = root.descendants().filter(function (d) {
      return !!d.children; });
    var dot = g.append("g")
      .selectAll("circle").data(internals).enter().append("circle")
      .attr("cx", function (d) { return d.hx; })
      .attr("cy", function (d) { return d.x; })
      .attr("r", 2.5)
      .attr("fill", strokeOf)
      .attr("stroke", ctx.theme.ink.surface).attr("stroke-width", 1);

    /* Hovering a junction lights its whole subtree and dims the rest,
       so one merge reads as the group of leaves it actually joins. */
    function highlight(target) {
      var inSet = {};
      target.descendants().forEach(function (d) { inSet[d.pvId] = true; });
      link.interrupt().attr("opacity", function (d) {
        return inSet[d.pvId] ? 1 : 0.2; });
      dot.attr("opacity", function (d) { return inSet[d.pvId] ? 1 : 0.2; });
      leafLab.interrupt().attr("opacity", function (d) {
        return inSet[d.pvId] ? 1 : 0.2; });
    }
    function unhighlight() {
      link.attr("opacity", 1);
      dot.attr("opacity", 1);
      leafLab.attr("opacity", 1);
    }

    g.append("g")
      .selectAll("circle").data(internals).enter().append("circle")
      .attr("cx", function (d) { return d.hx; })
      .attr("cy", function (d) { return d.x; })
      .attr("r", 9)
      .attr("fill", "transparent")
      .on("pointerenter pointermove", function (event, d) {
        highlight(d);
        var rows = ["<b>" + d.leaves().length + " leaves</b>",
                    "merge height: <b>" + ctx.fmt(d.data.height) + "</b>"];
        if (d.cluster != null) {
          rows.push(pv.swatchRow(strokeOf(d), "cluster", d.cluster));
        }
        pv.showTip(ctx, event, rows.join("<br>"));
      })
      .on("pointerleave", function () {
        unhighlight();
        pv.hideTip(ctx);
      })
      /* In Shiny, clicking a junction reports the merge: the names of
         every leaf under it and the height it happened at. */
      .on("click", function (event, d) {
        ctx.emit("click", {
          leaves: d.leaves().map(function (l) { return l.data.name; }),
          height: d.data.height
        });
      });

    /* Leaves answer with their full, untruncated name (plus cluster). An
       invisible strip the height of the label row makes the whole name
       hoverable. */
    g.append("g")
      .selectAll("rect").data(leaves).enter().append("rect")
      .attr("x", iw + 2)
      .attr("y", function (d) { return d.x - 7; })
      .attr("width", m.right - 4).attr("height", 14)
      .attr("fill", "transparent")
      .on("pointerenter pointermove", function (event, d) {
        leafLab.attr("fill", function (l) {
          return l === d ? ctx.theme.ink.primary : ctx.theme.ink.secondary;
        });
        var rows = ["<b>" + pv.esc(d.data.name) + "</b>"];
        if (d.cluster != null) {
          rows.push(pv.swatchRow(strokeOf(d), "cluster", d.cluster));
        }
        pv.showTip(ctx, event, rows.join("<br>"));
      })
      .on("pointerleave", function () {
        leafLab.attr("fill", ctx.theme.ink.secondary);
        pv.hideTip(ctx);
      });

    /* Entrance: the tree fades in from the root outward, labels last.
       Skipped entirely in instant mode - everything above is already
       drawn in its final state, so the chart is complete and static. */
    if (ctx.duration > 0) {
      var depthDelay = function (d) { return Math.min(d.depth * 40, 400); };
      link.attr("opacity", 0)
        .transition().duration(Math.max(200, ctx.duration * 0.6))
        .delay(depthDelay)
        .attr("opacity", 1);
      dot.attr("opacity", 0)
        .transition().duration(Math.max(200, ctx.duration * 0.6))
        .delay(depthDelay)
        .attr("opacity", 1);
      leafLab.attr("opacity", 0)
        .transition().delay(ctx.duration * 0.6).duration(250)
        .attr("opacity", 1);
    }
  };

})();
