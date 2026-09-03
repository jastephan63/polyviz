/*
 * Motion renderers: bar-chart race, bump chart.
 * See basic.js for the ctx contract. Both charts share one idea: an
 * entity's rank over time IS the story, so vertical position always
 * encodes rank and the horizontal dimension carries value (race) or
 * time (bump).
 */
(function () {

  /* Riding value labels want to be short: three significant digits, an
     SI suffix for big numbers. Tooltips carry the exact value. */
  function fmtVal(v) {
    return Math.abs(v) >= 10000 ?
      d3.format(".3~s")(v) : d3.format(".3~r")(v);
  }

  /* One palette slot per entity, in the order the R side sent them
     (final standings first). Entities beyond the palette's 8 slots
     reuse them faded toward the surface, like the sankey: a repeat
     reads as a dimmer cousin rather than a twin of an unrelated bar,
     and the riding name labels do the real identifying. */
  function slotColor(ctx, i) {
    var pal = ctx.theme.palette;
    var lap = Math.floor(i / pal.length);
    var base = pal[i % pal.length];
    if (!lap) return base;
    return d3.interpolate(base, ctx.theme.ink.surface)(
      Math.min(0.62, 0.28 * lap));
  }

  /* Times travel as numbers, or as ISO strings when the R column was a
     Date - turn both into comparable numbers (Dates become epoch ms). */
  function timeNumbers(ctx) {
    if (ctx.x.ttype === "date") {
      var parse = d3.timeParse("%Y-%m-%d");
      return ctx.x.times.map(function (t) { return +parse(t); });
    }
    return ctx.x.times.map(Number);
  }

  /* Formats one (possibly interpolated) time number for display. Years
     round to whole years; dates show the year alone once the span is a
     few years, the month otherwise. The formatter is built through
     ctx.fmtTime, so the big race readout and the tooltip timestamps
     wear the chart's own locale (pv_locale) when it has one - German
     month names under de-CH - and d3's stock English names otherwise. */
  function timeFormatter(ctx, timesN) {
    if (ctx.x.ttype === "date") {
      var years = (timesN[timesN.length - 1] - timesN[0]) / 31557600000;
      var f = ctx.fmtTime(years >= 4 ? "%Y" : "%b %Y");
      return function (tn) { return f(new Date(tn)); };
    }
    return function (tn) { return String(Math.round(tn)); };
  }

  /* ---------- bar-chart race ---------- */

  pvRenderers.race = function (ctx) {
    var topN = ctx.x.topN;
    var K = ctx.x.times.length;
    var timesN = timeNumbers(ctx);
    var fmtTime = timeFormatter(ctx, timesN);

    /* Index the complete grid the R side sent into one series per
       entity, aligned on the time index. */
    var byId = {};
    var series = ctx.x.entities.map(function (id, i) {
      var s = { id: String(id), slot: i, color: slotColor(ctx, i),
                values: new Array(K), ranks: new Array(K),
                v: 0, p: topN + 1 };
      byId[s.id] = s;
      return s;
    });
    var tIdx = {};
    ctx.x.times.forEach(function (t, i) { tIdx[String(t)] = i; });
    ctx.x.data.forEach(function (r) {
      var s = byId[String(r.id)];
      var i = tIdx[String(r.t)];
      if (s && i !== undefined) { s.values[i] = r.value; s.ranks[i] = r.rank; }
    });
    /* The x scale's ceiling at each keyframe is the leader's value;
       between keyframes it interpolates linearly, in step with the
       bar widths, so the leader always just fills the track. */
    var maxAt = timesN.map(function (t, i) {
      return d3.max(series, function (s) { return s.values[i] || 0; }) || 1;
    });

    /* Name and value labels ride the bar ends, so the bars themselves
       stop short of the right edge by a reserve wide enough for the
       longest "Name 12.3k" pair - capped at 40% of the chart so labels
       can never eat the plot. Names are truncated to what actually
       fits; the tooltip carries the full name. The value column is
       measured from the data, so short numbers don't waste width that
       narrow charts need for names. */
    var valW = Math.max(26, d3.max(series, function (s) {
      return d3.max(s.values, function (v) {
        return v == null ? 0 : pv.textWidth(fmtVal(v), 11);
      });
    }) + 4);
    var nameW = d3.max(series, function (s) {
      return pv.textWidth(s.id, 11.5); }) || 40;
    var reserve = Math.max(valW + 28,
      Math.min(0.4 * ctx.width, nameW + valW + 26));
    var maxChars = Math.max(3, Math.floor((reserve - valW - 26) / 6.9));

    /* No axes: every bar carries its own value, so gridlines would only
       repeat what the riding labels already say. The top margin keeps
       the first row clear of the replay control in the corner. */
    var m = { top: 34, right: 8, bottom: 6, left: 8 };
    var iw = Math.max(60, ctx.width - m.left - m.right - reserve);
    var ih = Math.max(80, ctx.height - m.top - m.bottom);
    var rowStep = ih / topN;
    var barH = Math.max(6, rowStep * 0.74);
    var svg = pv.baseSvg(ctx);
    var g = svg.append("g")
      .attr("transform", "translate(" + m.left + "," + m.top + ")");
    var x = d3.scaleLinear().range([0, iw]);

    /* The big time readout, bottom right of the plot, behind the bars.
       Muted enough to read as a watermark, tabular so it doesn't
       wobble while ticking. */
    var timeText = g.append("text")
      .attr("x", iw + reserve - 8).attr("y", ih - 10)
      .attr("text-anchor", "end")
      .attr("fill", ctx.theme.ink.baseline)
      .style("font-size",
        Math.max(28, Math.min(52, Math.round(ih * 0.17))) + "px")
      .style("font-weight", 700)
      .style("font-variant-numeric", "tabular-nums");
    var curTime = "";

    /* Bars park at rank topN + 1 while out of the running - one row
       below the last visible one. Clipping the row area turns that
       parking spot into the slide-in/slide-out edge. */
    var clipId = "pv-race-clip-" + Math.floor(Math.random() * 1e9);
    svg.append("clipPath").attr("id", clipId).append("rect")
      .attr("x", 0).attr("y", -1)
      .attr("width", iw + reserve).attr("height", ih + 1);
    var rows = g.append("g").attr("clip-path", "url(#" + clipId + ")");

    var ent = rows.selectAll("g.ent").data(series).enter().append("g")
      .attr("class", "ent");
    var bars = ent.append("path")
      .attr("class", "bar")
      .attr("fill", function (s) { return s.color; });
    ent.append("text")
      .attr("class", "name")
      .attr("y", barH / 2)
      .attr("dominant-baseline", "middle")
      .attr("fill", ctx.theme.ink.primary)
      .style("font-size", "11.5px")
      .style("font-weight", 600)
      .attr("pointer-events", "none")
      .text(function (s) { return pv.truncate(s.id, maxChars); });
    /* Where the value starts depends on how wide the (truncated) name
       actually renders - measure once, per entity. */
    ent.each(function (s) {
      var t = d3.select(this).select("text.name").node();
      s.nameEnd = (t.getComputedTextLength ?
        t.getComputedTextLength() : pv.textWidth(s.id, 11.5)) + 6;
    });
    ent.append("text")
      .attr("class", "val")
      .attr("y", barH / 2)
      .attr("dominant-baseline", "middle")
      .attr("fill", ctx.theme.ink.secondary)
      .style("font-size", "11px")
      .style("font-variant-numeric", "tabular-nums")
      .attr("pointer-events", "none");

    /* Draws the race at continuous position s in [0, K-1]: keyframe
       values interpolate linearly (so growth reads steady), rank moves
       ease cubically (so overtakes read as decisive swaps). */
    function draw(s) {
      var k = Math.max(0, Math.min(K - 2, Math.floor(s)));
      var f = Math.max(0, Math.min(1, s - k));
      var ef = d3.easeCubicInOut(f);
      x.domain([0, maxAt[k] + (maxAt[k + 1] - maxAt[k]) * f]);
      series.forEach(function (e) {
        var v0 = e.values[k] || 0, v1 = e.values[k + 1] || 0;
        var r0 = e.ranks[k] || topN + 1, r1 = e.ranks[k + 1] || topN + 1;
        e.v = v0 + (v1 - v0) * f;
        e.p = Math.min(topN + 1, r0 + (r1 - r0) * ef);
      });
      ent.attr("transform", function (e) {
        return "translate(0," +
          ((e.p - 1) * rowStep + (rowStep - barH) / 2) + ")";
      });
      ent.select("path.bar").attr("d", function (e) {
        return pv.rightRoundedBar(0, 0, Math.max(0, x(e.v)), barH, 4);
      });
      ent.select("text.name")
        .attr("x", function (e) { return x(e.v) + 8; });
      ent.select("text.val")
        .attr("x", function (e) { return x(e.v) + 8 + e.nameEnd; })
        .text(function (e) { return fmtVal(e.v); });
      curTime = fmtTime(timesN[k] + (timesN[k + 1] - timesN[k]) * f);
      timeText.text(curTime);
    }

    /* The replay control: a quiet circled arrow, top right inside the
       plot, only visible once the race has finished. */
    var replay = svg.append("g")
      .attr("transform", "translate(" + (ctx.width - 26) + ",17)")
      .style("cursor", "pointer")
      .attr("opacity", 0)
      .style("pointer-events", "none");
    replay.append("circle").attr("r", 12)
      .attr("fill", ctx.theme.ink.surface)
      .attr("stroke", ctx.theme.ink.baseline);
    var replayArc = replay.append("path")
      .attr("d", "M0,-5 A5,5 0 1 1 -4.33,-2.5")
      .attr("fill", "none")
      .attr("stroke", ctx.theme.ink.muted)
      .attr("stroke-width", 1.8)
      .attr("stroke-linecap", "round");
    var replayHead = replay.append("path")
      .attr("d", "M4.2,-5 L-0.4,-7.6 L-0.4,-2.4 Z")
      .attr("fill", ctx.theme.ink.muted);
    replay
      .on("pointerenter", function () {
        replayArc.attr("stroke", ctx.theme.ink.primary);
        replayHead.attr("fill", ctx.theme.ink.primary);
      })
      .on("pointerleave", function () {
        replayArc.attr("stroke", ctx.theme.ink.muted);
        replayHead.attr("fill", ctx.theme.ink.muted);
      })
      .on("click", function () { start(); });

    function showReplay(fade) {
      replay.style("pointer-events", "all");
      if (fade) { replay.transition().duration(200).attr("opacity", 1); }
      else { replay.attr("opacity", 1); }
    }
    function hideReplay() {
      replay.interrupt().attr("opacity", 0).style("pointer-events", "none");
    }

    /* One timer runs the whole race: ~900ms per keyframe step at the
       default duration of 500, with a short hold on the opening frame
       so viewers see the starting order. A re-render wipes the
       container, so the timer checks its svg is still attached and
       retires quietly if not. */
    var stepMs = ctx.duration > 0 ?
      Math.max(200, 900 * ctx.duration / 500) : 900;
    var holdMs = 400;
    var timer = null;
    function start() {
      hideReplay();
      if (timer) timer.stop();
      timer = d3.timer(function (elapsed) {
        if (!ctx.el.contains(svg.node())) { timer.stop(); return; }
        var s = Math.max(0, elapsed - holdMs) / stepMs;
        if (s >= K - 1) {
          draw(K - 1);
          timer.stop();
          timer = null;
          showReplay(true);
          return;
        }
        draw(s);
      });
    }

    /* pv_save()'s GIF export captures the race one exact frame at a
       time through this hook, rather than screenshotting the live timer
       run: the timer's clock (Chrome's virtual time included) only
       advances when the compositor happens to produce a frame, so
       wall-clock captures tie every frame to scheduler luck. draw() is
       a pure function of the timeline position, so seeking is exact and
       repeatable. Seeking stops any running timer and hides the replay
       control - a button nothing can press has no place inside a GIF.
       Returns the keyframe count so the caller can check its schedule
       against what the page actually holds. */
    ctx.el.__pvRaceSeek = function (s) {
      if (timer) { timer.stop(); timer = null; }
      hideReplay();
      draw(Math.max(0, Math.min(K - 1, +s || 0)));
      return K;
    };

    /* Hovering a bar dims the others and reads out the exact value at
       the moment shown. Hovering never pauses the race - the tooltip
       refreshes on every pointer move instead. */
    bars
      .on("pointerenter pointermove", function (event, e) {
        ent.attr("opacity", function (o) { return o === e ? 1 : 0.55; });
        pv.showTip(ctx, event, "<b>" + pv.esc(e.id) + "</b> &middot; " +
          pv.esc(curTime) + "<br>" +
          pv.swatchRow(e.color, ctx.x.vlab || "value", ctx.fmt(e.v)));
      })
      .on("pointerleave", function () {
        ent.attr("opacity", 1);
        pv.hideTip(ctx);
      })
      /* In Shiny, clicking a bar reports the entity with its value at
         the moment shown, as input$<id>_click. */
      .on("click", function (event, e) {
        ctx.emit("click", { id: e.id, value: e.v, time: curTime });
      });

    /* duration 0 is the still photograph: the final standings, drawn
       synchronously, no autoplay - only the replay control offers the
       animation. Otherwise fade in on the opening frame and go. */
    if (ctx.duration === 0) {
      draw(K - 1);
      showReplay(false);
    } else {
      draw(0);
      svg.attr("opacity", 0)
        .transition().duration(200).attr("opacity", 1);
      start();
    }
  };

  /* ---------- bump chart ---------- */

  pvRenderers.bump = function (ctx) {
    var topN = ctx.x.topN;
    var K = ctx.x.times.length;
    var timesN = timeNumbers(ctx);
    var fmtTime = timeFormatter(ctx, timesN);

    /* One series per entity: an array over ALL time indices, null where
       the entity holds no drawn rank - d3.line's defined() turns those
       nulls into real gaps, so a line spans exactly the times its
       entity ranks. */
    var byId = {};
    var series = ctx.x.entities.map(function (id, i) {
      var s = { id: String(id), slot: i, color: slotColor(ctx, i),
                pts: new Array(K) };
      byId[s.id] = s;
      return s;
    });
    var tIdx = {};
    ctx.x.times.forEach(function (t, i) { tIdx[String(t)] = i; });
    ctx.x.data.forEach(function (r) {
      var s = byId[String(r.id)];
      var i = tIdx[String(r.t)];
      if (s && i !== undefined) s.pts[i] = { rank: r.rank, value: r.value };
    });

    /* Name labels sit at both ends of each line, so both margins hold
       label text - each capped at 22% of the width (44% combined),
       truncated to fit, full names in the tooltip. Rank numerals get a
       thin extra column on the left when the chart is wide enough. */
    var nameW = d3.max(series, function (s) {
      return pv.textWidth(s.id, 11); }) || 40;
    var sideCap = Math.max(56, 0.22 * ctx.width);
    var labelW = Math.min(sideCap, nameW + 10);
    var maxChars = Math.max(3, Math.floor((labelW - 6) / 6.6));
    var numerals = ctx.width >= 420;
    var m = { top: 14, right: 10 + labelW, bottom: 30,
              left: (numerals ? 18 : 6) + labelW + 8 };
    var iw = Math.max(60, ctx.width - m.left - m.right);
    var ih = Math.max(80, ctx.height - m.top - m.bottom);
    var svg = pv.baseSvg(ctx);
    var g = svg.append("g")
      .attr("transform", "translate(" + m.left + "," + m.top + ")");

    var xs = (ctx.x.ttype === "date" ? d3.scaleTime() : d3.scaleLinear())
      .domain(d3.extent(timesN)).range([0, iw]);
    /* Rank 1 at the top; a little padding keeps dots off the edges. */
    function rankY(r) {
      return 6 + (r - 1) * (ih - 12) / Math.max(1, topN - 1);
    }

    /* One horizontal hairline per rank row anchors the eye; the rank
       numerals ride the left edge of the plot. */
    var rank;
    for (rank = 1; rank <= topN; rank++) {
      g.append("line")
        .attr("x1", 0).attr("x2", iw)
        .attr("y1", rankY(rank)).attr("y2", rankY(rank))
        .attr("stroke", ctx.theme.ink.grid);
      if (numerals) {
        g.append("text")
          .attr("x", -labelW - 12).attr("y", rankY(rank))
          .attr("text-anchor", "end")
          .attr("dominant-baseline", "middle")
          .attr("fill", ctx.theme.ink.muted)
          .style("font-size", "10px")
          .style("font-variant-numeric", "tabular-nums")
          .text(rank);
      }
    }

    var xAxis = d3.axisBottom(xs)
      .ticks(Math.min(K, Math.max(2, Math.floor(iw / 70))))
      .tickSizeOuter(0);
    if (ctx.x.ttype === "number") xAxis.tickFormat(pv.fmtTick);
    g.append("g").attr("transform", "translate(0," + ih + ")")
      .call(xAxis)
      .call(function (s) { pv.styleAxis(s, ctx.theme, true); });

    var lineGen = d3.line()
      .defined(function (p) { return p != null; })
      .curve(d3.curveMonotoneX)
      .x(function (p, i) { return xs(timesN[i]); })
      .y(function (p) { return rankY(p.rank); });

    var paths = g.selectAll("path.bump").data(series).enter()
      .append("path")
      .attr("class", "bump")
      .attr("fill", "none")
      .attr("stroke", function (s) { return s.color; })
      .attr("stroke-width", 2)
      .attr("stroke-linejoin", "round")
      .attr("stroke-linecap", "round")
      .attr("d", function (s) { return lineGen(s.pts); });

    /* A dot marks every actual measurement - shrunk a little when the
       time axis is dense enough for neighbours to touch. */
    var dotR = K > 30 ? 2.5 : 3.5;
    var dotData = [];
    series.forEach(function (s) {
      s.pts.forEach(function (p, i) {
        if (p) dotData.push({ s: s, ti: i, rank: p.rank, value: p.value });
      });
    });
    var dots = g.selectAll("circle.dot").data(dotData).enter()
      .append("circle")
      .attr("class", "dot")
      .attr("cx", function (d) { return xs(timesN[d.ti]); })
      .attr("cy", function (d) { return rankY(d.rank); })
      .attr("fill", function (d) { return d.s.color; })
      .attr("stroke", ctx.theme.ink.surface)
      .attr("stroke-width", 1.5)
      .attr("pointer-events", "none");

    /* Name labels at both ends of each line - the start rank on the
       left, the finish rank on the right. Labels landing within a line
       of text of each other get nudged apart, like the line chart's
       direct labels. */
    function makeLabels(side) {
      var items = [];
      series.forEach(function (s) {
        var idx = null, i;
        for (i = 0; i < K; i++) {
          var j = side === "left" ? i : K - 1 - i;
          if (s.pts[j]) { idx = j; break; }
        }
        if (idx === null) return;
        items.push({ s: s, x: xs(timesN[idx]) + (side === "left" ? -9 : 9),
                     ly: rankY(s.pts[idx].rank) });
      });
      items.sort(function (a, b) { return a.ly - b.ly; });
      var minGap = 13, i;
      for (i = 1; i < items.length; i++) {
        if (items[i].ly - items[i - 1].ly < minGap) {
          items[i].ly = items[i - 1].ly + minGap;
        }
      }
      return g.selectAll("text.lab-" + side).data(items).enter()
        .append("text")
        .attr("class", "lab lab-" + side)
        .attr("x", function (d) { return d.x; })
        .attr("y", function (d) { return d.ly; })
        .attr("text-anchor", side === "left" ? "end" : "start")
        .attr("dominant-baseline", "middle")
        .attr("fill", ctx.theme.ink.secondary)
        .style("font-size", "11px")
        /* A surface-coloured halo keeps a label legible when it lands
           inside the plot - a late entrant's start label sits at its
           entry point, on top of the rank hairline. */
        .attr("stroke", ctx.theme.ink.surface)
        .attr("stroke-width", 3)
        .style("paint-order", "stroke")
        .attr("pointer-events", "none")
        .text(function (d) { return pv.truncate(d.s.id, maxChars); });
    }
    var labels = d3.selectAll(
      [makeLabels("left"), makeLabels("right")].map(function (sel) {
        return sel.nodes();
      }).reduce(function (a, b) { return a.concat(b); }, []));

    /* Entrances: lines draw in left to right, dots and labels follow.
       With duration 0 everything above is already in its final state -
       no transitions are scheduled at all. */
    if (ctx.duration > 0) {
      paths.each(function () {
        var path = d3.select(this);
        var len = this.getTotalLength();
        path.attr("stroke-dasharray", len + " " + len)
          .attr("stroke-dashoffset", len)
          .transition().duration(ctx.duration).ease(d3.easeCubicInOut)
          .attr("stroke-dashoffset", 0)
          .on("end", function () {
            path.attr("stroke-dasharray", null);
          });
      });
      dots.attr("r", 0)
        .transition()
        .delay(function (d) {
          return K > 1 ? ctx.duration * d.ti / (K - 1) : 0; })
        .duration(150)
        .attr("r", dotR);
      labels.attr("opacity", 0)
        .transition().delay(ctx.duration * 0.5).duration(300)
        .attr("opacity", 1);
    } else {
      dots.attr("r", dotR);
    }

    /* A 2px line is a mean hover target - invisible 10px twins do the
       pointer work. Hovering raises the entity's line, dims the rest,
       and reads out the rank and value at the time nearest the
       pointer. */
    function focus(s) {
      paths.interrupt()
        .attr("stroke-opacity", function (o) { return o === s ? 1 : 0.15; })
        .attr("stroke-width", function (o) { return o === s ? 3 : 2; });
      paths.filter(function (o) { return o === s; }).raise();
      dots.interrupt()
        .attr("opacity", function (d) { return d.s === s ? 1 : 0.15; });
      labels.attr("opacity", function (d) { return d.s === s ? 1 : 0.25; });
    }
    function unfocus() {
      paths.attr("stroke-opacity", 1).attr("stroke-width", 2);
      dots.attr("opacity", 1);
      labels.attr("opacity", 1);
    }

    /* The index of the entity's measurement nearest the pointer's x
       position - what both the tooltip and the click report. */
    function nearestIdx(s, px) {
      var best = null, bd = Infinity, i;
      for (i = 0; i < K; i++) {
        if (!s.pts[i]) continue;
        var d = Math.abs(xs(timesN[i]) - px);
        if (d < bd) { bd = d; best = i; }
      }
      return best;
    }

    g.selectAll("path.hit").data(series).enter()
      .append("path")
      .attr("class", "hit")
      .attr("fill", "none")
      .attr("stroke", "transparent")
      .attr("stroke-width", 10)
      .style("pointer-events", "stroke")
      .attr("d", function (s) { return lineGen(s.pts); })
      .on("pointerenter pointermove", function (event, s) {
        focus(s);
        var best = nearestIdx(s, d3.pointer(event, g.node())[0]);
        if (best === null) return;
        var p = s.pts[best];
        pv.showTip(ctx, event, "<b>" + pv.esc(s.id) + "</b> &middot; " +
          pv.esc(fmtTime(timesN[best])) + "<br>" +
          "rank <b>" + p.rank + "</b><br>" +
          pv.swatchRow(s.color, ctx.x.vlab || "value", ctx.fmt(p.value)));
      })
      .on("pointerleave", function () {
        unfocus();
        pv.hideTip(ctx);
      })
      /* In Shiny, clicking a line reports the entity at the measurement
         nearest the click, as input$<id>_click. `t` is the time value
         exactly as the R side sent it. */
      .on("click", function (event, s) {
        var best = nearestIdx(s, d3.pointer(event, g.node())[0]);
        if (best === null) return;
        var p = s.pts[best];
        ctx.emit("click", {
          id: s.id, t: ctx.x.times[best],
          rank: p.rank, value: p.value
        });
      });
  };

})();
