/*
 * Geographic renderers: the choropleth and the bubble map.
 * See basic.js for the ctx contract. Projection and path drawing come
 * from the d3-geo functions bundled with d3 v7; the map itself travels
 * in the payload as a GeoJSON FeatureCollection.
 */
(function () {

  /* Resolves a TRUE/FALSE/"auto" option sent from R - the same helper
     the cartesian renderers carry. Explicit values win; "auto" (or an
     old payload without the field) takes the render-time decision. */
  function opt(v, autoDecision) {
    return v === "auto" || v == null ? autoDecision : !!v;
  }

  /* GeoJSON (RFC 7946) winds outer rings counterclockwise, but d3-geo
     works on the sphere with the opposite convention - a ring wound the
     "wrong" way is read as everything on Earth EXCEPT the region, which
     renders as one giant blob. Rings whose spherical area says they
     enclose more than half the planet are reversed; the payload's own
     map object stays untouched (re-renders start from it again). */
  function rewind(geo) {
    function fixPoly(coords) {
      return coords.map(function (ring, i) {
        var area = d3.geoArea({ type: "Polygon", coordinates: [ring] });
        var backwards = i === 0 ?
          area > 2 * Math.PI :   /* outer ring must enclose the small side */
          area < 2 * Math.PI;    /* holes wind opposite the outer ring */
        return backwards ? ring.slice().reverse() : ring;
      });
    }
    return {
      type: "FeatureCollection",
      /* Features without drawable coordinates (real-world files carry
         empty stubs - the bundled Lucerne map has 13) would only bind
         empty <path>s; leave them out. */
      features: geo.features.filter(function (f) {
        return f.geometry && f.geometry.coordinates &&
          f.geometry.coordinates.length;
      }).map(function (f) {
        var geom = f.geometry;
        if (geom.type === "Polygon") {
          geom = { type: "Polygon", coordinates: fixPoly(geom.coordinates) };
        } else if (geom.type === "MultiPolygon") {
          geom = { type: "MultiPolygon",
                   coordinates: geom.coordinates.map(fixPoly) };
        }
        return { type: "Feature", properties: f.properties, geometry: geom };
      })
    };
  }

  /* A feature's id or name, if it has a usable one. Background
     features (the lakes in the bundled Lucerne map) carry empty
     properties, which jsonlite serialises as {} - so only plain
     strings and numbers count as present. */
  function propOf(f, key) {
    var v = f.properties && f.properties[key];
    return typeof v === "string" || typeof v === "number" ? v : null;
  }

  /* Compact numbers for legend labels: three significant digits with
     an SI suffix past 10k ("23.3M"), at most two decimals below - the
     legend rounds, the tooltips carry the precision. */
  function legFmt(v) {
    return Math.abs(v) >= 10000 ?
      d3.format(".3~s")(v) : d3.format(",.2~f")(v);
  }

  /* The water tone: the theme's surface nudged toward the sequential
     ramp's low-mid blue - quiet enough to read as geography, desaturated
     enough not to pass for a data colour on the blue sequential ramp.
     `t` is how far to lean in: the fill stays pale, the shoreline goes
     deeper so lakes read as drawn objects, not pale data regions. */
  function waterTone(theme, t) {
    var seq = theme.sequential || [];
    return d3.interpolateRgb(theme.ink.surface, seq[2] || "#9cbdd7")(t);
  }

  /* Paints the lakes layer the R side attached (ctx.x.lakes), through
     the same projection as the base map. The country-wide region
     polygons include their lake surfaces, so the water goes OVER the
     region fills - the ThemaKart convention - and under everything
     interactive: the group ignores the pointer, and a hovered region
     rises only within its own group, staying beneath the water. Lakes
     shared with the neighbours (Lago Maggiore, Lac Léman) reach past
     the fitted extent, so the layer is clipped to the plot box. */
  function drawLakes(ctx, svg, g, path, iw, ih) {
    if (!ctx.x.lakes) return;
    var lakes = rewind(ctx.x.lakes);
    var clipId = "pv-lake-clip-" + Math.floor(Math.random() * 1e9);
    svg.append("clipPath").attr("id", clipId)
      .append("rect").attr("width", iw).attr("height", ih);
    g.append("g")
      .attr("clip-path", "url(#" + clipId + ")")
      .attr("pointer-events", "none")
      .selectAll("path.lake").data(lakes.features).enter()
      .append("path")
      .attr("class", "lake")
      .attr("d", path)
      .attr("fill", waterTone(ctx.theme, 0.45))
      .attr("stroke", waterTone(ctx.theme, 0.8))
      .attr("stroke-width", 0.75)
      .attr("stroke-linejoin", "round");
  }

  pvRenderers.choropleth = function (ctx) {
    var geo = rewind(ctx.x.map);
    var domain = ctx.x.domain;
    var diverging = ctx.x.palette === "diverging";
    var center = typeof ctx.x.center === "number" ? ctx.x.center : null;

    /* The join table: region id -> value. Ids are compared as strings on
       both sides (the R side does the same), so 1001 and "1001" name the
       same region. */
    var valueById = {};
    ctx.x.data.forEach(function (d) { valueById[String(d.id)] = d.value; });

    function valueOf(f) {
      var id = propOf(f, "id");
      return id === null ? undefined : valueById[String(id)];
    }

    /* The colour of a region. Sequential glides through the theme's
       11-step ramp over the data range; diverging pins its neutral
       midpoint to the reference value (the R side made the domain
       symmetric around it, so the poles carry equal weight). */
    var colorOf;
    if (diverging) {
      var dv = ctx.theme.diverging;
      colorOf = d3.scaleDiverging(
        d3.piecewise(d3.interpolateRgb, [dv.low, dv.mid, dv.high]))
        .domain([domain[0], center, domain[1]]).clamp(true);
    } else {
      var ramp = d3.interpolateRgbBasis(ctx.theme.sequential);
      var t = d3.scaleLinear().domain(domain).range([0, 1]).clamp(true);
      colorOf = function (v) { return ramp(t(v)); };
    }

    /* The map's legend is its colour scale: a small gradient bar in the
       header with the domain ends labelled - and, for diverging, a tick
       marking where the reference value sits, since that midpoint is
       what the whole palette pivots on. */
    var scaleRow = document.createElement("div");
    scaleRow.style.cssText =
      "display:flex;align-items:flex-start;gap:7px;margin-top:7px;" +
      "font-size:11px;line-height:12px;font-variant-numeric:tabular-nums;" +
      "color:" + ctx.theme.ink.muted + ";";
    /* The legend shows the slice of the scale the data actually spans -
       a symmetric diverging domain can reach far beyond the observed
       values, and labelling those phantom endpoints would put numbers on
       the legend that exist nowhere on the map. */
    var leg = ctx.x.obs || domain;
    var stops = [];
    for (var i = 0; i <= 10; i++) {
      stops.push(colorOf(leg[0] + (leg[1] - leg[0]) * i / 10) +
        " " + (i * 10) + "%");
    }
    var barWrap = document.createElement("span");
    barWrap.style.cssText = "position:relative;width:140px;" +
      "height:" + (diverging ? 26 : 10) + "px;flex:none;";
    var bar = document.createElement("span");
    bar.style.cssText = "position:absolute;left:0;top:2px;width:140px;" +
      "height:8px;border-radius:4px;" +
      "background:linear-gradient(90deg," + stops.join(",") + ");";
    barWrap.appendChild(bar);
    if (diverging && center >= leg[0] && center <= leg[1]) {
      /* The reference tick sits where the centre value falls within the
         observed range, not at a fixed midpoint. */
      var pct = 100 * (center - leg[0]) / (leg[1] - leg[0] || 1);
      var tick = document.createElement("span");
      tick.style.cssText = "position:absolute;left:" + pct + "%;top:0;" +
        "width:1px;height:12px;background:" + ctx.theme.ink.baseline + ";";
      var mid = document.createElement("span");
      mid.textContent = legFmt(center);
      mid.style.cssText = "position:absolute;left:" +
        Math.max(8, Math.min(92, pct)) + "%;top:14px;" +
        "transform:translateX(-50%);white-space:nowrap;";
      barWrap.appendChild(tick);
      barWrap.appendChild(mid);
    }
    var lo = document.createElement("span");
    lo.textContent = legFmt(leg[0]);
    var hi = document.createElement("span");
    hi.textContent = legFmt(leg[1]);
    scaleRow.appendChild(lo);
    scaleRow.appendChild(barWrap);
    scaleRow.appendChild(hi);
    ctx.header.appendChild(scaleRow);
    ctx.height = Math.max(120, ctx.height - scaleRow.offsetHeight - 7);

    /* No axes, so the margins are just breathing room around the shape. */
    var m = { top: 8, right: 16, bottom: 10, left: 16 };
    var iw = Math.max(50, ctx.width - m.left - m.right),
        ih = Math.max(80, ctx.height - m.top - m.bottom);
    var svg = pv.baseSvg(ctx);
    var g = svg.append("g").attr("transform",
      "translate(" + m.left + "," + m.top + ")");

    /* Fit the whole FeatureCollection into the plot box; fitSize keeps
       the aspect ratio and centres the map inside it. */
    var projection = d3.geoMercator().fitSize([iw, ih], geo);
    var path = d3.geoPath(projection);

    /* Each region's resting stroke: hairline surface-coloured borders
       between coloured regions, and a quiet dashed outline in the muted
       label grey on regions without data, so absence stays visible
       rather than passing itself off as a low value - a diverging
       palette's neutral midpoint is dangerously close to the no-data
       grey by fill alone. */
    function restStroke(sel) {
      sel.attr("stroke", function (f) {
        return valueOf(f) === undefined ?
          ctx.theme.ink.muted : ctx.theme.ink.surface;
      })
      .attr("stroke-width", 1)
      .attr("stroke-dasharray", function (f) {
        return valueOf(f) === undefined ? "3,2" : null;
      });
    }

    /* The regions live in a group of their own, so the hover raise
       further down lifts a region above its neighbours but never above
       the lakes drawn after them. */
    var regions = g.append("g")
      .selectAll("path.region").data(geo.features).enter()
      .append("path")
      .attr("class", "region")
      .attr("d", path)
      .attr("fill", function (f) {
        var v = valueOf(f);
        return v === undefined ? ctx.theme.ink.grid : colorOf(v);
      })
      .attr("stroke-linejoin", "round")
      .call(restStroke)
      /* Features with neither id nor name (the lakes) are background
         geography - nothing to say about them, so no hover either. */
      .style("pointer-events", function (f) {
        return propOf(f, "id") === null && propOf(f, "name") === null ?
          "none" : null;
      });

    /* The Swiss lakes, when the R side attached them. Appended after the
       regions so the water stays visible over the coloured fills. */
    drawLakes(ctx, svg, g, path, iw, ih);

    /* Linked selection (pv_link): regions key on their id, as a string.
       While a selection exists - made here by clicking, or anywhere
       else in the crosstalk group - regions outside it fade through
       fill-opacity. Background features without an id never dim. */
    function keyOf(f) {
      var id = propOf(f, "id");
      return id === null ? null : String(id);
    }
    var selected = (ctx.selected || []).slice();
    function applySelection() {
      regions.attr("fill-opacity", function (f) {
        var k = keyOf(f);
        return k === null ? 1 : pv.keyOpacity(ctx, k, 1, 0.25);
      });
    }
    applySelection();

    /* Clicking a region toggles it in the selection and tells both
       Shiny (input$<id>_click) and the crosstalk group about it;
       clicking the empty background clears the whole selection. The
       local set keeps the toggle working even without crosstalk. */
    regions.filter(function (f) { return keyOf(f) !== null; })
      .style("cursor", "pointer")
      .on("click", function (event, f) {
        event.stopPropagation();
        var k = keyOf(f);
        var i = selected.indexOf(k);
        if (i >= 0) { selected.splice(i, 1); } else { selected.push(k); }
        ctx.selected = selected.length ? selected.slice() : null;
        ctx.el.__pvSelected = ctx.selected;
        applySelection();
        ctx.select(selected.slice());
        var v = valueOf(f);
        ctx.emit("click", {
          id: k, name: propOf(f, "name"),
          value: v === undefined ? null : v,
          selected: i < 0
        });
      });
    svg.on("click", function () {
      if (!selected.length) return;
      selected = [];
      ctx.selected = null;
      ctx.el.__pvSelected = null;
      applySelection();
      ctx.select([]);
    });

    /* Entrance: regions fade in swept west to east across the map, by
       centroid. Kept well under 600ms in total - and skipped entirely at
       duration 0, where the final state must exist synchronously. */
    if (ctx.duration > 0) {
      var xs = geo.features.map(function (f) {
        var cx = path.centroid(f)[0];
        return isFinite(cx) ? cx : 0;
      });
      var xMin = d3.min(xs), xSpan = Math.max(1, d3.max(xs) - xMin);
      var fade = Math.min(200, ctx.duration);
      var sweep = Math.min(400, Math.max(0, ctx.duration - fade));
      regions.attr("opacity", 0)
        .transition().duration(fade)
        .delay(function (f, i) { return (xs[i] - xMin) / xSpan * sweep; })
        .ease(d3.easeCubicOut)
        .attr("opacity", 1);
    }

    /* Hovering a region raises it (so its outline isn't buried under the
       neighbours' borders) and outlines it in primary ink; the tooltip
       names it and gives the exact value - or says "no data" plainly. */
    regions
      .on("pointerenter pointermove", function (event, f) {
        d3.select(this).raise()
          .attr("stroke", ctx.theme.ink.primary)
          .attr("stroke-width", 1.5)
          .attr("stroke-dasharray", null);
        var name = propOf(f, "name");
        var head = name === null ? "Region " + propOf(f, "id") : name;
        var v = valueOf(f);
        var body = v === undefined ? "no data" :
          pv.esc(ctx.x.vlab || "value") + ": <b>" + ctx.fmt(v) + "</b>";
        pv.showTip(ctx, event,
          "<b>" + pv.esc(head) + "</b><br>" + body);
      })
      .on("pointerleave", function () {
        d3.select(this).call(restStroke);
        pv.hideTip(ctx);
      });
  };

  /* ---------- bubble map ---------- */

  /* The largest "nice" number (1, 2, or 5 times a power of ten) not
     above v - the reference values of the circle-size legend, so the
     legend says "200k", never "421.3k". */
  function niceBelow(v) {
    var p = Math.pow(10, Math.floor(Math.log(v) / Math.LN10));
    var m = v / p;
    return (m >= 5 ? 5 : m >= 2 ? 2 : 1) * p;
  }

  pvRenderers.bubblemap = function (ctx) {
    var geo = rewind(ctx.x.map);
    var data = ctx.x.data;
    var hasSeries = data.length && data[0].series !== undefined;
    var seriesNames = hasSeries ?
      pv.uniq(data.map(function (d) { return d.series; })) : [];
    var color = d3.scaleOrdinal().domain(seriesNames)
      .range(ctx.theme.palette);
    /* "auto" shows the legend row exactly when a colour mapping exists;
       TRUE and FALSE override it - the scatter's rule. */
    var showLegend = opt(ctx.x.legend, hasSeries);
    if (showLegend && seriesNames.length) {
      pv.buildLegend(ctx.header, seriesNames, color, ctx.theme);
      ctx.height = Math.max(120, ctx.height - 26);
    }

    /* No axes - the margins are breathing room, as on the choropleth. */
    var m = { top: 8, right: 16, bottom: 10, left: 16 };
    var iw = Math.max(50, ctx.width - m.left - m.right),
        ih = Math.max(80, ctx.height - m.top - m.bottom);
    var svg = pv.baseSvg(ctx);
    var g = svg.append("g").attr("transform",
      "translate(" + m.left + "," + m.top + ")");

    var projection = d3.geoMercator().fitSize([iw, ih], geo);
    var path = d3.geoPath(projection);

    /* The base map is context, not data: a quiet neutral fill with
       hairline surface-coloured borders, the lakes over it, and no
       pointer events anywhere - hovers belong to the circles. */
    g.append("g").attr("pointer-events", "none")
      .selectAll("path.region").data(geo.features).enter()
      .append("path")
      .attr("class", "region")
      .attr("d", path)
      .attr("fill", ctx.theme.ink.grid)
      .attr("stroke", ctx.theme.ink.surface)
      .attr("stroke-width", 1)
      .attr("stroke-linejoin", "round");
    drawLakes(ctx, svg, g, path, iw, ih);

    /* Circle area encodes the value: a sqrt radius scale anchored at
       zero, with the top radius following the map's pixel size so the
       biggest bubble stays in proportion on small and large charts. */
    var vmax = d3.max(data, function (d) { return d.size; }) || 1;
    var rMax = Math.max(10, Math.min(30, 0.055 * Math.min(iw, ih)));
    var r = d3.scaleSqrt().domain([0, vmax]).range([0, rMax]);

    /* Crowded maps overlap - lighten every fill as the count grows so
       stacked circles keep reading as circles, never below 0.4. */
    var n = data.length;
    var fillOp = n <= 30 ? 0.7 : Math.max(0.4, 0.7 * Math.sqrt(30 / n));

    function fillOf(d) {
      return hasSeries ? color(d.series) : ctx.theme.palette[0];
    }

    /* Big circles first, so every smaller neighbour stays on top and
       hoverable; each point keeps its projected pixel position. */
    var pts = data.slice().sort(function (a, b) { return b.size - a.size; });
    pts.forEach(function (d) {
      var p = projection([d.lon, d.lat]);
      d.px = p[0];
      d.py = p[1];
    });

    var circles = g.append("g")
      .selectAll("circle.pt").data(pts).enter()
      .append("circle")
      .attr("class", "pt")
      .attr("cx", function (d) { return d.px; })
      .attr("cy", function (d) { return d.py; })
      .attr("r", function (d) { return r(d.size); })
      .attr("fill", fillOf)
      .attr("fill-opacity", fillOp)
      .attr("stroke", ctx.theme.ink.surface)
      .attr("stroke-width", 2);

    /* Entrance: circles grow from nothing, biggest first - skipped
       entirely at duration 0, where the final state must exist
       synchronously. */
    if (ctx.duration > 0) {
      circles.attr("r", 0)
        .transition().duration(Math.min(400, ctx.duration))
        .delay(function (d, i) { return Math.min(i * 8, 400); })
        .ease(d3.easeCubicOut)
        .attr("r", function (d) { return r(d.size); });
    }

    /* A circle's datum as a plain object, for the Shiny round-trip. */
    function ptDatum(d) {
      var out = { lon: d.lon, lat: d.lat, size: d.size };
      if (hasSeries) { out.series = d.series; }
      if (d.label !== undefined) { out.label = d.label; }
      return out;
    }

    /* Hovering a circle brings it to full strength with a primary-ink
       ring; the tooltip names the point and gives the exact value. */
    circles
      .on("pointerenter pointermove", function (event, d) {
        d3.select(this)
          .attr("fill-opacity", Math.min(1, fillOp + 0.25))
          .attr("stroke", ctx.theme.ink.primary);
        if (event.type === "pointerenter") {
          ctx.emit("hover", ptDatum(d));
        }
        var rows = [];
        if (d.label !== undefined) {
          rows.push("<b>" + pv.esc(d.label) + "</b>");
        }
        if (hasSeries) {
          rows.push(pv.swatchRow(color(d.series), "group",
            pv.esc(d.series)));
        }
        rows.push(pv.esc(ctx.x.sizelab || "size") + ": <b>" +
          ctx.fmt(d.size) + "</b>");
        pv.showTip(ctx, event, rows.join("<br>"));
      })
      .on("pointerleave", function () {
        d3.select(this)
          .attr("fill-opacity", fillOp)
          .attr("stroke", ctx.theme.ink.surface);
        pv.hideTip(ctx);
      })
      .on("click", function (event, d) {
        ctx.emit("click", ptDatum(d));
      });

    /* The size legend: nested reference circles in the bottom-right
       corner, bottom-aligned the way the circles themselves sit on the
       map, each with a leader line to its value. Two or three "nice"
       values cover the range; references too small to read are left
       out. */
    var refs = pv.uniq([vmax, vmax / 3, vmax / 10].map(niceBelow))
      .filter(function (v) { return r(v) >= 3; })
      .slice(0, 3);
    if (refs.length >= 2) {
      var rTop = r(refs[0]);
      var cxL = iw - rTop - 44;
      var byL = ih - 4;
      var leg = g.append("g").attr("pointer-events", "none");
      refs.forEach(function (v) {
        leg.append("circle")
          .attr("cx", cxL).attr("cy", byL - r(v)).attr("r", r(v))
          .attr("fill", "none")
          .attr("stroke", ctx.theme.ink.muted)
          .attr("stroke-width", 1);
        leg.append("line")
          .attr("x1", cxL).attr("x2", cxL + rTop + 5)
          .attr("y1", byL - 2 * r(v)).attr("y2", byL - 2 * r(v))
          .attr("stroke", ctx.theme.ink.muted)
          .attr("stroke-width", 0.75)
          .attr("stroke-dasharray", "2,2");
        leg.append("text")
          .attr("x", cxL + rTop + 8).attr("y", byL - 2 * r(v))
          .attr("dominant-baseline", "middle")
          .attr("fill", ctx.theme.ink.muted)
          .style("font-size", "10.5px")
          .style("font-variant-numeric", "tabular-nums")
          .text(legFmt(v));
      });
    }
  };

  /* ---------- flow map ---------- */

  /* A point and the tangent of the quadratic bezier p1 -> c -> p2 at t,
     for sampling the flow bands below. */
  function qPoint(p1, c, p2, t) {
    var u = 1 - t;
    return [
      u * u * p1[0] + 2 * u * t * c[0] + t * t * p2[0],
      u * u * p1[1] + 2 * u * t * c[1] + t * t * p2[1]
    ];
  }
  function qTangent(p1, c, p2, t) {
    return [
      2 * (1 - t) * (c[0] - p1[0]) + 2 * t * (p2[0] - c[0]),
      2 * (1 - t) * (c[1] - p1[1]) + 2 * t * (p2[1] - c[1])
    ];
  }

  /* The control point bowing a flow's arc: the chord midpoint pushed to
     the right of the travel direction. Every arc bows to the same side
     of its own direction (clockwise on screen), so a pair of opposite
     flows parts to the two sides of their shared chord instead of
     overprinting - the offset is the direction itself. */
  function flowControl(p1, p2) {
    var dx = p2[0] - p1[0], dy = p2[1] - p1[1];
    var len = Math.sqrt(dx * dx + dy * dy) || 1;
    var bow = 0.16 * len;
    return [(p1[0] + p2[0]) / 2 - dy / len * bow,
            (p1[1] + p2[1]) / 2 + dx / len * bow];
  }

  /* The filled band around a flow's centerline: the curve sampled into
     short segments, each pushed apart by half the local width. The width
     tapers from full at the origin to a narrow nose at the destination -
     the thin end points the way, quieter than an arrowhead. */
  function flowBand(p1, c, p2, w) {
    var S = 28, left = [], right = [];
    for (var i = 0; i <= S; i++) {
      var t = i / S;
      var pt = qPoint(p1, c, p2, t);
      var tg = qTangent(p1, c, p2, t);
      var len = Math.sqrt(tg[0] * tg[0] + tg[1] * tg[1]) || 1;
      var nx = -tg[1] / len, ny = tg[0] / len;
      var h = (w / 2) * (1 - 0.8 * t);
      left.push((pt[0] + nx * h) + "," + (pt[1] + ny * h));
      right.push((pt[0] - nx * h) + "," + (pt[1] - ny * h));
    }
    return "M" + left.join("L") + "L" + right.reverse().join("L") + "Z";
  }

  pvRenderers.flowmap = function (ctx) {
    var geo = rewind(ctx.x.map);
    var flows = ctx.x.data;
    var places = ctx.x.places || [];

    /* No axes - the margins are breathing room, as on the map siblings. */
    var m = { top: 8, right: 16, bottom: 10, left: 16 };
    var iw = Math.max(50, ctx.width - m.left - m.right),
        ih = Math.max(80, ctx.height - m.top - m.bottom);
    var svg = pv.baseSvg(ctx);
    var g = svg.append("g").attr("transform",
      "translate(" + m.left + "," + m.top + ")");

    var projection = d3.geoMercator().fitSize([iw, ih], geo);
    var path = d3.geoPath(projection);

    /* The base map is context, not data - the bubble map's quiet
       treatment: neutral fills, hairline borders, lakes over them, no
       pointer events anywhere. */
    g.append("g").attr("pointer-events", "none")
      .selectAll("path.region").data(geo.features).enter()
      .append("path")
      .attr("class", "region")
      .attr("d", path)
      .attr("fill", ctx.theme.ink.grid)
      .attr("stroke", ctx.theme.ink.surface)
      .attr("stroke-width", 1)
      .attr("stroke-linejoin", "round");
    drawLakes(ctx, svg, g, path, iw, ih);

    /* Each place's pixel position, keyed by name, shared by the bands,
       the dots, and the labels. */
    var pos = {};
    places.forEach(function (p) {
      var xy = projection([p.lon, p.lat]);
      pos[p.place] = { x: xy[0], y: xy[1] };
    });

    /* Band width follows sqrt(value) - the same honesty as circle
       areas - clamped so the fattest flow stays a band rather than a
       wedge, and the thinnest stays visible. */
    var vmax = d3.max(flows, function (d) { return d.value; }) || 1;
    var wMax = Math.max(6, Math.min(14, 0.03 * Math.min(iw, ih)));
    var wSc = d3.scaleSqrt().domain([0, vmax]).range([0, wMax]);
    function widthOf(v) { return Math.max(1.5, wSc(v)); }

    /* Crowded maps stack translucent bands - lighten every fill as the
       count grows so crossings keep reading, never below 0.3. */
    var n = flows.length;
    var flowOp = n <= 12 ? 0.62 : Math.max(0.3, 0.62 * Math.sqrt(12 / n));
    var accent = ctx.theme.palette[0];

    /* Big flows first, so smaller ones paint on top and their hover
       twins are reachable. */
    var fl = flows.slice().sort(function (a, b) {
      return b.value - a.value;
    });

    var flowSel = g.append("g")
      .selectAll("g.flow").data(fl).enter()
      .append("g").attr("class", "flow");

    var bands = flowSel.append("path")
      .attr("class", "band")
      .attr("pointer-events", "none")
      .attr("d", function (d) {
        var a = pos[d.from], b = pos[d.to];
        var p1 = [a.x, a.y], p2 = [b.x, b.y];
        return flowBand(p1, flowControl(p1, p2), p2, widthOf(d.value));
      })
      .attr("fill", accent)
      .attr("fill-opacity", flowOp)
      .attr("stroke", ctx.theme.ink.surface)
      .attr("stroke-width", 0.75)
      .attr("stroke-linejoin", "round");

    /* A thin band is a mean hover target, so each flow also carries an
       invisible fat twin along its centerline - the comparison charts'
       trick. Hovering lights that one flow and dims the rest. */
    flowSel.append("path")
      .attr("class", "hover")
      .attr("d", function (d) {
        var a = pos[d.from], b = pos[d.to];
        var c = flowControl([a.x, a.y], [b.x, b.y]);
        return "M" + a.x + "," + a.y + "Q" + c[0] + "," + c[1] +
          " " + b.x + "," + b.y;
      })
      .attr("fill", "none")
      .attr("stroke", "transparent")
      .attr("stroke-width", function (d) {
        return Math.max(14, widthOf(d.value) + 8);
      })
      .style("cursor", "pointer")
      .on("pointerenter pointermove", function (event, d) {
        d3.select(this.parentNode).raise();
        bands.attr("fill-opacity", function (e) {
          return e === d ? Math.min(1, flowOp + 0.35) : 0.08;
        });
        if (event.type === "pointerenter") {
          ctx.emit("hover", { from: d.from, to: d.to, value: d.value });
        }
        pv.showTip(ctx, event,
          "<b>" + pv.esc(d.from) + " → " + pv.esc(d.to) + "</b><br>" +
          pv.esc(ctx.x.vlab || "value") + ": <b>" + ctx.fmt(d.value) +
          "</b>");
      })
      .on("pointerleave", function () {
        bands.attr("fill-opacity", flowOp);
        pv.hideTip(ctx);
      })
      .on("click", function (event, d) {
        ctx.emit("click", { from: d.from, to: d.to, value: d.value });
      });

    /* Endpoint dots, sized by total throughput (in plus out), wearing
       the usual 2px surface ring. They sit above the bands but take no
       pointer events - the hovers belong to the flows converging on
       them. */
    var tmax = d3.max(places, function (p) { return p.total; }) || 1;
    var rMax = Math.max(4, Math.min(9, 0.02 * Math.min(iw, ih)));
    var rSc = d3.scaleSqrt().domain([0, tmax]).range([0, rMax]);
    function rOf(p) { return Math.max(2.5, rSc(p.total)); }

    var dots = g.append("g").attr("pointer-events", "none")
      .selectAll("circle.place").data(places).enter()
      .append("circle")
      .attr("class", "place")
      .attr("cx", function (p) { return pos[p.place].x; })
      .attr("cy", function (p) { return pos[p.place].y; })
      .attr("r", rOf)
      .attr("fill", accent)
      .attr("stroke", ctx.theme.ink.surface)
      .attr("stroke-width", 2);

    /* Direct labels beside the dots for the places the R side flagged.
       A surface-coloured halo keeps them readable over bands and
       borders, and labels near the right edge flip to the other side of
       their dot. */
    var labels = g.append("g").attr("pointer-events", "none")
      .selectAll("text.place").data(places.filter(function (p) {
        return p.labelled;
      })).enter()
      .append("text")
      .attr("class", "place")
      .attr("x", function (p) {
        var flip = pos[p.place].x > iw - 80;
        return pos[p.place].x + (flip ? -1 : 1) * (rOf(p) + 5);
      })
      .attr("y", function (p) { return pos[p.place].y; })
      .attr("text-anchor", function (p) {
        return pos[p.place].x > iw - 80 ? "end" : "start";
      })
      .attr("dominant-baseline", "middle")
      .attr("fill", ctx.theme.ink.secondary)
      .attr("stroke", ctx.theme.ink.surface)
      .attr("stroke-width", 3)
      .attr("paint-order", "stroke")
      .attr("stroke-linejoin", "round")
      .style("font-size", "11px")
      .text(function (p) { return p.place; });

    /* Entrance: bands fade in biggest first, dots grow, labels follow -
       skipped entirely at duration 0, where the final state must exist
       synchronously. */
    if (ctx.duration > 0) {
      var fade = Math.min(300, ctx.duration);
      bands.attr("opacity", 0)
        .transition().duration(fade)
        .delay(function (d, i) { return Math.min(i * 25, 250); })
        .ease(d3.easeCubicOut)
        .attr("opacity", 1);
      dots.attr("r", 0)
        .transition().duration(Math.min(300, ctx.duration))
        .ease(d3.easeCubicOut)
        .attr("r", rOf);
      labels.attr("opacity", 0)
        .transition().delay(fade / 2).duration(fade)
        .attr("opacity", 1);
    }
  };

})();
