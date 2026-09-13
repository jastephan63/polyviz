/*
 * Geographic renderers: the choropleth, the hex cartogram, the bubble
 * map, the flow map, and the isochrone map. See basic.js for the ctx
 * contract. Projection and path drawing come from the d3-geo functions
 * bundled with d3 v7; the map itself travels in the payload as a
 * GeoJSON FeatureCollection (the hex cartogram instead carries its
 * hand-curated grid of axial coordinates, and the isochrone adds a
 * flat travel-time raster that d3-contour turns into band polygons).
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

  /* The colour of a region's value, shared by the choropleth and the
     hex cartogram. Sequential glides through the theme's ramp over the
     data range; diverging pins its neutral midpoint to the reference
     value (the R side made the domain symmetric around it, so the
     poles carry equal weight). */
  function regionColour(theme, palette, domain, center) {
    if (palette === "diverging") {
      var dv = theme.diverging;
      return d3.scaleDiverging(
        d3.piecewise(d3.interpolateRgb, [dv.low, dv.mid, dv.high]))
        .domain([domain[0], center, domain[1]]).clamp(true);
    }
    var ramp = d3.interpolateRgbBasis(theme.sequential);
    var t = d3.scaleLinear().domain(domain).range([0, 1]).clamp(true);
    return function (v) { return ramp(t(v)); };
  }

  /* The region charts' legend is their colour scale: a small gradient
     bar in the header with the domain ends labelled - and, for
     diverging, a tick marking where the reference value sits, since
     that midpoint is what the whole palette pivots on. The legend shows
     the slice of the scale the data actually spans (ctx.x.obs) - a
     symmetric diverging domain can reach far beyond the observed
     values, and labelling those phantom endpoints would put numbers on
     the legend that exist nowhere on the map. Steals its own height
     from ctx.height, like every header row. The optional unit string
     rides on the high label ("120 min") so a scale in real-world units
     can say so - the region charts pass none and read as before. */
  function scaleLegend(ctx, colorOf, diverging, center, unit) {
    var scaleRow = document.createElement("div");
    scaleRow.style.cssText =
      "display:flex;align-items:flex-start;gap:7px;margin-top:7px;" +
      "font-size:11px;line-height:12px;font-variant-numeric:tabular-nums;" +
      "color:" + ctx.theme.ink.muted + ";";
    var leg = ctx.x.obs || ctx.x.domain;
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
    hi.textContent = legFmt(leg[1]) + (unit || "");
    scaleRow.appendChild(lo);
    scaleRow.appendChild(barWrap);
    scaleRow.appendChild(hi);
    ctx.header.appendChild(scaleRow);
    ctx.height = Math.max(120, ctx.height - scaleRow.offsetHeight - 7);
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

    /* The shared region-colour scale, and the gradient-bar legend it
       feeds - both live above, since the hex cartogram wears the same
       pair. */
    var colorOf = regionColour(ctx.theme, ctx.x.palette, domain, center);
    scaleLegend(ctx, colorOf, diverging, center);

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

  /* ---------- hex cartogram ---------- */

  /* The corner points of one pointy-top hexagon, as an SVG polygon
     points string: six corners at 60-degree steps starting from the
     top-right, so a flat edge faces east and west and a point faces
     north - the orientation whose rows read left to right, the way
     Switzerland is wide. */
  function hexPoints(cx, cy, rad) {
    var pts = [];
    for (var k = 0; k < 6; k++) {
      var a = Math.PI * (60 * k - 30) / 180;
      pts.push((cx + rad * Math.cos(a)) + "," + (cy + rad * Math.sin(a)));
    }
    return pts.join(" ");
  }

  pvRenderers.hexmap = function (ctx) {
    var layout = ctx.x.layout;
    var domain = ctx.x.domain;
    var diverging = ctx.x.palette === "diverging";
    var center = typeof ctx.x.center === "number" ? ctx.x.center : null;

    /* The join table: canton code -> value. The R side already resolved
       every id to its two-letter code, so the join here is exact. */
    var valueByCode = {};
    ctx.x.data.forEach(function (d) { valueByCode[d.code] = d.value; });

    /* The choropleth's colour scale and gradient-bar legend, shared. */
    var colorOf = regionColour(ctx.theme, ctx.x.palette, domain, center);
    scaleLegend(ctx, colorOf, diverging, center);

    /* No axes - the margins are breathing room, as on the map siblings. */
    var m = { top: 8, right: 16, bottom: 10, left: 16 };
    var iw = Math.max(50, ctx.width - m.left - m.right),
        ih = Math.max(80, ctx.height - m.top - m.bottom);
    var svg = pv.baseSvg(ctx);
    var g = svg.append("g").attr("transform",
      "translate(" + m.left + "," + m.top + ")");

    /* Axial coordinates to unit centres (hex radius 1): a cell's x
       follows q + r/2, rows sit 1.5 radii apart. The grid then scales
       to whatever fits the plot box and centres itself, so the hexagons
       size themselves from the container - a resize re-renders and
       re-fits, like every chart. */
    var SQ3 = Math.sqrt(3);
    layout.forEach(function (c) {
      c.ux = SQ3 * (c.q + c.r / 2);
      c.uy = 1.5 * c.r;
    });
    var xmin = d3.min(layout, function (c) { return c.ux; });
    var ymin = d3.min(layout, function (c) { return c.uy; });
    var xspan = d3.max(layout, function (c) { return c.ux; }) - xmin;
    var yspan = d3.max(layout, function (c) { return c.uy; }) - ymin;
    /* One hex width (sqrt(3) units) and one hex height (2 units) of
       padding turn centre spans into edge-to-edge extents. */
    var s = Math.min(iw / (xspan + SQ3), ih / (yspan + 2));
    var ox = (iw - s * (xspan + SQ3)) / 2 + s * (SQ3 / 2 - xmin);
    var oy = (ih - s * (yspan + 2)) / 2 + s * (1 - ymin);
    layout.forEach(function (c) {
      c.px = ox + s * c.ux;
      c.py = oy + s * c.uy;
    });

    /* Each cell's resting stroke - the choropleth's rule: hairline
       surface-coloured seams between coloured hexagons, a quiet dashed
       outline in the muted grey on cantons without data, so absence
       stays visible rather than passing itself off as a low value. */
    function restStroke(sel) {
      sel.attr("stroke", function (c) {
        return valueByCode[c.code] === undefined ?
          ctx.theme.ink.muted : ctx.theme.ink.surface;
      })
      .attr("stroke-width", 1)
      .attr("stroke-dasharray", function (c) {
        return valueByCode[c.code] === undefined ? "3,2" : null;
      });
    }

    var hexes = g.append("g")
      .selectAll("polygon.hex").data(layout).enter()
      .append("polygon")
      .attr("class", "hex")
      .attr("points", function (c) { return hexPoints(c.px, c.py, s); })
      .attr("fill", function (c) {
        var v = valueByCode[c.code];
        return v === undefined ? ctx.theme.ink.grid : colorOf(v);
      })
      .attr("stroke-linejoin", "round")
      .call(restStroke);

    /* The two-letter codes, one per hexagon, unless the R side turned
       them off. Each label wears whichever theme ink sits further from
       its hexagon's fill in lightness, so codes stay readable on the
       deep end of the ramp in light mode and on the pale end in dark
       mode alike. Labels take no pointer events - hovers belong to the
       hexagons under them. */
    var labels = null;
    if (ctx.x.labels !== false) {
      var inkA = ctx.theme.ink.primary;
      var inkB = ctx.theme.ink.surface;
      var la = d3.lab(inkA).l;
      var lb = d3.lab(inkB).l;
      labels = g.append("g").attr("pointer-events", "none")
        .selectAll("text.hex").data(layout).enter()
        .append("text")
        .attr("class", "hex")
        .attr("x", function (c) { return c.px; })
        .attr("y", function (c) { return c.py; })
        .attr("text-anchor", "middle")
        .attr("dominant-baseline", "central")
        .attr("fill", function (c) {
          var v = valueByCode[c.code];
          if (v === undefined) return ctx.theme.ink.muted;
          var lf = d3.lab(colorOf(v)).l;
          return Math.abs(la - lf) >= Math.abs(lb - lf) ? inkA : inkB;
        })
        .style("font-size",
          Math.max(9, Math.min(14, 0.55 * s)) + "px")
        .style("font-weight", 600)
        .style("letter-spacing", "0.02em")
        .text(function (c) { return c.code; });
    }

    /* Entrance: hexagons fade in swept west to east across the grid -
       the choropleth's sweep - and the codes follow. Skipped entirely
       at duration 0, where the final state must exist synchronously. */
    if (ctx.duration > 0) {
      var pxs = layout.map(function (c) { return c.px; });
      var pxMin = d3.min(pxs);
      var pxSpan = Math.max(1, d3.max(pxs) - pxMin);
      var fade = Math.min(200, ctx.duration);
      var sweep = Math.min(400, Math.max(0, ctx.duration - fade));
      var delayOf = function (c) {
        return (c.px - pxMin) / pxSpan * sweep;
      };
      hexes.attr("opacity", 0)
        .transition().duration(fade).delay(delayOf)
        .ease(d3.easeCubicOut)
        .attr("opacity", 1);
      if (labels) {
        labels.attr("opacity", 0)
          .transition().duration(fade).delay(delayOf)
          .ease(d3.easeCubicOut)
          .attr("opacity", 1);
      }
    }

    /* Hovering a hexagon raises it (so its outline isn't buried under
       the neighbours' seams) and outlines it in primary ink; the
       tooltip gives the full canton name with the code, and the exact
       value - or says "no data" plainly. The labels live in a later
       group, so a raised hexagon never covers its own code. */
    hexes
      .style("cursor", "pointer")
      .on("pointerenter pointermove", function (event, c) {
        d3.select(this).raise()
          .attr("stroke", ctx.theme.ink.primary)
          .attr("stroke-width", 1.5)
          .attr("stroke-dasharray", null);
        var v = valueByCode[c.code];
        var body = v === undefined ? "no data" :
          pv.esc(ctx.x.vlab || "value") + ": <b>" + ctx.fmt(v) + "</b>";
        if (event.type === "pointerenter") {
          ctx.emit("hover", {
            code: c.code, name: c.name,
            value: v === undefined ? null : v
          });
        }
        pv.showTip(ctx, event,
          "<b>" + pv.esc(c.name) + "</b> (" + pv.esc(c.code) + ")<br>" +
          body);
      })
      .on("pointerleave", function () {
        d3.select(this).call(restStroke);
        pv.hideTip(ctx);
      })
      .on("click", function (event, c) {
        var v = valueByCode[c.code];
        ctx.emit("click", {
          code: c.code, name: c.name,
          value: v === undefined ? null : v
        });
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

  /* ---------- isochrone map ---------- */

  pvRenderers.isochrone = function (ctx) {
    var geo = rewind(ctx.x.map);
    /* htmlwidgets auto-unboxes length-1 vectors, so a single break
       arrives as a bare number; concat makes both shapes an array. */
    var breaks = [].concat(ctx.x.breaks || []);
    var nx = ctx.x.nx, ny = ctx.x.ny, bb = ctx.x.bbox;
    /* The raster, with the R side's NA cells (beyond the walk cutoff)
       pushed far below every threshold. NaN would be the natural
       sentinel, but marching squares interpolates its band edges from
       neighbouring values, and a finite floor keeps that arithmetic
       defined right up to the last reached cell. */
    var FLOOR = -1e9;
    var values = (ctx.x.grid || []).map(function (v) {
      return typeof v === "number" ? v : FLOOR;
    });

    /* One colour per band, stepped off the theme's sequential ramp:
       band 0 (near the origin) sits at the pale end, the open-ended
       band past the last break at the deep end - dark mode's ramp runs
       the other way, so "near recedes toward the surface" holds there
       too. */
    var ramp = d3.interpolateRgbBasis(ctx.theme.sequential);
    var nBands = breaks.length + 1;
    var bandColour = [];
    for (var bi = 0; bi < nBands; bi++) {
      bandColour.push(ramp(nBands > 1 ? bi / (nBands - 1) : 0.6));
    }

    /* A band's spoken range. Integer minute edges read as timetable
       ranges - the band above 30 starts at 31 - while fractional edges
       keep their exact value; the band past the last break is open. */
    function bandLabel(i) {
      if (i >= breaks.length) {
        return "over " + legFmt(breaks[breaks.length - 1]) + " min";
      }
      var hi = breaks[i];
      if (i === 0) return "0–" + legFmt(hi) + " min";
      var lo = breaks[i - 1];
      var loShown = (lo % 1 === 0 && hi % 1 === 0) ? lo + 1 : lo;
      return legFmt(loShown) + "–" + legFmt(hi) + " min";
    }

    /* The header legend is the choropleth's gradient bar wearing the
       stepped band scale over 0 to the last break, its high end
       labelled in minutes. The open-ended band only exists on the map
       (and in its tooltip) - stretching the bar past the last break
       would put a number on the legend that means nothing. */
    function colorOfMinutes(v) {
      var i = 0;
      while (i < breaks.length && v > breaks[i]) i++;
      return bandColour[Math.min(i, nBands - 1)];
    }
    ctx.x.obs = [0, breaks[breaks.length - 1]];
    scaleLegend(ctx, colorOfMinutes, false, null, " min");

    /* No axes - the margins are breathing room, as on the map siblings. */
    var m = { top: 8, right: 16, bottom: 10, left: 16 };
    var iw = Math.max(50, ctx.width - m.left - m.right),
        ih = Math.max(80, ctx.height - m.top - m.bottom);
    var svg = pv.baseSvg(ctx);
    var g = svg.append("g").attr("transform",
      "translate(" + m.left + "," + m.top + ")");

    var projection = d3.geoMercator().fitSize([iw, ih], geo);
    var path = d3.geoPath(projection);

    /* The base country wears the plain chart surface: unreached ground
       is an absence, not a value, so it looks like the page rather
       than like data. The country's shape comes from the bands, the
       lakes, and the canton hairlines drawn further down. */
    g.append("g").attr("pointer-events", "none")
      .selectAll("path.region").data(geo.features).enter()
      .append("path")
      .attr("class", "region")
      .attr("d", path)
      .attr("fill", ctx.theme.ink.surface)
      .attr("stroke", "none");

    /* d3-contour works in grid coordinates, where the value at index
       i + j*nx sits at (i + 0.5, j + 0.5). This planar transform walks
       each band vertex back to lon/lat (row 0 is the raster's northern
       edge) and through the map's own projection - no spherical
       resampling, no winding-order questions. */
    var lonSpan = bb[1] - bb[0], latSpan = bb[3] - bb[2];
    var gridPath = d3.geoPath(d3.geoTransform({
      point: function (gx, gy) {
        var p = projection([bb[0] + gx / nx * lonSpan,
                            bb[3] - gy / ny * latSpan]);
        this.stream.point(p[0], p[1]);
      }
    }));

    /* The filled bands: one polygon per threshold, each the region at
       or beyond that many minutes, painted near-to-far so every darker
       band sits on the lighter ones - the value under the cursor is
       always the topmost paint, which makes hover exact. The raster
       reaches past the border (a station near Basel serves German
       soil), so the whole stack is clipped to the country: the union
       of the canton shapes, which is what a clipPath's children form. */
    var clipId = "pv-iso-clip-" + Math.floor(Math.random() * 1e9);
    var clip = svg.append("clipPath").attr("id", clipId);
    geo.features.forEach(function (f) {
      clip.append("path").attr("d", path(f));
    });

    var gen = d3.contours().size([nx, ny]);
    var bandData = [];
    for (var ti = 0; ti < nBands; ti++) {
      bandData.push({
        i: ti,
        label: bandLabel(ti),
        geom: gen.contour(values, ti === 0 ? 0 : breaks[ti - 1])
      });
    }
    /* A band no cell reaches (nothing beyond the last break, say) has
       no geometry; binding it would only put empty <path>s in the way
       of the keyboard walk - rewind's rule, applied to bands. */
    bandData = bandData.filter(function (d) {
      return d.geom.coordinates.length > 0;
    });

    var bands = g.append("g")
      .attr("clip-path", "url(#" + clipId + ")")
      .selectAll("path.band").data(bandData).enter()
      .append("path")
      .attr("class", "band")
      .attr("d", function (d) { return gridPath(d.geom); })
      .attr("fill", function (d) { return bandColour[d.i]; })
      .attr("stroke", ctx.theme.ink.surface)
      .attr("stroke-width", 0.75)
      .attr("stroke-linejoin", "round");

    /* The Swiss lakes over the bands - the ThemaKart convention every
       country-wide map here follows - then the canton borders as
       hairlines over the water, so the political geography stays
       legible across band fills and lake alike. */
    drawLakes(ctx, svg, g, path, iw, ih);
    g.append("g").attr("pointer-events", "none")
      .selectAll("path.border").data(geo.features).enter()
      .append("path")
      .attr("class", "border")
      .attr("d", path)
      .attr("fill", "none")
      .attr("stroke", ctx.theme.ink.baseline)
      .attr("stroke-width", 0.5)
      .attr("stroke-linejoin", "round");

    /* The origin as a ringed marker in primary ink - readable on the
       pale near band in light mode and the dark near band in dark
       mode, where the accent blue would sink into the ramp. */
    if (ctx.x.origin && typeof ctx.x.origin.lon === "number") {
      var op = projection([ctx.x.origin.lon, ctx.x.origin.lat]);
      var om = g.append("g").attr("pointer-events", "none");
      om.append("circle")
        .attr("cx", op[0]).attr("cy", op[1]).attr("r", 8)
        .attr("fill", "none")
        .attr("stroke", ctx.theme.ink.primary)
        .attr("stroke-width", 1.5);
      om.append("circle")
        .attr("cx", op[0]).attr("cy", op[1]).attr("r", 3.5)
        .attr("fill", ctx.theme.ink.primary)
        .attr("stroke", ctx.theme.ink.surface)
        .attr("stroke-width", 2);
    }

    /* Entrance: bands fade in near-to-far, the journey playing outward
       from the origin. Skipped entirely at duration 0, where the final
       state must exist synchronously. */
    if (ctx.duration > 0) {
      var fade = Math.min(200, ctx.duration);
      var sweep = Math.min(400, Math.max(0, ctx.duration - fade));
      bands.attr("opacity", 0)
        .transition().duration(fade)
        .delay(function (d) {
          return d.i / Math.max(1, nBands - 1) * sweep;
        })
        .ease(d3.easeCubicOut)
        .attr("opacity", 1);
    }

    /* Hovering a band outlines its rim in primary ink (never raised -
       lifting the near band would bury every farther one) and the
       tooltip reads the band's range; unreached ground has no band to
       hover and stays silent. */
    bands
      .on("pointerenter pointermove", function (event, d) {
        d3.select(this)
          .attr("stroke", ctx.theme.ink.primary)
          .attr("stroke-width", 1.25);
        if (event.type === "pointerenter") {
          ctx.emit("hover", {
            band: d.i, label: d.label,
            from: d.i === 0 ? 0 : breaks[d.i - 1],
            to: d.i >= breaks.length ? null : breaks[d.i]
          });
        }
        pv.showTip(ctx, event,
          "<b>" + pv.esc(d.label) + "</b><br>travel time (" +
          pv.esc(ctx.x.vlab || "minutes") + ")");
      })
      .on("pointerleave", function () {
        d3.select(this)
          .attr("stroke", ctx.theme.ink.surface)
          .attr("stroke-width", 0.75);
        pv.hideTip(ctx);
      })
      .on("click", function (event, d) {
        ctx.emit("click", {
          band: d.i, label: d.label,
          from: d.i === 0 ? 0 : breaks[d.i - 1],
          to: d.i >= breaks.length ? null : breaks[d.i]
        });
      });
  };

})();
