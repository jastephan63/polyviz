/*
 * Geographic renderers: the choropleth map.
 * See basic.js for the ctx contract. Projection and path drawing come
 * from the d3-geo functions bundled with d3 v7; the map itself travels
 * in the payload as a GeoJSON FeatureCollection.
 */
(function () {

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

    /* A feature's id or name, if it has a usable one. Background
       features (the lakes in the bundled Lucerne map) carry empty
       properties, which jsonlite serialises as {} - so only plain
       strings and numbers count as present. */
    function propOf(f, key) {
      var v = f.properties && f.properties[key];
      return typeof v === "string" || typeof v === "number" ? v : null;
    }
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

    /* Compact numbers for the legend ends: three significant digits with
       an SI suffix past 10k ("23.3M"), at most two decimals below - the
       legend rounds, the tooltips carry the precision. */
    var legFmt = function (v) {
      return Math.abs(v) >= 10000 ?
        d3.format(".3~s")(v) : d3.format(",.2~f")(v);
    };

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

    var regions = g.selectAll("path.region").data(geo.features).enter()
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

})();
