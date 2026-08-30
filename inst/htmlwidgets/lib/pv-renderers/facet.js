/*
 * Small-multiples renderer. The R side (pv_facet) rewrites a bar, line,
 * scatter, or area payload into type "facet": the rows live in x.panels
 * as {name, data} pieces and every other field stays once at the top
 * level. This file only lays out the grid - each panel is drawn by the
 * original chart's own renderer, handed a cloned, panel-sized ctx.
 */
(function () {

  /* The payload for one panel: every top-level option, plus that panel's
     own rows. The heading is drawn once for the whole grid and the panel
     name plays the title's role inside a panel, so title, subtitle, and
     legend are cleared on the clone. Shared xlim/ylim ride along, which
     is what keeps the panels' axes identical. */
  function panelX(x, panel) {
    var out = {}, k;
    for (k in x) {
      if (Object.prototype.hasOwnProperty.call(x, k)) out[k] = x[k];
    }
    out.type = x.subtype;
    out.data = panel.data;
    out.panels = null;
    out.title = null;
    out.subtitle = null;
    out.legend = false;
    out.showLegend = false;
    /* Area payloads carry the full series list, and the area renderer
       zero-fills any series missing from the data. Inside a panel that
       would draw flat bands for every other panel's series, so the list
       shrinks to the series this panel actually holds. */
    if (out.type === "area" && out.series != null) {
      out.series = pv.uniq(panel.data.map(function (d) {
        return d.series;
      }));
    }
    return out;
  }

  pvRenderers.facet = function (ctx) {
    var x = ctx.x;
    var panels = x.panels || [];
    var n = panels.length;
    var sub = pvRenderers[x.subtype];
    if (!n) return;
    if (!sub) {
      ctx.el.appendChild(document.createTextNode(
        "polyviz: unknown facet subtype '" + x.subtype + "'"));
      return;
    }

    /* Column count: what R asked for, otherwise a near-square grid.
       Never more than 4 columns - 2 on narrow containers, and a single
       scrolling column on phone-narrow ones - so every panel keeps
       enough width for its own axes and tick labels. */
    var maxCols = ctx.width < 440 ? 1 : ctx.width < 700 ? 2 : 4;
    var ncol = +x.ncol > 0 ? Math.round(+x.ncol) : Math.ceil(Math.sqrt(n));
    ncol = Math.max(1, Math.min(ncol, maxCols, n));
    var nrow = Math.ceil(n / ncol);

    var pw = Math.floor(ctx.width / ncol);
    var nameH = 20;
    /* Panels shorter than ~160px squeeze their five y ticks into
       unreadable mush, so hold a floor and let the grid scroll
       vertically instead when the rows no longer fit - a scrolled panel
       stays legible, a shrunken one does not. The scroll also keeps the
       grid from drawing over the source line below it. */
    var ph = Math.max(nameH + 140, Math.floor(ctx.height / nrow));

    var grid = document.createElement("div");
    grid.style.cssText = "position:relative;width:" + ctx.width +
      "px;height:" + ctx.height + "px;";
    if (nrow * ph > ctx.height) {
      grid.style.overflowY = "auto";
      grid.style.overflowX = "hidden";
    }
    ctx.el.appendChild(grid);

    panels.forEach(function (panel, i) {
      var col = i % ncol, row = Math.floor(i / ncol);
      var cell = document.createElement("div");
      cell.style.cssText = "position:absolute;left:" + (col * pw) +
        "px;top:" + (row * ph) + "px;width:" + pw + "px;height:" + ph +
        "px;";
      grid.appendChild(cell);

      /* The panel's name sits in its own strip above the drawing, a few
         pixels in from the edge, so it can never collide with the chart.
         Long names truncate; the strip's title attribute keeps the full
         text a hover away. */
      var name = document.createElement("div");
      name.textContent = pv.truncate(panel.name,
        Math.max(4, Math.floor((pw - 16) / 7)));
      name.title = panel.name;
      name.style.cssText = "height:" + nameH + "px;box-sizing:border-box;" +
        "padding:4px 8px 0 8px;overflow:hidden;white-space:nowrap;" +
        "font-size:12px;font-weight:700;color:" +
        ctx.theme.ink.secondary + ";";
      cell.appendChild(name);

      var body = document.createElement("div");
      body.style.cssText = "position:relative;width:" + pw + "px;height:" +
        (ph - nameH) + "px;";
      cell.appendChild(body);

      /* One tooltip serves the whole grid. It positions itself inside
         the element the renderer knows as ctx.el, so adopt it into
         whichever panel the pointer enters - entering any mark enters
         its panel first, so the tip is always in place before it shows. */
      body.addEventListener("pointerenter", function () {
        if (ctx.tip.parentNode !== body) body.appendChild(ctx.tip);
      });

      /* The sub-ctx mirrors what pvchart.js builds for a whole chart,
         scaled down to this panel. The header is a detached element:
         anything a renderer would append there (a stray legend) simply
         never reaches the page. Tooltip, duration, Shiny emit, and
         crosstalk selection state are shared with the parent. */
      sub({
        el: body,
        x: panelX(x, panel),
        theme: ctx.theme,
        tip: ctx.tip,
        header: document.createElement("div"),
        width: pw,
        height: ph - nameH,
        duration: ctx.duration,
        fmt: ctx.fmt,
        emit: ctx.emit,
        selected: ctx.selected,
        select: ctx.select
      });
    });
  };

})();
