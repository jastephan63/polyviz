/*
 * This file is the JavaScript entry point of polyviz. You never call it
 * directly: the R functions (pv_bar, pv_line, ...) bundle up your data
 * plus a "type" label, htmlwidgets ships it over, and the code below hands
 * it to the matching renderer.
 *
 * The pieces:
 *   - lib/pv-common/pv-common.js   shared chrome (header, legend, tooltip,
 *                                  axis styling) on the `pv` global
 *   - lib/pv-renderers/*.js        one file per chart family; each file
 *                                  registers its charts on `pvRenderers`
 *   - this file                    widget wiring: resolves the light/dark
 *                                  theme, builds the ctx object, dispatches,
 *                                  re-renders on resize and on theme change
 */

/* A headless browser is a screenshot machine - knitr converting a
   document, pv_save() capturing a file, a test suite. It gets the
   finished chart straight away, with no entry animation for a capture
   to race, and "auto" mode resolves to light, the printed page's
   default. An explicit mode or duration from R still wins. */
var pvHeadless = typeof navigator !== "undefined" &&
  /HeadlessChrome/.test(navigator.userAgent || "");

HTMLWidgets.widget({
  name: "pvchart",
  type: "output",

  factory: function (el, width, height) {
    var lastX = null;
    var lastW = 0, lastH = 0;
    var mq = window.matchMedia ?
      window.matchMedia("(prefers-color-scheme: dark)") : null;

    function draw(w, h) {
      if (!lastX) return;
      lastW = w || el.offsetWidth || width;
      lastH = h || el.offsetHeight || height;
      pvRender(el, lastX, lastW, lastH, mq);
    }
    if (mq && mq.addEventListener) {
      mq.addEventListener("change", function () { draw(); });
    }

    /* Containers change size without a window resize event - viewer
       panes get dragged, screenshot tools emulate viewports, layouts
       reflow. Watch the element itself and re-render (debounced) any
       time its box actually changes, so a chart never stays laid out
       for a width it no longer has. */
    if (window.ResizeObserver) {
      var pending = null;
      new ResizeObserver(function () {
        var w = el.offsetWidth, h = el.offsetHeight;
        if (!lastX || !w || !h) return;
        if (Math.abs(w - lastW) < 2 && Math.abs(h - lastH) < 2) return;
        clearTimeout(pending);
        pending = setTimeout(function () { draw(w, h); }, 120);
      }).observe(el);
    }

    return {
      renderValue: function (x) { lastX = x; draw(); },
      resize: function (w, h) { draw(w, h); }
    };
  }
});

/* The payload field each chart family draws from. When that field is
   missing or empty there is nothing to draw, and the widget should say
   so plainly. Types not listed here are left to their renderer. */
var pvDataKeys = {
  bar: "data", line: "data", scatter: "data", histogram: "data",
  area: "data", heatmap: "data", calendar: "data", donut: "data",
  lollipop: "data", beeswarm: "data", parallel: "data", race: "data",
  bump: "data", choropleth: "data", bubblemap: "data",
  table: "data", pairs: "data",
  slope: "data", dumbbell: "data", pyramid: "data",
  waterfall: "data", bullet: "data",
  waffle: "data", horizon: "data", flowmap: "data", hexmap: "data",
  force: "nodes", sankey: "nodes", arc: "nodes",
  chord: "matrix",
  sunburst: "root", pack: "root", treemap: "root", icicle: "root",
  dendrogram: "tree",
  boxplot: "boxes", violin: "violins", ridgeline: "ridges",
  facet: "panels"
};

function pvHasData(x) {
  var key = pvDataKeys[x.type];
  if (!key) return true;
  var v = x[key];
  if (v == null) return false;
  if (Array.isArray(v)) return v.length > 0;
  /* Hierarchy roots and trees are objects; existing is enough here,
     anything deeper is the renderer's business. */
  return true;
}

/* Locale support (pv_locale on the R side). A payload built while a
   locale was set carries the d3 locale definitions; the instances are
   built once per locale tag and shared by every chart on the page.
   d3's own stock en-US definitions are kept verbatim below so a chart
   WITHOUT a locale can put the defaults back after a localised chart
   swapped them - a page that never sees a locale never touches d3's
   defaults at all. */
var pvStockNumberLocale = {
  thousands: ",", grouping: [3], currency: ["$", ""]
};
var pvStockTimeLocale = {
  dateTime: "%x, %X", date: "%-m/%-d/%Y", time: "%-I:%M:%S %p",
  periods: ["AM", "PM"],
  days: ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday",
    "Friday", "Saturday"],
  shortDays: ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"],
  months: ["January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December"],
  shortMonths: ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
    "Sep", "Oct", "Nov", "Dec"]
};
var pvLocaleCache = {};
var pvLocaleSwapped = false;

function pvGetLocale(x) {
  var def = x.locale;
  if (!def || !def.number || !def.time) return null;
  var key = def.tag || "custom";
  if (!pvLocaleCache[key]) {
    pvLocaleCache[key] = {
      number: d3.formatLocale(def.number),
      time: d3.timeFormatLocale(def.time),
      decimal: def.number.decimal || "."
    };
  }
  return pvLocaleCache[key];
}

/* Writes a short notice into a cleared widget. The colours are
   hard-coded mid-tones that read on light and dark pages alike, because
   the theme itself may be the thing that failed. */
function pvNotice(el, text, isError) {
  el.innerHTML = "";
  var box = document.createElement("div");
  box.textContent = text;
  box.style.cssText =
    "box-sizing:border-box;padding:14px 16px;" +
    "font-family:system-ui,-apple-system,'Segoe UI',sans-serif;" +
    "font-size:12.5px;line-height:1.5;" +
    "color:" + (isError ? "#b3564d" : "#8a8a88") + ";";
  el.appendChild(box);
}

function pvRender(el, x, width, height, mq) {
  el.__pvLastX = x;
  /* The written description generated in R (R/alt-text.R) rides on the
     payload; applying it here makes every widget a labelled image for
     screen readers. */
  if (x.alt) {
    el.setAttribute("role", "img");
    el.setAttribute("aria-label", x.alt);
  }
  /* pv_static: the finished chart, inert. Blocking pointer events at
     the container silences every renderer's hover, tooltip, brush, and
     click wiring in one move, without touching how the chart draws.
     The keyboard access goes with it: no tabindex, so the chart never
     lands in the tab order, and no dangling describedby - the hidden
     description node is not rebuilt for a static render. Interactive
     charts get both back from pv.keyboardNav on this same render. */
  el.style.pointerEvents = x.static ? "none" : "";
  if (x.static) {
    el.removeAttribute("tabindex");
    el.removeAttribute("aria-describedby");
  }
  /* Whatever goes wrong below - a broken payload, a renderer bug -
     becomes a visible message in the widget itself; a silently blank
     box would look like the data's fault. The console keeps the
     original error and its stack for the developer. */
  try {
    pvRenderChart(el, x, width, height, mq);
  } catch (err) {
    if (window.console && console.error) console.error(err);
    pvNotice(el, "polyviz: rendering failed — " +
      (err && err.message ? err.message : String(err)), true);
  }
}

function pvRenderChart(el, x, width, height, mq) {
  if (!pvHasData(x)) {
    pvNotice(el, "polyviz: no data to display", false);
    return;
  }
  /* A static chart sheds its interactive affordances. The zoom strip
     goes here, before layout, so the plot keeps the strip's space. */
  if (x.static && x.zoom) x.zoom = false;
  /* Pick the light or dark colour set. "auto" follows the viewer's own
     system preference; the R side can also force one mode. */
  var mode = x.mode === "auto" ?
    (!pvHeadless && mq && mq.matches ? "dark" : "light") : x.mode;
  var ink = x.theme.ink[mode];
  /* Older payloads may not carry tooltip tokens; derive quiet defaults. */
  ink.tooltipBg = ink.tooltipBg || (mode === "dark" ? "#232322" : "#ffffff");
  ink.tooltipText = ink.tooltipText || ink.primary;
  ink.tooltipBorder = ink.tooltipBorder || ink.baseline;

  var theme = {
    mode: mode,
    palette: x.theme.categorical[mode],
    sequential: (x.theme.sequential && x.theme.sequential[mode]) ||
      x.theme.sequential,
    diverging: (x.theme.diverging && x.theme.diverging[mode]) ||
      x.theme.diverging,
    ink: ink
  };

  /* When the payload carries a locale, swap d3's default number and
     time locales in for the whole draw: every d3.format/d3.timeFormat
     call in the renderers, and the month names a time axis picks by
     itself, then come out localised without the renderers knowing.
     A chart without a locale restores the stock defaults first, so
     differently-localised charts coexist on one page. */
  var locale = pvGetLocale(x);
  if (locale) {
    d3.formatDefaultLocale(x.locale.number);
    d3.timeFormatDefaultLocale(x.locale.time);
    pvLocaleSwapped = true;
  } else if (pvLocaleSwapped) {
    d3.formatDefaultLocale(pvStockNumberLocale);
    d3.timeFormatDefaultLocale(pvStockTimeLocale);
    pvLocaleSwapped = false;
  }
  /* pv-common's tick formatter needs the locale too (pv.fmtTick is
     called without a ctx), so hand it over before anything draws. */
  pv.setLocale(locale);

  /* Start from a blank container every time - re-rendering is cheaper to
     reason about than patching an existing drawing. */
  el.innerHTML = "";
  el.style.position = "relative";
  el.style.background = ink.surface;
  el.style.fontFamily = (x.theme.font ||
    'system-ui, -apple-system, "Segoe UI", sans-serif');

  var header = pv.buildHeader(el, x, theme);
  /* The hover control for saving the chart, on unless pv_downloads(FALSE)
     turned it off. The header keeps clear of the corner it sits in, so
     a title or a wide legend never runs underneath it. */
  if (x.downloads !== false && !x.static) {
    pv.buildDownloadControl(el, x, theme);
    header.style.paddingRight = "48px";
  }
  /* The source/credit line sits at the very bottom, small and grey, the
     way FT and The Economist compose their charts. It's absolutely
     positioned so the SVG area doesn't have to know about it. */
  var footerH = 0;
  if (x.source) {
    var foot = document.createElement("div");
    foot.textContent = x.source;
    foot.style.cssText =
      "position:absolute;left:16px;right:16px;bottom:6px;font-size:11px;" +
      "line-height:1.35;color:" + theme.ink.muted + ";";
    el.appendChild(foot);
    /* Measure the real rendered height - the credit can wrap on narrow
       charts, and the plot must not draw underneath it. */
    footerH = foot.offsetHeight + 10;
  }
  var innerH = Math.max(120, height - header.offsetHeight - footerH);
  var tip = pv.buildTooltip(el, theme);

  var ctx = {
    el: el, x: x, theme: theme, tip: tip, header: header,
    width: width, height: innerH,
    duration: (pvHeadless || x.static) ?
      0 : (x.duration == null ? 500 : x.duration),
    static: !!x.static,
    /* Tooltip number formatter and (for date read-outs) the matching
       time formatter factory, both wearing the chart's locale when it
       has one. Without a locale these are exactly d3's stock output. */
    fmt: (locale ? locale.number.format : d3.format)(",.2~f"),
    fmtTime: locale ? locale.time.format : d3.timeFormat
  };

  /* Shiny round-trip: renderers report interactions through ctx.emit and,
     when the chart lives inside a Shiny app, they arrive as input values
     named <outputId>_<event> (e.g. input$mychart_click). Outside Shiny
     the call is a no-op, so renderers never need to check. */
  ctx.emit = function (event, payload) {
    if (window.Shiny && Shiny.setInputValue && el.id) {
      Shiny.setInputValue(el.id + "_" + event, payload,
        { priority: "event" });
    }
  };

  /* Crosstalk linking: when the R side attached a selection group
     (pv_link), keep a live handle on it. A selection made anywhere in
     the group re-renders this chart with ctx.selected holding the keys,
     and renderers dim what isn't in it via pv.keyOpacity. Selections
     this chart makes go out through ctx.select. */
  ctx.selected = el.__pvSelected || null;
  ctx.select = function () {};
  if (x.ctGroup && window.crosstalk) {
    if (!el.__pvCtHandle) {
      el.__pvCtHandle = new crosstalk.SelectionHandle();
      el.__pvCtHandle.setGroup(x.ctGroup);
      el.__pvCtHandle.on("change", function (e) {
        el.__pvSelected = (e.value && e.value.length) ?
          e.value : null;
        pvRender(el, el.__pvLastX || x, el.offsetWidth || width,
          el.offsetHeight || height, mq);
      });
    }
    ctx.selected = el.__pvSelected || null;
    ctx.select = function (keys) {
      el.__pvCtHandle.set(keys && keys.length ? keys : null);
    };
  }

  var renderer = pvRenderers[x.type];
  if (!renderer) {
    pvNotice(el, "polyviz: unknown chart type '" + x.type + "'", true);
    return;
  }
  renderer(ctx);

  /* Keyboard access, wired centrally so no renderer has to know about
     it: the marks are the elements already carrying tooltip handlers
     (pv.a11yMarks), and pv.keyboardNav makes the container focusable,
     walks them with the arrow keys, shows each mark's own tooltip, and
     echoes its text into an aria-live region. A chart whose hover
     binds some other way simply has no marks to walk - the container
     still focuses and describes itself. Static charts shed this along
     with every other interaction, and nothing here ever focuses on its
     own, so headless captures stay untouched. */
  if (!ctx.static) {
    pv.keyboardNav(ctx, pv.a11yMarks(el));
  }
}
