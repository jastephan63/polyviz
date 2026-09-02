/* The table renderer. polyviz's one non-SVG chart: a sortable,
   design-system-styled HTML table with in-cell encodings. Registered
   on pvRenderers like every other chart family. */
(function () {

  var SVG_NS = "http://www.w3.org/2000/svg";

  /* Pick whichever of the two ink extremes reads better on a given cell
     fill, by WCAG contrast ratio from relative luminance - the same
     arithmetic the heatmap uses for its in-cell labels. */
  function lum(c) {
    var rgb = d3.rgb(c);
    function chan(u) {
      u /= 255;
      return u <= 0.04045 ? u / 12.92 : Math.pow((u + 0.055) / 1.055, 2.4);
    }
    return 0.2126 * chan(rgb.r) + 0.7152 * chan(rgb.g) +
      0.0722 * chan(rgb.b);
  }

  pvRenderers.table = function (ctx) {
    var x = ctx.x;
    var ink = ctx.theme.ink;
    var cols = x.columns || [];
    var rows = x.data || [];
    var sortable = x.sortable !== false;
    var pageSize = (typeof x.pageSize === "number" && x.pageSize > 0) ?
      Math.floor(x.pageSize) : null;

    var lumPrimary = lum(ink.primary);
    var lumSurface = lum(ink.surface);
    function inkFor(fill) {
      var L = lum(fill);
      var vsPrimary = (Math.max(L, lumPrimary) + 0.05) /
        (Math.min(L, lumPrimary) + 0.05);
      var vsSurface = (Math.max(L, lumSurface) + 0.05) /
        (Math.min(L, lumSurface) + 0.05);
      return vsPrimary >= vsSurface ? ink.primary : ink.surface;
    }

    /* Cell number formatting. The R side already decided each column's
       decimal count, whether it is fixed (explicit `digits`) or trimmed
       (automatic), and whether the column is small whole numbers - the
       package-wide tick rule that keeps years ungrouped ("2020", never
       "2,020"). The locale, when the payload carries one, contributes
       its grouping and decimal marks exactly as it does on axis ticks;
       unlike ticks, cells keep their full precision - a table is where
       the exact numbers live. */
    var fmtOf = {};
    cols.forEach(function (col) {
      if (col.type !== "number") return;
      var make = pv.locale ? pv.locale.number.format : d3.format;
      var spec = (col.small ? "" : ",") + "." + (col.digits || 0) +
        (col.fixed ? "f" : "~f");
      fmtOf[col.key] = make(spec);
    });

    /* The sequential ramp for shaded cells, low to high over the domain
       the R side measured per column. */
    var ramp = d3.interpolateRgbBasis(ctx.theme.sequential);
    var accent = ctx.theme.palette[0];

    /* Sort and page state live on the widget element, like the zoom
       window does, so they survive the full re-renders that resizes and
       theme flips trigger. dir is +1 ascending, -1 descending. */
    var state = ctx.el.__pvTableState ||
      { key: null, dir: 1, page: 0 };
    ctx.el.__pvTableState = state;

    function colByKey(key) {
      for (var i = 0; i < cols.length; i++) {
        if (cols[i].key === key) return cols[i];
      }
      return null;
    }

    /* The rows in display order: the payload's order until a header
       click chose a column. Numbers sort numerically, text lexically,
       and missing cells sink to the bottom either way. */
    function orderedRows() {
      var out = rows.slice();
      var col = state.key && colByKey(state.key);
      if (!col || col.type === "spark") return out;
      var dir = state.dir;
      var numeric = col.type === "number";
      out.sort(function (a, b) {
        var va = a[col.key], vb = b[col.key];
        var ma = va == null, mb = vb == null;
        if (ma || mb) return ma && mb ? 0 : (ma ? 1 : -1);
        if (numeric) return dir * (va - vb);
        return dir * String(va).localeCompare(String(vb));
      });
      return out;
    }

    /* ---------- chrome ---------- */

    var wrap = document.createElement("div");
    wrap.className = "pv-table";
    wrap.style.cssText =
      "box-sizing:border-box;height:" + ctx.height + "px;" +
      "padding:2px 16px 8px 16px;display:flex;flex-direction:column;";
    var scroller = document.createElement("div");
    scroller.style.cssText = "flex:1 1 auto;min-height:0;overflow:auto;";
    var table = document.createElement("table");
    /* border-collapse stays "separate" so the sticky header keeps its
       own bottom rule while rows scroll underneath it. */
    table.style.cssText =
      "width:100%;border-collapse:separate;border-spacing:0;" +
      "font-size:12px;line-height:1.4;color:" + ink.primary + ";";

    var thead = document.createElement("thead");
    var headRow = document.createElement("tr");
    var heads = [];

    cols.forEach(function (col, ci) {
      var th = document.createElement("th");
      th.setAttribute("scope", "col");
      var right = col.type === "number";
      th.style.cssText =
        "position:sticky;top:0;z-index:1;background:" + ink.surface + ";" +
        "text-align:" + (right ? "right" : "left") + ";" +
        "font-size:11.5px;font-weight:600;color:" + ink.secondary + ";" +
        "padding:6px 14px 7px 0;white-space:nowrap;" +
        "border-bottom:1px solid " + ink.baseline + ";";
      if (ci === cols.length - 1) th.style.paddingRight = "0";
      /* A bar column's cells end in the fixed bar zone; pad the header
         past it so the label right-aligns over the numbers. */
      if (col.bar) {
        th.style.paddingRight =
          (parseFloat(th.style.paddingRight) + 86) + "px";
      }
      th.appendChild(document.createTextNode(col.label));

      var canSort = sortable && col.type !== "spark";
      var arrow = null;
      if (canSort) {
        /* The sort arrow: hidden until its column is the active sort.
           Hiding goes through inline display, which the SVG exporter
           honours when it clones svg elements, so inactive arrows never
           leak into a saved chart. */
        arrow = document.createElementNS(SVG_NS, "svg");
        arrow.setAttribute("width", "8");
        arrow.setAttribute("height", "8");
        arrow.setAttribute("viewBox", "0 0 8 8");
        arrow.setAttribute("aria-hidden", "true");
        arrow.style.cssText = "display:none;margin-left:5px;";
        var tri = document.createElementNS(SVG_NS, "path");
        tri.setAttribute("fill", ink.secondary);
        arrow.appendChild(tri);
        th.appendChild(arrow);

        th.style.cursor = "pointer";
        th.addEventListener("click", function () {
          if (state.key === col.key) {
            state.dir = -state.dir;
          } else {
            state.key = col.key;
            /* Numbers open largest-first - the question a value column
               answers - text opens A to Z. */
            state.dir = col.type === "number" ? -1 : 1;
          }
          state.page = 0;
          drawHeads();
          drawBody();
          drawPager();
        });
        th.addEventListener("mouseenter", function () {
          th.style.color = ink.primary;
        });
        th.addEventListener("mouseleave", function () {
          th.style.color = ink.secondary;
        });
      }
      heads.push({ col: col, arrow: arrow });
      headRow.appendChild(th);
    });
    thead.appendChild(headRow);

    function drawHeads() {
      heads.forEach(function (h) {
        if (!h.arrow) return;
        var active = state.key === h.col.key;
        h.arrow.style.display = active ? "inline" : "none";
        if (active) {
          h.arrow.firstChild.setAttribute("d", state.dir > 0 ?
            "M4 1.6 7 6.4H1z" : "M4 6.4 1 1.6h6z");
        }
      });
    }

    /* ---------- cells ---------- */

    function dash(td) {
      td.textContent = "—";
      td.style.color = ink.muted;
    }

    /* The in-cell bar: the value right-aligned first, then a thin run
       of the series-1 colour growing from the column's shared baseline
       just after the numbers, rounded at the data end and scaled to the
       column maximum the R side measured. The bar zone is a fixed-width
       slot pinned to the cell's right side, so every bar in the column
       starts from the same edge. Values at or below zero draw no bar -
       the number still says what they are. */
    function drawBar(td, col, v, text) {
      var box = document.createElement("div");
      box.style.cssText = "display:flex;align-items:center;";
      var num = document.createElement("span");
      num.style.cssText =
        "flex:1 1 auto;text-align:right;" +
        "font-variant-numeric:tabular-nums;";
      num.textContent = text;
      var zone = document.createElement("div");
      zone.style.cssText =
        "flex:0 0 76px;height:6px;position:relative;margin-left:10px;";
      var frac = col.barMax > 0 ?
        Math.max(0, Math.min(1, v / col.barMax)) : 0;
      if (frac > 0) {
        var bar = document.createElement("div");
        bar.style.cssText =
          "position:absolute;left:0;top:0;height:6px;" +
          "border-radius:0 3px 3px 0;background:" + accent + ";" +
          "width:" + (frac * 100) + "%;";
        zone.appendChild(bar);
      }
      box.appendChild(num);
      box.appendChild(zone);
      td.appendChild(box);
    }

    /* An inline sparkline: the cell's numeric vector as a tiny line
       with a dot on its final value, no axes, each cell scaled to its
       own range. Hover reads out the span - first value, last value,
       and how many points carried them. */
    function drawSpark(td, col, v) {
      var vals = Array.isArray(v) ? v : (v == null ? [] : [v]);
      var pts = [];
      vals.forEach(function (d, i) {
        var n = Number(d);
        if (d != null && isFinite(n)) pts.push({ i: i, v: n });
      });
      if (!pts.length) {
        dash(td);
        return;
      }
      var w = 64, h = 20, pad = 3;
      var svg = d3.select(td).append("svg")
        .attr("width", w).attr("height", h)
        .style("display", "block");
      var sx = d3.scaleLinear()
        .domain([0, Math.max(1, vals.length - 1)]).range([pad, w - pad]);
      var lo = d3.min(pts, function (p) { return p.v; });
      var hi = d3.max(pts, function (p) { return p.v; });
      /* A flat series still deserves a line - pin it to mid-height
         rather than divide by a zero range. */
      var sy = hi > lo ?
        d3.scaleLinear().domain([lo, hi]).range([h - pad, pad]) :
        function () { return h / 2; };
      if (pts.length > 1) {
        svg.append("path").datum(pts)
          .attr("fill", "none")
          .attr("stroke", accent)
          .attr("stroke-width", 1.5)
          .attr("stroke-linecap", "round")
          .attr("stroke-linejoin", "round")
          .attr("d", d3.line()
            .x(function (p) { return sx(p.i); })
            .y(function (p) { return sy(p.v); }));
      }
      var last = pts[pts.length - 1];
      svg.append("circle")
        .attr("cx", sx(last.i)).attr("cy", sy(last.v))
        .attr("r", 2).attr("fill", accent);
      var first = pts[0];
      td.addEventListener("pointerenter", showSpan);
      td.addEventListener("pointermove", showSpan);
      td.addEventListener("pointerleave", function () { pv.hideTip(ctx); });
      function showSpan(event) {
        pv.showTip(ctx, event, "<b>" + pv.esc(col.label) + "</b><br>" +
          ctx.fmt(first.v) + " → " + ctx.fmt(last.v) +
          " (" + pts.length + " points)");
      }
    }

    function cell(row, col, ci) {
      var td = document.createElement("td");
      var right = col.type === "number";
      td.style.cssText =
        "padding:5px 14px 5px 0;vertical-align:middle;" +
        "border-bottom:1px solid " + ink.grid + ";" +
        (right ?
          "text-align:right;font-variant-numeric:tabular-nums;" : "");
      if (ci === cols.length - 1) td.style.paddingRight = "0";
      var v = row[col.key];
      if (col.type === "spark") {
        drawSpark(td, col, v);
        return td;
      }
      if (col.type === "number") {
        if (v == null || isNaN(v)) {
          dash(td);
          return td;
        }
        var text = fmtOf[col.key](v);
        if (col.bar) {
          drawBar(td, col, v, text);
          return td;
        }
        if (col.shade) {
          var lo = col.shadeMin, hi = col.shadeMax;
          var t = hi > lo ? (v - lo) / (hi - lo) : 0.5;
          var fill = ramp(Math.max(0, Math.min(1, t)));
          td.style.background = fill;
          td.style.color = inkFor(fill);
          td.style.paddingLeft = "8px";
        }
        td.textContent = text;
        return td;
      }
      if (v == null || v === "") {
        dash(td);
        return td;
      }
      td.textContent = String(v);
      return td;
    }

    /* ---------- body & pager ---------- */

    var tbody = document.createElement("tbody");

    function drawBody() {
      tbody.innerHTML = "";
      var out = orderedRows();
      if (pageSize) {
        var pages = Math.max(1, Math.ceil(out.length / pageSize));
        state.page = Math.max(0, Math.min(state.page, pages - 1));
        out = out.slice(state.page * pageSize,
          (state.page + 1) * pageSize);
      }
      out.forEach(function (row) {
        var tr = document.createElement("tr");
        /* Hover: a subtle surface shift over the whole row. Shaded
           cells keep their own backgrounds on top of it. */
        tr.addEventListener("pointerenter", function () {
          tr.style.background = ink.grid;
        });
        tr.addEventListener("pointerleave", function () {
          tr.style.background = "";
        });
        cols.forEach(function (col, ci) {
          tr.appendChild(cell(row, col, ci));
        });
        tbody.appendChild(tr);
      });
    }

    /* The pager, when a page size was set and the rows outgrow it: a
       row range plus previous/next, quiet and right-aligned in the
       footer next to the source line. The buttons are <button>
       elements, which the SVG exporter skips, so a saved chart keeps
       only the honest "rows m-n of N" note. */
    var pager = null, rangeEl = null, prevBtn = null, nextBtn = null;

    function pagerBtn(glyph, label) {
      var b = document.createElement("button");
      b.type = "button";
      b.textContent = glyph;
      b.setAttribute("aria-label", label);
      b.style.cssText =
        "width:22px;height:20px;padding:0;border-radius:6px;" +
        "border:1px solid " + ink.baseline + ";" +
        "background:" + ink.surface + ";color:" + ink.secondary + ";" +
        "font:inherit;font-size:13px;line-height:1;cursor:pointer;";
      return b;
    }

    function drawPager() {
      if (!pager) return;
      var total = rows.length;
      var pages = Math.max(1, Math.ceil(total / pageSize));
      var from = state.page * pageSize + 1;
      var to = Math.min(total, (state.page + 1) * pageSize);
      rangeEl.textContent =
        from + "–" + to + " of " + total + " rows";
      prevBtn.disabled = state.page <= 0;
      nextBtn.disabled = state.page >= pages - 1;
      prevBtn.style.opacity = prevBtn.disabled ? 0.35 : 1;
      nextBtn.style.opacity = nextBtn.disabled ? 0.35 : 1;
      prevBtn.style.cursor = prevBtn.disabled ? "default" : "pointer";
      nextBtn.style.cursor = nextBtn.disabled ? "default" : "pointer";
    }

    if (pageSize && rows.length > pageSize) {
      pager = document.createElement("div");
      pager.className = "pv-table-pager";
      pager.style.cssText =
        "flex:0 0 auto;display:flex;align-items:center;" +
        "justify-content:flex-end;gap:8px;padding-top:7px;" +
        "font-size:11px;color:" + ink.muted + ";" +
        "font-variant-numeric:tabular-nums;";
      rangeEl = document.createElement("span");
      prevBtn = pagerBtn("‹", "Previous page");
      nextBtn = pagerBtn("›", "Next page");
      prevBtn.addEventListener("click", function () {
        if (state.page > 0) {
          state.page--;
          drawBody();
          drawPager();
        }
      });
      nextBtn.addEventListener("click", function () {
        if ((state.page + 1) * pageSize < rows.length) {
          state.page++;
          drawBody();
          drawPager();
        }
      });
      pager.appendChild(rangeEl);
      pager.appendChild(prevBtn);
      pager.appendChild(nextBtn);
    }

    table.appendChild(thead);
    table.appendChild(tbody);
    scroller.appendChild(table);
    wrap.appendChild(scroller);
    if (pager) wrap.appendChild(pager);
    ctx.el.appendChild(wrap);

    drawHeads();
    drawBody();
    drawPager();
  };

  /* Hairline rescue for exported tables. The standalone-SVG serialiser
     (pv-common.js) rebuilds a widget from its text nodes and background
     colours, which drops CSS borders - and a table's structure IS its
     hairline rules. So wrap the serialiser the way the zoom strip does:
     after the stock walk, measure every table row still laid out on the
     page and append its bottom rule as a one-pixel rect in the rule's
     own computed colour. pv_save()'s .pdf format prints through this
     same serialiser, so a table PDF keeps the header rule and the row
     separators. */
  var serialise = pv.toStandaloneSvg;
  pv.toStandaloneSvg = function (el, x, theme) {
    var out = serialise(el, x, theme);
    try {
      var wrap = el && el.querySelector ?
        el.querySelector(".pv-table") : null;
      if (!wrap) return out;
      var base = el.getBoundingClientRect();
      var add = "";
      var rowEls = wrap.querySelectorAll("tr");
      for (var i = 0; i < rowEls.length; i++) {
        var cell0 = rowEls[i].cells && rowEls[i].cells[0];
        if (!cell0) continue;
        var r = rowEls[i].getBoundingClientRect();
        if (!r.width || !r.height) continue;
        var color = getComputedStyle(cell0).borderBottomColor;
        add += '<rect x="' + (Math.round((r.left - base.left) * 100) / 100) +
          '" y="' + (Math.round((r.bottom - base.top - 1) * 100) / 100) +
          '" width="' + (Math.round(r.width * 100) / 100) +
          '" height="1" fill="' + color + '"/>';
      }
      if (add) out = out.replace(/<\/svg>\s*$/, add + "</svg>");
    } catch (e) {
      /* A failed measurement must never cost the export itself. */
    }
    return out;
  };

})();
