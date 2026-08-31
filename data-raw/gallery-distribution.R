# Demo gallery examples for the distribution family. Each block below is
# one gallery entry: an explanation for the demo page, then a single
# runnable expression built on a bundled dataset.

## histogram
# explain: A histogram counts values into equal-width bins to show the
#   shape of one numeric variable — where values pile up, how far they
#   spread, and whether the distribution is skewed. Hover any bar for its
#   exact bin range and count; the overlaid curve is a kernel density
#   estimate rescaled to count space, so it traces the same shape the bars
#   show. Here it's the tax resource potential per resident of the 79
#   Lucerne municipalities in 2025: most cluster between roughly 2,000 and
#   3,500 CHF, with a long right tail of wealthy lakeside communities
#   running out to Meggen at about 10,600 CHF.
pv_histogram(subset(pv_fiscal, year == 2025), x = "resource_per_capita",
             bins = 20, density = TRUE,
             title = "Most Lucerne municipalities have modest tax bases",
             subtitle = "Tax resource potential per resident in CHF, 2025",
             source = "Source: LUSTAT Statistik Luzern")

## boxplot
# explain: A boxplot compresses a whole distribution into five numbers —
#   quartile box, median line, whiskers to the last values within 1.5 IQR,
#   dots for outliers beyond — which makes it the right tool for comparing
#   several distributions side by side. Hovering a box dims the others and
#   reads out all five statistics plus the group size; the jittered points
#   behind each box show the underlying values. By default (points =
#   "auto") the point cloud appears only when the groups hold at most 600
#   values in total; here points = TRUE forces it on for all 638. On
#   narrow screens crowded group labels thin themselves out or shorten
#   with an ellipsis — the full name is always in the tooltip. Across
#   2020-2027 the median Lucerne municipality's tax resources creep up
#   from about 2,600 to 2,800 CHF per resident, while the same handful of
#   wealthy outliers (Meggen, Weggis, Vitznau) float far above the boxes
#   every single year.
pv_boxplot(pv_fiscal, value = "resource_per_capita", group = "year",
           points = TRUE,
           title = "A stable middle, the same wealthy outliers every year",
           subtitle = "Tax resource potential per resident in CHF, by year",
           source = "Source: LUSTAT Statistik Luzern")

## violin
# explain: A violin plot mirrors a kernel density curve around each
#   group's centre line, trading the boxplot's five-number summary for
#   the distribution's whole shape — skew and bimodality stay visible.
#   The slim box inside each violin anchors the shape to its exact
#   quartiles and median (box = "auto" keeps it on), and hovering a
#   violin dims the others and reads out the group's size, median, and
#   quartiles; points = TRUE would add the boxplot family's jittered raw
#   values behind. The same data as the boxplot tells a sharper story
#   here: municipal tax resources per resident are strongly
#   right-skewed every single year — the violins bulge around 2,600-2,800
#   CHF and taper into a long thin neck toward the handful of wealthy
#   lakeside outliers above 8,000.
pv_violin(pv_fiscal, value = "resource_per_capita", group = "year",
          title = "The same skewed shape, year after year",
          subtitle = "Tax resource potential per resident in CHF, by year",
          source = "Source: LUSTAT Statistik Luzern")

## violin-points
# explain: The violin's two overlays anchor its smooth silhouette to
#   reality. box = TRUE (the default) draws a slim Tukey box inside each
#   shape, pinning it to its exact median and quartiles; points = TRUE
#   adds the raw values themselves, jittered no wider than the violin is
#   at each value, so the cloud of dots traces the same outline the
#   density claims. Reach for the dots when the audience should see that
#   the data really is there and the groups are modest enough for every
#   observation to read - "auto" draws them while no group holds more
#   than 200 values, and TRUE forces them on, thinning any group past
#   400 to a fixed-seed sample. Every one of 2023's 365 days lands here
#   as one dot inside its season's shape: summer packed tightly around a
#   median of 20.5 °C, winter reaching from -5 to +9, and autumn
#   stretched widest of all - a warm September crowds the top of its
#   violin while late November sinks toward freezing.
local({
  wx <- subset(pv_weather, format(date, "%Y") == "2023")
  # Meteorological seasons; December counts toward the winter block.
  m <- as.integer(format(wx$date, "%m"))
  wx$season <- c("Winter", "Winter", "Spring", "Spring", "Spring",
                 "Summer", "Summer", "Summer",
                 "Autumn", "Autumn", "Autumn", "Winter")[m]
  pv_violin(wx, value = "temp_mean", group = "season",
            box = TRUE, points = TRUE,
            xlab = NA, ylab = "Daily mean temperature (°C)",
            title = "A year of days, sorted into seasons",
            subtitle = "Every day of 2023 as a dot inside its season's distribution",
            source = "Source: MeteoSwiss")
})

## ridgeline
# explain: A ridgeline chart stacks one density curve per group on
#   overlapping baselines, so your eye can track how the shape of a
#   distribution shifts across groups — typically across time. Ridges are
#   ordered by median from the top down, and hovering one lifts it to full
#   opacity while dimming the rest, with the group's median and size in
#   the tooltip. Group labels claim at most 30% of the chart width — on
#   narrow screens long names shorten with an ellipsis and stay complete
#   in the tooltip. The resource index sets the cantonal average to 100, and
#   the bulk of Lucerne's municipalities slides steadily leftward: the
#   median falls from 78 in 2020 to 70 by 2027, meaning the typical
#   municipality drifts further below an average that a few rich outliers
#   pull up.
pv_ridgeline(pv_fiscal, value = "resource_index", group = "year",
             title = "The typical municipality falls further below average",
             subtitle = "Resource index across municipalities (cantonal average = 100)",
             source = "Source: LUSTAT Statistik Luzern")
