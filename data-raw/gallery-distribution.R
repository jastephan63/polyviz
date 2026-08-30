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
#   behind each box show every underlying value. Across 2020-2027 the
#   median Lucerne municipality's tax resources creep up from about 2,600
#   to 2,800 CHF per resident, while the same handful of wealthy outliers
#   (Meggen, Weggis, Vitznau) float far above the boxes every single year.
pv_boxplot(pv_fiscal, value = "resource_per_capita", group = "year",
           points = TRUE,
           title = "A stable middle, the same wealthy outliers every year",
           subtitle = "Tax resource potential per resident in CHF, by year",
           source = "Source: LUSTAT Statistik Luzern")

## ridgeline
# explain: A ridgeline chart stacks one density curve per group on
#   overlapping baselines, so your eye can track how the shape of a
#   distribution shifts across groups — typically across time. Ridges are
#   ordered by median from the top down, and hovering one lifts it to full
#   opacity while dimming the rest, with the group's median and size in
#   the tooltip. The resource index sets the cantonal average to 100, and
#   the bulk of Lucerne's municipalities slides steadily leftward: the
#   median falls from 78 in 2020 to 70 by 2027, meaning the typical
#   municipality drifts further below an average that a few rich outliers
#   pull up.
pv_ridgeline(pv_fiscal, value = "resource_index", group = "year",
             title = "The typical municipality falls further below average",
             subtitle = "Resource index across municipalities (cantonal average = 100)",
             source = "Source: LUSTAT Statistik Luzern")
