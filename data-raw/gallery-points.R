# Demo gallery examples for the points family. Each block below is one
# gallery entry: an explanation for the demo page, then a single runnable
# expression built on a bundled dataset.

## beeswarm
# explain: A beeswarm gives every observation its own dot — position along
#   the axis is the value, and a collision layout nudges overlapping dots
#   apart so each one stays visible. Use it instead of a histogram or
#   boxplot when the individual cases have names worth knowing, up to a
#   few hundred of them. Hovering a dot grows it and names it with its
#   exact value. Here every Lucerne municipality is one dot on the 2025
#   resource index (cantonal average = 100): the receiving majority — 53
#   of 79 municipalities — clusters left of 100 around a median of 74,
#   while a short tail of contributors stretches right, out to Meggen at
#   an index of 292.
f25 <- subset(pv_fiscal, year == 2025)
f25$side <- ifelse(f25$equalization_chf > 0,
                   "receives equalization", "contributes")
pv_beeswarm(f25, value = "resource_index", group = "side",
            label = "municipality",
            xlab = "Resource index (cantonal average = 100)",
            title = "Every municipality is a dot; most sit below average",
            subtitle = "Lucerne municipalities by resource index and equalization side, 2025",
            source = "Source: LUSTAT Statistik Luzern")
