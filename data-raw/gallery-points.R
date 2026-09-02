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

## pairs
# explain: The scatterplot matrix is the chart to make first of any
#   unfamiliar set of measures: every pairwise view at once, scatter
#   cells below the diagonal, each variable's own histogram on it, and
#   the correlation coefficient above it - printed large and inked by
#   sign and strength on the diverging ramp, so the strong pairs
#   announce themselves before a single cell is read. method= picks the
#   coefficient, and it earns its keep here: equalization payments fall
#   as resources rise but stop dead at the cantonal average, a bend so
#   sharp that Pearson's r reports only -0.29, while Spearman's rank
#   coefficient reads the monotone rule underneath at -0.78. The perfect
#   1.00 in the first pair outs resource_index as resource_per_capita in
#   disguise - the index is the same figure rescaled to the average -
#   which is exactly the derived-column bookkeeping a pairs matrix
#   surfaces for free. One grouping colour splits the dots into the
#   scheme's contributors and receivers.
f25 <- subset(pv_fiscal, year == 2025)
f25$side <- ifelse(f25$equalization_chf > 0, "receives equalization",
                   "contributes")
pv_pairs(f25,
         columns = c("resource_per_capita", "resource_index",
                     "equalization_chf"),
         color = "side", label = "municipality", method = "spearman",
         title = "Every pair of fiscal measures at once",
         subtitle = "Three fiscal measures across the 79 Lucerne municipalities, 2025",
         source = "Source: LUSTAT Statistik Luzern")
