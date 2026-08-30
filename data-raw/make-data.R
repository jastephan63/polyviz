# Generates the package datasets (data/) and example data files (inst/extdata).
# Run from the package root: Rscript data-raw/make-data.R
set.seed(2026)

# ---- pv_sales: 24 months x 4 regions x 5 products -------------------------
months <- seq(as.Date("2024-01-01"), as.Date("2025-12-01"), by = "month")
regions <- c("North", "South", "East", "West")
products <- c("Aster", "Betula", "Cedrus", "Dahlia", "Erica")
unit_price <- c(Aster = 129, Betula = 89.5, Cedrus = 210, Dahlia = 159.99,
                Erica = 49)
base_units <- c(Aster = 320, Betula = 460, Cedrus = 120, Dahlia = 180,
                Erica = 640)
region_mult <- c(North = 1.25, South = 0.85, East = 1.05, West = 0.95)

pv_sales <- expand.grid(date = months, region = regions, product = products,
                        KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
t <- as.integer(format(pv_sales$date, "%m"))
month_idx <- match(pv_sales$date, months)
season <- 1 + 0.18 * sin((t - 3) / 12 * 2 * pi)
growth <- 1 + 0.012 * month_idx
noise <- exp(rnorm(nrow(pv_sales), 0, 0.10))
pv_sales$units <- round(base_units[pv_sales$product] *
                          region_mult[pv_sales$region] *
                          season * growth * noise)
pv_sales$revenue <- round(pv_sales$units * unit_price[pv_sales$product] *
                            runif(nrow(pv_sales), 0.92, 1), 2)
pv_sales$month <- format(pv_sales$date, "%Y-%m")
pv_sales <- pv_sales[order(pv_sales$date, pv_sales$region, pv_sales$product),
                     c("date", "month", "region", "product", "units", "revenue")]
rownames(pv_sales) <- NULL

# ---- pv_network: a 16-node, 4-team collaboration graph --------------------
teams <- c("Data", "Platform", "Design", "Research")
people <- c("Ada", "Grace", "Alan", "Edsger", "Barbara", "Donald", "Radia",
            "Ken", "Dennis", "Bjarne", "Guido", "Hedy", "Katherine", "Annie",
            "Margaret", "Linus")
nodes <- data.frame(
  id = people,
  group = rep(teams, each = 4),
  stringsAsFactors = FALSE
)
pair_pool <- t(combn(people, 2))
in_team <- nodes$group[match(pair_pool[, 1], nodes$id)] ==
  nodes$group[match(pair_pool[, 2], nodes$id)]
p <- ifelse(in_team, 0.55, 0.10)
keep <- runif(nrow(pair_pool)) < p
links <- data.frame(
  source = pair_pool[keep, 1],
  target = pair_pool[keep, 2],
  value = sample(1:8, sum(keep), replace = TRUE),
  stringsAsFactors = FALSE
)
# guarantee no isolated nodes
isolated <- setdiff(people, c(links$source, links$target))
if (length(isolated)) {
  links <- rbind(links, data.frame(
    source = isolated,
    target = sample(setdiff(people, isolated), length(isolated)),
    value = sample(1:4, length(isolated), replace = TRUE)))
}
pv_network <- list(nodes = nodes, links = links)

# ---- pv_flows: 5x5 inter-warehouse shipment matrix ------------------------
hubs <- c("Berlin", "Lyon", "Porto", "Milan", "Gdansk")
pv_flows <- matrix(round(runif(25, 40, 400)), 5, 5,
                   dimnames = list(hubs, hubs))
diag(pv_flows) <- 0

save(pv_sales, file = "data/pv_sales.rda", compress = "bzip2")
save(pv_network, file = "data/pv_network.rda", compress = "bzip2")
save(pv_flows, file = "data/pv_flows.rda", compress = "bzip2")

# ---- inst/extdata ---------------------------------------------------------
pkgload::load_all(".", quiet = TRUE)

db_path <- "inst/extdata/demo.sqlite"
unlink(db_path)
con <- pv_db_connect(db_path)
pv_db_write(con, "sales", pv_sales)
pv_run_sql_file(con, "inst/sql/demo.sql")
pv_db_disconnect(con)

labelled <- pv_set_labels(
  pv_sales[pv_sales$month >= "2025-07", ],
  c(date = "Calendar month (first day)",
    month = "Calendar month (YYYY-MM)",
    region = "Sales region",
    product = "Product line",
    units = "Units sold",
    revenue = "Net revenue, EUR")
)
pv_write_sas(labelled, "inst/extdata/demo_sales.xpt")

cat("Wrote data/ and inst/extdata/ artefacts.\n")
