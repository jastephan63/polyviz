expect_pvchart <- function(w, type) {
  expect_s3_class(w, "htmlwidget")
  expect_equal(attr(w, "package"), "polyviz")
  expect_equal(w$x$type, type)
  invisible(w)
}

# A tiny race with a known plot twist: C starts far behind, overtakes
# everyone by year 3; D exists but never makes the top 2; E only shows
# up (and ranks) in year 2.
toy_race <- function() {
  data.frame(
    yr = rep(c(2001, 2002, 2003), each = 4),
    who = rep(c("A", "B", "C", "D"), 3),
    val = c(100, 80, 10, 5,
            90, 85, 95, 6,
            80, 70, 120, 7)
  )
}

test_that("race keeps the union of top_n entities across all times", {
  d <- toy_race()
  w <- expect_pvchart(pv_race(d, "yr", "who", "val", top_n = 2), "race")
  # A and B rank top-2 in 2001, C from 2002 - D never does.
  expect_setequal(w$x$entities, c("A", "B", "C"))
  expect_false("D" %in% w$x$entities)
  expect_equal(w$x$times, c(2001, 2002, 2003))
  expect_equal(w$x$ttype, "number")
  expect_equal(w$x$topN, 2L)
  expect_equal(w$x$vlab, "val")
})

test_that("race sends a complete grid with per-time ranks", {
  d <- toy_race()
  w <- pv_race(d, "yr", "who", "val", top_n = 2)
  # 3 entities x 3 times, every combination present.
  expect_equal(nrow(w$x$data), 9)
  y1 <- w$x$data[w$x$data$t == 2001, ]
  expect_equal(y1$id[y1$rank == 1], "A")
  expect_equal(y1$id[y1$rank == 2], "B")
  # C ranks 3rd in 2001 (its true rank, beyond topN - the renderer
  # clamps it below the visible rows), and 1st by 2003.
  expect_equal(y1$rank[y1$id == "C"], 3)
  y3 <- w$x$data[w$x$data$t == 2003, ]
  expect_equal(y3$id[y3$rank == 1], "C")
})

test_that("race parks absent time/entity combinations below the field", {
  d <- toy_race()
  d <- d[!(d$who == "C" & d$yr == 2001), ] # C absent at the start
  w <- pv_race(d, "yr", "who", "val", top_n = 2)
  gap <- w$x$data[w$x$data$t == 2001 & w$x$data$id == "C", ]
  expect_equal(gap$value, 0)
  expect_equal(gap$rank, 3) # top_n + 1
})

test_that("race orders entities by final standings, departed ones last", {
  d <- toy_race()
  w <- pv_race(d, "yr", "who", "val", top_n = 2)
  # Final year order: C (120), A (80), B (70).
  expect_equal(w$x$entities, c("C", "A", "B"))
  # An entity that vanishes before the end still gets a slot, after the
  # finishers.
  d2 <- d[!(d$who == "A" & d$yr == 2003), ]
  w2 <- pv_race(d2, "yr", "who", "val", top_n = 2)
  expect_equal(w2$x$entities, c("C", "B", "A"))
})

test_that("race caps top_n at the entities that ever qualify", {
  d <- toy_race()
  w <- pv_race(d, "yr", "who", "val", top_n = 10)
  expect_setequal(w$x$entities, c("A", "B", "C", "D"))
  expect_equal(w$x$topN, 4L)
})

test_that("race accepts Date time columns", {
  d <- toy_race()
  d$yr <- as.Date(paste0(d$yr, "-01-01"))
  w <- pv_race(d, "yr", "who", "val", top_n = 2)
  expect_equal(w$x$ttype, "date")
  expect_equal(w$x$times,
               c("2001-01-01", "2002-01-01", "2003-01-01"))
})

test_that("race validates its inputs with clear messages", {
  d <- toy_race()
  expect_error(pv_race(d, "nope", "who", "val"), "not in `data`")
  expect_error(pv_race(d, "yr", "who", "val", top_n = 1), "at least 2")
  expect_error(pv_race(d, "yr", "who", "val", top_n = 2.5), "whole number")
  one_t <- d[d$yr == 2001, ]
  expect_error(pv_race(one_t, "yr", "who", "val"), "2 distinct time points")
  d$who2 <- "same"
  expect_error(pv_race(d, "yr", "who2", "val"), "one row per time/id")
  neg <- toy_race()
  neg$val[1] <- -3
  expect_error(pv_race(neg, "yr", "who", "val"), "non-negative")
  chr <- toy_race()
  chr$yr <- as.character(chr$yr)
  expect_error(pv_race(chr, "yr", "who", "val"), "numeric or Date")
})

test_that("race treats NA values as absence, not as zero data", {
  d <- toy_race()
  d$val[d$who == "C" & d$yr == 2001] <- NA
  w <- pv_race(d, "yr", "who", "val", top_n = 2)
  gap <- w$x$data[w$x$data$t == 2001 & w$x$data$id == "C", ]
  expect_equal(gap$rank, 3)
})

test_that("bump follows the final-time top_n in final-rank order", {
  w <- expect_pvchart(
    pv_bump(pv_city_population, "year", "city", "population", top_n = 10),
    "bump")
  expect_length(w$x$entities, 10)
  expect_equal(w$x$entities[1:4],
               c("Zürich", "Genève", "Basel", "Lausanne"))
  expect_equal(w$x$topN, 10)
  expect_equal(w$x$vlab, "population")
})

test_that("bump drops the time points where an entity ranks too low", {
  w <- pv_bump(pv_city_population, "year", "city", "population", top_n = 10)
  # Lugano ranked 12th in 1930 and 11th in 1970 - outside the top 10 -
  # so its line only spans 1980 onwards.
  lugano <- w$x$data[w$x$data$id == "Lugano", ]
  expect_equal(sort(lugano$t), c(1980, 1990, 2000, 2014, 2024))
  # Every drawn rank sits inside the visible band.
  expect_true(all(w$x$data$rank >= 1 & w$x$data$rank <= 10))
  # And rows carry the value for the tooltip.
  z24 <- w$x$data[w$x$data$id == "Zürich" & w$x$data$t == 2024, ]
  expect_equal(z24$rank, 1)
  expect_equal(z24$value,
               pv_city_population$population[
                 pv_city_population$city == "Zürich" &
                   pv_city_population$year == 2024])
})

test_that("bump ranks against the whole field, not just the chosen few", {
  d <- toy_race()
  # Final year: C 120, A 80, D 7 ... with top_n = 2 only C and A drawn.
  w <- pv_bump(d, "yr", "who", "val", top_n = 2)
  expect_equal(w$x$entities, c("C", "A"))
  # In 2002 the full-field ranking is C, A, B - A holds rank 2 there
  # even though B is not drawn.
  y2 <- w$x$data[w$x$data$t == 2002, ]
  expect_equal(y2$id[y2$rank == 1], "C")
  expect_false("B" %in% w$x$data$id)
})

test_that("bump trims the rank axis to what is actually drawn", {
  d <- toy_race()
  w <- pv_bump(d, "yr", "who", "val", top_n = 10)
  # Only 4 entities exist, so the deepest drawn rank is 4.
  expect_equal(w$x$topN, 4)
})

test_that("bump validates like race", {
  d <- toy_race()
  expect_error(pv_bump(d, "yr", "who", "nope"), "not in `data`")
  expect_error(pv_bump(d, "yr", "who", "val", top_n = 0), "at least 2")
  one_t <- d[d$yr == 2003, ]
  expect_error(pv_bump(one_t, "yr", "who", "val"), "2 distinct time points")
})
