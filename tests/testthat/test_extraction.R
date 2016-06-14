context("FOMC")

test_that("Get all the hedge tags", {
  ts <- get_tagset("fomc/2004_04_1-1.xml")
  expect_equal(nrow(ts), 8)
  expect_equal(ts$start, c("1", "110", "310", "427", "483", "556", "700", "843"))
  expect_equal(ts$type, c("notspeculation", "speculation", "notspeculation", "notspeculation",
                          "notspeculation", "speculation", "speculation", "speculation"))
  expect_equal(ts$text[8], "With inflation quite low and resource use slack, the Committee believes that it can be patient in removing its policy accommodation.")
})

test_that("Get all the hedge and note tags", {
  ts <- get_tagset("fomc/2004_03_2-1.xml", c("hedge", "note"))
  expect_equal(nrow(ts), 10)
  expect_equal(ts$here[9], "This is all pretty contingency ")
  expect_true(is.na(ts$type[9]))
  expect_true(is.na(ts$here[8]))
  expect_equal(ts$here[10], "")
  expect_equal(ts$node[9], "note")
  expect_equal(ts$id[9], "N0")
})

test_that("Warn but keep trucking if node is not there", {
  expect_message(get_tagset("fomc/2004_03_2-1.xml", c("hedge", "notepaper")))
})

test_that("We can parse a complete folder", {
  tss <- get_tagsets("fomc", nodes=c('hedge', 'note'))
  expect_message(get_tagsets("fomc", nodes=c('hedge', 'note')))
  expect_equal(dim(tss), c(55, 8))
})
