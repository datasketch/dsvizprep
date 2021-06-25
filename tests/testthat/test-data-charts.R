# Function data_charts_prep -----------------------------------------------------------------------------------

test_that("data_charts_prep() works with simples arguments", {

  df <- sample_data("Cat")
  out <- data_charts_prep(data = df, ftype = "Cat", agg= "sum", ptage_col = NULL,
                          palette = "#FEAFEA")
  out <- out$data
  expect_identical(names(out), c("a", "..count", "..percentage", "value", "..colors"))

  df <- sample_data("Dat")
  out <- data_charts_prep(data = df, ftype = "Dat", agg= "sum", ptage_col = NULL,
                          palette = "#FEAFEA")
  out <- out$data
  expect_identical(names(out), c("a", "..count", "..percentage", "value", "..colors"))

  df <- sample_data("Cat-Num-Num")
  out <- data_charts_prep(data = df, ftype = "Cat", agg= "sum", ptage_col = NULL,
                          palette = "#FEAFEA")
  out <- out$data
  expect_identical(names(out), c("a", "..count", "..percentage", "b", "c", "value", "..colors"))


  df <- sample_data("Dat-Cat-Num")
  out <- data_charts_prep(data = df, ftype = "Dat-Cat", agg= "sum", ptage_col = NULL,
                          palette = "#FEAFEA")
  out <- out$data
  expect_identical(names(out), c("a", "..count", "..percentage", "b", "c", "value", "..colors"))

  df <- sample_data("Dat-Cat-Num")
  out <- data_charts_prep(data = df, ftype = "Dat-Cat-Num", agg= "sum", ptage_col = names(df)[1],
                          palette = "#FEAFEA")
  out <- out$data
  expect_identical(names(out), c("a", "b", "c", "..count", "..percentage", "value", "..colors"))

})



test_that("data_charts_prep() works with order instructions",{

  df <- data.frame(thinks = c("Rocks", "Papers", "Scissors", NA),
                   total = c(23, 45, -10, 1),
                   otros = c("One", "Two", "Three", "BLA"),
                   nature = c("Cat", "Fox", "Spider", "BLU"))

  test_order <- c("Scissors", "Papers", "Rocks")
  out <- data_charts_prep(data = df, ftype = "Cat", agg= "sum", ptage_col = NULL,
                          palette = "#FEAFEA", order = test_order)
  out <- out$data$a
  expect_identical(out, c(test_order, "na"))

  out <- data_charts_prep(data = df, ftype = "Cat-Num", agg= "sum", ptage_col = NULL,
                          palette = "#FEAFEA", order = test_order)
  out <- out$data$a
  expect_identical(out, c(test_order, "na"))
})
