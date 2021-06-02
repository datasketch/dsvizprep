
test_that("Process data options", {

  d <- data.frame(a = c("Cats", "Cats", NA), b = c(1:3), stringsAsFactors = FALSE)
  d2 <- preprocessData(d, drop_na = TRUE)
  expect_equal(nrow(d2), 2)
  d <- data.frame(a = factor(c("Cats", "Cats", NA)), b = c(1:3), stringsAsFactors = FALSE)
  d3 <- preprocessData(d, drop_na = FALSE, na_label = "NoInfo", na_label_cols = "a")
  expect_equal(levels(d3$a), c("Cats","NoInfo"))

})

test_that("Agreggation by groups", {

  d <- data.frame(transport = c("ship", "ship", "ship", "plane", "train"), units = runif(5),
                  stringsAsFactors = FALSE)
  d2 <- summarizeData(d, "sum", to_agg = units, transport)
  expect_equal(sort(unique(d2$transport)), sort(unique(d$transport)))

})

test_that("Aggs work",{

  dd <- tibble(x = c("A","A", "B", "C"), y = c("X","Y","Y","Z"), z = c(1,1,3,NA))

  no_na <- preprocessData(dd, drop_na = TRUE)
  expect_false(any(is.na(no_na)))

  d_summ1 <- summarizeData(dd, agg = "sum", to_agg = z, x)
  expect_true(d_summ1$z[d_summ1$x == "A"] == 2)

  d_summ2 <- summarizeData(dd, "sum", to_agg = z, y)
  expect_equal(d_summ2$z, c(1,4,0))

})

test_that("Function agg", {

  df <- sample_data("Cat-Num", addNA = F)
  names(df) <- c("a", "b")
  expect_error(function_agg(df, "sum", to_agg = "a"))

  out <- function_agg(df, "sum", to_agg = "b")
  expect_equal(out$b, sum(df$b))

  out <- function_agg(df, "sum", to_agg = "b", a)
  expect_equal(names(out), c("a", "b", "..count"))
  expect_equal(out$a, sort(unique(df$a)))

})

test_that("Percentage data", {

  df <- sample_data("Cat-Num", addNA = F)
  names(df) <- c("a", "b")
  expect_error(percentage_data(df, "b", by_col = a))

  out <- percentage_data(df, "b")
  expect_equal(sum(out$..percentage), 100)

  out <- percentage_data(df, "b", by_col = "a")
  expect_equal(names(out), c("a", "b", "..percentage"))
  cat_sample <- sample(unique(df$a), 1)
  test_out <- out %>% filter(a == cat_sample)
  expect_equal(sum(test_out$..percentage, na.rm = T), 100)

})

test_that("Collapse data", {

  df <- sample_data("Cat-Cat", addNA = F)
  names(df) <- c("a", "b")
  out <- collapse_data(df)
  expect_equal(out$a, paste0(unique(df$a), collapse = ". "))
  expect_equal(out$b, paste0(unique(df$b), collapse = ". "))

  out <- collapse_data(df, a)
  cat_sample <- sample(unique(df$a), 1)
  test_out <- out %>% filter(a == cat_sample)
  df_filter <- df %>% filter(a == cat_sample)
  expect_equal(test_out$b, paste0(unique(df_filter$b), collapse = ". "))


})

