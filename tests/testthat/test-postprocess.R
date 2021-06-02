test_that("Sort data", {

  df <- sample_data("Cat-Num", addNA = F)
  names(df) <- c("a", "b")
  out <- postprocess(d = df, col = "b", sort = "desc")
  expect_identical(out$b, df %>% arrange(desc(b)) %>% .$b)

  out <- postprocess(d = df, col = "b", sort = "asc")
  expect_identical(out$b, df %>% arrange(b) %>% .$b)

})

test_that("Slice data", {

  df <- sample_data("Cat-Num", addNA = F)
  names(df) <- c("a", "b")
  out <- postprocess(d = df, col = "b", sort = "desc", slice_n = 3)
  expect_identical(out$b, (df %>% arrange(desc(b)) %>% .$b)[1:3])

  out <- postprocess(d = df, col = "b", sort = "asc", slice_n = 5)
  expect_identical(out$b, (df %>% arrange(b) %>% .$b)[1:5])

})
