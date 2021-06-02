test_that("Order Categories", {

  data <- data.frame(game = c("play", "soccer", "tennis"), value = c(3, 7, 9))
  out <- order_category(data, "game", order = c("soccer"), label_wrap = NULL)
  expect_identical(out$game[1], "soccer")
  out <- order_category(data, "game", order = c("soccer", "tennis"), label_wrap = NULL)
  expect_identical(unique(out$game), c("soccer", "tennis", "play"))

})
