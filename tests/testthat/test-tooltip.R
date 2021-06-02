
test_that("Tooltip maps", {

  data <- sample_data("Gcd-Cat-Num-Num-Cat-Gnm")
  f <- homodatum::fringe(data)
  nms <- homodatum::fringe_labels(f)
  lf <- names(data)[c(1,3,5)]
  l_tooltip <- tooltip_map(nms,
                            label_ftype = lf,
                            tooltip = "")

  nms_filter <-  nms[nms %in% lf]
  nms_names <- names(nms_filter)
  l <- map(seq_along(nms_filter), function(i){
    paste0(nms_filter[[i]], ": {", nms_names[i], "_label}")
  }) %>% unlist()
  tooltip <- paste0(l, collapse = "<br/>")
  expect_identical(l_tooltip, tooltip)

})
