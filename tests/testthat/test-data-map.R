# Function data_map_prep -----------------------------------------------------------------------------------

test_that("data_map_prep() works with geoname or geocode", {

  df <- sample_data("Gcd")
  out <- data_map_prep(data = df, ftype = "Gcd", agg= "sum", ptage_col = NULL)
  out <- out$data
  expect_identical(names(out), c("a", "..count", "..percetage", "value"))

  df <- sample_data("Gnm")
  out <- data_map_prep(data = df, ftype = "Gnm", agg= "sum", ptage_col = NULL)
  out <- out$data
  expect_identical(names(out), c("a", "..count", "..percetage", "value"))

  df <- sample_data("Gcd-Num-Num")
  out <- data_map_prep(data = df, ftype = "Gcd", agg= "sum", ptage_col = NULL)
  out <- out$data
  expect_identical(names(out), c("a", "..count", "..percetage", "b", "c", "value"))

  df <- sample_data("Gcd-Num")
  out <- data_map_prep(data = df, ftype = "Gcd-Num", agg= "sum", ptage_col = NULL)
  out <- out$data
  expect_identical(names(out), c("a", "b", "..count", "..percetage", "value"))

  df <- sample_data("Gnm-Cat-Num")
  out <- data_map_prep(data = df, ftype = "Gnm", agg= "sum", ptage_col = NULL)
  out <- out$data
  expect_identical(names(out), c("a", "..count", "..percetage", "b", "c", "value"))

  df <- sample_data("Gnm-Cat-Num")
  out <- data_map_prep(data = df, ftype = "Gnm-Cat-Num", agg= "sum", ptage_col = names(df)[1])
  out <- out$data
  expect_identical(names(out), c("a", "b", "c", "..count", "..percetage", "value"))

})

test_that("data_map_prep() works with latitude and longitude", {

  df <- sample_data("Glt-Gln")
  out <- data_map_prep(data = df, ftype = "Glt-Gln", agg= "sum", ptage_col = NULL)
  out <- out$data
  expect_identical(names(out), c("a", "b", "..count", "..percetage", "value"))


  df <- sample_data("Glt-Gln-Gnm")
  out <- data_map_prep(data = df, ftype = "Glt-Gln", agg= "sum", ptage_col = NULL)
  out <- out$data
  expect_identical(names(out), c("a", "b", "..count", "..percetage", "c", "value"))

  df <- sample_data("Glt-Gln-Gnm")
  out <- data_map_prep(data = df, ftype = "Glt-Gln-Gnm", agg= "sum", ptage_col = NULL)
  out <- out$data
  expect_identical(names(out), c("a", "b", "c", "..count", "..percetage", "value"))

  df <- sample_data("Glt-Gln-Num")
  out <- data_map_prep(data = df, ftype = "Glt-Gln-Num", agg= "sum", ptage_col = NULL)
  out <- out$data
  expect_identical(names(out), c("a", "b", "c", "..count", "..percetage", "value"))

  df <- sample_data("Glt-Gln-Cat-Num")
  out <- data_map_prep(data = df, ftype = "Glt-Gln-Cat", agg= "sum", ptage_col = names(df)[3])
  out <- out$data
  expect_identical(names(out), c("a", "b", "c", "..count", "..percetage", "d", "value"))

  df <- sample_data("Glt-Gln-Cat-Num")
  out <- data_map_prep(data = df, ftype = "Glt-Gln-Cat-Num", agg= "sum", ptage_col = NULL)
  out <- out$data
  expect_identical(names(out), c("a", "b", "c", "d", "..count", "..percetage", "value"))

})

