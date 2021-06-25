#' General function for processing general data
#'
#' @param data A data frame.
#' @param ftype A string value with type of data to be plotted
#' @param agg Statistics which can be applied to all data subsets (sum, mean, median)
#' @param color_by A character with the name of the variable by which you want to color the graph. Default is NULL
#' @param ptage_col A string value with the name of the categorical variable against which the percentage is calculated.
#' @param group_extra_num A logical indicating
#'
#' @examples
#'
#' df <- sample_data("Cat-Num")
#' data_charts_prep(data = df, ftype = "Cat-Num". agg = "sum")
#'
#'
#' df <- sample_data("Cat-Cat-Num")
#' data_charts_prep(data = df, ftype = "Cat-Cat-Num", agg = "mean")
#'
#' @export
data_charts_prep <- function (data,
                              ftype,
                              agg,
                              color_by = NULL,
                              ptage_col = NULL,
                              drop_na = FALSE,
                              na_label = "na",
                              drop_na_legend = TRUE,
                              sort_opts = NULL,
                              slice_n = NULL,
                              palette = NULL,
                              highlight_value = NULL,
                              highlight_value_color = NULL,
                              order_legend = NULL,
                              order = NULL,
                              label_wrap_legend = NULL,
                              label_wrap = NULL,
                              group_extra_num = TRUE) {


  if (is.null(data)) return()


  f <- homodatum::fringe(data)
  nms <- homodatum::fringe_labels(f)
  nms[length(nms)+1] <- c("%")
  names(nms) <- c(names(nms)[-length(nms)], "..percentage")
  nms[length(nms)+1] <- c("Count")
  names(nms) <- c(names(nms)[-length(nms)], "..count")
  d <- homodatum::fringe_d(f)

  frtype <- f$frtype
  dic <- f$dic
  dic$id <- names(d)

  dic <- dic %>%
    dplyr::bind_rows(
      data.frame(id = c("..percentage", "..count", "value"),
                 label = c("Percentage", "Count", "Domain"),
                 hdType = rep("Num", 3), stringsAsFactors = FALSE)
    )



  # dictionary and data preparation when variable is yea or pct -------------

  if (grepl("Pct", frtype)) {
    dic$hdType[dic$hdType == "Pct"] <- "Num"
    frtype <- gsub("Pct", "Num", frtype)
  }

  if (grepl("Yea", frtype)) {
    has_year <- dic$id[dic$hdType == "Yea"]
    #if (any(is.na(d[has_year]))) {
    d[[has_year]] <- as.character(d[[has_year]])
  }


  # detection of variable types ---------------------------------------------


  ncols_d <- ncol(d)

  ftype_vec <- stringr::str_split(ftype,pattern = "-") %>% unlist()
  ftype_length <- length(ftype_vec)

  add_cols <- ncols_d != ftype_length

  dd <- d[,1:ftype_length]
  dic_p <- dic %>% dplyr::filter(id %in% names(dd))


  # type data to work
  has_num <- grepl("Num", ftype)
  var_num <- NULL
  agg_var <- "..count"
  if (has_num) {
    var_num <- dic_p %>% dplyr::filter(hdType %in% "Num") %>% .$id
    agg_var <- names(nms)[grep("Num", ftype_vec)]
  }

  has_cat <- grepl("Cat|Gnm|Gcd|Yea|Dat", ftype)
  var_cat <- NULL
  if (has_cat) var_cat <- dic_p %>% dplyr::filter(hdType %in% c("Cat", "Gcd", "Gnm", "Yea", "Dat")) %>% .$id

  has_dat <- grepl("Dat", ftype)
  var_dat <- NULL
  if (has_dat) var_dat <- dic_p %>% dplyr::filter(hdType %in% "Dat") %>% .$id

  # cases where the data does not contain categories
  if (!has_cat) {
    if (length(var_num) == 1) {
      stop("the data must contain another numerical or categorical variable")
    } else {
      dd <- dd %>% drop_na()
    }
  }

  # case where the data contains categories
  if (has_cat) {
    if (length(var_cat) == 1) {
      dd <- dsvizprep::function_agg(dd, agg, to_agg = var_num, a)
      ptage_col <- NULL
    } else if (length(var_cat) == 2) {
      dd <- dsvizprep::function_agg(dd, agg, to_agg = var_num, a, b)
      dd <- dsvizprep::preprocessData(dd, drop_na = drop_na_legend,
                                      na_label = na_label, na_label_cols = "a")
    } else if (length(var_cat) == 3) {
      dd <- dsvizprep::function_agg(dd, agg, to_agg = var_num, a, b, c)
    }
  }



  # percentage calculation
  if (!is.null(ptage_col))  ptage_col <- names(nms[match(ptage_col, nms)])
  dd <- dsvizprep::percentage_data(dd, agg_var = agg_var, by_col = ptage_col)


  # add extra columns
  if (add_cols) {
    join_cols <- dic_p$id[1:length(var_cat)]
    extra_cols <- setdiff(dic$id, c(dic_p$id, "..percentage", "..count", "value"))
    dj <- d[c(join_cols, extra_cols)]

    # extra num cols
    dic_extra <- dic %>% dplyr::filter(id %in% extra_cols)
    var_num_extra <- dic_extra$id[dic_extra$hdType == "Num"]
    var_cat_extra <- dic_extra$id[dic_extra$hdType == "Cat"]
    if (!identical(var_cat_extra, character())) {
      dic$hdType[dic$id %in% var_cat_extra] <- "Cat.."
    }

    if (!identical(var_num_extra, character())) {
      dic$hdType[dic$id %in% var_num_extra] <- "Cat.."
    }

    if (length(join_cols) == 1) {
      dj <- dsvizprep::collapse_data(dj, a)
    } else if (length(join_cols) == 2) {
      dj <- dsvizprep::collapse_data(dj, a, b)
    } else if (length(join_cols) == 3) {
      dj <- dsvizprep::collapse_data(dj, a, b, c)
    }

    dd <- dd %>% dplyr::left_join(dj, by = join_cols)

  }

  # preproccess in variables disticst to dates
  if (!has_dat) {
    if (has_cat) {
      dd <- dsvizprep::preprocessData(dd, drop_na, na_label, na_label_cols = var_cat)
      dd <- dsvizprep::postprocess(dd, col = agg_var, sort = sort_opts, slice_n = slice_n)
    }
  }

  dd$value <- dd[[agg_var]]

  if (!is.null(color_by)) color_by <- names(nms[match(color_by, nms)])
  if (length(var_cat) == 2) color_by <- "a"



  if ("color" %in% dic$hdType) {
    dd$..colors <- dd[[dic$id[dic$hdType == "color"][1]]]
  } else {
    dd$..colors <- paletero::map_colors(dd, color_by, palette, colors_df = NULL)
  }


  if (!is.null(highlight_value)) {
    if (sum(grepl("Dat|Cat|Yea", ftype_vec)) == 2) dd$..colors <- palette[1]
    w <- grep(paste0(highlight_value, collapse = '|'), dd[[color_by %||% "a"]])
    dd$..colors[w] <- highlight_value_color
  }


  # order -------------------------------------------------------------------


  if (sum(grepl("Dat|Cat|Yea", ftype_vec)) == 1) {
    if (!grepl("Dat", ftype)) {
      dd <- dsvizprep::order_category(dd, col = "a", order = order, label_wrap = label_wrap)
    }
  }

  if (sum(grepl("Dat|Cat|Yea", ftype_vec)) == 2) {
    dd <- dsvizprep::order_category(dd, col = "a", order = order_legend, label_wrap = label_wrap_legend)

    if (!grepl("Dat", frtype)) {
      dd <- dsvizprep::order_category(dd, col = "b", order = order, label_wrap = label_wrap)
    }
  }

  l <- list(
    data = dd,
    dic = dic,
    nms = nms#,
    #nms_tooltip = nms_tooltip #default tooltip when this is null
  )
  l

}
