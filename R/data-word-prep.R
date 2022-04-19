#' Clean text to plot wordCloud
#'
#' @param data A data frame with geografical info.
#' @param ftype A string value with type of data to be plotted
#' @export
data_word_prep <- function(data, ftype = NULL) {

  if (any(class(data) %in% c("data.frame", "fringe", "tbl_df","tbl"))) {
   if (ftype == "Cat") {
   data <- paste0(data[[1]], sep = " ", collapse = " ")
   }
  }
 stopwords <- readr::read_csv(paste0(
                               path.package("dsvizprep"), "/inst/resources/stopwords.csv"
                                    )
                              )
  if (any(class(data) %in% "character")) {
    if (is.null(ftype)) ftype <- "Cat"
    data <- gsub("[[:punct:]]", "", data)
    data <- gsub("[[:digit:]]", "", data)
    data <- gsub("\\s+", " ", data)
    words <- data.frame(words = tokenizers::tokenize_words(data) %>% unlist())
    data <- anti_join(words, stopwords, by = c("words" = "STOPWORD"))
  }
  data
}

