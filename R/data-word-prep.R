#' Clean text to plot wordCloud
#'
#' @param data A data frame with geografical info.
#' @param ftype A string value with type of data to be plotted
#' @export
data_word_prep <- function(data, ftype = NULL, lang = "es", stopwords = FALSE) {

  if (any(class(data) %in% c("data.frame", "fringe", "tbl_df","tbl"))) {
   if (ftype == "Cat") {
   data <- paste0(data[[1]], sep = " ", collapse = " ")
   }
  }

  if (stopwords) {
  if (lang == "es")  stopwords <- stopwords_es$STOPWORD
  if (lang == "en")  stopwords <- stopwords_en$STOPWORD
  } else {
    stopwords <- NULL
  }

  if (any(class(data) %in% "character")) {
    if (is.null(ftype)) ftype <- "Cat"
    data <- gsub("[[:punct:]]", "", data)
    data <- gsub("[[:digit:]]", "", data)
    data <- gsub("\\s+", " ", data)
    data <- data.frame(words = tokenizers::tokenize_words(data, stopwords = stopwords) %>% unlist())
  }
  data
}

