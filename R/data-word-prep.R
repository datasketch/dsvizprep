library(rvest)
simple <- read_html("https://elpais.com/internacional/2022-03-14/gabriel-boric-propone-un-plan-global-para-resolver-la-crisis-migratoria-venezolana.html")
dd <- simple %>%
  html_nodes("p") %>%
  html_text()
class(dd)

data_word_prep <- function(data) {

  if (class(data) %in% c("data.frame", "fringe")) {
   data <- paste0(data[[1]], sep = " ", collapse = " ")
  }

  if (class(data) %in% "character") {
    data <- gsub("[[:punct:]]", "", data)
    data <- gsub("[[:digit:]]", "", data)
    data <- gsub("\\s+", " ", data)
    data <- data.frame(content = tokenizers::tokenize_words(data) %>% unlist())
  }



}

library(tokenizers)
