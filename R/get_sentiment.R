#' Get sentiment from a text
#'
#' @aliases get_sentiment
#'
#' @description
#' For a text written in a given language, this function extracts the sentiment 
#' it conveys. Returned labels are "positive" and "negative".
#'
#' @usage
#' get_sentiment(text, language, all = FALSE, encoding = "UTF-8", token)
#'
#' @param text String with the text.
#' @param language String with the language. Supported languages are 
#' "en" (English), "de" (German) and "es" (Spanish). 
#' @param all Boolean. If TRUE, both the positive and negative probabilities 
#' are displayed. If FALSE, only the largest probability is displayed.
#' @param encoding Character encoding. Default "UTF-8".
#' @param token The access token.
#'
#' @return
#' A data frame with columns:
#' \enumerate{
#'   \item text Text analyzed.
#'   \item sentiment Sentiment that the text conveys.
#'   \item probab Probability of the sentiment returned.
#' }
#'
#' @author
#' Symanto
#'
#' @examples
#' \dontrun{
#' token <- "Request your API Key: https://developer.symanto.net/"
#' # English:
#' text <- "I am very happy"
#' language <- "en"
#' get_sentiment(text, language, TRUE, "UTF-8", token)
#' get_sentiment(text, language, FALSE, "UTF-8", token)
#' 
#' # German:
#' text <- "Ich bin sehr zufrieden"
#' language <- "de"
#' get_sentiment(text, language, TRUE, "UTF-8", token)
#' 
#' # Spanish:
#' text <- "Estoy muy contento"
#' language <- "es"
#' gs <- get_sentiment(text, language, TRUE, "UTF-8", token)
#' gs
#' # Probabilities sum indeed one:
#' sum(gs[grepl("prob", names(gs))])
#' }
#' 
#' @export

get_sentiment <- function(text, language, all = FALSE, encoding = "UTF-8", token) {
  url <- paste("https://api.symanto.net/api/v1/sentiment?all=", tolower(all),
               "&api_key=", token, sep = "")
  
  headers <- c(`Content-Type` = 'application/json')
  
  data <- paste("[{\"id\":\"", text, "\",\"language\":\"", language,  
                "\",\"text\":\"", text, "\"}]", sep = "")
  
  response <- POST(url = url, add_headers(.headers = headers), body = data)
  text_info <- content(response, "text", encoding = encoding)
  json_info <- fromJSON(text_info, flatten = TRUE)
  
  info <- as.data.frame(t(unlist(json_info)), stringsAsFactors = FALSE)
  
  if (all) {
    if (language %in% c("en", "de")) {
      colnames(info) <- c("text", paste("sentiment", 1:2, sep = ""), 
                          paste("probab", 1:2, sep = "")) 
      info$probab1 <- round(as.numeric(info$probab1), 3)
      info$probab2 <- round(as.numeric(info$probab2), 3)
    }else{
      colnames(info) <- c("text", paste("sentiment", 1:3, sep = ""), 
                          paste("probab", 1:3, sep = ""))
      info$probab1 <- round(as.numeric(info$probab1), 3)
      info$probab2 <- round(as.numeric(info$probab2), 3)
      info$probab3 <- round(as.numeric(info$probab3), 3)
    }
  }else{
    colnames(info) <- c("text", "sentiment", "probab")
    info$probab <- round(as.numeric(info$probab), 3)
  }
  
  return(info)
}
