#' Language detection
#'
#' @aliases get_language_detection
#'
#' @description
#' This function identifies the language of the input text. 
#' Any language can be detected.
#'
#' @usage
#' get_language_detection(url_api, text, encoding = "UTF-8", token)
#'
#' @param url_api URL to the API.
#' @param text String with the texts. A set of maximum 64 texts is allowed.
#' @param encoding Character encoding. Default "UTF-8".
#' @param token The access token.
#' 
#' @note 
#' The type of array for one text is: \cr
#' data <- "[{\"id\":\"1\",\"text\":\"love\",\"language\":\"en\"}]" \cr
#' For more than one text is: \cr
#' data <- "[{\"id\":\"1\",\"text\":\"love\",\"language\":\"en\"}, 
#' \cr
#'           {\"id\":\"2\",\"text\":\"hate\",\"language\":\"en\"}]" \cr
#' (write it in one line). 
#' In the first case, add a curly bracket after the first box bracket and 
#' before the last box bracket. In the second case, add the curly brackets
#' for every set of id, text and language.
#'
#' @return
#' A data frame with the text and the language in which the text
#' was written.
#'
#' @author
#' Symanto
#'
#' @examples
#' \dontrun{
#' token <- "Request your API Key: https://developers.symanto.net/signup"
#' url_api <- "https://api.symanto.net"
#' 
#' # For only one text:
#' # English:
#' text <- "Hello I love the service."
#' get_language_detection(url_api, text, "UTF-8", token)
#' 
#' # German:
#' text <- "Ich bin sehr zufrieden."
#' get_language_detection(url_api, text, "UTF-8", token)
#' 
#' # Spanish:
#' text <- "Estoy muy contento."
#' get_language_detection(url_api, text, "UTF-8", token)
#' 
#' # For more than one text:
#' # English:
#' text <- c("love", "hate", "felicidad")
#' get_language_detection(url_api, text, "UTF-8", token)
#' }
#' 
#' @export

get_language_detection <- function(url_api, text, encoding = "UTF-8", token) {
  text <- gsub("\\\"", "'", text)
  # More special characters (\ < > |):
  spec_char <- "\\\\|<|>|\\|"
  text <- gsub(spec_char, "", text)
  
  url <- paste(url_api,  "/language-detection?&api_key=", token, sep = "")
  
  headers <- c(`Content-Type` = 'application/json')
  
  data_aux <- paste("{\"id\":\"", text, 
                    "\",\"text\":\"", text, "\"}", 
                    sep = "", collapse = ",")
  data <- paste("[", data_aux, "]", sep = "")
  
  response <- POST(url = url, add_headers(.headers = headers), body = data)
  
  if (response$status_code != 200) {
    info <- NULL
  }else{
    text_info <- content(response, "text", encoding = encoding)
    info <- fromJSON(text_info, flatten = TRUE)
  }
  colnames(info)[1] <- "text"
  
  return(info)
}
