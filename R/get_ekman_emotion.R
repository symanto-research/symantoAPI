#' Ekman's emotions
#'
#' @aliases get_ekman_emotion
#'
#' @description
#' For a text written in a given language, this function extracts the Ekman's 
#' emotions that it conveys. Returned labels are \emph{anger}, \emph{disgust}, 
#' \emph{fear}, \emph{joy}, \emph{no_emotion}, \emph{sadness} and \emph{surprise}.
#'
#' @usage
#' get_ekman_emotion(url_api, text, language, all = FALSE, encoding = "UTF-8", token)
#'
#' @param url_api URL to the API.
#' @param text String with the texts. A set of maximum 512 texts is allowed.
#' @param language Language of the text. Supported languages are 
#' "en" (English), "de" (German) and "es" (Spanish). 
#' @param all Boolean. If TRUE, all the probabilities
#' are displayed. If FALSE, only the largest probability is displayed.
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
#' An explanation of the Ekman's foundations can be found 
#' \href{https://www.paulekman.com/universal-emotions/}{here.}
#'
#' @return
#' A data frame with the texts and the probabilities of the 
#' psychological features obtained.
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
#' text <- "I love the service."
#' language <- "en"
#' get_ekman_emotion(url_api, text, language, TRUE, "UTF-8", token)
#' get_ekman_emotion(url_api, text, language, FALSE, "UTF-8", token)
#' 
#' # German:
#' text <- "Ich bin sehr zufrieden."
#' language <- "de"
#' get_ekman_emotion(url_api, text, language, TRUE, "UTF-8", token)
#' get_ekman_emotion(url_api, text, language, FALSE, "UTF-8", token)
#' 
#' # Spanish:
#' text <- "Estoy muy contento."
#' language <- "es"
#' get_ekman_emotion(url_api, text, language, TRUE, "UTF-8", token)
#' get_ekman_emotion(url_api, text, language, FALSE, "UTF-8", token)
#' 
#' # For more than one text:
#' # English:
#' df <- data.frame(text = c("love", "hate"), language = c("en", "en"))
#' text <- df$text
#' language <- df$language
#' get_ekman_emotion(url_api, text, language, all = TRUE, "UTF-8", token)
#' get_ekman_emotion(url_api, text, language, all = FALSE, "UTF-8", token)
#' }
#' 
#' @export

get_ekman_emotion <- function(url_api, text, language, all = FALSE, encoding = "UTF-8", token) {
  predictions <- probability <- prediction <- id <- NULL
  
  text <- gsub("\\\"", "'", text)
  # More special characters (\ < > |):
  spec_char <- "\\\\|<|>|\\|"
  text <- gsub(spec_char, "", text)
  
  url <- paste(url_api, "/ekman-emotion?all=", tolower(all), "&api_key=", token, sep = "")
  
  headers <- c(`Content-Type` = 'application/json')
  
  data_aux <- paste("{\"id\":\"", text, 
                    "\",\"text\":\"", text, 
                    "\",\"language\":\"", language, "\"}", 
                    sep = "", collapse = ",")
  data <- paste("[", data_aux, "]", sep = "")
  
  response <- POST(url = url, add_headers(.headers = headers), body = data)
  
  if (response$status_code != 200) {
    info <- NULL
  }else{
    text_info <- content(response, "text", encoding = encoding)
    json_info <- fromJSON(text_info, flatten = TRUE)
    info_aux <- as.data.frame(unnest(json_info, predictions))
    
    # The rounding from https://biostatmatt.com/archives/2902 does not
    # sum 1 neither because the original computed probabilities do not.
    info <- info_aux %>% 
      mutate(probability = round(probability, 3)) %>%
      pivot_wider(names_from = prediction, values_from = probability) %>%
      as.data.frame() %>%
      rename(text = id)
    
    colnames(info)[-1] <- gsub("-", "_", paste(colnames(info)[-1], "_ekman", sep = ""))
  }
  
  return(info)
}
