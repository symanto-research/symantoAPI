#' Communication style
#'
#' @aliases get_communic_style
#'
#' @description
#' This function detects the communication purpose and style of the text. 
#' The style includes: 
#' \enumerate{
#' \item \emph{self_revealing} (sharing one's experience and opinion). 
#' \item \emph{fact_oriented} (focusing on factual information, objective observations 
#' or statements). 
#' \item \emph{information_seeking} (posing questions). 
#' \item \emph{action_seeking} (aiming to trigger someone's action by giving 
#' recommendation, requests or advice).
#' }
#'
#' @usage
#' get_communic_style(url_api, text, language, all = FALSE, encoding = "UTF-8", token)
#'
#' @param url_api URL to the API.
#' @param text String with the texts. A set of maximum 32 texts is allowed.
#' @param language Language of the text. Supported languages are
#' "ar" (Arabic), "de" (German), "en" (English), "es" (Spanish), "fr" (French), 
#' "it" (Italian), "nl" (Dutch), "pt" (Portuguese), "ru" (Russian), "tr" (Turkish)
#'  and "zh" (Chinese).
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
#' get_communic_style(url_api, text, language, all = TRUE, "UTF-8", token)
#' get_communic_style(url_api, text, language, all = FALSE, "UTF-8", token)
#' 
#' # German:
#' text <- "Menschen langweilen sich zu Hause."
#' language <- "de"
#' get_communic_style(url_api, text, language, TRUE, "UTF-8", token)
#' get_communic_style(url_api, text, language, FALSE, "UTF-8", token)
#' 
#' # Spanish:
#' text <- "Estoy muy contento."
#' language <- "es"
#' get_communic_style(url_api, text, language, TRUE, "UTF-8", token)
#' get_communic_style(url_api, text, language, FALSE, "UTF-8", token)
#' 
#' # For more than one text:
#' # English:
#' df <- data.frame(text = c("love", "hate"), language = c("en", "en"))
#' text <- df$text
#' language <- df$language
#' get_communic_style(url_api, text, language, all = TRUE, "UTF-8", token)
#' get_communic_style(url_api, text, language, all = FALSE, "UTF-8", token)
#' }
#' 
#' @importFrom tidyr unnest pivot_wider
#' @importFrom dplyr mutate rename
#' 
#' @export

get_communic_style <- function(url_api, text, language, all = FALSE, 
                               encoding = "UTF-8", token) {
  predictions <- probability <- prediction <- id <- NULL
  
  text <- gsub("\\\"", "'", text)
  # More special characters (\ < > |):
  spec_char <- "\\\\|<|>|\\|"
  text <- gsub(spec_char, "", text)
  
  #url <- paste("https://api.symanto.net/communication?all=", 
  #             tolower(all), "&api_key=", token, sep = "")
  url <- paste(url_api, "/communication?all=", tolower(all), "&api_key=", token, sep = "")
  
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
    
    colnames(info)[-1] <- gsub("-", "_", colnames(info)[-1])
  }
  
  return(info)
}
