#' Topic sentiments
#'
#' @aliases get_topic_sentiment
#'
#' @description
#' For a text written in a given language, this function detects topics and 
#' the topic category of each topic. Then, it analyzes the sentiment towards 
#' each of the topics mentioned.
#'
#' @usage
#' get_topic_sentiment(url_api, text, language, encoding = "UTF-8", token)
#'
#' @param url_api URL to the API.
#' @param text String with the texts.
#' @param language Language of the text. Supported languages are 
#' "en" (English) and "de" (German). 
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
#' A data frame with columns related to the topicSentiments, sentiments
#' and topics.
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
#' language <- "en"
#' gts_en <- get_topic_sentiment(url_api, text, language, "UTF-8", token)
#' 
#' # German:
#' text <- "Ich bin sehr zufrieden."
#' language <- "de"
#' gts_de <- get_topic_sentiment(url_api, text, language, "UTF-8", token)
#' 
#' text <- "Ich lese gerade 'Das Buch des Vaters' von Urs Widmer. 
#'          Interessantes Buch. Empfehlenswert"
#' language <- "de"
#' gts_de_urs <- get_topic_sentiment(url_api, text, language, "UTF-8", token)
#' 
#' # For more than one text:
#' # English:
#' df <- data.frame(text = c("love", "hate"), language = c("en", "en"))
#' text <- df$text
#' language <- df$language
#' gts_all <- get_topic_sentiment(url_api, text, language, "UTF-8", token)
#' }
#' 
#' @importFrom data.table rbindlist
#' @importFrom dplyr select filter matches
#' 
#' @export

get_topic_sentiment <- function(url_api, text, language, encoding = "UTF-8", token) {
  id <- NULL
  
  info_all <- data.frame()
  # I have to iterate because for the case of several texts, I haven't found
  # the way of joining them in a general data frame, since the json file
  # does not have always the same number of elements.
  for (i in 1:length(text)) {
    text_iter <- text[i]
    lang_iter <- language[i]
    
    text_iter <- gsub("\\\"", "'", text_iter)
    # More special characters (\ < > |):
    spec_char <- "\\\\|<|>|\\|"
    text_iter <- gsub(spec_char, "", text_iter)
    
    url <- paste(url_api, "/topic-sentiment?&api_key=", token, sep = "")
    
    headers <- c(`Content-Type` = 'application/json')
    
    data_aux <- paste("{\"id\":\"", text_iter, 
                      "\",\"text\":\"", text_iter, 
                      "\",\"language\":\"", lang_iter, "\"}", 
                      sep = "", collapse = ",")
    data <- paste("[", data_aux, "]", sep = "")
    
    response <- POST(url = url, add_headers(.headers = headers), body = data)
    
    if (response$status_code != 200) {
      info <- NULL
    }else{
      text_info <- content(response, "text", encoding = encoding)
      json_info <- fromJSON(text_info, flatten = TRUE)
      
      if (length(json_info$topicSentiments[[1]]) != 0) {
        if (nrow(json_info$topicSentiments[[1]]) > 1) {
          json_info$topicSentiments[[1]] <- apply(json_info$topicSentiments[[1]], 2, 
                                                  paste, collapse = ", " ) 
        } 
      }
      
      if (length(json_info$sentiments[[1]]) != 0) {
        if (nrow(json_info$sentiments[[1]]) > 1) {
          json_info$sentiments[[1]] <- apply(json_info$sentiments[[1]], 2, 
                                             paste, collapse = ", " ) 
        }
      }
      
      if (length(json_info$topics[[1]]) != 0) {
        if (nrow(json_info$topics[[1]]) > 1) {
          json_info$topics[[1]] <- apply(json_info$topics[[1]], 2, 
                                         paste, collapse = ", " ) 
        }
      }
      
      # For more than one text, this is providing messy results that cannot
      # be converted in a general data frame.
      #info_aux <- as.data.frame(unnest(json_info, 
      #                                 c(topicSentiments, topics, sentiments),
      #                                 keep_empty = TRUE,
      #                                 names_repair = "minimal"))
      
      
      info_aux<- as.data.frame(t(unlist(json_info)), stringsAsFactors = FALSE)
      colnames(info_aux) <- gsub("\\.", "_", colnames(info_aux))
      
      info <- info_aux %>%
        select(-id, -language, -matches("sentence")) %>%
        filter(!duplicated(text))
    }
    
    # https://www.biostars.org/p/385180/
    info_all <- rbindlist(list(info_all, info), fill = TRUE)
  }
  
  return(info_all)
}