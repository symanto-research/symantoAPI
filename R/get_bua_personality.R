#' Personality
#'
#' @aliases get_bua_personality
#'
#' @description
#' Personality traits evaluate the content and writing style to determine how the 
#' author makes decisions. The model identifies whether an author is emotional 
#' (relationship-oriented, focusing on social values and empathy) or rational 
#' (objective and pragmatic, focusing on facts and logical deduction).
#'
#' @usage
#' get_bua_personality(text, language, all = FALSE, encoding = "UTF-8", token)
#'
#' @param text String with the text.
#' @param language String with the language. For the time being, the supported 
#' languages are "en" (English) and "de" (German). 
#' @param all Boolean. If TRUE, all the BUA probabilities
#' are displayed. If FALSE, only the largest probability is displayed.
#' @param encoding Character encoding. Default "UTF-8".
#' @param token The access token.
#'
#' @return
#' A data frame with columns:
#' \enumerate{
#'   \item text Text analyzed.
#'   \item personality Checks whether the personality traits are 
#'   \emph{emotional} or \emph{rational}.
#'   \item probab Probability of personality traits.
#' }
#'
#' @author
#' Symanto
#' 
#' @seealso
#' \code{\link{get_bua}}
#'
#' @examples
#' \dontrun{
#' token <- "Request your API Key: https://developer.symanto.net/"
#' # English:
#' text <- "People get bored at home."
#' language <- "en"
#' get_bua_personality(text, language, TRUE, "UTF-8", token)
#' get_bua_personality(text, language, FALSE, "UTF-8", token)
#' 
#' # German:
#' text <- "Menschen langweilen sich zu Hause."
#' language <- "de"
#' get_bua_personality(text, language, TRUE, "UTF-8", token)
#' }
#' 
#' @export

get_bua_personality <- function(text, language, all = FALSE, encoding = "UTF-8", token) {
  url <- paste("https://api.symanto.net/api/v1/bua-personality?all=", tolower(all),
               "&api_key=", token, sep = "")
  
  headers <- c(`Content-Type` = 'application/json')
  
  data <- paste("[{\"id\":\"", text, "\",\"language\":\"", language,  
                "\",\"text\":\"", text, "\"}]", sep = "")
  
  response <- POST(url = url, add_headers(.headers = headers), body = data)
  text_info <- content(response, "text", encoding = encoding)
  json_info <- fromJSON(text_info, flatten = TRUE)
  
  info <- as.data.frame(t(unlist(json_info)), stringsAsFactors = FALSE)
  
  if (all) {
    colnames(info) <- c("text", 
                        paste("personality_pred", 1:2, sep = ""), 
                        paste("personality_probab", 1:2, sep = "")) 
    info[grep("probab", names(info))] <- round(as.numeric(info[grep("probab", 
                                                                    names(info))]), 3)
  }else{
    colnames(info) <- c("text", "personality_pred", "personality_probab")
    info$personality_probab <- round(as.numeric(info$personality_probab), 3)
  }
  
  return(info)
}
