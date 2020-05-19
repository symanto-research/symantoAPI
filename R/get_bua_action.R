#' Action seeking
#'
#' @aliases get_bua_action
#'
#' @description
#' Analysis of the author's intention to create an impact or cause effect 
#' or to exert an influence. Through a request, a direct opinion or an 
#' indirect suggestion, the author expects their message to prompt action.
#'
#' @usage
#' get_bua_action(text, language, all = FALSE, encoding = "UTF-8", token)
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
#'   \item action Checks whether the author seeks to create an 
#'   impact (\emph{yes}) or not (\emph{no}).
#'   \item probab Probability of seeking an action.
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
#' text <- "We cannot accept the government decisions. We must do something."
#' language <- "en"
#' get_bua_action(text, language, TRUE, "UTF-8", token)
#' get_bua_action(text, language, FALSE, "UTF-8", token)
#' 
#' # German:
#' text <- "Die Regierungsentscheidungen sind inakzeptabel. Man muss etwas machen."
#' language <- "de"
#' get_bua_action(text, language, TRUE, "UTF-8", token)
#' }
#' 
#' @export

get_bua_action <- function(text, language, all = FALSE, encoding = "UTF-8", token) {
  url <- paste("https://api.symanto.net/api/v1/bua-action?all=", tolower(all),
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
                        paste("action_pred", 1:2, sep = ""), 
                        paste("action_probab", 1:2, sep = "")) 
    info[grep("probab", names(info))] <- round(as.numeric(info[grep("probab", 
                                                                    names(info))]), 3)
  }else{
    colnames(info) <- c("text", "action_pred", "action_probab")
    info$action_probab <- round(as.numeric(info$action_probab), 3)
  }
  
  return(info)
}
