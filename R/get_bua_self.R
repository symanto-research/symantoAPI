#' Self revealing
#'
#' @aliases get_bua_self
#'
#' @description
#' Analysis of the author's intention to share personal preferences, 
#' experiences and feelings.
#'
#' @usage
#' get_bua_self(text, language, all = FALSE, encoding = "UTF-8", token)
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
#'   \item self Checks whether the author shares personal
#'   feelings (\emph{yes}) or not (\emph{no}).
#'   \item probab Probability of self revealing.
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
#' text <- "Joker is a great movie."
#' language <- "en"
#' get_bua_self(text, language, TRUE, "UTF-8", token)
#' get_bua_self(text, language, FALSE, "UTF-8", token)
#' 
#' # German:
#' text <- "Joker ist ein grossartiger Film."
#' language <- "de"
#' get_bua_self(text, language, TRUE, "UTF-8", token)
#' }
#' 
#' @export

get_bua_self <- function(text, language, all = FALSE, encoding = "UTF-8", token) {
  url <- paste("https://api.symanto.net/api/v1/bua-self?all=", tolower(all),
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
                        paste("self_pred", 1:2, sep = ""), 
                        paste("self_probab", 1:2, sep = "")) 
    info[grep("probab", names(info))] <- round(as.numeric(info[grep("probab", 
                                                                    names(info))]), 3)
  }else{
    colnames(info) <- c("text", "self_pred", "self_probab")
    info$self_probab <- round(as.numeric(info$self_probab), 3)
  }
  
  return(info)
}
