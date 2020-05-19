#' Understand Basic User Aspects (BUA)
#'
#' @aliases get_bua
#'
#' @description
#' Understanding of the basic user aspects of the author. This is an aggregation call 
#' for all the other BUA function calls. The BUAs include personality, self revealing,
#' information seeking, action seeking and fact oriented.
#'
#' @usage
#' get_bua(text, language, all = FALSE, encoding = "UTF-8", token)
#'
#' @param text String with the text.
#' @param language String with the language. For the time being, the supported 
#' language is "en" (English). 
#' @param all Boolean. If TRUE, all the BUA probabilities
#' are displayed. If FALSE, only the largest probability is displayed.
#' @param encoding Character encoding. Default "UTF-8".
#' @param token The access token.
#'
#' @return
#' A data frame with columns:
#' \enumerate{
#'   \item text Text analyzed.
#'   \item bua BUAs that the text conveys.
#'   \item probab Probability of the emotion returned.
#' }
#' 
#' @details 
#' \itemize{
#'   \item The personality traits are \emph{emotional} or \emph{rational}.
#'   \item The self revealing checks whether the author shares personal
#'         feelings (\emph{yes}) or not (\emph{no}).
#'   \item The information seeking checks whether the author shows an active 
#'         interest in a subject (\emph{yes}) or not (\emph{no}).
#'   \item The action seeking checks whether the author seeks to create an 
#'         impact (\emph{yes}) or not (\emph{no}).
#'   \item The fact oriented checks whether the author expresses 
#'         objective facts (\emph{yes}) or not (\emph{no}).
#' }       
#'
#' @author
#' Symanto
#' 
#' @seealso
#' \code{\link{get_bua_action}}, \code{\link{get_bua_fact}},
#' \code{\link{get_bua_self}}, \code{\link{get_bua_personality}},
#' \code{\link{get_bua_info}}
#'
#' @examples
#' \dontrun{
#' token <- "Request your API Key: https://developer.symanto.net/"
#' # English:
#' text <- "I am very happy"
#' language <- "en"
#' get_bua(text, language, all = TRUE, "UTF-8", token)
#' get_bua(text, language, all = FALSE, "UTF-8", token)
#' }
#' 
#' @export

get_bua <- function(text, language, all = FALSE, encoding = "UTF-8", token) {
  url <- paste("https://api.symanto.net/api/v1/bua?all=", tolower(all),
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
                       paste("personality_probab", 1:2, sep = ""), 
                       paste("self_pred", 1:2, sep = ""),
                       paste("self_probab", 1:2, sep = ""), 
                       paste("info_pred", 1:2, sep = ""),
                       paste("info_probab", 1:2, sep = ""), 
                       paste("action_pred", 1:2, sep = ""),
                       paste("action_probab", 1:2, sep = ""), 
                       paste("fact_pred", 1:2, sep = ""),
                       paste("fact_probab", 1:2, sep = "")) 
   info[grep("probab", names(info))] <- round(as.numeric(info[grep("probab", 
                                                                   names(info))]), 3)
  }else{
    colnames(info) <- c("text", 
                        "personality_pred", 
                        "personality_probab",
                        "self_pred",
                        "self_probab",
                        "info_pred",
                        "info_probab",
                        "action_pred",
                        "action_probab",
                        "fact_pred",
                        "fact_probab")
    info[grep("probab", names(info))] <- round(as.numeric(info[grep("probab", 
                                                                    names(info))]), 3)
  }
  
  return(info)
}
