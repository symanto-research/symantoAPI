#' Information seeking
#'
#' @aliases get_bua_info
#'
#' @description
#' Analysis of the author's intention to show an active interest in a subject. 
#' The message aims to extend knowledge through filling some knowledge gap, 
#' receive a reply or reassurance to the communication.
#'
#' @usage
#' get_bua_info(text, language, all = FALSE, encoding = "UTF-8", token)
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
#'   \item info Checks whether the author shows an active 
#'   interest in a subject (\emph{yes}) or not (\emph{no}).
#'   \item probab Probability of information seeking.
#' }
#'
#' @author
#' Symanto
#' 
#' @note 
#' The information requested is not limited to facts.
#' 
#' @seealso
#' \code{\link{get_bua}}
#'
#' @examples
#' \dontrun{
#' token <- "Request your API Key: https://developer.symanto.net/"
#' # English:
#' text <- "I am interested in good literature."
#' language <- "en"
#' get_bua_info(text, language, TRUE, "UTF-8", token)
#' get_bua_info(text, language, FALSE, "UTF-8", token)
#' 
#' # German:
#' text <- "Ich habe ein Interesse an guter Literatur."
#' language <- "de"
#' get_bua_info(text, language, TRUE, "UTF-8", token)
#' }
#' 
#' @export

get_bua_info <- function(text, language, all = FALSE, encoding = "UTF-8", token) {
  url <- paste("https://api.symanto.net/api/v1/bua-info?all=", tolower(all),
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
                        paste("info_pred", 1:2, sep = ""), 
                        paste("info_probab", 1:2, sep = "")) 
    info[grep("probab", names(info))] <- round(as.numeric(info[grep("probab", 
                                                                    names(info))]), 3)
  }else{
    colnames(info) <- c("text", "info_pred", "info_probab")
    info$info_probab <- round(as.numeric(info$info_probab), 3)
  }
  
  return(info)
}
