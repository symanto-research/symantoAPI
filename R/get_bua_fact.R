#' Fact oriented
#'
#' @aliases get_bua_fact
#'
#' @description
#' Analysis of the author's intention to express objective and factual information.
#'
#' @usage
#' get_bua_fact(text, language, all = FALSE, encoding = "UTF-8", token)
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
#'   \item fact Checks whether the author expresses 
#'   objective facts (\emph{yes}) or not (\emph{no}).
#'   \item probab Probability of factual information.
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
#' text <- "Messi is the world's best soccer player."
#' language <- "en"
#' get_bua_fact(text, language, TRUE, "UTF-8", token)
#' get_bua_fact(text, language, FALSE, "UTF-8", token)
#' 
#' # German:
#' text <- "Messi ist der beste Fussballspieler der Welt."
#' language <- "de"
#' get_bua_fact(text, language, TRUE, "UTF-8", token)
#' }
#' 
#' @export

get_bua_fact <- function(text, language, all = FALSE, encoding = "UTF-8", token) {
  url <- paste("https://api.symanto.net/api/v1/bua-fact?all=", tolower(all),
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
                        paste("fact_pred", 1:2, sep = ""), 
                        paste("fact_probab", 1:2, sep = "")) 
    info[grep("probab", names(info))] <- round(as.numeric(info[grep("probab", 
                                                                    names(info))]), 3)
  }else{
    colnames(info) <- c("text", "fact_pred", "fact_probab")
    info$fact_probab <- round(as.numeric(info$fact_probab), 3)
  }
  
  return(info)
}
