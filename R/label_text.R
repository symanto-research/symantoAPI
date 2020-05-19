#' Label texts
#'
#' @aliases label_text
#'
#' @description
#' Given a certain (Excel) file with some posts and all the empty columns related 
#' to sentiments, emotions and Basic User Aspects (BUAs), this function
#' fills the columns that the user wants to.
#'
#' @usage
#' label_text(excel_file, user_actions, encoding = "UTF-8", token, verbose = FALSE)
#'
#' @param excel_file Excel file.
#' @param user_actions Vector with the set of columns (psychological properties) 
#' that the user wants to fill in. Possible values are "sentiments", "emotions", 
#' "action_seeking", "fact_oriented", "self_revealing", "personality" and 
#' "information_seeking".
#' @param encoding Character encoding. Default "UTF-8".
#' @param token The access token.
#' @param verbose Provides a flow of information.
#'
#' @return
#' The \code{excel_file} filled with the probablities related to the 
#' psychological properties desired by the user.
#' 
#' @details 
#' \itemize{
#'   \item The sentiments are \emph{positive} and \emph{negative}.
#'   \item The emotions are \emph{anger}, \emph{joy}, \emph{love}, 
#'   \emph{sadness} and \emph{unrecognized}.
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
#' \code{\link{get_emotion}}, \code{\link{get_sentiment}},
#' \code{\link{get_bua_action}}, \code{\link{get_bua_fact}},
#' \code{\link{get_bua_self}}, \code{\link{get_bua_personality}},
#' \code{\link{get_bua_info}}
#'
#' @examples
#' \dontrun{
#' #library(openxlsx)
#' token <- "Request your API Key: https://developer.symanto.net/"
#' 
#' # All actions:
#' #user_actions <- c("sentiments", "emotions", "action_seeking", "fact_oriented", 
#' #                  "self_revealing", "personality", "information_seeking")  
#' #excel_dest_l1 <- label_text(excel_orig, user_actions, "UTF-8", token, TRUE)
#' #write.xlsx(excel_dest_l1, "excel_dest_l1.xlsx")
#' 
#' # A sample of actions:
#' user_actions <- c("sentiments", "fact_oriented", "personality")  
#' excel_dest_l2 <- label_text(excel_orig, user_actions, "UTF-8", token, TRUE)
#' #write.xlsx(excel_dest_l2, "excel_dest_l2.xlsx")
#' }
#' 
#' @export

label_text <- function(excel_file, user_actions, encoding = "UTF-8", token, verbose = FALSE) {
  excel_file$text <- gsub("\\\"", "'", excel_file$text)
  df0 <- excel_file
       
  if ("sentiments" %in% user_actions) {
    if (verbose) {
      print("Sentiments:")
    }
    for (i in 1:nrow(df0)) {
      if (verbose) {
        print(i)
      }
      gs <- get_sentiment(df0$text[i], df0$language[i], TRUE, encoding, token)
      df0[i, gs$sentiment1] <- gs$probab1
      df0[i, gs$sentiment2] <- gs$probab2
    }
    if (verbose) {
      print("Sentiments done!")
      print("---")
    }
  }

  if ("emotions" %in% user_actions) {
    if (verbose) {
      print("Emotions:")
    }
    for (i in 1:nrow(df0)) {
      if (verbose) {
        print(i)
      }
      ge <- get_emotion(df0$text[i], df0$language[i], TRUE, encoding, token)
      df0[i, ge$emotion1] <- ge$probab1
      df0[i, ge$emotion2] <- ge$probab2
      df0[i, ge$emotion3] <- ge$probab3
      df0[i, ge$emotion4] <- ge$probab4
      df0[i, ge$emotion5] <- ge$probab5
      df0[i, ge$emotion6] <- ge$probab6
    }
    if (verbose) {
      print("Emotions done!")
      print("---")
    }
  }

  if ("action_seeking" %in% user_actions) {
    if (verbose) {
      print("Action seeking:")
    }
    for (i in 1:nrow(df0)) {
      if (verbose) {
        print(i)
      }
      gba <- get_bua_action(df0$text[i], df0$language[i], FALSE, encoding, token)
      df0[i, "action"] <- paste(gba$action_pred, " (", 
                                gba$action_probab, ")", sep = "")
    }
    if (verbose) {
      print("Action seeking done!")
      print("---")
    }
  }

  if ("fact_oriented" %in% user_actions) {
    if (verbose) {
      print("Fact oriented:")
    }
    for (i in 1:nrow(df0)) {
      if (verbose) {
        print(i)
      }
      gbf <- get_bua_fact(df0$text[i], df0$language[i], FALSE, encoding, token)
      df0[i, "fact"] <- paste(gbf$fact_pred, " (", 
                              gbf$fact_probab, ")", sep = "")
    }
    if (verbose) {
      print("Fact oriented done!")
      print("---")
    }
  }
  
  if ("self_revealing" %in% user_actions) {
    if (verbose) {
      print("Self revealing:")
    }
    for (i in 1:nrow(df0)) {
      if (verbose) {
        print(i)
      }
      gbs <- get_bua_self(df0$text[i], df0$language[i], FALSE, encoding, token)
      df0[i, "self"] <- paste(gbs$self_pred, " (", 
                              gbs$self_probab, ")", sep = "")
    }
    if (verbose) {
      print("Self revealing done!")
      print("---")
    }
  }  
  
  if ("personality" %in% user_actions) {
    if (verbose) {
      print("Personality:")
    }
    for (i in 1:nrow(df0)) {
      if (verbose) {
        print(i)
      }
      gbp <- get_bua_personality(df0$text[i], df0$language[i], FALSE, encoding, token)
      df0[i, "personality"] <- paste(gbp$personality_pred, " (", 
                                     gbp$personality_probab, ")", sep = "")
    }
    if (verbose) {
      print("Personality done!")
      print("---")
    }
  }
  
  if ("information_seeking" %in% user_actions) {
    if (verbose) {
      print("Information seeking:")
    }
    for (i in 1:nrow(df0)) {
      if (verbose) {
        print(i)
      }
      gbi <- get_bua_info(df0$text[i], df0$language[i], FALSE, encoding, token)
      df0[i, "info"] <- paste(gbi$info_pred, " (", 
                              gbi$info_probab, ")", sep = "")
    }
    if (verbose) {
      print("Information seeking done!")
      print("---")
    }
  }
  
  if (any(!user_actions %in% c("sentiments", "emotions", "action_seeking",
                           "fact_oriented", "self_revealing",
                           "personality", "information_seeking"))) {
    stop("Please be sure that the user actions are 'sentiments', 'emotions',
         'action_seeking', 'fact_oriented', 'self_revealing',
         'personality' or 'information_seeking'")
  }
  
  return(df0)
}  