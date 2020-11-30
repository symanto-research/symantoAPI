#' Label texts
#'
#' @aliases label_text
#'
#' @description
#' Given a certain (Excel) file with some posts, this function assigns the 
#' psychological features that the user wants to. In the interests of computational
#' speed, the whole set of texts is splitted into smaller blocks.
#'
#' @usage
#' label_text(url_api, excel_file, user_actions, encoding = "UTF-8", token, 
#'            texts_in_blocks, user_language = NULL, verbose = FALSE)
#'
#' @param url_api URL to the API.
#' @param excel_file Excel file. It must contain at least one column with the texts,
#' named "Text" or "text". Another column with the language of the texts is 
#' optional (see the \code{user_language} parameter below). Other complementary columns 
#' can be also added to be incorporated in the returned data frame.
#' @param user_actions Vector with the set of psychological properties that the user 
#' wants to obtain. Possible values are "personality", 
#' "communication", "emotions", "ekman_emotions", "sentiments", "topic_sentiments" 
#' and "language".
#' @param encoding Character encoding. Default "UTF-8".
#' @param token The access token.
#' @param texts_in_blocks Number of texts in every block. Basically, if there are 
#' 50 texts and \code{texts_in_blocks} is 25, there will be two blocks with 25 texts. 
#' The procedure will be applied to both of them iteratively. 
#' This method is much faster than going text by text.
#' @param user_language If the language of the texts is not available, 
#' \code{\link{get_language_detection}} will be used to assign a language to 
#' every text. For the few cases where this tool fails, the assigned language is
#' this parameter provided by the user. Default NULL.
#' @param verbose Provides a flow of information.
#' 
#' @note 
#' The accepted codes for the \code{user_language} param are as follows: 
#' "ar" (Arabic), "de" (German), "en" (English), "es" (Spanish),
#' "fr" (French), "it" (Italian), "nl" (Dutch), "pt" (Portuguese),
#' "ru" (Russian), "tr" (Turkish) and "zh" (Chinese).
#' 
#' The lowest common denominator for creating blocks of texts in every function is 32, 
#' so this should be the largest possible number in \code{texts_in_blocks} when all the
#' psychological properties are selected in \code{user_actions}. However, the
#' Ekman's emotions seem not to accept this number of texts, so we recommend
#' to choose 5 if the Ekman's emotions belong to the desired \code{user_actions}.
#' 
#' Due to the internal procedure of the computational model, only blocks of texts
#' of the same language are created.
#' 
#' @details 
#' \itemize{
#'   \item The personality traits are \emph{emotional} and \emph{rational}.
#'   \item The communication style are \emph{action_seeking}, \emph{fact_oriented}, 
#'   \emph{information_seeking} and \emph{self_revealing}.
#'   \item The emotions are \emph{anger}, \emph{joy}, \emph{love}, 
#'   \emph{sadness} and \emph{surprise}.
#'   \item The Ekman's emotions are \emph{anger}, \emph{disgust}, \emph{fear}, 
#'   \emph{joy} and \emph{no_emotion}, \emph{sadness} and \emph{surprise}.
#'   \item The sentiments are \emph{positive} and \emph{negative}.
#' }
#' 
#' @return
#' The \code{excel_file} filled with the probablities related to the 
#' psychological properties desired by the user.
#'
#' @author
#' Symanto
#' 
#' @seealso
#' \code{\link{get_communic_style}}, \code{\link{get_emotion}}, 
#' \code{\link{get_ekman_emotion}}, \code{\link{get_language_detection}}, 
#' \code{\link{get_personality}}, \code{\link{get_sentiment}}, 
#' \code{\link{get_topic_sentiment}}
#'
#' @examples
#' \dontrun{
#' #library(openxlsx)
#' token <- "Request your API Key: https://developers.symanto.net/signup"
#' url_api <- "https://api.symanto.net"
#' 
#' # All actions:
#' user_actions <- c("personality", "communication", "emotions", "ekman_emotions", 
#'                   "sentiments", "topic_sentiments", "language")  
#' excel_dest_l1 <- label_text(url_api, excel_orig, user_actions, "UTF-8", 
#'                             token, 5, NULL, TRUE)
#' #write.xlsx(excel_dest_l1, "excel_dest_l1.xlsx")
#' 
#' # A sample of actions:
#' user_actions <- c("personality", "sentiments")  
#' excel_dest_l2 <- label_text(url_api, excel_orig, user_actions, "UTF-8", 
#'                             token, 32, NULL, TRUE)
#' #write.xlsx(excel_dest_l2, "excel_dest_l2.xlsx")
#' 
#' excel_orig1 <- excel_orig[, c("id", "text")]
#' user_actions <- c("personality", "sentiments", "language")  
#' excel_dest_l3 <- label_text(url_api, excel_orig1, user_actions, "UTF-8", 
#'                             token, 32, "en", TRUE)
#' #write.xlsx(excel_dest_l3, "excel_dest_l3.xlsx")
#' }
#' 
#' @importFrom janitor clean_names
#' @importFrom magrittr "%>%"
#' @importFrom dplyr left_join
#' 
#' @export

label_text <- function(url_api, excel_file, user_actions, encoding = "UTF-8", token, 
                       texts_in_blocks, user_language = NULL, verbose = FALSE) {
  text <- NULL
  lang_api <- c("ar", "de", "en", "es", "fr", "it", "nl", "pt", "ru", "tr", "zh")
  lang_three <- c("en", "de", "es")
  lang_two <- c("en", "de")
  
  if (any(!user_actions %in% c("personality", "communication", "emotions", 
                               "ekman_emotions", "sentiments", 
                               "topic_sentiments", "language"))) {
    stop("Please be sure that the user actions are 'personality', 'communication',
         'emotions', 'ekman_emotions', 'sentiments', 'topic_sentiments' or 'language'")
  }
  
  df0 <- excel_file %>%
    clean_names() %>%
    mutate(text = gsub("\\\"", "'", text)) %>%
    as.data.frame()
  
  # More special characters (\ < > |):
  spec_char <- "\\\\|<|>|\\|"
  df0$text <- gsub(spec_char, "", df0$text)
  
  # Run the loop for blocks of a certain number of texts.
  # Create blocks of texts with the same language due to 
  # the fact that the API does not allow blocks with texts
  # of different languages.

  # The strategy depends on if the language of the texts is given or not.
  # It given, the blocks are created for every language.
  # If not given, first the blocks are simply created and inside the
  # blocks loop, the language is detected with get_language_detection
  # inside each block and if there are texts with different languages,
  # this block is further splitted for the corresponding texts.
  
  # Note: It may happen that the language function returns an NA. This means
  # that the system is accepting queries, but the text is too messy to 
  # identify its language. In this case, we label manually the text with
  # the input language of the user.
  
  # stackoverflow split-a-vector-into-chunks
  if (is.null(user_language)) { # Case: Language available in the data set.
    df0$language[!df0$language %in% lang_api] <- user_language
    lang_texts <- unique(df0$language)
    blocks <- list()
    for (i in 1:length(lang_texts)) {
      df0_l <- df0[df0$language == lang_texts[i], ]
      blocks_iter <- split(df0_l, ceiling(1:nrow(df0_l) / texts_in_blocks))
      blocks <- append(blocks, blocks_iter)
    }
    names(blocks) <- as.character(1:length(blocks))
  }else{ # Case: Language not available, we will need to spot it.
    blocks <- split(df0, ceiling(1:nrow(df0) / texts_in_blocks))
  }
  
  if (verbose) {
    print("Number of initial blocks:")
    print(length(blocks))
  }
  
  df0_out_all <- data.frame()
  i <- 1
  while(i <= length(blocks)) {
    if (verbose) {
      print("Block number:")
      print(i)
    }
    df0_out <- blocks[[i]]
    df0_out <- as.data.frame(df0_out)

    df0_out <- df0_out %>%
      filter(!duplicated(text))
    # Duplicated texts give this error in pivot_wider:
    #Warning message:
    #  Values in `val` are not uniquely identified; output will contain list-cols.
    #* Use `values_fn = list(val = list)` to suppress this warning.
    #* Use `values_fn = list(val = length)` to identify where the duplicates arise
    #* Use `values_fn = list(val = summary_fun)` to summarise duplicates
    
    # This is a first condition to see if the links were correctly
    # accessed or if otherwise there was an error message such as:
    #  statusCode                                                             message
    #1        403 Out of call volume quota. Quota will be replenished in 20.09:58:55.
    gla <- get_language_detection(url_api, df0_out$text[1], encoding, token)
    if (is.null(gla$detected_language)) {
      break
    }
    
    if (!is.null(user_language)) {
      gla <- get_language_detection(url_api, df0_out$text, "UTF-8", token)
      df0_out$language <- gla$detected_language
      df0_out$language[!df0_out$language %in% lang_api] <- user_language
      
      if (length(unique(df0_out$language)) > 1) {
        subblocks <- split(df0_out, df0_out$language)
        # The texts with different languages are further added
        # to the list of blocks, except for the first one, which
        # is kept to label in this iteration.
        blocks <- append(blocks, subblocks[-1])
        names(blocks) <- as.character(1:length(blocks))
        df0_out <- subblocks[[1]]
        
        if (verbose) {
          print("Number of updated blocks:")
          print(length(blocks))
        }
      }
    }
    
    if ("personality" %in% user_actions) {
      if (verbose) {
        print("Personality traits:")
      }
      gp <- get_personality(url_api, df0_out$text, df0_out$language, TRUE, encoding, token)
      df0_out <- left_join(df0_out, gp, by = "text")
    }  
  
    if ("communication" %in% user_actions) {
      if (verbose) {
        print("Communication style:")
      }
      gco <- get_communic_style(url_api, df0_out$text, df0_out$language, TRUE, encoding, token)
      df0_out <- left_join(df0_out, gco, by = "text")
    }
  
    if ("emotions" %in% user_actions) {
      if (verbose) {
        print("Emotions:")
      }
      if (!all(df0_out$language %in% lang_three)) {
        df0_out$language[!df0_out$language %in% lang_three] <- user_language
      }
      ge <- get_emotion(url_api, df0_out$text, df0_out$language, TRUE, encoding, token)
      df0_out <- left_join(df0_out, ge, by = "text")
    }
    
    if ("ekman_emotions" %in% user_actions) {
      if (verbose) {
        print("Ekman's emotions:")
      }
      if (!all(df0_out$language %in% lang_three)) {
        df0_out$language[!df0_out$language %in% lang_three] <- user_language
      }
      gekm <- get_ekman_emotion(url_api, df0_out$text, df0_out$language, TRUE, encoding, token)
      df0_out <- left_join(df0_out, gekm, by = "text")
    }
  
    if ("sentiments" %in% user_actions) {
      if (verbose) {
        print("Sentiments:")
      }
      if (!all(df0_out$language %in% lang_three)) {
        df0_out$language[!df0_out$language %in% lang_three] <- user_language
      }
      gs <- get_sentiment(url_api, df0_out$text, df0_out$language, TRUE, encoding, token)
      df0_out <- left_join(df0_out, gs, by = "text")
    }

    if ("topic_sentiments" %in% user_actions) {
      if (verbose) {
        print("Topic sentiments:")
      }
      if (!all(df0_out$language %in% lang_two)) {
        df0_out$language[!df0_out$language %in% lang_two] <- user_language
      }
      gts <- get_topic_sentiment(url_api, df0_out$text, df0_out$language, encoding, token)
      df0_out <- left_join(df0_out, gts, by = "text")
    }

    if ("language" %in% user_actions) {
      if (verbose) {
        print("Language detection:")
      }
      gla <- get_language_detection(url_api, df0_out$text, encoding, token)
      df0_out <- left_join(df0_out, gla, by = "text")
    }
    
    df0_out_all <- rbindlist(list(df0_out_all, df0_out), fill = TRUE)
    i <- i + 1
  } # End while length(blocks) loop.
  
  return(df0_out_all)
}  