% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_topic_sentiment.R
\name{get_topic_sentiment}
\alias{get_topic_sentiment}
\title{Topic sentiments}
\usage{
get_topic_sentiment(url_api, text, language, encoding = "UTF-8", token)
}
\arguments{
\item{url_api}{URL to the API.}

\item{text}{String with the texts.}

\item{language}{Language of the text. Supported languages are 
"en" (English) and "de" (German).}

\item{encoding}{Character encoding. Default "UTF-8".}

\item{token}{The access token.}
}
\value{
A data frame with columns related to the topicSentiments, sentiments
and topics.
}
\description{
For a text written in a given language, this function detects topics and 
the topic category of each topic. Then, it analyzes the sentiment towards 
each of the topics mentioned.
}
\note{
The type of array for one text is: \cr
data <- "[{\"id\":\"1\",\"text\":\"love\",\"language\":\"en\"}]" \cr
For more than one text is: \cr
data <- "[{\"id\":\"1\",\"text\":\"love\",\"language\":\"en\"}, 
\cr
          {\"id\":\"2\",\"text\":\"hate\",\"language\":\"en\"}]" \cr
(write it in one line). 
In the first case, add a curly bracket after the first box bracket and 
before the last box bracket. In the second case, add the curly brackets
for every set of id, text and language.
}
\examples{
\dontrun{
token <- "Request your API Key: https://developers.symanto.net/signup"
url_api <- "https://api.symanto.net"

# For only one text:
# English:
text <- "Hello I love the service."
language <- "en"
gts_en <- get_topic_sentiment(url_api, text, language, "UTF-8", token)

# German:
text <- "Ich bin sehr zufrieden."
language <- "de"
gts_de <- get_topic_sentiment(url_api, text, language, "UTF-8", token)

text <- "Ich lese gerade 'Das Buch des Vaters' von Urs Widmer. 
         Interessantes Buch. Empfehlenswert"
language <- "de"
gts_de_urs <- get_topic_sentiment(url_api, text, language, "UTF-8", token)

# For more than one text:
# English:
df <- data.frame(text = c("love", "hate"), language = c("en", "en"))
text <- df$text
language <- df$language
gts_all <- get_topic_sentiment(url_api, text, language, "UTF-8", token)
}

}
\author{
Symanto
}
