\name{sentiment_analys}
\alias{sentiment_analys}

\title{
Sentiment Analysis of a Tweet
}
\description{
This function analyzes the sentiment of a tweet and returns the sentiment score and the text of the tweet.
}
\usage{
sentiment_analys(tweet)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tweet}{
A character string containing the text of the tweet
}
}

\value{
A data.frame containing two columns:
   text: The text of the tweet
   ave_sentiment: The sentiment score of the tweet
In addition, it presents a plot to effectively visualize the spectrum of human emotions.    
}

\author{
Leila Marvian Mashhad and Andriette Bekker and Mohammad Arashi and Priyanka Nagar.
}

\examples{
#Example 1
tweet_text <- "I love R!"
sentiment_result <- sentiment_analys(tweet_text)
print(sentiment_result)

#Example2
tweets <- c("I hate R!", "R is a great language!", "R is difficult to learn!")
sentiment_results <- sapply(tweets, sentiment_analys)
print(sentiment_results)
}
