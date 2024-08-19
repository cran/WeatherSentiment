\name{process_tweet}
\alias{process_tweet}

\title{
Preprocess Tweets for Sentiment Analysis
}
\description{
This function takes a list of tweets as input and performs various preprocessing steps to prepare the data for sentiment analysis.
}
\usage{
process_tweet(tweet)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tweet}{
A vector of tweets
}
}

\value{
A list including:

   A vector containing preprocessed tweets.

   A vector containing tokens of tweets.
}

\author{
Leila Marvian Mashhad and Andriette Bekker and Mohammad Arashi and Priyanka Nagar.
}

\examples{
tweets_data <- "I'm feeling really happy today! #goodvibes"

preprocessed_tweets <- process_tweet(tweets_data)
print(preprocessed_tweets)
}
