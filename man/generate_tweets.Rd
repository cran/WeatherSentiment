\name{generate_tweets}
\alias{generate_tweets}

\title{
Generate Random Tweets}

\description{
This function generates n random tweets about the weather. Each tweet consists of a randomly selected positive or negative phrase about a randomly selected weather condition.
}
\usage{
generate_tweets(n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
The number of tweets to generate
}
}

\value{
A data.frame containing two columns:
Date: The date of the tweet
T1: The text of the tweet
}

\author{
Leila Marvian Mashhad and Andriette Bekker and Mohammad Arashi and Priyanka Nagar.
}

\examples{
tweets <- generate_tweets(10)
print(tweets)
}
