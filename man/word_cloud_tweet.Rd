\name{word_cloud_tweet}
\alias{word_cloud_tweet}

\title{
Generate Word Cloud from Tweet Text
}
\description{
This function generates a word cloud visualization of the most frequent words in a tweet after basic cleaning. Stop words and words less than 3 characters are removed.
}
\usage{
word_cloud_tweet(tweet)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tweet}{
A character string containing the text of the tweet.
}
}

\value{
void (generates a word cloud image).
}

\author{
Leila Marvian Mashhad and Andriette Bekker and Mohammad Arashi and Priyanka Nagar.
}
 
\examples{
# Generate word cloud from a single tweet
tweet_text <- "This is a sample tweet for word cloud generation!"
word_cloud_tweet(tweet_text)

## This will generate a word cloud image where the most frequent words 
## in the tweet will be displayed larger.

}
