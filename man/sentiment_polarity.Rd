\name{sentiment_polarity}
\alias{sentiment_polarity}

\title{
Analyze Sentiment Polarity of a Tweet}

\description{
This function takes a tweet text as input and performs sentiment analysis to visualize its overall sentiment polarity.
}
\usage{
sentiment_polarity(tweet)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tweet}{
A character vector containing the tweet text.
}
}

\value{
A ggplot object displaying a bar chart with sentiment polarity (positive/negative) on the x-axis and sentiment score on the y-axis.
}

\author{
Leila Marvian Mashhad and Andriette Bekker and Mohammad Arashi and Priyanka Nagar.
}

\examples{
e <- c("The rain is ruining my outdoor plans today.", 
"I love the sunny weather today!")

s1 <- sentiment_polarity(e)
print(s1)
}
