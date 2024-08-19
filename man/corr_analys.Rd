\name{corr_analys}
\alias{corr_analys}

\title{
Calculate Correlation between Sentiment and Weather Variable
}
\description{
This function calculates the Pearson correlation coefficient between sentiment scores extracted from tweets and a weather variable (e.g., temperature) in a merged dataset.
}
\usage{
corr_analys(t, w, com_var = "Date", var1 = "T1", var2 = "T2")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{t}{
A data.frame containing tweets with a 'text' column
}
  \item{w}{
A data.frame containing weather data with a column matching the 'com_var'
}
  \item{com_var}{
The name of the common variable for merging the tweet and weather data. Defaults to "Date".
}
  \item{var1}{
The name of the column in 't' containing the tweet text. Defaults to "T1".
}
  \item{var2}{
The name of the column in 'w' containing the weather variable. Defaults to "T2".
}
}

\value{
The Pearson correlation coefficient between sentiment scores and the weather variable.
}

\author{
Leila Marvian Mashhad and Andriette Bekker and Mohammad Arashi and Priyanka Nagar.
}

\examples{
Date1 <- c('2024-01-01', '2024-01-02')
T1 <- c('I love sunny days', 'Rainy days are the worst') 
tweet <- data.frame(Date = Date1 , T1 = T1)
weather <- data.frame(Date = Date1, T2 = c(25, 15))
cor1 <- corr_analys(tweet, weather, com_var = "Date", var1 = "T1", var2 = "T2")
print(cor1)

}
