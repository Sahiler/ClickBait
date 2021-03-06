% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysis_functions.R
\name{determine_score}
\alias{determine_score}
\title{Calculates the score of articles from a csv of articles.}
\usage{
determine_score(csv)
determine_score(titles,texts)
}
\arguments{
\item{titles}{A list of titles.}

\item{texts}{A list of article texts.}

\item{csv}{A data frame of articles with the title attribute and text attribute.}
}
\value{
A list of the scores of each article.
}
\description{
Calculates the score of articles from a csv of articles.
}
