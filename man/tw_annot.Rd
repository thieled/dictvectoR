% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{tw_annot}
\alias{tw_annot}
\title{Annotated Tweets from German politicians.}
\format{
A data frame with 20838 rows and 8 variables:
\describe{
\item{status_id}{Tweet ID}
\item{ane}{Binary hand-coding of anti-elitism}
\item{ppc}{Binary hand-coding of people-centrism}
\item{pop}{Binary hand-coding of populism. \code{1} if either \code{ane} or \code{ppc} is \code{1}}
\item{coder}{Coder identifier. \code{A} or \code{B}. \code{AB} if Tweet was parallel code.
In this case, the coding is \code{1} if at least one coder decided to code \code{1}.}
\item{user_id}{Twitter user ID}
\item{twitter_handle}{Twitter handle}
\item{party}{Political party}
\item{followers_count}{The number of followers in Aug 2022}
\item{created_at}{Date and time when the Tweet was created}
\item{full_text}{Uncleaned Tweet}
\item{rel_test}{Indicates if Tweet was parallel-coded for reliability test}
\item{ane_A}{Binary hand-coding of anti-elitism from coder A}
\item{ane_B}{Binary hand-coding of anti-elitism from coder B}
\item{ppc_A}{Binary hand-coding of people-centrism from coder A}
\item{ppc_B}{Binary hand-coding of people-centrism from coder B}

}
}
\usage{
tw_annot
}
\description{
A dataset containing 1,000 hand-coded Tweets, drawn from \verb{[tw_data]}.
}
\details{
The dataset contains 1,000 Tweets which were drawn as stratified random sample from the population data \verb{[tw_data]}.
Each political party is represented with (at least) 120 Tweets. The populist parties AfD (250 Tweets) and Die Linke (150 Tweets)
were oversampled, as we anticipated more populist content from these parties.

TWo expert coders, one the author of this package, hand-coded the Tweets along two binary categories for populist communication:
Anti-elitism and people-centrism.

The coding followed the instructions documented in the online supplementary files of Thiele (2022).

90 Tweets were parallel-coded for reliability testing, resulting in Krippendorff's Alphas of .86 for anti-elitism, and
.71 for people-centrism, as documented by the variables \code{ane_A}, \code{ane_B}, \code{ppc_A}, and \code{ppc_B}.
}
\references{
Thiele, D. (2022). Pandemic Populism? How Covid-19 Triggered Populist Facebook User Comments
in Germany and Austria. Politics and Governance, 10(1), 185–196.
\url{https://doi.org/10.17645/pag.v10i1.4712}
}
\keyword{datasets}
