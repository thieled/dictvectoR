#' Tweets from German politicians.
#'
#' A dataset containing 20,838 Tweets from 32 German politicians,
#' posted between 2020-03-11 and 2021-09-25.
#'
#' @details
#' The Twitter accounts were selected from the EPINetz Twitter Politicians Dataset 2021 (König et al., 2022).
#' For each of the seven political parties represented in the German Parliament, the five most popular
#' Twitter accounts from politicians active at the federal level or party representatives were selected.
#' Only accounts with more than 30,000 followers were selected.
#'
#' The timeframe starts with the beginning of the Covid-19 pandemic and ends one day before
#' the German general elections 2021.
#'
#' For each account, the maximum number (3,200) of Tweets returned by the API v2 were downloaded in August 2022,
#' using rtweet (Kearney, 2022). Quotes, re-tweets, and Tweets outside the timeframe were excluded.
#' Completeness of this dataset cannot be guaranteed.
#'
#' The dataset also includes a variable extracted from the POPPA Populism and Political Parties Expert Survey,
#' indicating the mean expert rating of populism per political party.
#'
#'
#' @format A data frame with 20838 rows and 8 variables:
#' \describe{
#'   \item{user_id}{Twitter user ID}
#'   \item{twitter_handle}{Twitter handle}
#'   \item{party}{Political party}
#'   \item{followers_count}{The number of followers in Aug 2022}
#'   \item{status_id}{Tweet ID}
#'   \item{created_at}{Date and time when the Tweet was created}
#'   \item{full_text}{Uncleaned text of the Tweet}
#'   \item{poppa_populism}{Mean populism score from POPPA expert survey}
#' }
#' @source  [Twitter](https://developer.twitter.com/),
#'  [EPINet Twitter Dataset](https://doi.org/10.7802/2415),
#'  [POPPA](https://doi.org/10.7910/DVN/8NEL7B)
#' @references
#' Kearney, M. W., Sancho, L. R., Wickham, H., Heiss, A., Briatte, F., & Sidi, J. (2022).
#' rtweet: Collecting Twitter Data. Retrieved from \url{https://CRAN.R-project.org/package=rtweet}
#'
#' König, T., Schünemann, W. J., Brand, A., Freyberg, J., & Gertz, M. (2022).
#' The EPINetz Twitter Politicians Dataset 2021. A New Resource for the Study
#' of the German Twittersphere and Its Application for the 2021 Federal Elections.
#' Politische Vierteljahresschrift. \url{https://doi.org/10.1007/s11615-022-00405-7}
#'
#' Meijers, M., & Zaslove, A. (2020). Populism and Political Parties Expert Survey 2018 (POPPA) (Data set).
#' Harvard Dataverse. \url{https://doi.org/10.7910/DVN/8NEL7B}
#'
"tw_data"




#' Annotated Tweets from German politicians.
#'
#' A dataset containing 1,000 hand-coded Tweets, drawn from `[tw_data]`.
#'
#' @details
#' The dataset contains 1,000 Tweets which were drawn as stratified random sample from the population data `[tw_data]`.
#' Each political party is represented with (at least) 120 Tweets. The populist parties AfD (250 Tweets) and Die Linke (150 Tweets)
#' were oversampled, as we anticipated more populist content from these parties.
#'
#' TWo expert coders, one the author of this package, hand-coded the Tweets along two binary categories for populist communication:
#' Anti-elitism and people-centrism.
#'
#' The coding followed the instructions documented in the online supplementary files of Thiele (2022).
#'
#' 90 Tweets were parallel-coded for reliability testing, resulting in Krippendorff's Alphas of .86 for anti-elitism, and
#' .71 for people-centrism, as documented by the variables `ane_A`, `ane_B`, `ppc_A`, and `ppc_B`.
#'
#' @format A data frame with 20838 rows and 8 variables:
#' \describe{
#'   \item{status_id}{Tweet ID}
#'   \item{ane}{Binary hand-coding of anti-elitism}
#'   \item{ppc}{Binary hand-coding of people-centrism}
#'   \item{pop}{Binary hand-coding of populism. `1` if either `ane` or `ppc` is `1`}
#'   \item{coder}{Coder identifier. `A` or `B`. `AB` if Tweet was parallel code.
#'   In this case, the coding is `1` if at least one coder decided to code `1`.}
#'   \item{user_id}{Twitter user ID}
#'   \item{twitter_handle}{Twitter handle}
#'   \item{party}{Political party}
#'   \item{followers_count}{The number of followers in Aug 2022}
#'   \item{created_at}{Date and time when the Tweet was created}
#'   \item{full_text}{Uncleaned Tweet}
#'   \item{rel_test}{Indicates if Tweet was parallel-coded for reliability test}
#'   \item{ane_A}{Binary hand-coding of anti-elitism from coder A}
#'   \item{ane_B}{Binary hand-coding of anti-elitism from coder B}
#'   \item{ppc_A}{Binary hand-coding of people-centrism from coder A}
#'   \item{ppc_B}{Binary hand-coding of people-centrism from coder B}
#'
#' }
#' @references
#' Thiele, D. (2022). Pandemic Populism? How Covid-19 Triggered Populist Facebook User Comments
#' in Germany and Austria. Politics and Governance, 10(1), 185–196.
#' \url{https://doi.org/10.17645/pag.v10i1.4712}
#'
"tw_annot"
