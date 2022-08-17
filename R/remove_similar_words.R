#' Detect similar words
#'
#' Detects similar words in a data.frame, using a fastText model.
#' If requested, compares similar words along a variable specified in `compare_by`,
#' and by the number of occurrences stored in the variable named `hits`.
#' Counts how often one word 'beats' its similar other in pairwise comparison.
#' This count is returned as 'wins_'.
#'
#' Forces the 'word_df' to have a unique identifying variable called 'word_id';
#' re-uses any variable named 'id' that is unique.
#'
#' @param word_df A [data.frame] containing a column with words or multi-word expressions.
#' @param word_field Character. The name of the column in word_df that contains the words.
#' @param model A fastText model, loaded by \code{\link[fastrtext]{load_model}}.
#' @param compare_by Character. Default `NULL`. The name of a column that should be compared.
#' @param compare_hits Logical. Default `TRUE`. If true counts how often one word 'beats'
#' its similar other in regard to occurrences.
#' @param min_simil Numerical (0-1). Default .7. Similarity threshold. Word pairs below
#' this threshold are considered dissimilar.
#' @returns A data.frame. Containing a pairwise similarity table of all similar words.
#'@export
#'@importFrom magrittr `%>%`
#'@importFrom magrittr `%<>%`
#'@examples
#'model <- fastrtext::load_model(system.file("extdata",
#' "tw_demo_model_sml.bin",
#'package = "dictvectoR"))
#'word_df <- data.frame(words = c("unsere steuern",
#' "steuerzahler",
#'  "unsere",
#'  "steuern"), hits = c(2, 3, 15, 4))
#'detect_similar_words(word_df, model)
detect_similar_words <- function(word_df,
                                 model,
                                 word_field = "words",
                                 compare_by = NULL,
                                 compare_hits = T,
                                 min_simil = .7){


  # Force word_df to have a unique identifier called word_id
  # Note: if any column in word_df contains 'id' and is a unique identifier,
  # it is considered as 'word_id'
  word_df %<>% add_word_id()

  # Find word index in word_df
  word_index <- 0
  word_index <- match(word_field, names(word_df))
  if (is.na(word_index))
    stop("word_field column not found or invalid")
  if (!is.character(word_df[[word_index]]))
    stop("word_field must refer to a character mode column")

  # Find compare_by index in word_df
  if(!is.null(compare_by)){
    comp_index <- 0
    comp_index <- match(compare_by, names(word_df))
    if (is.na(comp_index))
      stop("compare_by column not found or invalid")
    if (!is.numeric(word_df[[comp_index]]))
      stop("compare_by must refer to a numeric mode column")
  }


  # Find hits index, if compare_hits == T
  if(compare_hits == T){
    hits_index <- 0
    hits_index <- match("hits", names(word_df))
    if (is.na(hits_index)){
      print("ignore hits - no hits to compare.")
      compare_hits = F
      }
  }


  # get fasttext representation of terms
  word_rep <-  get_word_representations(word_df = word_df,
                                        word_field = word_field,
                                        model = model)
  rownames(word_rep) <- word_df$word_id

  # Get similarity matrix and reshape into comparison table
  sim_df <- quanteda.textstats::textstat_simil(x = quanteda::as.dfm(word_rep),
                                               method = "cosine",
                                               min_simil = min_simil) %>%
    as.matrix() %>%
    as.data.frame.table() %>%
    dplyr::filter(Var1 != Var2, !is.na(Freq), Freq < 1, Freq >= min_simil) %>%
    dplyr::rename(word_id1 = Var1, word_id2 = Var2, simil = Freq)


  ## Prepare data.frames for merging info from word_df

  # Table 1
  w1 <- as.data.frame(matrix(nrow=nrow(word_df)))
  w1$word_id1 <- word_df$word_id
  w1$word1 <- word_df[[word_index]]
  w1 <- w1[,-1]

  if(!is.null(compare_by)){
    w1$score1 <- word_df[[comp_index]]
  }

  if(compare_hits == T){
    w1$hits1 <- word_df[[hits_index]]
  }

  # Table 2
  w2 <- as.data.frame(matrix(nrow=nrow(word_df)))
  w2$word_id2 <- word_df$word_id
  w2$word2 <- word_df[[word_index]]
  w2 <- w2[,-1]

  if(!is.null(compare_by)){
    w2$score2 <- word_df[[comp_index]]
  }

  if(compare_hits == T){
    w2$hits2 <- word_df[[hits_index]]
  }


  # Join info
  suppressMessages( sim_df <- dplyr::left_join(sim_df, w1) )
  suppressMessages( sim_df <- dplyr::left_join(sim_df, w2) )

  # Arrange vars
  sim_df %<>% dplyr::select(simil, dplyr::ends_with("1"), dplyr::ends_with("2"))

  # Compare score
  if(!is.null(compare_by)){
    sim_df %<>% dplyr::mutate(wins_score1 = ifelse(score1 > score2, 1, 0))
  }

  # Compare hits (note: greater or equal = 1)
  if(compare_hits == T){
    sim_df %<>% dplyr::mutate(wins_hits1 = ifelse(hits1 >= hits2, 1, 0))
  }

  return(sim_df)

}




#' Determine which similar terms to drop
#'
#' Takes the pairwise similarity table returned from \code{\link{detect_similar_words}}.
#' Returns a data.frame of terms that should be dropped according to the specified
#' decision rules.
#' The function can compare the words in the similarity table along two aspects:
#' \itemize{
#'   \item The number of comparison wins regarding the score specified by `'compare_by'`
#'   in \code{\link{detect_similar_words}}, resp. \code{\link{remove_similar_words}}.
#'   This variable must be stored in the similarity table as `'wins_score1`.
#'   \item The number of comparison wins regarding the frequency of occurrences,
#'   set as `'compare_hits = T'` in \code{\link{detect_similar_words}},
#'   resp. \code{\link{remove_similar_words}}.
#'   This variable must be stored in the similarity table as `'wins_hits1`.
#'   }
#' Both variables can be compared at the same time. If 'wins_score1' is missing,
#'  the function will compare 'hits'.
#'@param simil_table data.frame. A pairwise similarity table returned
#'from \code{\link{detect_similar_words}}.
#'@param compare_hits logical. Default `TRUE`. If TRUE, will consider 'hits' in comparison.
#'@param win_threshold Numerical (0-1). Default .5. Determines the threshold to drop words,
#' defined as proportion of won pairwise comparisons.
#' If hits and scores are compared, it is the mean of both proportions.
#' Words will be suggested for dropping if the computed value is smaller than the value
#' set here.
#'@returns A data.frame with words suggested for dropping.
#'@importFrom magrittr `%>%`
#'@importFrom magrittr `%<>%`
#'@export
#'@examples
#' model <- fastrtext::load_model(system.file("extdata",
#'                                           "tw_demo_model_sml.bin",
#'                                            package = "dictvectoR"))
#' set.seed(1)
#' word_df <- data.frame(words = c("unsere steuern", "steuerzahler",
#'                                "unsere", "steuern"),
#' hits = c(2, 3, 15, 4), score = rnorm(4))
#' sim_t <- detect_similar_words(word_df, model, compare_by = "score")
#' drop_which(sim_t, compare_hits = TRUE, win_threshold = .4)
drop_which <- function(simil_table,
                       compare_hits = T,
                       win_threshold = .5){

  ## Variable names in simil_table set by 'detect_similar_words' that sets names.

  # Check if comparing variable 'wins_score1' is existing, if not compare by hits.
  # If neither, stop.
  if(!'wins_score1' %in% names(simil_table)){
    message('No variable named \'wins_score1\' to compare in simil_table, try \'wins_hits1\'...')
    if('wins_hits1' %in% names(simil_table)){
      compare_hits = T
    }else{
      stop('No variable named \'wins_score1\' or \'wins_hits1\' to compare in simil_table. \n
            Specify \'compare_by\' or \'compare_hits\' in \'detect_similar_words\' or \'remove_similar_words\'.')
    }
  }

  # Compare 'wins_scores'
  if('wins_score1' %in% names(simil_table)){
    if(compare_hits == T){
      grouped_table <- simil_table %>% dplyr::group_by(word_id1) %>%
      dplyr::summarise(words = dplyr::first(word1),
                       word_id = dplyr::first(word_id1),
                       wins_score_pct = sum(wins_score1)/dplyr::n(),
                       wins_hits_pct = sum(wins_hits1)/dplyr::n()) %>%
      dplyr::mutate(win = (wins_score_pct+wins_hits_pct)/2)
      }else{
      grouped_table <- simil_table %>% dplyr::group_by(word_id1) %>%
      dplyr::summarise(words = dplyr::first(word1),
                       word_id = dplyr::first(word_id1),
                       win = sum(wins_score1)/dplyr::n())
      }
  }else{
      grouped_table <- simil_table %>% dplyr::group_by(word_id1) %>%
      dplyr::summarise(words = dplyr::first(word1),
                       word_id = dplyr::first(word_id1),
                       win = sum(wins_hits1)/dplyr::n())
      }

  grouped_table %<>% dplyr::filter(win < win_threshold)
  return(grouped_table)
}







#' Remove too similar terms
#'
#' Removes similar terms from a data.frame `word_df`.
#' @details
#' Detects similar words and multiword expressions in `word_df`, using a `fastText` model.
#'  The cosine similarity threshold is set by `min_simil`.
#' If requested, similar words are compared along a variable specified in `compare_by`,
#' and/or by the number of occurrences stored in the variable named `hits`.
#'
#' The threshold for dropping terms is set by `win_threshold`.
#' This indicates the proportion of how many of the pairwise comparisons are 'won'
#' by the word in question. If `compare_hits = T` and a `compare_by` is set,
#' it is the mean of both proportions. If a word 'wins' all comparisions
#' regarding frequency (wins_hits1 == 1),
#' but loses all comparisions regaring the set score ('wins_score1' == 0), this value is .5.
#'
#' Forces `'word_df'` to contain a unique identifier called 'word_id';
#' re-uses the first variable named 'id' that is such a unique identifier.
#'
#' @param word_df A [data.frame] containing a column with words or multi-word expressions.
#' @param word_field character. The name of the column in word_df that contains the words.
#' @param model A fastText model, loaded by \code{\link[fastrtext]{load_model}}.
#' @param compare_by character. Default `NULL`. The name of a column that should be compared.
#' @param compare_hits logical. Default `TRUE`. If true counts how often one word 'beats'
#'  its similar other in regard to occurrences.
#' @param min_simil Numerical (0-1). Default .7. Similarity threshold. Word pairs below this
#'  threshold are considered dissimilar.
#'@param win_threshold Numerical (0-1). Default .5. Determines the threshold to drop words,
#' defined as proportion of won pairwise comparisons;
#'resp., if both `compare_by` and `compare_hit` are set, the mean of both proportional wins.
#' @returns A data.frame. Containing a pairwise similarity table of all similar words.
#'@export
#'@importFrom magrittr `%>%`
#'@importFrom magrittr `%<>%`
#'@examples
#'model <- fastrtext::load_model(system.file("extdata",
#'                                           "tw_demo_model_sml.bin",
#'                                           package = "dictvectoR"))
#'set.seed(1)
#'word_df <- data.frame(words = c("unsere steuern",
#'                                "steuerzahler",
#'                                "unsere",
#'                                "steuern"),
#'                      hits = c(2, 3, 15, 4),
#'                      score = rnorm(4))
#'remove_similar_words(word_df,
#'                     model,
#'                     compare_by = "score",
#'                     compare_hits = FALSE,
#'                     win_threshold = .4)
remove_similar_words <- function(word_df,
                                 model,
                                 word_field = "words",
                                 compare_by = NULL,
                                 compare_hits = T,
                                 min_simil = .7,
                                 win_threshold = .5){

  # Add word_id if not yet there
  word_df %<>% add_word_id()

  simil_table <- detect_similar_words(word_df = word_df,
                                      model = model,
                                      word_field = word_field,
                                      compare_by = compare_by,
                                      compare_hits = compare_hits,
                                      min_simil = min_simil)

  losers <- drop_which(simil_table = simil_table,
                       compare_hits = compare_hits,
                       win_threshold = win_threshold)

  # Drop similar terms that lost too often in comparison
  reduced_df <- word_df %>% dplyr::filter(!word_id %in% losers$word_id)

  return(reduced_df)

}
