#' Get combinations of keywords
#'
#' Returns combinations of keywords.
#'
#' @details Takes a data.frame `word_df` with a character column specified by `words`
#' as input.
#' As default, it will return all combinations of various lengths of these words.
#' Additionally, the function can account for conceptual dimensions, identified by
#' a categorical (character, numerical, or factor)
#' column in `word_df` specified by `dims`. If dimensions are specified, the function
#'  will find all combinations for each dimension, and
#' will return all combinations of these combinations. CAUTION: This can lead quickly
#'  to an extremely large number of combinations.
#'
#' The number of combinations can be limited in several ways:
#'
#' Firstly, the minimum number of words returned per dimension is specified by `min_per_dim`, the maximum number of words overall is set by `max_overall`.
#'
#' Secondly, the function allows for random sampling of combinations.
#' This is recommended, as it drastically reduces the number of returned combinations,
#' reduces the computational load, and speeds up the process. (Of course, this comes at
#'  the cost of completeness).
#' Random sampling is implemented using the \code{\link[RcppAlgos]{comboSample}} function.
#'  Setting a `limit` will cap the number of combinations
#' of equal length for each dimension.
#'
#' E.g., if `limit = 5`, `min_per_dim = 2`, `max_overall = 6` is set for a `word_df`
#' containing two dimensions `a` and `b`,
#' the function will pick max. five combinations of length 2 for `a`, and five of
#' length 2 for `b`, five of length 3 for `a`,
#' and five of length 3 for `b`, and will return all combinations of these combinations.
#'
#' @param word_df A [data.frame] containing a column with words or multi-word expressions.
#' @param word_field character. Default is `'words'`. The name of the column in
#' word_df that contains the words.
#' @param dims character. Default is `NULL`. The name of the column in word_df that groups
#'  the words into dimensions. Can be ignored.
#' @param min_per_dim numerical. Default is `1`. Minimum number of words (per dimension)
#'  returned. Will be replaced by 1 if < 1.
#' @param max_overall numerical. Maximum number of words per returned combination.
#' @param limit numerical. Default is `NULL`. Limits the number of combinations of equal
#'  length per dimension by randomization.
#' @param seed numerical. Default is `1`. Input to make randomization reproducible.
#' @param save_settings logical. Default is `TRUE`. Saves randomization settings in
#' df to make it reproducible.
#' @param save_input logical. Default is `FALSE`. Saves input words and dims in df as
#'  character string.
#' @returns A data.frame of combinations.
#' The combinations are stored as a [list] of character vectors in `combs_split`.
#' Use this column to pass it to \code{\link{get_many_F1s}} or \code{\link{get_many_RPFs}}.
#'
#' The data.frame will include string variables for the words in the combinations
#' called `combs` for each dimension.
#' Do not use these columns passing the dictionaries on to \code{\link{get_many_F1s}},
#'  as this will result in a
#' faulty average representation, caused by how representations for multi-word expressions
#'  are queried.
#'
#' Additionally, the data.frame includes a rowid, a count variable for the number of words
#'  overall (`sum_nterms`),
#' and counts for each dimension, which can be used to remove imbalanced dictionaries.
#'  If requested, `settings` stores the randomization settings,
#' and `input` the words and dimensions used as input.
#'
#' @examples
#' test_df <- data.frame(words = letters[1:8],
#'                       dim = rep(paste0("c_", 1:2), 4))
#' t0 <- get_combis(test_df,
#'                  word_field = "words",
#'                  dims = "dim",
#'                  max_overall = 5)
#' t1 <- get_combis(test_df,
#'                  word_field = "words",
#'                  dims = "dim",
#'                  max_overall = 5,
#'                  limit = 5)
#' @importFrom magrittr `%>%`
#' @importFrom magrittr `%<>%`
#' @export
get_combis <- function(word_df,
                       word_field = "words",
                       dims = NULL,
                       min_per_dim = 1,
                       max_overall,
                       limit = NULL,
                       seed = 1,
                       save_settings = T,
                       save_input = F) {

  # Define word index in word_df
  word_index <- 0
  if (is.character(word_field)) {
    word_index <- match(word_field, names(word_df))
  }
  if (is.na(word_index))
    stop("word_field column not found or invalid")
  if (!is.character(word_df[[word_index]]))
    stop("word_field must refer to a character mode column")

  # Define dim index in word_df
  if(is.null(dims)){
    word_df$dims <- 1
  }else{
    dim_index <- match(dims, names(word_df))
    word_df$dims <- word_df[[dim_index]]
  }

  # Get number of dimensions
  ndims = word_df$dims %>% factor() %>% nlevels()

  # Split words by dim
  splits  <- base::split(word_df[[word_index]],
                         factor(word_df$dims), drop = T)

  # Function for combinations of single vector
  get_random_combinations <- function(v1,
                                      min_per_dim,
                                      max_words,
                                      limit = NULL,
                                      seed) {
    if (min_per_dim < 1) {min_per_dim = 1} # return min 1 word per dimension
    if (length(v1) < max_words) {max_words = length(v1)} # if max words > words provided, use max. n

    # set limit to infinite if empty
    limit <- ifelse(is.null(limit), Inf, limit)

    # get (sampled) combinations for each dimension in all specified lengths
    c1 <- do.call("c", lapply(min_per_dim:max_words,
                              function(i) {
                                nn = choose(length(v1), 1:max_words)
                                nn = ifelse(nn > limit, limit, nn)
                                RcppAlgos::comboSample(v = v1,
                                                       m = i,
                                                       n = nn[i],
                                                       seed = seed,
                                                       FUN = as.character)
                              }
    ))
    return(c1)
  }

  # Max. words per dimension
  if (max_overall < ndims) {max_overall <- ndims} # at least 1 word per dimension
  max_words_perdim = round(max_overall/ndims, 0) # how many words p. dimension at max

  # Apply get_combinations on list of dimensions
  combi_list <- lapply(splits,
                       get_random_combinations,
                       min_per_dim = min_per_dim,
                       max_words = max_words_perdim,
                       limit = limit, seed = seed)

  # Get combinations of combinations
  df <- expand.grid(combi_list)

  # Wrangle strings
  df %<>% dplyr::mutate_all(~sapply(., toString))
  # Replace NAs
  df %<>% dplyr::mutate_all(list(~dplyr::na_if(.,"")))

  # Define string count +1 function
  str_count_plusone <- function(string, pattern){
    stringr::str_count(string = string, pattern = pattern)+1
  }

  # Count number of terms per dimension
  df_count <- df %>%
    dplyr::mutate(
      dplyr::across(
        .cols  = c(1:ncol(df)),
        .fns   = str_count_plusone,
        ", ",
        .names = "{col}_nterms"
      )
    )
  df_count %<>% tibble::rowid_to_column()

  #Calculate sum of all words
  df_count %<>%
    dplyr::rowwise() %>%
    dplyr::mutate(sum_nterms = sum(dplyr::c_across(dplyr::ends_with("nterms")), na.rm = T))

  # Unite string variables
  df_united <- df %>%
    tidyr::unite("combs", names(df), sep = ", ", na.rm = T) %>%
    tibble::rowid_to_column()

  # sort alphabetically
  df_united %<>%
    dplyr::mutate(combs = purrr::map_chr(
      stringr::str_split(combs, ", "),
      .f = function(x) {
        x %>%
          stringr::str_sort() %>%
          stringr::str_c(collapse = ", ")
      }))

  # Merge count info
  df <- dplyr::left_join(df_united, df_count)

  # Keep only distinct combis
  df %<>% dplyr::distinct(combs, .keep_all = T)

  # Split combis along ', ' for use in get_many_F1s function
  df$combs_split <- df$combs %>% stringr::str_split(", ")

  # Store information about settings and input
  if(save_settings == T){
  df$settings <- paste0('limit = ', ifelse(is.null(limit), 'NULL', limit),
                              ', seed = ', seed,
                              ', time: ', Sys.time())
  }

  # Store input
  if(save_input == T){
  df$input <- paste0('words: ', toString(word_df[[word_index]]),
                     ', dims: ', ifelse(is.null(dims), 'NA', toString(word_df[[dim_index]])))
  }

  return(df)

}




