#' Find multi-word expressions.
#'
#' Adds multi-word expressions found in a quanteda tokens object to a data.frame of words.
#' @param word_df A data.frame containing words.
#' @param tokens A word-tokens object, returned by quanteda::tokens.
#' @param min_hits Numerical. Default is 3. Minimum occurrence of the found multi-word
#' expressions.
#' @param word_field Character. Default is "words". Name of the column in `word_df`
#' that contains the words.
#' @param levels Numerical (1 or 2). The window size for multi-word expressions.
#' @returns A data.frame with all information from `word_df`, but with added multi-word
#' expressions in rows.
#' Additionally, the returned data.frame contains the columns...
#'  \itemize{
#'   \item `'orig_id'` with a unique identifier for each original word, re-used from `word_df` or created new if not present
#'   \item `'from'` indicating the word of origin
#'   \item `'word_id` (character) with a unique identifier for all words and multi-words
#'   \item `'hits` indicating the number of occurrences of the word or multi-word
#' }
#' @importFrom magrittr `%>%`
#' @importFrom magrittr `%<>%`
#' @export
#' @examples
#' tw_data %<>% head(100) %>% clean_text(text_field = 'full_text')
#' toks <- quanteda::tokens(tw_data$text)
#' data.frame(words = c("deutschen", "millionen")) %>%
#'             add_multiwords(tokens = toks,
#'             min_hits = 1,
#'             levels = 1)
add_multiwords <- function(word_df,
                           tokens,
                           min_hits = 3,
                           word_field = "words",
                           levels = c(1,2)) {

  get_multiwords_for_one <- function(w, tokens = tokens, min_hits = min_hits){

    # split original words into single words
    wp <- stringr::str_split(w, pattern = " ", simplify = TRUE) %>%
      as.character()

    # Get preceding words - window = 1
    pre <- quanteda::tokens_select(tokens,
                                   pattern = quanteda::phrase(w),
                                   window = c(1,0)) %>%
      quanteda::dfm() %>%
      quanteda::featfreq() %>%
      sort(decreasing = T)
    pre <- data.frame(hits = pre) %>%
      dplyr::filter(hits >= min_hits) %>%
      tibble::rownames_to_column("words") %>%
      dplyr::filter(!words %in% wp) %>%
      dplyr::mutate(words = stringr::str_c(paste(words), paste(w), sep = " "))

    # Get successive words - window = 1
    pos <- quanteda::tokens_select(tokens,
                                   pattern = quanteda::phrase(w), window = c(0,1)) %>%
      quanteda::dfm() %>%
      quanteda::featfreq() %>%
      sort(decreasing = T)
    pos <- data.frame(hits = pos) %>%
      dplyr::filter(hits >= min_hits) %>%
      tibble::rownames_to_column("words") %>%
      dplyr::filter(!words %in% wp) %>%
      dplyr::mutate(words = stringr::str_c(paste(w), paste(words), sep = " "))

    # Bind
    res <- dplyr::bind_rows(pre, pos)
    # Attach original word
    res %<>% dplyr::mutate(from = w)
    return(res)

  }

  # Define word index in word_df
  word_index <- 0
  if (is.character(word_field)) {
    word_index <- match(word_field, names(word_df))
  }
  if (is.na(word_index))
    stop("word_field column not found or invalid")
  if (!is.character(word_df[[word_index]]))
    stop("word_field must refer to a character mode column")

  # Apply for all words in word_df
  print("Finding multi-word expressions in window = 1...")
  mw <- pbapply::pblapply(word_df[[word_index]],
                          get_multiwords_for_one,
                          tokens = tokens, min_hits = min_hits) %>%
    dplyr::bind_rows()

  # # Get second level multiwords
  if(levels == 2 && nrow(mw) > 0){
    print("Finding multi-word expressions in window = 2...")
    mw_lvl2 <- pbapply::pblapply(mw$word,
                                 get_multiwords_for_one,
                                 tokens = tokens, min_hits = min_hits) %>%
      dplyr::bind_rows()

    # # Fix original word in lvl2
    from_orig <- mw %>% dplyr::filter(stringr::str_count(from, "\\w+") == 1) %>%
      dplyr::select(w = words, from)
    from_else <- mw_lvl2 %>% dplyr::filter(stringr::str_count(from, "\\w+") != 1) %>%
      dplyr::rename(w = from)
    mw_lvl2 <- dplyr::left_join(from_else, from_orig, by = "w") %>%
      dplyr::select(-w)

    # Bind
    mw <- dplyr::bind_rows(mw, mw_lvl2)
  }

  # Assign multiword id
  mw %<>% dplyr::arrange(from, dplyr::desc(hits)) %>%
                    dplyr::group_by(from) %>%
                    dplyr::mutate(mw_id = sprintf("%03d", dplyr::row_number())) %>%
                    dplyr::ungroup()

  # Keep only distinct multiwords
  mw %<>% dplyr::distinct(words, .keep_all = T)

  # Add word_id to original word_df
  word_df %<>% add_word_id()

  # Update word index if changed
  word_index <- match(word_field, names(word_df))

  # Merge original ids
  word_df$from <- word_df[[word_index]]
  # word_df$word_id %<>% as.character()
  # word_df$mw_id %<>% as.character()
  id_df <- word_df %>% dplyr::select(from, orig_id = word_id)
  mw <- dplyr::left_join(mw, id_df)
  mw %<>% dplyr::mutate(word_id = stringr::str_c(orig_id, mw_id, sep = "_"))

  # Harmonize variable names
  if(names(word_df[word_index])!="words"){
    word_df$words <- word_df[[word_index]]
    word_df <- word_df[-word_index]
  }
  word_df$orig_id <- word_df$word_id

  # Bind multiword df and original df
  #word_df %<>% select(-from)
  new_df <- dplyr::bind_rows(word_df, mw) %>%
    dplyr::arrange(word_id)

  # Arrange vars
  new_df %<>% dplyr::select(words, from, word_id, orig_id, hits, dplyr::everything(), -mw_id)

  # Drop duplicates
  new_df %<>% dplyr::distinct(words, .keep_all = T)

  # Add hits for single words
  print("Adding missing count of original words:")
  new_df <- get_hits(new_df, tokens = tokens, word_field = word_field, replace = F)

  return(new_df)
}



#' Get occurrence frequency of words.
#'
#' Adds the number of occurrences of a word or multi-word expression
#' in a quanteda tokens object to a data.frame.
#' By default, it checks if 'hits' have been counted before and only fills
#'  in missing values. Works with GLOB-style wildcards.
#' @param word_df A data.frame containing words.
#' @param tokens A word-tokens object, returned by quanteda::tokens.
#' @param word_field Character. Default is "words". Name of the column in
#'  `word_df` that contains the words.
#' @param replace Logical. Default is `FALSE`. If `FALSE` checks fills in 'hits'
#'  only for missing observations. If `TRUE` counts 'hits' for all words in word_df.
#' @returns A data.frame with a column `'hits'` indicating the frequency of this term
#'  in the tokens. Arranged by descending number of hits.
#' @importFrom magrittr `%>%`
#' @importFrom magrittr `%<>%`
#' @export
#' @examples
#' tw_data %<>% head(100) %>% clean_text(text_field = 'full_text')
#' toks <- quanteda::tokens(tw_data$text)
#' word_df <- data.frame(words = c("der deutschen", "steuer*", "xyz")) %>%
#' get_hits(tokens = toks)
get_hits <- function(word_df,
                     tokens,
                     word_field = "words",
                     replace = F){

  # Define word index in word_df
  word_index <- 0
  if (is.character(word_field)) {
    word_index <- match(word_field, names(word_df))
  }
  if (is.na(word_index))
    stop("word_field column not found or invalid")
  if (!is.character(word_df[[word_index]]))
    stop("word_field must refer to a character mode column")


  # Function to get frequency for one word
  get_hit <- function(word, tokens){
    n <- quanteda::tokens_select(tokens,
                                 pattern = quanteda::phrase(word)) %>%
      quanteda::dfm() %>%
      quanteda::featfreq() %>%
      as.numeric()
    return(n)
  }


  # Check if some observations already have hits stored
  if("hits" %in% names(word_df) && replace == F){
    keep_df <- word_df %>% dplyr::filter(!is.na(hits))
    word_df %<>% dplyr::filter(is.na(hits))
  }

  # Get frequencies
  print("Counting word occurrences...")
  word_df$hits <- pbapply::pbsapply(word_df[[word_index]],
                                    get_hit,
                                    tokens = tokens) %>%
                                                lapply(function(l) l[][1]) %>%
                                                as.numeric() %>%
                                                tidyr::replace_na(0)

  # Merge with keep_df
  if("hits" %in% names(word_df) && replace == F){
    # dirty bugfix
    if(!exists("keep_df")){
      keep_df <- word_df[0,]
    }
    word_df <- dplyr::bind_rows(word_df, keep_df)
  }

  # arrange by number of hits
  ix <- order(word_df[, word_field])
  return(word_df[ix, ])
}




