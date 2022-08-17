#'Clean text
#'
#'Cleans text stored in a data frame. Function is tailored for German texts.
#'@param df A data frame.
#'@param text_field character. Default is 'text'. Name of a column in `df` that
#' contains text that should be cleaned. Default is 'text'.
#'@param clean_field character. Default is 'text'. Name given to a new column in `df`
#'that will contain the cleaned text.
#'@param tolower logical. Lowercase text? Default is TRUE.
#'@param remove_punct logical. Remove punctuation?  Default TRUE.
#'@param simplify_punct logical. Replace all !, :, ?, ... by a single . Default FALSE.
#'@param replace_emojis logical. Replace a list of emojis by German words for certain
#'emotions ('wut', 'angst', 'freude'). Default TRUE.
#'@param replace_numbers logical. Replace numbers by German words for those numbers.
#'Default TRUE.
#'@param remove_stopwords logical. Remove German stopwords from the 'nltk' list with
#'the exception of some words defined by the author. Default FALSE.
#'@param store_uncleaned logical. Save uncleaned text as 'uncleaned_text' column?
#'Default TRUE.
#'@param count logical. Count number of words and strings after cleaning? Will create
#'the columns 'n_words' and 'n_chars'. Default TRUE.
#'@return A data.frame with the same columns as the df. The cleaned text is in the
#'column specified by `text_field` or, if not specified in 'text.
#'@importFrom magrittr `%>%`
#'@importFrom magrittr `%<>%`
#'@export
#'@examples
#' tw_data %<>% head(10) %>% clean_text(text_field = 'full_text')
#' tw_data$text
clean_text <- function(df,
                       text_field = "text",
                       clean_field = "text",
                       tolower = T,
                       remove_punct = T,
                       simplify_punct = F,
                       replace_emojis = T,
                       replace_numbers = T,
                       remove_stopwords = F,
                       store_uncleaned = T,
                       count = T
){

  ## Use text_field column if specified
  text_index <- 0
  text_index <- match(text_field, names(df))
  if (is.na(text_index))
    stop("text_field column not found or invalid")
  if (!is.character(df[[text_index]]))
    stop("text_field must refer to a character mode column")

  # Make sure that text is UTF-8 ecnoded
  df[[text_index]] <- stringi::stri_enc_toutf8(df[[text_index]],
                                               is_unknown_8bit = F,
                                               validate = T)

  # Store uncleaned text
  if (store_uncleaned == T) {
    df$uncleaned_text <- df[[text_index]]
  }

  # Create column for cleaned text
  df[[clean_field]] <- df[[text_index]]

  ## Replace text_index using the new column
  text_index <- match(clean_field, names(df))

  # Set regular expressions for cleaning hyperlinks and other artifacts
  regex_links = stringr::regex("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)")
  regex_links2 = stringr::regex("(www\\.)(.*)(\\.)(.*)")
  regex_amp = stringr::regex(" amp | gt | lt ") # regex fo artifacts from  & > <
  regex_people = stringr::regex("@\\w+") # tags on Twitter


  # Replace emojis
  if (replace_emojis == T) {

    # Regexes for emoji - replacement (annotated by hand)
    regex_joy <- emoji_df %>%
      dplyr::filter(emotion == "joy") %>%
      dplyr::pull("code") %>%
      stringr::str_c(collapse = "|") %>%
      stringr::regex()
    regex_anger <- emoji_df %>%
      dplyr::filter(emotion == "anger") %>%
      dplyr::pull("code") %>%
      stringr::str_c(collapse = "|") %>%
      stringr::regex()
    regex_fear <- emoji_df %>%
      dplyr::filter(emotion == "fear") %>%
      dplyr::pull("code") %>%
      stringr::str_c(collapse = "|") %>%
      stringr::regex()

    # Germany flag
    regex_de <- stringr::regex("\\U0001F1E9\\U0001F1EA")


    df[[text_index]] %<>%
      stringr::str_replace_all(regex_anger, " wut ") %>% # replace all anger emojis
      stringr::str_replace_all(regex_fear, " angst ") %>% # replace all fear emojis
      stringr::str_replace_all(regex_joy, " freude ") %>%  # replace all joy emojis
      stringr::str_replace_all(regex_de, " deutschland ")

  }

  # lowercase
  if (tolower == T) {
    df[[text_index]] %<>% quanteda::char_tolower()
  }

  # Remove hyperlinks
  df[[text_index]] %<>% stringr::str_replace_all(regex_links, " ") %>%  # remove all html links
    stringr::str_replace_all(regex_links2, " ") %>% # remove all www links
    stringr::str_replace_all(regex_people, " ") # remove tagged people in tweets

  # Replace Numbers
  if (replace_numbers == T) {

    # Lood number patterns
    lookup <- number_patterns$replace
    names(lookup) <- number_patterns$string

    df[[text_index]] %<>% stringr::str_replace_all(lookup)

  }


  if(remove_stopwords == T){

    regex_de_stopwords = paste0('\\b',
                                paste(rev(de_stopw), collapse = '\\b|\\b'), '\\b')

    df[[text_index]] %<>% stringr::str_replace_all(regex_de_stopwords, " ")
  }


  if(remove_punct == T){
    df[[text_index]] %<>% stringr::str_replace_all("[^[:alpha:]]", " ")
  } else {
    df[[text_index]] %<>% stringr::str_replace_all("[^[:alpha:][:punct:]]", " ")
  }
  if(simplify_punct == T){
    df[[text_index]] %<>% stringr::str_replace_all("[\\.\\!\\?\\:]+", ". ") %>%
      stringr::str_replace_all("[^[:alpha:]\\.]", " ")
  }


  # Remove other artifacts
  df[[text_index]] %<>% stringr::str_replace_all("[\\x{1D400}-\\x{1D7FF}]", " ") %>% #rm maths
    stringr::str_replace_all(regex_amp, " ") %>% # remove ampersand artifacts
    stringr::str_replace_all("[ \t]{2,}", " ") %>% # remove tabs
    stringr::str_replace_all("^\\s+|\\s+$", "")  # remove all white space

  # Count
  if(count == T){
    df$n_words <- df[[text_index]] %>% stringr::str_count("\\w+")
    df$n_chars <- df[[text_index]] %>% stringr::str_length()
  }

  return(df)
}




#' Prepare text for fastText-model-training
#'
#' Helper function to prepare text for training a fastText model.
#' (Caution: Tailored to texts in German language).
#'
#' @details Takes a data.frame containing text. First checks the length of
#' the texts specified in `text_field` using \code{\link[quanteda]{nsentence}}.
#' Texts with more than 3 sentences are tokenized by \code{\link[quanteda]{tokens}}
#' into sentences.
#' All texts are passed to \code{\link{clean_text}} with the fixed settings:
#' \itemize{
#'   \item `tolower = T`
#'   \item `remove_punct = T`
#'   \item `replace_emojis = T`
#'   \item `replace_numbers = T`
#'   \item `remove_stopwords = F`
#'   \item `store_uncleaned = F`
#'   \item `count = T`
#'   }
#'  The cleaned, short texts are shuffled and returned as a character vector.
#' @returns A character vector that can be directly used to train a fasttext model
#' with \code{\link[fastrtext]{build_vectors}}.
#' @param df A data frame.
#' @param text_field Name of a column in that `df` that contains text that should be
#' cleaned. Default is 'text'.
#' @param seed Used for random shuffling. Default is 1.
#'
#' @importFrom magrittr `%>%`
#' @importFrom magrittr `%<>%`
#' @export
#' @seealso \code{\link[fastrtext]{build_vectors}}, \code{\link[quanteda]{tokens}}, \code{\link{clean_text}}
#' @examples
#' texts <- prepare_train_data(head(tw_data, 10), text_field = 'full_text')
#'
prepare_train_data <- function(df, text_field = "text", seed = 1){

  ## Use text_field column if specified
  # (Code re-used from quanteda package)
  text_index <- 0
  if (length(text_field) != 1)
    stop("text_field must refer to a single column")
  if (is.character(text_field)) {
    text_index <- match(text_field, names(df))
  } else {
    text_index <- match(text_field, seq(length(df)))
  }
  if (is.na(text_index))
    stop("text_field column not found or invalid")
  if (!is.character(df[[text_index]]))
    stop("text_field must refer to a character mode column")

  # Set text variable name
  df$text <- df[[text_index]]

  # Get sentence count using quanteda
  df$n_sentences <- quanteda::nsentence(df$text)

  # drop unneccessary variables
  df %<>% dplyr::select(text, n_sentences)

  # Split: short comments == 3 sentence & long comments
  df_long <- dplyr::filter(df, n_sentences > 3)
  df <- dplyr::filter(df, n_sentences <= 3) %>% dplyr::select(-n_sentences)

  if(nrow(df_long)>0){
          # Tokenize df_long to sentences using quanteda
          corpus_long <- quanteda::corpus(df_long)
          df_long <- quanteda::tokens(corpus_long, what = "sentence",
                                      include_docvars = F) %>%
                          unlist() %>%
                          as.data.frame(stringsAsFactors = F) %>%
                          dplyr::rename(text = 1)

          # Bind dfs
          df <- dplyr::bind_rows(df_long, df)
  }

 # Clean text
 df <- clean_text(df = df,
                  text_field = "text",
                  clean_field = "text",
                  tolower = T,
                  remove_punct = T,
                  replace_emojis = T,
                  replace_numbers = T,
                  remove_stopwords = F,
                  store_uncleaned = F,
                  count = T
                  )

# Filter out empty or very short observations
df %<>% dplyr::filter(n_words > 4)

# Drop duplicates
df %<>% dplyr::distinct(text)

# Assign random id
set.seed(seed)
df %<>% dplyr::mutate(random_id = sprintf("%10d", sample(nrow(df)))) %>%
  dplyr::arrange(random_id)

return(df$text)

}


