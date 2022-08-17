#' Vector representation of a corpus.
#'
#' Returns the average word-vector representation of a text column in a data frame,
#'  using a `fastText` model.
#' @param df A [data.frame] with a column containing text identified by `text_field`.
#' @param text_field A [character] string indicating the name of the text column in df.
#' @param model A fastText model, loaded by [fastrtext::load_model()].
#' @param normalize Logical. Default `TRUE`. Normalize the vectors to their Euclidean norm?
#' @returns A single-row sparse matrix of class \linkS4class{dgCMatrix}
#' as returned by \code{\link[Matrix]{Matrix}}.
#' @examples
#' model <- fastrtext::load_model(system.file("extdata",
#'                                            "tw_demo_model_sml.bin",
#'                                             package = "dictvectoR"))
#' tw_annot <- tw_annot %>% head(15) %>% clean_text(text_field = "full_text")
#' corpus_rep <- get_corpus_representation(tw_annot, model)
#' @export
#' @importFrom magrittr `%>%`
#' @importFrom magrittr `%<>%`
#' @seealso \code{\link[fastrtext]{get_sentence_representation}}, \code{\link{get_word_representations}}
get_corpus_representation <- function(df,
                                      model,
                                      text_field = "text",
                                      normalize = T){
  # get text_index
  text_index <- 0
  if (is.character(text_field)) {
    text_index <- match(text_field, names(df))
  }
  if (is.na(text_index))
    stop("text_field column not found")
  if (!is.character(df[[text_index]]))
    stop("text_field must refer to a character mode column")

  # Return average corpus representation from fasttext model
  corpus_rep <- fastrtext::get_sentence_representation(model, df[[text_index]]) %>%
    colMeans() %>%
    Matrix::Matrix(sparse = T, nrow = 1) %>%
    normalize()

  # Normalize
  if(normalize == T){
    corpus_rep <- normalize(corpus_rep)
  }

  return(corpus_rep)
}



#' Get word representations.
#'
#' Wrapper around \code{\link[fastrtext]{get_sentence_representation}} to return
#'  the `fastText` word-vector representation
#' of words or multiwords, stored in a column of a data frame `'word_df`.
#' @param word_df A [data.frame] containing a column with words or multiword expressions.
#' @param word_field A [character] string indicating the name of the column
#' in word_df that contains the words.
#' @param model A fastText model, loaded by [fastrtext::load_model()].
#' @param normalize Logical. Default `TRUE`. Normalize the vectors to their Euclidean norm?
#' @returns A sparse matrix of class \linkS4class{dgCMatrix} as
#' returned by \code{\link[Matrix]{Matrix}}.
#' Has the same number of rows as `word_df` and same number of columns
#' as dimensions of the `model`.
#' @examples
#' model <- fastrtext::load_model(system.file("extdata",
#'                                            "tw_demo_model_sml.bin",
#'                                             package = "dictvectoR"))
#' word_df <- data.frame(words = c("das ist", "ein", "test"))
#' word_rep <- get_word_representations(word_df, model)
#' @export
#' @importFrom magrittr `%>%`
#' @importFrom magrittr `%<>%`
#' @seealso \code{\link[fastrtext]{get_sentence_representation}}, \code{\link{get_corpus_representation}}
get_word_representations <- function(word_df,
                                    model,
                                    word_field = 'words',
                                    normalize = T){
  # Define word index in word_df
  word_index <- 0
  if (is.character(word_field)) {
    word_index <- match(word_field, names(word_df))
  }
  if (is.na(word_index))
    stop("word_field column not found or invalid")
  if (!is.character(word_df[[word_index]]))
    stop("word_field must refer to a character mode column")

  # Return word representation from fasttext model
  word_rep <- fastrtext::get_sentence_representation(model, word_df[[word_index]]) %>%
    Matrix::Matrix(sparse = T)

  if(normalize == T){
    word_rep <- normalize(word_rep)
  }

  return(word_rep)
}



#' Cosine similarity between words and a given vector.
#'
#' Returns the cosine similarity of each word in a data frame and a given vector
#'  representation.
#' @param word_df A [data.frame] containing a column with words or multiword expressions.
#' @param word_field A [character] string indicating the name of the column in
#' word_df that contains the words.
#' @param rep A given word-vector representation, stored as numerical vector,
#' matrix, or sparse matrix object. `length`, resp. `ncol` of `rep` must be equal
#'  to the dimensions of the used `fastText` model.
#' @param model A fastText model, loaded by [fastrtext::load_model()].
#' @export
#' @importFrom magrittr `%>%`
#' @importFrom magrittr `%<>%`
#' @seealso \code{\link{get_corpus_representation}}, [find_distinctive()]
#' @examples
#' model <- fastrtext::load_model(system.file("extdata",
#'                                "tw_demo_model_sml.bin",
#'                                 package = "dictvectoR"))
#' pop_rep <- tw_annot %>%
#'            dplyr::filter(pop == 1) %>%
#'            clean_text(text_field = "full_text") %>%
#'            get_corpus_representation(model = model)
#' words_df <- data.frame(words = c("coronadeutschland", "skandal"))
#' words_df$popsimil <- simil_words2rep(words_df,
#'                                      word_field = "words",
#'                                      rep = pop_rep,
#'                                      model)
simil_words2rep <- function(word_df, word_field = "words", rep, model){

  rep <- normalize(rep) %>% Matrix::Matrix(sparse = T) # Make sure that corups representation is normalized and of the same type of matrix
  word_rep <- get_word_representations(word_df = word_df,
                                       word_field = word_field,
                                       model = model,
                                       normalize = T) %>%
                     Matrix::Matrix(sparse = T)

   # Compute cosine similarity between each document representation and each
  result <- proxyC::simil(word_rep, rep, 1, method = "cosine", use_nan = T) %>% as.numeric()
  return(result) # return the result
}





#' Find distinctive keywords
#'
#' Compares a word-vector representations of words to the representations of
#' an annotated data.frame of texts.
#' Aims to detect words that distinctively characterize a concept.
#'
#' @details Takes an annotated data.frame `df` of texts as input.
#' The varialbe specified by `concept_field` in this df indicates the presence or
#'  absence of a theoretical concept in the text.
#' Two average word-vector representations are computed, using a fastText `model`:
#' One for all texts that contain the concept, and one for those that do not.
#' A second data.frame, `word_df`, contains one (multi-)word per row in `'word_field'`.
#' Three new columns in `word_df` are created: The first, ending with `'_possim'`,
#' indicates the cosine similarity between the
#' word and the positive concept corpus. The second `'_negsim'`,
#' indicates the similarity the remaining corpus.
#' The third, ending with `'_distinctive'` is the difference between the two.
#' @param df A data.frame containing one annotated document per row.
#' @param concept_field character. Name of the column that contains a binary,
#'  (hand-coded) indicator of the presence of absence of a concept.
#' @param text_field character. Name of column in `df` that contains
#'  the text of the documents. Default is "text".
#' @param word_df A [data.frame] containing a column with words or multi-word expressions.
#' @param word_field character. The name of the column in word_df that contains the words.
#' @param model A fastText model, loaded by [fastrtext::load_model()].
#' @export
#' @importFrom magrittr `%>%`
#' @importFrom magrittr `%<>%`
#' @seealso [get_corpus_representation()], [find_distinctive()]
#' @examples
#' model <- fastrtext::load_model(system.file("extdata",
#'                                "tw_demo_model_sml.bin",
#'                                 package = "dictvectoR"))
#' tw_annot %<>% clean_text(text_field = "full_text")
#' word_df <- data.frame(words = c("skandal", "deutschland", "wundervoll"))
#' find_distinctive(tw_annot,
#'                  "pop",
#'                   word_df = word_df,
#'                   model = model)
find_distinctive <- function(df,
                             concept_field,
                             text_field = "text",
                             word_df,
                             word_field = "words",
                             model){

  # Define concept variable index
  concept_index <- 0
  if (is.character(concept_field)) {
    concept_index <- match(concept_field, names(df))
  }
  if (is.na(concept_index))
    stop("concept_field column not found")

  # Split df into postive and negative concept dfs
  pos_concept_df <- df %>% dplyr::filter(dplyr::if_any(dplyr::all_of(concept_index), ~ . == 1))
  neg_concept_df <- df %>% dplyr::filter(dplyr::if_any(dplyr::all_of(concept_index), ~ . == 0))

  # Get corpus representation for postive and negative concept corpora
  pos_rep <- get_corpus_representation(df = pos_concept_df,
                                       model = model,
                                       text_field = text_field)
  neg_rep <- get_corpus_representation(df = neg_concept_df,
                                       model = model,
                                       text_field = text_field)

  # Get cosine similarity to positive and negative corpus for a data frame of words
  word_df[[paste0(concept_field, "_possim")]] <- simil_words2rep(word_df = word_df,
                                                                 word_field = word_field,
                                                                 pos_rep,
                                                                 model = model)
  word_df[[paste0(concept_field, "_negsim")]] <- simil_words2rep(word_df = word_df,
                                                                 word_field = word_field,
                                                                 neg_rep,
                                                                 model = model)

  word_df[[paste0(concept_field, "_distinctive")]]  <- word_df[[paste0(concept_field, "_possim")]] - word_df[[paste0(concept_field, "_negsim")]]

  # order
  ix <- order(word_df[, paste0(concept_field, "_distinctive")], decreasing = TRUE)
  return(word_df[ix, ])
}






