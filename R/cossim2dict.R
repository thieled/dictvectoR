#' @title Similarity of documents to a dictionary
#' @description Computes the cosinal similarity between the average word vector
#' representation of each document in a data frame and the average
#' word vector representation of a dictionary, using fasttext word vector model.
#' @details
#' Implements the method called 'Distributed Dictionary Representation' (DDR),
#' introduced by Garten et al. (2018).
#'
#' The average dictionary vector is calculated as the mean vector of all words in
#' a dictioary, stored as a character vector.
#' The document vectors are calculated as mean vectors of all words per observation
#' in a column named 'text' of a dataframe.
#' One row in this dataframe represents one document. Both, the average dictionary
#' vector and document vectors are L2 normalized.
#' The function returns the cosinal similarity to the dictionary vector for each
#' document in the dataframe.
#' @param df A dataframe containing one document per row.
#' @param dictionary A character vector containing the keywords of your dictionary.
#' @param model A fasttext model as loaded by \code{\link[fastrtext]{load_model}}.
#' @param text_field Name of column in \code{df} that contains the text of the documents.
#' Default is "text".
#' @param replace_na Specifies the value used to replace NAs. Default is 'mean-sd'.
#' Can take values:
#'  \itemize{
#'   \item 'mean-sd' (charcter): replace NAs by mean - 1sd. Default.
#'   \item 'min' (charcter): replace NAs by minimum.
#'   \item 0 (numerical): replace NAs by 0.
#'   \item FALSE (logical): do not replace NAs.
#'   }
#' @return Numerical. Cosinal similarity, ranging (theoretically) from -1 to +1.
#' Indicating the similarity between the average fasttext word vector of all words
#' in `dictionary`
#' and the average fasttext word vector of each document in `df`.
#' @examples
#' model <- fastrtext::load_model(system.file("extdata",
#' "tw_demo_model_sml.bin", package = "dictvectoR"))
#' tw_annot %<>% head(100) %>% clean_text(remove_stopwords = TRUE,
#'                                        text_field = "full_text")
#' dict <- c("skandal", "deutschland", "steuerzahler")
#' tw_annot$ddr <- cossim2dict(tw_annot, dict, model)
#' @importFrom magrittr `%>%`
#' @importFrom magrittr `%<>%`
#' @export
#' @importFrom fastrtext get_sentence_representation
#' @references
#' Garten, J., Hoover, J., Johnson, K. M., Boghrati, R., Iskiwitch, C., &
#' Dehghani, M. (2018). Dictionaries and distributions: Combining expert knowledge
#'  and large scale textual data content analysis.
#'  Behavior Research Methods, 50(1), 344 - 361.
#'   \url{https://doi.org/10.3758/s13428-017-0875-9}
cossim2dict <- function(df,
                        dictionary,
                        model,
                        text_field = "text",
                        replace_na = c('mean-sd', 'min', 0, F)){

  ## Check missing columns
  if (missing(df)) {
    stop("You need to provide a data frame.")
  }
  if (missing(dictionary)) {
    stop("You need to provide a dictionary.")
  }
  if (missing(model)) {
    stop("You need to provide a fasttext model.")
  }

  ## Use text_field column if specified
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

  # Return average dictionary representation from fasttext model
  dic_rep <- fastrtext::get_sentence_representation(model, dictionary) %>%
    colMeans() %>%
    Matrix::Matrix(sparse = T, nrow = 1) %>%
    normalize() # get sentence representation for each expression in dictionary

  # Return average document representation
  doc_rep <- fastrtext::get_sentence_representation(model, df[[text_index]]) %>%
    Matrix::Matrix(sparse = T) %>%
    normalize() # get sentence representation for each document

  # Compute cosine similarity between each document representation and each
  result <- proxyC::simil(doc_rep, dic_rep, 1, method = "cosine", use_nan = T) %>%
    as.numeric()

  # Replace NAs
  if (missing(replace_na)) {
    replace_na = "mean-sd"
  }
  result <- repl_na(result, replace_na = replace_na)

  return(result)

}







