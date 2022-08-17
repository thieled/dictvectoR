#'Get binary prediction
#'
#'Returns the prediction of a binary variable by a gradual measurement using
#'logistic regression.
#'@details Predictions are considered '1' if the predicted probability is >= .5.
#'@param data A data frame.
#'@param dv A binary variable in that data frame.
#'@param iv A gradual variable in that data frame used for predicting `dv`.
#'@return A factor with levels 0, 1.
#'@seealso [stats::glm()]
#'@importFrom magrittr `%>%`
#'@export
#'@examples
#'data(mtcars)
#'mtcars$pred <- get_prediction(mtcars, mtcars$am, mtcars$drat)
get_prediction <- function(data, dv, iv) {
  logit <- stats::glm(dv ~ iv, data = data, family = "binomial")
  probabilities <- stats::predict(logit, data, type = "response")
  predicted.classes <- factor(ifelse(probabilities >= 0.5, 1, 0))
  return(predicted.classes)
}




#'Get confusion matrix
#'
#'Returns the confusion matrix and validation scores for a binary reference variable,
#' and a binary prediction variable.
#'@param reference A binary reference vector.
#'@param prediction A binary prediction vector.
#'@details The values of `reference` and `prediction` must be 0 or 1. Both variables
#' can be stored in a data.frame or as vectors of equal length. Can be of type numeric
#'  or factor.
#'@importFrom caret confusionMatrix
#'@returns A `confusionMatrix` object from [caret::confusionMatrix()].
#'@seealso [get_prediction()], [caret::confusionMatrix()]
#'@export
#'@examples
#' mtcars$pred <- get_prediction(mtcars, mtcars$am, mtcars$drat)
#' confuse(mtcars$am, mtcars$pred)
confuse <- function(reference, prediction){
  confusion_matrix <- table(factor(prediction), factor(reference)) # caret order: prediction, truth
  caret::confusionMatrix(confusion_matrix, positive = "1", mode = "prec_recall")
}



#'Get Accuracy, Recall, Precision, and F1
#'
#' Returns Accuracy, Recall, Precision, and F1 scores for a binary reference variable
#'  and a binary prediction variable.
#'@param reference A binary reference vector.
#'@param prediction A binary prediction vector.
#'@details The values of `reference` and `prediction` must be 0 or 1.
#'Both variables can be stored in a data.frame or as vectors of equal length.
#'Can be of type numeric or factor.
#'@returns A data.frame with columns 'accuracy', 'recall', 'precision', and 'F1'.
#'@seealso [confuse()], [get_prediction()], [caret::confusionMatrix()]
#'@export
#'@examples
#' mtcars$pred <- get_prediction(mtcars, mtcars$am, mtcars$drat)
#' get_ARPF(mtcars$am, mtcars$pred)
get_ARPF <- function(reference, prediction){
  c <- confuse(reference, prediction)
  data.frame(accuracy = c$overall[["Accuracy"]],
             recall = c$byClass[["Recall"]],
             precision = c$byClass[["Precision"]],
             F1 = c$byClass[["F1"]])
}



#' Get F1 score for a DDR measure
#'
#' Returns the F1 score for a DDR measurement in predicting a binary reference
#' (i.e. a manually annotated variable).
#' @details The gradual DDR measurement is passed to [get_prediction()] to obtain a binary
#'  prediction through logistic regression.
#' The F1 scores indicate the performance of these words/dictionaries in predicting a binary
#'  coding, when used in the DDR method.
#' The F1 score is the harmonic mean between Recall and Precision (1).
#'@references
#'(1) Chinchor, N. (1992). MUC-4 evaluation metrics. Proceedings of the 4th
#'Conference on Message Understanding, 22–29.
#'\url{https://doi.org/10.3115/1072064.1072067}
#'@seealso [cossim2dict()], [get_prediction()], [get_many_F1s()], [get_many_RPFs()], [caret::confusionMatrix()]
#' @param df A data.frame containing one annotated document or sentence per row.
#' @param dictionary A character vector containing the keywords of a dictionary,
#'  passed to \code{\link{cossim2dict}}.
#' @param model A fasttext model as loaded by \code{\link[fastrtext]{load_model}}.
#' @param text_field Name of column in \code{df} that contains the text of the documents.
#' Default is "text".
#' @param reference Name of the binary reference column in df.
#' @param replace_na Specifies the value used to replace NAs in the DDR measurement.
#' Default is 'mean-sd'. Can take values:
#'  \itemize{
#'   \item `'mean-sd'` (charcter): replace NAs by mean - 1sd. Default.
#'   \item `'min'` (charcter): replace NAs by minimum.
#'   \item `0` (numerical): replace NAs by 0.
#'   \item `FALSE` (logical): do not replace NAs.
#' }
#' @importFrom magrittr `%>%`
#' @importFrom magrittr `%<>%`
#' @export
#' @examples
#' model <- fastrtext::load_model(system.file("extdata",
#'                                "tw_demo_model_sml.bin",
#'                                 package = "dictvectoR"))
#' tw_annot %<>% clean_text(text_field = "full_text")
#' dict <- c("skandal", "deutschland", "steuerzahler")
#' get_F1(tw_annot, dict, model, 'pop')
get_F1 <- function(df,
                       dictionary,
                       model,
                       reference,
                       text_field = "text",
                       replace_na = c('mean-sd', 'min', 0, F)
                   ){

  # Find index of reference variable
  if(!is.null(reference)){
    reference_index <- 0
    reference_index <- match(reference, names(df))
    if (is.na(reference))
      stop("reference column not found")
    if (!is.numeric(df[[reference_index]]))
      stop("compare_by must refer to a numeric mode column")
  }

  # Default for replace NA
  if (missing(replace_na)) {
    replace_na = "mean-sd"
  }

  # Get cosine similarity of each word to document
  df$x <- cossim2dict(df = df,
                      dictionary = dictionary,
                      model = model,
                      text_field = text_field,
                      replace_na = replace_na)

  df$pred <- tryCatch({get_prediction(df,
                                      dv = df[[reference_index]],
                                      iv = df$x)}, error = function(e) {0}) # Get predictions
  confusionmatrix <- tryCatch({confuse(reference = df[[reference_index]],
                                       prediction = df$pred)},
                              error = function(e) {NA}) # compare

  suppressWarnings(if(is.na(confusionmatrix[[1]])){
    result <- 0
    }else{
    result <- confusionmatrix$byClass[["F1"]]
    }
  )

  return(result)
}








#' Get F1 scores for many words or dictionaries.
#'
#' Efficiently computes F1 scores for all elements of a vector containing keywords,
#' or a list containing dictionaries, when used in DDR method.
#'
#' @details
#' A numerical F1 score is returned for each element (i.e. word or dictionary) of
#' the vector or list.
#' The F1 scores indicate the performance of these words/dictionaries in predicting
#' a binary coding, when used in the DDR method.
#' The resulting gradual measure from the DDR measure is passed to a logistic regression,
#'  with the binary coding as dependent variable.
#' Binary predictions are calculated from this logistic model and compared with the binary
#'  coding.
#' The F1 score is the harmonic mean between Recall and Precision (1).
#'
#'@references
#'(1) Chinchor, N. (1992). MUC-4 evaluation metrics. Proceedings of the 4th
#'Conference on Message Understanding, 22–29.
#'\url{https://doi.org/10.3115/1072064.1072067}
#'@seealso [cossim2dict()], [get_prediction()], [get_F1()], [get_many_RPFs()]
#' @param words A character vector containing keywords, or a list of character vectors
#'  containing dictionaries.
#' @param model A fastText model, loaded by [fastrtext::load_model()].
#' @param df A data.frame containing one annotated document per row.
#' @param reference Name of the binary reference column in `df` (character).
#' @param text_field Name of column in \code{df} that contains the text of the documents.
#'  Default is "text".
#' @param replace_na Specifies the value used to replace NAs in the DDR measurement.
#'  Default is 'mean-sd'. Can take values:
#'  \itemize{
#'   \item `'mean-sd'` (charcter): replace NAs by mean - 1sd. Default.
#'   \item `'min'` (charcter): replace NAs by minimum.
#'   \item `0` (numerical): replace NAs by 0.
#'   \item `FALSE` (logical): do not replace NAs.
#' }
#'@importFrom magrittr `%>%`
#'@importFrom magrittr `%<>%`
#'@export
#'@examples
#' model <- fastrtext::load_model(system.file("extdata",
#'                                "tw_demo_model_sml.bin",
#'                                 package = "dictvectoR"))
#' tw_annot %<>% clean_text(text_field = "full_text")
#' dict_df <- data.frame(id = 1:3)
#' dict_df$combis <- list(c("mehrheit deutschen", "merkel", "skandal"),
#'                       c("steuerzahler", "bundesregierung",
#'                       "komplett gescheitert"),
#'                       c( "arbeitnehmer", "groko", "wahnsinn"))
#' dict_df$F1 <- get_many_F1s(dict_df$combis,
#'                            model = model,
#'                            df = tw_annot,
#'                            reference = "pop")
get_many_F1s <- function(words,
                         model,
                         df,
                         reference,
                         text_field = "text",
                         replace_na = c('mean-sd', 'min', 0, F)
                         ){

  # Find index of reference variable
  if(!is.null(reference)){
    reference_index <- 0
    reference_index <- match(reference, names(df))
    if (is.na(reference))
      stop("reference column not found")
    if (!is.numeric(df[[reference_index]]))
      stop("compare_by must refer to a numeric mode column")
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

  # Set default for replace NA
  if (missing(replace_na)) {
    replace_na = "mean-sd"
  }

  # Calculate document representation once
  doc_rep <- fastrtext::get_sentence_representation(model, df[[text_index]]) %>%
                                                  Matrix::Matrix(sparse = T) %>%
                                                  normalize()

  # Slightly tweaked cossim2dict function - applicable for previously queried document representation
  cossim2dict_faster <- function(doc_rep,
                                        dictionary,
                                        model,
                                        replace_na = c('mean-sd', 'min', 0, F)
                                        ){

           # Return average dictionary representation from fasttext model
           dic_rep <- fastrtext::get_sentence_representation(model, dictionary) %>%
                      colMeans() %>%
                      Matrix::Matrix(sparse = T, nrow = 1) %>%
                      normalize()# get dict rep

           # Compute cosine similarity between each document representation and each
           result <- proxyC::simil(x = doc_rep,
                                   y = dic_rep,
                                   1,
                                   method = "cosine",
                                   use_nan = T) %>% as.numeric()

           # Replace NAs
           if (missing(replace_na)) {
                      replace_na = "mean-sd"
                    }
            result <- repl_na(result, replace_na = replace_na)

            return(result) # return the result

            }


  # Function to get F1 score for one dictionary
  f1 <- function(df = df,
                 doc_rep = doc_rep,
                 dictionary = dictionary,
                 model = model,
                 replace_na = replace_na,
                 reference_index = reference_index){

                  # Get cosine similarity of each word to document
                  df$x <- cossim2dict_faster(doc_rep = doc_rep,
                                            dictionary = dictionary,
                                            model = model,
                                            replace_na = replace_na)

                  df$pred <- tryCatch({get_prediction(df,
                                                      dv = df[[reference_index]],
                                                      iv = df$x)}, error = function(e) {0}) # Get predictions
                  confusionmatrix <- tryCatch({confuse(reference = df[[reference_index]],
                                                       prediction = df$pred)},
                                              error = function(e) {NA}) # compare

                  suppressWarnings(if(is.na(confusionmatrix[[1]])){
                    result <- 0
                  }else{
                    result <- confusionmatrix$byClass[["F1"]]
                  }
                  )
                return(result)
              }


  # Apply on list, show progressbar
  res <- pbapply::pbsapply(words,
                           f1,
                           df = df,
                           doc_rep = doc_rep,
                           model = model,
                           replace_na = replace_na,
                           reference_index = reference_index)

  res %<>% tidyr::replace_na(replace = 0)

  return(res)

}








#' Get Recall, Precision, F1 for many
#'
#' Efficiently computes Recall, Precision, and F1 scores for a character vector of
#' keywords stored in a data.frame,
#' or a [list] of dictionaries stored in a data.frame. Adds Recall, Precision, and F1
#'  to the data.frame.
#'
#' @details For each element (i.e. word or dictionary) of the character vector or list
#'  in the data.frame .
#' The F1 scores indicate the performance of these words/dictionaries in predicting a
#'  binary coding, when used in the DDR method.
#' The resulting gradual measure from the DDR measure is passed to a logistic regression,
#'  with the binary coding as dependent variable.
#' Binary predictions are calculated from this logistic model and compared with the
#'  binary coding.
#' The F1 score is the harmonic mean between Recall and Precision (Chinchor, 1992).
#'
#'@references
#'Chinchor, N. (1992). MUC-4 evaluation metrics. Proceedings of the
#'4th Conference on Message Understanding, 22–29.
#'\url{https://doi.org/10.3115/1072064.1072067}
#'@seealso \code{\link{cossim2dict}}, \code{\link{get_prediction}}, \code{\link{get_F1}}, \code{\link{get_many_RPFs}}, \code{\link[caret]{confusionMatrix}}

#' @param keyword_df A data.frame, containing a column with a
#' character vector of words, or with a list of dictionaries.
#' @param keyword_field character. The name of the column in `keyword_df` that is
#' either a character vector of single keywords,
#' or a [list] of dictionaries, stored as separate character vectors with one
#' element per word.
#' @param model A fastText model, loaded by \code{\link[fastrtext]{load_model}}.
#' @param text_df A data.frame containing one annotated document per row.
#' @param reference Name of the binary reference column in `df` (character).
#' @param text_field Name of column in \code{df} that contains the text of the documents.
#'  Default is "text".
#' @param replace_na Specifies the value used to replace NAs in the DDR measurement.
#' Default is 'mean-sd'. Can take values:
#'  \itemize{
#'   \item `'mean-sd'` (charcter): replace NAs by mean - 1sd. Default.
#'   \item `'min'` (charcter): replace NAs by minimum.
#'   \item `0` (numerical): replace NAs by 0.
#'   \item `FALSE` (logical): do not replace NAs.
#' }
#'@seealso \code{\link{cossim2dict}}, \code{\link{get_prediction}}, \code{\link{get_many_F1s}}
#'@importFrom magrittr `%>%`
#'@importFrom magrittr `%<>%`
#'@export
#' @examples
#' model <- fastrtext::load_model(system.file("extdata",
#'                                "tw_demo_model_sml.bin",
#'                                 package = "dictvectoR"))
#' tw_annot %<>% clean_text(text_field = "full_text")
#' dict_df <- data.frame(id = 1:3)
#' dict_df$combis <- list(c("mehrheit deutschen", "merkel", "skandal"),
#'                       c("steuerzahler", "bundesregierung",
#'                       "komplett gescheitert"),
#'                       c( "arbeitnehmer", "groko", "wahnsinn"))
#' get_many_RPFs(keyword_df = dict_df,
#'        keyword_field = "combis",
#'        model = model,
#'        text_df = tw_annot, reference = "pop")
get_many_RPFs <- function(keyword_df,
                          keyword_field = "words",
                          model,
                          text_df,
                          reference,
                          text_field = "text",
                          replace_na = c('mean-sd', 'min', 0, F)
                          ){

  # Add id to keyword_df if not there
  keyword_df %<>% add_id()

  # Find index in keyword_df for words or dictionaries
  keyword_index <- 0
  keyword_index <- match(keyword_field, names(keyword_df))
  if (is.na(keyword_index))
    stop("keyword column not found or invalid")


  # Find index of reference variable
  if(!is.null(reference)){
    reference_index <- 0
    reference_index <- match(reference, names(text_df))
    if (is.na(reference))
      stop("reference column not found")
    if (!is.numeric(text_df[[reference_index]]))
      stop("compare_by must refer to a numeric mode column")
  }

  ## Use text_field column if specified
  text_index <- 0
  if (length(text_field) != 1)
    stop("text_field must refer to a single column")
  if (is.character(text_field)) {
    text_index <- match(text_field, names(text_df))
  } else {
    text_index <- match(text_field, seq(length(text_df)))
  }
  if (is.na(text_index))
    stop("text_field column not found or invalid")
  if (!is.character(text_df[[text_index]]))
    stop("text_field must refer to a character mode column")

  # Set default for replace NA
  if (missing(replace_na)) {
    replace_na = "mean-sd"
  }



  # Calculate document representation once
  doc_rep <- fastrtext::get_sentence_representation(model, text_df[[text_index]]) %>%
    Matrix::Matrix(sparse = T) %>% normalize() # get doc representation

  # Slightly tweaked cossim2dict function - applicable for previously queried document
  representation
  cossim2dict_faster <- function(doc_rep,
                                 dictionary,
                                 model,
                                 replace_na = c('mean-sd', 'min', 0, F)
  ){

    # Return average dictionary representation from fasttext model
    dic_rep <- fastrtext::get_sentence_representation(model, dictionary) %>%
      colMeans() %>%
      Matrix::Matrix(sparse = T, nrow = 1) %>%
      normalize()# get dict rep

    # Compute cosine similarity between each document representation and each
    result <- proxyC::simil(x = doc_rep,
                            y = dic_rep,
                            1,
                            method = "cosine",
                            use_nan = T) %>%
      as.numeric()

    # Replace NAs
    if (missing(replace_na)) {
      replace_na = "mean-sd"
    }
    result <- repl_na(result, replace_na = replace_na)

    return(result) # return the result

  }


              # Function to get Recall, Precision and F1
              get_rpf <- function(text_df = text_df,
                                  doc_rep = doc_rep,
                                  dictionary = dictionary,
                                  model = model,
                                  replace_na = replace_na,
                                  reference_index = reference_index){

                # Get cosine similarity of each word to document
                text_df$x <- cossim2dict_faster(doc_rep = doc_rep,
                                           dictionary = dictionary,
                                           model = model,
                                           replace_na = replace_na)

                text_df$pred <- tryCatch({get_prediction(text_df,
                                                         dv = text_df[[reference_index]],
                                                         iv = text_df$x)},
                                         error = function(e) {0}) # Get predictions
                confusionmatrix <- tryCatch({confuse(reference = text_df[[reference_index]],
                                                     prediction = text_df$pred)},
                                            error = function(e) {NA}) # compare

                suppressWarnings(if(is.na(confusionmatrix[[1]])){  ### cond > 1 --> error
                  result <- data.frame(recall = 0, precision = 0, F1 = 0)
                }else{
                  result <- data.frame(recall = confusionmatrix$byClass[["Recall"]],
                                       precision = confusionmatrix$byClass[["Precision"]],
                                       F1 = confusionmatrix$byClass[["F1"]])
                }
                )
                return(result)
              }

  # Apply on list/vector, show progressbar
  res <- pbapply::pblapply(keyword_df[[keyword_index]],
                           get_rpf,
                           text_df = text_df,
                           doc_rep = doc_rep,
                           model = model,
                           replace_na = replace_na,
                           reference_index = reference_index)

  # Bind as data.frame
  res %<>% dplyr::bind_rows()

  # Replace any NAs with 0
  res %<>% dplyr::mutate(
    dplyr::across(dplyr::everything(), ~tidyr::replace_na(.x, 0)) # replace NAs with 0
  )

  # Add row id
  keyword_df %<>% tibble::rowid_to_column()
  res %<>% tibble::rowid_to_column()

  # Merge dfs
  keyword_df <- dplyr::left_join(keyword_df, res)
  keyword_df %<>% dplyr::select(-rowid)

  return(keyword_df)

}








#' Get F1 for many by a grouping varialbe
#'
#' Efficiently computes F1 scores for a character vector of keywords stored in a
#' data.frame,
#' or a [list] of dictionaries stored in a data.frame - for a reference data.frame
#' grouped by `group_field`.
#' ADD DETAILS....
#'
#'#'@seealso \code{\link{cossim2dict}}, \code{\link{get_prediction}}, \code{\link{get_F1}}, \code{\link{get_many_RPFs}}, \code{\link[caret]{confusionMatrix}}

#' @param keyword_df A data.frame, containing a column with a character vector of words,
#' or with a list of dictionaries.
#' @param keyword_field character. The name of the column in `keyword_df` that is either
#'  a character vector of single keywords,
#' or a [list] of dictionaries, stored as separate character vectors with one element
#'  per word.
#' @param id A unique identifier for each keyword.
#' @param model A fastText model, loaded by \code{\link[fastrtext]{load_model}}.
#' @param text_df A data.frame containing one annotated document per row.
#' @param reference character. Name of the binary reference column in `df`.
#' @param group_field character. Name of a categorical grouping variable in `df`.
#' @param text_field Name of column in \code{df} that contains the text of the documents.
#'  Default is "text".
#' @param replace_na Specifies the value used to replace NAs in the DDR measurement.
#'  Default is 'mean-sd'. Can take values:
#'  \itemize{
#'   \item `'mean-sd'` (charcter): replace NAs by mean - 1sd. Default.
#'   \item `'min'` (charcter): replace NAs by minimum.
#'   \item `0` (numerical): replace NAs by 0.
#'   \item `FALSE` (logical): do not replace NAs.
#' }
#'@seealso \code{\link{cossim2dict}}, \code{\link{get_prediction}}, \code{\link{get_many_F1s}}
#'@importFrom magrittr `%>%`
#'@importFrom magrittr `%<>%`
#'@export
#' @examples
#' model <- fastrtext::load_model(system.file("extdata",
#'                                "tw_demo_model_sml.bin",
#'                                 package = "dictvectoR"))
#' tw_annot %<>% clean_text(text_field = "full_text")
#' dict_df <- data.frame(id = 1:3)
#' dict_df$combis <- list(c("mehrheit deutschen", "merkel", "skandal"),
#'                       c("steuerzahler", "bundesregierung",
#'                       "komplett gescheitert"),
#'                       c( "arbeitnehmer", "groko", "wahnsinn"))
#' get_many_F1s_by_group(keyword_df = dict_df,
#'                      keyword_field = "combis",
#'                      id = "id",
#'                      model = model,
#'                      text_df = tw_annot,
#'                      group_field = "party",
#'                      reference = 'pop')
#'
get_many_F1s_by_group <- function(keyword_df,
                                  keyword_field = "words",
                                  id = "id",
                                  model,
                                  text_df,
                                  group_field,
                                  reference,
                                  text_field = "text",
                                  replace_na = c('mean-sd', 'min', 0, F)){

  # Find index in keyword_df for id
  id_index <- 0
  id_index <- match(id, names(keyword_df))
  if (is.na(id))
    stop("id column not found or invalid")


  # Find index in keyword_df for words or dictionaries
  keyword_index <- 0
  keyword_index <- match(keyword_field, names(keyword_df))
  if (is.na(keyword_index))
    stop("keyword column not found or invalid")


  # Get dictionary representation function
  get_dict_reps <- function(model, dict){
    rep <- fastrtext::get_sentence_representation(model, dict) %>%
      colMeans() %>%
      Matrix::Matrix(sparse = T, nrow = 1) %>%
      normalize()# get dict rep
    return(rep)
  }

  # Get dictionary representations as list
  dict_reps <- pbapply::pbsapply(keyword_df[[keyword_index]],
                                 get_dict_reps,
                                 model = model)
  # Store IDs as names
  names(dict_reps) <- keyword_df[[id_index]]


  # Function to get many F1s from a list of dict reps
  get_many_F1s_from_dictreps <- function(dict_reps,
                                         model,
                                         df,
                                         reference,
                                         text_field = "text",
                                         replace_na = c('mean-sd', 'min', 0, F)) {

    # Find index of reference variable
    if(!is.null(reference)){
      reference_index <- 0
      reference_index <- match(reference, names(df))
      if (is.na(reference))
        stop("reference column not found")
      if (!is.numeric(df[[reference_index]]))
        stop("compare_by must refer to a numeric mode column")
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

    # Set default for replace NA
    if (missing(replace_na)) {
      replace_na = "mean-sd"
    }

    # Calculate document representation once
    doc_rep <- fastrtext::get_sentence_representation(model, df[[text_index]]) %>%
      Matrix::Matrix(sparse = T) %>%
      normalize()

    # Slightly tweaked cossim2dict function - applicable for previously queried document rep
    cossim2dict_fromdictrep <- function(doc_rep,
                                        dic_rep,
                                        model,
                                        replace_na = c('mean-sd', 'min', 0, F)
    ){


      # Compute cosine similarity between each document representation and each
      result <- proxyC::simil(x = doc_rep, y = dic_rep, 1, method = "cosine", use_nan = T) %>%
        as.numeric()

      # Replace NAs
      if (missing(replace_na)) {
        replace_na = "mean-sd"
      }
      result <- repl_na(result, replace_na = replace_na)

      return(result) # return the result

    }

    # Function to get F1 score for one dictionary
    f1 <- function(df = df,
                   doc_rep = doc_rep,
                   dic_rep = dic_rep,
                   model = model,
                   replace_na = replace_na,
                   reference_index = reference_index){

      # Get cosine similarity of each word to document
      df$x <- cossim2dict_fromdictrep(doc_rep = doc_rep,
                                      dic_rep = dic_rep,
                                      model = model,
                                      replace_na = replace_na)

      df$pred <- tryCatch({dictvectoR::get_prediction(df, dv = df[[reference_index]],
                                                      iv = df$x)},
                          error = function(e) {0}) # Get predictions
      confusionmatrix <- tryCatch({dictvectoR::confuse(reference = df[[reference_index]],
                                                       prediction = df$pred)},
                                  error = function(e) {NA}) # compare

      suppressWarnings(if(is.na(confusionmatrix[[1]])){
        result <- 0
      }else{
        result <- confusionmatrix$byClass[["F1"]]
      }
      )
      return(result)
    }

    # Apply on list, show progressbar
    F1 <- pbapply::pbsapply(dict_reps,
                            f1,
                            df = df,
                            doc_rep = doc_rep,
                            model = model,
                            replace_na = replace_na,
                            reference_index = reference_index)

    F1 %<>% tidyr::replace_na(replace = 0)


    ## Turn into dataframe
    F1 <- data.frame(F1)
    # Add id - by its original name
    F1[,paste(id)] <- names(dict_reps)

    return(F1)

  }

  # Map on df by grouping variable and apply
  results_df <- text_df %>%
    dplyr::group_by_at(group_field) %>%
    dplyr::group_modify(~ get_many_F1s_from_dictreps(df = .x,
                                                     dict_reps = dict_reps,
                                                     model = model,
                                                     reference = reference,
                                                     text_field = text_field,
                                                     replace_na = 'mean-sd'), .keep = T)  %>%
    tidyr::pivot_wider(names_from = 1, names_prefix = "F1_", values_from = F1)



  ## From keywords_df keep id and variables that are not in results_df
  v <- setdiff(names(keyword_df), names(results_df))
  v <- append(id, v)
  keyword_df <- keyword_df %>% dplyr::select(dplyr::all_of(v))
  keyword_df[,paste(id)] %<>% as.character()

  # Join
  join_df <- dplyr::left_join(keyword_df, results_df)

  return(join_df)

}



