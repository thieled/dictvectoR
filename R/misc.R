# Eliminating 'no visible binding' note
utils::globalVariables(c("Freq",
  "Var1",
  "Var2",
  "combs",
  "from",
  "hits",
  "hits1",
  "hits2",
  "mw_id",
  "n_sentences",
  "n_words",
  "orig_id",
  "random_id",
  "rowid",
  "score1",
  "score2",
  "simil",
  "text",
  "toks",
  "w",
  "win",
  "wins_hits1",
  "wins_hits_pct",
  "wins_score1",
  "wins_score_pct",
  "word1",
  "word_id",
  "word_id1",
  "words",
  "F1",
  "emotion",
  "representation"))




#' Normalize a vector
#'
#' Normalizes a (matrix) vector to its Euclidean norm, or 'L2'-norm.
#' @param x a numeric vector, or a matrix of vectors.
#' @export
#' @details
#' The norm is defined as
#' \deqn{||x|| = \sqrt{\sum(x^2)}}
#' The Euclidean norm can be interpreted as the length of the connection
#' between zero and a point specified by the vector.
#' The normalized vector, then, is computed as:
#' \deqn{x' = x/||x||}
#' @examples
#' v <- rnorm(10)
#' normalize(v) == v/sqrt(sum(v^2))
normalize <- function(x){x/sqrt(sum(x^2))}




#' Filter by ntile.
#'
#' Filter a data frame for the top quantile of a specified variable.
#' @param df A [data.frame].
#' @param var_field A character string indicating the name of the variable used
#'  for filtering.
#' @param probs A numerical value between 0 and 1 (i.e. proportion) indicating the
#'  quantile threshold used for filtering.
#' @return A [data.frame], containing all rows for which the specified variable greater
#'  or equal the specified threshold.
#' @export
#' @examples
#' df <- data.frame(a = rnorm(10), b = sample(letters, 10, replace = TRUE))
#' filter_ntile(df, "a", .75)
filter_ntile <- function(df, var_field, probs){
  # Find index of variable
  var_index <- 0
  if (is.character(var_field)) {
    var_index <- match(var_field, names(df))
  } else {
    var_index <- match(var_field, seq(length(df)))
  }
  if (is.na(var_index))
    stop("var_field not found")
  if (!is.numeric(df[[var_index]]))
    stop("var_field must refer to a numeric mode column")

  # Filter df - base R method to allow index matching
  df <- df[which(df[[var_index]] >= stats::quantile(df[[var_index]],
                                                    probs = probs)), ,
           drop = FALSE]
  return(df)

  }


#' Find index of unique id in df.
#'
#' Searches a data.frame for a column that includes 'id' in its name and
#'  is an unique identifier.
#' Returns the index of the first column in the data.frame that meets these conditions.
#' @param df A data.frame.
#' @return A numeric, indicating the index of the unique id column.
#' @export
#' @examples
#' df <- data.frame(id_not_unique = rep(1, 10),
#' unique_id = 1:10,
#' also_id = 21:30,
#' other = letters[1:10])
#' find_unique_id(df)
#'
find_unique_id <- function(df){
  id_vars_index <- stringr::str_detect(names(df), "id") %>% which() # variables in df that contain "id"
  is_unique <- function(df, ix) nrow(dplyr::distinct(df[ix])) == nrow(df) # check if a variable is unique id
  uniques_ix <- which(sapply(id_vars_index, is_unique, df = df))[1]
  if(is.na(uniques_ix)) uniques_ix <- 0
  id_vars_index[uniques_ix] # pick first variable that contains 'id' and is a unique id
}



#' Add word_id.
#'
#' Function to add word_id to a data.frame if it is not already there.
#' @details Searches the data.frame for variables that include 'id' in their name.
#'  Checks if any of these is a unique identifier. If so, this variable is renamed
#'  'word_id' and forced into type 'character'.
#' If not, a new column named 'word_id' of type 'character' is created, containing a
#' string of numbers that uniquely identify all rows in the data.frame.
#' @param df A data.frame.
#' @return A data.frame that includes a column called 'word_id' of type 'character' which
#'  contains a unique identifiers for all rows in the data.frame.
#' @export
#' @seealso \code{\link{find_unique_id}},  \code{\link{add_multiwords}}
#' @examples
#'t1 <- data.frame(wid = 1:10, a = sample(letters, 10))
#'t2 <- data.frame(id = rep(1, 10),
#'                 word_id = sprintf("%02d", 1:10),
#'                 a = sample(letters, 10))
#'t3 <- data.frame(a = sample(letters, 10))
#'add_word_id(t1)
#'add_word_id(t2)
#'add_word_id(t3)
#'@importFrom magrittr `%>%`
add_word_id <- function(df){

  # Function to use %in% with pattern
  `%rin%` = function (pattern, list) {
    vapply(pattern, function (p) any(grepl(p, list)), logical(1L), USE.NAMES = FALSE)
  }

  # check how long ids should be
  digits <- floor(max(log10(nrow(df)))) + 1

  if(!"id" %rin% names(df)){  # if no variable contains "id"
    df$word_id <- sprintf(paste0("%0", digits, "d"), 1:nrow(df)) # ...create word_id
  }else{ # if a variable contains 'id'
    id_index <- 0
    id_index <- find_unique_id(df) # ...check which
    if(purrr::is_empty(id_index)){ # if df contains 'id' vars which are not unique ids
      df$word_id <- sprintf(paste0("%0", digits, "d"), 1:nrow(df)) # ...create word_id
      }else{ # if df contains id vars which are unique ids
        if(names(df[id_index]) != "word_id"){# but not called 'word_id'
          df$word_id <- df[[id_index]] %>% as.character() # ...create it
          df <- df[-id_index]
          }else{ # if df contains unique id var called 'word_id'
          df$word_id <- df$word_id %>% as.character() # ...force it to be character
        }
      }
    }
  return(df)
}



#' Add id
#'
#' Function to add 'id' to a data.frame if it is not already there.
#' @details Searches the data.frame for variables that include 'id' in their name.
#' Checks if any of these is a unique identifier. If so, this variable is renamed
#' 'word_id' and forced into type 'character'.
#' If not, a new column named 'word_id' of type 'character' is created, containing
#' a string of numbers that uniquely identify all rows in the data.frame.
#' @param df A data.frame.
#' @return A data.frame that includes a column called 'id' of type 'character' which
#'  contains a unique identifiers for all rows in the data.frame.
#' @export
#' @seealso \code{\link{find_unique_id}},  \code{\link{add_multiwords}}
#' @examples
#'t1 <- data.frame(id = 1:10, a = sample(letters, 10))
#'t2 <- data.frame(id = rep(1, 10),
#'                 word_id = sprintf("%02d", 1:10),
#'                 a = sample(letters, 10))
#'t3 <- data.frame(a = sample(letters, 10))
#'add_id(t1)
#'add_id(t2)
#'add_id(t3)
#'@importFrom magrittr `%>%`
add_id <- function(df){

  # Function to use %in% with pattern
  `%rin%` = function (pattern, list) {
    vapply(pattern, function (p) any(grepl(p, list)), logical(1L), USE.NAMES = FALSE)
  }

  # check how long ids should be
  digits <- floor(max(log10(nrow(df)))) + 1

  if(!"id" %rin% names(df)){  # if no variable contains "id"
    df$id <- sprintf(paste0("%0", digits, "d"), 1:nrow(df)) # ...create word_id
  }else{ # if a variable contains 'id'
    id_index <- 0
    id_index <- find_unique_id(df) # ...check which
    if(purrr::is_empty(id_index)){ # if df contains 'id' vars which are not unique ids
      df$id <- sprintf(paste0("%0", digits, "d"), 1:nrow(df)) # ...create word_id
    }else{ # if df contains id vars which are unique ids
      if(names(df[id_index]) != "id"){# but not called 'word_id'
        df$id <- df[[id_index]] %>% as.character() # ...create it
        df <- df[-id_index]
      }else{ # if df contains unique id var called 'word_id'
        df$id <- df$id %>% as.character() # ...force it to be character
      }
    }
  }
  return(df)
}






#' Replace missing values.
#'
#' Helper to replace missing values. Default is to repalce NAs by 'mean' - 1 'sd'.
#'
#' @param x A numerical vector.
#' @param replace_na Specifies the value used to replace NAs. Can take values:
#'  \itemize{
#'   \item 'mean-sd' (charcter): replace NAs by mean - 1sd. Default.
#'   \item 'min' (charcter): replace NAs by minimum.
#'   \item 0 (numerical): replace NAs by 0.
#'   \item FALSE (logical): do not replace NAs.
#' }
#' @importFrom stats "sd"
#' @export
#' @examples
#' a <- c(rnorm(7), NA, NA, NA)
#' repl_na(a) == repl_na(a, 'mean-sd')
#' repl_na(a, 'min')
#' repl_na(a, 0)
#' repl_na(a, FALSE)
repl_na <- function(x, replace_na = c('mean-sd', 'min', 0, F)){

  if (missing(replace_na)) {
    x <- tidyr::replace_na(x, replace = (mean(x, na.rm = T) - sd(x, na.rm = T)))
  }else{
    if (replace_na == "mean-sd") {
      x <- tidyr::replace_na(x, replace = (mean(x, na.rm = T) - sd(x, na.rm = T)))
    }
    if (replace_na == "min") {
      x <- tidyr::replace_na(x, replace = min(x, na.rm = T)) # Replace missings with min
    }
    if (replace_na == "0") {
      x <- tidyr::replace_na(x, replace = 0) # Replace missings with minimum value
    }
    if (replace_na == "FALSE") {
      x
    }
  }
  return(x)
}



