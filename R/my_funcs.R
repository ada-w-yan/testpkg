#' adds two numbers
#' 
#' \code{my_add} adds two numbers
#' 
#' @param x numeric vector of length 1
#' @param y numeric vector of length 1
#' @return numeric vector of length 1
#' @export
my_add <- function(x, y){
  x + y
}

#' makes a line plot
#' 
#' \code{plot_line} takes a data frame and makes a line plot with two of the 
#' columns, one on each axis.
#' 
#' @param data_frame data frame with at least columns x_name and y_name
#' @param x_name character vector of length 1: name of variable to plot on x axis
#' @param y_name character vector of length 1: name of variable to plot on y axis
#' @return ggplot object
#' @import ggplot2
#' @export
plot_line <- function(data_frame, x_name, y_name){
  stopifnot(x_name %in% colnames(data_frame) &&
              y_name %in% colnames(data_frame))
  ggplot(data_frame, aes_string(x = x_name, y = y_name)) + geom_line()
}

#' returns the nth Fibonacci number
#' 
#' \code{calc_fibonacci} returns the nth Fibonacci number
#' 
#' @param n numeric vector of length 1
#' @return numeric vector of length 1: nth Fibonacci number
#' @export
calc_fibonacci <- function(n) {
  stopifnot(n > 0)
  stopifnot(isTRUE(all.equal(round(n), n)))
  if(n <= 2) {
    1
  } else {
    calc_fibonacci(n - 1) + calc_fibonacci(n - 2)
  }
}

#' extracts the nth element from a string
#' 
#' \code{extract_string_element} extracts the nth element from a string
#' 
#' @param string1 character vector of length 1
#' @param n numeric vector of length 1
#' @return character vector of length 1 containing a single character
extract_string_element <- function(string1, n) {
    substr(string1, n, n)
}

#' reverses a string
#' 
#' \code{reverse_string} reverses a string
#' 
#' @param my_string character vector
#' @return character vector
#' @export
reverse_string <- function(my_string) {
    reverse_vec <- rev(seq_len(nchar(my_string)))
    paste0(vapply(reverse_vec, function(n) extract_string_element(my_string, n),
                  character(1)), collapse = "")
}

#' randomly capitalises letters in a string
#' 
#' \code{teenage_capitalise} randomly capitalises letters in a string
#' 
#' @param my_string character vector
#' @return character vector
#' @importFrom stats runif
#' @export
teenage_capitalise <- function(my_string) {
    sep_string <- vapply(seq_len(nchar(my_string)),
                         function(x) extract_string_element(my_string, x),
                         character(1))
    randomly_choose_capitalisation <- function(my_char) {
        letter_index <- grep(my_char, letters)
        if(isTRUE(all.equal(letter_index, integer(0)))) {
            my_char
        } else if(runif(1) < .5) {
            letters[letter_index]
        } else {
            LETTERS[letter_index]
        }
    }
    random_cap_sep_string <- vapply(sep_string, randomly_choose_capitalisation, character(1))
    paste0(random_cap_sep_string, collapse = "")
}