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
#' @export
plot_line <- function(data_frame, x_name, y_name){
  stopifnot(x_name %in% colnames(data_frame) &&
              y_name %in% colnames(data_frame))
  ggplot(data_frame, aes_string(x = x_name, y = y_name)) + geom_line()
}