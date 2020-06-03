#' Create a Set of Circle Co-Ordinates
#'
#' Creates a set of circle co-ordinates, of radius r, at position x,y
#'
#' @param x x position of the center of the circle
#' @param y y position of the center of the circle
#' @param r radius of the circle
#' @return Matrix of x,y co-ordinates for a circle
#' @examples
#' plot(circle(0, 0, r = 1), type = 'l', asp = 1)
#' @export
circle <- function(x, y, r = 1){
  rads <- seq(0,2*pi,length.out = 360)
  xco <- cos(rads) * r + x
  yco <- sin(rads) * r + y
  cbind(xco, yco)
}
