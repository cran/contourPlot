#' Plot a contour map
#'
#' Takes x,y,z co-ordinates and plots them on a contour map. Smoothing and interpolation is done by means of fitting a spline to the data.
#'
#' @param x a vector of x co-ordinates
#' @param y a vector of y co-ordinates
#' @param z a vector of z co-ordinates representing the height of the contours
#' @param nx The number of pixels that will be in final plot. default is length(unique(x))
#' @param main Title of plot
#' @param axis logical if TRUE displays the axes of the plot
#' @param legend logical if TRUE displays the legend
#' @param xlab label on x axis
#' @param ylab label on y axis
#' @param col list of colors to be applied to contours.
#' @param breaks list of values indicating the contour ranges
#' @param nlevels useful if breaks and col are left as null. Sets the number of levels of the contours to be plotted
#' @param legend_pos set position of the colour bar. Default = 4.
#' @return A contour plot (similar to those in base, additional elements can be added using lines, points functions etc.
#' @examples
#' x <- Volcontour$x
#' y <- Volcontour$y
#' z <- Volcontour$z
#'
#' contourPlot(x = x, y = y, z = z)
#'
#' # A smoother contour
#' contourPlot(x = x, y = y, z = z, nx = 500)
#'
#' # Changing breaks and colours
#' breaks = pretty(c(min(z),max(z)))
#' col = brewer.pal(n = length(breaks)-1, "Blues")
#' contourPlot(x = x, y = y, z = z, nx = 500, breaks = breaks, col = col)
#'
#' # add lines
#' lines(circle(0, 0, 26.5))
#' @export
contourPlot<-function(
  x,y,z,
  nx = length(unique(x)),
  main = NULL, axis = TRUE, legend = TRUE, xlab = "", ylab = "",
  col = NULL, breaks = NULL, nlevels = 10, legend_pos = 4
){
  # Convert x, y, z to matrix and interpolate values in between points
  Mat = interp(x,y,z, nx = nx, ny = nx )[[3]]

  # Get info
  r = max(sqrt((x^2)+(y^2)))
  ticks = seq(-r,r,length.out = nrow(Mat))
  Ticks = sort(union(pretty(c(0,r)),-pretty(c(0,r))))

  # sort out colors and breaks:
  if (!missing(breaks) & !missing(col)){
    if (length(breaks) - length(col) != 1){
      stop("breaks must be 1 element longer than cols")
    }
    nlevels <- length(breaks) - 1
  }
  if (missing(breaks) & !missing(col)){
    breaks <- seq(min(Mat,na.rm = TRUE), max(Mat, na.rm = TRUE), length = length(col) + 1)
    nlevels <- length(breaks) - 1
  }
  if (missing(col) & !missing(breaks)){
    col <- rev(heat.colors(length(breaks) - 1))
    nlevels <- length(breaks) - 1
  }
  if (missing(breaks) & missing(col)){
    breaks <- seq(min(Mat,na.rm = TRUE), max(Mat, na.rm = TRUE), length = nlevels + 1)
    col <- rev(heat.colors(nlevels))
  }

  # Image
  image(
    x = ticks, y = ticks, Mat, asp = 1,
    axes = FALSE, xlab = xlab, ylab = ylab, main = main,
    col = col, breaks = breaks,
    xlim = range(ticks)*1.1, ylim = range(ticks)*1.1
  )


  # Legend
  if(legend){
    ylevs <- seq(-r, r, length = nlevels + 1)
    if(legend_pos==1){
      rect(ylevs[1:(length(ylevs) - 1)], -1.2 * r, ylevs[2:length(ylevs)], -1.3 * r, col = col, border = NA, xpd = TRUE)
      rect(min(ylevs), -1.2 *r, max(ylevs), -1.3 *r, border = "#66666650", xpd = TRUE)
      if(length(breaks)<5){
        text(ylevs, -1.3 * r, round(breaks, 1), pos = 1, xpd = TRUE)
      }else{
        text(seq(-r, r, length = 5), -1.3 * r, round(seq(min(breaks),max(breaks),length.out = 5),1), pos = 1, xpd = TRUE)
      }

      # Axis
      if(axis){
        axis(2, pos = -1.2 * r, at = Ticks, labels = NA)
        text( -1.21 * r, Ticks,Ticks, xpd = TRUE, pos = 2)
        axis(3, pos = 1.2 * r, at = Ticks)
      }

    }
    if(legend_pos==2){
      rect(-1.2 * r, ylevs[1:(length(ylevs) - 1)], -1.3 * r, ylevs[2:length(ylevs)], col = col, border = NA, xpd = TRUE)
      rect(-1.2 *r, min(ylevs), -1.3 *r, max(ylevs), border = "#66666650", xpd = TRUE)
      if(length(breaks)<15){
        text(-1.3 * r, ylevs, round(breaks, 1), pos = 2, xpd = TRUE)
      }else{
        text(-1.3 * r, seq(-r, r, length = 15), round(seq(min(breaks),max(breaks),length.out = 15),1), pos = 2, xpd = TRUE)
      }

      # Axis
      if(axis){
        axis(4, pos = 1.2 * r, at = Ticks, labels = NA)
        text( 1.21 * r, Ticks,Ticks, xpd = TRUE, pos = 4)
        axis(1, pos = -1.2 * r, at = Ticks)
      }

    }
    if(legend_pos==3){
      rect(ylevs[1:(length(ylevs) - 1)], 1.2 * r, ylevs[2:length(ylevs)], 1.3 * r, col = col, border = NA, xpd = TRUE)
      rect(min(ylevs), 1.2 *r, max(ylevs), 1.3 *r, border = "#66666650", xpd = TRUE)
      if(length(breaks)<5){
        text(ylevs, 1.3 * r, round(breaks, 1), pos = 3, xpd = TRUE)
      }else{
        text(seq(-r, r, length = 5), 1.3 * r, round(seq(min(breaks),max(breaks),length.out = 5),1), pos = 3, xpd = TRUE)
      }

      # Axis
      if(axis){
        axis(2, pos = -1.2 * r, at = Ticks, labels = NA)
        text( -1.21 * r, Ticks,Ticks, xpd = TRUE, pos = 2)
        axis(1, pos = -1.2 * r, at = Ticks)
      }

    }
    if(legend_pos==4){
      rect(1.2 * r, ylevs[1:(length(ylevs) - 1)], 1.3 * r, ylevs[2:length(ylevs)], col = col, border = NA, xpd = TRUE)
      rect(1.2 *r, min(ylevs), 1.3 *r, max(ylevs), border = "#66666650", xpd = TRUE)
      if(length(breaks)<15){
        text(1.3 * r, ylevs, round(breaks, 1), pos = 4, xpd = TRUE)
      }else{
        text(1.3 * r, seq(-r, r, length = 15), round(seq(min(breaks),max(breaks),length.out = 15),1), pos = 4, xpd = TRUE)
      }

      # Axis
      if(axis){
        axis(2, pos = -1.2 * r, at = Ticks, labels = NA)
        text( -1.21 * r, Ticks,Ticks, xpd = TRUE, pos = 2)
        axis(1, pos = -1.2 * r, at = Ticks)
      }

    }
  } else {
    # Axis
    if(axis){
      axis(2, pos = -1.2 * r, at = Ticks, labels = NA)
      text( -1.21 * r, Ticks,Ticks, xpd = TRUE, pos = 2)
      axis(1, pos = -1.2 * r, at = Ticks)
    }
  }
}
