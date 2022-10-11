#' Draw an ellipse
#'
#' @param x Center x coordinate of ellipse.
#' @param y Center y coordinate of ellipse.
#' @param width Width of ellipse.
#' @param height Length of ellipse.
#' @param theta Angle/heading.
#' @param npoints Number of vertices to use to define the ellipse boundary.
#' @param plot Boolean; print a diagnostic plot?
#'
#' @return A list with three slots: `coords` (a `data.frame` of coordinates
#' defining ellipse boundary), `center` (the `x` and `y` inputs), and `theta` (the input angle).
#'
#' @export
#' @import plotrix
#'
ellipse <- function(x,
                    y,
                    width,
                    height=width,
                    theta=0,
                    npoints=10,
                    plot=FALSE) {
    #ellipse(x=0, y=-500, width=.15*200, height=200, theta=310)

    theta <- -1*theta*(pi/180)
    a <- width/2
    b <- height/2
    xcoord <- seq(-a,a,length=npoints)
    ycoord.neg <- sqrt(b^2*(1-(xcoord)^2/a^2))
    ycoord.pos <- -sqrt(b^2*(1-(xcoord)^2/a^2))
    xx <- c(xcoord,xcoord[npoints:1])
    yy <- c(ycoord.neg,ycoord.pos)
    x.theta <- xx*cos(2*pi-theta)+yy*sin(2*pi-theta)+x
    y.theta <- yy*cos(2*pi-theta)-xx*sin(2*pi-theta)+y

    if(plot){polygon(x.theta,y.theta,density=0)}

    return(list(coords=data.frame(x=x.theta,y=y.theta),
                center=c(x,y),
                theta=theta))
}
