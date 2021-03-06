\name{ms.polygram}
\alias{ms.polygram}
\alias{ms.polygon}
\alias{ms.filled.polygon}
\alias{ms.male}
\alias{ms.female}
\alias{ms.arrows}
\alias{ms.sunflowers}
\alias{ms.image}
\alias{ms.face}
\title{Symbol functions/data to be passed as symb argument to
  my.symbols}
\description{
  These functions/data matricies are examples of what can be passed as
  the \code{symb} argument in the \code{my.symbols} function.  They are
  provided both to be used for some common symbols and as examples of
  what can be passed as the \code{symb} argument.
}
\usage{
ms.polygram(n, r=1, adj=pi/2, ...)
ms.polygon(n, r=1, adj=pi/2, ...)
ms.filled.polygon(n, r=1, adj=pi/2, fg=par('fg'), bg=par('fg'), ... )
ms.male
ms.female
ms.arrows(angle, r=1, adj=0.5, length=0.1, ...)
ms.sunflowers(n,r=0.3,adj=pi/2, ...)
ms.image(img, transpose=TRUE, ...)
ms.face(features, ...)
}
\arguments{
  \item{n}{The number of sides for polygons and polygrams, the number of
  petals(lines) for sunflowers.}
  \item{r}{The radius of the enclosing circle for polygons and polygrams
  (1 means that it will pretty much fill the bounding square).  For
  sunflowers this is the radius (relative to the inches square) of the
  inner circle.  For arrows this controls the length of the arrow, a
  value of 2 means the length of the arrow will be the same as inches
  (but it may then stick out of the box if adj != 1).}
  \item{adj}{For polygons, polygrams, and sunflowers this is the angle
  in radians that the first corner/point will be.  The default puts a
  corner/point straight up, this can be used to rotate the symbols.  For
  arrows, this determines the positioning of the arrow, a value of 0
  means the arrow will start at the x,y point and point away from it,
  0.5 means the arrow will be centered at x,y and 1 means that the arrow
  will end (point at) x,y.}
  \item{fg, bg}{Colors for the filled polygons.  \code{fg} is the color
  of the line around the polygon and \code{bg} is the fill color, see
  \code{\link{polygon}}.}
  \item{angle}{The angle in radians that the arrow will point.}
  \item{length}{The length of the arrow head (see
    \code{\link{arrows}}).}
  \item{img}{ A 3 dimensional array representing an image such as
    produced by the png or EBImage packages.}
  \item{transpose}{Should the image be tranposed, use TRUE for images
    imported using package png and FALSE for images imported using
    EBImage.}
  \item{features}{A list of data representing the features of
    the faces, each element represents 1 face and the values need to be
    scaled between 0 and 1, see \code{\link{faces}} for details on which
    elements match which features.}
  \item{...}{additional parameters that will be passed to plotting
    functions or be ignored.}
}
\details{
  These functions/matricies can be passed as the \code{symb} argument to
  the \code{my.symbols} function.  The represent examples that can be
  used to create your own symbols or may be used directly.
}
\value{
  These functions either return a 2 column matrix of points to be passed
  to \code{lines} or \code{NULL}.
}
\author{Greg Snow \email{538280@gmail.com}}
\seealso{\code{\link{my.symbols}}, \code{\link{polygon}},
  \code{\link{arrows}}, \code{\link{lines}}, \code{\link{faces}}, also see
  \code{\link{rasterImage}} for an alternative to ms.image
}
\examples{

plot(1:10,1:10)
my.symbols(1:10,1:10, ms.polygram, n=1:10, r=seq(0.5,1,length.out=10),
inches=0.3)

my.symbols(1:10,1:10, ms.polygon, n=1:10, add=FALSE, inches=0.3)

my.symbols(1:5, 5:1, ms.filled.polygon, add=FALSE, n=3:7, fg='green',
  bg=c('red','blue','yellow','black','white'), inches=0.3 )

my.symbols( 1:10, 1:10, ms.female, inches=0.3, add=FALSE)
my.symbols( 1:10, 10:1, ms.male, inches=0.3, add=TRUE)

plot(1:10, 1:10)
my.symbols(1:10, 1:10, ms.arrows, angle=runif(10)*2*pi, inches=0.5,
adj=seq(0,1,length.out=10), symb.plots=TRUE)

my.symbols(1:10, 1:10, ms.sunflowers, n=1:10, inches=0.3, add=FALSE)

if( require(png) ) {
  img <- readPNG(system.file("img", "Rlogo.png", package="png"))

  my.symbols( runif(10), runif(10), ms.image, MoreArgs=list(img=img),
                 inches=0.5, symb.plots=TRUE, add=FALSE)
}

tmp.mtcars <- scale(mtcars, center=sapply(mtcars,min),
	scale=sapply(mtcars,function(x) diff(range(x))) )
tmp2.mtcars <- lapply( seq_len(nrow(tmp.mtcars)), function(i) tmp.mtcars[i,] )
my.symbols(mtcars$wt, mtcars$mpg, ms.face, inches=0.3, features=tmp2.mtcars,
	add=FALSE)


}
\keyword{dplot}
\keyword{aplot}