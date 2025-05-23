% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bng.R
\name{as_bng_reference}
\alias{as_bng_reference}
\alias{as_bng_reference.default}
\alias{as_bng_reference.BNGReference}
\alias{as_bng_reference.character}
\alias{is_bng_reference}
\title{Create BNG reference objects}
\usage{
as_bng_reference(bng_ref, ...)

\method{as_bng_reference}{default}(bng_ref, ...)

\method{as_bng_reference}{BNGReference}(bng_ref, ...)

\method{as_bng_reference}{character}(bng_ref, ...)

is_bng_reference(bng_ref)
}
\arguments{
\item{bng_ref}{A character vector of grid references to be created or tested.}

\item{...}{Additional parameters. Not currently used.}
}
\value{
An object of type \code{BNGReference}.
}
\description{
Convert or test user input (strings) to a custom object for handling British
National Grid (BNG) references.
}
\details{
Converts a BNG reference string into a \code{BNGReference} object, ensuring
type consistency across the package. All functions accepting or returning BNG
references enforce the use of this class.

The BNG is structured using a hierarchical system of grid squares at various
resolutions. At its highest level, the grid is divided into 100 km by 100 km
squares, each of which is identified by a two-letter code. Successive levels
of resolution further subdivide the grid squares into finer detail, down to
individual 1-meter squares.

The package supports the 'standard' and 'intermediate' quadtree resolutions:
100km, 50km, 10km, 5km, 1km, 500m, 100m, 50m, 10m, 5m and 1m.
}
\examples{
as_bng_reference("TQ1234")

as_bng_reference(c("TQ1234", "SU5678"))

}
\seealso{
\code{\link[=is_valid_bng]{is_valid_bng()}}, \link{BNGReference}
}
