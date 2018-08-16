\name{define_species}
\alias{define_species}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Define species parameters for SplitR
}
\description{
A function rarely called on its own to define appropriate species parameters for input to SplitR model.
}
\usage{
define_species(species)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{species}{
species name character string. Current accepted values are:
\enumerate{
   \item so2
   \item so4
 }
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
This function returns a data table of species parameter accepted by SplitR, including columns named \code{name}, \code{pdiam}, \code{density}, \code{shape_factor}, \code{resuspension}, and \code{ddep_vel}.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
define_species( "so2")
define_species( "so4")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
