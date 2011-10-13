\name{saves}
\alias{saves}
\title{Save the variables of a data...}
\usage{saves(..., list=character(), file, overwrite=FALSE, ultra.fast=FALSE)
}
\description{Save the variables of a data.frame in distinct binary files}
\details{\code{saves} does what the name suggests: it saves dataframe(s) or list(s) to disk in a special, binary format.
This binary format consists of distinct binary files of all separate variables of a dataframe/list
merged into an uncompressed tar archive. This is done via a loop, which saves each variable/column
to an an external representation of the R objects via \code{save} in a temporary directory.
Theese 'RData' files are archived to an 'RDatas' tar archive, uncompressed for better speed.}
\value{The saved filename(s) (invisible).}
\seealso{\code{loads} to load R objects from RDatas binary format}
\author{Gergely Daróczi \email{gergely@snowl.net}}
\arguments{\item{...}{R objects: the names of the objects to be saved (as symbols or character strings)}
\item{list}{character vector: the name(s) of the data frame(s) or list(s) to save}
\item{file}{character vector: the (RDatas) filename(s) in which to save the variables in the current
working directory}
\item{overwrite}{boolean: if TRUE, existing files will be deleted before saving. Default set to FALSE, which
will report error on conflicting file names.}
\item{ultra.fast}{boolean: if TRUE, ultra fast (...) processing is done without any check to parameters, also no 
archiving or compression is done. Be sure if using this setting, as many uncompressed files could be
generated in the working directory's subdirectory named to \code{df}. Only recommended
for servers dealing with lot of R objects' saves and loads in a monitored environment.}
}
\examples{\dontrun{
# Saving the demo dataset to evs.2000.hun.RDatas in current working directory.
data(evs.2000.hun)
saves(evs.2000.hun)
# Saving both the demo dataset and mtcars to current working directory
saves(evs.2000.hun, mtcars)
saves(list=c('evs.2000.hun', 'mtcars'))
# Saving all kind of cars :)
saves(cars, mtcars, overwrite = T)
saves(list=c('cars', 'mtcars'), overwrite = T)
}}

