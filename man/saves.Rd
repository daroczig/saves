\name{saves}
\alias{saves}
\title{Save the variables of a data...}
\usage{saves(df, file=paste(df, ".Rdatas", sep = ""), ultra.fast=FALSE)
}
\description{Save the variables of a data.frame in distinct binary files}
\details{\code{saves} does what the name suggests: it saves a dataframe or list to disk in a special, binary format.
This binary format consists of distinct binary files of all separate variables of a dataframe/list
merged into an uncompressed tar archive. This is done via a loop, which saves each variable/column
to an an external representation of the R objects via \code{save} in a temporary directory.
Theese 'Rdata' files are archived to an 'Rdatas' tar archive, uncompressed for better speed.}
\value{The saved file's name (invisible).}
\seealso{\code{loads} to load R objects from Rdatas binary format}
\author{Gergely Daróczi \email{gergely@snowl.net}}
\arguments{\item{df}{character string: the name of the data frame or list to save}
\item{file}{character string: the (Rdatas) filename in which to save the variables in the current
working directory}
\item{ultra.fast}{boolean: if TRUE, ultra fast (...) processing is done without any check to parameters, also no 
archiving or compression is done. Be sure if using this setting, as many uncompressed files could be
generated in the working directory's subdirectory named to \code{df}. Only recommended
for servers dealing with lot of R objects' saves and loads in a monitored environment.}
}
\examples{\dontrun{
# Saving the demo dataset to evs.2000.hun.Rdatas in current working directory.
data(evs.2000.hun)
saves("evs.2000.hun")
}}

