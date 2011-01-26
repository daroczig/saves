\name{saves}
\alias{saves}
\title{Save the variables of a data...}
\usage{saves(df, file=paste(df, ".Rdatas", sep = ""))
}
\description{Save the variables of a data.frame in distinct binary files}
\details{\code{saves} does what the name suggests: it saves a dataframe or list to disk in a special, binary format.
This binary format consists of distinct binary files of all separate variables of a dataframe/list
merged into an uncompressed tar archive. This is done via a loop, which saves each variable/column
to an an external representation of the R objects via \code{save} in a temporary directory.
Theese 'Rdata' files are archived to an 'Rdatas' tar archive, uncompressed for better speed.

The special file format can be load via \code{loads} function.}
\value{The saved file's name (invisible).}
\author{Gergely Daróczi \email{daroczi.gergely@btk.ppke.hu}}
\arguments{\item{df}{character string: the name of the data frame or list to save}
\item{file}{character string: the filename in which to save the variables in the current working directory}
}
\examples{\dontrun{
# Saving the demo dataset to evs.2000.hun.Rdatas in current working directory.
data(evs.2000.hun)
saves("evs.2000.hun")
}}

