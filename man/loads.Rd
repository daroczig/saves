\name{loads}
\alias{loads}
\title{Loading only given variables of a data...}
\usage{loads(file, variables, to.data.frame=FALSE)
}
\description{Loading only given variables of a data.frame from binary file}
\details{\code{loads} does what the name suggests: it loads data from a special binary file format (Rdatas) made
up by the \code{saves} function. This special, uncompressed tar archive inlcudes several separate
Rdata files (saved by \code{save} function) as being columns/variables of a data.frame.

The purpose of this function is to be able only a few variables of a data.frame really fast. It is
done by reading and writing datas in binary format without any transformations, and combining the
speed of only reading the needed part of an archive.

Some minor experiments shows a huge performance gain against using SQLite/MySQL backends or loading
whole binary data, but be conscious always choosing the aprropriate method to write and read data.

The author of this package would like to emphasize: this package could be useful only in few cases!}
\value{Loaded data.frame}
\author{Gergely Daróczi \email{daroczi.gergely@btk.ppke.hu}}
\arguments{\item{file}{character string: the filename from which to load the variables}
\item{variables}{A character vector containing the variable names to load}
\item{to.data.frame}{boolean: the default behavior of \code{loads} is to concatenate the variables to a list.
This could be overriden with TRUE argument specified at to.data.frame parameter, which will return
a dataframe instead of list. Only do this if all your variables have the same number of cases!}
}
\examples{\dontrun{
# Loading the 'v1' and 'v5' variables of the demo dataset.
data(evs.2000.hun)
saves("evs.2000.hun")
evs.filtered.list <- loads("evs.2000.hun.Rdatas", c('v1', 'v5'))
evs.filtered.df <- loads("evs.2000.hun.Rdatas", c('v1', 'v5'), to.data.frame=TRUE)
}}

