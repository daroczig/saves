\name{loads}
\alias{loads}
\title{Loading only given variables of a data...}
\usage{loads(file, variables, to.data.frame=FALSE, ultra.fast=FALSE)
}
\description{Loading only given variables of a data.frame from binary file}
\details{\code{\link{loads}} does what the name suggests: it loads data from a special binary file format (RDatas) made
up by the \code{\link{saves}} function. This special, uncompressed tar archive inlcudes several separate
RData files (saved by \code{\link{save}} function) as being columns/variables of a data.frame.

The purpose of this function is to be able only a few variables of a data.frame really fast. It is
done by reading and writing datas in binary format without any transformations, and combining the
speed of only reading the needed part of an archive.

Some minor experiments shows a huge performance gain against using SQLite/MySQL backends or loading
whole binary data, but be conscious always choosing the aprropriate method to write and read data.

The author of this package would like to emphasize: this package could be useful only in few cases!}
\value{Loaded data.frame}
\seealso{\code{\link{saves}} to save R objects to RDatas binary format}
\author{Gergely Daróczi \email{gergely@snowl.net}}
\arguments{\item{file}{character string: the (RDatas) filename from which to load the variables. If using
ultra.fast = TRUE option, specify the directory holding the uncompressed R objects (saved via
\code{saves(..., ultra.fast = TRUE)}).}
\item{variables}{A character vector containing the variable names to load}
\item{to.data.frame}{boolean: the default behavior of \code{loads} is to concatenate the variables to a list.
This could be overriden with TRUE argument specified at to.data.frame parameter, which will return
a dataframe instead of list. Only do this if all your variables have the same number of cases!}
\item{ultra.fast}{boolean: if TRUE, ultra fast (...) processing is done without any check to parameters
or file existence/permissions. Be sure if using this setting as no debugging is done! Only recommended
for servers dealing with lot of R objects' saves and loads in a monitored environment. Also, for
performance gain, it is advised not to convert the list to data frame (to.data.frame = FALSE).}
}
\examples{\dontrun{
# Loading the 'v1' and 'v5' variables of the demo dataset.
data(evs.2000.hun)
saves(evs.2000.hun)
evs.filtered.list <- loads("evs.2000.hun.RDatas", c('v1', 'v5'))
evs.filtered.df <- loads("evs.2000.hun.RDatas", c('v1', 'v5'), to.data.frame=TRUE)
}}

