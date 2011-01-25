#' Loading only given variables of a data.frame from binary file
#' 
#' \code{loads} does what the name suggests: it loads data from a special binary file format (Rdatas) made
#' up by the \code{saves} function. This special, uncompressed tar archive inlcudes several separate
#' Rdata files (saved by \code{save} function) as being columns/variables of a data.frame.
#' 
#' The purpose of this function is to be able only a few variables of a data.frame really fast. It is
#' done by reading and writing datas in binary format without any transformations, and combining the
#' speed of only reading the needed part of an archive.
#' 
#' Some minor experiments shows a huge performance gain against using SQLite/MySQL backends or loading
#' whole binary data, but be conscious always choosing the aprropriate method to write and read data.
#' 
#' The author of this package would like to emphasize: this package could be useful only in few cases! 
#' 
#' @param file character string: the filename from which to load the variables 
#' @param variables A character vector containing the variable names to load
#' @param to.data.frame boolean: the default behavior of \code{loads} is to concatenate the variables to a list.
#' This could be overriden with TRUE argument specified at to.data.frame parameter, which will return
#' a dataframe instead of list. Only do this if all your variables have the same number of cases!
#' @return Loaded data.frame
#' @export
#' @examples \dontrun{
#' # Loading the 'v1' and 'v5' variables of the demo dataset.
#' data(evs.2000.hun)
#' saves("evs.2000.hun")
#' evs.filtered.list <- loads("evs.2000.hun.Rdatas", c('v1', 'v5'))
#' evs.filtered.df <- loads("evs.2000.hun.Rdatas", c('v1', 'v5'), to.data.frame=TRUE)
#' }
#' @author Gergely Daróczi \email{daroczi.gergely@@btk.ppke.hu} 

loads <- function (file=NULL, variables=NULL, to.data.frame=FALSE) {
	if (is.null(variables) | is.null(file)) {
		stop('Arguments missing! Specify a filename and variable names also to load.')
	}
	if (!file.exists(file)) {
		stop('Archive not found!')
	}
	tmp <-  paste(tempdir(), '/saves.temp', sep='')
	dir.create(tmp)
	untar(file, exdir=tmp)
	for (i in 1:length(variables)) {
		f <- paste(tmp, "/", variables[i], '.Rdata', sep='')
		if (!file.exists(f)) {
			stop(paste('Variable: <<', variables[i], '>> not found!'))
		}
		if (i == 1) {
			if (to.data.frame == FALSE) {
				data <- list(local(get(load(f))))
			} else {
				data <- data.frame(local(get(load(f))))
			}
		} else {
			data[paste(variables[i])] <- as.data.frame(local(get(load(f))))
		}
	}
	names(data) <- variables
	unlink(tmp, recursive = TRUE)
	return(data)
}