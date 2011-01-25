#' Save the variables of a data.frame in distinct binary files
#' 
#' \code{saves} does what the name suggests: it saves a dataframe or list to disk in a special, binary format.
#' This binary format consists of distinct binary files of all separate variables of a dataframe/list
#' merged into an uncompressed tar archive. This is done via a loop, which saves each variable/column
#' to an an external representation of the R objects via \code{save} in a temporary directory.
#' Theese 'Rdata' files are archived to an 'Rdatas' tar archive, uncompressed for better speed.
#' 
#' The special file format can be load via \code{loads} function.
#' 
#' @param df character string: the name of the data frame or list to save
#' @param file character string: the filename in which to save the variables in the current working directory
#' @return The saved file's name (invisible).
#' @export
#' @examples \dontrun{
#' # Saving the demo dataset to evs.2000.hun.Rdatas in current working directory.
#' data(evs.2000.hun)
#' saves("evs.2000.hun")
#' }
#' @author Gergely Daróczi \email{daroczi.gergely@@btk.ppke.hu} 

saves <- function (df, file=paste(df, '.Rdatas', sep='')) {
	if (inherits(try(data <- get(df), silent=TRUE), "try-error")) {
		stop('No dataframe/list given or the given df is not a dataframe/list!')
	}
	if (file.exists(file)) {
		stop('Destination filename already exists! Use other filename.')
	}
	if (!is.data.frame(data) & !is.list(data)) {
		stop('No dataframe/list given or the given df is not a dataframe/list!')
	}
	tmp <- paste(tempdir(), '/saves.temp', sep='')
	dir.create(tmp)
	attach(data, warn.conflicts = FALSE)
	for (i in 1:length(data)) {
		save(list=names(data)[i], file=paste(tmp, '/', names(data)[i], '.Rdata', sep=''))
	}
	detach(data)
	w <- getwd()
	setwd(tmp)
	tar(paste(w, '/', file, sep=''), '.', compression='none')
	setwd(w)
	unlink(tmp, recursive = TRUE)
	invisible(file)
}