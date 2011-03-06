#' Save the variables of a data.frame in distinct binary files
#' 
#' \code{saves} does what the name suggests: it saves a dataframe or list to disk in a special, binary format.
#' This binary format consists of distinct binary files of all separate variables of a dataframe/list
#' merged into an uncompressed tar archive. This is done via a loop, which saves each variable/column
#' to an an external representation of the R objects via \code{save} in a temporary directory.
#' Theese 'Rdata' files are archived to an 'Rdatas' tar archive, uncompressed for better speed.
#' 
#' @param df character string: the name of the data frame or list to save
#' @param file character string: the (Rdatas) filename in which to save the variables in the current
#' working directory
#' @param ultra.fast boolean: if TRUE, ultra fast (...) processing is done without any check to parameters, also no 
#' archiving or compression is done. Be sure if using this setting, as many uncompressed files could be
#' generated in the working directory's subdirectory named to \code{df}. Only recommended
#' for servers dealing with lot of R objects' saves and loads in a monitored environment.
#' @return The saved file's name (invisible).
#' @export
#' @seealso 
#' 	\code{loads} to load R objects from Rdatas binary format 
#' @examples \dontrun{
#' # Saving the demo dataset to evs.2000.hun.Rdatas in current working directory.
#' data(evs.2000.hun)
#' saves("evs.2000.hun")
#' }
#' @author Gergely Daróczi \email{gergely@@snowl.net} 

saves <- function (df, file=paste(df, '.Rdatas', sep=''), ultra.fast=FALSE) {
	if (ultra.fast == TRUE) {
		data <- get(df)
		dir.create(df)
		attach(data, warn.conflicts = FALSE)
		for (i in 1:length(data)) {
			save(list=names(data)[i], file=paste(df, '/', names(data)[i], '.Rdata', sep=''),
					compress=FALSE, precheck=FALSE)
		}
		detach(data)
		return(invisible(df))
	}
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
	attach(data, warn.conflicts = FALSE)   # add envir!
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