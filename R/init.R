.onLoad <- function(lib,pkg) {
	assign("ggpt.opts", new.env(parent=.GlobalEnv), envir=.GlobalEnv)
	# might be better to use: assign('variable name', 'value', envir=get("ggpt",envir=.GlobalEnv))
	ggpt.opts$fileprefix <- ''
	ggpt.opts$lang <- 'en'
	ggpt.opts$fontfamily <- 'garamond'
	ggpt.opts$en <- list(
			samplesize = 'Sample size: ',
			basesample = 'sample',
			base = 'Observations: ',
			other = '')
}