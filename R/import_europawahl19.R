import_europawahl19 <- function() {
	fn <- system.file("europawahl19.xlsx", package="fd.maxent")
	tabs <- c("tab.votes", "tab.base", "tab.gender.votes", "tab.edu.votes")
	for (x in tabs) {
		dat <- read_excel(fn, x)
		class(dat) <- "data.frame"
		if("vote_for" %in% colnames(dat)) {
			# browser()
			rownames(dat) <- dat[,"vote_for"]
			dat[,"vote_for"] <- NULL
		}
		assign(x, dat)
	}
	save(list=tabs, file="europawahl19.rda")
}
