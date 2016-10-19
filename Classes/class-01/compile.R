
library(rmarkdown)
library(knitr)
library(pander)

# http://rmarkdown.rstudio.com/revealjs_presentation_format.html

setwd(file.path(Sys.getenv("USERPROFILE"),
	"Google Drive/WFA8437-Decision-Making/classes/class-01"))
render("class-01-deck.Rmd")



