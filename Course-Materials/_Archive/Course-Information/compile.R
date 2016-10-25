
library(rmarkdown)
library(knitr)
library(pander)

setwd(file.path(Sys.getenv("USERPROFILE"),
	"Google Drive/WFA8437-Decision-Making/Course-Information"))
render("syllabus.Rmd")



