
library(rmarkdown)
library(knitr)
library(pander)

# http://rmarkdown.rstudio.com/revealjs_presentation_format.html

setwd('C:/Users/mcolvin/Google Drive/WFA8433-Natural-Resource-Decision-Making/Classes/class-01')
render("class-01-deck.Rmd",
    output_format='all',
    output_dir='C:/Users/mcolvin/Google Drive/WFA8433-Natural-Resource-Decision-Making/Docs')



