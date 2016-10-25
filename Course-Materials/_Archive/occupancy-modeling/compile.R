
library(rmarkdown)
library(knitr)
setwd("C:/Users/mcolvin/Documents/Teaching/Classes/2017/occupancy-modeling")

render("main.Rmd")


knitr::pandoc('main.md', format='html')
