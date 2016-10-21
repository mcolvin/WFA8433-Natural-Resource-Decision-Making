
library(rmarkdown)
library(knitr)
library(pander)


setwd('C:/Users/mcolvin/Google Drive/WFA8433-Natural-Resource-Decision-Making/Course-Materials')
render("index.Rmd",
    output_format='all',
    output_dir='C:/Users/mcolvin/Google Drive/WFA8433-Natural-Resource-Decision-Making/Docs')


