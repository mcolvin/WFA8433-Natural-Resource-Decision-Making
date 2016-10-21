
library(rmarkdown)
setwd('C:/Users/mcolvin/Google Drive/WFA8433-Natural-Resource-Decision-Making/Classes/class-01')

# DECK
rmarkdown::render("class-01-deck.Rmd",
    output_format='revealjs::revealjs_presentation',
    output_dir='C:/Users/mcolvin/Google Drive/WFA8433-Natural-Resource-Decision-Making/Docs')

# HTML
rmarkdown::render("class-01-deck.Rmd",
    output_format='html_document',
    output_dir='C:/Users/mcolvin/Google Drive/WFA8433-Natural-Resource-Decision-Making/Docs')

    
    