

library(knitr)


p<- purl("C:/Users/mcolvin/Documents/Teaching/WFA8433-Natural-Resource-Decision-Making/Course-Materials/Class-09.Rmd")

read_chunk(p)
chunks <- knitr:::knit_code$get()
chunkss<- lapply(1:length(chunks),function(x)
    {
    if(!(names(chunks[x]) %in% c("echo=FALSE" ,"eval=FALSE")))
        {
        c(paste0("## ----", names(chunks)[x] ,"---- ##"), 
        chunks[[x]],
        " "," "," ")
        }
    })

xxx<- unlist(chunkss)
writeLines(xxx, 
    "C:/Users/mcolvin/Documents/Teaching/WFA8433-Natural-Resource-Decision-Making/Course-Materials/scripts/Class-09.R")

    
    
    