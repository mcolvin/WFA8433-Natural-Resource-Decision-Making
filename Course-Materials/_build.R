
build<- function(fileName,bld="PAGE",docs=FALSE)
    {# BUILD: PAGE, ENTIRE, SCRIPT
    if(bld=="PAGE")
        {
        library(knitr)
        rmarkdown::render_site(paste(fileName,
            ".Rmd",sep=""))# build website
        } else
    if(bld=="ENTIRE")
        {
        library(knitr)
        rmarkdown::render_site()# build webpage
        } else
    if(bld=="SCRIPT")
        {
        ## PURL R CODE FROM CLASS NOTES
        p<- knitr::purl(paste(fileName,
            ".Rmd",sep=""))
        knitr::read_chunk(p)
        chunks <- knitr:::knit_code$get()
        chunkss<- lapply(1:length(chunks),
            function(x){if(!(names(chunks[x]) %in% c("echo=FALSE" ,"eval=FALSE"))){c(paste0("## ----", names(chunks)[x] ,"---- ##"),chunks[[x]]," "," "," ")}})
        xxx<- unlist(chunkss)
        fp<- paste("./scripts/", fileName,".R",
            sep="")
        writeLines(xxx,fp)
        system(paste("xcopy",
            '"C:/Users/mcolvin/Documents/Teaching/WFA8433-Natural-Resource-Decision-Making/Course-Materials/_site"',     
            '"C:/Users/mcolvin/Documents/Teaching/WFA8433-Natural-Resource-Decision-Making/Docs"',     "/E /C /H /R /K /O /Y")) 
        } else
    if(docs==TRUE)
        {
        system(paste("xcopy",'"C:/Users/mcolvin/Documents/Teaching/WFA8433-Natural-Resource-Decision-Making/Course-Materials/_site"',     '"C:/Users/mcolvin/Documents/Teaching/WFA8433-Natural-Resource-Decision-Making/Docs"',     "/E /C /H /R /K /O /Y")) 
        } else {return(NULL)}
    q(save="no")   
    }