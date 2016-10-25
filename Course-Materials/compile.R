
setwd('C:/Users/mcolvin/Google Drive/WFA8433-Natural-Resource-Decision-Making/Course-Materials')
# RENDER SITE
rmarkdown::render_site()# build website
# COPY FILES TO DOCS FOR GITHUB.IO
system(paste("xcopy", 
    '"C:/Users/mcolvin/Google Drive/WFA8433-Natural-Resource-Decision-Making/Course-Materials/_site"', 
    '"C:/Users/mcolvin/Google Drive/WFA8433-Natural-Resource-Decision-Making/Docs"',
    "/E /C /H /R /K /O /Y")) 
    
    
    