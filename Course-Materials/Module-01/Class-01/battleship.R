
ships<- c(2,3,3,4,5)
names(ships)<-c("Destroyer","Sub","Cruiser","Battleship", "Carrier")

horz<-c(1:10)
vert<- letters[horz]
coords1<-coords2<-expand.grid(horz,vert)

# RANDOM PLACING

## GENERATE POSSIBLE LOCATIONS


locs<-data.frame()
for(j in ships)
	{
	x<-sort(rev(vert)[-c(1:(ships[j]-1))])
	y<-vert[-c(1:(ships[j]-1))]
	for(i in 1:10)
		{
		locs<-rbind(locs,data.frame(class=names(ships)[j],
			orientation="vertical",vertId=vert[i],start=tmp[,1],stop=tmp[,2]))
		}
	
	x<-sort(rev(horz)[-c(1:(ships[j]-1))])
	y<-horz[-c(1:(ships[j]-1))]
	for(i in 1:10)
		{
		locs<-rbind(locs,data.frame(class=names(ships)[j],
			orientation="horizontal",vertId=vert[i],start=tmp[,1],stop=tmp[,2]))
		}	
	}

#' # SELECT RANDOM POINTS

#' ## UNIFORM

#' ## CLUSTERED

#' ## RANDOM






#' # STRATEGIES

#' ## SYSTEMATIC HUNT AND TARGET (BLAST AROUND HITS)

#' ## RANDOM HUNT AND TARGET (BLAST AROUND HITS)

#' ## HIGHER PROBABILITY FOR SPACES SHIPS CAN FIT? 

#' ## RANDOM [SHAM]

