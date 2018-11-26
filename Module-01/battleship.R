
ships<- data.frame(ship_dim=c(2,3,3,4,5),
    type=c("Destroyer","Sub","Cruiser","Battleship", "Carrier"))

grd<-c(1:10)
coords<-expand.grid(grd,LETTERS[grd])

# RANDOM PLACING

## GENERATE POSSIBLE LOCATIONS

# DETERMINE ALL POSSIBLE LOCATIONS FOR EACH
# SHIP FOR EACH ORIENTATION.
out<-data.frame()
for(j in 1:nrow(ships))
	{
 	start<-sort(rev(grd)[-c(1:(ships$ship_dim[j]-1))])
	stop<-grd[-c(1:(ships$ship_dim[j]-1))]
    tmp<-cbind(start,stop)
    locs<-data.frame()
	for(i in 1:10)
		{
		locs<-rbind(locs,data.frame(class=ships$type[j],
            rc=i,
            start=tmp[,1],stop=tmp[,2]))
		}
    locs$h<-sapply(1:nrow(locs),function(x)
        {
        xx<-locs$start[x]:locs$stop[x]
        xx<-paste(xx,LETTERS[locs$rc[x]],sep="")
        tmpp<- paste(xx,collapse=',')
        })
    locs$v<-sapply(1:nrow(locs),function(x)
        {
        xx<-LETTERS[locs$start[x]:locs$stop[x]]
        xx<-paste(locs$rc[x],xx,sep="")
        tmpp<- paste(xx,collapse=',')
        })  
    out<-rbind(out,locs)
	}

    
# BEGIN PUTTING SHIPS ON BOARD
coords_noships<-coords
ships$start<-NA
ships$stop<-NA


#' # SELECT RANDOM POINTS

# sample a location
xy<-coords_noships[sample(1:nrow(coords_noships),1),]
# convert letter to number 
xy[2]<- which(xy[2]==LETTERS[1:10])

# what ship size
indx<-which(is.na(ships$start))
setup<-sample(indx,1)
# POSSIBLE LOCATIONS (1,2,3,4)
# WHICH ONES CAN FIT
## 1
end_coords<- data.frame(
    x=unlist(c(xy[1],xy[1]-ships$ship_dim[setup],
        ships$ship_dim[setup],xy[1]),xy[1]),
    y= unlist(c(xy[2]-ships$ship_dim[setup],xy[2],
        xy[2]+ships$ship_dim[setup],xy[2])))
end_coords<- end_coords[which(end_coords[,1]>0 & end_coords[,2]>0),]
# select a row
# select a row
indx<-sample(1:nrow(end_coords),1)
xxx<-rownames(end_coords)[indx]
if(xxx %in% c(1,3){}# vertical
if(xxx %in% c(2,4){}# horizonatal




#' ## UNIFORM

#' ## CLUSTERED

#' ## RANDOM






#' # STRATEGIES

#' ## SYSTEMATIC HUNT AND TARGET (BLAST AROUND HITS)

#' ## RANDOM HUNT AND TARGET (BLAST AROUND HITS)

#' ## HIGHER PROBABILITY FOR SPACES SHIPS CAN FIT? 

#' ## RANDOM [SHAM]

