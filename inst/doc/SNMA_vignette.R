## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  dev = "png"
)

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  knitr::knit_exit()
}

if (!requireNamespace("ggnewscale", quietly = TRUE)) {
  knitr::knit_exit()
}




## -----------------------------------------------------------------------------
library(ggplot2)
library(ggnewscale)
library(SNMA)

ggplot()+
  geom_sf(data=stream.boundaries,fill="lightblue",color="lightblue")+ #include the shapefile of the stream area as an illustration
  geom_sf(data=stream.line,aes(color=as.factor(id)),linewidth = 1)+ #include the required shapefile of the center of the stream network including each segment
  theme(axis.text=element_blank(),panel.background=element_blank())+
  labs(color="Stream\nsegments")+
  new_scale_color() +
  geom_point(data=nodes,aes(y=lat,x=lon),size=2) #include the nodes

## -----------------------------------------------------------------------------
str(stream.line)

## -----------------------------------------------------------------------------
check.segments <- as.data.frame(sf::st_coordinates(stream.line)) #extract coordinates and segment IDs
check.segments$order <- 1:nrow(check.segments) #add column of order information
#(in this case continuous across all segments, 
#but could be modified to restart at each segment for better visualization)

#make a plot
ggplot(check.segments,aes(x=X,y=Y,color=order))+
  geom_point()


## -----------------------------------------------------------------------------
nodes

## -----------------------------------------------------------------------------
animal.points

## -----------------------------------------------------------------------------
network.20 <- prep.data(l=stream.line,freq=20,nodes=nodes,lon.name="lon",lat.name="lat",node.name="id")

#the increased.line object contains the new coordinates that were added
ggplot(network.20$increased.line,aes(x=lon,y=lat,color=as.character(id)))+
  geom_point()+
  theme(axis.text=element_blank(),panel.background=element_blank())+
  labs(color="Stream\nsegments")

## -----------------------------------------------------------------------------
network.10 <- prep.data(l=stream.line,freq=10,nodes=nodes,lon.name="lon",lat.name="lat",node.name="id")

ggplot(network.10$increased.line,aes(x=lon,y=lat,color=as.character(id)))+
  geom_point()+
  theme(axis.text=element_blank(),panel.background=element_blank())+
  labs(color="Stream\nsegments")

## -----------------------------------------------------------------------------
network.1 <- prep.data(l=stream.line,freq=1,nodes=nodes,lon.name="lon",lat.name="lat",node.name="id")

ggplot(network.1$increased.line,aes(x=lon,y=lat,color=as.character(id)))+
  geom_point()+
  theme(axis.text=element_blank(),panel.background=element_blank())+
  labs(color="Stream\nsegments")

## -----------------------------------------------------------------------------
ggplot()+
  geom_sf(data=stream.boundaries,fill="lightblue",color="lightblue")+
  geom_point(data=network.1$increased.line,aes(x=lon,y=lat,color=as.character(id)),size=1)+
  theme(axis.text=element_blank(),panel.background=element_blank())+
  labs(color="Stream\nsegments")+
  new_scale_color()+
  geom_point(aes(x=-88.99900,y=17.1774),color="blue",size=6)+
  geom_point(aes(x=-88.99865,y=17.17837),color="blue",size=6)

## -----------------------------------------------------------------------------
calc.stream.dist(p1=c(-88.99900,17.1774),p2=c(-88.99865,17.17837),data=network.1)

## -----------------------------------------------------------------------------
nodes$nodes <- "nodes"

ggplot()+
   geom_sf(data=stream.boundaries,fill="lightblue",color="lightblue")+
  geom_sf(data=stream.line,aes(color=as.factor(id)),linewidth = 1)+
  theme(axis.text=element_blank(),panel.background=element_blank())+
  labs(color="Stream\nsegments",x="lon",y="lat")+
  new_scale_color() +
  geom_point(data=animal.points,aes(x=lon.raw,y=lat.raw,fill=id))+
  geom_text(data=animal.points,label=animal.points$point,aes(x=lon.raw,y=lat.raw,color=id),nudge_y=0.00015,nudge_x=0.00008)+
  scale_color_manual(values=c("red","blue"))+
  guides(color="none")+
  geom_point(data=nodes,aes(y=lat,x=lon,fill=nodes),size=2)+
  geom_text(data=nodes,label=nodes$id,aes(x=lon,y=lat),nudge_y=0.00015,nudge_x=0.00008)+
  geom_point(data=animal.points,aes(x=lon.raw,y=lat.raw,fill=id),shape=22,size=2)+
  scale_fill_manual(values=c("black","red","blue"))+
  labs(fill="Points")

## -----------------------------------------------------------------------------
ggplot()+
   #geom_sf(data=stream.boundaries,fill="lightblue",color="lightblue")+
  geom_sf(data=stream.line,aes(color=as.factor(id)),linewidth = 1)+
  theme(axis.text=element_blank(),panel.background=element_blank())+
  labs(color="Stream\nsegments",x="lon",y="lat")+
  new_scale_color() +
  geom_point(data=animal.points,aes(x=lon.shifted,y=lat.shifted,fill=id))+
  geom_text(data=animal.points,label=animal.points$point,aes(x=lon.shifted,y=lat.shifted,color=id),nudge_y=0.00015,nudge_x=0.00008)+
  scale_color_manual(values=c("red","blue"))+
  guides(color="none")+
  geom_point(data=nodes,aes(y=lat,x=lon,fill=nodes),size=2)+
  geom_text(data=nodes,label=nodes$id,aes(x=lon,y=lat),nudge_y=0.00015,nudge_x=0.00008)+
  geom_point(data=animal.points,aes(x=lon.shifted,y=lat.shifted,fill=id),shape=22,size=2)+
  scale_fill_manual(values=c("black","red","blue"))+
  labs(fill="Points")

## -----------------------------------------------------------------------------
move.results <- movements(data=network.1,
          space.use=T,
          from.previous=T,
          cumulative=T,
          downstream.node=1,
          coords=animal.points,
          lon.name="lon.raw",
          lat.name="lat.raw",
          id.name="id",
          date.time.name="date.time")

move.results

## -----------------------------------------------------------------------------
ggplot(move.results[move.results$id=="turtle2",],aes(x=date.time,y=space.use))+
  geom_point(color="blue")+
  geom_line(color="blue")+
  theme(panel.background=element_blank())

## -----------------------------------------------------------------------------
dist.over.time(data=network.1,
               coords=animal.points,
               lon.name="lon.raw",
               lat.name="lat.raw",
               id.name="id",
               date.time.name="date.time",
               units="days",
               time.diff=1,
               diff.max=NULL,
               sensitivity.min=0,
               sensitivity.max=0,
               sensitivity.change=0)

## -----------------------------------------------------------------------------
dist.over.time(data=network.1,
               coords=animal.points,
               lon.name="lon.raw",
               lat.name="lat.raw",
               id.name="id",
               date.time.name="date.time",
               units="days",
               time.diff=1,
               diff.max=NULL,
               sensitivity.min=0.1,
               sensitivity.max=0.1,
               sensitivity.change=0)

## -----------------------------------------------------------------------------
dist.over.time(data=network.1,
               coords=animal.points,
               lon.name="lon.raw",
               lat.name="lat.raw",
               id.name="id",
               date.time.name="date.time",
               units="days",
               time.diff=1,
               diff.max=NULL,
               sensitivity.min=0.1,
               sensitivity.max=0.2,
               sensitivity.change=0.02)

## -----------------------------------------------------------------------------
dist.over.time(data=network.1,
               coords=animal.points,
               lon.name="lon.raw",
               lat.name="lat.raw",
               id.name="id",
               date.time.name="date.time",
               units="days",
               time.diff=2,
               diff.max=NULL,
               sensitivity.min=0.1,
               sensitivity.max=0.2,
               sensitivity.change=0.04)

## -----------------------------------------------------------------------------
dist.over.time(data=network.1,
               coords=animal.points,
               lon.name="lon.raw",
               lat.name="lat.raw",
               id.name="id",
               date.time.name="date.time",
               units="days",
               sensitivity.min=0.1,
               sensitivity.max=0.2,
               sensitivity.change=0.02,
               custom.times = c(2,4,8))

## -----------------------------------------------------------------------------
dist.over.time(data=network.1,
               coords=animal.points,
               lon.name="lon.raw",
               lat.name="lat.raw",
               id.name="id",
               date.time.name="date.time",
               units="days",
               custom.times = c(2,4,8),
               custom.sensitivity = c(0.1,0.15,.2))

## -----------------------------------------------------------------------------
dist.over.time(data=network.1,
               coords=animal.points,
               lon.name="lon.raw",
               lat.name="lat.raw",
               id.name="id",
               date.time.name="date.time",
               units="days",
               time.diff=1,
               custom.sensitivity = c(.1,.11,.13,.16,.20,.25,.31,.37))

## -----------------------------------------------------------------------------
dist.res <- dist.over.time(data=network.1,
               coords=animal.points,
               lon.name="lon.raw",
               lat.name="lat.raw",
               id.name="id",
               date.time.name="date.time",
               units="days",
               time.diff=1,
               diff.max=NULL,
               sensitivity.min=0.1,
               sensitivity.max=0.2,
               sensitivity.change=0.02)

ggplot(dist.res[dist.res$id=="turtle2",],aes(x=time.diff, y=dist))+
  geom_point(color="blue")+
  geom_smooth(color="blue",fill="blue")+
  theme(panel.background=element_blank())

