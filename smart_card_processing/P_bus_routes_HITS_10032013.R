library(rjson)
json_file <-"http://cheeaun.github.com/busrouter-sg/data/bus-services.json"
bus_services1<-fromJSON(paste(readLines(json_file), collapse=""))
m <- lapply(bus_services1$`Trunk Bus Services`, function(x) c(x$no, x$dir, x$provider))
m[[80]][3]<-"smrt"
m <- matrix(unlist(m), ncol=3, byrow=TRUE)
m<-data.frame(m, stringsAsFactors=F)
bus_services1<-m
n<-nrow(bus_services1)

bus_route1<-matrix(0,1,3)
bus_route2<-matrix(0,1,3)
bus_stop1<-matrix(0,1,3)
bus_stop2<-matrix(0,1,3)

for (i in 1:n){

download.file(paste("http://cheeaun.github.com/busrouter-sg/data/bus-services/",m[i,1],".json", sep=""), paste("Z:/Thesis/Station-based activities/Other datasets/bus_stops_routes/route_",i, sep=""))

}

bus_route1<-NULL
bus_route2<-NULL
bus_stop1<-NULL
bus_stop2<-NULL

for (i in 1:n){
json_file <-paste("http://cheeaun.github.com/busrouter-sg/data/bus-services/",m[i,1],".json", sep="")
bus_route<-fromJSON(paste(readLines(json_file), collapse=""))
bus_route1<-rbind(bus_route1, cbind(row(as.data.frame(bus_route$'1'$route)),bus_route$'1'$route, m[i,1]))
if(length(bus_route$'1'$stops)>0){
bus_stop1<-rbind(bus_stop1, cbind(row(as.data.frame(bus_route$'1'$stops)),bus_route$'1'$stops, m[i,1]))
}
if(length(bus_route$'2'$route)>0){
bus_route2<-rbind(bus_route2, cbind(row(as.data.frame(bus_route$'2'$route)),bus_route$'2'$route, m[i,1]))
}
if(length(bus_route$'2'$stops)>0){
bus_stop2<-rbind(bus_stop2, cbind(row(as.data.frame(bus_route$'2'$stops)),bus_route$'2'$stops, m[i,1]))
}
}




json_file<-"http://cheeaun.github.com/busrouter-sg/data/bus-stops.json"
bus_stops<-fromJSON(paste(readLines(json_file), collapse=""))
m <- lapply(bus_stops, function(x) c(x$coords, x$name))
m <- matrix(unlist(m), ncol=2, byrow=TRUE)
m<-data.frame(m)
bus_stops<-m

bus_route<-matrix(0,1,3)
bus_stop<-matrix(0,1,3)
for (i in 1:n){
if(nrow(bus_route1[bus_route1$V3==m[i,1],]) >= nrow(bus_route2[bus_route1$V3==m[i,1],])){ 
bus_route_temp<-bus_route1[bus_route1$V3==m[i,1],]
bus_stop_temp<-bus_stop1[bus_stop1$V3==m[i,1],]
} else {
bus_route_temp<-bus_route2[bus_route2$V3==m[i,1],]
bus_stop_temp<-bus_stop2[bus_stop2$V3==m[i,1],]
}
bus_route<-rbind(bus_route, bus_route_temp)
bus_stop<-rbind(bus_stop, bus_stop_temp)
}



json_file <-"http://cheeaun.github.com/busrouter-sg/data/bus-stops-services.json"
bus_stops_services<-fromJSON(paste(readLines(json_file), collapse=""))


HITS08_trips$gmap_url<-ifelse(!is.na(HITS08_trips$origin_lat) & !is.na(HITS08_trips$dest_lat) & HITS08_trips$T10_Mode %in% c("Car driver","Car passenger", "Company bus", "Cycle", "Motorcycle rider", "Others", "School bus", "Shuttle bus", "Taxi",  "Van/lorry driver", "Van/lorry passenger", "Motorcycle passenger"),  
paste("http://maps.googleapis.com/maps/api/directions/json?origin=", HITS08_trips$origin_lat, ",", HITS08_trips$origin_lon,"&destination=", HITS08_trips$dest_lat, ",", HITS08_trips$dest_lon, "&region=sg&sensor=false", sep=""), 0)


Route<-matrix(0, 1, 11)
n<-nrow(HITS08_trips)

for (i in 1:n){
if(HITS08_trips$gmap_url[i]!=0){
route<-fromJSON(paste(readLines(HITS08_trips$gmap_url[i]), collapse=""))
if(length(route$routes)==0){
Sys.sleep(10)
i=i-1
}else{
m <- lapply(route$routes[[1]]$legs[[1]]$steps, function(x) c(x$distance$text, x$distance$value
, x$duration$value, x$end_location$lat, x$end_location$lng, x$start_location$lat, x$start_location$lng))
m <- as.data.frame(do.call(rbind, m))
m1<- as.matrix(cbind(HITS08_trips$H1_HHID[i], HITS08_trips$Pax_ID[i], HITS08_trips$Trip_ID[i], HITS08_trips$Stage_ID[i], m, i))
Route<-rbind(Route, m1)
}
}
}


table(HITS08_trips$T11_Board_Svc_Stn[HITS08_trips$T10_Mode=="MRT"])
table(HITS08_trips$T12_Alight_Stn[HITS08_trips$T10_Mode=="MRT"])
table(HITS08_trips$T11_Board_Svc_Stn[HITS08_trips$T10_Mode=="LRT"])

library(igraph)
sg_rail<-read.csv(file.choose(), header=T, stringsAsFactors=F)
sg_rail_network<-graph.data.frame(sg_rail, directed=F)
sg_rail_stop<-read.csv(file.choose(), header=T, stringsAsFactors=F)

HITS08_trips$tag<-ifelse(nchar(as.character(HITS08_trips$T3_StartTime))==4, 1, 0)
HITS08_trips$s_time<-ifelse(HITS08_trips$tag==1, as.numeric(substr(as.character(HITS08_trips$T3_StartTime),1,2))* 60 +
as.numeric(substr(as.character(HITS08_trips$T3_StartTime),3,4)), as.numeric(substr(as.character(HITS08_trips$T3_StartTime),1,1))* 60 + as.numeric(substr(as.character(HITS08_trips$T3_StartTime),2,3)))
HITS08_trips$s_time<-ifelse(nchar(as.character(HITS08_trips$T3_StartTime))==2, as.numeric(substr(as.character(HITS08_trips$T3_StartTime),1,2)), HITS08_trips$s_time)

HITS08_trips$tag<-ifelse(nchar(as.character(HITS08_trips$T4_EndTime))==4, 1, 0)
HITS08_trips$e_time<-ifelse(HITS08_trips$tag==1, as.numeric(substr(as.character(HITS08_trips$T4_EndTime),1,2))* 60 +
as.numeric(substr(as.character(HITS08_trips$T4_EndTime),3,4)), as.numeric(substr(as.character(HITS08_trips$T4_EndTime),1,1))* 60 + as.numeric(substr(as.character(HITS08_trips$T4_EndTime),2,3)))
HITS08_trips$e_time<-ifelse(nchar(as.character(HITS08_trips$T4_EndTime))==2, as.numeric(substr(as.character(HITS08_trips$T4_EndTime),1,2)), HITS08_trips$e_time)

HITS08_trips$e_time<-ifelse(HITS08_trips$e_time>1440, HITS08_trips$e_time-1440, HITS08_trips$e_time)
HITS08_trips$t_time1<-HITS08_trips$s_time+HITS08_trips$T10a_WalkTime/2+HITS08_trips$T13_WaitTime
HITS08_trips$t_time2<-HITS08_trips$e_time - HITS08_trips$t_time1
HITS08_trips$t_time2<-ifelse(HITS08_trips$t_time2<0, HITS08_trips$t_time2+1440, HITS08_trips$t_time2)
HITS08_trips$t_time2<-ifelse(HITS08_trips$t_time2>800, HITS08_trips$e_time-HITS08_trips$s_time, HITS08_trips$t_time2)

hits_rail_stops<-read.csv(file.choose(), header=T, stringsAsFactors=F)

hits_rail_stops<-read.csv(file.choose(), header=T, stringsAsFactors=F)
test<-cbind(hits_rail_stops$name, hits_rail_stops$station_id)
colnames(test)<-c("T11_Board_Svc_Stn", "board")
HITS08_trips<-merge(HITS08_trips, test, by="T11_Board_Svc_Stn", all.x=T)
colnames(test)<-c("T12_Alight_Stn", "alight")
HITS08_trips<-merge(HITS08_trips, test, by="T12_Alight_Stn", all.x=T)

Route_MRT<-matrix(0, 1, 24)
n<-nrow(HITS08_trips)

for (i in 76045:88600){
if(HITS08_trips$T10_Mode[i] %in% c("MRT", "LRT")){
m<-sg_rail_stop[get.shortest.paths(sg_rail_network, HITS08_trips$board[i], HITS08_trips$alight[i])[[1]],]
#board<-sg_rail_stop[sg_rail_stop$stop_id==HITS08_trips$board[i],]
#m<-rbind(board,m)
m$dist<-c(0, sqrt(diff(m$lng)^2 + diff(m$lat)^2))
m$time<-HITS08_trips$t_time1[i] + HITS08_trips$t_time2[i] * cumsum(m$dist)/sum(m$dist) 
m$type<-"station"
m$type[nrow(m)]<-"alight_station"
m$type[1]<-"board_station"
a<-c(0, "origin",  HITS08_trips$origin_lat[i], HITS08_trips$origin_lon[i], sqrt((HITS08_trips$origin_lat[i]-m$lng[1])^2 + (HITS08_trips$origin_lon[i]-m$lat[1])^2), HITS08_trips$s_time[i], "origin")
b<-c(0, "destination",  HITS08_trips$dest_lat[i], HITS08_trips$dest_lon[i], sqrt((HITS08_trips$dest_lat[i]-m$lng[nrow(m)])^2 + (HITS08_trips$origin_lon[i]-m$lat[nrow(m)])^2), HITS08_trips$e_time[i], as.character(HITS08_trips$T5_PlaceType[i]))
m<-rbind(a, m, b)
m$path_id<-row(m)[,1]
m1<- as.matrix(cbind(HITS08_trips$H1_HHID[i], HITS08_trips$Pax_ID[i], HITS08_trips$Trip_ID[i], HITS08_trips$Stage_ID[i], HITS08_trips$T10_Mode[i], HITS08_trips$T6_Purpose[i], HITS08_trips$origin_lat[i], HITS08_trips$origin_lon[i], HITS08_trips$dest_lat[i], HITS08_trips$dest_lon[i], HITS08_trips$s_time[i], HITS08_trips$e_time[i], HITS08_trips$T10a_WalkTime[i], HITS08_trips$T13_WaitTime[i], HITS08_trips$T14_InVehTime[i], m, i))
Route_MRT<-rbind(Route_MRT, m1)
}
}

library(rjson)
json_file<-"http://cheeaun.github.com/busrouter-sg/data/bus-stops.json"
bus_stops<-fromJSON(paste(readLines(json_file), collapse=""))
test<-attr(bus_stops, "names")
m <- lapply(bus_stops, function(x) c(x$coords, x$name))
m <- matrix(unlist(m), ncol=2, byrow=TRUE)
m<-data.frame(m)
bus_stops<-m
test<-gsub(".coords", "", test)
test<-gsub(".name", "", test)
bus_stops$id<-unique(test)
bus_stop1<-as.data.frame(bus_stop1)
bus_stop2<-as.data.frame(bus_stop2)


colnames(bus_stop1)<-c("order", "sid", "route")
colnames(bus_stop2)<-c("order", "sid", "route")
colnames(bus_stops)<-c("latlon", "name", "sid")
bus_stop1<-merge( bus_stop1, bus_stops, by="sid", all.x=T)
bus_stop2<-merge( bus_stop2, bus_stops, by="sid", all.x=T)
bus_stop1$latlon<-as.character(bus_stop1$latlon)
bus_stop1<-bus_stop1[2:nrow(bus_stop1),]
bus_stop2<-bus_stop2[2:nrow(bus_stop2),]
bus_stop1$route<-as.character(bus_stop1$route)
bus_stop2$route<-as.character(bus_stop2$route)
bus_stop2$lon<-sapply(bus_stop2$latlon, function(x)as.numeric(strsplit(as.character(x), ",")[[1]][2]))
bus_stop2$lat<-sapply(bus_stop2$latlon, function(x)as.numeric(strsplit(as.character(x), ",")[[1]][1]))
bus_stop1$lon<-sapply(bus_stop1$latlon, function(x)as.numeric(strsplit(as.character(x), ",")[[1]][2]))
bus_stop1$lat<-sapply(bus_stop1$latlon, function(x)as.numeric(strsplit(as.character(x), ",")[[1]][1]))
bus_stop1$order<-as.numeric(as.character(bus_stop1$order))
bus_stop2$order<-as.numeric(as.character(bus_stop2$order))

find_busstop<-function(lat, lon, lat1, lon1, route){
l<-bus_stop1[bus_stop1$route==route,]
l<-l[order(l$order),]
if (nrow(l)>0){
a<-which.min((l$lat-lat)^2+(l$lon-lon)^2)
b<-which.min((l$lat-lat1)^2+(l$lon-lon1)^2)
if (length(l$order[a])>0 & length(l$order[b])>0){
if(l$order[a]>l$order[b]){
return(c(l$lat[a], l$lon[a], l$name[a], l$sid[a], l$order[a], l$lat[b], l$lon[b], l$name[b], l$sid[b], l$order[b], 1))
}
else {
l<-bus_stop2[bus_stop2$route==route,]
l<-l[order(l$order),]
a<-which.min((l$lat-lat)^2+(l$lon-lon)^2)
b<-which.min((l$lat-lat1)^2+(l$lon-lon1)^2)
return(c(l$lat[a], l$lon[a], l$name[a], l$sid[a], l$order[a], l$lat[b], l$lon[b], l$name[b], l$sid[b], l$order[b], 2))
}
if(a==b){
return(c(l$lat[a], l$lon[a], l$name[a], l$sid[a], l$order[a], l$lat[b+1], l$lon[b+1], l$name[b+1], l$sid[b+1], l$order[b+1], 1))
}
} else{
return(c(0,0,0,0,0,0,0,0,0,0,0))
}
} else {
return(c(0,0,0,0,0,0,0,0,0,0,0))
}
}



##find_busstop(103.85, 1.29, 103.95, 1.37, 2)
 bus_route1<-as.data.frame(bus_route1)
 colnames(bus_route1)<-c("rorder", "latlon", "route")
 bus_route1<-bus_route1[-1,]
 bus_route1$lon<-as.data.frame(matrix(unlist(strsplit(as.character(bus_route1[,2]), ",")),ncol=2, byrow=T))[,2]
 bus_route1$lat<-as.data.frame(matrix(unlist(strsplit(as.character(bus_route1[,2]), ",")),ncol=2, byrow=T))[,1]
 bus_route1$lon<-as.numeric(as.character(bus_route1$lon))
 bus_route1$lat<-as.numeric(as.character(bus_route1$lat))
 bus_route1$rorder<-as.numeric(as.character(bus_route1$rorder)) 
 bus_route1$route<-as.character(bus_route1$route)

 bus_route2<-as.data.frame(bus_route2)
 colnames(bus_route2)<-c("rorder", "latlon", "route")
 bus_route2<-bus_route2[-1,]
 bus_route2$lon<-as.data.frame(matrix(unlist(strsplit(as.character(bus_route2[,2]), ",")),ncol=2, byrow=T))[,2]
 bus_route2$lat<-as.data.frame(matrix(unlist(strsplit(as.character(bus_route2[,2]), ",")),ncol=2, byrow=T))[,1]
 bus_route2$lon<-as.numeric(as.character(bus_route2$lon))
 bus_route2$lat<-as.numeric(as.character(bus_route2$lat))
 bus_route2$rorder<-as.numeric(as.character(bus_route2$rorder)) 
 bus_route2$route<-as.character(bus_route2$route)

 
 bus_route$lon<-as.data.frame(matrix(unlist(strsplit(as.character(bus_route[,2]), ",")),ncol=2, byrow=T))[,2]
 bus_route$lat<-as.data.frame(matrix(unlist(strsplit(as.character(bus_route[,2]), ",")),ncol=2, byrow=T))[,1]
 bus_route$lon<-as.numeric(as.character(bus_route$lon))
 bus_route$lat<-as.numeric(as.character(bus_route$lat))

 
 find_busroute<-function(HHID, person_id, trip_id, stage_id, lat, lon, lat1, lon1, route, dir){
 if (dir!=0){
 l<-bus_route[bus_route$route==route,]
 l<-l[order(l$order),]
 a<-which.min((l$lat-lat)^2+(l$lon-lon)^2)
 b<-which.min((l$lat-lat1)^2+(l$lon-lon1)^2)
 ori<-rbind(c(a-1, paste(lat, lon, sep=","), route,  lon, lat))
 des<-rbind(c(b+1, paste(lat1, lon1, sep=","), route, lon1, lat1))
 if(l$order[a]<l$order[b]){  
 l<-as.matrix(l)
 l1<-rbind(ori, l[a:b,], des)
 } else {
 l<-as.matrix(l)
 l1<-rbind(des, l[b:a,], ori)
 l1<-l1[nrow(l1):1,]
 }
 l2<-cbind(HHID, person_id, trip_id, stage_id, l1)
 l2<-as.data.frame(l2, stringsAsFactors=F)
 rownames(l2)<-1:nrow(l2)
 return(l2)
 }
 }
 
 bus_route1$sid<-0
 bus_route2$sid<-0

 HITS08_trips<-read.csv(file.choose(), header=T, stringsAsFactors=F)
 
 HITS08_trips$bus_o_lat<-ifelse(HITS08_trips$T10_Mode == "Public Bus", find_busstop(HITS08_trips$origin_lon, HITS08_trips$origin_lat, HITS08_trips$dest_lon, HITS08_trips$dest_lat, HITS08_trips$route)[1],0)
 
 bus_trip<-sapply(1:nrow(HITS08_trips), function(i, x){find_busstop(x$origin_lon[i], x$origin_lat[i], x$dest_lon[i], x$dest_lat[i], x$route[i])}, HITS08_trips) 
 
 HITS08_trips$bus_o_lat<-sapply(1:nrow(HITS08_trips), function(i,x,y){ifelse(x$T10_Mode[i] == "Public Bus" & length(y[[i]][[1]])>0 ,  y[[i]][[1]], 0)}, HITS08_trips, bus_trip)
 
 find_busstop(HITS08_trips$origin_lon[51374], HITS08_trips$origin_lat[51374], HITS08_trips$dest_lon[51374], HITS08_trips$dest_lat[51374], HITS08_trips$route[51374])
 
  find_busroute(HITS08_trips$H1_HHID[59374], HITS08_trips$Pax_ID[59374], HITS08_trips$Trip_ID[59374], HITS08_trips$Stage_ID[59374], bus_trip1$stop1_lat[59374], bus_trip1$stop1_lon[59374], bus_trip1$stop2_lat[59374], bus_trip1$stop2_lon[59374], HITS08_trips$route[59374], bus_trip1$dir[59374], bus_trip1$stop1_sid[59374], bus_trip1$stop2_sid[59374])
  
  bus_trip<-as.data.frame(t(bus_trip), stringsAsFactors=F)
  colnames(bus_trip)<-c("stop1_lat", "stop1_lon", "stop1_name", "stop1_sid", "stop1_order", "stop2_lat", "stop2_lon", "stop2_name", "stop2_sid", "stop2_order", "dir")
 
  Route_bus<-matrix (0, 1, 11)
  
  for (i in 51397:76043){
  if (bus_trip1$dir[i]!=0 & bus_trip1$stop1_name[i]!=2){
  m<-find_busroute(HITS08_trips$H1_HHID[i], HITS08_trips$Pax_ID[i], HITS08_trips$Trip_ID[i], HITS08_trips$Stage_ID[i], bus_trip1$stop1_lat[i], bus_trip1$stop1_lon[i], bus_trip1$stop2_lat[i], bus_trip1$stop2_lon[i], HITS08_trips$route[i], bus_trip1$dir[i])
  m$dist<-c(0, sqrt(diff(as.numeric(m[,8]))^2 + diff(as.numeric(m[,9]))^2))
  m$time<-HITS08_trips$t_time1[i] + HITS08_trips$t_time2[i] * cumsum(m$dist)/sum(m$dist)
  m$order<-row(m)[,1]
  m<-as.matrix(m)
  Route_bus<-rbind(Route_bus, m)
  }
  }
  
  
 HITS08_trips$route<-ifelse(HITS_bus_stop$T10_Mode=="Public Bus", HITS_bus_stop$T11_Board_Svc_Stn, 0)
 
 #HITS08_trips$SegID<-paste(HITS08_trips$H1_HHID, HITS08_trips$Pax_ID, HITS08_trips$Trip_ID, HITS08_trips$Stage_ID, sep="") 
 #HITS08$SegID<-paste(HITS08$H1_HHID, HITS08$Pax_ID, HITS08$Trip_ID, HITS08$Stage_ID, sep="") 
 
 # which(HITS_bus_stop$V4=="Public Bus" & (HITS_bus_stop$V2 != HITS_bus_stop$V3))
 
 
 
 Route_car$path_id<-paste(Route_car$hhid, Route_car$personid, Route_car$tripid, Route_car$stageid, sep="")
 HITS08_trips$path_id<-paste(HITS08_trips$H1_HHID, HITS08_trips$Pax_ID, HITS08_trips$Trip_ID, HITS08_trips$Stage_ID, sep="")
 attach(HITS08_trips)
 hits_path<-cbind(path_id, T10_Mode, T6_Purpose, origin_lat, origin_lon, dest_lat, dest_lon, s_time, e_time, T10a_WalkTime, T13_WaitTime, T5_PlaceType, route)
 hits_path<-as.data.frame(hits_path, stringsAsFactors=F)
 Route_car1<-merge(Route_car, hits_path, by="path_id", all.x=T) 
 library(plyr)
 route_car_total<-ddply(Route_car1, c("path_id"), function(x)c(sum(x$distm), sum(x$min)))
 colnames(route_car_total)<-c("path_id","tot_dist", "tot_sec")
 Route_car1<-merge(Route_car1, route_car_total, by="path_id", all.x=T)
 
 Route_car1[,"cum_dist"] <- ave(Route_car1$distm,by=Route_car1$path_id, FUN=cumsum)
 Route_car1$e_time<-as.numeric(Route_car1$e_time)
 Route_car1$s_time<-as.numeric(Route_car1$s_time)
 Route_car1[,"time"] <-Route_car1$s_time + (Route_car1$e_time-Route_car1$s_time)*Route_car1$cum_dist/Route_car1$tot_dist
 
 Route_car1$X<-row(Route_car1)[,1]
 route_car_first<-ddply(Route_car1, c("path_id"), function(x)c(x[1,]))
 Route_car1$X<-NULL
 route_car_first1<-Route_car1[route_car_first$V1,]
 Route_car1[,order]<-ave(1,by=Route_car1$path_id, FUN=cumsum)
 
 Route_car1$count=1
 Route_car1[,"order"]<-ave(Route_car1$count,by=Route_car1$path_id, FUN=cumsum)
 Route_car1$count=NULL
 Route_car1$type<-ifelse(Route_car1$cum_dist == Route_car1$tot_dist, Route_car1$T5_PlaceType, "on road") 
 
 route_car_first1$latd<-route_car_first1$lato
 route_car_first1$lngd<-route_car_first1$lngo
 route_car_first1$order=0
 route_car_first1$type="origin"
 
 Route_car2<-rbind(route_car_first1, Route_car1)
 Route_car2<-Route_car2[order(Route_car2$path_id),]

 attach(Route_car2)
 Path_car<-cbind(path_id, hhid, personid, tripid, T10_Mode, T6_Purpose, latd, lngd, order, type, time, distm, cum_dist, route)
 Path_car<-as.data.frame(Path_car, stringsAsFactors=F)
 colnames(Path_car)<-c("path_id", "hhid", "personid", "tripid", "mode", "activity", "latd", "lngd", "order", "type", "time", "dist", "cum_dist", "route")
 detach(Route_car2)
 
 Route_bus1$path_id<-paste(Route_bus1$HHID, Route_bus1$person_id, Route_bus1$trip_id, Route_bus1$stage_id, sep="")
 # Route_bus1<-merge(Route_bus1, hits_path1, by="path_id", all.x=T) 
 # Route_bus1$type<-Route_bus1$sid
 
 Route_bus1$count=1
 Route_bus1[,"order"]<-ave(Route_bus1$count,by=Route_bus1$path_id, FUN=cumsum)
 bus_maxn<-ddply(Route_bus1, "path_id", function(x)max(x$order))
 hits_path1<-merge(hits_path1, bus_maxn, by="path_id", all.x=T)

 hits_path1$HHID<-substr(hits_path1$path_id,1,8)
 hits_path1$person_id<-substr(hits_path1$path_id,9,9)
 hits_path1$trip_id<-substr(hits_path1$path_id,10,10)
 hits_path1$stage_id<-substr(hits_path1$path_id,11,11)
 
 attach(hits_path1)
 path_bus_first<-cbind(HHID, person_id, trip_id, route, origin_lat, origin_lon, 0, s_time, path_id, 0, T6_Purpose)
 path_bus_last<-cbind(HHID, person_id, trip_id, route, dest_lat, dest_lon, -1, e_time, path_id, V1+1, T5_PlaceType)
 colnames(path_bus_first)<-c("HHID", "person_id", "trip_id", "route", "lon", "lat", "dist", "time", "path_id", "order", "type")
 colnames(path_bus_last)<-c("HHID", "person_id", "trip_id", "route", "lon", "lat", "dist", "time", "path_id", "order", "type")
 path_bus_first<-as.data.frame(path_bus_first, stringsAsFactors=F)
 path_bus_last<-as.data.frame(path_bus_last, stringsAsFactors=F)
 Route_bus1<-rbind(path_bus_first, Route_bus1, path_bus_last)
 detach(hits_path1)
 Route_bus1$mode<-"bus"
 Route_bus1<-Route_bus1[order(Route_bus1$path_id, Route_bus1$order),]
 Route_bus1[,"cum_dist"]<-ave(Route_bus1$dist, by=Route_bus1$path_id, FUN=cumsum)
 
 attach(Route_bus1)
 Path_transit<-cbind(path_id, HHID, person_id, trip_id, mode, activity1, lat, lon, order, type, time, dist, cum_dist, route)
 Path_transit<-as.data.frame(Path_transit, stringsAsFactors=F)
 colnames(Path_transit)<-c("path_id", "hhid", "personid", "tripid", "mode", "activity", "latd", "lngd", "order", "type", "time", "dist", "cum_dist", "route")
 detach(Route_bus1)
 
 Route_MRT<-read.csv(file.choose(), header=T, stringsAsFactors=F)
 colnames(Route_MRT)<-c("HHID", "person_id", "trip_id", "stage_id", "mode", "purpose", "origin_lon", "origin_lat", "dest_lon", "dest_lat", "s_time", "e_time", "walk_time", "wait_time", "in_veh_time", "SID", "stop_id", "lng", "lat", "dist", "time", "type", 
 "order", "id1")
 Route_MRT$path_id<-paste(Route_MRT$HHID, Route_MRT$person_id, Route_MRT$trip_id, Route_MRT$stage_id, sep="")

 Route_MRT[,"cum_dist"]<-ave(Route_MRT$dist, by=Route_MRT$path_id, FUN=cumsum)
 Route_MRT$route<-Route_MRT$mode
 
 attach(Route_MRT)
 Path_mrt<-cbind(path_id, HHID, person_id, trip_id, mode, purpose, lat, lng, order, type, time, dist, cum_dist, route)
 Path_mrt<-as.data.frame(Path_mrt, stringsAsFactors=F)
 colnames(Path_mrt)<-c("path_id", "hhid", "personid", "tripid", "mode", "activity", "latd", "lngd", "order", "type", "time", "dist", "cum_dist", "route")
 detach(Route_MRT)
 
 ### Other Modes: Company Car 
 path<-rbind(Path_car, Path_mrt)
 path<-path[order(path$path_id),]
 path_exist<-unique(path$path_id)
 path_test<-path[path$path_id%in%path_exist[1:1000],]
 path_test$person_id<-paste(path_test$hhid, path_test$personid, sep="")

 Path_mrt1<-Path_mrt[Path_mrt$path_id%in%path_exist[1:200],]
 Path_mrt1$person_id<-paste(Path_mrt1$hhid, Path_mrt1$personid, sep="")
 Path_mrt2<-Path_mrt1[,c("person_id", "latd", "lngd", "time","type")]
 path_json_mrt<-toJSON(makeList(Path_mrt2))
 write.table(path_json_mrt, "path_mrt.json", row.names=F, col.names =F)
 
 Path_bus1<-Path_transit[Path_transit$path_id%in%path_exist[1:200],]
 Path_bus1$person_id<-paste(Path_bus1$hhid, Path_bus1$personid, sep="")
 Path_bus2<-Path_bus1[,c("person_id", "latd", "lngd", "time","type")]
 path_json_bus<-toJSON(makeList(Path_bus2))
 write.table(path_json_bus, "path_bus.json", row.names=F, col.names =F)
 
 Path_car1<-Path_car[Path_car$path_id%in%path_exist[1:200],]
 Path_car1$person_id<-paste(Path_car1$hhid, Path_car1$personid, sep="")
 Path_car2<-Path_car1[,c("person_id", "latd", "lngd", "time","type")]
 path_json_car<-toJSON(makeList(Path_car2))
 write.table(path_json_car, "path_car.json", row.names=F, col.names =F)
 
 rownames(path_test)<-path_test$person_id
 path_test1<-path_test[,c("person_id", "latd", "lngd", "time","type")]
 
 makeList<-function(x){
  if(ncol(x)>4){
    listSplit<-split(x[-1],x[1],drop=T)
    lapply(names(listSplit),function(y){list(path_id=y,path=makeList(listSplit[[y]]))})
  }else{
    lapply(seq(nrow(x[1])),function(y){list(lat=x[,1][y],lon=x[,2][y],time=x[,3][y],loc=x[,4][y])})
  }
}

 makeList<-function(x){
  if(ncol(x)>4){
    listSplit<-split(x[c(-1:-2)],x[c(1:2)],drop=T)
    lapply(names(listSplit),function(y){list(path_id=y,path=makeList(listSplit[[y]]))})
  }else{
    lapply(seq(nrow(x[1])),function(y){list(lat=x[,1][y],lon=x[,2][y],time=x[,3][y],loc=x[,4][y])})
  }
}



path_json_1000<-toJSON(makeList(path_test1))
write.table(path_json_1000, "path1000.json", row.names=F, col.names =F)




