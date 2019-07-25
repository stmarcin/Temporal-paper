# library loading
Packages <- c("data.table", "chron", "plyr", "ggplot2")
lapply(Packages, library, character.only = TRUE)
rm(Packages)


folder <- "materialy/GTFS_Szczecin"

routes <- fread(paste(folder, "routes.txt", sep="/"))
stop_times <- fread(paste(folder, "stop_times.txt", sep="/"))
trips <- fread(paste(folder, "trips.txt", sep="/"))
calendar <- fread(paste(folder, "calendar.txt", sep="/"))

# select trips on Tuesdays
SelID <- subset(calendar, tuesday==1, select="service_id")

stop_times <- merge(stop_times[,1:5], trips[,c(1:3)], by="trip_id")
stop_times <- stop_times[, .SD[1], trip_id]
stop_times$h <- as.integer(substr(stop_times$arrival_time,1,2))

NumTrips <- stop_times[stop_times$service_id %in% SelID$service_id]

CountTrip <- as.data.table(count(NumTrips, "h"))

# combine travels between the days
CountTrip[1,2] <- sum(subset(CountTrip, select=freq, CountTrip$h==3| CountTrip$h==27))
CountTrip[2,2] <- sum(subset(CountTrip, select=freq, CountTrip$h==4| CountTrip$h==28))
CountTrip <- CountTrip[-c(25,26),]
CountTrip[23,1] <- 1
CountTrip[24,1] <- 2
CountTrip <- CountTrip[order(h)]

CountTrip$h <- CountTrip$h+0.5
CountTrip$sel = "grey"
CountTrip[c(2,7,10,22),3]<- "black"


ggplot(CountTrip, aes(x=h, y=freq)) +
    geom_bar(stat="identity", aes(fill=sel)) +
    scale_fill_manual(values=c("darkred", "darkgrey"))+
    labs(x="hour",y="number of vehicles") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y=element_text(size=16, margin = margin(t = 0, r = 0, b = 0, l = 10)),
          axis.title = element_text(size=30),
          axis.text.x = element_text(size=16),
          legend.position="none") +
    scale_x_continuous(limits=c(0,25), breaks=c(6,12,18,24)) +
    geom_rect(aes(xmin = 18.2, xmax = 19.7, ymin = 580, ymax = 635), fill = "darkred") +
    annotate("text", x = 20, y = 610, label = "Selected 1-hour-long \n periods", hjust = 0, size = 7)
    
ggsave("Plot_Freq.png", path="Results/t07_Graphs", width=11.5,height=8.6, scale=3, units="cm" )

