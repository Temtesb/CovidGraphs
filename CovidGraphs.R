#https://covid19.healthdata.org/projections
#https://stackoverflow.com/questions/39190511/assigning-value-to-the-list-element-in-r
#pacman::p_load(pacman, readr, dplyr)# faster option, but we can't use readr
pacman::p_load(pacman, dplyr, ggplot2, pdftools, data.table, lubridate,gridExtra, grDevices, grid)


baseUrl<-"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports"
startDate<-as.Date("2020-03-23") #using this date because there was no data before this point
lastDay<-Sys.Date()-1

dateRange<-format(seq(startDate,lastDay,"day"),"%m-%d-%Y")
dateRangeGraph<-sort(as.Date(seq(startDate,lastDay,"day")),descending=FALSE)

fileList<-paste0(baseUrl,"/",dateRange, ".csv")

#data<-lapply(fileList,read_csv) # faster option, but we can't use readr
data<-lapply(fileList,read.csv)
localCities<-c("Chesapeake","Hampton","James City","Newport News","Norfolk","Portsmouth","Suffolk","Virginia Beach","Williamsburg")
combinedKeyLocations<-paste(localCities,"Virginia", "US", sep=", ")

output<-list()
for(i in 1:length(data)) { 
  nam <- paste0("localData",i)
  dat <- data[[i]]
  localDatFiltered<-dat %>%
    filter(Combined_Key %in% combinedKeyLocations)%>%
    select(Last_Update,Confirmed, Deaths, Recovered, Active)
  localDatSummary<-localDatFiltered%>%
    summarise(Confirmed=sum(Confirmed),Deaths=sum(Deaths),Recovered=sum(Recovered))
  
  
  assign(nam, localDatSummary)
  localDatSummary<-as.numeric(localDatSummary)
  output[[i]]<-localDatSummary
}
str(output)
output<-data.frame(matrix(unlist(output),ncol=3, byrow=TRUE))

output<-cbind(dateRangeGraph,output)
colnames(output)<-c("Date", "Confirmed", "Deaths", "Recovered")

Confirmed_Cases<-output$Confirmed
Deaths<-output$Deaths
Date<-dateRangeGraph


#--------------Old plots
plot(Date, Confirmed_Cases, type="p", main="Local NNSY Area COVID-19 Cases")
#par(new=TRUE)
plot(Date, Deaths, type="p", main="Local NNSY Area COVID-19 Cases")

#--------------New plots
group<-c("Deaths","Confirmed")

outputPerDay<-data.frame(diff(as.matrix(output[[2]])))
outputPerDay2 <-data.frame(cbind(dateRangeGraph[1:(length(dateRangeGraph)-1)],outputPerDay))
colnames(outputPerDay2)<-c("Date", "Change")

n_forMean<-7
outputPerDay3<-na.omit(data.frame((frollmean(outputPerDay2, n=n_forMean))))
colnames(outputPerDay3)<-c("Date","Change")
outputPerDay3<-mutate(outputPerDay3, Date = as.Date(Date, origin = "1970-01-01"))

P0<-ggplot(outputPerDay3, aes(Date))+
  geom_col(aes(y=Change), colour="red") +
  ylab("Change")+
  ggtitle("Daily Change in Cases (7 Day Avg)")

P1<-ggplot(output, aes(x=Date, color="black"))+
  geom_line(aes(y=Confirmed), colour="green")+
  ylab("Confirmed Cases")+ 
  scale_fill_discrete(breaks=c("trt1"))+
  ggtitle("Hampton Roads Area Confirmed Cases")

P2<-ggplot(output, aes(x=Date, color="black"))+
  geom_line(aes(y=Confirmed), colour="green")+
  ylab("Confirmed Cases")+ 
  scale_fill_discrete(breaks=c("trt1"))+
  ggtitle("Hampton Roads Area Confirmed Cases (Log Scale)")+
  scale_y_continuous(trans='log10')


P3<-ggplot(output, aes(Date))+
  geom_line(aes(y=Deaths), colour="red") +
  ylab("Deaths")+
  ggtitle("Deaths")

#--------------Detail graphs
outputDetails<-list()
#write.csv(outputDetails,"outputDetails.csv")
for(i in 5:length(data)) { 
  nam <- paste0("localData",i)
  dat <- data[[i]]
  localDatFiltered<-dat %>%
    filter(Combined_Key %in% combinedKeyLocations)%>%
    select(Combined_Key, Confirmed, Deaths, Recovered, Active)
  
  assign(nam, localDatSummary)
  localDatFilteredConfirmed <- as.numeric(melt(localDatFiltered, id=1, measure = "Confirmed")[[3]])
  
  outputDetails[[i]]<-localDatFilteredConfirmed
}
str(outputDetails)
outputDetails<-data.frame(matrix(unlist(outputDetails),ncol=9, byrow=TRUE))
colnames(outputDetails)<-localCities


dateRangeGraph2<-sort(as.Date(seq(startDate+4,lastDay,"day")),descending=FALSE)
length(dateRangeGraph2)
outputDetails


outputDetailsForReport<-cbind(dateRangeGraph2,outputDetails)
write.csv(outputDetailsForReport,"outputDetailsForReport.csv")
outputDetails<-list()

for(i in 5:length(data)) { 
  nam <- paste0("localData",i)
  dat <- data[[i]]
  localDatFiltered<-dat %>%
    filter(Combined_Key %in% combinedKeyLocations)%>%
    select(Last_Update, Combined_Key, Confirmed)
  
  assign(nam, localDatSummary)
  #	localDatFilteredConfirmed <- as.numeric(melt(localDatFiltered, id=1, measure = "Confirmed")[[3]])
  localDatFilteredConfirmed <- localDatFiltered
  
  
  outputDetails<-rbind(outputDetails,localDatFilteredConfirmed)
}
outputDetails2<-outputDetails%>%
  mutate(Last_Update=as.Date(parse_date_time(outputDetails[[1]], c("mdy_HM","ymd_HMS"))))


outputDetails3<-melt(outputDetails2, id=1:2, measure="Confirmed")
P4<-ggplot(outputDetails3, aes(x=Last_Update, y=value))+
  geom_line(aes(color=Combined_Key, linetype=Combined_Key), lwd=1.5)+
  ggtitle("City Comparison")


pdf("COVID.pdf", width=8.5, height=10.5)
print(grid.arrange(P0,P1,P2, P4, nrow=4))
#grid.newpage()

#grid.table(outputDetailsForReport)
dev.off()

shell.exec("COVID.pdf")

#--------------Text mining
#https://data.library.virginia.edu/reading-pdf-files-into-r-for-text-mining/
#https://data.library.virginia.noclick_edu/reading-pdf-files-into-r-for-text-mining/
#https://connect.medrxiv.org/relate/feed/181
#https://connect.medrxiv.org/relate/content/181
#https://covid19.healthdata.org/projections




#https://openopps.usajobs.gov/tasks/1700?fromSearch
#https://www.usajobs.gov/coronavirus
#https://myteam.navair.navy.mil/corpapps/dar/rotations/pages/ViewRotation.aspx?FilterField1=Rotation_x0020_ID&FilterValue1=735




p_unload(all)
