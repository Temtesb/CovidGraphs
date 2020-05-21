#https://covid19.healthdata.org/projections
#https://stackoverflow.com/questions/39190511/assigning-value-to-the-list-element-in-r

pacman::p_load(pacman, dplyr, ggplot2, pdftools, data.table, lubridate,gridExtra, grDevices, grid)


baseUrl<-"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports"
startDate<-as.Date("2020-03-23") #using this date because there was no data before this point
lastDay<-Sys.Date()-1

dateRange<-format(seq(startDate,lastDay,"day"),"%m-%d-%Y")
dateRangeGraph<-sort(as.Date(seq(startDate,lastDay,"day")),descending=FALSE)

fileList<-paste0(baseUrl,"/",dateRange, ".csv")

#data<-lapply(fileList,read_csv) # faster option, but we can't use readr
data<-lapply(fileList,read.csv)
localCities<-c("Chesapeake","Hampton","James City","Newport News","Norfolk","Portsmouth","Suffolk","Virginia Beach","Williamsburg","Currituck")
localStates<-c("Virginia","Virginia","Virginia","Virginia","Virginia","Virginia","Virginia","Virginia","Virginia","North Carolina")
combinedKeyLocations<-paste(localCities,localStates, "US", sep=", ")

localPopulation<-c(235429,136454, 73147, 182385, 246393, 96201, 88161, 452745, 15052, 27072)

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

n_forMean<-5
outputPerDay3<-na.omit(data.frame((frollmean(outputPerDay2, n=n_forMean))))
colnames(outputPerDay3)<-c("Date","Change")
outputPerDay3<-mutate(outputPerDay3, Date = as.Date(Date, origin = "1970-01-01"))


P0<-ggplot(outputPerDay3, aes(Date))+
  geom_col(aes(y=Change), colour="red") +
  ylab("Change")+
  ggtitle(paste0("Daily Change in Cases (",n_forMean," Day Avg)"))

#using linear regression to determine slope of last 14 days
slope5_data<-tail(outputPerDay2,n=5)
slope5<-lm(slope5_data$Change ~ 0 + slope5_data$Date)
summary(lm(slope5_data$Change ~ slope5_data$Date))

lm(formula = slope5_data$Change ~ slope5_data$Date)

P0a<-ggplot(slope5_data, aes(Date))+
  geom_line(aes(y=Change)) +
  ylab("Change")+
  ggtitle("Daily Change in Cases (5 Days)")


P0b<-ggplot(slope5_data,aes(Date, Change)) +
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm', formula= y~x)+
  ggtitle("Least Squares Trend (5 Days)")


#using linear regression to determine slope of last 14 days
slope14_data<-tail(outputPerDay2,n=14)
slope14<-lm(slope14_data$Change ~ 0 + slope14_data$Date)
summary(lm(slope14_data$Change ~ slope14_data$Date))
lm(formula = slope14_data$Change ~ slope14_data$Date)

P0c<-ggplot(slope14_data, aes(Date))+
  geom_line(aes(y=Change)) +
  ylab("Change")+
  ggtitle("Daily Change in Cases (14 Days)")


P0d<-ggplot(slope14_data,aes(Date, Change)) +
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm', formula= y~x)+
  ggtitle("Least Squares Trend (14 Days)")

#using linear regression to determine slope of last 14 days
slope30_data<-tail(outputPerDay2,n=30)
slope30<-lm(slope30_data$Change ~ 0 + slope30_data$Date)
summary(lm(slope30_data$Change ~ slope30_data$Date))
lm(formula = slope30_data$Change ~ slope30_data$Date)

P0e<-ggplot(slope30_data, aes(Date))+
  geom_line(aes(y=Change)) +
  ylab("Change")+
  ggtitle("Daily Change in Cases (30 Days)")


P0f<-ggplot(slope30_data,aes(Date, Change)) +
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm', formula= y~x)+
  ggtitle("Least Squares Trend (30 Days)")




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
outputDetails<-data.frame(matrix(unlist(outputDetails),ncol=length(localCities), byrow=TRUE))
colnames(outputDetails)<-localCities


lastConfirmed<-tail(outputDetails, n=1)
perCapita<-lastConfirmed/localPopulation

overallPerCapitaValue<-tail(output$Confirmed, n=1)/sum(localPopulation)
overallPerCapita<-data.frame(c(overallPerCapitaValue))
colnames(overallPerCapita)<-c("Overall")
finalPerCapita<-cbind(overallPerCapita,perCapita)

perCapitaCities<-as.factor(colnames(finalPerCapita))
perCapitaVals<-as.vector(unlist(finalPerCapita, use.names = FALSE))


Pbp<-barplot(perCapitaVals,names.arg=perCapitaCities, col= rainbow(5), las=2, border = 0, cex.lab=1, cex.axis=1, font=1,col.axis="black",main="Per Capita Confirmed Cases")



perCapitaData<-data.frame(city=perCapitaCities, value=perCapitaVals)


Pbp<-ggplot(data=perCapitaData, aes(x=city, y=value)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=round(value,4)), vjust=-0.3, size=3.5)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))






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


print(grid.arrange(P0, grid.arrange(P0c,P0d, ncol=2),grid.arrange(P0a,P0b, ncol=2), P1,P2, P4, nrow=5))


pdf("COVID.pdf", width=8.5, height=10.5)
print(grid.arrange(Pbp,P0, P1, nrow=3))
print(grid.arrange(P2, P3, P4,  nrow=3))
#grid.newpage()
print(grid.arrange(P0a, P0b, P0c, P0d, P0e, P0f, ncol=2))

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

#https://www.census.gov/data/tables/time-series/demo/popest/2010s-total-cities-and-towns.html


p_unload(all)
