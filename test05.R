
# date time packages
# library(lubridate)
library(plotrix)
library(anytime)
library(readxl)


# excel reading 

temp_main <- read_excel("C:/Users/prati/Desktop/R mini proj/Temperature Analytics030689 tmpr001.xlsx", range = "C1:I1039", na = "0")
# View(temp_main)

# check
#class(temp_main$rec_type)
#levels(temp_main$rec_type)

# attach
attach(temp_main)

# converting to dataframe
df1<- data.frame(date_time_stamps,temp_rec,rec_type,active_power,reactive_power,voltage,intensity)
#df1

#class(df1$rec_type)
#levels(df1$rec_type)

temp_rec_types <- as.factor(df1$rec_type)
#temp_rec_types
levels(temp_rec_types)

# check
# class(temp_rec_types)
# levels(temp_rec_types)


# date and time separation
# dt <- df1$date_time_stamps
# class(dt)
# dt

#d4 <-as.POSIXct(d1,tz=Sys.timezone()) 
#d4

# class(d4)
par(mfrow=c(1,1))
names(temp_main)



# basic plot
# plot(temp_rec,date_time_stamps, main="All Records",xlab="Temperature in Celcius",ylab="Date Time/ Days",col=4,las=1,xlim=c(25,50),cex=0.8,pch=2)
plot(date_time_stamps,temp_rec, main="All Records",xlab="Date Time/ Days",ylab="Temperature",col=4,las=1,cex=0.8,pch=2)



# all entries pie chart

#types_of_rec<-temp_main$rec_type
types<- rec_type
count_all <- table(types) 

label00 <- c(paste("In",count_all[1],sep=" "),paste("Out",count_all[2],sep=" "))
pie3D(count_all,labels=label00,explode=0.1,main="In vs Out Observations",col=c(3,2),theta=pi/4,start=2)
legend("top",c("Inside Temp","Outside Temp"),cex=0.7,fill=c(3,2))
# ?pie3D
# all power histograms
par(mfrow=c(2,2))

hist(active_power,ylim=c(0,350),xlim=c(0,8),xlab="Global Active Power(kilo watts)",col=2,freq=T,las=1)
hist(reactive_power,ylim=c(0,10),xlim=c(0,0.6),xlab="Global Reactive Power",col=2,freq=F,las=1)
hist(intensity,ylim=c(0,350),xlim=c(0,40),xlab="Global Active Intensity",col=2,freq=T,las=1)
hist(voltage,ylim=c(0,240),xlim=c(230,250),xlab="Voltage",col=2,freq=T,las=1)




# single day c1,i276, skip 143

temp1 <- read_excel("C:/Users/prati/Desktop/R mini proj/Temperature Analytics030689 tmpr001.xlsx", range = "C1:I1039", na = "0")


# attach(temp1)
single_day <- data.frame(temp1[c(144:275),])
#single_day
par(mfrow=c(1,1))
# flipped 
#plot(single_day$temp_rec[rec_type=="Out"],single_day$date_time_stamps[rec_type=="Out"],cex=2, main="Scatter Plot",xlab="Temperature in Celcius",ylab="Time",las=1,xlim=c(31,43),cex.main=1.6,cex.lab=1.4,pch=1,col.lab=4,col.main=4,col=2,)
#points(single_day$temp_rec[rec_type=="In"],single_day$date_time_stamps[rec_type=="In"],cex=1.3,col=3,pch=16)
# plot(single_day$temp_rec[rec_type=="Out"],single_day$date_time_stamps[rec_type=="Out"])


par(mfrow=c(1,1))
# Straight
plot(single_day$date_time_stamps[rec_type=="Out"],single_day$temp_rec[rec_type=="Out"],cex=1.8, main="Time vs Temp",xlab="Time",ylab="Temperature in Celcius",las=1,ylim=c(32,46),cex.main=1.4,cex.lab=1.2,pch=1,col.lab=4,col.main=4,col=2,)
points(single_day$date_time_stamps[rec_type=="In"],single_day$temp_rec[rec_type=="In"],cex=1.3,col=3,pch=16)
legend("topleft",legend=c("Inside Temp","Outside Temp"),cex=1,col=c(3,2),pch=c(16,1))


# single split plotting

par(mfrow=c(1,2))

plot(single_day$date_time_stamps[rec_type=="Out"],single_day$temp_rec[rec_type=="Out"],cex=1.3, main="Out",xlab="Outside Temperature",ylab="Time",las=1,cex.main=1.2,cex.lab=1.1,pch=16,col.lab=4,col.main=4,col=2,)

plot(single_day$date_time_stamps[rec_type=="In"],single_day$temp_rec[rec_type=="In"],cex=1.3, main="In",xlab="Inside Temperature ",ylab="Time",las=1,cex.main=1.2,cex.lab=1.1,pch=16,col.lab=4,col.main=4,col=3,)



# pie chart for categorical variavllebs single day
par(mfrow=c(1,1))
types<- single_day$rec_type
count <- table(types) 

label1 <- c(paste("In",count[1],sep=" "),paste("Out",count[2],sep=" "))
pie3D(count,labels=label1,explode=0.1,main="In vs Out Observations",col=c(3,2))
legend("top",c("Inside Temp","Outside Temp"),cex=0.7,fill=c(3,2))

# power consumption graphs

# hist(single_day$active_power[rec_type=="Out"],ylim=c(0,80),xlim=c(1.5,4.9))
# hist(single_day$active_power[rec_type=="In"],ylim=c(0,35),xlim=c(1.5,4.6))
# hist(single_day$reactive_power)
# hist(single_day$voltage)
# hist(single_day$intensity)

par(mfrow=c(1,1))
hist(single_day$active_power,ylim=c(0,120),xlim=c(1,6),xlab="Active Power(kilo watts)",col=3,main="Single day active power",freq=T,las=1)
hist(single_day$reactive_power,ylim=c(0,30),xlim=c(0.02,0.21),xlab="Reactive Power",col=3,main="Single day reactive power",freq=F,las=1)
hist(single_day$intensity,ylim=c(0,60),xlim=c(5,16),xlab=" Active Intensity",col=3,main="Single day intensity",freq=T,las=1)
hist(single_day$voltage,ylim=c(0,40),xlim=c(230,240),xlab="Voltage",col=3,freq=T,main="Single day voltage",las=1)






# next day
par(mfrow=c(1,1))
day3 <- data.frame(temp1[c(277:867),])
day3

plot(day3$date_time_stamps[rec_type=="Out"],day3$temp_rec[rec_type=="Out"],cex=2, main="Scatter Plot",xlab="date",ylab="Temp",las=1,,cex.main=1.6,cex.lab=1.4,pch=17,col.lab=4,col.main=4,col=2,)
points(day3$date_time_stamps[rec_type=="In"],day3$temp_rec[rec_type=="In"],cex=2,col=3,pch=16)





