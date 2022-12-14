library(plotrix)
library(readxl)

# excel reading 

temp_main <- read_excel("", range = "C1:I1039", na = "0")

# attach
attach(temp_main)

# converting to dataframe
df1<- data.frame(date_time_stamps,temp_rec,rec_type,active_power,reactive_power,voltage,intensity)
df1


temp_rec_types <- as.factor(df1$rec_type)
temp_rec_types



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


hist(active_power,ylim=c(0,350),xlim=c(0,8),xlab="Global Active Power(kilo watts)",col=2,freq=T,las=1)
hist(reactive_power,ylim=c(0,10),xlim=c(0,0.6),xlab="Global Reactive Power",col=2,freq=F,las=1)
hist(intensity,ylim=c(0,350),xlim=c(0,40),xlab="Global Active Intensity",col=2,freq=T,las=1)
hist(voltage,ylim=c(0,240),xlim=c(230,250),xlab="Voltage",col=2,freq=T,las=1)

# single day c1,i276, skip 143

temp1 <- read_excel("C:/Users/prati/Desktop/R mini proj/Temperature Analytics030689 tmpr001.xlsx", range = "C1:I1039", na = "0")

# attach(temp1)
single_day <- data.frame(temp1[c(144:275),])
single_day

#par(mfrow=c(1,1))
# Straight
plot(single_day$date_time_stamps[rec_type=="Out"],single_day$temp_rec[rec_type=="Out"],cex=1.8, main="Time vs Temp",xlab="Time",ylab="Temperature in Celcius",las=1,ylim=c(32,46),cex.main=1.4,cex.lab=1.2,pch=1,col.lab=4,col.main=4,col=2,)
points(single_day$date_time_stamps[rec_type=="In"],single_day$temp_rec[rec_type=="In"],cex=1.3,col=3,pch=16)
legend("topleft",legend=c("Inside Temp","Outside Temp"),cex=1,col=c(3,2),pch=c(16,1))

# single split plotting

#par(mfrow=c(1,2))
plot(single_day$date_time_stamps[rec_type=="Out"],single_day$temp_rec[rec_type=="Out"],cex=1.3, main="Out",xlab="Outside Temperature",ylab="Time",las=1,cex.main=1.2,cex.lab=1.1,pch=16,col.lab=4,col.main=4,col=2,)
plot(single_day$date_time_stamps[rec_type=="In"],single_day$temp_rec[rec_type=="In"],cex=1.3, main="In",xlab="Inside Temperature ",ylab="Time",las=1,cex.main=1.2,cex.lab=1.1,pch=16,col.lab=4,col.main=4,col=3,)

# pie chart for categorical variavllebs single day

types<- single_day$rec_type
count <- table(types) 

label1 <- c(paste("In",count[1],sep=" "),paste("Out",count[2],sep=" "))
pie3D(count,labels=label1,explode=0.1,main="In vs Out Observations",col=c(3,2))
legend("top",c("Inside Temp","Outside Temp"),cex=0.7,fill=c(3,2))

hist(single_day$active_power,ylim=c(0,120),xlim=c(1,6),xlab="Active Power(kilo watts)",col=3,main="Single day active power",freq=T,las=1)
hist(single_day$reactive_power,ylim=c(0,30),xlim=c(0.02,0.21),xlab="Reactive Power",col=3,main="Single day reactive power",freq=F,las=1)
hist(single_day$intensity,ylim=c(0,60),xlim=c(5,16),xlab=" Active Intensity",col=3,main="Single day intensity",freq=T,las=1)
hist(single_day$voltage,ylim=c(0,40),xlim=c(230,240),xlab="Voltage",col=3,freq=T,main="Single day voltage",las=1)





