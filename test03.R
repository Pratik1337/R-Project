library(readxl) 

tempr1 <- read_excel("C:/Users/prati/Desktop/R mini proj/Temperature Analytics020689.xlsx", range = "C1:I1039", na = "o") 
View(tempr1) 
names(tempr1) 
attach(tempr1)