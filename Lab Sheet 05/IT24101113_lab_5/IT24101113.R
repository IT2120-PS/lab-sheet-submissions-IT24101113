setwd("C:\\Users\\it24101113\\Desktop\\IT24101113_lab_5")
getwd()
data<-read.table("Data.txt",header = TRUE, sep = ",")
fix(data)
attach(data)

names(data)<- c("X1","X2")
attach(data)
fix(data)
data$X2
hist(X2, main = "Histogram of X2")

summary(X2)
histogram <- hist(X2, main = "Histogram of X2", breaks = seq(130,270, length = 8))
breaks <- round(histogram$breaks)
freq <- histogram$counts
mids <- histogram$mids
classes <- c()
for (i in 1:length(breaks)-1) {
  classes[i] <- paste0("[",breaks[i],",",breaks[i+1],")")
}
cbind(Classes = classes,frequency= freq)
lines(mids, freq)
plot(mids, freq , type = "l", main = "Frequency polygon for shareholders", xlab= "shareholders", ylab = "frequency", ylim = c(0,max(freq)))
cum.freq<-cumsum(freq)
new<-c()
for(i in 1:length(breaks)){
  if(i==1){
    new[i]=0
  }else{
    new[i]= cum.freq[i-1]
  }
}
plot(breaks, new, type = 'l',main = "Cumalative Frequency polygon for Shareholders",xlab = "Shareholders", ylab = "cumulative Frequency", ylim=c(0,max(cum.freq)))
cbind(Upper = breaks, CumFreq = new)




#Exercise

Delivery_Items <- read.table("Exercise - Lab 05.txt",header= TRUE, sep = ",")
fix(Delivery_Items)
names(Delivery_Items)<- c("X1")
hist(Delivery_Items$X1, main="Histogram for Delivery Times")
#the histogram shows a unimodel distribution with peak around 35-40 munites. te shape is roughly symmetric,though slightly spreed out to the right. most delivery times fall between 30 and 50 munites
freq <- hist(Delivery_Items$X1,breaks = seq(20,70,by = 5),right = TRUE,
             plot = FALSE)
cum.freq<-cumsum(freq$counts)
plot((mids, freq, type = 'l',main = "Cumalative Frequency polygon for 0give",xlab = "Delivery Times(in minutes", ylab = "cumulative Frequency", ylim=c(0,max(cum.freq)))
class <-c()
for(i in 1:length(breaks)-1){
  class[i]<paste0("{",breaks[i],",",breaks[i+1],"}")
}     
class cbind(classes = Class,Frequency = freq)
lines(mids,freq)

plot((mids,freq,type="0",main= "Cumalative Frequency polygon for 0give",xlab = "Delivery Times(in minutes", ylab = "cumulative Frequency", ylim=c(0,max(cum.freq)))
     cum.freq<-cumsum(freq)
cum.freq     

