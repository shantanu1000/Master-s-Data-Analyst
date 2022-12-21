library(readxl)
Data <- read.csv("D:/Shantanu/Simplilearn/R/project for submission/1567503160_comcasttelecomcomplaintsdata/Comcast Telecom Complaints data.csv", stringsAsFactors = T)
View(Data)

head(Data,5)
str(Data)


library(lubridate)
li<-parse_date_time(x = Data$Date,
                    orders = c("d m y", "d B Y", "m/d/y"),
                    locale = Sys.getlocale("LC_TIME"))
data2<-Data
data2$Date <- li

data2$Month <- format(as.Date(data2$Date), "%m")
data2$Month<- month.abb[as.integer(data2$Month)]
head(data2)

library(dplyr)
data_date<-data2 %>% group_by(Date) %>% dplyr::summarise(frequency = n())
df <-data_date[order(-data_date$frequency),]
dff<-head(df)
dff

library(ggplot2)
ggplot(data_date, aes(Date, frequency, group = 1)) + 
  geom_point() + 
  geom_line() +
  xlab("Date") + 
  ylab("Number of Complaints")

ggplot(dff, aes(Date, frequency, group = 1)) + 
  geom_point() + 
  geom_line() +
  xlab("Date") + 
  ylab("Number of Complaints")


data_month<-data2 %>% 
  group_by(Month) %>% dplyr :: summarise(frequency = n())
data_month


data2$Month <- as.factor(data2$Month)
levels(data2$Month)

ggplot(data_month, aes(Month, frequency, group = 1)) + 
  geom_point() + 
  geom_line() +
  xlab("Month") + 
  ylab("Number of Complaints")

data3<-data2%>% mutate(Customer.Complaint = tolower(Customer.Complaint))
CustTable <- table(data3$Customer.Complaint)
CustTable <- data.frame(CustTable)
filtered<-CustTable %>% 
  rename(
    CustomerComplaintType = Var1,
    Frequency = Freq
  )
final <- filtered %>% arrange(desc(Frequency))

final_most<-head(final,20)
final_most

ggplot(head(final_most,6), aes(CustomerComplaintType, Frequency)) +
  geom_bar(stat = "identity")
library(stringr)
library(tidyverse)
levels(Data$Status)

library(plyr)
Data$Status_New<-revalue(Data$Status, c(Pending = "Open", Solved = "Closed"))
head(Data)

levels(Data$State)

tab <- table(Data$State,Data$Status_New)
tab <- cbind(tab, Total = rowSums(tab))
head(tab,15)


library(gridExtra)
ggplot(Data, aes(y = State)) + geom_bar(aes(fill = Status_New))


levels(Data$Received.Via)
ggplot(Data, aes(y = Received.Via )) + geom_bar(aes(fill = Status_New))


df1 <- table(Data$Received.Via, Data$Status_New)
df1 <- cbind(df1, Total = rowSums(df1))
df1

slices <- c(864, 255)
lbls <- c("Closed", "Open")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Received Via Call")


slices <- c(843, 262)
lbls <- c("Closed", "Open")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Received Via Internet")

### The highest number of complaints are recorded during the month of July. The company can employ more
#staff during this month to tackle the high number of complaints.
###Most of the complains are mainly because of services of Comcast like internet speed, data caps and 
#Billing methods. The company should focus more on improving these services.
###States like Georgia and Florida have the maximum complaints. States like California, Colorado and Illinois
#have most percentage of open complaints. The company needs to improve their services in these states.


for (year in 1:5)
  
{Yr=print(year)}


