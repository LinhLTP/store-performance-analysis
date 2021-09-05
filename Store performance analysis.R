setwd("/Users/admin/Documents/Linh-R Studio/SaleData")

library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(anytime)

Saledata <- read.csv("sales_report_interview_data.csv")
table(Saledata$Store.ID) # 30 store 

#---- Change factor to Date
Saledata$Date <- as.POSIXct(Saledata$Date, format = '%B %d, %Y') 
Saledata$Day <- weekdays(as.Date(Saledata$Date))
#---- Adding Day of Week and arrage from Mon to Sun
Saledata$Day <- factor(Saledata$Day, levels= c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
Saledata <- Saledata[order(Saledata$Day), ]
#--- Check min date - max date of each unique value 

library(dplyr)
df1 <- Saledata %>%
  group_by(Store.ID) %>%
  mutate(minDia=min(Date, na.rm=T), maxDia=max(Date, na.rm=T))

df2 <- df1 %>%
  group_by(Store.ID) %>%
  mutate(minDia=min(Date, na.rm=T), maxDia=max(Date, na.rm=T)) %>%
  ungroup() %>%
  distinct(Store.ID, minDia, maxDia)

table(Saledata$Period)
Saledata$Store.ID <- as.character(Saledata$Store.ID) #Gia dinh day la giai doan co Chuong trinh Marketing 

# BOD OVERVIEW ---------------------------------
# Comment: Phan tich tong quan tinh hinh nen khong loai di bat cu CH nao, muc dich la de thay trend. 

# 1. Luong traffic qua cac ngay: Peak time/ off peak time la may gio----
# Peak time in Date
WHEN_PeakTime <- Saledata %>% group_by(Store.Type,Hour) %>% summarise(sumTraffic = sum(Traffic), sumInteraction = sum(Interaction), sumTransaction = sum(Transaction))
# Peak time in Week-Date
Day_WHEN_PeakTime <- Saledata %>% group_by(Store.Type,Day) %>% summarise(sumTraffic = sum(Traffic), sumInteraction = sum(Interaction), sumTransaction = sum(Transaction))

# 2. Basket size---- 
# Histogram 
cor(Saledata$Transaction, Saledata$Avg.Basket.Size) # No correlation 
Saledata1 <- Saledata %>% filter(Store.Type == "SHOPPING_MALL")
hist(Saledata1$Avg.Basket.Size)
Saledata2 <- Saledata %>% filter(Store.Type == "INDEPENDENT")
hist(Saledata2$Avg.Basket.Size)

par(
  mfrow=c(1,2),
  mar=c(4,4,1,0)
)
hist(Saledata1$Avg.Basket.Size, breaks=30 , xlim=c(0,30) , col=rgb(1,0,0,0.5) , xlab="Avg.BasketSize" , ylab="Frequency" , main="" )
hist(Saledata2$Avg.Basket.Size, breaks=30 , xlim=c(0,30) , col=rgb(0,0,1,0.5) , xlab="Avg.BasketSize" , ylab="Frequency" , main="")

quantile(Saledata1$Avg.Basket.Size, c(0.0, 0.25, 0.50, 0.75, 1.0))
quantile(Saledata2$Avg.Basket.Size, c(0.0, 0.25, 0.50, 0.75, 1.0))


# 3. Shopping Mall----
# Peak time - Week day 
SHOPPING_MALL <- Day_WHEN_PeakTime %>% filter(Store.Type == "SHOPPING_MALL")
SHOPPING_MALL_long <- gather(SHOPPING_MALL, Condition, Measurement, sumTraffic:sumTransaction, factor_key=TRUE)

SHOPPING_MALL_long <- SHOPPING_MALL_long %>% filter(Condition == "sumTraffic")

ggplot(SHOPPING_MALL_long, aes(fill=Condition, y=Measurement, x=Day)) + geom_bar(position="stack", stat="identity") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(values = c("#0073C2FF"))+
  scale_fill_manual(values = c("#0073C2FF")) + geom_text(
  aes(label = Measurement), 
  position = position_dodge(0.8),
  vjust = -0.3, size = 5.0
)   #stack bar

ggplot(data=SHOPPING_MALL_long, aes(x=Day, y=Measurement, group=1)) +
  geom_line()+
  geom_point()+ theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(values = c("#0073C2FF"))+
  scale_fill_manual(values = c("#0073C2FF")) + geom_text(
  aes(label = Measurement), 
  position = position_dodge(0.8),
  vjust = -0.3, size = 4.0
) # line cHART


# Peak time - Hour in a Date  
SHOPPING_MALL <- WHEN_PeakTime %>% filter(Store.Type == "SHOPPING_MALL")
SHOPPING_MALL_long <- gather(SHOPPING_MALL, Condition, Measurement, sumTraffic:sumTransaction, factor_key=TRUE)

ggplot(data = SHOPPING_MALL, aes(x = Hour, y = sumTraffic))+
  geom_line(color = "#00AFBB", size = 0.7) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + geom_area(fill = "lightpink") 


ggplot(SHOPPING_MALL_long, aes(x = Hour, y = Measurement)) + 
  geom_line(aes(color = Condition), size = 1.0) +
  scale_color_manual(values = c("#EFC000FF", "#6A00A8FF", "#B12A90FF")) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + geom_text(aes(label = Measurement), position = position_dodge(0.8), vjust = -0.3, size = 2.0)


# Conversion rate accorrding to time
SHOPPING_MALL <- SHOPPING_MALL %>% mutate(noInteraction_Rate = paste0(round(((sumTraffic - sumInteraction)/sumTraffic) * 100,0), "%"))

SHOPPING_MALL_Performance <- SHOPPING_MALL %>%
  mutate(Traffic_conRate = round((sumTransaction / sumTraffic) * 100, 2)) %>% 
  mutate(Interaction_conRate = round((sumTransaction / sumInteraction) * 100, 2))
SHOPPING_MALL_Performance 

# Funnel Traffic
SHOPPING_MALL_F <- SHOPPING_MALL %>% group_by(Store.Type) %>% summarise(Traffic = sum(sumTraffic), Interaction = sum(sumInteraction), Convert = sum(sumTransaction))
SHOPPING_MALL_F$Store.Type <- NULL 
SHOPPING_MALL_F <- gather(SHOPPING_MALL_F, Condition, Measurement, Traffic:Convert, factor_key=TRUE) # convert wide to long
library(plotly)
fig <- plot_ly() 
fig <- fig %>%
  add_trace(type = "funnel",
            y = SHOPPING_MALL_F$Condition,
            x = SHOPPING_MALL_F$Measurement,
            textposition = "inside",
            textinfo = "value+percent initial",
            opacity = 0.65,
            marker = list(color = c("deepskyblue", "lightsalmon", "tan", "teal", "silver"),
                          line = list(width = c(4, 2, 2, 3, 1, 1), color = c("wheat", "wheat", "blue", "wheat", "wheat"))),
            connector = list(line = list(color = "royalblue", dash = "dot", width = 3)))
fig <- fig %>%
  layout(yaxis = list(categoryarray = SHOPPING_MALL_F$Condition))
fig


# 4. Indipendent Store-----
# Peak time - when - Day 
INDEPENDENT <- Day_WHEN_PeakTime %>% filter(Store.Type == "INDEPENDENT")
INDEPENDENT_long <- gather(INDEPENDENT, Condition, Measurement, sumTraffic:sumTransaction, factor_key=TRUE)

INDEPENDENT_long <- INDEPENDENT_long %>% filter(Condition == "sumTraffic")

ggplot(INDEPENDENT_long, aes(fill=Condition, y=Measurement, x=Day)) + 
    geom_bar(position="stack", stat="identity") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(values = c("#EFC000FF"))+
  scale_fill_manual(values = c("#EFC000FF")) + geom_text(
  aes(label = Measurement), 
  position = position_dodge(0.8),
  vjust = -0.3, size = 3.5
) #stack bar


ggplot(data=INDEPENDENT_long, aes(x=Day, y=Measurement, group=1)) +
  geom_line()+
  geom_point()+ theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(values = c("#0073C2FF"))+
  scale_fill_manual(values = c("#0073C2FF")) + geom_text(
  aes(label = Measurement), 
  position = position_dodge(0.8),
  vjust = -0.3, size = 4.0
) # line cHART

# Peak time - when - Hour
INDEPENDENT <- WHEN_PeakTime %>% filter(Store.Type == "INDEPENDENT")
INDEPENDENT_long <- gather(INDEPENDENT, Condition, Measurement, sumTraffic:sumTransaction, factor_key=TRUE)

ggplot(data = INDEPENDENT, aes(x = Hour, y = sumTraffic))+
  geom_line(color = "#00AFBB", size = 0.3) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + geom_area(fill = "lightblue")

ggplot(INDEPENDENT_long, aes(x = Hour, y = Measurement)) + 
  geom_line(aes(color = Condition), size = 1.0) +
  scale_color_manual(values = c("#b9005f", "#5fb900","#005fb9")) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 


# Conversion rate accorrding to time
INDEPENDENT <- INDEPENDENT %>% mutate(noInteraction_Rate = paste0(round(((sumTraffic - sumInteraction)/sumTraffic) * 100,0), "%"))
INDEPENDENT_Performance <- INDEPENDENT %>%
  mutate(Traffic_conRate = round((sumTransaction / sumTraffic) * 100, 2)) %>% 
  mutate(Interaction_conRate = round((sumTransaction / sumInteraction) * 100, 2))
INDEPENDENT_Performance

# Funnel Traffic
INDEPENDENT_F <- INDEPENDENT %>% group_by(Store.Type) %>% summarise(Traffic = sum(sumTraffic), Interaction = sum(sumInteraction), Convert = sum(sumTransaction))
INDEPENDENT_F$Store.Type <- NULL 
INDEPENDENT_F <- gather(INDEPENDENT_F, Condition, Measurement, Traffic:Convert, factor_key=TRUE) # convert wide to long
library(plotly)
fig <- plot_ly() 
fig <- fig %>%
  add_trace(type = "funnel",
            y = INDEPENDENT_F$Condition,
            x = INDEPENDENT_F$Measurement,
            textposition = "inside",
            textinfo = "value+percent initial",
            opacity = 0.65,
            marker = list(color = c("deepskyblue", "lightsalmon", "tan", "teal", "silver"),
                          line = list(width = c(4, 2, 2, 3, 1, 1), color = c("wheat", "wheat", "blue", "wheat", "wheat"))),
            connector = list(line = list(color = "royalblue", dash = "dot", width = 3))) 
fig <- fig %>%
  layout(yaxis = list(categoryarray = INDEPENDENT_F$Condition))
fig

# 5. Comparision----
# 5.1 Funnel----
library(plotly)
fig <- plot_ly(
    type = "funnel",
    name = 'Shopping_Mall',
    y = SHOPPING_MALL_F$Condition,
    x = SHOPPING_MALL_F$Measurement,
    textinfo = "value+percent initial") 
fig <- fig %>%
  add_trace(
    type = "funnel",
    name = 'Independent',
    orientation = "h",
    y = INDEPENDENT_F$Condition,
    x = INDEPENDENT_F$Measurement,
    textposition = "inside",
    textinfo = "value+percent previous") 
fig <- fig %>%
  layout(yaxis = list(categoryarray = c("Traffic", "Interaction", "Convert")))
fig
# 5.2 Interaction----
# Interaction Rate by Day in Week 
Saledata_1 <- Saledata %>% group_by(Store.Type,Day) %>% summarise(sumTraffic = sum(Traffic), sumInteraction = sum(Interaction), sumTransaction = sum(Transaction))

Saledata_1 <- Saledata_1 %>% mutate(Interaction_conRate = paste(round((sumInteraction / sumTraffic) * 100, 1),'%'))

Saledata_1$sumTraffic <- NULL
Saledata_1$sumInteraction <- NULL
Saledata_1$sumTransaction <- NULL

ggplot(Saledata_1, aes(x=Day, y=Interaction_conRate, group=Store.Type)) +
  geom_line(aes(linetype=Store.Type, color=Store.Type))+
  geom_point(aes(color=Store.Type))+
  theme(legend.position="top") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + geom_text(
  aes(label = Interaction_conRate), 
  position = position_dodge(0.8),
  vjust = -0.3, size = 3.5
) #stack bar

# Interaction Rate by Hour in Day 
Saledata_1 <- Saledata %>% group_by(Store.Type,Hour) %>% summarise(sumTraffic = sum(Traffic), sumInteraction = sum(Interaction), sumTransaction = sum(Transaction))

Saledata_1 <- Saledata_1 %>% mutate(Interaction_conRate = paste(round((sumInteraction / sumTraffic) * 100, 1),'%'))

Saledata_1$sumTraffic <- NULL
Saledata_1$sumInteraction <- NULL
Saledata_1$sumTransaction <- NULL

ggplot(Saledata_1, aes(x=Hour, y=Interaction_conRate, group=Store.Type)) +
  geom_line(aes(linetype=Store.Type, color=Store.Type))+
  geom_point(aes(color=Store.Type))+
  theme(legend.position="top") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + geom_text(
  aes(label = Interaction_conRate), 
  position = position_dodge(0.8),
  vjust = -0.3, size = 3.5)

# 5.3 Transaction----
# Transaction Rate by Day in Week 
Saledata_1 <- Saledata %>% group_by(Store.Type,Day) %>% summarise(sumTraffic = sum(Traffic), sumInteraction = sum(Interaction), sumTransaction = sum(Transaction))

Saledata_1 <- Saledata_1 %>% mutate(Transaction_conRate = paste(round((sumTransaction / sumInteraction) * 100, 1),'%'))

Saledata_1$sumTraffic <- NULL
Saledata_1$sumInteraction <- NULL
Saledata_1$sumTransaction <- NULL

ggplot(Saledata_1, aes(x=Day, y=Transaction_conRate, group=Store.Type)) +
  geom_line(aes(linetype=Store.Type, color=Store.Type))+
  geom_point(aes(color=Store.Type))+
  theme(legend.position="top") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + geom_text(
  aes(label = Transaction_conRate), 
  position = position_dodge(0.8),
  vjust = -0.3, size = 3.5
) #stack bar

# Transaction Rate by Hour in Day 
Saledata_1 <- Saledata %>% group_by(Store.Type,Hour) %>% summarise(sumTraffic = sum(Traffic), sumInteraction = sum(Interaction), sumTransaction = sum(Transaction))

Saledata_1 <- Saledata_1 %>% mutate(Transaction_conRate = paste(round((sumTransaction / sumInteraction) * 100, 2),'%'))

Saledata_1$sumTraffic <- NULL
Saledata_1$sumInteraction <- NULL
Saledata_1$sumTransaction <- NULL

ggplot(Saledata_1, aes(x=Hour, y=Transaction_conRate, group=Store.Type)) +
  geom_line(aes(linetype=Store.Type, color=Store.Type))+
  geom_point(aes(color=Store.Type))+
  theme(legend.position="top") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + geom_text(
  aes(label = Transaction_conRate), 
  position = position_dodge(0.8),
  vjust = -0.3, size = 3.5) #stack bar

# 5.4 Performance over period----
# Store.Type
Saledata_2 <- Saledata %>% group_by(Store.Type,Period) %>% summarise(sumTraffic = sum(Traffic), sumInteraction = sum(Interaction), sumTransaction = sum(Transaction))
# Saledata_2 <- Saledata_2 %>% mutate(Interaction_conRate = paste(round((sumInteraction / sumTraffic) * 100, 1),'%'))
# Change long to wide                                    
library(reshape2)
meltSaledata2 <- melt(Saledata_2, id.vars=1:2)
u <- dcast(meltSaledata2, Store.Type ~ Period + variable, fun.aggregate = sum)
library(writexl)
write_xlsx(x = u, path = "u.xlsx",col_names = TRUE)

#Store.ID
Saledata_3 <- Saledata %>% filter(Store.Type == "INDEPENDENT") %>% group_by(Store.ID,Period) %>% summarise(sumTraffic = sum(Traffic), sumInteraction = sum(Interaction), sumTransaction = sum(Transaction))
# Change long to wide                                    
meltSaledata3 <- melt(Saledata_3, id.vars=1:2)
z <- dcast(meltSaledata3, Store.ID ~ Period + variable, fun.aggregate = sum)
library(writexl)
write_xlsx(x = z, path = "z2.xlsx",col_names = TRUE)




# SHOPPING MALL - OPERATION ---- 
## 1. Data preperation----
Shopping_mall <- Saledata %>% filter(Store.Type == "SHOPPING_MALL")
# table(Shopping_mall$Store.ID)# So lieu lay khong dong nhat giua cac CH
# table(Shopping_mall$Date)
# #Loai 2 CH 228 & 220 vi data chi co dung 1 tuan (23June - 28June)
# library(dplyr)
# Shopping_mall %>%
#   group_by(Store.ID) %>%
#   summarise(count = n_distinct(Store.ID))
# Shopping_mall_test <- Shopping_mall %>% filter(Store.ID == "108") # 96
# min(Shopping_mall_test$Date)
# max(Shopping_mall_test$Date)
# Shopping_mall <- Saledata %>% filter(Store.ID != "220" & Store.ID != "228" & Store.ID !="96")
# Chi lay tu ngay "2020-05-20 ICT" vi co 2 CH start from "2020-05-20"
# Shopping_mall <- Shopping_mall %>% filter(Date >= "2020-05-20 ICT")

library(dplyr)
df1 <- Shopping_mall %>%
  group_by(Store.ID) %>%
  mutate(minDia=min(Date, na.rm=T), maxDia=max(Date, na.rm=T))
df2 <- df1 %>%
  group_by(Store.ID) %>%
  mutate(minDia=min(Date, na.rm=T), maxDia=max(Date, na.rm=T)) %>%
  ungroup() %>%
  distinct(Store.ID, minDia, maxDia)

####NOTE: CH 220, 228, 97 Du lieu thieu 20/5 & 23/6. Nen tam coi day la CH hoat dong thieu hieu qua (less traffic, less interaction, less transaction)


# 2. Traffic by store - CH nao co luong traffic tot theo ngay----
# 2.1 Traffic======
# Treemap
Traffic <- Shopping_mall %>% group_by(Store.ID) %>% summarise(TotalTraffic = sum(Traffic))
library(treemap)
    treemap(Traffic,
            index=c("Store.ID"), #single index
            vSize="TotalTraffic",
            vColor="TotalTraffic",
            palette="Purples",
            type="value") 

# Line chart by date
Traffic <- Shopping_mall %>% group_by(Store.ID, Date) %>% summarise(TotalTraffic = sum(Traffic))

ggplot(data=Traffic[which(Traffic$Date <= "2020-05-17"),], aes(x = Date, y = TotalTraffic)) + geom_line(aes(color = as.factor(Store.ID)), size = 0.7) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 

ggplot(data=Traffic[which(Traffic$Date <= "2020-05-24" & Traffic$Date >= "2020-05-20" ),], aes(x = Date, y = TotalTraffic)) + geom_line(aes(color = as.factor(Store.ID)), size = 0.7) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggplot(data=Traffic[which(Traffic$Date <= "2020-06-14" & Traffic$Date >= "2020-06-05" ),], aes(x = Date, y = TotalTraffic)) + geom_line(aes(color = as.factor(Store.ID)), size = 0.7) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  

ggplot(data=Traffic[which(Traffic$Date <= "2020-06-28" & Traffic$Date >= "2020-06-19" ),], aes(x = Date, y = TotalTraffic)) + geom_line(aes(color = as.factor(Store.ID)), size = 0.7) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  


# 2.2 Ranking---- 
# "2020-05-24"
table(Saledata$Period)
my_theme <- function() {
  # Colors
  color.background = "white"
  color.text = "#22211d"
  # Begin construction of chart
  theme_bw(base_size=15) +
    # Format background colors
    theme(panel.background = element_rect(fill=color.background, color=color.background)) +
    theme(plot.background  = element_rect(fill=color.background, color=color.background)) +
    theme(panel.border     = element_rect(color=color.background)) +
    theme(strip.background = element_rect(fill=color.background, color=color.background)) +
    # Format the grid
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(axis.ticks       = element_blank()) +
    # Format the legend
    theme(legend.position = "none") +
    # Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=20, face = "bold")) +
    theme(axis.title.x     = element_text(size=14, color="black", face = "bold")) +
    theme(axis.title.y     = element_text(size=14, color="black", face = "bold", vjust=1.25)) +
    theme(axis.text.x      = element_text(size=10, vjust=0.5, hjust=0.5, color = color.text)) +
    theme(axis.text.y      = element_text(size=10, color = color.text)) +
    theme(strip.text       = element_text(face = "bold")) +
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

Traffic1 <- subset(Traffic, Date <= "2020-05-17")

Transformed <- Traffic1 %>% group_by(Store.ID) %>% transform(., day=match(Date, unique(Date)))

df.rankings <- Transformed %>%
  group_by(Date) %>%
  arrange(Date, desc(TotalTraffic), Store.ID) %>%
  mutate(ranking = row_number()) %>% as.data.frame()

df.rankings$Store.ID <- as.character(df.rankings$Store.ID)

ggplot(data = df.rankings, aes(x = day, y = ranking, group = Store.ID)) +
  geom_line(aes(color = Store.ID, alpha = 1), size = 1) +
  geom_point(aes(color = Store.ID, alpha = 1), size = 3) +
  scale_y_reverse(breaks = 1:nrow(df.rankings)) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) #stack bar

# Top 10
show.top.n <- 10
ggplot(data = df.rankings, aes(x = day, y = ranking, group = Store.ID)) +
  geom_line(aes(color = Store.ID, alpha = 1), size = 1) +
  geom_point(aes(color = Store.ID, alpha = 1), size = 3) +
  geom_point(color = "#FFFFFF", size = 1) +
  scale_y_reverse(breaks = 1:show.top.n) +
  scale_x_continuous(breaks = 1:5, minor_breaks = 1:5, expand = c(.05, .05)) +
  geom_text(data = df.rankings %>% filter(day == "1"),
            aes(label = Store.ID, x = 0.5) , hjust = .85, fontface = "bold", color = "#888888", size = 4) +
  geom_text(data = df.rankings %>% filter(day == "5"),
            aes(label = Store.ID, x = 16.5) , hjust = 0.15, fontface = "bold", color = "#888888", size = 4) +
  coord_cartesian(ylim = c(1,show.top.n)) + 
  my_theme() 

## Highlight
df.rankings <- df.rankings %>%
  mutate(flag = ifelse(Store.ID %in% c("38","84","83"), TRUE, FALSE),
         country_col = if_else(flag == TRUE, Store.ID, "zzz"))

show.top.n <- 10
ggplot(data = df.rankings, aes(x = day, y = ranking, group = Store.ID)) +
  geom_line(aes(color = country_col, alpha = 1), size = 2) +
  geom_point(color = "#FFFFFF", size = 4) +
  geom_point(aes(color = country_col, alpha = 1), size = 4) +
  geom_point(color = "#FFFFFF", size = 1) +
  scale_y_reverse(breaks = 1:show.top.n) +
  scale_x_continuous(breaks = 1:5, minor_breaks = 1:5, expand = c(.05, .05)) +
  geom_text(data = df.rankings %>% filter(day == "1"),
            aes(label = Store.ID, x = 0.5) , hjust = .85, fontface = "bold", color = "#888888", size = 4) +
  geom_text(data = df.rankings %>% filter(day == "5"),
            aes(label = Store.ID, x = 16.5) , hjust = 0.15, fontface = "bold", color = "#888888", size = 4) +
  coord_cartesian(ylim = c(1,show.top.n)) + 
  theme(legend.position = "none") +
  my_theme() +
  scale_color_manual(values = c("#F70020","#191A1A","#FB9701","#1A7D00","#072C8F","grey"))


# From "2020-20/5" - "2020-24/5"
Traffic2 <- subset(Traffic, Date <= "2020-05-24" & Date >= "2020-05-20")
Transformed <- Traffic2 %>% group_by(Store.ID) %>% transform(., day=match(Date, unique(Date)))
df.rankings <- Transformed %>%
  group_by(Date) %>%
  arrange(Date, desc(TotalTraffic), Store.ID) %>%
  mutate(ranking = row_number()) %>% as.data.frame()
df.rankings$Store.ID <- as.character(df.rankings$Store.ID)
ggplot(data = df.rankings, aes(x = day, y = ranking, group = Store.ID)) +
  geom_line(aes(color = Store.ID, alpha = 1), size = 1) +
  geom_point(aes(color = Store.ID, alpha = 1), size = 3) +
  scale_y_reverse(breaks = 1:nrow(df.rankings)) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) #stack bar
# Top 10
show.top.n <- 10
ggplot(data = df.rankings, aes(x = day, y = ranking, group = Store.ID)) +
  geom_line(aes(color = Store.ID, alpha = 1), size = 1) +
  geom_point(aes(color = Store.ID, alpha = 1), size = 3) +
  geom_point(color = "#FFFFFF", size = 1) +
  scale_y_reverse(breaks = 1:show.top.n) +
  scale_x_continuous(breaks = 1:10, minor_breaks = 1:10, expand = c(.05, .05)) +
  geom_text(data = df.rankings %>% filter(day == "1"),
            aes(label = Store.ID, x = 0.5) , hjust = .85, fontface = "bold", color = "#888888", size = 4) +
  geom_text(data = df.rankings %>% filter(day == "5"),
            aes(label = Store.ID, x = 16.5) , hjust = 0.15, fontface = "bold", color = "#888888", size = 4) +
  coord_cartesian(ylim = c(1,show.top.n)) + 
  my_theme() 

## Highlight
df.rankings <- df.rankings %>%
  mutate(flag = ifelse(Store.ID %in% c("38","84","83"), TRUE, FALSE),
         country_col = if_else(flag == TRUE, Store.ID, "zzz"))
show.top.n <- 10
ggplot(data = df.rankings, aes(x = day, y = ranking, group = Store.ID)) +
  geom_line(aes(color = country_col, alpha = 1), size = 2) +
  geom_point(color = "#FFFFFF", size = 4) +
  geom_point(aes(color = country_col, alpha = 1), size = 4) +
  geom_point(color = "#FFFFFF", size = 1) +
  scale_y_reverse(breaks = 1:show.top.n) +
  scale_x_continuous(breaks = 1:5, minor_breaks = 1:5, expand = c(.05, .05)) +
  geom_text(data = df.rankings %>% filter(day == "1"),
            aes(label = Store.ID, x = 0.5) , hjust = .85, fontface = "bold", color = "#888888", size = 4) +
  geom_text(data = df.rankings %>% filter(day == "5"),
            aes(label = Store.ID, x = 16.5) , hjust = 0.15, fontface = "bold", color = "#888888", size = 4) +
  coord_cartesian(ylim = c(1,show.top.n)) + 
  theme(legend.position = "none") +
  my_theme() +
  scale_color_manual(values = c("#F70020","#191A1A","#FB9701","#1A7D00","#072C8F","grey"))



# 3. Interaction by Store - CH nao co interaction tot theo ngay---- 
Shopping_mall <- Saledata %>% filter(Store.Type == "SHOPPING_MALL")
# 3.1 Interaction----
# Treemap 
Mall_Interaction <- Shopping_mall %>% group_by(Store.ID) %>% summarise(Total_Interaction = sum(Interaction), TotalTraffic = sum(Traffic)) %>% mutate(Interaction_conRate = round((Total_Interaction / TotalTraffic) * 100, 2))
Mall_Interaction

library(treemap)
    treemap(Mall_Interaction,
            index=c("Store.ID"), #single index
            vSize="Interaction_conRate",
            vColor="Interaction_conRate",
            palette="Set3",
            type="value") 
    
# Interaction line graph by date
Mall_Interaction <- Shopping_mall %>% group_by(Store.ID, Date) %>% summarise(Total_Interaction = sum(Interaction), TotalTraffic = sum(Traffic)) %>% mutate(Interaction_conRate = round((Total_Interaction / TotalTraffic) * 100, 2))

ggplot(data=Mall_Interaction[which(Mall_Interaction$Date <= "2020-05-17"),], aes(x = Date, y = Interaction_conRate)) + geom_line(aes(color = as.factor(Store.ID)), size = 0.7) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 

ggplot(data=Mall_Interaction[which(Mall_Interaction$Date <= "2020-05-24" & Mall_Interaction$Date >= "2020-05-20" ),], aes(x = Date, y = Interaction_conRate)) + geom_line(aes(color = as.factor(Store.ID)), size = 0.7) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggplot(data=Mall_Interaction[which(Mall_Interaction$Date <= "2020-06-14" & Mall_Interaction$Date >= "2020-06-05" ),], aes(x = Date, y = Interaction_conRate)) + geom_line(aes(color = as.factor(Store.ID)), size = 0.7) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  

ggplot(data=Mall_Interaction[which(Mall_Interaction$Date <= "2020-06-28" & Mall_Interaction$Date >= "2020-06-19" ),], aes(x = Date, y = Interaction_conRate)) + geom_line(aes(color = as.factor(Store.ID)), size = 0.7) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 


# 3.2 Ranking----
## Demo "2020-05-24"
Mall_Interaction1 <- subset(Mall_Interaction, Date <= "2020-05-24")
Transformed <- Mall_Interaction1 %>% group_by(Store.ID) %>% transform(., day=match(Date, unique(Date)))

df.rankings <- Transformed %>%
  group_by(Date) %>%
  arrange(Date, desc(Interaction_conRate), Store.ID) %>%
  mutate(ranking = row_number()) %>% as.data.frame()

df.rankings$Store.ID <- as.character(df.rankings$Store.ID)

ggplot(data = df.rankings, aes(x = day, y = ranking, group = Store.ID)) +
  geom_line(aes(color = Store.ID, alpha = 1), size = 1.2) +
  geom_point(aes(color = Store.ID, alpha = 1), size = 1.5) +
  scale_y_reverse(breaks = 1:nrow(df.rankings))



# 4. Transaction by Store - CH nao co transaction convert tot---- 
# 4.1 Transaction---- 
Shopping_mall <- Saledata %>% filter(Store.Type == "SHOPPING_MALL")
Shopping_mall
# Treemap 
Mall_Transaction <- Shopping_mall %>% group_by(Store.ID) %>% summarise(Total_Transaction = sum(Transaction), Total_Interaction = sum(Interaction)) %>% mutate(Transaction_conRate = round((Total_Transaction / Total_Interaction) * 100, 2))
Mall_Transaction
library(treemap)
treemap(Mall_Transaction,
            index="Store.ID",
            vSize="Transaction_conRate",
            type="index" # type = Index thi khong co scale bar
            )
library(treemap)
    treemap(Mall_Transaction,
            index=c("Store.ID"), #single index
            vSize="Transaction_conRate",
            vColor="Transaction_conRate",
            palette="RdBu",
            type="value") # type = value thi co scale bar

### By date 
Mall_Transaction <- Shopping_mall %>% group_by(Store.ID, Date) %>% summarise(Total_Transaction = sum(Transaction), Total_Interaction = sum(Interaction)) %>% mutate(Transaction_conRate = round((Total_Transaction / Total_Interaction) * 100, 2))

ggplot(data=Mall_Transaction[which(Mall_Transaction$Date <= "2020-05-17"),], aes(x = Date, y = Transaction_conRate)) + geom_line(aes(color = as.factor(Store.ID)), size = 0.7) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 

ggplot(data=Mall_Transaction[which(Mall_Transaction$Date <= "2020-05-24" & Mall_Transaction$Date >= "2020-05-20"),], aes(x = Date, y = Transaction_conRate)) + geom_line(aes(color = as.factor(Store.ID)), size = 0.7) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggplot(data=Mall_Transaction[which(Mall_Transaction$Date <= "2020-06-14" & Mall_Transaction$Date >= "2020-06-05" ),], aes(x = Date, y = Transaction_conRate)) + geom_line(aes(color = as.factor(Store.ID)), size = 0.7) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  

ggplot(data=Mall_Transaction[which(Mall_Transaction$Date <= "2020-06-28" & Mall_Transaction$Date >= "2020-06-19" ),], aes(x = Date, y = Transaction_conRate)) + geom_line(aes(color = as.factor(Store.ID)), size = 0.7) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 

# 4.2 Ranking---- 
## "2020-05-17"
Mall_Transaction1 <- subset(Mall_Transaction, Date <= "2020-06-14" & Date >= "2020-06-05")
Transformed <- Mall_Transaction1 %>% group_by(Store.ID) %>% transform(., day=match(Date, unique(Date))) 

df.rankings <- Transformed %>%
  group_by(Date) %>%
  arrange(Date, desc(Transaction_conRate), Store.ID) %>%
  mutate(ranking = row_number()) %>% as.data.frame()

df.rankings$Store.ID <- as.character(df.rankings$Store.ID)

ggplot(data = df.rankings, aes(x = day, y = ranking, group = Store.ID)) +
  geom_line(aes(color = Store.ID, alpha = 1), size = 1.2) +
  geom_point(aes(color = Store.ID, alpha = 1), size = 1.5) +
  scale_y_reverse(breaks = 1:nrow(df.rankings)) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) #stack bar

# bMall_Transaction <- Shopping_mall %>% group_by(Store.ID, Period) %>% summarise(Total_Transaction = sum(Transaction), Total_Interaction = sum(Interaction)) %>% mutate(Transaction_conRate = round((Total_Transaction / Total_Interaction) * 100, 2))
# 
# df.rankings <- bMall_Transaction %>% group_by(Period) %>%
#   arrange(Period, desc(Transaction_conRate), Store.ID) %>%
#   mutate(ranking = row_number()) %>% as.data.frame()
# 
# ggplot(data = df.rankings, aes(x = Period, y = ranking, group = Store.ID)) + geom_line(aes(color = Store.ID, alpha = 1), size = 1.0) +
#   geom_point(aes(color = Store.ID, alpha = 1), size = 2.0) +
#   scale_y_reverse(breaks = 1:nrow(df.rankings)) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) #stack bar

my_theme <- function() {

  # Colors
  color.background = "white"
  color.text = "#22211d"

  # Begin construction of chart
  theme_bw(base_size=15) +

    # Format background colors
    theme(panel.background = element_rect(fill=color.background, color=color.background)) +
    theme(plot.background  = element_rect(fill=color.background, color=color.background)) +
    theme(panel.border     = element_rect(color=color.background)) +
    theme(strip.background = element_rect(fill=color.background, color=color.background)) +

    # Format the grid
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(axis.ticks       = element_blank()) +

    # Format the legend
    theme(legend.position = "none") +

    # Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=20, face = "bold")) +
    theme(axis.title.x     = element_text(size=14, color="black", face = "bold")) +
    theme(axis.title.y     = element_text(size=14, color="black", face = "bold", vjust=1.25)) +
    theme(axis.text.x      = element_text(size=10, vjust=0.5, hjust=0.5, color = color.text)) +
    theme(axis.text.y      = element_text(size=10, color = color.text)) +
    theme(strip.text       = element_text(face = "bold")) +

    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}
## Top 10
show.top.n <- 10
ggplot(data = df.rankings, aes(x = day, y = ranking, group = Store.ID)) +
  geom_line(aes(color = Store.ID, alpha = 1), size = 2) +
  geom_point(aes(color = Store.ID, alpha = 1), size = 4) +
  geom_point(color = "#FFFFFF", size = 1) +
  scale_y_reverse(breaks = 1:show.top.n) +
  scale_x_continuous(breaks = 1:5, minor_breaks = 1:5, expand = c(.05, .05)) +
  geom_text(data = df.rankings %>% filter(day == "1"),
            aes(label = Store.ID, x = 0.5) , hjust = .85, fontface = "bold", color = "#888888", size = 4) +
  geom_text(data = df.rankings %>% filter(day == "5"),
            aes(label = Store.ID, x = 16.5) , hjust = 0.15, fontface = "bold", color = "#888888", size = 4) +
  coord_cartesian(ylim = c(1,show.top.n)) + 
  my_theme() 

## Highlight
df.rankings <- df.rankings %>%
  mutate(flag = ifelse(Store.ID %in% c("79","95","80"), TRUE, FALSE),
         country_col = if_else(flag == TRUE, Store.ID, "zzz"))

show.top.n <- 10

ggplot(data = df.rankings, aes(x = day, y = ranking, group = Store.ID)) +
  geom_line(aes(color = country_col, alpha = 1), size = 2) +
  geom_point(color = "#FFFFFF", size = 4) +
  geom_point(aes(color = country_col, alpha = 1), size = 4) +
  geom_point(color = "#FFFFFF", size = 1) +
  scale_y_reverse(breaks = 1:show.top.n) +
  scale_x_continuous(breaks = 1:5, minor_breaks = 1:5, expand = c(.05, .05)) +
  geom_text(data = df.rankings %>% filter(day == "1"),
            aes(label = Store.ID, x = 0.5) , hjust = .85, fontface = "bold", color = "#888888", size = 4) +
  geom_text(data = df.rankings %>% filter(day == "5"),
            aes(label = Store.ID, x = 16.5) , hjust = 0.15, fontface = "bold", color = "#888888", size = 4) +
  coord_cartesian(ylim = c(1,show.top.n)) + 
  theme(legend.position = "none") +
  my_theme() +
  scale_color_manual(values = c("#F70020","#191A1A","#FB9701","#1A7D00","#072C8F","grey"))


# From "2020-06-5" - "2020-06-14"
Mall_Transaction2 <- subset(Mall_Transaction, Mall_Transaction$Date <= "2020-05-24" & Mall_Transaction$Date >= "2020-05-20")
Transformed <- Mall_Transaction2 %>% group_by(Store.ID) %>% transform(., day=match(Date, unique(Date)))

df.rankings <- Transformed %>%
  group_by(Date) %>%
  arrange(Date, desc(Transaction_conRate), Store.ID) %>%
  mutate(ranking = row_number()) %>% as.data.frame()

df.rankings$Store.ID <- as.character(df.rankings$Store.ID)

ggplot(data = df.rankings, aes(x = day, y = ranking, group = Store.ID)) +
  geom_line(aes(color = Store.ID, alpha = 1), size = 1.2) +
  geom_point(aes(color = Store.ID, alpha = 1), size = 1.5) +
  scale_y_reverse(breaks = 1:nrow(df.rankings))

# Top 10
show.top.n <- 10
ggplot(data = df.rankings, aes(x = day, y = ranking, group = Store.ID)) +
  geom_line(aes(color = Store.ID, alpha = 1), size = 2) +
  geom_point(aes(color = Store.ID, alpha = 1), size = 4) +
  geom_point(color = "#FFFFFF", size = 1) +
  scale_y_reverse(breaks = 1:show.top.n) +
  scale_x_continuous(breaks = 1:10, minor_breaks = 1:10, expand = c(.05, .05)) +
  geom_text(data = df.rankings %>% filter(day == "1"),
            aes(label = Store.ID, x = 0.5) , hjust = .85, fontface = "bold", color = "#888888", size = 4) +
  geom_text(data = df.rankings %>% filter(day == "5"),
            aes(label = Store.ID, x = 16.5) , hjust = 0.15, fontface = "bold", color = "#888888", size = 4) +
  coord_cartesian(ylim = c(1,show.top.n)) + 
  my_theme() 
# Highlight
df.rankings <- df.rankings %>%
  mutate(flag = ifelse(Store.ID %in% c("82","93","97"), TRUE, FALSE),
         country_col = if_else(flag == TRUE, Store.ID, "zzz"))

show.top.n <- 10
ggplot(data = df.rankings, aes(x = day, y = ranking, group = Store.ID)) +
  geom_line(aes(color = country_col, alpha = 1), size = 2) +
  geom_point(color = "#FFFFFF", size = 4) +
  geom_point(aes(color = country_col, alpha = 1), size = 4) +
  geom_point(color = "#FFFFFF", size = 1) +
  scale_y_reverse(breaks = 1:show.top.n) +
  scale_x_continuous(breaks = 1:5, minor_breaks = 1:5, expand = c(.05, .05)) +
  geom_text(data = df.rankings %>% filter(day == "1"),
            aes(label = Store.ID, x = 0.5) , hjust = .85, fontface = "bold", color = "#888888", size = 4) +
  geom_text(data = df.rankings %>% filter(day == "5"),
            aes(label = Store.ID, x = 16.5) , hjust = 0.15, fontface = "bold", color = "#888888", size = 4) +
  coord_cartesian(ylim = c(1,show.top.n)) + 
  theme(legend.position = "none") +
  my_theme() +
  scale_color_manual(values = c("#F70020","#191A1A","#FB9701","#1A7D00","#072C8F","grey"))

# From "2020-06-19" - "2020-06-28": Lam tuong tu 

# 5. Funnel for each Store----
Shopping_mall <- Saledata %>% filter(Store.Type == "SHOPPING_MALL")

Shopping_mall_F <- Shopping_mall %>% group_by(Store.ID) %>% summarise(Traffic = sum(Traffic), Interaction = sum(Interaction), Convert = sum(Transaction))
Shopping_mall_F

Shopping_mall_F <- gather(Shopping_mall_F, Condition, Measurement, Traffic:Convert, factor_key=TRUE) # convert wide to long

Shopping_mall_F <- Shopping_mall_F %>% filter(Store.ID == "90")
Shopping_mall_F

library(plotly)
fig <- plot_ly() 
fig <- fig %>%
  add_trace(type = "funnel",
            y = Shopping_mall_F$Condition,
            x = Shopping_mall_F$Measurement,
            textposition = "inside",
            textinfo = "value+percent initial",
            opacity = 0.65,
            marker = list(color = c("deepskyblue", "lightsalmon", "tan", "teal", "silver"),
                          line = list(width = c(4, 2, 2, 3, 1, 1), color = c("wheat", "wheat", "blue", "wheat", "wheat"))),
            connector = list(line = list(color = "royalblue", dash = "dot", width = 3)))
fig <- fig %>%
  layout(yaxis = list(categoryarray = Shopping_mall_F$Condition))
fig



# 6. Cua hang nao co ca ba chi so deu tot---- 
Shopping_mall <- Saledata %>% filter(Store.Type == "SHOPPING_MALL")

library(tidyverse)
library(magrittr)

Shopping_mall_test <- Shopping_mall %>% group_by(Store.ID) %>% summarise(sumTraffic = sum(Traffic),sumInteraction = sum(Interaction),sumTransaction = sum(Transaction),meanBasetSize = mean(Avg.Basket.Size))

Shopping_mall_test$Store.ID <- as.character(Shopping_mall_test$Store.ID)

require(knitr)
kable(head(Shopping_mall_test))
summary(Shopping_mall_test)

Shopping_mall_test <- na.omit(Shopping_mall_test)
summary(Shopping_mall_test)

dsData <- Shopping_mall_test

row.names(dsData) <- Shopping_mall_test$Store.ID
dsData <- scale(dsData[,2:5])
summary(dsData)

set.seed(123)
km.res <- kmeans(dsData, 5, nstart = 25)
km.res

a <- as.data.frame(km.res$centers)

length(km.res$cluster) #Coi so luong observation 
nrow(Shopping_mall_test)
Shopping_mall_test$clusterk <- as.factor(km.res$cluster)
kable(head(Shopping_mall_test))

library(writexl)
write_xlsx(x = Shopping_mall_test, path = "Shopping_mall_test.xlsx",col_names = TRUE) # Xuat ra file excel 




# INDI - OPERATION ---- 
## 1. Data preperation----
IND <- Saledata %>% filter(Store.Type == "INDEPENDENT")

library(dplyr)
df1 <- IND %>%
  group_by(Store.ID) %>%
  mutate(minDia=min(Date, na.rm=T), maxDia=max(Date, na.rm=T))
df2 <- df1 %>%
  group_by(Store.ID) %>%
  mutate(minDia=min(Date, na.rm=T), maxDia=max(Date, na.rm=T)) %>%
  ungroup() %>%
  distinct(Store.ID, minDia, maxDia)

# 2. Traffic by store - CH nao co luong traffic tot theo ngay----
# 2.1 Traffic======
# Treemap
Traffic <- IND %>% group_by(Store.ID) %>% summarise(TotalTraffic = sum(Traffic))
library(treemap)
    treemap(Traffic,
            index=c("Store.ID"), #single index
            vSize="TotalTraffic",
            vColor="TotalTraffic",
            palette="Purples",
            type="value") 

# Line chart by date
Traffic <- IND %>% group_by(Store.ID, Date) %>% summarise(TotalTraffic = sum(Traffic))

ggplot(data=Traffic[which(Traffic$Date <= "2020-05-17"),], aes(x = Date, y = TotalTraffic)) + geom_line(aes(color = as.factor(Store.ID)), size = 0.7) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 

ggplot(data=Traffic[which(Traffic$Date <= "2020-05-24" & Traffic$Date >= "2020-05-20" ),], aes(x = Date, y = TotalTraffic)) + geom_line(aes(color = as.factor(Store.ID)), size = 0.7) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggplot(data=Traffic[which(Traffic$Date <= "2020-06-14" & Traffic$Date >= "2020-06-05" ),], aes(x = Date, y = TotalTraffic)) + geom_line(aes(color = as.factor(Store.ID)), size = 0.7) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  

ggplot(data=Traffic[which(Traffic$Date <= "2020-06-28" & Traffic$Date >= "2020-06-19" ),], aes(x = Date, y = TotalTraffic)) + geom_line(aes(color = as.factor(Store.ID)), size = 0.7) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  


# 2.2 Ranking---- 
# "2020-05-24"
table(Saledata$Period)
my_theme <- function() {

  # Colors
  color.background = "white"
  color.text = "#22211d"

  # Begin construction of chart
  theme_bw(base_size=15) +

    # Format background colors
    theme(panel.background = element_rect(fill=color.background, color=color.background)) +
    theme(plot.background  = element_rect(fill=color.background, color=color.background)) +
    theme(panel.border     = element_rect(color=color.background)) +
    theme(strip.background = element_rect(fill=color.background, color=color.background)) +

    # Format the grid
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(axis.ticks       = element_blank()) +

    # Format the legend
    theme(legend.position = "none") +

    # Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=20, face = "bold")) +
    theme(axis.title.x     = element_text(size=14, color="black", face = "bold")) +
    theme(axis.title.y     = element_text(size=14, color="black", face = "bold", vjust=1.25)) +
    theme(axis.text.x      = element_text(size=10, vjust=0.5, hjust=0.5, color = color.text)) +
    theme(axis.text.y      = element_text(size=10, color = color.text)) +
    theme(strip.text       = element_text(face = "bold")) +

    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

Traffic1 <- subset(Traffic, Date <= "2020-05-17")

Transformed <- Traffic1 %>% group_by(Store.ID) %>% transform(., day=match(Date, unique(Date)))

df.rankings <- Transformed %>%
  group_by(Date) %>%
  arrange(Date, desc(TotalTraffic), Store.ID) %>%
  mutate(ranking = row_number()) %>% as.data.frame()

df.rankings$Store.ID <- as.character(df.rankings$Store.ID)

ggplot(data = df.rankings, aes(x = day, y = ranking, group = Store.ID)) +
  geom_line(aes(color = Store.ID, alpha = 1), size = 1) +
  geom_point(aes(color = Store.ID, alpha = 1), size = 3) +
  scale_y_reverse(breaks = 1:nrow(df.rankings)) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) #stack bar


# Top 10
show.top.n <- 10

ggplot(data = df.rankings, aes(x = day, y = ranking, group = Store.ID)) +
  geom_line(aes(color = Store.ID, alpha = 1), size = 1) +
  geom_point(aes(color = Store.ID, alpha = 1), size = 3) +
  geom_point(color = "#FFFFFF", size = 1) +
  scale_y_reverse(breaks = 1:show.top.n) +
  scale_x_continuous(breaks = 1:5, minor_breaks = 1:5, expand = c(.05, .05)) +
  geom_text(data = df.rankings %>% filter(day == "1"),
            aes(label = Store.ID, x = 0.5) , hjust = .85, fontface = "bold", color = "#888888", size = 4) +
  geom_text(data = df.rankings %>% filter(day == "5"),
            aes(label = Store.ID, x = 16.5) , hjust = 0.15, fontface = "bold", color = "#888888", size = 4) +
  coord_cartesian(ylim = c(1,show.top.n)) + 
  my_theme() 

## Highlight
df.rankings <- df.rankings %>%
  mutate(flag = ifelse(Store.ID %in% c("38","84","83"), TRUE, FALSE),
         country_col = if_else(flag == TRUE, Store.ID, "zzz"))

show.top.n <- 10

ggplot(data = df.rankings, aes(x = day, y = ranking, group = Store.ID)) +
  geom_line(aes(color = country_col, alpha = 1), size = 2) +
  geom_point(color = "#FFFFFF", size = 4) +
  geom_point(aes(color = country_col, alpha = 1), size = 4) +
  geom_point(color = "#FFFFFF", size = 1) +
  scale_y_reverse(breaks = 1:show.top.n) +
  scale_x_continuous(breaks = 1:5, minor_breaks = 1:5, expand = c(.05, .05)) +
  geom_text(data = df.rankings %>% filter(day == "1"),
            aes(label = Store.ID, x = 0.5) , hjust = .85, fontface = "bold", color = "#888888", size = 4) +
  geom_text(data = df.rankings %>% filter(day == "5"),
            aes(label = Store.ID, x = 16.5) , hjust = 0.15, fontface = "bold", color = "#888888", size = 4) +
  coord_cartesian(ylim = c(1,show.top.n)) + 
  theme(legend.position = "none") +
  my_theme() +
  scale_color_manual(values = c("#F70020","#191A1A","#FB9701","#1A7D00","#072C8F","grey"))

# From "2020-20/5" - "2020-24/5"
Traffic2 <- subset(Traffic, Date <= "2020-05-24" & Date >= "2020-05-20")

Transformed <- Traffic2 %>% group_by(Store.ID) %>% transform(., day=match(Date, unique(Date)))

df.rankings <- Transformed %>%
  group_by(Date) %>%
  arrange(Date, desc(TotalTraffic), Store.ID) %>%
  mutate(ranking = row_number()) %>% as.data.frame()

df.rankings$Store.ID <- as.character(df.rankings$Store.ID)

ggplot(data = df.rankings, aes(x = day, y = ranking, group = Store.ID)) +
  geom_line(aes(color = Store.ID, alpha = 1), size = 1) +
  geom_point(aes(color = Store.ID, alpha = 1), size = 3) +
  scale_y_reverse(breaks = 1:nrow(df.rankings)) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) #stack bar

# Top 10
show.top.n <- 10
ggplot(data = df.rankings, aes(x = day, y = ranking, group = Store.ID)) +
  geom_line(aes(color = Store.ID, alpha = 1), size = 1) +
  geom_point(aes(color = Store.ID, alpha = 1), size = 3) +
  geom_point(color = "#FFFFFF", size = 1) +
  scale_y_reverse(breaks = 1:show.top.n) +
  scale_x_continuous(breaks = 1:10, minor_breaks = 1:10, expand = c(.05, .05)) +
  geom_text(data = df.rankings %>% filter(day == "1"),
            aes(label = Store.ID, x = 0.5) , hjust = .85, fontface = "bold", color = "#888888", size = 4) +
  geom_text(data = df.rankings %>% filter(day == "5"),
            aes(label = Store.ID, x = 16.5) , hjust = 0.15, fontface = "bold", color = "#888888", size = 4) +
  coord_cartesian(ylim = c(1,show.top.n)) + 
  my_theme() 

## Highlight
df.rankings <- df.rankings %>%
  mutate(flag = ifelse(Store.ID %in% c("38","84","83"), TRUE, FALSE),
         country_col = if_else(flag == TRUE, Store.ID, "zzz"))

show.top.n <- 10

ggplot(data = df.rankings, aes(x = day, y = ranking, group = Store.ID)) +
  geom_line(aes(color = country_col, alpha = 1), size = 2) +
  geom_point(color = "#FFFFFF", size = 4) +
  geom_point(aes(color = country_col, alpha = 1), size = 4) +
  geom_point(color = "#FFFFFF", size = 1) +
  scale_y_reverse(breaks = 1:show.top.n) +
  scale_x_continuous(breaks = 1:5, minor_breaks = 1:5, expand = c(.05, .05)) +
  geom_text(data = df.rankings %>% filter(day == "1"),
            aes(label = Store.ID, x = 0.5) , hjust = .85, fontface = "bold", color = "#888888", size = 4) +
  geom_text(data = df.rankings %>% filter(day == "5"),
            aes(label = Store.ID, x = 16.5) , hjust = 0.15, fontface = "bold", color = "#888888", size = 4) +
  coord_cartesian(ylim = c(1,show.top.n)) + 
  theme(legend.position = "none") +
  my_theme() +
  scale_color_manual(values = c("#F70020","#191A1A","#FB9701","#1A7D00","#072C8F","grey"))


## From "2020-06-5" - "2020-06-14"
Traffic3 <- subset(Traffic, Date <= "2020-06-14" & Date >= "2020-06-05")
Transformed <- Traffic3 %>% group_by(Store.ID) %>% transform(., day=match(Date, unique(Date)))
df.rankings <- Transformed %>%
  group_by(Date) %>%
  arrange(Date, desc(TotalTraffic), Store.ID) %>%
  mutate(ranking = row_number()) %>% as.data.frame()
df.rankings$Store.ID <- as.character(df.rankings$Store.ID)

ggplot(data = df.rankings, aes(x = day, y = ranking, group = Store.ID)) +
  geom_line(aes(color = Store.ID, alpha = 1), size = 1) +
  geom_point(aes(color = Store.ID, alpha = 1), size = 3) +
  scale_y_reverse(breaks = 1:nrow(df.rankings)) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) #stack bar

# ggplot(data = df.rankings, aes(x = day, y = ranking, group = Store.ID)) +
#   geom_line(aes(color = Store.ID, alpha = 1), size = 1.2) +
#   geom_point(aes(color = Store.ID, alpha = 1), size = 1.5) +
#   scale_y_reverse(breaks = 1:nrow(df.rankings))

# Top 10
show.top.n <- 10
ggplot(data = df.rankings, aes(x = day, y = ranking, group = Store.ID)) +
  geom_line(aes(color = Store.ID, alpha = 1), size = 1) +
  geom_point(aes(color = Store.ID, alpha = 1), size = 3) +
  geom_point(color = "#FFFFFF", size = 1) +
  scale_y_reverse(breaks = 1:show.top.n) +
  scale_x_continuous(breaks = 1:10, minor_breaks = 1:10, expand = c(.05, .05)) +
  geom_text(data = df.rankings %>% filter(day == "1"),
            aes(label = Store.ID, x = 0.5) , hjust = .85, fontface = "bold", color = "#888888", size = 4) +
  geom_text(data = df.rankings %>% filter(day == "5"),
            aes(label = Store.ID, x = 16.5) , hjust = 0.15, fontface = "bold", color = "#888888", size = 4) +
  coord_cartesian(ylim = c(1,show.top.n)) + 
  my_theme() 
# Highlight
df.rankings <- df.rankings %>%
  mutate(flag = ifelse(Store.ID %in% c("38","84","83"), TRUE, FALSE),
         country_col = if_else(flag == TRUE, Store.ID, "zzz"))
show.top.n <- 10
ggplot(data = df.rankings, aes(x = day, y = ranking, group = Store.ID)) +
  geom_line(aes(color = country_col, alpha = 1), size = 2) +
  geom_point(color = "#FFFFFF", size = 4) +
  geom_point(aes(color = country_col, alpha = 1), size = 4) +
  geom_point(color = "#FFFFFF", size = 1) +
  scale_y_reverse(breaks = 1:show.top.n) +
  scale_x_continuous(breaks = 1:10, minor_breaks = 1:10, expand = c(.05, .05)) +
  geom_text(data = df.rankings %>% filter(day == "1"),
            aes(label = Store.ID, x = 0.5) , hjust = .85, fontface = "bold", color = "#888888", size = 4) +
  geom_text(data = df.rankings %>% filter(day == "5"),
            aes(label = Store.ID, x = 16.5) , hjust = 0.15, fontface = "bold", color = "#888888", size = 4) +
  coord_cartesian(ylim = c(1,show.top.n)) + 
  theme(legend.position = "none") +
  my_theme() +
  scale_color_manual(values = c("#F70020","#191A1A","#FB9701","#1A7D00","#072C8F","grey"))


## From "2020-06-19" - "2020-06-28"
Traffic4 <- subset(Traffic, Date <= "2020-06-28" & Date >= "2020-06-19")
Transformed <- Traffic4 %>% group_by(Store.ID) %>% transform(., day=match(Date, unique(Date)))
df.rankings <- Transformed %>%
  group_by(Date) %>%
  arrange(Date, desc(TotalTraffic), Store.ID) %>%
  mutate(ranking = row_number()) %>% as.data.frame()
df.rankings$Store.ID <- as.character(df.rankings$Store.ID)

ggplot(data = df.rankings, aes(x = day, y = ranking, group = Store.ID)) +
  geom_line(aes(color = Store.ID, alpha = 1), size = 1) +
  geom_point(aes(color = Store.ID, alpha = 1), size = 3) +
  scale_y_reverse(breaks = 1:nrow(df.rankings)) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) #stack bar

# ggplot(data = df.rankings, aes(x = day, y = ranking, group = Store.ID)) +
#   geom_line(aes(color = Store.ID, alpha = 1), size = 1.2) +
#   geom_point(aes(color = Store.ID, alpha = 1), size = 1.5) +
#   scale_y_reverse(breaks = 1:nrow(df.rankings))

# Top 10
show.top.n <- 10
ggplot(data = df.rankings, aes(x = day, y = ranking, group = Store.ID)) +
  geom_line(aes(color = Store.ID, alpha = 1), size = 1) +
  geom_point(aes(color = Store.ID, alpha = 1), size = 3) +
  geom_point(color = "#FFFFFF", size = 1) +
  scale_y_reverse(breaks = 1:show.top.n) +
  scale_x_continuous(breaks = 1:10, minor_breaks = 1:10, expand = c(.05, .05)) +
  geom_text(data = df.rankings %>% filter(day == "1"),
            aes(label = Store.ID, x = 0.5) , hjust = .85, fontface = "bold", color = "#888888", size = 4) +
  geom_text(data = df.rankings %>% filter(day == "5"),
            aes(label = Store.ID, x = 16.5) , hjust = 0.15, fontface = "bold", color = "#888888", size = 4) +
  coord_cartesian(ylim = c(1,show.top.n)) + 
  my_theme() 
# Highlight
df.rankings <- df.rankings %>%
  mutate(flag = ifelse(Store.ID %in% c("38","84","83"), TRUE, FALSE),
         country_col = if_else(flag == TRUE, Store.ID, "zzz"))
show.top.n <- 10
ggplot(data = df.rankings, aes(x = day, y = ranking, group = Store.ID)) +
  geom_line(aes(color = country_col, alpha = 1), size = 2) +
  geom_point(color = "#FFFFFF", size = 4) +
  geom_point(aes(color = country_col, alpha = 1), size = 4) +
  geom_point(color = "#FFFFFF", size = 1) +
  scale_y_reverse(breaks = 1:show.top.n) +
  scale_x_continuous(breaks = 1:10, minor_breaks = 1:10, expand = c(.05, .05)) +
  geom_text(data = df.rankings %>% filter(day == "1"),
            aes(label = Store.ID, x = 0.5) , hjust = .85, fontface = "bold", color = "#888888", size = 4) +
  geom_text(data = df.rankings %>% filter(day == "5"),
            aes(label = Store.ID, x = 16.5) , hjust = 0.15, fontface = "bold", color = "#888888", size = 4) +
  coord_cartesian(ylim = c(1,show.top.n)) + 
  theme(legend.position = "none") +
  my_theme() +
  scale_color_manual(values = c("#F70020","#191A1A","#FB9701","#1A7D00","#072C8F","grey"))


# 3. Interaction by Store - CH nao co interaction tot theo ngay---- 
IND <- Saledata %>% filter(Store.Type == "INDEPENDENT")
# 3.1 Interaction----
# Treemap 
Mall_Interaction <- IND %>% group_by(Store.ID) %>% summarise(Total_Interaction = sum(Interaction), TotalTraffic = sum(Traffic)) %>% mutate(Interaction_conRate = round((Total_Interaction / TotalTraffic) * 100, 2))
Mall_Interaction

library(treemap)
    treemap(Mall_Interaction,
            index=c("Store.ID"), #single index
            vSize="Interaction_conRate",
            vColor="Interaction_conRate",
            palette="Set3",
            type="value") 
    
# Interaction line graph by date
Mall_Interaction <- IND %>% group_by(Store.ID, Date) %>% summarise(Total_Interaction = sum(Interaction), TotalTraffic = sum(Traffic)) %>% mutate(Interaction_conRate = round((Total_Interaction / TotalTraffic) * 100, 2))

ggplot(data=Mall_Interaction[which(Mall_Interaction$Date <= "2020-05-17"),], aes(x = Date, y = Interaction_conRate)) + geom_line(aes(color = as.factor(Store.ID)), size = 0.7) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 

ggplot(data=Mall_Interaction[which(Mall_Interaction$Date <= "2020-05-24" & Mall_Interaction$Date >= "2020-05-20" ),], aes(x = Date, y = Interaction_conRate)) + geom_line(aes(color = as.factor(Store.ID)), size = 0.7) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggplot(data=Mall_Interaction[which(Mall_Interaction$Date <= "2020-06-14" & Mall_Interaction$Date >= "2020-06-05" ),], aes(x = Date, y = Interaction_conRate)) + geom_line(aes(color = as.factor(Store.ID)), size = 0.7) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  

ggplot(data=Mall_Interaction[which(Mall_Interaction$Date <= "2020-06-28" & Mall_Interaction$Date >= "2020-06-19" ),], aes(x = Date, y = Interaction_conRate)) + geom_line(aes(color = as.factor(Store.ID)), size = 0.7) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 


# 3.2 Ranking----
## "2020-05-24"
Mall_Interaction1 <- subset(Mall_Interaction, Date <= "2020-05-24")
Transformed <- Mall_Interaction1 %>% group_by(Store.ID) %>% transform(., day=match(Date, unique(Date)))

df.rankings <- Transformed %>%
  group_by(Date) %>%
  arrange(Date, desc(Interaction_conRate), Store.ID) %>%
  mutate(ranking = row_number()) %>% as.data.frame()

df.rankings$Store.ID <- as.character(df.rankings$Store.ID)

ggplot(data = df.rankings, aes(x = day, y = ranking, group = Store.ID)) +
  geom_line(aes(color = Store.ID, alpha = 1), size = 1.2) +
  geom_point(aes(color = Store.ID, alpha = 1), size = 1.5) +
  scale_y_reverse(breaks = 1:nrow(df.rankings))

my_theme <- function() {

  # Colors
  color.background = "white"
  color.text = "#22211d"

  # Begin construction of chart
  theme_bw(base_size=15) +

    # Format background colors
    theme(panel.background = element_rect(fill=color.background, color=color.background)) +
    theme(plot.background  = element_rect(fill=color.background, color=color.background)) +
    theme(panel.border     = element_rect(color=color.background)) +
    theme(strip.background = element_rect(fill=color.background, color=color.background)) +

    # Format the grid
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(axis.ticks       = element_blank()) +

    # Format the legend
    theme(legend.position = "none") +

    # Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=20, face = "bold")) +
    theme(axis.title.x     = element_text(size=14, color="black", face = "bold")) +
    theme(axis.title.y     = element_text(size=14, color="black", face = "bold", vjust=1.25)) +
    theme(axis.text.x      = element_text(size=10, vjust=0.5, hjust=0.5, color = color.text)) +
    theme(axis.text.y      = element_text(size=10, color = color.text)) +
    theme(strip.text       = element_text(face = "bold")) +

    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}
## Top 10
show.top.n <- 10
ggplot(data = df.rankings, aes(x = day, y = ranking, group = Store.ID)) +
  geom_line(aes(color = Store.ID, alpha = 1), size = 2) +
  geom_point(aes(color = Store.ID, alpha = 1), size = 4) +
  geom_point(color = "#FFFFFF", size = 1) +
  scale_y_reverse(breaks = 1:show.top.n) +
  scale_x_continuous(breaks = 1:5, minor_breaks = 1:5, expand = c(.05, .05)) +
  geom_text(data = df.rankings %>% filter(day == "1"),
            aes(label = Store.ID, x = 0.5) , hjust = .85, fontface = "bold", color = "#888888", size = 4) +
  geom_text(data = df.rankings %>% filter(day == "5"),
            aes(label = Store.ID, x = 16.5) , hjust = 0.15, fontface = "bold", color = "#888888", size = 4) +
  coord_cartesian(ylim = c(1,show.top.n)) + 
  my_theme() 

## Highlight
df.rankings <- df.rankings %>%
  mutate(flag = ifelse(Store.ID %in% c("79","95","80"), TRUE, FALSE),
         country_col = if_else(flag == TRUE, Store.ID, "zzz"))

show.top.n <- 10

ggplot(data = df.rankings, aes(x = day, y = ranking, group = Store.ID)) +
  geom_line(aes(color = country_col, alpha = 1), size = 2) +
  geom_point(color = "#FFFFFF", size = 4) +
  geom_point(aes(color = country_col, alpha = 1), size = 4) +
  geom_point(color = "#FFFFFF", size = 1) +
  scale_y_reverse(breaks = 1:show.top.n) +
  scale_x_continuous(breaks = 1:5, minor_breaks = 1:5, expand = c(.05, .05)) +
  geom_text(data = df.rankings %>% filter(day == "1"),
            aes(label = Store.ID, x = 0.5) , hjust = .85, fontface = "bold", color = "#888888", size = 4) +
  geom_text(data = df.rankings %>% filter(day == "5"),
            aes(label = Store.ID, x = 16.5) , hjust = 0.15, fontface = "bold", color = "#888888", size = 4) +
  coord_cartesian(ylim = c(1,show.top.n)) + 
  theme(legend.position = "none") +
  my_theme() +
  scale_color_manual(values = c("#F70020","#191A1A","#FB9701","#1A7D00","#072C8F","grey"))


# From "2020-06-5" - "2020-06-14"
Mall_Interaction2 <- subset(Mall_Interaction, Date <= "2020-06-14" & Date >= "2020-06-05")
Transformed <- Mall_Interaction2 %>% group_by(Store.ID) %>% transform(., day=match(Date, unique(Date)))

df.rankings <- Transformed %>%
  group_by(Date) %>%
  arrange(Date, desc(Interaction_conRate), Store.ID) %>%
  mutate(ranking = row_number()) %>% as.data.frame()

df.rankings$Store.ID <- as.character(df.rankings$Store.ID)

ggplot(data = df.rankings, aes(x = day, y = ranking, group = Store.ID)) +
  geom_line(aes(color = Store.ID, alpha = 1), size = 1.2) +
  geom_point(aes(color = Store.ID, alpha = 1), size = 1.5) +
  scale_y_reverse(breaks = 1:nrow(df.rankings))

# Top 10
show.top.n <- 10
ggplot(data = df.rankings, aes(x = day, y = ranking, group = Store.ID)) +
  geom_line(aes(color = Store.ID, alpha = 1), size = 2) +
  geom_point(aes(color = Store.ID, alpha = 1), size = 4) +
  geom_point(color = "#FFFFFF", size = 1) +
  scale_y_reverse(breaks = 1:show.top.n) +
  scale_x_continuous(breaks = 1:10, minor_breaks = 1:10, expand = c(.05, .05)) +
  geom_text(data = df.rankings %>% filter(day == "1"),
            aes(label = Store.ID, x = 0.5) , hjust = .85, fontface = "bold", color = "#888888", size = 4) +
  geom_text(data = df.rankings %>% filter(day == "5"),
            aes(label = Store.ID, x = 16.5) , hjust = 0.15, fontface = "bold", color = "#888888", size = 4) +
  coord_cartesian(ylim = c(1,show.top.n)) + 
  my_theme() 
# Highlight
df.rankings <- df.rankings %>%
  mutate(flag = ifelse(Store.ID %in% c("82","93","97"), TRUE, FALSE),
         country_col = if_else(flag == TRUE, Store.ID, "zzz"))

show.top.n <- 10
ggplot(data = df.rankings, aes(x = day, y = ranking, group = Store.ID)) +
  geom_line(aes(color = country_col, alpha = 1), size = 2) +
  geom_point(color = "#FFFFFF", size = 4) +
  geom_point(aes(color = country_col, alpha = 1), size = 4) +
  geom_point(color = "#FFFFFF", size = 1) +
  scale_y_reverse(breaks = 1:show.top.n) +
  scale_x_continuous(breaks = 1:5, minor_breaks = 1:5, expand = c(.05, .05)) +
  geom_text(data = df.rankings %>% filter(day == "1"),
            aes(label = Store.ID, x = 0.5) , hjust = .85, fontface = "bold", color = "#888888", size = 4) +
  geom_text(data = df.rankings %>% filter(day == "5"),
            aes(label = Store.ID, x = 16.5) , hjust = 0.15, fontface = "bold", color = "#888888", size = 4) +
  coord_cartesian(ylim = c(1,show.top.n)) + 
  theme(legend.position = "none") +
  my_theme() +
  scale_color_manual(values = c("#F70020","#191A1A","#FB9701","#1A7D00","#072C8F","grey"))

# From "2020-06-19" - "2020-06-28": Lam tuong tu 


# 4. Transaction by Store - CH nao co transaction convert tot---- 
# 4.1 Transaction---- 
IND <- Saledata %>% filter(Store.Type == "INDEPENDENT")

# Treemap 
IND_Transaction <- IND %>% group_by(Store.ID) %>% summarise(Total_Transaction = sum(Transaction), Total_Interaction = sum(Interaction)) %>% mutate(Transaction_conRate = round((Total_Transaction / Total_Interaction) * 100, 2))

library(treemap)
    treemap(IND_Transaction,
            index=c("Store.ID"), #single index
            vSize="Transaction_conRate",
            vColor="Transaction_conRate",
            palette="RdBu",
            type="value") # type = value thi co scale bar

### By date 
Mall_Transaction <- IND %>% group_by(Store.ID, Date) %>% summarise(Total_Transaction = sum(Transaction), Total_Interaction = sum(Interaction)) %>% mutate(Transaction_conRate = round((Total_Transaction / Total_Interaction) * 100, 2))

ggplot(data=Mall_Transaction[which(Mall_Transaction$Date <= "2020-05-17"),], aes(x = Date, y = Transaction_conRate)) + geom_line(aes(color = as.factor(Store.ID)), size = 0.7) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 

ggplot(data=Mall_Transaction[which(Mall_Transaction$Date <= "2020-05-24" & Mall_Transaction$Date >= "2020-05-20" ),], aes(x = Date, y = Transaction_conRate)) + geom_line(aes(color = as.factor(Store.ID)), size = 0.7) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggplot(data=Mall_Transaction[which(Mall_Transaction$Date <= "2020-06-14" & Mall_Transaction$Date >= "2020-06-05" ),], aes(x = Date, y = Transaction_conRate)) + geom_line(aes(color = as.factor(Store.ID)), size = 0.7) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  

ggplot(data=Mall_Transaction[which(Mall_Transaction$Date <= "2020-06-28" & Mall_Transaction$Date >= "2020-06-19" ),], aes(x = Date, y = Transaction_conRate)) + geom_line(aes(color = as.factor(Store.ID)), size = 0.7) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 

# 4.2 Ranking---- 
## "2020-05-24"
Mall_Transaction1 <- subset(Mall_Transaction, Date <= "2020-06-14" & Date >= "2020-06-05")
Transformed <- Mall_Transaction1 %>% group_by(Store.ID) %>% transform(., day=match(Date, unique(Date))) 

df.rankings <- Transformed %>%
  group_by(Date) %>%
  arrange(Date, desc(Transaction_conRate), Store.ID) %>%
  mutate(ranking = row_number()) %>% as.data.frame()

df.rankings$Store.ID <- as.character(df.rankings$Store.ID)

ggplot(data = df.rankings, aes(x = day, y = ranking, group = Store.ID)) +
  geom_line(aes(color = Store.ID, alpha = 1), size = 1.2) +
  geom_point(aes(color = Store.ID, alpha = 1), size = 1.5) +
  scale_y_reverse(breaks = 1:nrow(df.rankings)) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) #stack bar

my_theme <- function() {

  # Colors
  color.background = "white"
  color.text = "#22211d"

  # Begin construction of chart
  theme_bw(base_size=15) +

    # Format background colors
    theme(panel.background = element_rect(fill=color.background, color=color.background)) +
    theme(plot.background  = element_rect(fill=color.background, color=color.background)) +
    theme(panel.border     = element_rect(color=color.background)) +
    theme(strip.background = element_rect(fill=color.background, color=color.background)) +

    # Format the grid
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(axis.ticks       = element_blank()) +

    # Format the legend
    theme(legend.position = "none") +

    # Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=20, face = "bold")) +
    theme(axis.title.x     = element_text(size=14, color="black", face = "bold")) +
    theme(axis.title.y     = element_text(size=14, color="black", face = "bold", vjust=1.25)) +
    theme(axis.text.x      = element_text(size=10, vjust=0.5, hjust=0.5, color = color.text)) +
    theme(axis.text.y      = element_text(size=10, color = color.text)) +
    theme(strip.text       = element_text(face = "bold")) +

    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}
## Top 10
show.top.n <- 10
ggplot(data = df.rankings, aes(x = day, y = ranking, group = Store.ID)) +
  geom_line(aes(color = Store.ID, alpha = 1), size = 2) +
  geom_point(aes(color = Store.ID, alpha = 1), size = 4) +
  geom_point(color = "#FFFFFF", size = 1) +
  scale_y_reverse(breaks = 1:show.top.n) +
  scale_x_continuous(breaks = 1:5, minor_breaks = 1:5, expand = c(.05, .05)) +
  geom_text(data = df.rankings %>% filter(day == "1"),
            aes(label = Store.ID, x = 0.5) , hjust = .85, fontface = "bold", color = "#888888", size = 4) +
  geom_text(data = df.rankings %>% filter(day == "5"),
            aes(label = Store.ID, x = 16.5) , hjust = 0.15, fontface = "bold", color = "#888888", size = 4) +
  coord_cartesian(ylim = c(1,show.top.n)) + 
  my_theme() 

## Highlight
df.rankings <- df.rankings %>%
  mutate(flag = ifelse(Store.ID %in% c("79","95","80"), TRUE, FALSE),
         country_col = if_else(flag == TRUE, Store.ID, "zzz"))

show.top.n <- 10

ggplot(data = df.rankings, aes(x = day, y = ranking, group = Store.ID)) +
  geom_line(aes(color = country_col, alpha = 1), size = 2) +
  geom_point(color = "#FFFFFF", size = 4) +
  geom_point(aes(color = country_col, alpha = 1), size = 4) +
  geom_point(color = "#FFFFFF", size = 1) +
  scale_y_reverse(breaks = 1:show.top.n) +
  scale_x_continuous(breaks = 1:5, minor_breaks = 1:5, expand = c(.05, .05)) +
  geom_text(data = df.rankings %>% filter(day == "1"),
            aes(label = Store.ID, x = 0.5) , hjust = .85, fontface = "bold", color = "#888888", size = 4) +
  geom_text(data = df.rankings %>% filter(day == "5"),
            aes(label = Store.ID, x = 16.5) , hjust = 0.15, fontface = "bold", color = "#888888", size = 4) +
  coord_cartesian(ylim = c(1,show.top.n)) + 
  theme(legend.position = "none") +
  my_theme() +
  scale_color_manual(values = c("#F70020","#191A1A","#FB9701","#1A7D00","#072C8F","grey"))


# From "2020-06-5" - "2020-06-14"
Mall_Transaction2 <- subset(Mall_Transaction, Date <= "2020-06-14" & Date >= "2020-06-05")
Transformed <- Mall_Transaction2 %>% group_by(Store.ID) %>% transform(., day=match(Date, unique(Date)))

df.rankings <- Transformed %>%
  group_by(Date) %>%
  arrange(Date, desc(Transaction_conRate), Store.ID) %>%
  mutate(ranking = row_number()) %>% as.data.frame()

df.rankings$Store.ID <- as.character(df.rankings$Store.ID)

ggplot(data = df.rankings, aes(x = day, y = ranking, group = Store.ID)) +
  geom_line(aes(color = Store.ID, alpha = 1), size = 1.2) +
  geom_point(aes(color = Store.ID, alpha = 1), size = 1.5) +
  scale_y_reverse(breaks = 1:nrow(df.rankings))

# Top 10
show.top.n <- 10
ggplot(data = df.rankings, aes(x = day, y = ranking, group = Store.ID)) +
  geom_line(aes(color = Store.ID, alpha = 1), size = 2) +
  geom_point(aes(color = Store.ID, alpha = 1), size = 4) +
  geom_point(color = "#FFFFFF", size = 1) +
  scale_y_reverse(breaks = 1:show.top.n) +
  scale_x_continuous(breaks = 1:10, minor_breaks = 1:10, expand = c(.05, .05)) +
  geom_text(data = df.rankings %>% filter(day == "1"),
            aes(label = Store.ID, x = 0.5) , hjust = .85, fontface = "bold", color = "#888888", size = 4) +
  geom_text(data = df.rankings %>% filter(day == "5"),
            aes(label = Store.ID, x = 16.5) , hjust = 0.15, fontface = "bold", color = "#888888", size = 4) +
  coord_cartesian(ylim = c(1,show.top.n)) + 
  my_theme() 
# Highlight
df.rankings <- df.rankings %>%
  mutate(flag = ifelse(Store.ID %in% c("82","93","97"), TRUE, FALSE),
         country_col = if_else(flag == TRUE, Store.ID, "zzz"))

show.top.n <- 10
ggplot(data = df.rankings, aes(x = day, y = ranking, group = Store.ID)) +
  geom_line(aes(color = country_col, alpha = 1), size = 2) +
  geom_point(color = "#FFFFFF", size = 4) +
  geom_point(aes(color = country_col, alpha = 1), size = 4) +
  geom_point(color = "#FFFFFF", size = 1) +
  scale_y_reverse(breaks = 1:show.top.n) +
  scale_x_continuous(breaks = 1:5, minor_breaks = 1:5, expand = c(.05, .05)) +
  geom_text(data = df.rankings %>% filter(day == "1"),
            aes(label = Store.ID, x = 0.5) , hjust = .85, fontface = "bold", color = "#888888", size = 4) +
  geom_text(data = df.rankings %>% filter(day == "5"),
            aes(label = Store.ID, x = 16.5) , hjust = 0.15, fontface = "bold", color = "#888888", size = 4) +
  coord_cartesian(ylim = c(1,show.top.n)) + 
  theme(legend.position = "none") +
  my_theme() +
  scale_color_manual(values = c("#F70020","#191A1A","#FB9701","#1A7D00","#072C8F","grey"))

# From "2020-06-19" - "2020-06-28": Lam tuong tu 

# 5. Funnel for each Store----
Shopping_mall <- Saledata %>% filter(Store.Type == "SHOPPING_MALL")

Shopping_mall_F <- Shopping_mall %>% group_by(Store.ID) %>% summarise(Traffic = sum(Traffic), Interaction = sum(Interaction), Convert = sum(Transaction))
Shopping_mall_F

Shopping_mall_F <- gather(Shopping_mall_F, Condition, Measurement, Traffic:Convert, factor_key=TRUE) # convert wide to long

Shopping_mall_F <- Shopping_mall_F %>% filter(Store.ID == "90")
Shopping_mall_F

library(plotly)
fig <- plot_ly() 
fig <- fig %>%
  add_trace(type = "funnel",
            y = Shopping_mall_F$Condition,
            x = Shopping_mall_F$Measurement,
            textposition = "inside",
            textinfo = "value+percent initial",
            opacity = 0.65,
            marker = list(color = c("deepskyblue", "lightsalmon", "tan", "teal", "silver"),
                          line = list(width = c(4, 2, 2, 3, 1, 1), color = c("wheat", "wheat", "blue", "wheat", "wheat"))),
            connector = list(line = list(color = "royalblue", dash = "dot", width = 3)))
fig <- fig %>%
  layout(yaxis = list(categoryarray = Shopping_mall_F$Condition))
fig


# 6. Cua hang nao co ca ba chi so deu tot---- 
IND <- Saledata %>% filter(Store.Type == "INDEPENDENT")

library(tidyverse)
library(magrittr)

Shopping_mall_test <- IND %>% group_by(Store.ID) %>% summarise(sumTraffic = sum(Traffic),sumInteraction = sum(Interaction),sumTransaction = sum(Transaction),meanBasetSize = mean(Avg.Basket.Size))

Shopping_mall_test$Store.ID <- as.character(Shopping_mall_test$Store.ID)

require(knitr)
kable(head(Shopping_mall_test))
summary(Shopping_mall_test)

Shopping_mall_test <- na.omit(Shopping_mall_test)
summary(Shopping_mall_test)

dsData <- Shopping_mall_test

row.names(dsData) <- Shopping_mall_test$Store.ID
dsData <- scale(dsData[,2:5])
summary(dsData)

set.seed(123)
km.res <- kmeans(dsData, 5, nstart = 25)
km.res

a <- as.data.frame(km.res$centers)

length(km.res$cluster) #Coi so luong observation 
nrow(Shopping_mall_test)
Shopping_mall_test$clusterk <- as.factor(km.res$cluster)
kable(head(Shopping_mall_test))

library(writexl)
write_xlsx(x = Shopping_mall_test, path = "IND_Shopping_mall_test.xlsx",col_names = TRUE) # Xuat ra file excel 



