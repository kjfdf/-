# read file
library(readxl)
library(dplyr)
library(tidyr)
library(survival)
install.packages("writexl")
library(writexl)
library(lubridate)
snu <- read_xlsx("snuh.xlsx")
dim(snu) #1220 observations
length(unique(snu$등록번호)) #468 subjects
snu$bulbar <- as.integer(ifelse(snu$speech<4|snu$salivation|snu$swallowing,1,0))
snu$cervical <- as.integer(ifelse(snu$handwriting<4|snu$cutting_withoutgast<4,1,0))
snu$lumbosacral <- as.integer(ifelse(snu$walking,1,0))
#King's system neither 4a or 4b
snu_fil$King <- snu_fil$bulbar+snu_fil$cervical+snu_fil$lumbosacral
snu$b_c_ls <- snu$bulbar+snu$cervical+snu$lumbosacral
snu$b_c_ls <- as.factor(snu$b_c_ls)
#King's system either 4a or 4b
snu$bulbar <- snu %>% filter(speech!=NA&salivation!=NA&swallowing!=NA)
snu$King <- snu$b_c_ls
snu$King <- ifelse(snu$respi_insuf<4,"4b",ifelse(snu$cutting_gast<=4,"4a",as.character(snu$b_c_ls)))
#King's system determine by excel and save result
write_xlsx(snu,"snuh1.xlsx")
snu1 <- read_xlsx("snuh1.xlsx")
snu1$swallowing = ifelse(snu1$swallowing <= 1, 1, 0)
snu1$breathing = ifelse(snu1$dyspnea<=1|snu1$respi_insuf <= 2, 1, 0)
snu1$communicating = ifelse(snu1$speech <= 1 & snu1$handwriting <= 1, 
                            1, 0)
snu1$movement = ifelse(snu1$dressing <= 1|snu1$walking <=1, 
                       1, 0)
snu1 = snu1 %>%
  mutate(MiToS = swallowing + breathing + communicating + movement)
table(snu1$King,snu1$MiToS)
snu <- read_excel("snuh2.xlsx")
stage <- snu %>%
  select(등록번호, 작성일자, ALSFRS_R, King, MiToS)
stage = stage[complete.cases(stage),]
dim(stage) #1212*5
length(unique(stage$등록번호)) #466 subjects
t <- table(stage$King,stage$MiToS) 
# Cross table King's(row), MiToS(column)
#      0   1   2   3   4 
# 0    1   1   1   0   0
# 1    0   2   0   0   0
# 2  247  54  17   4   1
# 3  398 308  54   5   0
# 4a  16  13   9   9   1
# 4b   5  15  25  13  13
King <- c(0,1,2,3,"4a","4b")
MiToS <- c(0   ,1   ,2,   3,   4)

# Create the data frame
df <- expand.grid(King, MiToS)
df$value <- c(1,1,1,0,0,0,2,0,0,0,247,54,17,4,1,398,308,54,5,0,16,13,9,9,1,5,15,25,13,13)
library(ggplot2)
#Plot the Data
g <- ggplot(df, aes(Var1, Var2)) + geom_point(aes(size = value), colour = "yellow") + theme_bw() + xlab("King's system") + ylab("MiToS system")+ggtitle("Comparison between King's and MiToS system")+scale_size_continuous(range=c(10,30)) + geom_text(aes(label = value))+theme(plot.title=element_text(family="TT times newroman",face="bold",size="30",hjust=0.5,color="darkblue"))
g
# stages at entry (King's vs. MiToS) - barplot, summary statistics 
snu$King <- as.factor(snu$King)
snu$MiToS <- as.factor(snu$MiToS)
range(snu$Date,na.rm=T)
snu$Date <- as.Date(snu$Date)
snu$Date1 <- as.Date(snu$Date1)
snu1 <- snu %>% 
  group_by(ID) %>% 
  mutate(interval=last(Date)-first(Date))
table(snu1$interval)
snu1$interval <- as.numeric(snu1$interval)
shapiro.test(snu1$interval)
summary(snu1$interval)
interval_plot <- ggplot(snu1,aes(x=interval))+
  geom_histogram(binwidth=1,color="black",fill="gray")+
  geom_vline(aes(xintercept=median(interval,na.rm=T)),color="black",linetype="dashed",size=1)+
  ggtitle("Histogram of intervals")+
  theme(plot.title=element_text(family="TT times newroman",face="bold",size="20",hjust=0.5,color="darkblue"))
interval_plot
snu_firstvisit <- snu %>% group_by(ID) %>% 
  filter(Date==min(Date)) %>% 
  summarise(first=first(Date),
            ALSFRS_R,
            King,
            MiToS)
summary(snu_firstvisit)
snu_firstvisitstage <- snu_firstvisit %>% group_by(King,MiToS) %>% summarise(n=n())
ggplot(snu_firstvisitstage,aes(King,MiToS,fill=MiToS))+geom_bar(stat='identity',position='dodge')
# fu duration (time from first to last alsfrs scores)  - histogram, summary statistics 
snu1 <- snu %>% group_by(ID) %>% mutate(interval=last(Date1)-first(Date1))
snu_fualsfrs <- snu %>% group_by(ID) %>% 
  filter(Date%in%c(min(Date),max(Date))) %>% 
  summarise(ALSFRS_R,
            King,
            MiToS,
            Date)
summary(snu_fualsfrs)
snu_fualsfrs_first <- snu %>% group_by(ID) %>% 
  filter(Date==min(Date)) %>% 
  summarise(ALSFRS_R,
            King,
            MiToS,
            Date) 
snu_fualsfrs_first$group <- as.factor("first visit")
summary(snu_fualsfrs_first)
snu_fualsfrs_last <- snu %>% group_by(ID) %>% 
  filter(Date==max(Date)) %>% 
  summarise(ALSFRS_R,
            King,
            MiToS,
            Date)
snu_fualsfrs_last$group <- as.factor("last visit")
summary(snu_fualsfrs_last)
snu_fualsfrsgroup <- rbind(snu_fualsfrs_first,snu_fualsfrs_last)
library(plyr)
mu <- ddply(snu_fualsfrsgroup,"group",summarise,grp.mean=mean(ALSFRS_R))
ggplot(snu_fualsfrsgroup,aes(x=ALSFRS_R,fill=group,color=group))+
  geom_histogram(aes(y=..density..),alpha=0.5,position="identity")+
  geom_density(alpha=0.3)+
  geom_vline(data=mu, aes(xintercept=grp.mean,color=group),
             linetype="dashed",size=1)+
  theme(legend.position="top")+
  theme_minimal()+
  ggtitle("Distribution of ALSFRS-R at first and last visits")
# progression of alsfrs total scores over time - line plot, linear regression - comparisons between groups categorized based on the entry stage   
King_ <- list(0,2,3,"4a","4b")
MiToS_ <- list(0,1,2,3,4)
for(i in 1:5){
  f <- as.formula(paste0)
}
snu_King_1 <- snu1 %>% group_by(ALSFRS_R) %>% 
  filter(King==1)
table(is.na(snu_King_1))
result <- lm(ALSFRS_R~interval, data=snu_King_1,na.rm=T)
ggplot(snu1,aes(interval,ALSFRS_R))+geom_line(aes(color=King))
 summary(result)
par(mfrow=c(2,2))
plot(result)
install.packages("fmsb")
library(fmsb)
VIF(result) #다중공선성 확인. VIF>5 면 다중공선성 존재, 10보다 크면 심한 다중공선성
sqrt(VIF(result))
influencePlot(fit,id.method="identify")
# KM curves for time to any progression in stage (King's vs. MiToS) 
# KM curves for time to >= 2 stage progression (King's vs. MiToS) 
# Concordance between King's and MiToS stage progression (>= 1 stage, >= 2 stage) 