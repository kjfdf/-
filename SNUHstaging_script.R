# read file
library(readxl)
install.packages("extrafont")
library(extrafont)
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
write_xlsx(snu,"snuh-.xlsx")
snu1 <- read_xlsx("snuh2.xlsx")
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
# fu duration (time from first to last alsfrs scores)  - histogram, summary statistics 
snu$Date <- as.Date(snu$Date)
snu$Date1 <- as.Date(snu$Date1)
snu1 <- snu %>% 
  group_by(ID) %>% 
  mutate(interval=Date1-first(Date1))
write_xlsx(snu1,"snuh_interval.xlsx")
table(snu1$interval)
snu1$interval <- as.numeric(snu1$interval)
shapiro.test(snu1$interval)
summary(snu1$interval)
interval_plot <- ggplot(snu1,aes(x=interval))+
  geom_histogram(color="black",fill="gray")+
  geom_vline(aes(xintercept=median(interval,na.rm=T)),color="blue",linetype="dashed",size=1)+
  ggtitle("Histogram of intervals")+
  theme(plot.title=element_text(family="TT times newroman",face="bold",size="10",hjust=0.5,color="darkblue"))+
  coord_cartesian(xlim = c(0,max(snu1$interval)+10),ylim = c(0,500))+
  annotate("text", x=200, y=500, label="median=91", size=5,color="blue")
interval_plot
# progression of alsfrs total scores over time - line plot, linear regression - comparisons between groups categorized based on the entry stage   
snu3$MiToS_entry <- as.factor((snu3$MiToS_entry))
snu3$King_entry <- as.factor((snu3$King_entry))
snu2 <- snu1 %>% group_by(ID) %>% filter(Date1==min(Date1))
snu2 <- snu2 %>% mutate(King_entry=King,MiToS_entry=MiToS)
snu2 <- snu2 %>% select(ID,King_entry,MiToS_entry)
snu3 <- merge(x=snu1,y=snu2,by="ID",all.x=T)
write_xlsx(snu3,"snuh_entrystage.xlsx")
# initial King stage에 따른 ALSFRS 변화 plot
ggplot(snu3,aes(interval,ALSFRS_R))+geom_jitter(aes(color=King_entry))+geom_smooth(method=lm,aes(color=King_entry,fill=King_entry))+ggtitle("ALSFRS-R trajectory based on initial King's staging")+theme(plot.title = element_text(face="bold",size=15,hjust=0.5))
# initial MiToS stage에 따른 ALSFRS 변화 plot
ggplot(data=subset(snu3,!is.na(MiToS_entry)),aes(interval,ALSFRS_R))+geom_jitter(aes(color=MiToS_entry))+geom_smooth(method=lm,aes(color=MiToS_entry,fill=MiToS_entry))+ggtitle("ALSFRS-R trajectory based on initial MiToS staging")+theme(plot.title = element_text(face="bold",size=15,hjust=0.5)) #116 elements of NA removed
# snu1$stage <- paste(snu$King,snu$MiToS)
# ggplot(data=subset(snu1,!is.na(MiToS)),aes(interval,ALSFRS_R))+geom_jitter(aes(color=stage))+geom_smooth(method=lm,aes(color=stage,fill=stage))+ggtitle("ALSFRS-R trajectory based on MiToS staging system")+theme(plot.title = element_text(face="bold",size=15,hjust=0.5)) #116 elements of NA removed
# ggplot(data=subset(snu1,!is.na(MiToS)),aes(interval,ALSFRS_R,color=King,shape=MiToS,group=interaction(King,MiToS)))+geom_jitter()+geom_smooth(method=lm)+ggtitle("ALSFRS-R trajectory based on MiToS staging system")+theme(plot.title = element_text(face="bold",size=15,hjust=0.5)) #116 elements of NA removed
# KM curves for time to any progression in stage (King's vs. MiToS) 
snu3$King <- as.numeric(snu3$King)
snu3$MiToS <- as.numeric(snu3$MiToS)
snu4 <- snu3 %>% group_by(ID) %>% 
  mutate(prog_K=King-lag(King,default=King[1]),
         prog_M=MiToS-lag(MiToS,default=MiToS[1]),
         prog_K1=ifelse(prog_K==1,1,0),
         prog_M1=ifelse(prog_M==1,1,0),
         prog_K2=ifelse(prog_K==2,1,0),
         prog_M2=ifelse(prog_M==2,1,0))
snu4$King <- as.factor(snu4$King)
snu4$MiToS <- as.factor(snu4$MiToS)
snu4$King <- ifelse(snu4$King==5,"4a",ifelse(snu4$King==6,"4b",snu4$King))
write_xlsx(snu4,"snuh_progress.xlsx")
library(survival)
install.packages("survminer")
library(survminer)
snu4$ts <- Surv(snu4$interval,snu4$prog_K1==1) #1이면 King stage가 1이상 증가, interval은 entry부터 f/u까지의 duration
fit <- survfit(ts~King_entry,data=snu4)
ggsurvplot(fit, legend.title="King's staging at entry",legend.labs=c("King's stage 0","King's stage 2","King's stage 3","King's stage 4a","King's stage 4b"),
           conf.int=T,pval=T,surv.median.line="hv",risk.table=T,fun="event")
# KM curves for time to >= 2 stage progression (King's vs. MiToS) 
# Concordance between King's and MiToS stage progression (>= 1 stage, >= 2 stage) 