library("survival")
library("ggplot2")
library("survminer")

file_path = "C:/Users/nickb/Documents/Facebook/roster_only.csv"
df = read.table(file_path, sep=",", header=T, stringsAsFactors=T)
df <- df[which(df$JOB_LEVEL2 %in% c('Junior','Senior')),]
# df <- df[which(df$GROUPING %in% c('JUNIOR_NOTCLOSE','SENIOR_CLOSE')),]


km = survfit(Surv(TENURE, CENSOR) ~ JOB_LEVEL2, data=df, conf.type="log-log")
ggsurvplot(fit=km,risk.table=T,censor=F,conf.int=F,conf.int.fill='blue',
           conf.int.style='step', palette=c('#ff3232','#3232ff'))

# 
dif <- survdiff(Surv(TENURE, CENSOR) ~ JOB_LEVEL2, data=df)
dif
