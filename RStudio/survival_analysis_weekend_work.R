# install.package(c("survival","survminer","ggplot2","muhaz"))
library("survival")
library("ggplot2")
library("survminer")
library("muhaz")

# read data
file_path1 <- "C:/Users/nickb/Documents/DataMining_RGA/Cox Regression/facebook/weekend work/data/dataset_for_r.csv"
df <- read.table(file_path1, sep=",", header=T)

# setup Surv object and fit cox regression ph model
fit <- coxph(Surv(time1, time2, status==1, type='counting')
             ~ weekend_binary, data=df)
summary(fit)

# create survival curves and plot them
km = survfit(Surv(time1, time2, status==1) ~ weekend_binary, data=df, conf.type="log-log")
ggsurvplot(fit=km,risk.table=T,censor=F,conf.int=T,conf.int.fill='blue',conf.int.style='step')

# check for time-varying coefficients
zp <- cox.zph(fit) # zp
plot(zp)


# plot the baseline smoothed hazard rate curve
file_path_h <- "C:/Users/nickb/Documents/DataMining_RGA/Cox Regression/facebook/weekend work/data/data_for_hazard_curve.csv"
df_h <- read.table(file_path_h, sep=",", header=T)
shc = with(df_h,muhaz(times=tenure_months, delta=status, bw.method='local',
                      bw.smooth=5))
plot(shc, lwd=2, xlab="Months")

# plot cumulative hazard rate curve
plot(km, fun='cumhaz', conf.int=T, col=c(2,4),
     ylab="Cumulative Hazard Rate", xlab="Months", xmax=45)
legend(x=0, y=1.1, c("No Weekend Work","Weekend Work"),
       col=c("red", "blue"),lty=1)

# plot "cloglog" to 
plot(km, fun="cloglog", conf.int=F, col=c(2,4))
legend(x=0.2, y=0, c("No Weekend Work","Weekend Work"),
       col=c("red", "blue"),lty=1)


# test if two groups have different survival curves
dif_fit <- survdiff(Surv(time1, time2, status==1, type='counting')
                 ~ weekend_binary, data=df)
