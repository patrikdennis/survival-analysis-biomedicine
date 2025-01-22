library(tidyverse)
library(GGally)
library(survival) #For survfit
library(survminer) #For ggsurvplot



dt <- read.csv(file="whas500.csv",sep=";")

colnames(dt)

#Format GENDER variable
dt$GENDER <- factor(dt$GENDER,levels=c(0,1),labels = c("Male","Female"))

#Distribution of LENFOL

ggplot(dt[dt$FSTAT==1,],aes(x=LENFOL))+geom_histogram(col="white")

#Q: overlay a distribution
ggplot(dt[dt$FSTAT==1,],aes(x=LENFOL))+geom_histogram(col="white",aes(y=..density..))+
  geom_density(col="green")

#Cumulative distribution of LENFOL
ggplot(dt[dt$FSTAT==1,],aes(x=LENFOL))+stat_ecdf()
#Q: calculate Survival as S=1-F

P <- ecdf(x=dt$LENFOL[dt$FSTAT==1])

#CDF IS DYING PRIOR TO A CERTAIN DAY SO 1-CDF SAYS DYING BEYOND A CERTAIN TIME
#PERIOD
1-P(200)


#Can look at the hazard function which is the death rate,
#The death rate will be constant which implies memoryless.
#Life lengths of individual that do not remember the age 
#slope gives that parameter of the exp distribution.

#Survival is S(T) = P(T>x) dying beyond the certain time. 
(1-P(1000))




#plot survival rate
data1 = dt$LENFOL[dt$FSTAT==1]
cdf = ecdf(data1)
s_emp <- 1-cdf(sort(data1))
plot(x=sort(data1), s_emp, type="l", xlab='Time (t)', ylab='S(t)')

cdf = ecdf(data1)
cdf(200)
1-(1-P(1000))
1-P(1000)
s_emp <- 1-cdf(1:length(data1))
#plot survival rate
s_emp <- 1-cdf(sort(data1))
1-s_emp(1000)
s_emp(1000)

plot(x=sort(data1), s_emp, type="l", xlab='Time (t)', ylab='S(t)')



#Summary statistics
dt %>% summarise(N=n(),Mean=mean(LENFOL),SD=sd(LENFOL),Min=min(LENFOL),Max=max(LENFOL))
#Q: summarise other variables

dt %>% summarise(N=n(),Mean=mean(AGE),SD=sd(AGE),Min=min(AGE),Max=max(AGE))

dt %>% summarise(N=n(),Mean=mean(BMI),SD=sd(BMI),Min=min(BMI),Max=max(BMI))

dt %>% summarise(N=n(),Mean=mean(HR),SD=sd(HR),Min=min(HR),Max=max(HR))

summary(dt)

cor(dt[,c("LENFOL","AGE","BMI","HR")])

dt_died <- dt[dt$FSTAT==1,]

ggpairs(dt_died[,c("LENFOL","GENDER","AGE","BMI","HR")])

dt_survived <- dt[dt$FSTAT==0,]

ggpairs(dt_survived[,c("LENFOL","GENDER","AGE","BMI","HR")])

ggpairs(dt[,c("LENFOL","GENDER","AGE","BMI","HR")])

#Q: interpret cor and ggpairs results


#Survival analysis
surv.data <- with(dt,Surv(LENFOL, FSTAT))
fit1 <- survfit(surv.data~1,data=dt) #Median CI based on var(log(S(t)))
survdiff()


plot(fit1)
abline(h=0.5, col='green')
abline(v=1627,col='purple')
fit1

#see that the median value 1627 has a higher than 50% and therefore cannot compute for median.
KM.plot.1 <- ggsurvplot(fit1,
                        surv.scale="percent",
                        risk.table = FALSE,
                        conf.int=TRUE,
                        surv.median.line = "h",
                        break.time.by=100,
                        font.tickslab=10,
                        font.x=12,
                        font.y=12,
                        xlab = "Time (days)",
                        ylab = "Percent survival"
                       ) 

KM.plot.1$plot + theme(axis.text.x = element_text(angle=90))


print(KM.plot.1)

fit2 <- survfit(surv.data~1,data=dt,conf.type="log-log") #Median CI based on var(log(-log(S(t))))
fit1
fit2


KM.plot.1 <- ggsurvplot(fit,
                        surv.scale="percent",
                        risk.table = FALSE,
                        conf.int=TRUE,
                        surv.median.line = "h",
                        break.time.by=100,
                        font.tickslab=10,
                        font.x=12,
                        font.y=12,
                        xlab = "Time (days)",
                        ylab = "Percent survival") 

KM.plot.1$plot + theme(axis.text.x = element_text(angle=90))

#Q: Create manually a table with
#Time, number at risk, number of observed events, number of censored events,
#survival, failure, survival standard error, 95% CI for survival,
#number of failed subjects and number of left subjects

fit <- survfit(surv.data~1,data=dt)

d <- data.frame(time = fit1$time,
                n.risk = fit1$n.risk,
                n.event = fit1$n.event,
                n.censor = fit1$n.censor,
                surv = fit1$surv,
                failure = 1-fit1$surv,
                se = fit1$std.err,
                upper = fit1$upper,
                lower = fit1$lower)

d$n.failed <- cumsum(fit1$n.event)
d$n.left <- fit1$n - d$n.failed

#Plot survival curve
ggsurvplot(fit,
           risk.table = TRUE,
           surv.median.line = "hv",
           ggtheme = theme_bw())

#Plot cumulative hazard curve
ggsurvplot(fit,
           fun = "cumhaz",
           risk.table = TRUE,
           ggtheme = theme_bw())

#Survival time quartiles
probs <- c(0.25, 0.5, 0.75)
surv.qs <- quantile(fit1, probs = probs, conf.int = TRUE)
data.frame(Percet=probs,Estimate=surv.qs$quantile,Lower=surv.qs$lower,Upper=surv.qs$upper)
surv_median(fit1)
survmean

#Repeat analysis by GENDER
fit2 <- survfit(surv.data~GENDER,data=dt)
fit2
summary(fit2)

ggsurvplot(fit2 ,
           risk.table = TRUE ,
           surv.median.line = "hv",
           ggtheme = theme_bw())

ggsurvplot(fit2,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"))

ggsurvplot(fit2,
           linetype = "strata",
           fun = "cumhaz",
           risk.table = TRUE,
           ggtheme = theme_bw(),
           palette = c("#E7B800", "#2E9FDF"))

ggsurvplot(
  fit2,                     # survfit object with calculated statistics.
  pval = TRUE,             # show p-value of log-rank test.
  conf.int = TRUE,         # show confidence intervals for 
  # point estimaes of survival curves.
  conf.int.style = "step",  # customize style of confidence intervals
  xlab = "Time in days",   # customize X axis label.
  break.time.by = 200,     # break X axis in time intervals by 200.
  ggtheme = theme_light(), # customize plot and risk table with a theme.
  risk.table = "abs_pct",  # absolute number and percentage at risk.
  risk.table.y.text.col = T,# colour risk table text annotations.
  risk.table.y.text = FALSE,# show bars instead of names in text annotations
  # in legend of risk table.
  ncensor.plot = TRUE,      # plot the number of censored subjects at time t
  surv.median.line = "hv",  # add the median survival pointer.
  legend.labs = 
    c("Male", "Female"),    # change legend labels.
  palette = 
    c("#E7B800", "#2E9FDF") # custom color palettes.
)

surv.test <- survdiff(surv.data~GENDER,data=dt) #Mantel-Haenszel test
surv.test
surv.test <- survdiff(surv.data~GENDER,data=dt,rho=1) #Peto&Peto mod
surv.test

#Synthetic example
pll <- function(beta){
  psi <- exp(beta)
  result <- log(psi)-log(3*psi+3)-log(3*psi+1)-log(2*psi+1)
  result
}

ll <- function(beta){
  psi <- exp(beta)
  result.ll <- psi/((3*psi+3)(3*psi+1)*(2*psi+1))
  result.ll
}

op.pll <- optim(par=0,fn=pll,method="L-BFGS-B",
                control=list(fnscale=-1),
                lower=-3, upper=1)

op.ll <- optim(par=0,fn=ll,method="L-BFGS-B",
               control=list(fnscale=-1),
               lower=-3, upper=1)

op.pll$par


pl <- data.frame(beta=seq(-4,1,by=0.1))
pl$lpl <- pll(pl$beta)

ggplot(data=pl,aes(x=beta,y=lpl))+geom_line()+
  geom_vline(xintercept = op.pll$par,linetype="dashed")+
  labs(y="log Partial Likelihood")

pll.data <- data.frame(Patient=1:6,
                       Survtime=c(6,7,10,15,19,25),
                       Censor=c(1,0,1,1,0,1),
                       Group=c("C","C","T","C","T","T"))

result <- coxph(Surv(Survtime,Censor)~Group,data=pll.data)
result
summary(result)



#Repeat analysis of whas500 by GENDER and AGE (Cox regression)
fit3 <- coxph(surv.data~GENDER+AGE,data=dt)
summary(fit3)
surv.fit <- survfit(fit3, data=dt)

ggsurvplot(surv.fit,
           risk.table = TRUE, # Add risk table
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw() # Change ggplot2 theme
)
#Q: compare to Surv(LENFOL, FSTAT)~1



#Q: Plot survival curves by GENDER for average age

sex.dt <- with(dt,
               data.frame(GENDER = c("Male", "Female"), 
                          AGE = rep(mean(AGE, na.rm = TRUE), 2)
               )
)
sex.dt

surv.fit.gender <- survfit(fit3, data=dt, newdata = sex.dt)
ggsurvplot(surv.fit.gender, conf.int = TRUE, legend.labs=c("Male", "Female"),
           ggtheme = theme_minimal())


AIC(fit3)

#*
#* Measuring Aikake score for taking interaction terms into considerartion.
#* 

fit4 <- coxph(surv.data~GENDER+AGE+GENDER:AGE, data=dt)
AIC(fit4)

fit5 <- coxph(surv.data ~ HR+AGE+HR:AGE, data=dt)
AIC(fit5)

fit6 <- coxph(surv.data ~ BMI + HR, data = dt)
AIC(fit6)

fit5 <- coxph(surv.data ~ AGE+BMI+HR, data=dt)
AIC(fit5)

fit6 <- coxph(surv.data ~ AGE + BMI + BMI:AGE, data=dt)
AIC(fit6)

fit6 <- coxph(surv.data ~ BMI+GENDER+HR+(GENDER:HR)+(GENDER:BMI)+(HR:BMI), data=dt)
AIC(fit6)

fit7 <- coxph(surv.data ~ BMI+GENDER+AGE+HR+BMI:GENDER+BMI:AGE+HR:GENDER+HR:AGE+AGE:GENDER, data=dt)
AIC(fit7)

fit8 <- coxph(surv.data ~ LENFOL , data=dt)

AIC(fit8)




