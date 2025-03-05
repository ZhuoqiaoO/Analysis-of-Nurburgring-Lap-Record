install.packages("mice")
install.packages("VIM")
install.packages("DescTools")
library('DescTools')
library('mice')
library('VIM')
library(MASS)
library(car)
dt <- LP0
dt[30,7] <- c('UK')
summary(dt)
table(dt$Driver)
table(dt$Manufacturer)
table(dt$Model)
table(dt$Country.of.Origin)
table(dt$Type)
table(dt$Version)
table(dt$Transmission)
table(dt$Layout)
table(dt$Engine.Position)
mice_plot <- aggr(dt, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(dt), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))
sum(apply(dt, 1, function(row) any(is.na(row))))

#Processing missing values in Version variable
summary(lm(dt$Version~dt$Year.Tested))

#Dataset After Second Attempt of Data Collection: Complete Dataset for Analysis ----------------------------------------------------------
dt <- LP1
summary(dt)
dt <- dt[-c(20,51),]
year_tested <- dt$Year.Tested
min <- dt$Lap.Time.Minutes
sec <- dt$Lap.Time.Seconds
lp <- min*60 + sec
driver <- dt$Driver
brand <- dt$Manufacturer
model <- dt$Model
coo <- dt$Country.of.Origin
type <- dt$Type
ver <- dt$Version
#the following are variables for fitting linear model
tm <- dt$Transmission
ep <- dt$Engine.Position
layout <- dt$Layout
hp <- dt$Horse.Power
tq <- dt$Torque
wt <- dt$Kerb.Weight
ec <- dt$Effective.Capacity
cyl <- dt$Cylinders
acc <- dt$Acceleration
mpg <- dt$Gas.Mileage
co2 <- dt$CO2.Emmission
tp <- dt$Maximum.Speed

#Part1. Fitting Multiple Linear Regression to the variables tm, ep, hp, tq, wt, ec, cyl, acc, mpg, co2, tp, layout--------------------------------

#Step1. Explore the Predicting Variables
tm <- as.factor(tm)
layout <- as.factor(layout)
ep <- as.factor(ep)

boxplot(hp,tq,wt,ec,tpmain = "Boxplot by Variables",
        xlab = "Variables",
        ylab = "Value",
        names = c("HorsePower", "Torque", "Weight","Efficient Capacity"))

boxplot(cyl,acc,mpg,co2,tpmain = "Boxplot by Variables",
        xlab = "Variables",
        ylab = "Value",
        names = c("Cylinders","Acceleration","MPG","CO2 Emission"))
boxplot(acc)

lm_hp <- lm(lp~hp)
plot(hp,lp)
plot(lm_hp$fitted.values,lm_hp$residuals)
plot(lm_hp)

lm_tq <- lm(lp~tq)
plot(tq,lp)
plot(lm_tq$fitted.values,lm_tq$residuals)
plot(lm(lp~trans_tq))
plot(trans_tq,lp)
summary(lm_tq)$r.squared
summary(lm(lp~trans_tq))$r.squared

lm_wt <- lm(lp~wt)
plot(wt,lp)
plot(lm_wt$fitted.values,lm_wt$residuals)
plot(lm_wt)
trans_wt <- wt^(-3)
plot(lm(lp~trans_wt))
summary(lm_wt)$r.squared
summary(lm(lp~trans_wt))$r.squared

lm_ec <- lm(lp~ec)
plot(ec,lp)
plot(lm_ec$fitted.values,lm_ec$residuals)
plot(lm_ec)
trans_ec <- ec^(-1/2)
plot(trans_ec,lp)
lm_ec1 <- lm(lp~trans_ec)
plot(lm_ec1$fitted.values,lm_ec1$residuals)
plot(lm(lp~trans_ec))

lm_cyl <- lm(lp~cyl)
plot(cyl,lp)
plot(lm_cyl$fitted.values,lm_cyl$residuals)
trans_cyl <- log(cyl)
plot(trans_cyl,lp)
lm_cyl1 <- lm(lp~trans_cyl)
plot(lm_cyl1$fitted.values,lm_cyl1$residuals)

lm_acc <- lm(lp~acc)
plot(acc,lp)
plot(lm_acc$fitted.values,lm_acc$residuals)

lm_tp <- lm(lp~tp)
boxplot(tp)
plot(tp,lp)
plot(lm_tp$fitted.values,lm_tp$residuals)
plot(lm_tp)

lm_mpg <- lm(lp~mpg)
boxplot(mpg)
plot(mpg,lp)
plot(lm_mpg$fitted.values,lm_mpg$residuals)
plot(lm_mpg)

lm_co2 <- lm(lp~co2)
boxplot(co2)
plot(co2,lp)
plot(lm_co2$fitted.values,lm_co2$residuals)
plot(lm_co2)
trans_co2 <- co2^(-2)
plot(trans_co2,lp)
lm_co3 <- lm(lp~trans_co2)
plot(lm_co3$fitted.values,lm_co3$residuals)
plot(lm_co3)

#Step2.Linear Transformations 
trans_ec <- ec^(-1.5)
plot(trans_ec,lp)
cor.test(trans_ec,lp)

trans_tq <- tq^(-2)
plot(trans_tq,lp)
cor.test(trans_tq,lp)

trans_co2 <- co2^(-1.5)
plot(trans_co2,lp)
cor.test(trans_co2,lp)
cor(co2,lp,use="complete")

trans_cyl <- cyl^(-1)
plot(trans_cyl,lp)
cor.test(trans_cyl,lp)
cor(cyl,lp,use="complete")

trans_wt <- wt^(-3)
plot(trans_wt,lp)
cor.test(trans_wt,lp)
cor(wt,lp,use="complete")

#Step 3. Multicolinearity Problem
m <- data.frame(lp,hp,trans_tq,trans_wt,trans_ec,trans_cyl,mpg,trans_co2,tp,acc)
cor(m,use="complete")
model <- lm(lp~hp+trans_tq+trans_wt+trans_ec+trans_cyl+acc+tp+mpg+trans_co2+tm+layout+ep)
vif(model)

combined_var <- as.numeric(hp+trans_tq+trans_ec+trans_cyl+trans_co2)
model_new <- lm(lp~combined_var+tp+mpg+tm+layout+ep)
vif(model_new)

#Step 4. fitting linear model
summary(model_new)
plot(model_new$fitted.values,model_new$residuals,xlab=c('fitted values'),ylab=c('residuals'))
shapiro.test(model_new$residuals)
plot(model_new)
model_new$coefficients

#Reduce Number of Predictors
data <- data.frame(lp,combined_var,tp,mpg,tm,layout,ep)
data <- na.omit(data)
model1 <- lm(lp~.,data=data)
step_model <- step(model1,direction=c('backward'))
summary(step_model)
vif(step_model)

#Part2. One Way ANOVA ---------------------------------------------------------------------------------------------------------------------------
#Step 1. Forming Dataset
fourwd_dt <- subset(dt, Layout == "4WD")
awd_dt <- subset(dt, Layout == "AWD")
fwd_dt <- subset(dt, Layout == "FWD")
rwd_dt <- subset(dt, Layout == "RWD")
lp_fourwd <- fourwd_dt$Lap.Time.Minutes+fourwd_dt$Lap.Time.Seconds/60
lp_awd <- awd_dt$Lap.Time.Minutes+awd_dt$Lap.Time.Seconds/60
lp_fwd <- fwd_dt$Lap.Time.Minutes+fwd_dt$Lap.Time.Seconds/60
lp_rwd <- rwd_dt$Lap.Time.Minutes+rwd_dt$Lap.Time.Seconds/60
boxplot(lp_fourwd,lp_awd,lp_fwd,lp_rwd,
        names = c("4 Wheel Drive","All Wheel Drive","Forward Wheel Drive","Rear Wheel Drive"),
        ylab =c('Lap Time in Minutes'))
layout <- rep(c("4WD","AWD","FWD","RWD"),times = c(length(lp_fourwd),length(lp_awd),length(lp_fwd),length(lp_rwd)))
laptime <- c(lp_fourwd,lp_awd,lp_fwd,lp_rwd)
layout <- as.factor(layout)
lpset <- data.frame(layout,laptime)

#Step 2. Testing Equal Variance and Normality Assumptions
anova_model <- aov(laptime~layout,data=lpset)
plot(anova_model)
leveneTest(laptime ~ layout, data = lpset,center=mean)
bartlett.test(laptime ~ layout, data = lpset)

lpset_log <- data.frame(layout,log(laptime))
colnames(lpset_log) <- c('layout','laptime')
leveneTest(laptime ~ layout, data = lpset_log,center=mean)
bartlett.test(laptime ~ layout, data = lpset_log)

lpset_sqrt <- data.frame(layout,sqrt(laptime))
colnames(lpset_sqrt) <- c('layout','laptime')
leveneTest(laptime ~ layout, data = lpset_sqrt,center=mean)
bartlett.test(laptime ~ layout, data = lpset_sqrt)

shapiro.test(anova_model$residuals)

#Analyzing ANOVA Results
summary(anova_model)
TukeyHSD(anova_model)
pairwise.t.test(laptime, layout, p.adjust.method = "bonferroni")
ScheffeTest(anova_model)

#in the case of unequal variance
weight <- 1/fitted(anova_model)^2
wls_model <- lm(laptime~layout, weights = weight)
anova(wls_model)
