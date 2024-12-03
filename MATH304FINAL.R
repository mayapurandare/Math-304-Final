####read in the csv file
dat.cp=read.csv("CarPrice_Assignment2.csv")
ls()

###feature names 
print(names(dat.cp))


##forward selection
##create full and null 
install.packages("MASS")
library(MASS)
res.full=lm(PRICE~SYM+LUX+PREM+STND+GAS+ASP+X4DOOR+CONV+HATCH+SEDAN+WAGON+RWD+FWD+FRONT+
              WHEELBASE+LENGTH+WIDTH+HEIGHT+WEIGHT+DOHC+OHC+L+ROTOR+OHCF+OHCV+CYL+SIZE+
              CARB+ELEC+BORERATIO+STROKE+COMPRATIO+HP+RPM+CITYMPG+HWMPG, data = dat.cp)
res.null=lm(PRICE~1, data=dat.cp)

forward=step(res.null, scope = list(upper=res.full),
             direction="forward",
             test="F")

install.packages("MPV")
install.packages("olsrr")
install.packages("ggcorrplot")

#Best Model based on forward selection
res.best = lm(PRICE ~ SIZE + STND + RWD + FRONT + WIDTH + BORERATIO + RPM + 
                ASP + OHCV + STROKE + ROTOR + OHC + HATCH + CONV + HEIGHT + 
                LENGTH + WEIGHT + SEDAN + WHEELBASE + HWMPG + COMPRATIO + 
                GAS + LUX, data = dat.cp)

##PRESS
library(MPV)
PRESS(res.best) #1,334,924,553


#Mallows Cp
library(olsrr)
ols_mallows_cp(model=res.best, fullmodel=res.full) #20.91823

summary(res.best) #0.9197


library(MASS)
plot(res.best$fitted.values,resid(res.best),xlab="Fitted Values",ylab="Residuals")

#Multicollinearity
library(car)
vif(res.best)
##VIF > 5: WIDTH, WHEELBASE, HWMPG
##VIF > 10: SIZE, LENGTH, WEIGHT, COMPRATIO, GAS

##correlation matrix
corrdat = subset(dat.cp, select = c(SIZE, STND, RWD, FRONT, WIDTH, BORERATIO, RPM, 
                       ASP, OHCV, STROKE, ROTOR, OHC, HATCH, CONV, HEIGHT, 
                       LENGTH, WEIGHT, SEDAN, WHEELBASE, HWMPG, COMPRATIO, 
                       GAS, LUX))
cor_matrix <- cor(corrdat)
library(ggcorrplot)
ggcorrplot(cor_matrix, lab = TRUE)


# Residuals v Independent Variables
err = resid(res.best)
# WIDTH, WHEELBASE, HWMPG
plot(dat.cp$WIDTH,err,xlab="Width",ylab="Residuals")
plot(dat.cp$WHEELBASE,err,xlab="WheelBase",ylab="Residuals")
plot(dat.cp$HWMPG,err,xlab="Highway mpg",ylab="Residuals")

# WIDTH WHEELBASE and HWMPG
res2 = lm(PRICE ~ WHEELBASE + HWMPG + STND + RWD + FRONT + WIDTH + BORERATIO + RPM + 
                       ASP + OHCV + STROKE + ROTOR + OHC + HATCH + CONV + HEIGHT + 
                       + SEDAN + LUX, data = dat.cp)
summary(res2)
## RES 2 Adj R 0.8206


# Log transform high VIF features
data.cp = within(dat.cp, {
  lnSIZE = log(dat.cp$SIZE)
  lnLENGTH = log(dat.cp$LENGTH)
  lnWEIGHT = log(dat.cp$WEIGHT)
  lnCOMPRATIO = log(dat.cp$COMPRATIO)
})
res3 = lm(PRICE ~ lnSIZE + STND + RWD + FRONT + WIDTH + BORERATIO + RPM + 
                ASP + OHCV + STROKE + ROTOR + OHC + HATCH + CONV + HEIGHT + 
                lnLENGTH + lnWEIGHT + SEDAN + WHEELBASE + HWMPG + lnCOMPRATIO + 
                LUX, data = dat.cp)
summary(res3)
## RES 3 Adj R 0.8856
ols_mallows_cp(model=res3, fullmodel=res.full) #95.81694


# Removing all features with high VIF
res4 = lm(PRICE ~  STND + RWD + FRONT + BORERATIO + RPM + WIDTH +
                   ASP + OHCV + STROKE + ROTOR + OHC + HATCH + CONV + HEIGHT + 
                   + SEDAN + LUX, data = dat.cp)
summary(res4) #adj 0.808
PRESS(res4) # 2,826,311,341

## QQ PLOTS
student.err=rstandard(res.best) #studentized residual
rstudent=rstudent(res.best) #r-student
qqnorm(student.err)
qqline(student.err)
qqnorm(err)
qqline(err)

#find outliers
which(student.err>3)
which(rstudent>3)



#Find the influence measurements
###leverage
hatvalues(res.best)
round(hatvalues(res.best),3)
#If you have a big dataset, you can use which( ) to detect potential influence points
#for example, the cutoff for leverage is 24/205=0.117
#observations 18 and 27 have hat values greater than the cutoff
which(hatvalues(res.best)>0.117)
#check the studentized residuals for observations 18 and 27
#if their R-studnts are small, then they might not be influence points
round(rstudent(res.best),3)

###Cook's D
cooks.distance(res.best)
round(cooks.distance(res.best),3) #keep 3 decimal places, easier to check

### DFFITS
round(dffits(res.best),3)
2*sqrt(24/205)
### DFBETAS
df.beta=dfbetas(res.best)
round(df.beta,3)
2/sqrt(205)



### Calculate the cutoff values for the four measurements above and
#report potential influence (leverage) observations based on each measurement
which(abs(dffits(res.best))>0.6843)
which(abs(dfbetas(res.best))>0.1397, arr.ind=TRUE)
