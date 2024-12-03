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


## RES 5 uses types of engines
res5 = lm(PRICE ~ SIZE + FRONT + BORERATIO + RPM + ASP + STROKE + 
            SEDAN + HATCH + CONV + WAGON + WEIGHT, data = dat.cp)
summary(res5)
vif(res5)
plot(res.best$fitted.values,resid(res5),xlab="Fitted Values",ylab="Residuals")


## RES 6 removes engine types and weight
res6 = lm(PRICE ~ SIZE + FRONT + BORERATIO + RPM + ASP + STROKE, data = dat.cp)
summary(res6)
vif(res6)
PRESS(res6)
ols_mallows_cp(model=res6, fullmodel=res.full)

# DO PRESS AND MALLOWS CP FOR WRITE UP

# Summary Table
columns_to_summarize = c("SIZE", "FRONT", "BORERATIO", "RPM", "ASP", 
                         "STROKE", "PRICE")

# Summary table
summary_table <- data.frame(
  Mean = sapply(dat.cp[columns_to_summarize], mean, na.rm = TRUE),
  StdDev = sapply(dat.cp[columns_to_summarize], sd, na.rm = TRUE),
  Min = sapply(dat.cp[columns_to_summarize], min, na.rm = TRUE),
  Max = sapply(dat.cp[columns_to_summarize], max, na.rm = TRUE)
)

print(summary_table)


# Histograms of Variables
hist(dat.cp$SIZE,
     main = "Histogram of Engine Size",  # Title of the plot
     xlab = "SIZE",              # Label for x-axis
     col = "skyblue",                # Color of bars
     border = "black")

hist(dat.cp$FRONT,
     main = "Histogram of Engine Location",  # Title of the plot
     xlab = "FRONT",              # Label for x-axis
     col = "skyblue",                # Color of bars
     border = "black")  

hist(dat.cp$BORERATIO,
     main = "Histogram of Bore Ratio",  # Title of the plot
     xlab = "BORERATIO",              # Label for x-axis
     col = "skyblue",                # Color of bars
     border = "black")  

hist(dat.cp$RPM,
     main = "Histogram of RPM",  # Title of the plot
     xlab = "RPM",              # Label for x-axis
     col = "skyblue",                # Color of bars
     border = "black")  

hist(dat.cp$ASP,
     main = "Histogram of Aspiration Type",  # Title of the plot
     xlab = "ASP",              # Label for x-axis
     col = "skyblue",                # Color of bars
     border = "black")

hist(dat.cp$STROKE,
     main = "Histogram of Stroke Length",  # Title of the plot
     xlab = "STROKE",              # Label for x-axis
     col = "skyblue",                # Color of bars
     border = "black")

hist(dat.cp$PRICE,
     main = "Histogram of Price",  # Title of the plot
     xlab = "PRICE",              # Label for x-axis
     col = "skyblue",                # Color of bars
     border = "black")

# Residuals v Independent Variables
err = resid(res6)
plot(dat.cp$SIZE,err,xlab="Engine Size",ylab="Residuals")
plot(dat.cp$FRONT,err,xlab="Engine Location",ylab="Residuals")
plot(dat.cp$BORERATIO,err,xlab="Bore Ratio",ylab="Residuals")
plot(dat.cp$RPM,err,xlab="Car Peak RPM",ylab="Residuals")
plot(dat.cp$ASP,err,xlab="Standard or Turbo Engine",ylab="Residuals")
plot(dat.cp$STROKE,err,xlab="Stroke",ylab="Residuals")

library(MASS)
# Residual v Fitted Plot
plot(res6$fitted.values,resid(res6),xlab="Fitted Values",ylab="Residuals")


## QQ PLOTS
student.err=rstandard(res6) #studentized residual
rstudent=rstudent(res6) #r-student
qqnorm(student.err)
qqline(student.err)
qqnorm(err)
qqline(err)


# RES 7 LOG TRANSFORM PRICE
data.cp = within(dat.cp, {
  lnPRICE = log(PRICE)
  lnSIZE = log(dat.cp$SIZE)
  lnBORERATIO = log(dat.cp$BORERATIO)
  lnRPM = log(dat.cp$RPM)
  lnSTROKE = log(dat.cp$STROKE)
})
res7 = lm(lnPRICE ~ lnSIZE + FRONT + lnBORERATIO + lnRPM + 
            ASP + lnSTROKE, data = dat.cp)
summary(res7)

plot(res7$fitted.values,resid(res7),xlab="Fitted Values",ylab="Residuals")
vif(res7)
PRESS(res7)
ols_mallows_cp(model=res7, fullmodel=res.full)

## QQ PLOTS
student.err=rstandard(res7) #studentized residual
rstudent=rstudent(res7) #r-student
qqnorm(student.err)
qqline(student.err)
qqnorm(err)
qqline(err)


#find outliers
which(student.err>3)
which(rstudent>3)



#Find the influence measurements
###leverage
hatvalues(res7)
round(hatvalues(res7),3)
#If you have a big dataset, you can use which( ) to detect potential influence points
#for example, the cutoff for leverage is 24/205=0.117
#observations 18 and 27 have hat values greater than the cutoff
which(hatvalues(res7)>0.03414)
#check the studentized residuals for observations 18 and 27
#if their R-studnts are small, then they might not be influence points
round(rstudent(res7),3)

###Cook's D
cooks.distance(res7)
round(cooks.distance(res7),3) #keep 3 decimal places, easier to check

### DFFITS
round(dffits(res7),3)
2*sqrt(7/205)
### DFBETAS
df.beta=dfbetas(res7)
round(df7,3)
2/sqrt(205)



### Calculate the cutoff values for the four measurements above and
#report potential influence (leverage) observations based on each measurement
which(abs(dffits(res7))>0.36957)
which(abs(dfbetas(res7))>0.1397, arr.ind=TRUE)
