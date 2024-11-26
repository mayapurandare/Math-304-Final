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

#Best Model based on forwrad selection
res.best = lm(PRICE ~ SIZE + STND + RWD + FRONT + WIDTH + BORERATIO + RPM + 
                ASP + OHCV + STROKE + ROTOR + OHC + HATCH + CONV + HEIGHT + 
                LENGTH + WEIGHT + SEDAN + WHEELBASE + HWMPG + COMPRATIO + 
                GAS + LUX, data = dat.cp)

##PRESS
library(MPV)
PRESS(res.best)

#Mallow's CP
#Mallows Cp
library(olsrr)
ols_mallows_cp(model=res.best, fullmodel=res.full)
