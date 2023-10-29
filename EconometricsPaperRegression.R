## ----libraries, echo=FALSE, results = FALSE, message=FALSE, warning=FALSE-----
library(lmtest)
library(sandwich)
library(pscl)
library(mfx)
library(jtools)
library(ggstance)
library(huxtable)
library(dplyr)
library(tidyr)
library(leaps)
library(car)
library(broom.mixed)
library(relaimpo)
library(gvlma)
library(readxl)
library(plm)
library(cdlTools)


## ----data, echo=FALSE, results = FALSE, message=FALSE, warning=FALSE----------
df.data <- read.csv("DATASET.csv")
summary(df.data)


## ----clean, echo=FALSE, results = FALSE, message=FALSE, warning=FALSE---------
df.data <- subset(df.data, STATE != 0)
df.data$STATE <- fips(df.data$STATE, to ='Abbreviation')
df.data <- subset(df.data, AGE.REFP != 999)
df.data <- subset(df.data, AGE.SPOUSE != 999)
df.data <- subset(df.data, SALARY != 9999998.00)
df.data <- subset(df.data, SALARY != 9999999.00)
df.data <- subset(df.data, SALARY != 1000000.0)
df.data <- subset(df.data, EDU.FATHER != 99)
df.data <- subset(df.data, EDU.MOTHER != 99)
df.data <- subset(df.data, RACE.SPOUSE != 9)
df.data <- subset(df.data, RELIGION.SPOUSE != 99)
df.data <- subset(df.data, RACE.REFP != 9)
df.data <- subset(df.data, RELIGION.REFP != 99)
df.data$lnSALARY <- ifelse(df.data$SALARY != 0, log(df.data$SALARY), df.data$SALARY)
df.data$SAMESEX <- ifelse(df.data$SEX.REFP == df.data$SEX.SPOUSE, 1, 0)
summary(df.data)


## ----age stratifying binary, echo=FALSE, results = FALSE, message=FALSE, warning=FALSE----
df.data$x40OROLDER.REFP <- ifelse(df.data$AGE.REFP >= 40, 1, 0)
df.data$x50OROLDER.REFP <- ifelse(df.data$AGE.REFP >= 50, 1, 0)
df.data$x60OROLDER.REFP <- ifelse(df.data$AGE.REFP >= 60, 1, 0)

df.data$x40OROLDER.SPOUSE <- ifelse(df.data$AGE.SPOUSE >= 40, 1, 0)
df.data$x50OROLDER.SPOUSE <- ifelse(df.data$AGE.SPOUSE >= 50, 1, 0)
df.data$x60OROLDER.SPOUSE <- ifelse(df.data$AGE.SPOUSE >= 60, 1, 0)

summary(df.data)


## ----efe state salary, echo=FALSE, results = FALSE, message=FALSE, warning=FALSE----
reg.efe.state.salary <- lm(NUM.CHILD ~ SALARY + factor(STATE) - 1, 
                    data = df.data)
summary(reg.efe.state.salary)


## ----efe state ln salary, echo=FALSE, results = FALSE, message=FALSE, warning=FALSE----
reg.efe.state.lnsalary <- lm(NUM.CHILD ~ lnSALARY + factor(STATE) - 1, 
                    data = df.data)
summary(reg.efe.state.lnsalary)


## ----efe state exluding age stratifying binary and interaction, echo=FALSE, results = FALSE, message=FALSE, warning=FALSE----
reg.efe.state.excluASBandINT <- lm(NUM.CHILD ~ AGE.REFP+SEX.REFP+AGE.SPOUSE+SEX.SPOUSE+lnSALARY+EDU.FATHER+EDU.MOTHER+RACE.SPOUSE+RELIGION.SPOUSE+RACE.REFP+RELIGION.REFP+SAMESEX+ factor(STATE) - 1, data = df.data)
summary(reg.efe.state.excluASBandINT)


## ----efe state exluding interaction, echo=FALSE, results = FALSE, message=FALSE, warning=FALSE----
reg.efe.state.excluINT <- lm(NUM.CHILD ~ AGE.REFP+SEX.REFP+AGE.SPOUSE+SEX.SPOUSE+lnSALARY+EDU.FATHER+EDU.MOTHER+RACE.SPOUSE+RELIGION.SPOUSE+RACE.REFP+RELIGION.REFP+SAMESEX+x40OROLDER.REFP+x50OROLDER.REFP+x60OROLDER.REFP+x40OROLDER.SPOUSE+x50OROLDER.SPOUSE+x60OROLDER.SPOUSE+ factor(STATE) - 1, data = df.data)
summary(reg.efe.state.excluINT)


## ----efe state all, echo=FALSE, results = FALSE, message=FALSE, warning=FALSE----
reg.efe.state1 <- lm(NUM.CHILD ~ AGE.REFP+SEX.REFP+AGE.SPOUSE+SEX.SPOUSE+lnSALARY+EDU.FATHER+EDU.MOTHER+RACE.SPOUSE+RELIGION.SPOUSE+RACE.REFP+RELIGION.REFP+SAMESEX+x40OROLDER.REFP+x50OROLDER.REFP+x60OROLDER.REFP+x40OROLDER.SPOUSE+x50OROLDER.SPOUSE+x60OROLDER.SPOUSE+AGE.REFP*x40OROLDER.REFP+AGE.REFP*x50OROLDER.REFP+AGE.REFP*x60OROLDER.REFP+AGE.SPOUSE*x40OROLDER.SPOUSE+AGE.SPOUSE*x50OROLDER.SPOUSE+AGE.SPOUSE*x60OROLDER.SPOUSE + factor(STATE) - 1, data = df.data)
summary(reg.efe.state1)


## ----efe state drop non ss coef, echo=FALSE, results = TRUE, message=FALSE, warning=FALSE----
reg.efe.state2 <- lm(NUM.CHILD ~ AGE.REFP+SEX.REFP+AGE.SPOUSE+SEX.SPOUSE+lnSALARY+EDU.FATHER+RELIGION.SPOUSE+RACE.REFP+RELIGION.REFP+SAMESEX+x40OROLDER.REFP+x60OROLDER.REFP+x40OROLDER.SPOUSE+x50OROLDER.SPOUSE+x60OROLDER.SPOUSE+AGE.REFP*x40OROLDER.REFP+AGE.REFP*x60OROLDER.REFP+AGE.SPOUSE*x40OROLDER.SPOUSE+AGE.SPOUSE*x50OROLDER.SPOUSE+AGE.SPOUSE*x60OROLDER.SPOUSE + factor(STATE) - 1, data = df.data)
summary(reg.efe.state2)


## ----coeffTable, echo=FALSE, results = FALSE, message=FALSE, warning=FALSE----
export_summs(reg.efe.state.salary,reg.efe.state.lnsalary, reg.efe.state.excluASBandINT, reg.efe.state.excluINT, reg.efe.state2, scale = TRUE, model.names = c("Salary Model", "lnSalary Model", "All Model Excluding Age Strata and Interaction Terms", "All Model Excluding Interaction Terms", "All SS Coefficient Model"), digits = 3)

