library(readr)

library(MASS) 
#for ploting linear model that remoces the outliers

dataset <- read.csv("https://raw.githubusercontent.com/s-shakir/Parkinson-s-Disease-Data-Analysis/main/DiseaseDataset.csv")
View(dataset)

# SCATTER PLOTS #

#-------------------------------------------------------age----------------------------------#

plot(age~Jitter.Absolute., data = dataset)

plot(age~Jitter.RAP, data = dataset)

plot(age~HNR, data = dataset)


#-----------------------Jitter.Absolute.-----------------------#

plot(Jitter.Absolute.~age, data = dataset)

plot(Jitter.Absolute.~Jitter.RAP, data = dataset)

plot(Jitter.Absolute.~HNR, data = dataset)

#--------------------------Jitter.RAP--------------------------#

plot(Jitter.RAP~age, data = dataset)

plot(Jitter.RAP~Jitter.Absolute., data = dataset)

plot(Jitter.RAP~HNR, data = dataset)

#--------------------------HNR--------------------------#

plot(HNR~age, data = dataset)

plot(HNR~Jitter.Absolute., data = dataset)

plot(HNR~Jitter.RAP, data = dataset)




#  CORRELATION MATRIX #

result <- cor(dataset)
round(result, 2)




# LINEAR REGRESSION MODEL #

#-------------------------------------------------------age----------------------------------#

age_jita <- lm(dataset$age~dataset$Jitter.Absolute.)

age_jitr <- lm(dataset$age~dataset$Jitter.RAP)

age_hnr <- lm(dataset$age~dataset$HNR)

#-----------------------Jitter.Absolute.-----------------------#

jita_age <- lm(dataset$Jitter.Absolute.~dataset$age)

jita_jitr <- lm(dataset$Jitter.Absolute.~dataset$Jitter.RAP)

jita_hnr <- lm(dataset$Jitter.Absolute.~dataset$HNR)

#--------------------------Jitter.RAP--------------------------#

jitr_age <- lm(dataset$Jitter.RAP~dataset$age)

jitr_jita <- lm(dataset$Jitter.RAP~dataset$Jitter.Absolute.)

jitr_hnr <- lm(dataset$Jitter.RAP~dataset$HNR)

#--------------------------HNR--------------------------#

hnr_age <- lm(dataset$HNR~dataset$age)

hnr_jita <- lm(dataset$HNR~dataset$Jitter.Absolute.)

hnr_jitr <- lm(dataset$HNR~dataset$Jitter.RAP)



# PLOTTING MODEL AND BEST FIT LINE #

#-------------------------------------------------------age----------------------------------#

plot(age~Jitter.Absolute., data = dataset)
abline(age_jita, col="red")

plot(age~Jitter.RAP, data = dataset)
abline(age_jitr, col="red")

plot(age~HNR, data = dataset)
abline(age_hnr, col="red")

#-----------------------Jitter.Absolute.-----------------------#

plot(Jitter.Absolute.~age, data = dataset)
abline(jita_age, col="red")

plot(Jitter.Absolute.~Jitter.RAP, data = dataset)
abline(jita_jitr, col="red")

plot(Jitter.Absolute.~HNR, data = dataset)
abline(jita_hnr, col="red")

#--------------------------Jitter.RAP--------------------------#

plot(Jitter.RAP~age, data = dataset)
abline(jitr_age, col="red")

plot(Jitter.RAP~Jitter.Absolute., data = dataset)
abline(jitr_jita, col="red")

plot(Jitter.RAP~HNR, data = dataset)
abline(jitr_hnr, col="red")

#--------------------------HNR--------------------------#

plot(HNR~age, data = dataset)
abline(hnr_age, col="red")

plot(HNR~Jitter.Absolute., data = dataset)
abline(hnr_jita, col="red")

plot(HNR~Jitter.RAP, data = dataset)
abline(hnr_jitr, col="red")



# RESIDUAL PLOTS #

#-------------------------------------------------------age----------------------------------#

res_age_jita <- resid(age_jita)
plot(dataset$Jitter.Absolute., res_age_jita)
abline(0,0)


res_age_jitr <- resid(age_jitr)
plot(dataset$Jitter.RAP, res_age_jitr)
abline(0,0)

res_age_hnr <- resid(age_hnr)
plot(dataset$HNR, res_age_hnr)
abline(0,0)

#-----------------------Jitter.Absolute.-----------------------#

res_jita_age <- resid(jita_age)
plot(dataset$age, res_jita_age)
abline(0,0)

res_jita_jitr <- resid(jita_jitr)
plot(dataset$Jitter.RAP, res_jita_jitr)
abline(0,0)

res_jita_hnr <- resid(jita_hnr)
plot(dataset$HNR, res_jita_hnr)
abline(0,0)

#--------------------------Jitter.RAP--------------------------#

res_jitr_age <- resid(jitr_age)
plot(dataset$age, res_jitr_age)
abline(0, 0)

res_jitr_jita <- resid(jitr_jita)
plot(dataset$Jitter.Absolute., res_jitr_jita)
abline(0, 0)

res_jitr_hnr <- resid(jitr_hnr)
plot(dataset$HNR, res_jitr_hnr)
abline(0, 0)

#--------------------------HNR--------------------------#

res_hnr_age <- resid(hnr_age)
plot(dataset$age, res_hnr_age)
abline(0,0)

res_hnr_jita <- resid(hnr_jita)
plot(dataset$Jitter.Absolute., res_hnr_jita)
abline(0,0)

res_hnr_jitr <- resid(hnr_jitr)
plot(dataset$Jitter.RAP, res_hnr_jitr)
abline(0,0)




# REMOVING OUTLIERS AND PLOTTING THE LINEAR MODEL #

#-------------------------------------------------------age----------------------------------#

rlm_age_jita <- rlm(dataset$age~dataset$Jitter.Absolute.)
plot(age~Jitter.Absolute., data = dataset)
abline(rlm_age_jita, col="red")

rlm_age_jitr <- rlm(dataset$age~dataset$Jitter.RAP)
plot(age~Jitter.RAP, data = dataset)
abline(rlm_age_jitr, col="red")

rlm_age_hnr <- rlm(dataset$age~dataset$HNR)
plot(age~HNR, data = dataset)
abline(rlm_age_hnr, col="red")

#-----------------------Jitter.Absolute.-----------------------#

rlm_jita_age <- lm(dataset$Jitter.Absolute.~dataset$age)
plot(Jitter.Absolute.~age, data = dataset)
abline(jita_age, col="red")

rlm_jita_jitr <- rlm(dataset$Jitter.Absolute.~dataset$Jitter.RAP)
plot(Jitter.Absolute.~Jitter.RAP, data = dataset)
abline(rlm_jita_jitr, col="red")

rlm_jita_hnr <- rlm(dataset$Jitter.Absolute.~dataset$HNR)
plot(Jitter.Absolute.~HNR, data = dataset)
abline(rlm_jita_hnr, col="red")

#--------------------------Jitter.RAP--------------------------#

rlm_jitr_age <- rlm(dataset$Jitter.RAP~dataset$age)
plot(Jitter.RAP~age, data = dataset)
abline(rlm_jitr_age, col="red")

rlm_jitr_jita <- rlm(dataset$Jitter.RAP~dataset$Jitter.Absolute.)
plot(Jitter.RAP~Jitter.Absolute., data = dataset)
abline(rlm_jitr_jita, col="red")

rlm_jitr_hnr <- rlm(dataset$Jitter.RAP~dataset$HNR)
plot(Jitter.RAP~HNR, data = dataset)
abline(rlm_jitr_hnr, col="red")

#--------------------------HNR--------------------------#

rlm_hnr_age <- rlm(dataset$HNR~dataset$age)
plot(HNR~age, data = dataset)
abline(rlm_hnr_age, col="red")

rlm_hnr_jita <- rlm(dataset$HNR~dataset$Jitter.Absolute.)
plot(HNR~Jitter.Absolute., data = dataset)
abline(rlm_hnr_jita, col="red")

rlm_hnr_jitr <- rlm(dataset$HNR~dataset$Jitter.RAP)
plot(HNR~Jitter.RAP, data = dataset)
abline(rlm_hnr_jitr, col="red")
