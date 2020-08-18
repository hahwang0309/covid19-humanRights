dt <- read.csv("dt_covid19_humanRights.csv")

# Correlation analysis
options(digits=2)
cor(dt[,5:14], method="pearson")

# Regression models

model_1 <- lm(DRICs~A1+A2+A3+A4+M1+M2+M3+M4, data=dt)
summary(model_1)

model_2 <- lm(DRICs~A1*P1+A2*P1+A3*P1+A4*P1+M1+M2+M3+M4, data=dt)
summary(model_2)

model_3 <- lm(DRICs~A1*P1+A2*P1+A3*P1+A4*P1+M1*P1+M2*P1+M3*P1+M4*P1, data=dt)
summary(model_3)


par(mfrow=c(2,2))
plot(model_1)
plot(model_2)
plot(model_3)