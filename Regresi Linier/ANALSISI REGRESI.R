library(moments)
library(readxl)

#INPUT DATA KE PROGRAM R
DATA1 <- read_excel("data.xlsx")
View(DATA1)                                                                                               

#Model
model1=lm(Y~X1+X2,data=DATA1)

#Uji asumsi normalitas residual
shapiro.test(model1$residuals)
    
#UJI LINIERITAS
resettest(model1)

#UJI HOMOSKEDASTISITAS
bptest(model1,studentize=FALSE, data = data)
    
#UJI AUTOKORELASI
dwtest(model1, alternative = "two.sided")
    
#runs test
library(randtests)
runs.test(model1$residuals)

#VIF #asumsi non multikolinieitas
reg=lm(Y~X1+X2,data=DATA1)
vif(reg)

summary(model1)
    

