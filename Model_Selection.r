```R
install.packages('olsrr')
library('olsrr')
 
CVD=read.csv("C:/.../CVD_All.csv",header=T)
names(CVD)
mean(CVD$age,na.rm=T)
CVD_a <- CVD[complete.cases(CVD),] ##將完整資料留下
head(CVD_a)

# DATA EXPLORING
#用cor函數計算所有變數之間的相關係數:
  #DBP和history,followtime,CVD,betel_palm,smoking,smoking_level幾乎無關(cor0.01-0.09)-->這些變數不放到模型中
round(cor(CVD_a),2)
#迴歸；篩除幾乎無關的變數->base model
CVD_core <- lm(DBP~age+gender+waist+SBP+GLU+HDL+triglyceride+drinking, data=CVD_a, x=T)

#干擾變數: age, gender，always將他們留在模型中
#Create base model:cf
cf <- lm(DBP~age+gender,data=CVD_a)
#使用step函數 搭配AIC指標，進行逐步回歸變數篩選- ????result does show AIC
#summary(step(CVD_core,scope=list(upper=~age+gender+waist+SBP+GLU+HDL+triglyceride+drinking,lower=~age+gender),trace=FALSE,k=2,method="both"))

#方法一  自動選模
#使用step函數 搭配AIC指標，進行逐步回歸變數篩選
summary(step(CVD_core),k=2,method="both")
#R-squared是0.57
#模型為y=20.09 + (-0.07)*age + 0.11*waist + 0.42*SBP + (-0.009)*GLU + 0.004*triglyceride + 0.7*drinking

#使用step函數 搭配BIC指標，進行逐步回歸變數篩選
summary(step(CVD_core),k=log(nrow(CVD_core)))
#選出來的模型和AIC一樣，R-square也一樣

#方法二: Cp
model1<-lm(DBP~age+gender+waist,data=CVD_a)
model2 <-lm(DBP~age+gender+waist+SBP,data=CVD_a);model2
model3 <-lm(DBP~age+gender+waist+SBP+GLU,data=CVD_a)
model4 <-lm(DBP~age+gender+waist+SBP+GLU+HDL,data=CVD_a)
model5 <-lm(DBP~age+gender+waist+SBP+GLU+HDL+triglyceride,data=CVD_a)
model5 <-lm(DBP~age+gender+waist+SBP+GLU+HDL+triglyceride+drinking,data=CVD_a)
anova(model1,model2,model3,model4,model5,test='Cp')
# Cp值很大，模型皆不適配

#方法三: LRT = -2[L0-L1] = Deviance0 - Deviance1
anova(model1,model2,model3,model4,model5,test='LRT')
#model4 (加入HDL後)的p-value>0.05, model 1,2,5的p-value都顯著，
#但model1的SST(總變異)遠大於model2&5,而model2變數較model5少，所以選model2較佳
```
