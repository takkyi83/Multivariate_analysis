```R
install.packages('olsrr')
library('olsrr')
 
CVD=read.csv("C:/.../CVD_All.csv",header=T)
names(CVD)
mean(CVD$age,na.rm=T)
CVD_a <- CVD[complete.cases(CVD),] ##將完整資料留下

model=lm(DBP~age+gender+smoking+drinking+waist,data=CVD_a)
s_m=summary(model);s_m
names(model)
names(s_m)
s_m$sigma  ##sigma estimator
sqrt(deviance(model)/df.residual(model)) ##sigma estimator
model$df.residual
s_m$df
s_m$r.squared 
model2=lm(DBP~age+gender+smoking+drinking,data=CVD_a); model2
model3=lm(DBP~age+gender,data=CVD_a); model3


##compare two nested model
deviance(model)
deviance(model2)
summary(model2)
anova(model)
anova(model3, model2, model, test='F')
anova(model3, model2, model, test='Cp')

# LRT 概度比檢定
anova(model3, model2, model, test='LRT')  # pvalse檢定不同model多出來的變數"是否為0"
# H0：model 1 (small model)
# Ha: model 2 (bigger model)
#<=>
# H0: B-smoking = B-drinking = 0
# Ha: H0不全相等
#卡方分布
#自由度為不同model多出來的變數個數
#model3最好

str(CVD)

aic=ols_aic(model, method = c("R", "STATA", "SAS"))

model <- lm(DBP~ ., data = CVD_a)  ##forward
ols_step_forward(model)

model <- lm(DBP~ ., data = CVD_a)  ##stepwise
ols_stepwise(model)

model <- lm(DBP~age+gender+smoking+drinking+waist, data = CVD_a) 
ols_step_forward(model)

##cp,aic,bic,mse,jp,pc,sp
k=ols_all_subset(model)
plot(k)
names(k)

##Best Subsets Regression
k2 = ols_best_subset(model)
plot(k2)

##leverage
lev=ols_leverage(model)
plot(lev)

##Actual vs Fitted Values Plot
ols_ovsp_plot(model)
# fitted value = y-hat (估計值)
#if the model is good, red line should have trend as blue line
#blue line is 對角線

##Studentized Residuals vs Leverage Plot
ols_rsdlev_plot(model)
#showing "cutpoint" for leverage

##Residual QQ Plot
ols_rsd_qqplot(model)

##Residual Histogram
ols_rsd_hist(model)
#normal distribution will close to red line
#if it is not normal distribution we need to 轉換(做分類,取log較不好
#因不好解釋)，不然怎麼做模都不好

ols_rvsp_plot(model)

##
ols_coll_diag(model)     # vif and tolerance
ols_vif_tol(model)       # eigenvalues and condition indices
ols_eigen_cindex(model)  # collinearity diagnostics

##Cooks’ D Bar Plot
ols_cooksd_barplot(model)

##Cooks’ D Chart
ols_cooksd_chart(model)

##Part and Partial Correlations
ols_correlations(model)

##Correlation Test For Normality
ols_corr_test(model)

##DFBETAs Panel
ols_dfbetas_panel(model)

##Added Variable Plot
k1=ols_avplots(model)
```



