library(openxlsx)
library(dplyr)
library(reshape2)
library(sf)
library(raster)
library(terra)
library(exactextractr)
library(Hmisc)
library(png)
library(MASS)
library(zoo)
library(ggplot2)
library(stringr)
library(png)
library(dlnm)
library(lm.beta)
library(splines)
library(car)
library(trend)
library(MetBrewer)
library(lmtest)
library(sandwich)
library(patchwork)
library(scales)
library(colorspace)
library(ggnewscale)
library(pheatmap)
library(purrr)
library(parameters)
library(effectsize)

##########Compiling final results##########
#####International traffic trend#####
Trend.trend<-read.xlsx("Trend/Data/International traffic.xlsx")
Trend.trend$Dea.trend[which(is.na(Trend.trend$Dea.trend))]<-0

#Main model (selected)
Original.model<-glm(pct_slope~Elderly+Merchandise+Exp.com.ser.trend+
                      Death+Score,
                    family=gaussian(link="identity"),
                    data=Trend.trend,weights=Weight2)

#Calculating R2 for each variable
R2.var<-relaimpo::calc.relimp(Original.model,type='lmg')
R2.var$lmg

# coefficients
results <- model_parameters(Original.model, ci=0.95, vcov="HC3")
Expression=paste0(round(results$Coefficient[-1]*1000,2)," (",
                                     round(results$CI_low[-1]*1000,2),", ",
                                     round(results$CI_high[-1]*1000,2),")")
print(cbind(results$Parameter[-1], Expression))
Expression=paste0(round(results$Coefficient[-1]*1000,2)," (",
                                     round((results$Coefficient[-1] - 1.96*results$SE[-1])*1000,2),", ",
                                     round((results$Coefficient[-1] + 1.96*results$SE[-1])*1000,2),")")
print(cbind(results$Parameter[-1], Expression))
print(results)

# 直接对你拟合好的 Original.model 提取标准化系数，并指定使用 HC3 稳健标准误
std_results <- model_parameters(
  Original.model, 
  standardize = "refit", # 重新拟合以获取完全标准化的结果
  vcov = "HC3",        # 指定使用 HC3 稳健标准误
  ci = 0.95              # 95% 置信区间
)
Expression=paste0(round(std_results$Coefficient[-1]*1000,2)," (",
                                     round(std_results$CI_low[-1]*1000,2),", ",
                                     round(std_results$CI_high[-1]*1000,2),")")
print(cbind(results$Parameter[-1], Expression))


#Main model (all)
Original.model<-glm(pct_slope~Density.grid+Elderly+Inbound.rate+
                      Outbound.ratio+Eld.trend+Ref.trend+Inb.num.trend+
                      Merchandise+Imports.travel+Imp.trend+
                      Exp.com.ser.trend+Imp.goo.trend+Death+Score,
                    family=gaussian(link="identity"),
                    data=Trend.trend,weights=Weight2)
bptest(Original.model)
Model.robust<-vcovHC(Original.model,type="HC3")
LR<-coeftest(Original.model,Model.robust)
Result<-data.frame(Var=row.names(LR)[-1],
                   Coef=LR[-1,1]*1000, 
                   Se=LR[-1,2]*1000,
                   Lower=LR[-1,1]*1000-1.96*LR[-1,2]*1000,
                   Upper=LR[-1,1]*1000+1.96*LR[-1,2]*1000,
                   Expression=paste0(round(LR[-1,1]*1000,2)," (",
                                     round(LR[-1,1]*1000-1.96*LR[-1,2]*1000,2),", ",
                                     round(LR[-1,1]*1000+1.96*LR[-1,2]*1000,2),")"),
                   AIC=round(Original.model$aic,2),
                   P=LR[-1,4])

#Calculating R2 for each variable
R2.var<-relaimpo::calc.relimp(Original.model,type='lmg')
R2.var$lmg

print(Result$Expression)

# coefficients
results <- model_parameters(Original.model, ci=0.95, vcov="HC3")
Expression=paste0(round(results$Coefficient[-1]*1000,2)," (",
                                     round(results$CI_low[-1]*1000,2),", ",
                                     round(results$CI_high[-1]*1000,2),")")
print(cbind(results$Parameter[-1], Expression))


# 直接对你拟合好的 Original.model 提取标准化系数，并指定使用 HC3 稳健标准误
std_results <- model_parameters(
  Original.model, 
  standardize = "refit", # 重新拟合以获取完全标准化的结果
  vcov = "HC3",        # 指定使用 HC3 稳健标准误
  ci = 0.95              # 95% 置信区间
)

Expression=paste0(round(std_results$Coefficient[-1]*1000,2)," (",
                                     round(std_results$CI_low[-1]*1000,2),", ",
                                     round(std_results$CI_high[-1]*1000,2),")")
print(cbind(results$Parameter[-1], Expression))

# write.xlsx(Result,"F:/Ming/OAG/Traffic/Trend/Inter amount/Stardardized all.xlsx",rowNames=F)

#Likelihood ratio test
Model.simple<-glm(pct_slope~Elderly+Merchandise+Exp.com.ser.trend+
                    Death+Score,
                  family=gaussian(link="identity"),
                  data=Trend.trend,weights=Weight2)
Model.complex<-glm(pct_slope~Density.grid+Elderly+Inbound.rate+
                     Outbound.ratio+Eld.trend+Ref.trend+Inb.num.trend+
                     Merchandise+Imports.travel+Imp.trend+
                     Exp.com.ser.trend+Imp.goo.trend+Death+Score,
                   family=gaussian(link="identity"),
                   data=Trend.trend,weights=Weight2)
logLik.simple<-logLik(Model.simple)
logLik.complex<-logLik(Model.complex)
lr.statistic<-(-2)*(logLik.simple-logLik.complex)
df.diff<-Model.complex$rank-Model.simple$rank
P.value<-pchisq(lr.statistic,df.diff,lower.tail=FALSE)
print(P.value)

#Interaction model
#98
which(colnames(Trend.trend)=="Imp.mer.trend")
Trend.category<-Trend.trend%>%
  mutate(across(c(98),
                ~cut(.x,
                     breaks=quantile(.x,probs=c(0,0.33,0.66,1),na.rm=T),
                     labels=c(0,1,2),include.lowest=T)))
Trend.category<-Trend.category[,c(4,98)]
colnames(Trend.category)<-paste0(colnames(Trend.category),".tertiary")
Trend.tertiary<-left_join(Trend.trend,Trend.category,by=c("ISO3"="ISO3.tertiary"))

Original.model<-glm(pct_slope~Elderly+Exp.com.ser.trend+
                    Death+Score+Merchandise+Imp.mer.trend.tertiary+
                    Imp.mer.trend.tertiary:Merchandise,
                    family=gaussian(link="identity"),
                    data=Trend.tertiary,weights=Weight2)

#Calculating R2 for each variable
R2.var<-relaimpo::calc.relimp(Original.model,type='lmg')
R2.var$lmg

vif(Original.model)

# coefficients
results <- model_parameters(Original.model, ci=0.95, vcov="HC3")
Expression=paste0(round(results$Coefficient[-1]*1000,2)," (",
                                     round(results$CI_low[-1]*1000,2),", ",
                                     round(results$CI_high[-1]*1000,2),")")
print(cbind(results$Parameter[-1], Expression))

Expression=paste0(round(results$Coefficient[-1]*1000,2)," (",
                                     round((results$Coefficient[-1] - 1.96*results$SE[-1])*1000,2),", ",
                                     round((results$Coefficient[-1] + 1.96*results$SE[-1])*1000,2),")")
print(cbind(results$Parameter[-1], Expression))

# 直接对你拟合好的 Original.model 提取标准化系数，并指定使用 HC3 稳健标准误
std_results <- model_parameters(
  Original.model, 
  standardize = "refit", # 重新拟合以获取完全标准化的结果
  vcov = "HC3",        # 指定使用 HC3 稳健标准误
  ci = 0.95              # 95% 置信区间
)

Expression=paste0(round(std_results$Coefficient[-1]*1000,2)," (",
                                     round(std_results$CI_low[-1]*1000,2),", ",
                                     round(std_results$CI_high[-1]*1000,2),")")
print(cbind(results$Parameter[-1], Expression))


Single.model<-glm(pct_slope~Imp.mer.trend.tertiary,
                  family=gaussian(link="identity"),data=Trend.tertiary,weights=Weight2)
bptest(Single.model)
Model.robust<-vcovHC(Single.model,type="HC3")
LR<-coeftest(Single.model,Model.robust)
Result<-data.frame(Var=row.names(LR)[-1],
                   Coef=LR[-1,1]*1000, 
                   Se=LR[-1,2]*1000,
                   Lower=LR[-1,1]*1000-1.96*LR[-1,2]*1000,
                   Upper=LR[-1,1]*1000+1.96*LR[-1,2]*1000,
                   Expression=paste0(round(LR[-1,1]*1000,2)," (",
                                     round(LR[-1,1]*1000-1.96*LR[-1,2]*1000,2),", ",
                                     round(LR[-1,1]*1000+1.96*LR[-1,2]*1000,2),")"),
                   AIC=round(Single.model$aic,2),
                   P=LR[-1,4])

Single.model<-glm(pct_slope~Imp.mer.trend.tertiary:Merchandise,
                  family=gaussian(link="identity"),data=Trend.tertiary,weights=Weight2)
bptest(Single.model)
Model.robust<-vcovHC(Single.model,type="HC3")
LR<-coeftest(Single.model,Model.robust)
Result<-data.frame(Var=row.names(LR)[-1],
                   Coef=LR[-1,1]*1000, 
                   Se=LR[-1,2]*1000,
                   Lower=LR[-1,1]*1000-1.96*LR[-1,2]*1000,
                   Upper=LR[-1,1]*1000+1.96*LR[-1,2]*1000,
                   Expression=paste0(round(LR[-1,1]*1000,2)," (",
                                     round(LR[-1,1]*1000-1.96*LR[-1,2]*1000,2),", ",
                                     round(LR[-1,1]*1000+1.96*LR[-1,2]*1000,2),")"),
                   AIC=round(Single.model$aic,2),
                   P=LR[-1,4])





#####Domestic traffic trend#####
Trend.trend<-read.xlsx("Trend/Data/Domestic traffic.xlsx")
Trend.trend$HSR<-0
Trend.trend$HSR[which(Trend.trend$country=="CN"|Trend.trend$country=="ES"|
                        Trend.trend$country=="FR"|Trend.trend$country=="DE"|
                        Trend.trend$country=="JP"|Trend.trend$country=="SE"|
                        Trend.trend$country=="KR"|Trend.trend$country=="GB"|
                        Trend.trend$country=="IT"|Trend.trend$country=="TR"|
                        Trend.trend$country=="FI"|Trend.trend$country=="RU"|
                        Trend.trend$country=="UZ"|Trend.trend$country=="SA"|
                        Trend.trend$country=="TW"|Trend.trend$country=="PL"|
                        Trend.trend$country=="AT"|Trend.trend$country=="BE"|
                        Trend.trend$country=="PT"|Trend.trend$country=="MA"|
                        Trend.trend$country=="NL"|Trend.trend$country=="NO"|
                        Trend.trend$country=="CH"|Trend.trend$country=="GR"|
                        Trend.trend$country=="ID"|Trend.trend$country=="US"|
                        Trend.trend$country=="RS"|Trend.trend$country=="DK"|
                        Trend.trend$country=="HK")]<-1

#Main model (selected)
Original.model<-glm(pct_slope~Population+Elderly+Electricity+
                      GDP.trend,
                    family=gaussian(link="identity"),
                    data=Trend.trend,weights=Weight2)
#Calculating R2 for each variable
R2.var<-relaimpo::calc.relimp(Original.model,type='lmg')
R2.var$lmg


# coefficients
results <- model_parameters(Original.model, ci=0.95, vcov="HC3")
Expression=paste0(round(results$Coefficient[-1]*1000,2)," (",
                                     round(results$CI_low[-1]*1000,2),", ",
                                     round(results$CI_high[-1]*1000,2),")")
print(cbind(results$Parameter[-1], Expression))


# 直接对你拟合好的 Original.model 提取标准化系数，并指定使用 HC3 稳健标准误
std_results <- model_parameters(
  Original.model, 
  standardize = "refit", # 重新拟合以获取完全标准化的结果
  vcov = "HC3",        # 指定使用 HC3 稳健标准误
  ci = 0.95              # 95% 置信区间
)
Expression=paste0(round(std_results$Coefficient[-1]*1000,2)," (",
                                     round(std_results$CI_low[-1]*1000,2),", ",
                                     round(std_results$CI_high[-1]*1000,2),")")
print(cbind(results$Parameter[-1], Expression))




#Main model (all)
Original.model<-glm(pct_slope~Population+Elderly+Electricity+
                      GDP.trend+Seat.pop.area+Years.conflicts,
                    family=gaussian(link="identity"),
                    data=Trend.trend,weights=Weight2)
#Calculating R2 for each variable
R2.var<-relaimpo::calc.relimp(Original.model,type='lmg')
R2.var$lmg

# coefficients
results <- model_parameters(Original.model, ci=0.95, vcov="HC3")
Expression=paste0(round(results$Coefficient[-1]*1000,2)," (",
                                     round(results$CI_low[-1]*1000,2),", ",
                                     round(results$CI_high[-1]*1000,2),")")
print(cbind(results$Parameter[-1], Expression))


# 直接对你拟合好的 Original.model 提取标准化系数，并指定使用 HC3 稳健标准误
std_results <- model_parameters(
  Original.model, 
  standardize = "refit", # 重新拟合以获取完全标准化的结果
  vcov = "HC3",        # 指定使用 HC3 稳健标准误
  ci = 0.95              # 95% 置信区间
)
Expression=paste0(round(std_results$Coefficient[-1]*1000,2)," (",
                                     round(std_results$CI_low[-1]*1000,2),", ",
                                     round(std_results$CI_high[-1]*1000,2),")")
print(cbind(results$Parameter[-1], Expression))



#Likelihood ratio test
Model.simple<-glm(pct_slope~Population+Elderly+Electricity+
                    GDP.trend,
                  family=gaussian(link="identity"),
                  data=Trend.trend,weights=Weight2)
Model.complex<-glm(pct_slope~Population+Elderly+Electricity+
                     GDP.trend+Seat.pop.area+Years.conflicts,
                   family=gaussian(link="identity"),
                   data=Trend.trend,weights=Weight2)
logLik.simple<-logLik(Model.simple)
logLik.complex<-logLik(Model.complex)
lr.statistic<-(-2)*(logLik.simple-logLik.complex)
df.diff<-Model.complex$rank-Model.simple$rank
P.value<-pchisq(lr.statistic,df.diff,lower.tail=FALSE)

#20
which(colnames(Trend.trend)=="GDP")
Trend.category<-Trend.trend%>%
  mutate(across(c(20),
                ~cut(.x,
                     breaks=quantile(.x,probs=c(0,0.33,0.66,1),na.rm=T),
                     labels=c(0,1,2),include.lowest=T)))
Trend.category<-Trend.category[,c(4,20)]
colnames(Trend.category)<-paste0(colnames(Trend.category),".tertiary")
Trend.tertiary<-left_join(Trend.trend,Trend.category,by=c("ISO3"="ISO3.tertiary"))

Original.model<-glm(pct_slope~Population+Elderly+GDP.trend+
                      Electricity+GDP.tertiary+
                      GDP.tertiary:Electricity,
                    family=gaussian(link="identity"),
                    data=Trend.tertiary,weights=Weight2)
#Calculating R2 for each variable
R2.var<-relaimpo::calc.relimp(Original.model,type='lmg')
R2.var$lmg


# coefficients
results <- model_parameters(Original.model, ci=0.95, vcov="HC3")
Expression=paste0(round(results$Coefficient[-1]*1000,2)," (",
                                     round(results$CI_low[-1]*1000,2),", ",
                                     round(results$CI_high[-1]*1000,2),")")
print(cbind(results$Parameter[-1], Expression))


# 直接对你拟合好的 Original.model 提取标准化系数，并指定使用 HC3 稳健标准误
std_results <- model_parameters(
  Original.model, 
  standardize = "refit", # 重新拟合以获取完全标准化的结果
  vcov = "HC3",        # 指定使用 HC3 稳健标准误
  ci = 0.95              # 95% 置信区间
)
Expression=paste0(round(std_results$Coefficient[-1]*1000,2)," (",
                                     round(std_results$CI_low[-1]*1000,2),", ",
                                     round(std_results$CI_high[-1]*1000,2),")")
print(cbind(results$Parameter[-1], Expression))


vif(Original.model)

Single.model<-glm(pct_slope~GDP.tertiary,
                  family=gaussian(link="identity"),data=Trend.tertiary,weights=Weight2)
bptest(Single.model)
Model.robust<-vcovHC(Single.model,type="HC3")
LR<-coeftest(Single.model,Model.robust)
Result<-data.frame(Var=row.names(LR)[-1],
                   Coef=LR[-1,1]*1000, 
                   Se=LR[-1,2]*1000,
                   Lower=LR[-1,1]*1000-1.96*LR[-1,2]*1000,
                   Upper=LR[-1,1]*1000+1.96*LR[-1,2]*1000,
                   Expression=paste0(round(LR[-1,1]*1000,2)," (",
                                     round(LR[-1,1]*1000-1.96*LR[-1,2]*1000,2),", ",
                                     round(LR[-1,1]*1000+1.96*LR[-1,2]*1000,2),")"),
                   AIC=round(Single.model$aic,2),
                   P=LR[-1,4])

Single.model<-glm(pct_slope~GDP.tertiary:Electricity,
                  family=gaussian(link="identity"),data=Trend.tertiary,weights=Weight2)
bptest(Single.model)
Model.robust<-vcovHC(Single.model,type="HC3")
LR<-coeftest(Single.model,Model.robust)
Result<-data.frame(Var=row.names(LR)[-1],
                   Coef=LR[-1,1]*1000, 
                   Se=LR[-1,2]*1000,
                   Lower=LR[-1,1]*1000-1.96*LR[-1,2]*1000,
                   Upper=LR[-1,1]*1000+1.96*LR[-1,2]*1000,
                   Expression=paste0(round(LR[-1,1]*1000,2)," (",
                                     round(LR[-1,1]*1000-1.96*LR[-1,2]*1000,2),", ",
                                     round(LR[-1,1]*1000+1.96*LR[-1,2]*1000,2),")"),
                   AIC=round(Single.model$aic,2),
                   P=LR[-1,4])




#####International segment trend#####
Trend.trend<-read.xlsx("Trend/Data/International segment.xlsx")
Trend.trend$Dea.trend[which(is.na(Trend.trend$Dea.trend))]<-0

#Main model (selected)
Original.model<-glm(pct_slope~Imp.mer.trend+Death,
                    family=gaussian(link="identity"),
                    data=Trend.trend,weights=Weight2)
#Calculating R2 for each variable
R2.var<-relaimpo::calc.relimp(Original.model,type='lmg')
R2.var$lmg

# coefficients
results <- model_parameters(Original.model, ci=0.95, vcov="HC3")
Expression=paste0(round(results$Coefficient[-1]*1000,2)," (",
                                     round(results$CI_low[-1]*1000,2),", ",
                                     round(results$CI_high[-1]*1000,2),")")
print(cbind(results$Parameter[-1], Expression))


# 直接对你拟合好的 Original.model 提取标准化系数，并指定使用 HC3 稳健标准误
std_results <- model_parameters(
  Original.model, 
  standardize = "refit", # 重新拟合以获取完全标准化的结果
  vcov = "HC3",        # 指定使用 HC3 稳健标准误
  ci = 0.95              # 95% 置信区间
)
Expression=paste0(round(std_results$Coefficient[-1]*1000,2)," (",
                                     round(std_results$CI_low[-1]*1000,2),", ",
                                     round(std_results$CI_high[-1]*1000,2),")")
print(cbind(results$Parameter[-1], Expression))


#Main model (all)
Original.model<-glm(pct_slope~Migrant+Pop.trend+Inb.num.trend+
                      Electricity+Exp.goo.trend+Imp.mer.trend+
                      Death+Score,
                    family=gaussian(link="identity"),
                    data=Trend.trend,weights=Weight2)
#Calculating R2 for each variable
R2.var<-relaimpo::calc.relimp(Original.model,type='lmg')
R2.var$lmg



# coefficients
results <- model_parameters(Original.model, ci=0.95, vcov="HC3")
Expression=paste0(round(results$Coefficient[-1]*1000,2)," (",
                                     round(results$CI_low[-1]*1000,2),", ",
                                     round(results$CI_high[-1]*1000,2),")")
print(cbind(results$Parameter[-1], Expression))


# 直接对你拟合好的 Original.model 提取标准化系数，并指定使用 HC3 稳健标准误
std_results <- model_parameters(
  Original.model, 
  standardize = "refit", # 重新拟合以获取完全标准化的结果
  vcov = "HC3",        # 指定使用 HC3 稳健标准误
  ci = 0.95              # 95% 置信区间
)
Expression=paste0(round(std_results$Coefficient[-1]*1000,2)," (",
                                     round(std_results$CI_low[-1]*1000,2),", ",
                                     round(std_results$CI_high[-1]*1000,2),")")
print(cbind(results$Parameter[-1], Expression))


#Likelihood ratio test
Model.simple<-glm(pct_slope~Imp.mer.trend+Death,
                  family=gaussian(link="identity"),
                  data=Trend.trend,weights=Weight2)
Model.complex<-glm(scale(pct_slope)~scale(Migrant)+scale(Pop.trend)+
                     scale(Inb.num.trend)+scale(Electricity)+
                     scale(Exp.goo.trend)+scale(Imp.mer.trend)+
                     scale(Death)+scale(Score),
                   family=gaussian(link="identity"),data=Trend.trend,weights=Weight2)
logLik.simple<-logLik(Model.simple)
logLik.complex<-logLik(Model.complex)
lr.statistic<-(-2)*(logLik.simple-logLik.complex)
df.diff<-Model.complex$rank-Model.simple$rank
P.value<-pchisq(lr.statistic,df.diff,lower.tail=FALSE)

#31
which(colnames(Trend.trend)=="Exports.commercial.services")
Trend.category<-Trend.trend%>%
  mutate(across(c(31),
                ~cut(.x,
                     breaks=quantile(.x,probs=c(0,0.5,1),na.rm=T),
                     labels=c(0,1),include.lowest=T)))
Trend.category<-Trend.category[,c(3,31)]
colnames(Trend.category)<-paste0(colnames(Trend.category),".binary")
Trend.binary<-left_join(Trend.trend,Trend.category,by=c("ISO3"="ISO3.binary"))

Original.model<-glm(pct_slope~Imp.mer.trend+Death+
                      Exports.commercial.services.binary+
                      Exports.commercial.services.binary:Death,
                    family=gaussian(link="identity"),
                    data=Trend.binary,weights=Weight2)
#Calculating R2 for each variable
R2.var<-relaimpo::calc.relimp(Original.model,type='lmg')
R2.var$lmg


# coefficients
results <- model_parameters(Original.model, ci=0.95, vcov="HC3")
Expression=paste0(round(results$Coefficient[-1]*1000,2)," (",
                                     round(results$CI_low[-1]*1000,2),", ",
                                     round(results$CI_high[-1]*1000,2),")")
print(cbind(results$Parameter[-1], Expression))


# 直接对你拟合好的 Original.model 提取标准化系数，并指定使用 HC3 稳健标准误
std_results <- model_parameters(
  Original.model, 
  standardize = "refit", # 重新拟合以获取完全标准化的结果
  vcov = "HC3",        # 指定使用 HC3 稳健标准误
  ci = 0.95              # 95% 置信区间
)
Expression=paste0(round(std_results$Coefficient[-1]*1000,2)," (",
                                     round(std_results$CI_low[-1]*1000,2),", ",
                                     round(std_results$CI_high[-1]*1000,2),")")
print(cbind(results$Parameter[-1], Expression))


vif(Original.model)

Single.model<-glm(pct_slope~Exports.commercial.services.binary,
                  family=gaussian(link="identity"),data=Trend.binary,weights=Weight2)
bptest(Single.model)
Model.robust<-vcovHC(Single.model,type="HC3")
LR<-coeftest(Single.model,Model.robust)
Result<-data.frame(Var=row.names(LR)[-1],
                   Coef=LR[-1,1]*1000, 
                   Se=LR[-1,2]*1000,
                   Lower=LR[-1,1]*1000-1.96*LR[-1,2]*1000,
                   Upper=LR[-1,1]*1000+1.96*LR[-1,2]*1000,
                   Expression=paste0(round(LR[-1,1]*1000,2)," (",
                                     round(LR[-1,1]*1000-1.96*LR[-1,2]*1000,2),", ",
                                     round(LR[-1,1]*1000+1.96*LR[-1,2]*1000,2),")"),
                   AIC=round(Single.model$aic,2),
                   P=LR[-1,4])

Single.model<-glm(pct_slope~Exports.commercial.services.binary:Death,
                  family=gaussian(link="identity"),data=Trend.binary,weights=Weight2)
bptest(Single.model)
Model.robust<-vcovHC(Single.model,type="HC3")
LR<-coeftest(Single.model,Model.robust)
Result<-data.frame(Var=row.names(LR)[-1],
                   Coef=LR[-1,1]*1000, 
                   Se=LR[-1,2]*1000,
                   Lower=LR[-1,1]*1000-1.96*LR[-1,2]*1000,
                   Upper=LR[-1,1]*1000+1.96*LR[-1,2]*1000,
                   Expression=paste0(round(LR[-1,1]*1000,2)," (",
                                     round(LR[-1,1]*1000-1.96*LR[-1,2]*1000,2),", ",
                                     round(LR[-1,1]*1000+1.96*LR[-1,2]*1000,2),")"),
                   AIC=round(Single.model$aic,2),
                   P=LR[-1,4])





#####Domestic segment trend#####
Trend.trend<-read.xlsx("Trend/Data/Domestic segment.xlsx")
Trend.trend$HSR<-0
Trend.trend$HSR[which(Trend.trend$country=="CN"|Trend.trend$country=="ES"|
                        Trend.trend$country=="FR"|Trend.trend$country=="DE"|
                        Trend.trend$country=="JP"|Trend.trend$country=="SE"|
                        Trend.trend$country=="KR"|Trend.trend$country=="GB"|
                        Trend.trend$country=="IT"|Trend.trend$country=="TR"|
                        Trend.trend$country=="FI"|Trend.trend$country=="RU"|
                        Trend.trend$country=="UZ"|Trend.trend$country=="SA"|
                        Trend.trend$country=="TW"|Trend.trend$country=="PL"|
                        Trend.trend$country=="AT"|Trend.trend$country=="BE"|
                        Trend.trend$country=="PT"|Trend.trend$country=="MA"|
                        Trend.trend$country=="NL"|Trend.trend$country=="NO"|
                        Trend.trend$country=="CH"|Trend.trend$country=="GR"|
                        Trend.trend$country=="ID"|Trend.trend$country=="US"|
                        Trend.trend$country=="RS"|Trend.trend$country=="DK"|
                        Trend.trend$country=="HK")]<-1

#Main model (selected)
Original.model<-glm(pct_slope~Den.trend+Nig.trend+Area+
                      Death,
                    family=gaussian(link="identity"),
                    data=Trend.trend,weights=Weight2)
#Calculating R2 for each variable
R2.var<-relaimpo::calc.relimp(Original.model,type='lmg')
R2.var$lmg

# coefficients
results <- model_parameters(Original.model, ci=0.95, vcov="HC3")
Expression=paste0(round(results$Coefficient[-1]*1000,2)," (",
                                     round(results$CI_low[-1]*1000,2),", ",
                                     round(results$CI_high[-1]*1000,2),")")
print(cbind(results$Parameter[-1], Expression))


# 直接对你拟合好的 Original.model 提取标准化系数，并指定使用 HC3 稳健标准误
std_results <- model_parameters(
  Original.model, 
  standardize = "refit", # 重新拟合以获取完全标准化的结果
  vcov = "HC3",        # 指定使用 HC3 稳健标准误
  ci = 0.95              # 95% 置信区间
)
Expression=paste0(round(std_results$Coefficient[-1]*1000,2)," (",
                                     round(std_results$CI_low[-1]*1000,2),", ",
                                     round(std_results$CI_high[-1]*1000,2),")")
print(cbind(results$Parameter[-1], Expression))



#Main model (all)
Original.model<-glm(pct_slope~Den.trend+CPI+GDP.trend+
                      Nig.trend+Segment.pop+Area+
                      Death+Dea.trend,
                    family=gaussian(link="identity"),
                    data=Trend.trend,weights=Weight2)
#Calculating R2 for each variable
R2.var<-relaimpo::calc.relimp(Original.model,type='lmg')
R2.var$lmg

# coefficients
results <- model_parameters(Original.model, ci=0.95, vcov="HC3")
Expression=paste0(round(results$Coefficient[-1]*1000,2)," (",
                                     round(results$CI_low[-1]*1000,2),", ",
                                     round(results$CI_high[-1]*1000,2),")")
print(cbind(results$Parameter[-1], Expression))


# 直接对你拟合好的 Original.model 提取标准化系数，并指定使用 HC3 稳健标准误
std_results <- model_parameters(
  Original.model, 
  standardize = "refit", # 重新拟合以获取完全标准化的结果
  vcov = "HC3",        # 指定使用 HC3 稳健标准误
  ci = 0.95              # 95% 置信区间
)
Expression=paste0(round(std_results$Coefficient[-1]*1000,2)," (",
                                     round(std_results$CI_low[-1]*1000,2),", ",
                                     round(std_results$CI_high[-1]*1000,2),")")
print(cbind(results$Parameter[-1], Expression))


#Likelihood ratio test
Model.simple<-glm(pct_slope~Den.trend+CPI+GDP.trend+
                    Nig.trend+Segment.pop+Area+
                    Death+Dea.trend,
                  family=gaussian(link="identity"),
                  data=Trend.trend,weights=Weight2)
Model.complex<-glm(scale(pct_slope)~scale(Den.trend)+scale(CPI)+
                     scale(GDP.trend)+scale(Nig.trend)+
                     scale(Segment.pop)+scale(Area)+
                     scale(Death)+scale(Dea.trend),
                   family=gaussian(link="identity"),data=Trend.trend,weights=Weight2)
logLik.simple<-logLik(Model.simple)
logLik.complex<-logLik(Model.complex)
lr.statistic<-(-2)*(logLik.simple-logLik.complex)
df.diff<-Model.complex$rank-Model.simple$rank
P.value<-pchisq(lr.statistic,df.diff,lower.tail=FALSE)

#59
which(colnames(Trend.trend)=="Seat.pop")
Trend.category<-Trend.trend%>%
  mutate(across(c(59),
                ~cut(.x,
                     breaks=quantile(.x,probs=c(0,0.33,0.66,1),na.rm=T),
                     labels=c(0,1,2),include.lowest=T)))
Trend.category<-Trend.category[,c(3,59)]
colnames(Trend.category)<-paste0(colnames(Trend.category),".tertiary")
Trend.tertiary<-left_join(Trend.trend,Trend.category,by=c("ISO3"="ISO3.tertiary"))

Original.model<-glm(pct_slope~Nig.trend+Area+Death+Den.trend+
                      Seat.pop.tertiary+
                      Seat.pop.tertiary:Den.trend,
                    family=gaussian(link="identity"),
                    data=Trend.tertiary,weights=Weight2)
#Calculating R2 for each variable
R2.var<-relaimpo::calc.relimp(Original.model,type='lmg')
R2.var$lmg


# coefficients
results <- model_parameters(Original.model, ci=0.95, vcov="HC3")
Expression=paste0(round(results$Coefficient[-1]*1000,2)," (",
                                     round(results$CI_low[-1]*1000,2),", ",
                                     round(results$CI_high[-1]*1000,2),")")
print(cbind(results$Parameter[-1], Expression))


# 直接对你拟合好的 Original.model 提取标准化系数，并指定使用 HC3 稳健标准误
std_results <- model_parameters(
  Original.model, 
  standardize = "refit", # 重新拟合以获取完全标准化的结果
  vcov = "HC3",        # 指定使用 HC3 稳健标准误
  ci = 0.95              # 95% 置信区间
)
Expression=paste0(round(std_results$Coefficient[-1]*1000,2)," (",
                                     round(std_results$CI_low[-1]*1000,2),", ",
                                     round(std_results$CI_high[-1]*1000,2),")")
print(cbind(results$Parameter[-1], Expression))


vif(Original.model)

Single.model<-glm(pct_slope~Seat.pop.tertiary,
                  family=gaussian(link="identity"),data=Trend.tertiary,weights=Weight2)
bptest(Single.model)
Model.robust<-vcovHC(Single.model,type="HC3")
LR<-coeftest(Single.model,Model.robust)
Result<-data.frame(Var=row.names(LR)[-1],
                   Coef=LR[-1,1]*1000, 
                   Se=LR[-1,2]*1000,
                   Lower=LR[-1,1]*1000-1.96*LR[-1,2]*1000,
                   Upper=LR[-1,1]*1000+1.96*LR[-1,2]*1000,
                   Expression=paste0(round(LR[-1,1]*1000,2)," (",
                                     round(LR[-1,1]*1000-1.96*LR[-1,2]*1000,2),", ",
                                     round(LR[-1,1]*1000+1.96*LR[-1,2]*1000,2),")"),
                   AIC=round(Single.model$aic,2),
                   P=LR[-1,4])

Single.model<-glm(pct_slope~Seat.pop.tertiary:Den.trend,
                  family=gaussian(link="identity"),data=Trend.tertiary,weights=Weight2)
bptest(Single.model)
Model.robust<-vcovHC(Single.model,type="HC3")
LR<-coeftest(Single.model,Model.robust)
Result<-data.frame(Var=row.names(LR)[-1],
                   Coef=LR[-1,1]*1000, 
                   Se=LR[-1,2]*1000,
                   Lower=LR[-1,1]*1000-1.96*LR[-1,2]*1000,
                   Upper=LR[-1,1]*1000+1.96*LR[-1,2]*1000,
                   Expression=paste0(round(LR[-1,1]*1000,2)," (",
                                     round(LR[-1,1]*1000-1.96*LR[-1,2]*1000,2),", ",
                                     round(LR[-1,1]*1000+1.96*LR[-1,2]*1000,2),")"),
                   AIC=round(Single.model$aic,2),
                   P=LR[-1,4])



#####International traffic per capita#####
Amount<-read.csv("Trend/Data/pax_weightd_by_departure_pop.csv")
Amount<-Amount[which(Amount$year<2020),]
Amount.capita<-aggregate(Amount$paxp_int,by=list(Amount$country),median)
colnames(Amount.capita)<-c("country","Median")

Trend.trend<-read.xlsx("Trend/Data/International traffic.xlsx")
Trend.trend$Dea.trend[which(is.na(Trend.trend$Dea.trend))]<-0
Trend.trend<-Trend.trend[,-c(1:3)]
Amount.capita<-left_join(Amount.capita,Trend.trend,by=c("country"))
Amount.capita$Median<-Amount.capita$Median#/1000

#Main model (selected)
Original.model<-glm(Median~Inflow+Rank,
                    family=gaussian(link="identity"),
                    data=Amount.capita)

#Calculating R2 for each variable
R2.var<-relaimpo::calc.relimp(Original.model,type='lmg')
R2.var$lmg

# coefficients
results <- model_parameters(Original.model, ci=0.95, vcov="HC3")
Expression=paste0(round(results$Coefficient[-1]*1000,2)," (",
                                     round(results$CI_low[-1]*1000,2),", ",
                                     round(results$CI_high[-1]*1000,2),")")
print(cbind(results$Parameter[-1], Expression))
Expression=paste0(round(results$Coefficient[-1]*1000,2)," (",
                                     round((results$Coefficient[-1] - 1.96*results$SE[-1])*1000,2),", ",
                                     round((results$Coefficient[-1] + 1.96*results$SE[-1])*1000,2),")")
print(cbind(results$Parameter[-1], Expression))

print(results)

# 直接对你拟合好的 Original.model 提取标准化系数，并指定使用 HC3 稳健标准误
std_results <- model_parameters(
  Original.model, 
  standardize = "refit", # 重新拟合以获取完全标准化的结果
  vcov = "HC3",        # 指定使用 HC3 稳健标准误
  ci = 0.95              # 95% 置信区间
)
Expression=paste0(round(std_results$Coefficient[-1]*1000,2)," (",
                                     round(std_results$CI_low[-1]*1000,2),", ",
                                     round(std_results$CI_high[-1]*1000,2),")")
print(cbind(results$Parameter[-1], Expression))

#Main model (all)
Original.model<-glm(Median~Population+Den.trend+Inflow+SDI.trend+
                      Exp.mer.trend+Years.conflicts+Rank+Dea.trend,
                    family=gaussian(link="identity"),
                    data=Amount.capita)
#Calculating R2 for each variable
R2.var<-relaimpo::calc.relimp(Original.model,type='lmg')
R2.var$lmg

# coefficients
results <- model_parameters(Original.model, ci=0.95, vcov="HC3")
Expression=paste0(round(results$Coefficient[-1]*1000,2)," (",
                                     round(results$CI_low[-1]*1000,2),", ",
                                     round(results$CI_high[-1]*1000,2),")")
print(cbind(results$Parameter[-1], Expression))
Expression=paste0(round(results$Coefficient[-1]*1000,2)," (",
                                     round((results$Coefficient[-1] - 1.96*results$SE[-1])*1000,2),", ",
                                     round((results$Coefficient[-1] + 1.96*results$SE[-1])*1000,2),")")
print(cbind(results$Parameter[-1], Expression))
print(results)


# 直接对你拟合好的 Original.model 提取标准化系数，并指定使用 HC3 稳健标准误
std_results <- model_parameters(
  Original.model, 
  standardize = "refit", # 重新拟合以获取完全标准化的结果
  vcov = "HC3",        # 指定使用 HC3 稳健标准误
  ci = 0.95              # 95% 置信区间
)
Expression=paste0(round(std_results$Coefficient[-1]*1000,2)," (",
                                     round(std_results$CI_low[-1]*1000,2),", ",
                                     round(std_results$CI_high[-1]*1000,2),")")
print(cbind(results$Parameter[-1], Expression))


#Likelihood ratio test
Model.simple<-glm(Median~Inflow+Rank,
                  family=gaussian(link="identity"),
                  data=Amount.capita)
Model.complex<-glm(scale(Median)~scale(Population)+scale(Den.trend)+
                     scale(Inflow)+scale(SDI.trend)+
                     scale(Exp.mer.trend)+scale(Years.conflicts)+
                     scale(Rank)+scale(Dea.trend),
                   family=gaussian(link="identity"),data=Amount.capita)
logLik.simple<-logLik(Model.simple)
logLik.complex<-logLik(Model.complex)
lr.statistic<-(-2)*(logLik.simple-logLik.complex)
df.diff<-Model.complex$rank-Model.simple$rank
P.value<-pchisq(lr.statistic,df.diff,lower.tail=FALSE)

#66
which(colnames(Amount.capita)=="Eld.trend")
Amount.category<-Amount.capita%>%
  mutate(across(c(66),
                ~cut(.x,
                     breaks=quantile(.x,probs=c(0,0.33,0.66,1),na.rm=T),
                     labels=c(0,1,2),include.lowest=T)))
Amount.category<-Amount.category[,c(3,66)]
colnames(Amount.category)<-paste0(colnames(Amount.category),".tertiary")
Amount.tertiary<-left_join(Amount.capita,Amount.category,by=c("ISO3"="ISO3.tertiary"))

Original.model<-glm(Median~Rank+Inflow+
                      Eld.trend.tertiary+
                      Eld.trend.tertiary:Inflow,
                    family=gaussian(link="identity"),
                    data=Amount.tertiary)
#Calculating R2 for each variable
R2.var<-relaimpo::calc.relimp(Original.model,type='lmg')
R2.var$lmg

# coefficients
results <- model_parameters(Original.model, ci=0.95, vcov="HC3")
Expression=paste0(round(results$Coefficient[-1]*1000,2)," (",
                                     round(results$CI_low[-1]*1000,2),", ",
                                     round(results$CI_high[-1]*1000,2),")")
print(cbind(results$Parameter[-1], Expression))
Expression=paste0(round(results$Coefficient[-1]*1000,2)," (",
                                     round((results$Coefficient[-1] - 1.96*results$SE[-1])*1000,2),", ",
                                     round((results$Coefficient[-1] + 1.96*results$SE[-1])*1000,2),")")
print(cbind(results$Parameter[-1], Expression))

# 直接对你拟合好的 Original.model 提取标准化系数，并指定使用 HC3 稳健标准误
std_results <- model_parameters(
  Original.model, 
  standardize = "refit", # 重新拟合以获取完全标准化的结果
  vcov = "HC3",        # 指定使用 HC3 稳健标准误
  ci = 0.95              # 95% 置信区间
)
Expression=paste0(round(std_results$Coefficient[-1]*1000,2)," (",
                                     round(std_results$CI_low[-1]*1000,2),", ",
                                     round(std_results$CI_high[-1]*1000,2),")")
print(cbind(results$Parameter[-1], Expression))
print(results)

vif(Original.model)

Single.model<-glm(Median~Eld.trend.tertiary,
                  family=gaussian(link="identity"),
                  data=Amount.tertiary)
bptest(Single.model)
Model.robust<-vcovHC(Single.model,type="HC3")
LR<-coeftest(Single.model,Model.robust)
Result<-data.frame(Var=row.names(LR)[-1],
                   Coef=LR[-1,1]*1000, 
                   Se=LR[-1,2]*1000,
                   Lower=LR[-1,1]*1000-1.96*LR[-1,2]*1000,
                   Upper=LR[-1,1]*1000+1.96*LR[-1,2]*1000,
                   Expression=paste0(round(LR[-1,1]*1000,2)," (",
                                     round(LR[-1,1]*1000-1.96*LR[-1,2]*1000,2),", ",
                                     round(LR[-1,1]*1000+1.96*LR[-1,2]*1000,2),")"),
                   AIC=round(Single.model$aic,2),
                   P=LR[-1,4])

Single.model<-glm(Median~Eld.trend.tertiary:Inflow,
                  family=gaussian(link="identity"),
                  data=Amount.tertiary)
bptest(Single.model)
Model.robust<-vcovHC(Single.model,type="HC3")
LR<-coeftest(Single.model,Model.robust)
Result<-data.frame(Var=row.names(LR)[-1],
                   Coef=LR[-1,1]*1000, 
                   Se=LR[-1,2]*1000,
                   Lower=LR[-1,1]*1000-1.96*LR[-1,2]*1000,
                   Upper=LR[-1,1]*1000+1.96*LR[-1,2]*1000,
                   Expression=paste0(round(LR[-1,1]*1000,2)," (",
                                     round(LR[-1,1]*1000-1.96*LR[-1,2]*1000,2),", ",
                                     round(LR[-1,1]*1000+1.96*LR[-1,2]*1000,2),")"),
                   AIC=round(Single.model$aic,2),
                   P=LR[-1,4])


#####Domestic traffic per capita#####
Amount<-read.csv("Trend/Data/pax_weightd_by_departure_pop.csv")
Amount<-Amount[which(Amount$year<2020),]
Amount.capita<-aggregate(Amount$paxp_dom,by=list(Amount$country),median)
colnames(Amount.capita)<-c("country","Median")
Amount.capita<-Amount.capita[-which(Amount.capita$Median==0),]

# Amount<-read.csv("Trend/Data/Dom pax per 1000.csv")
# Amount<-reshape2::melt(data=Amount,id.vars="year")
# Amount<-Amount[-which(Amount$year>2019),]
# Amount.capita<-aggregate(Amount$value,by=list(Amount$variable),median)
# colnames(Amount.capita)<-c("country","Median")
# Amount.capita<-Amount.capita[-which(Amount.capita$Median==0),]

Trend.trend<-read.xlsx("Trend/Data/Domestic segment.xlsx")
Trend.trend$HSR<-0
Trend.trend$HSR[which(Trend.trend$country=="CN"|Trend.trend$country=="ES"|
                        Trend.trend$country=="FR"|Trend.trend$country=="DE"|
                        Trend.trend$country=="JP"|Trend.trend$country=="SE"|
                        Trend.trend$country=="KR"|Trend.trend$country=="GB"|
                        Trend.trend$country=="IT"|Trend.trend$country=="TR"|
                        Trend.trend$country=="FI"|Trend.trend$country=="RU"|
                        Trend.trend$country=="UZ"|Trend.trend$country=="SA"|
                        Trend.trend$country=="TW"|Trend.trend$country=="PL"|
                        Trend.trend$country=="AT"|Trend.trend$country=="BE"|
                        Trend.trend$country=="PT"|Trend.trend$country=="MA"|
                        Trend.trend$country=="NL"|Trend.trend$country=="NO"|
                        Trend.trend$country=="CH"|Trend.trend$country=="GR"|
                        Trend.trend$country=="ID"|Trend.trend$country=="US"|
                        Trend.trend$country=="RS"|Trend.trend$country=="DK"|
                        Trend.trend$country=="HK")]<-1

Trend.trend<-Trend.trend[,-c(1,2)]
Amount.capita<-left_join(Amount.capita,Trend.trend,by=c("country"))
Amount.capita$Median<-Amount.capita$Median #/1000
Amount.capita$country<-NULL

#Main model (selected)
Original.model<-glm(Median~Density.grid+Refugee+Pop.trend+
                      Une.trend+Seat.pop,
                    family=gaussian(link="identity"),
                    data=Amount.capita)

#Calculating R2 for each variable
R2.var<-relaimpo::calc.relimp(Original.model,type='lmg')
R2.var$lmg


# coefficients
results <- model_parameters(Original.model, ci=0.95, vcov="HC3")
# 直接对你拟合好的 Original.model 提取标准化系数，并指定使用 HC3 稳健标准误
std_results <- model_parameters(
  Original.model, 
  standardize = "refit", # 重新拟合以获取完全标准化的结果
  vcov = "HC3",        # 指定使用 HC3 稳健标准误
  ci = 0.95              # 95% 置信区间
)

Expression=paste0(round(results$Coefficient[-1]*1000,2)," (",
                                     round(results$CI_low[-1]*1000,2),", ",
                                     round(results$CI_high[-1]*1000,2),")")
print(cbind(results$Parameter[-1], Expression))

Expression=paste0(round(std_results$Coefficient[-1]*1000,2)," (",
                                     round(std_results$CI_low[-1]*1000,2),", ",
                                     round(std_results$CI_high[-1]*1000,2),")")
print(cbind(results$Parameter[-1], Expression))

Expression=paste0(round(results$Coefficient[-1]*1000,2)," (",
                                     round((results$Coefficient[-1] - 1.96*results$SE[-1])*1000,2),", ",
                                     round((results$Coefficient[-1] + 1.96*results$SE[-1])*1000,2),")")
print(cbind(results$Parameter[-1], Expression))

print(results)

#Main model (all)
Original.model<-glm(Median~Density.grid+Refugee+Pop.trend+
                      GNI.trend+Une.trend+Urb.trend+Seat.pop+
                      Death+Dea.trend,
                    family=gaussian(link="identity"),
                    data=Amount.capita)
#Calculating R2 for each variable
R2.var<-relaimpo::calc.relimp(Original.model,type='lmg')
R2.var$lmg



# coefficients
results <- model_parameters(Original.model, ci=0.95, vcov="HC3")
# 直接对你拟合好的 Original.model 提取标准化系数，并指定使用 HC3 稳健标准误
std_results <- model_parameters(
  Original.model, 
  standardize = "refit", # 重新拟合以获取完全标准化的结果
  vcov = "HC3",        # 指定使用 HC3 稳健标准误
  ci = 0.95              # 95% 置信区间
)

Expression=paste0(round(results$Coefficient[-1]*1000,2)," (",
                                     round((results$Coefficient[-1] - 1.96*results$SE[-1])*1000,2),", ",
                                     round((results$Coefficient[-1] + 1.96*results$SE[-1])*1000,2),")")
print(cbind(results$Parameter[-1], Expression))

Expression=paste0(round(results$Coefficient[-1]*1000,2)," (",
                                     round(results$CI_low[-1]*1000,2),", ",
                                     round(results$CI_high[-1]*1000,2),")")
print(cbind(results$Parameter[-1], Expression))

Expression=paste0(round(std_results$Coefficient[-1]*1000,2)," (",
                                     round(std_results$CI_low[-1]*1000,2),", ",
                                     round(std_results$CI_high[-1]*1000,2),")")
print(cbind(results$Parameter[-1], Expression))

print(results)

#Likelihood ratio test
Model.simple<-glm(Median~Density.grid+Refugee+Pop.trend+
                    Une.trend+Seat.pop,
                  family=gaussian(link="identity"),
                  data=Amount.capita)
Model.complex<-glm(Median~Density.grid+Refugee+Pop.trend+
                     GNI.trend+Une.trend+Urb.trend+Seat.pop+
                     Death+Dea.trend,
                   family=gaussian(link="identity"),
                   data=Amount.capita)
logLik.simple<-logLik(Model.simple)
logLik.complex<-logLik(Model.complex)
lr.statistic<-(-2)*(logLik.simple-logLik.complex)
df.diff<-Model.complex$rank-Model.simple$rank
P.value<-pchisq(lr.statistic,df.diff,lower.tail=FALSE)

#59
which(colnames(Amount.capita)=="Segment.pop")
Amount.category<-Amount.capita%>%
  mutate(across(c(59),
                ~cut(.x,
                     breaks=quantile(.x,probs=c(0,0.33,0.66,1),na.rm=T),
                     labels=c(0,1,2),include.lowest=T)))
Amount.category<-Amount.category[,c(2,59)]
colnames(Amount.category)<-paste0(colnames(Amount.category),".tertiary")
Amount.tertiary<-left_join(Amount.capita, Amount.category,by=c("ISO3"="ISO3.tertiary"))
Amount.tertiary<-Amount.tertiary[-which(is.na(Amount.tertiary$ISO3)),]

Original.model<-glm(Median~Density.grid+Refugee+
                      Pop.trend+Unemployment+Seat.pop+
                      Segment.pop.tertiary+
                      Segment.pop.tertiary:Seat.pop,
                    family=gaussian(link="identity"),
                    data=Amount.tertiary)
#Calculating R2 for each variable
R2.var<-relaimpo::calc.relimp(Original.model,type='lmg')
R2.var$lmg


# coefficients
results <- model_parameters(Original.model, ci=0.95, vcov="HC3")
Expression=paste0(round(results$Coefficient[-1]*1000,2)," (",
                                     round(results$CI_low[-1]*1000,2),", ",
                                     round(results$CI_high[-1]*1000,2),")")
print(cbind(results$Parameter[-1], Expression))


# 直接对你拟合好的 Original.model 提取标准化系数，并指定使用 HC3 稳健标准误
std_results <- model_parameters(
  Original.model, 
  standardize = "refit", # 重新拟合以获取完全标准化的结果
  vcov = "HC3",        # 指定使用 HC3 稳健标准误
  ci = 0.95              # 95% 置信区间
)
Expression=paste0(round(std_results$Coefficient[-1]*1000,2)," (",
                                     round(std_results$CI_low[-1]*1000,2),", ",
                                     round(std_results$CI_high[-1]*1000,2),")")
print(cbind(results$Parameter[-1], Expression))
print(results)

vif(Original.model)


Single.model<-glm(Median~Segment.pop.tertiary:Seat.pop,
                  family=gaussian(link="identity"),
                  data=Amount.tertiary)
bptest(Single.model)
Model.robust<-vcovHC(Single.model,type="HC3")
LR<-coeftest(Single.model,Model.robust)
Result<-data.frame(Var=row.names(LR)[-1],
                   Coef=LR[-1,1]*1000, 
                   Se=LR[-1,2]*1000,
                   Lower=LR[-1,1]*1000-1.96*LR[-1,2]*1000,
                   Upper=LR[-1,1]*1000+1.96*LR[-1,2]*1000,
                   Expression=paste0(round(LR[-1,1]*1000,2)," (",
                                     round(LR[-1,1]*1000-1.96*LR[-1,2]*1000,2),", ",
                                     round(LR[-1,1]*1000+1.96*LR[-1,2]*1000,2),")"),
                   AIC=round(Single.model$aic,2),
                   P=LR[-1,4])
Result


