devtools::install_github("laresbernardo/lares")

library(RcmdrMisc)
library(corrplot)
library(tidyverse)
library(dplyr)
library(forcats)
library(car)
library(lares)
library(MASS)
library(caret)
library(caTools)
library("gplots")
library("effects")
source("BCA_functions_source_file.R")
#age would be a factor because older people(older than 35) would already have a RRSP and younger wouldn't have a RRSP so I think the younger people would have a higher
#probability of obtaing a RRSP.Income would be a factor too because the more income someone has the higher liklihodd they would obtain a RRSP
#The more accounts a person has for example if someone has 3 accounts in vancity (TFS, chequsing and saving) they might be more inclinced to 
#get a RRSP.Money in savings account if someone has a lot of money in their saving account they would be more inclined to move some of that money into 
#a RRSP.likewise with money in their Chequing account.

#2
#the age variable in my mental model can be mapped to the age in the dataset. the age variable has a non-linear relationship with the target variable
#because older people(older than 35) would already have a RRSP and younger wouldn't have a RRSP so I think the younger people would have a higher
#probability of obtaing a RRSP. I think this would be a concave relationship.
#Income can be measured by avginc1 I think this would be a nonlinear relationship because the more money someone has the more disposable income they have
# to invest into a RRSP, hence i think this would be a convex relationship.
#the number accounts a person can be measured by the TOTSERV(total number of distinct services (distinct product lines) held) in the dataset. I think this 
#would be a linear relationship with the target variable because the more products someone holds the more likelihood they would want to hold other products.
#for example  if someone has a saving and chequing account they might to try out other accounts as well.
#Amount of money in saving account can be measured by BALSAV in the data set. I think this would be a linear relationship with the target variable
#because 
VC<-read.csv("VC_data.csv",stringsAsFactors = TRUE)
variable.summary(VC)
#exclude:gendf: being female would not have impact of opening a RRSP account because RRSP is for retirement and is nondiscriminatory for gender,
#gendm:being male would not have impact of opening a RRSP account because RRSP is for retirement and is nondiscriminatory for gender,
#pcode: postal code does not make sense to include this in the model because if someone has particular postal code that does not increases their odds of 
#getting a RRSP.
#unique: unique is a identification number which is absolutely useless in our model.

VC<-VC[,-c(2,4:6)]
VC$valsegm<-fct_collapse(VC$valsegm,
                         valsegtop20=c("A","B","C"),
                         D="D",
                         E="E")

corld<-cor(select_if(VC,is.numeric))
corrplot(corld,method = "number",
         typer="lower",
         diag=FALSE,
         number.cex = .7)

corr_cross(VC, # name of dataset
           max_pvalue = 0.05, # display only significant correlations (at 5% level)
           top = 10 # display top 10 couples of variables (by correlation coefficient)
)
#NINDINC1 and numrr are highly correlated, avginc1 + avginv1, DUMMNORGG + BALMRGG,DUMNOLOAN+BALLOAN,CHNMSERV +CHNMPRD


VC$APURCH.num <- if_else(VC$APURCH == "Y",1,0)

# plot of means -----------------------------------------------------------


VC$BALSAV.cat <- binVariable(VC$BALSAV, bins = 3,
                             method = "intervals",
                            labels = NULL)

plotmeans(APURCH.num~ BALSAV.cat, data = VC)

VC$TXTEL.cat <- binVariable(VC$TXTEL, bins = 2,
                            method = "intervals",
                            labels = NULL)

plotmeans(APURCH.num~ TXTEL.cat, data = VC)

VC$TXTEL.cat <- binVariable(VC$TXTEL, bins = 2,
                            method = "intervals",
                            labels = NULL)

plotmeans(APURCH.num~ TXTEL.cat, data = VC)

VC$BALLOC.cat <- binVariable(VC$BALLOC, bins = 3,
                             method = "intervals",
                             labels = NULL)

plotmeans(APURCH.num~ BALLOC.cat, data = VC)

VC$BALLOAN.cat <- binVariable(VC$BALLOAN, bins = 3,
                             method = "intervals",
                             labels = NULL)

plotmeans(APURCH.num~ BALLOAN.cat, data = VC)

VC$BALMRGG.cat <- binVariable(VC$BALMRGG, bins = 3,
                              method = "intervals",
                              labels = NULL)

plotmeans(APURCH.num~ BALMRGG.cat, data = VC)


VC$BALCHQ.cat <- binVariable(VC$BALCHQ, bins = 4,
                             method = "proportions",
                             labels = NULL)

plotmeans(APURCH.num~ BALCHQ.cat, data = VC)



VC$TXATM.cat <- binVariable(VC$TXATM, bins = 3,
                            method = "proportions",
                            labels = NULL)

plotmeans(APURCH.num~ TXATM.cat, data = VC)

VC$TXCHQ.cat <- binVariable(VC$TXCHQ, bins = 3,
                            method = "proportions",
                            labels = NULL)

plotmeans(APURCH.num~ TXCHQ.cat, data = VC)

VC$TXCHQ.cat <- binVariable(VC$TXCHQ, bins = 3,
                            method = "proportions",
                            labels = NULL)

plotmeans(APURCH~ TXCHQ.cat, data = VC)

VC$TXTEL.cat <- binVariable(VC$TOTSERV, bins = 3,
                            method = "proportions",
                            labels = NULL)

plotmeans(APURCH.num~ TXTEL.cat, data = VC)

VC$NINDINC1.cat <- binVariable(VC$NINDINC1, bins = 8,
                               method = "proportions",
                               labels = NULL)

plotmeans(APURCH.num~ NINDINC1.cat, data = VC)

VC$numrr1.cat <- binVariable(VC$numrr1, bins =10,
                             method = "proportions",
                             labels = NULL)

plotmeans(APURCH.num~ numrr1.cat, data = VC)


VC$avginv1.cat <- binVariable(VC$avginv1, bins =10,
                              method = "proportions",
                              labels = NULL)

plotmeans(APURCH.num~ avginv1.cat, data = VC)

#Non-linear Relationships
par(mfrow=c(1,2))
VC$age.cat <- binVariable(VC$age, bins = 6,
                          method = "proportions",
                          labels = NULL)

plotmeans(APURCH.num~ age.cat, data = VC , xlab = 'Age', ylab ="Purchas RRSP")

VC$TOTDEP.cat <- binVariable(VC$TOTDEP, bins = 9,
                             method = "proportions",
                             labels = NULL)

plotmeans(APURCH.num~ TOTDEP.cat, data = VC, xlab = 'TotDep', ylab ="Purchas RRSP")

VC$TXBRAN.cat <- binVariable(VC$TXBRAN, bins = 7,
                             method = "proportions",
                             labels = NULL)

plotmeans(APURCH.num~ TXBRAN.cat, data = VC, xlab = 'TXbran', ylab ="Purchas RRSP")

VC$avginc1.cat <- binVariable(VC$avginc1, bins =10,
                              method = "proportions",
                              labels = NULL)

plotmeans(APURCH.num~ avginc1.cat, data = VC, xlab = 'average income', ylab ="Purchas RRSP")



# feature engineering and lift charts -------------------------------------


VC$age.log<-log(VC$age)
VC$TOTDEP.log<-log(VC$TOTDEP+1)
VC$TXBRAN.log<-log(VC$TXBRAN+1)
VC$avginc1.log<-log(VC$avginc1)


VC$Sample<-create.samples(VC,
                          est = 0.50, # allocate 50% to estimation sample
                          val = 0.50, # 50% to validation sample
                          rand.seed = 25)

trainingset<-filter(VC,VC$Sample=="Estimation")
testset<-filter(VC,VC$Sample=="Validation")
set.seed(25)
#atmcrd,age*,patdep,DUMNOCHQ,BALCHQ,DUMNOSAV*,BALSAV*,TOTDEP*,DUMNOLOAN,BALLOAN,DUMNOLOC,DUMNOMRGG,BALMRGG,NEWLOC,NEWMRGG,TXBRAN,TXATM
#TXPOS,TXCHQ,TXWEB,TOTSERV,valsegmS
logit<-glm(APURCH~.,
           data = trainingset[,1:30],
           family=binomial)
summary(logit)
#2990
logit2<-glm(APURCH~atmcrd+age+paydep+DUMNOCHQ+BALCHQ+DUMNOSAV+BALSAV+TOTDEP+DUMNOLOAN+BALLOAN+DUMNOLOC+DUMNOMRGG+BALMRGG+NEWLOC+NEWMRGG+TXBRAN+TXATM+TXPOS
            +TXCHQ+TXWEB+TOTSERV+valsegm,
            data = trainingset,
            family=binomial)
summary(logit2)
#2985.1
logit3<-glm(APURCH~atmcrd+age+paydep+BALCHQ+BALSAV+DUMNOLOAN+DUMNOLOC+BALMRGG+NEWLOC+TXBRAN+TXCHQ+TOTSERV+valsegm,
            data = trainingset,
            family=binomial)
summary(logit3)
#2974.9
logit4<-glm(APURCH~atmcrd+age+paydep+BALCHQ+DUMNOLOAN+BALMRGG+TXBRAN+TOTSERV+valsegm,
            data = trainingset,
            family=binomial)
summary(logit4)
#2983.1
logit5<-glm(APURCH~atmcrd+age+paydep+DUMNOCHQ+BALCHQ+DUMNOSAV+BALSAV+TOTDEP+DUMNOLOAN+BALLOAN+DUMNOLOC+DUMNOMRGG+BALMRGG+NEWLOC+NEWMRGG+TXBRAN+TXATM+TXPOS
            +TXCHQ+TXWEB+TOTSERV+valsegm,
            data = trainingset,
            family=binomial)
summary(logit5)
#2985.1
logit6<-glm(APURCH~atmcrd+age.log+paydep+DUMNOCHQ+BALCHQ+DUMNOSAV+BALSAV+TOTDEP+DUMNOLOAN+BALLOAN+DUMNOLOC
            +DUMNOMRGG+BALMRGG+NEWLOC+NEWMRGG+TXBRAN+TXATM+TXPOS
            +TXCHQ+TXWEB+TOTSERV+valsegm,
            data = trainingset,
            family=binomial)
summary(logit6)
#2988.1
logit7<-glm(APURCH~atmcrd+age.log+paydep+DUMNOCHQ+BALCHQ+DUMNOSAV+BALSAV+TOTDEP.log+DUMNOLOAN+BALLOAN+DUMNOLOC+DUMNOMRGG+BALMRGG+NEWLOC+NEWMRGG+TXBRAN+TXATM+TXPOS
            +TXCHQ+TXWEB+TOTSERV+valsegm,
            data = trainingset,
            family=binomial)
summary(logit7)
#2953.5
logit8<-glm(APURCH~atmcrd+age.log+paydep+DUMNOCHQ+BALCHQ+DUMNOSAV+BALSAV+TOTDEP.log+DUMNOLOAN+BALLOAN+DUMNOLOC+DUMNOMRGG+BALMRGG+NEWLOC+NEWMRGG+
              TXBRAN.log+TXATM+TXPOS
            +TXCHQ+TXWEB+TOTSERV+valsegm,
            data = trainingset,
            family=binomial)
summary(logit8)
#2951
logit9<-glm(APURCH~atmcrd+age.log+paydep+DUMNOCHQ+BALCHQ+DUMNOSAV+BALSAV+TOTDEP.log+DUMNOLOAN+BALLOAN+DUMNOLOC+DUMNOMRGG+BALMRGG+NEWLOC+NEWMRGG+
              TXBRAN.log+TXATM+TXPOS
            +TXCHQ+TXWEB+TOTSERV+valsegm+avginc1.log,
            data = trainingset,
            family=binomial)
summary(logit9)
#2952.9
logit10<-glm(APURCH~atmcrd+age.log+paydep+DUMNOCHQ+TOTDEP.log+DUMNOLOAN+BALMRGG+
               TXBRAN.log+TOTSERV+valsegm,
             data = trainingset,
             family=binomial)
summary(logit10)
#2946.9
logit11<-glm(APURCH~atmcrd+age.log+paydep+TOTDEP.log+DUMNOLOAN+BALMRGG+
               TXBRAN.log+TOTSERV+valsegm,
             data = trainingset,
             family=binomial)
summary(logit11)
#2944.9
lift.chart(modelList = c("logit7","logit8","logit9","logit10","logit11"),
           data = filter(VC, Sample == "Validation"), # or "Validation"
           targLevel = "Y", # Desired Level of target variable "MonthGive"
           trueResp = 0.022, # True response rate in original dataset
           type = "cumulative")
lift.chart(modelList = c("logit11"),
           data = filter(VC, Sample == "Validation"), # or "Validation"
           targLevel = "Y", # Desired Level of target variable "MonthGive"
           trueResp = 0.022, # True response rate in original dataset
           type = "cumulative", # Or "incremental"
           sub = "cumulative Validation Set") # Specify chart's subtitle
lift.chart(modelList = c("logit11"),
           data = filter(VC, Sample == "Validation"), # or "Validation"
           targLevel = "Y", # Desired Level of target variable "MonthGive"
           trueResp = 0.022, # True response rate in original dataset
           type = "incremental", # Or "incremental"
           sub = "incremental Validation Set") # Specify chart's subtitle

