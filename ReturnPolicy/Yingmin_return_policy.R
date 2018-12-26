#==================================================
# set up
#==================================================
```{r}
rm(list=ls())
setwd('/Users/YingminWang/Desktop/OMIS\ 2392/final\ -\ Return\ policy\ change')
dir()

install.packages('readstata13')
#install packages
#install.packages("ggeffects")
#install.packages("QuantPsyc")
#install.packages("VIF")
#install.packages("usdm")
#install.packages("lmtest")
#install.packages("multiwayvcov")
#install.packages("sandwich")
#install.packages("AER")
#install.packages("aod")
#install.packages("mfx")
install.packages("msm")

# Load libraries everytime you start a session
library(stargazer)
library(gdata)
library(ggplot2)
library(psych) 
library(ggeffects)
library(QuantPsyc)
#library(usdm)
library(lmtest)
library(multiwayvcov)
library(sandwich)
library(foreign)
library(AER)
library(aod)
library(Rcpp)
library(mfx)
library(nnet)
library(reshape2)
library(msm)
library(readstata13)

options(scipen=9)
```     
#===================================================
# read & explore data
#===================================================
BM_month_prod <- read.dta13('BM store monthly prod_cat sales-returns.dta')
# avgfemale: (female/all customer purchased a product) in a given product category in a month
BM_month_store <- read.dta13('BM store monthly sales-returns.dta')
# avgfemale: (female/all customer at a store)  in a given month
Online_daily_prod <- read.dta13('Online store daily prod_cat sales-returns.dta')

Online_daily_store <- read.dta13('Online store daily sales-returns.dta')


stargazer(BM_month_prod, type='text', title='descriptive analysis',
          digits=2, median=TRUE, iqr=TRUE)

stargazer(BM_month_store, type='text', title='descriptive analysis',
          digits=2, median=TRUE, iqr=TRUE)

stargazer(Online_daily_prod, type='text', title='descriptive analysis',
          digits=2, median=TRUE, iqr=TRUE)

stargazer(Online_daily_store, type='text', title='descriptive analysis',
          digits=2, median=TRUE, iqr=TRUE)



#===================================================
# Question1: What is the impact of the policy change on online channel sales?
#===================================================
stargazer(Online_daily_store, type='text', title='descriptive analysis',
          digits=2, median=TRUE, iqr=TRUE)

# Replace missing value with mean
Online_daily_store$avg_female[is.na(Online_daily_store$avg_female)] <- 0.62

Online_daily_store$avg_age[is.na(Online_daily_store$avg_age)] <- 4.37

Online_daily_store$avg_income[is.na(Online_daily_store$avg_income)] <-5.33

Online_daily_store$avg_homeowner[is.na(Online_daily_store$avg_homeowner)] <- 0.68

Online_daily_store$avg_residency[is.na(Online_daily_store$avg_residency)] <- 7.24

Online_daily_store$avg_childowner[is.na(Online_daily_store$avg_childowner)] <- 0.44


# set dummy variable for time group
#       time_group =0 : before policy change (2013.04.01-2013.09.30)
#       time_group =1 : after policy change(2013.10.01 - 2014.03.01)
# set dummy variable for store group
#       store_group = 0 : without policy change(store number=10)
#       store_group = 1 : with policy change(store number =2 or 6 )
Online_daily_store $ store_group <- ifelse(Online_daily_store $ store_number %in% c(2,6), 1, 0)
Online_daily_store $ time_group <- ifelse(Online_daily_store $month_dummy %in% c(4,5,6,7,8,9),0,1)





#######
# salesvalue
#######

### explore data
ggplot(Online_daily_store, aes(x=salesvalue)) + geom_histogram(color='green')
ggplot(Online_daily_store, aes(x=log(salesvalue))) + geom_histogram(color='green')
Online_daily_store$lnsalesvalue <- log(Online_daily_store$salesvalue+1)


### multicollinearity
df1a <- Online_daily_store[c('lnsalesvalue','store_group','time_group', 'avg_female','avg_age','avg_income',
                             'avg_homeowner','avg_residency','avg_childowner')]
cor(df1a)
df1b <- Online_daily_store[c('store_group','time_group', 'avg_female','avg_age','avg_income',
                             'avg_homeowner','avg_residency','avg_childowner')]

vif(df1b)

### linear model

lm0 <- lm(lnsalesvalue ~ time_group*store_group + avg_female + avg_age + avg_income + 
                   avg_homeowner + avg_residency +avg_childowner, data=Online_daily_store)
step<-stepAIC(lm0, direction='both')
step$anova

lm0a<- lm(lnsalesvalue ~ time_group*store_group + avg_female + avg_age + avg_income + 
                  avg_homeowner +avg_childowner, data=Online_daily_store)
anova(lm0,lm0a, test='Chisq')
# model with avg_trsidency is better (lm0)

lm1a<-lm(lnsalesvalue ~ time_group*store_group + avg_female + avg_age + avg_income + 
                 avg_homeowner +avg_childowner+ avg_residency, data=Online_daily_store)
        
stargazer(lm1a, type='text',title='regression result',
          column.labels = 'lm1a', df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))
# the impact of return policy on lnsalesvalue is insignificant 

##### Heteroskedasticity 
gqtest(lm1a)
bptest(lm1a)

HWrobstder <- sqrt(diag(vcovHC(lm1a, type='HC1')))

stargazer(lm1a, type='text',title='Online_Salesvalue', se=list(HWrobstder),
          column.labels = c("HW-Robust SE"), df=FALSE, digits=2, 
          star.cutoffs = c(0.05,0.01,0.001))

# the impact of return policy on lnsalevalue is insignificant 


#### Marginal Effect
meffects1a <- ggpredict(lm1a, terms=c('time_group', 'store_group'))
ggplot(meffects1a, aes(x, predicted, color=group)) + geom_line(size=1.3) + 
        xlab('Time') + ylab('ln(Sales Value)') + 
        scale_color_discrete(labels=c('Policy changed', 'Policy unchanged'))+ 
        scale_x_continuous(breaks = c(0,1), labels = c('Before policy change', 'After policy change'))


#######
# salesquantity
#######

ggplot(Online_daily_store, aes(salesquantity)) + geom_histogram(color = 'green')
ggplot(Online_daily_store, aes(log(salesquantity))) + geom_histogram(color = 'green')
# Online_daily_store$lnsalesquantity <- log(Online_daily_store$salesquantity+1)

### multicollinearity
df1c <- Online_daily_store[c('salesquantity','store_group','time_group', 'avg_female','avg_age','avg_income',
                             'avg_homeowner','avg_residency','avg_childowner')]
cor(df1c)

df1d <- Online_daily_store[c('store_group','time_group', 'avg_female','avg_age','avg_income',
                             'avg_homeowner','avg_residency','avg_childowner')]

vif(df1d)


####  poisson model
poisson1 <- glm(salesquantity~ time_group*store_group + avg_female + avg_age + avg_income + 
                        avg_homeowner + avg_residency +avg_childowner, 
                data=Online_daily_store, family = 'poisson')

poisson1a <-glm(salesquantity~ time_group*store_group + avg_female + avg_age + avg_income + 
                        avg_homeowner +avg_childowner, 
                data=Online_daily_store, family = 'poisson')

anova(poisson1, poisson1a, test='Chisq')
# significant ==> model with avg_residency is better(poisson1)

stargazer(poisson1, type='text', title='regression model', column.labels ='poisson1',
          apply.coef = exp, t.auto=F, p.auto=F,
          digits=2, df=FALSE, star.cutoffs=c(0.05,0.01,0.001) )
# with return policy, sales quantity will decrease 3%.


## model fit
poisson1b <- glm(salesquantity~1, data=Online_daily_store, family='poisson')
lrtest(poisson1,poisson1b)
# significant ==> poisson model does not fit the data


### Heteroskedasticity
bptest(poisson1)
gqtest(poisson1)
# significant ==> has heteroskedasticity

HWrobstder2 <- sqrt(diag(vcovHC(poisson1, type='HC1')))

stargazer(poisson1, type='text', title='regression model', column.labels ='poisson1-IRR-robust SE',
          apply.coef = exp, t.auto=F, p.auto=F,
          se=list(HWrobstder2),
          digits=2, df=FALSE, star.cutoffs=c(0.05,0.01,0.001) )

# with return policy, sales quantity will decrease by 3%.



####  negative binomial model
negbin1 <- glm.nb(salesquantity ~ time_group*store_group + avg_female + avg_age + avg_income +
                                 avg_homeowner + avg_residency +avg_childowner, 
                                data=Online_daily_store) 

negbin1a <- glm.nb(salesquantity ~ time_group*store_group + avg_female + avg_age + avg_income +
                          avg_homeowner + avg_childowner, 
                  data=Online_daily_store) 

anova(negbin1, negbin1a, test='Chisq')
# insignificant ==> with avg_residency is better (negbin1)

stargazer(negbin1,  title="Regression Results", type="text", column.labels=c("negbin1a-IRR"),
          apply.coef = exp, t.auto=F, p.auto=F,
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))
# return policy has no significant change on sales quantity


### Heterosckedasticity
gqtest(negbin1)    #significant
bptest(negbin1)    #significant

HWrobstder3<- sqrt(diag(vcovHC(negbin1, type='HC1')))

stargazer(negbin1,  title="Online_Salesquantity", type="text", column.labels=c("negbin1a-IRR-robust SE"),
          apply.coef = exp, t.auto=F, p.auto=F,
          se=list(HWrobstder3),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))
# interaction still insignificant ==> return policy has no significant change on sales quantity


### MODEL FIT
negbin1b <- glm.nb(salesquantity~1, data=Online_daily_store)
lrtest(negbin1, negbin1b)  #significant==> negative binomial model fits the data



######## CHOOSE MODEL BETWEEN POISSON & NEGATIVE BINOMIAL
lrtest(poisson1, negbin1)
# significant ==> choose negative binomial 

####### MARGINAL EFFECTS

meffects1b <- ggpredict(negbin1, terms=c('time_group', 'store_group'))
ggplot(meffects1b, aes(x, predicted, color=group)) + geom_line(size=1.3) + 
        xlab('Time') + ylab('Sales Quantity') + 
        scale_color_discrete(labels=c('Policy changed', 'Policy unchanged'))+ 
        scale_x_continuous(breaks = c(0,1), labels = c('Before policy change', 'After policy change'))

#===========
#Conclusion: return policy change has no significant impact on online salesvalue and sales quantity
#===========



#===================================================
# Question2: What is the impact of the policy change on physical store sales?
#===================================================
stargazer(BM_month_store, type='text', title='descriptive analysis',
          digits=2, median=TRUE, iqr=TRUE) 

# Replace missing value with mean
BM_month_store$avg_female[is.na(BM_month_store$avg_female)] <- 0.54

BM_month_store$avg_age[is.na(BM_month_store$avg_age)] <- 4.75

BM_month_store$avg_income[is.na(BM_month_store$avg_income)] <- 5.21

BM_month_store$avg_homeowner[is.na(BM_month_store$avg_homeowner)] <- 0.68

BM_month_store$avg_residency[is.na(BM_month_store$avg_residency)] <- 7.18

BM_month_store$avg_childowner[is.na(BM_month_store$avg_childowner)] <- 0.42

BM_month_store$store_average_price[is.na(BM_month_store$store_average_price)] <- 365.19

BM_month_store$store_number_of_skus[is.na(BM_month_store$store_number_of_skus)] <- 223.38

BM_month_store$sa_gender[is.na(BM_month_store$sa_gender)] <- 0.83

BM_month_store$sa_full_time[is.na(BM_month_store$sa_full_time)] <- 0.64

BM_month_store$sa_avg_years_of_exp[is.na(BM_month_store$sa_avg_years_of_exp)] <- 3.85

BM_month_store$sa_married[is.na(BM_month_store$sa_married)] <- 0.33

BM_month_store$sa_avg_rate_of_pay[is.na(BM_month_store$sa_avg_rate_of_pay)] <- 9.83

BM_month_store$sa_dependent[is.na(BM_month_store$sa_dependent)] <- 0.21

BM_month_store$sales_volume_group[is.na(BM_month_store$sales_volume_group)] <- 2.66


# set dummy variable for time group
#       time_group =0 : before policy change (2013.04.01-2013.09.30)
#       time_group =1 : after policy change(2013.10.01 - 2014.03.01)
# set dummy variable for brand group
#       brand_group = 0 : without policy change(brand number=8)
#       brand_group = 1 : with policy change(brand number =2 or 5 or 3 )

BM_month_store $ brand_group <- ifelse(BM_month_store $ brand_number == 8, 0, 1)
BM_month_store $ time_group <- ifelse(BM_month_store $ month_dummy %in% c(4,5,6,7,8,9),0,1)

########
# salesvalue
########

#### EXPLORE DATA
ggplot(BM_month_store, aes(salesvalue)) + geom_histogram(color='green')
ggplot(BM_month_store, aes(log(salesvalue))) + geom_histogram(color='green')
BM_month_store$lnsalesvalue <- log(BM_month_store$salesvalue+1)

#### MULTICOLINEARITY

df2a <- BM_month_store[c('lnsalesvalue', 'brand_group','time_group','avg_female', 'avg_age', 
                        'avg_income','avg_homeowner','avg_residency', 'avg_childowner', 
                        'store_average_price','store_number_of_skus','sa_gender', 'sa_full_time',
                        'sa_avg_years_of_exp','sa_married', 'sa_avg_rate_of_pay','sa_dependent',
                        'sales_volume_group'
                        )]
cor(df2a)

df2b <- BM_month_store[c( 'brand_group','time_group','avg_female', 'avg_age', 
                         'avg_income','avg_homeowner','avg_residency', 'avg_childowner', 
                         'store_average_price','store_number_of_skus','sa_gender', 'sa_full_time',
                         'sa_avg_years_of_exp','sa_married', 'sa_avg_rate_of_pay','sa_dependent',
                         'sales_volume_group'
                         )]
vif(df2b)    # sales_volume_group >3 


####### Linear Model
lm2a <- lm(lnsalesvalue~time_group * brand_group + avg_female + avg_age + avg_income + avg_homeowner + 
                   avg_residency + avg_childowner + store_number_of_skus + store_average_price+
                   sa_gender + sa_full_time+sa_avg_years_of_exp +sa_married + sa_avg_rate_of_pay+
                   sa_dependent+sales_volume_group
           , data=BM_month_store)

step<- stepAIC(lm2a, direction = 'both')
step$anova

lm2 <- lm(lnsalesvalue~time_group * brand_group + avg_female + avg_age + avg_income + avg_homeowner + 
                  avg_residency   +avg_childowner+ log(store_number_of_skus) + store_average_price+
                  sa_gender + sa_full_time+sa_avg_years_of_exp +sa_married + sa_avg_rate_of_pay+
                  sa_dependent
          , data=BM_month_store) 

lm2b <-lm(lnsalesvalue~time_group * brand_group + avg_female + avg_age + avg_income + avg_homeowner + 
                  avg_residency   +avg_childowner+ store_number_of_skus + store_average_price+
                  sa_gender + sa_full_time+sa_avg_years_of_exp +sa_married + sa_avg_rate_of_pay+
                  sa_dependent
          , data=BM_month_store) 

AIC(lm2,lm2b)   
BIC(lm2, lm2b)  #log is better (lm2 better)

stargazer(lm2, type='text', title='regression model',
          column.labels = 'lm2', df=FALSE, digits = 2, star.cutoffs = c(0.05,0.01,0.001))
# with policy change salesvalue of physical store will decrease by 8%

####### Heterosckedasticity 
gqtest(lm2)    #insignificant
bptest(lm2)    #significant

HWrobstder2a<- sqrt(diag(vcovHC(lm2, type='HC1')))

stargazer(lm2, lm2,  title="Q2-salesvalue-Regression Results", type="text", 
          column.labels=c("Normal SE", 'HW robust SE'),
          se=list(NULL, HWrobstder2a),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))
# with policy change salesvalue of physical store will increase by 8%


###### Marginal Effect
meffect2a <- ggpredict(lm2, terms=c('time_group', 'brand_group'))
ggplot(meffect2a, aes(x, predicted, color=group)) + geom_line(size=1.3) + 
        xlab('Time') + ylab('lnsalesvalue')+ 
        scale_color_discrete(labels=c('Policy unchanged', 'Policy changed'))+
        scale_x_continuous(breaks=c(0,1), labels=c('2013.04.01-2013.09.30', '2013.10.01-2014.03.01'))


##########
# Salesquantity
##########
df2c <- BM_month_store[c('brand_group','time_group','avg_female', 'avg_age', 
                         'avg_income','avg_homeowner','avg_residency', 'avg_childowner', 
                         'store_average_price','sa_gender', 'sa_full_time',
                         'sa_avg_years_of_exp','sa_married', 'sa_avg_rate_of_pay','sa_dependent',
                         'store_number_of_skus'
                         )]
cor(df2c)
# store_number_of_skus > 0.8 remove 

########   Poisson 
poisson2a <- glm(salesquantity ~ brand_group * time_group+ avg_female + avg_age + avg_income + avg_homeowner + 
                         avg_residency  + avg_childowner + store_average_price +
                         sa_gender + sa_full_time+sa_avg_years_of_exp +sa_married + sa_avg_rate_of_pay+
                         sa_dependent+store_number_of_skus
                 , data=BM_month_store, family='poisson' )

stargazer(poisson2a, type='text', title='Q2-salesquantity-poisson-IRR', column.labels = 'poisson_IRR',
          apply.coef = exp, t.auto=F, p.auto=F, 
          digits=2, df=F, star.cutoffs = c(0.05,0.01,0.001))

####### Heteroskedasticity
gqtest(poisson2a)    #insignificant
bptest(poisson2a)    #significant

HWrobstder2b<- sqrt(diag(vcovHC(poisson2a, type='HC1')))

stargazer(poisson2a, poisson2a,  title="Q2-salesquantity-Poisson Results", type="text", 
          column.labels=c("Normal SE", 'HW robust SE'),
          se=list(NULL, HWrobstder2b),
          apply.coef = exp, t.auto=F, p.auto=F,
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))
# ==> with return policy change, salesquantity will decrease by 4%  


###### Model Fit
poisson2b <- glm(salesquantity ~ 1, data= BM_month_store, family='poisson')
lrtest(poisson2a, poisson2b)
# significant ==> poisson model does not fit the data


######### Negative Binomial Model
negbin2a <- glm.nb(salesquantity ~ brand_group * time_group+ avg_female + avg_age + avg_income + avg_homeowner + 
                           avg_residency  + avg_childowner + store_average_price +
                           sa_gender + sa_full_time+sa_avg_years_of_exp +sa_married + sa_avg_rate_of_pay+
                           sa_dependent + store_number_of_skus
                   , data=BM_month_store)

stargazer(negbin2a, type='text', title='Q2-salesquantity-negbin-IRR', column.labels = 'negbin_IRR',
          apply.coef = exp, t.auto=F, p.auto=F, 
          digits=2, df=F, star.cutoffs = c(0.05,0.01,0.001))


###### Heteroskedasticity
gqtest(negbin2a)    #insignificant
bptest(negbin2a)    #significant

HWrobstder2c<- sqrt(diag(vcovHC(negbin2a, type='HC1')))

stargazer(negbin2a, negbin2a,  title="Q2-salesquantity-NB Results", type="text", 
          apply.coef = exp, t.auto=F, p.auto=F,
          column.labels=c("Normal SE", 'HW robust SE'),
          se=list(NULL, HWrobstder2c),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))
# ==> return policy change, insignificant impact


######  Model Fit
negbin2b <- glm.nb(salesquantity ~1, data= BM_month_store)
lrtest(negbin2a, negbin2b)
# significant ==> NB model fits the data

######## Model Choose
lrtest(poisson2a, negbin2a)      # significant ==> choose NB

#=========
# Conclusion: return policy change will increase sale value by 8%, has no significant impact on sales quantity. 
#=========

#===================================================
# Question 3: What is the impact of the policy change on online channel returns?
#===================================================

stargazer(Online_daily_store, type='text', title='descriptive analysis',
          digits=2, median=TRUE, iqr=TRUE)

############
##  Return Value
############

#### EXPLORE DATA 
ggplot(Online_daily_store, aes(returnvalue)) + geom_histogram(color='green')
ggplot(Online_daily_store, aes(log(returnvalue))) + geom_histogram(color='green')
Online_daily_store$lnreturnvalue <- log(Online_daily_store$returnvalue+1)
Online_daily_store$lnsalesvalue <- log(Online_daily_store$salesvalue+1)
Online_daily_store$lnsalesquantity <- log(Online_daily_store$salesquantity+1)

#### MULTICOLLINEARITY
df3a <- Online_daily_store[c('lnreturnvalue','store_group','time_group', 'avg_female','avg_age','avg_income',
                             'avg_homeowner','avg_residency','avg_childowner','lnsalesvalue')]
cor(df3a)    # lnsalesvalue >0.8 remove

df3b <- Online_daily_store[c('store_group','time_group', 'avg_female','avg_age','avg_income',
                             'avg_homeowner','avg_residency','avg_childowner')]
vif(df3b)

#### linear regression
lm3a <- lm(lnreturnvalue ~ time_group*store_group + avg_female + avg_age + avg_income + 
                  avg_homeowner + avg_residency +avg_childowner, data=Online_daily_store)
step <- stepAIC(lm3a, direction='both')
step$anova

lm3 <- lm(lnreturnvalue ~ time_group*store_group + avg_female + avg_age + avg_income + 
                  avg_homeowner + avg_residency +avg_childowner, data=Online_daily_store)

lm3b <-lm(lnreturnvalue ~ time_group*store_group + lnsalesvalue + avg_female + avg_age + avg_income + 
                  avg_homeowner + avg_residency +avg_childowner, data=Online_daily_store)

anova(lm3, lm3b, test='Chisq') # significant, include log(salesvalue+1) (lm3b)
#####
gqtest(lm3b)   #significant
bptest(lm3b)   # insignificant

HWrobstder3a <- sqrt(diag(vcovHC(lm3b, type='HC1')))

stargazer(lm3b, type='text',title='Online_Returnvalue', se=list(HWrobstder3a),
          column.labels = c("HW-Robust SE"), df=FALSE, digits=2, 
          star.cutoffs = c(0.05,0.01,0.001))
# change of return policy has insignificant impact on online store returnvalue 

## Marginal Effects
meffects3a <- ggpredict(lm3b, terms=c('time_group', 'store_group'))
ggplot(meffects3a, aes(x, predicted, color=group)) + geom_line(size=1.3) + 
        xlab('Time') + ylab('ln(Return Value)') + 
        scale_color_discrete(labels=c('Policy changed', 'Policy unchanged'))+ 
        scale_x_continuous(breaks = c(0,1), labels = c('Before policy change', 'After policy change'))




##########
# returnquantity
##########

#### Multicollinearity
df3c <- Online_daily_store[c('returnquantity','store_group','time_group', 'avg_female','avg_age','avg_income',
                             'avg_homeowner','avg_residency','avg_childowner','salesquantity')]
cor(df3c)    # salesquantity >0.8 remove


###### POISSON
poisson3 <- glm(returnquantity ~ time_group*store_group + avg_female + avg_age + avg_income + 
                        avg_homeowner + avg_residency +avg_childowner, data=Online_daily_store, family='poisson')


poisson3b <- glm(returnquantity ~ time_group*store_group +salesquantity+ avg_female + avg_age + avg_income + 
                        avg_homeowner + avg_residency +avg_childowner, data=Online_daily_store, family='poisson')


poisson3c <-glm(returnquantity ~ time_group*store_group +lnsalesquantity+ avg_female + avg_age + avg_income + 
                        avg_homeowner + avg_residency +avg_childowner, data=Online_daily_store, family='poisson')

AIC(poisson3b,poisson3c)
BIC(poisson3b,poisson3c)  # log(salesquantity+1) is better(poisson3c)

anova(poisson3,poisson3c, test='Chisq')  # significant ==> with log(salesquantity+1) is better(poisson3c)

stargazer(poisson3b, type='text', title='Q3-returnquantity-poisson', column.labels = 'poisson-IRR',
          apply.coef = exp, t.auto = F, p.auto = F,
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))

# return policy change will decrease return quantity by 19%.
### HETEROSKEDASTICITY
bptest(poisson3c)   # significant
gqtest(poisson3c)   # significant

HWrobstder3b <- sqrt(diag(vcov(poisson3c, type='HC1')))

stargazer(poisson3c, poisson3c, type='text', title='Q3-returnquantity-poisson-IRR', 
          apply.coef = exp, t.auto = F, p.auto = F,
          se=list(NULL, HWrobstder3b), column.labels = c("Normal SE", "HW-Robust SE"), 
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))
# ===> change of return policy decrease return quantity by 19% 


#### MODEL FIT
poisson3a <- glm(returnquantity~1, data=Online_daily_store,family='poisson')
lrtest(poisson3c, poisson3a)
# significant ==> the poisson model doe not fit the data



######## NEGATICE BINOMIAL
negbin3 <- glm.nb(returnquantity ~ time_group*store_group + avg_female + avg_age + avg_income + 
                          avg_homeowner + avg_residency +avg_childowner, data=Online_daily_store)

negbin3b <-glm.nb(returnquantity ~ time_group*store_group +lnsalesquantity+ avg_female + avg_age + avg_income + 
                        avg_homeowner + avg_residency +avg_childowner, data=Online_daily_store)

lrtest(negbin3, negbin3b)
# significant ==> with log(salesquantity+1) is better (negbin3b)


stargazer(negbin3b, type='text', title='Q3-returnquantity-NB', column.labels = 'negative binomial-IRR',
          apply.coef = exp, t.auto = F, p.auto = F,
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))
# return policy change will decrease return quantity by 11%




###### HETEROSKEDASTICITY
gqtest(negbin3b)  # significant 
bptest(negbin3b)  # significant

HWrobstder3c <- sqrt(diag(vcov(negbin3b, type='HC1')))

stargazer(negbin3b, type='text', title='Online_Returnquantity', 
          apply.coef = exp, t.auto = F, p.auto = F,
          se=list(HWrobstder3c), column.labels = c("HW-Robust SE"), 
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))
`# return policy change will decrease return quantity by 11%

#### MODEL fit
negbin3a <- glm.nb(returnquantity ~1, data =Online_daily_store)

lrtest(negbin3b, negbin3a)   # significant, NB model fits the data

###### CHOOSE MODEL
lrtest(poisson3c, negbin3b) # significant, NB model is better


#### Marginal Effects
meffects3b <- ggpredict(negbin3b, terms=c('time_group', 'store_group'))
ggplot(meffects3b, aes(x, predicted, color=group)) + geom_line(size=1.3) + 
        xlab('Time') + ylab('Return Quantity') + 
        scale_color_discrete(labels=c('Policy changed', 'Policy unchanged'))+ 
        scale_x_continuous(breaks = c(0,1), labels = c('Before policy change', 'After policy change'))




#=========
# Conclusion: change of return policy has insignificant impact on online store returnvalue,
#                                       but will decrease return quantity by 11%
#=========



#===================================================
# Question 4: What is the impact of the policy change on physical store returns?
#===================================================

stargazer(BM_month_store, type='text', title='descriptive analysis',
          digits=2, median=TRUE, iqr=TRUE) 

########
# Return Value
########

ggplot(BM_month_store, aes(x=returnvalue)) + geom_histogram(color='green')
ggplot(BM_month_store, aes(x=log(returnvalue))) + geom_histogram(color='green')
BM_month_store$lnreturnvalue <- log(BM_month_store$returnvalue+1)


####### Multicollinearity 

df4a <- BM_month_store[c('lnreturnvalue', 'brand_group','time_group','avg_female', 'avg_age', 
                         'avg_income','avg_homeowner','avg_residency', 'avg_childowner', 
                         'store_average_price','store_number_of_skus','sa_gender', 'sa_full_time',
                         'sa_avg_years_of_exp','sa_married', 'sa_avg_rate_of_pay','sa_dependent',
                         'sales_volume_group'
                         )]
cor(df4a)

df4b <- BM_month_store[c( 'brand_group','time_group','avg_female', 'avg_age', 
                     'avg_income','avg_homeowner','avg_residency', 'avg_childowner', 
                     'store_average_price','store_number_of_skus','sa_gender', 'sa_full_time',
                     'sa_avg_years_of_exp','sa_married', 'sa_avg_rate_of_pay','sa_dependent'
                     )]

vif(df4b)    # sales_volume_group >3 remove

########  Linear Model

lm4 <- lm(lnreturnvalue ~ time_group * brand_group + avg_female + avg_age + avg_income + avg_homeowner + 
                   avg_residency + avg_childowner + store_number_of_skus + store_average_price+
                   sa_gender + sa_full_time+sa_avg_years_of_exp +sa_married + sa_avg_rate_of_pay+
                   sa_dependent
           , data=BM_month_store)
step <- stepAIC(lm4, direction='both')
step$anova    
# without avg_female, sa_avg_years_of_exp, but I still include these two because conceptually they impact return value

stargazer(lm4, type='text', title='Q4-returnvalue-lm', column.labels = 'lm-returnvalue',
          df=F, digits=2, star.cutoffs = c(0.05, 0.01, 0.001))
# ===> return policy change will decrease return value by 41%

##### Heteroskedasticity
gqtest(lm4)   # insignificant
bptest(lm4)   # signidicant

HWrobstder4a <- sqrt(diag(vcovHC(lm4, type='HC1')))

stargazer(lm4, lm4, type='text', title='Q4-returnvalue-lm', 
          column.labels = c('Normal SE', 'Robust SE'), 
          se=list(NULL, HWrobstder4a),
          df=F, digits=2, star.cutoffs = c(0.05,0.01,0.001))
# ===> return policy change will decrease return value by 41% for physical store


#########
# Return Quantity
########

####### Multicollinearity 
df4c <- BM_month_store[c('returnquantity', 'brand_group','time_group','avg_female', 'avg_age', 
                         'avg_income','avg_homeowner','avg_residency', 'avg_childowner', 
                         'store_average_price','store_number_of_skus','sa_gender', 'sa_full_time',
                         'sa_avg_years_of_exp','sa_married', 'sa_avg_rate_of_pay','sa_dependent',
                         'sales_volume_group'
                         )]

cor(df4c)


####### Poisson

poisson4a <-glm(returnquantity ~ brand_group * time_group+ avg_female + avg_age + avg_income + avg_homeowner + 
                       avg_residency  + avg_childowner + store_average_price +
                       sa_gender + sa_full_time+sa_avg_years_of_exp +sa_married + sa_avg_rate_of_pay+
                       sa_dependent +store_number_of_skus
               , data=BM_month_store, family='poisson' )

poisson4b <-glm(returnquantity ~ brand_group * time_group+ avg_female + avg_age + avg_income + avg_homeowner + 
                        avg_residency  + avg_childowner + store_average_price +
                        sa_gender + sa_full_time+sa_avg_years_of_exp +sa_married + sa_avg_rate_of_pay+
                        sa_dependent 
                , data=BM_month_store, family='poisson' )

anova(poisson4a, poisson4b, test='Chisq') # with store_number_of_skus is better (poisson4a)

stargazer(poisson4a, type='text', title='Q4-returnquantity-poisson', column.labels = 'poisson-IRR',
          apply.coef = exp, t.auto = F, p.auto = F,
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))
# ==> changed return policy will decrease sales quantity by 29%


###### Heteroskedasticity

gqtest(poisson4a)  # insignificant
bptest(poisson4a)  # significant

HWrobstder4b <- sqrt(diag(vcovHC(poisson4a, type='HC1')))

stargazer(poisson4a, poisson4a, type='text', title='Q4-returnquantity-poisson', 
          column.labels =c('Normall SE', 'Robust SE'),
          se=list(NULL, HWrobstder4b),
          apply.coef = exp, t.auto = F, p.auto = F,
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))
# ==> changed return policy will decrease sales quantity by 29%


##### Model Fit
poisson4c <- glm(returnquantity ~ 1, data=BM_month_store, family='poisson')
lrtest(poisson4a, poisson4c) 
# significant ==> Poisson model does not fit the data


########## Negative Binomial 

negbin4a <- glm.nb(returnquantity ~ brand_group * time_group+ avg_female + avg_age + avg_income + avg_homeowner + 
                        avg_residency  + avg_childowner + store_average_price +
                        sa_gender + sa_full_time+sa_avg_years_of_exp +sa_married + sa_avg_rate_of_pay+
                        sa_dependent +store_number_of_skus
                , data=BM_month_store )

stargazer(negbin4a, type='text', title='Q4-returnquantity-NB', column.labels = 'NB-IRR',
          apply.coef = exp, t.auto = F, p.auto = F,
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))
# ==> Change return policy will decrease return quantity by 27% 


##### Heteroskedasticity

gqtest(negbin4a)  # insignificant
bptest(negbin4a)  # significant

HWrobstder4c <- sqrt(diag(vcovHC(negbin4a, type='HC1')))

stargazer(negbin4a, negbin4a, type='text', title='Q4-returnquantity-NB', 
          column.labels =c('Normall SE', 'Robust SE'),
          se=list(NULL, HWrobstder4c),
          apply.coef = exp, t.auto = F, p.auto = F,
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))
# ==> Change return policy will decrease return quantity by 27% 


##### Model Fit
negbin4b <- glm.nb(returnquantity~1, data=BM_month_store)

lrtest(negbin4a, negbin4b)    # significant ==> NB model fits the data

########## Model Choose
lrtest(negbin4a, poisson4a)  # significant ==> NB model is better


#=========
# Conclusion: change return policy will decrease return value by 41% for physical store
#                                  will decrease return quantity by 27% for physical store
#=========




#===================================================
# Question 5: What is the impact of the policy change on product level online sales and returns 
#               as well as on product level physical store sales and returns?
#===================================================

stargazer(Online_daily_prod, type='text', title='descriptive analysis',
          digits=2, median=TRUE, iqr=TRUE)



##############
# Online Sales
##############
stargazer(Online_daily_store, type='text', title='descriptive analysis',
          digits=2, median=TRUE, iqr=TRUE)

# Replace missing value with mean
Online_daily_store$avg_female[is.na(Online_daily_store$avg_female)] <- 0.62

Online_daily_store$avg_age[is.na(Online_daily_store$avg_age)] <- 4.37

Online_daily_store$avg_income[is.na(Online_daily_store$avg_income)] <-5.33

Online_daily_store$avg_homeowner[is.na(Online_daily_store$avg_homeowner)] <- 0.68

Online_daily_store$avg_residency[is.na(Online_daily_store$avg_residency)] <- 7.24

Online_daily_store$avg_childowner[is.na(Online_daily_store$avg_childowner)] <- 0.44


# set dummy variable for time group
#       time_group =0 : before policy change (2013.04.01-2013.09.30)
#       time_group =1 : after policy change(2013.10.01 - 2014.03.01)
# set dummy variable for store group
#       store_group = 0 : without policy change(store number=10)
#       store_group = 1 : with policy change(store number =2 or 6 )
Online_daily_store $ store_group <- ifelse(Online_daily_store $ store_number %in% c(2,6), 1, 0)
Online_daily_store $ time_group <- ifelse(Online_daily_store $month_dummy %in% c(4,5,6,7,8,9),0,1)





#######
# salesvalue
#######

### explore data
ggplot(Online_daily_store, aes(x=salesvalue)) + geom_histogram(color='green')
ggplot(Online_daily_store, aes(x=log(salesvalue))) + geom_histogram(color='green')
Online_daily_store$lnsalesvalue <- log(Online_daily_store$salesvalue+1)


### multicollinearity
df1a <- Online_daily_store[c('lnsalesvalue','store_group','time_group', 'avg_female','avg_age','avg_income',
                             'avg_homeowner','avg_residency','avg_childowner')]
cor(df1a)
df1b <- Online_daily_store[c('store_group','time_group', 'avg_female','avg_age','avg_income',
                             'avg_homeowner','avg_residency','avg_childowner')]

vif(df1b)

### linear model

lm0 <- lm(lnsalesvalue ~ time_group*store_group + avg_female + avg_age + avg_income + 
                  avg_homeowner + avg_residency +avg_childowner, data=Online_daily_store)
step<-stepAIC(lm0, direction='both')
step$anova

lm0a<- lm(lnsalesvalue ~ time_group*store_group + avg_female + avg_age + avg_income + 
                  avg_homeowner +avg_childowner, data=Online_daily_store)
anova(lm0,lm0a, test='Chisq')
# model with avg_trsidency is better (lm0)

lm1a<-lm(lnsalesvalue ~ time_group*store_group + avg_female + avg_age + avg_income + 
                 avg_homeowner +avg_childowner+ avg_residency, data=Online_daily_store)

stargazer(lm1a, type='text',title='regression result',
          column.labels = 'lm1a', df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))
# the impact of return policy on lnsalesvalue is insignificant 

##### Heteroskedasticity 
gqtest(lm1a)
bptest(lm1a)

HWrobstder <- sqrt(diag(vcovHC(lm1a, type='HC1')))

stargazer(lm1a, type='text',title='Online_Salesvalue', se=list(HWrobstder),
          column.labels = c("HW-Robust SE"), df=FALSE, digits=2, 
          star.cutoffs = c(0.05,0.01,0.001))

# the impact of return policy on lnsalevalue is insignificant 


#### Marginal Effect
meffects1a <- ggpredict(lm1a, terms=c('time_group', 'store_group'))
ggplot(meffects1a, aes(x, predicted, color=group)) + geom_line(size=1.3) + 
        xlab('Time') + ylab('ln(Sales Value)') + 
        scale_color_discrete(labels=c('Policy changed', 'Policy unchanged'))+ 
        scale_x_continuous(breaks = c(0,1), labels = c('Before policy change', 'After policy change'))


#######
# salesquantity
#######

ggplot(Online_daily_store, aes(salesquantity)) + geom_histogram(color = 'green')
ggplot(Online_daily_store, aes(log(salesquantity))) + geom_histogram(color = 'green')
# Online_daily_store$lnsalesquantity <- log(Online_daily_store$salesquantity+1)

### multicollinearity
df1c <- Online_daily_store[c('salesquantity','store_group','time_group', 'avg_female','avg_age','avg_income',
                             'avg_homeowner','avg_residency','avg_childowner')]
cor(df1c)

df1d <- Online_daily_store[c('store_group','time_group', 'avg_female','avg_age','avg_income',
                             'avg_homeowner','avg_residency','avg_childowner')]

vif(df1d)


####  poisson model
poisson1 <- glm(salesquantity~ time_group*store_group + avg_female + avg_age + avg_income + 
                        avg_homeowner + avg_residency +avg_childowner, 
                data=Online_daily_store, family = 'poisson')

poisson1a <-glm(salesquantity~ time_group*store_group + avg_female + avg_age + avg_income + 
                        avg_homeowner +avg_childowner, 
                data=Online_daily_store, family = 'poisson')

anova(poisson1, poisson1a, test='Chisq')
# significant ==> model with avg_residency is better(poisson1)

stargazer(poisson1, type='text', title='regression model', column.labels ='poisson1',
          apply.coef = exp, t.auto=F, p.auto=F,
          digits=2, df=FALSE, star.cutoffs=c(0.05,0.01,0.001) )
# with return policy, sales quantity will decrease 3%.


## model fit
poisson1b <- glm(salesquantity~1, data=Online_daily_store, family='poisson')
lrtest(poisson1,poisson1b)
# significant ==> poisson model does not fit the data


### Heteroskedasticity
bptest(poisson1)
gqtest(poisson1)
# significant ==> has heteroskedasticity

HWrobstder2 <- sqrt(diag(vcovHC(poisson1, type='HC1')))

stargazer(poisson1, type='text', title='regression model', column.labels ='poisson1-IRR-robust SE',
          apply.coef = exp, t.auto=F, p.auto=F,
          se=list(HWrobstder2),
          digits=2, df=FALSE, star.cutoffs=c(0.05,0.01,0.001) )

# with return policy, sales quantity will decrease by 3%.



####  negative binomial model
negbin1 <- glm.nb(salesquantity ~ time_group*store_group + avg_female + avg_age + avg_income +
                          avg_homeowner + avg_residency +avg_childowner, 
                  data=Online_daily_store) 

negbin1a <- glm.nb(salesquantity ~ time_group*store_group + avg_female + avg_age + avg_income +
                           avg_homeowner + avg_childowner, 
                   data=Online_daily_store) 

anova(negbin1, negbin1a, test='Chisq')
# insignificant ==> with avg_residency is better (negbin1)

stargazer(negbin1,  title="Regression Results", type="text", column.labels=c("negbin1a-IRR"),
          apply.coef = exp, t.auto=F, p.auto=F,
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))
# return policy has no significant change on sales quantity


### Heterosckedasticity
gqtest(negbin1)    #significant
bptest(negbin1)    #significant

HWrobstder3<- sqrt(diag(vcovHC(negbin1, type='HC1')))

stargazer(negbin1,  title="Online_Salesquantity", type="text", column.labels=c("negbin1a-IRR-robust SE"),
          apply.coef = exp, t.auto=F, p.auto=F,
          se=list(HWrobstder3),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))
# interaction still insignificant ==> return policy has no significant change on sales quantity


### MODEL FIT
negbin1b <- glm.nb(salesquantity~1, data=Online_daily_store)
lrtest(negbin1, negbin1b)  #significant==> negative binomial model fits the data



######## CHOOSE MODEL BETWEEN POISSON & NEGATIVE BINOMIAL
lrtest(poisson1, negbin1)
# significant ==> choose negative binomial 

####### MARGINAL EFFECTS

meffects1b <- ggpredict(negbin1, terms=c('time_group', 'store_group'))
ggplot(meffects1b, aes(x, predicted, color=group)) + geom_line(size=1.3) + 
        xlab('Time') + ylab('Sales Quantity') + 
        scale_color_discrete(labels=c('Policy changed', 'Policy unchanged'))+ 
        scale_x_continuous(breaks = c(0,1), labels = c('Before policy change', 'After policy change'))


















df5a<- Online_daily_prod[c('lnsalesvalue','product_category','time_group','store_group', 'avg_female','avg_age','avg_income',
                          'avg_homeowner','avg_residency','avg_childowner')]

cor(df5a)

df5b<- Online_daily_prod[c('product_category','store_group','time_group', 'avg_female','avg_age','avg_income',
                           'avg_homeowner','avg_residency','avg_childowner')]
vif(df5b)


###### Linear Regression




lm5a <- lm(lnsalesvalue~ time_group*store_group*factor(product_category) + avg_female + avg_age + 
                  avg_income + avg_homeowner + avg_residency +avg_childowner, data=Online_daily_prod)
stargazer(lm5, type='text', title='Q5-linear regression result',
          column.labels = 'lm5', df=F, digits=2, star.cutoffs = c(0.05,0.01,0.001))





#===================================================
# Question 6: How does the impact of the policy change vary across product categories
#===================================================

stargazer(Online_daily_prod, type='text', title='descriptive analysis',
          digits=2, median=TRUE, iqr=TRUE)

# Replace missing value with mean
Online_daily_prod$avg_female[is.na(Online_daily_prod$avg_female)] <- 0.57

Online_daily_prod$avg_age[is.na(Online_daily_prod$avg_age)] <- 4.32

Online_daily_prod$avg_income[is.na(Online_daily_prod$avg_income)] <-5.25

Online_daily_prod$avg_homeowner[is.na(Online_daily_prod$avg_homeowner)] <- 0.66

Online_daily_prod$avg_residency[is.na(Online_daily_prod$avg_residency)] <- 7.03


Online_daily_prod$avg_childowner[is.na(Online_daily_prod$avg_childowner)] <- 0.42


# set dummy variable for time group
#       time_group =0 : before policy change (2013.04.01-2013.09.30)
#       time_group =1 : after policy change(2013.10.01 - 2014.03.01)
# set dummy variable for store group
#       store_group = 0 : without policy change(store number=10)
#       store_group = 1 : with policy change(store number =2 or 6 )

Online_daily_prod $ store_group <- ifelse(Online_daily_prod $ store_number %in% c(2,6), 1, 0)
Online_daily_prod $ time_group <- ifelse(Online_daily_prod $month_dummy %in% c(4,5,6,7,8,9),0,1)
                                         
                                         
########
# Online Sales value
########

### Explore data
ggplot(Online_daily_prod,aes(salesvalue)) + geom_histogram(color='green')
ggplot(Online_daily_prod,aes(log(salesvalue))) + geom_histogram(color='green')
Online_daily_prod$lnsalesvalue <- log(Online_daily_prod$salesvalue+1)
Online_daily_prod$lnreturnvalue<- log(Online_daily_prod$returnvalue+1)
Online_daily_prod$lnsalesquantity <- log(Online_daily_prod$salesquantity+1)
Online_daily_prod$lnreturnquantity <- log(Online_daily_prod$returnquantity+1)


is.factor(Online_daily_prod$product_category)
table(Online_daily_prod$product_category)
Online_daily_prod$fproduct_category <- factor(Online_daily_prod$product_category, 
                                              levels = c("5" ,"1", "2",  "3",  "4",  "6",  "7",  
                                                         "8",  "9",  "10", "11", "12", "13", "14",
                                                         "15", "16", "17","18", "19", "20",'21'))

### Multicollinearity 

df6a <- Online_daily_prod[c('lnsalesvalue','store_group','time_group', 'avg_female','avg_age','avg_income',
                             'avg_homeowner','avg_residency','avg_childowner')]
cor(df6a)
df6b <- Online_daily_prod[c('store_group','time_group','product_category', 'avg_female','avg_age','avg_income',
                             'avg_homeowner','avg_residency','avg_childowner')]

vif(df6b)

###### Linear Regression
lm6b <-lm(lnsalesvalue ~ time_group*store_group*fproduct_category + avg_female + avg_age + avg_income + 
                  avg_homeowner + avg_residency + avg_childowner, data=Online_daily_prod)

step<- stepAIC(lm6a, direction='both')
step$anova

lm6a <-lm(lnsalesvalue ~ time_group*store_group*fproduct_category + avg_female + avg_age + avg_income + 
                  avg_homeowner  + avg_childowner, data=Online_daily_prod)

anova(lm6a, lm6b, test='Chisq')
# insignificant, without avg_residency is better(lm6a)

stargazer(lm6a, type='text',title='regression result',
          column.labels = 'lm6B', df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))

#### Heteroskedasticity

bptest(lm6a)  #significant 
gqtest(lm6a)  # significant

HWrobstder6a <- sqrt(diag(vcovHC(lm6a, type='HC1')))
stargazer(lm6a,lm6a, type='text', title='Q6- Impact on Online sales value across products',
          se=list(NULL, HWrobstder6a),
          column.labels = c('Normal SE', 'HW-robust SE'), df=F, digits=2, star.cutoffs = c(0.05,0.01,0.001))


##### Marginal Effects
meffects6a <- ggpredict(lm6a, terms=c('time_group','store_group','fproduct_category'))

ggplot(meffects6a, aes(x,predicted, color=group)) + geom_line(size=1.3)+
        xlab('Time') + ylab('ln(Online_Salesvalue)') + 
        scale_color_discrete(labels=c('Policy changed', 'Policy unchanged'))+ 
        scale_x_continuous(breaks = c(0,1), labels = c('Befoct', 'Aftoct'))+
        facet_wrap(~facet,ncol=4)



online_salesvalue_pc <- list()
for (n in 1:21) {
        online_salesvalue_pc[[n]] <- subset(Online_daily_prod, Online_daily_prod$fproduct_category==n) 
}

model_online_salesvalue_pc<-list()
for (m in c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,20,21)) {
        model_online_salesvalue_pc[[m]] <- lm( 
                lnsalesvalue ~ time_group*store_group
                + avg_female 
                + avg_age 
                + avg_income 
                + avg_homeowner 
                + avg_childowner 
                , data=online_salesvalue_pc[[m]] 
        )
}



for (n in c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,20,21)) {
        stargazer(model_online_salesvalue_pc[[n]],type="text",digits=2,titile=paste("model_pc",n,"Results"))
}








mydata6a_18 <- subset(Online_daily_prod, fproduct_category==18)

res6a_18 <- lm(lnsalesvalue ~ time_group*store_group + avg_female + avg_age + avg_income + 
                    avg_homeowner  + avg_childowner, data=mydata6a_18)


mydata6a_19 <- subset(Online_daily_prod, fproduct_category==19)

res6a_19 <- lm(lnsalesvalue ~ time_group*store_group + avg_female + avg_age + avg_income + 
                       avg_homeowner  + avg_childowner, data=mydata6a_19)

### can not run a model for product catrgory 18 & 19 because they are empty data set


########
# Online Sales Quantity
########
stargazer(Online_daily_prod, type='text', title='descriptive analysis',
          digits=2, median=TRUE, iqr=TRUE)

### Multicollinearity
df6c <- Online_daily_prod[c('salesquantity','store_group','time_group', 'avg_female','avg_age','avg_income',
                            'avg_homeowner','avg_residency','avg_childowner')]
cor(df6a)


############# Poisson
poisson6a <- glm(salesquantity ~ time_group*store_group*fproduct_category + avg_female + avg_age + avg_income + 
                         avg_homeowner + avg_childowner, data=Online_daily_prod, family='poisson')

poisson6 <- glm(salesquantity ~ time_group*store_group*fproduct_category + avg_female + avg_age + avg_income + 
                         avg_homeowner + avg_childowner + avg_residency, data=Online_daily_prod, family='poisson')
anova(poisson6a, poisson6, test='Chisq')
# significant ==> with avg_residency is better(poisson6)


stargazer(poisson6, type='text', title='Q6-salesquantity across products', 
          column.labels =c('Poisson-IRR'),
          apply.coef = exp, t.auto = F, p.auto = F,
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))


#### Heteroskedasticity
bptest(poisson6)  #significant 
gqtest(poisson6)  # significant

HWrobstder6b <- sqrt(diag(vcovHC(poisson6, type='HC1')))
stargazer(poisson6,poisson6, type='text', title='Q6- Impact on Online sales quantity across products',
          apply.coef = exp, t.auto = F, p.auto = F,
          se=list(NULL, HWrobstder6b),
          column.labels = c('Normal SE', 'HW-robust SE'), df=F, digits=2, star.cutoffs = c(0.05,0.01,0.001))


##### Model Fit
poisson6c <- glm(salesquantity ~ 1, data=Online_daily_prod, family='poisson')
lrtest(poisson6, poisson6c)
# significant ==> poisson does not fit the model



########### Negative Binomial

negbin6a <- glm.nb(salesquantity ~ time_group*store_group*fproduct_category + avg_female + avg_age + avg_income + 
                           avg_homeowner + avg_childowner + avg_residency, data=Online_daily_prod)

negbin6b <- glm.nb(salesquantity ~ time_group*store_group*fproduct_category + avg_female + avg_age + avg_income + 
                           avg_homeowner + avg_childowner, data=Online_daily_prod)
lrtest(negbin6a, negbin6b) # insignificant without avg_residency is better(negbin6b)

### Heteroskedasticity
bptest(negbin6b)  #significant 
gqtest(negbin6b)  # significant

HWrobstder6c <- sqrt(diag(vcovHC(negbin6b, type='HC1')))
stargazer(negbin6b,negbin6b, type='text', title='Q6- Impact on Online sales quantity across products-NB',
          apply.coef = exp, t.auto = F, p.auto = F,
          se=list(NULL, HWrobstder6c),
          column.labels = c('Normal SE', 'HW-robust SE'), df=F, digits=2, star.cutoffs = c(0.05,0.01,0.001))


#### Model Fit
negbin6c <- glm.nb(salesquantity~1, data=Online_daily_prod)
lrtest(negbin6b, negbin6c)
# significant, NB fit the model

#### Choose model (poisson & negative binomial)
lrtest(negbin6b, poisson6)
# significant ==> NB is better


##### Marginal Effects
meffects6b <- ggpredict(negbin6b, terms=c('time_group','store_group','fproduct_category'))

ggplot(meffects6b, aes(x,predicted, color=group)) + geom_line(size=1.3)+
        xlab('Time') + ylab('Online_SalesQuantity') + 
        scale_color_discrete(labels=c('Policy changed', 'Policy unchanged'))+ 
        scale_x_continuous(breaks = c(0,1), labels = c('BefOct', 'AftOct'))+
        facet_wrap(~facet,ncol=4)



online_salesquantity_pc <- list()
for (n in c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,20,21)) {
        online_salesquantity_pc[[n]] <- subset(Online_daily_prod, Online_daily_prod$fproduct_category==n) 
}

model_online_salesquantity_pc<-list()
for (m in c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,20,21)) {
        model_online_salesquantity_pc[[m]] <- glm.nb( 
                salesquantity ~ time_group*store_group
                + avg_female 
                + avg_age 
                + avg_income 
                + avg_homeowner 
                + avg_childowner
                , data=online_salesquantity_pc[[m]] 
        )
}

for (n in c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,20,21)) {
        stargazer(model_online_salesquantity_pc[[n]],type="text",titile=paste("model_online_salesquantity_pc",n,"Results"),
                  apply.coef = exp, t.auto = F, p.auto = F,
                  digits=2,df=FALSE, star.cutoffs = c(0.05,0.01,0.001) )
}






########
# Online Return value
########
stargazer(Online_daily_prod, type='text', title='descriptive analysis',
          digits=2, median=TRUE, iqr=TRUE)

### Explore data
ggplot(Online_daily_prod,aes(returnvalue)) + geom_histogram(color='green')
ggplot(Online_daily_prod,aes(log(returnvalue))) + geom_histogram(color='green')
Online_daily_prod$lnreturnvalue <- log(Online_daily_prod$returnvalue+1)
Online_daily_prod$lnsalesquantity <- log(Online_daily_prod$salesquantity+1.1)


###### Linear Regression
lm6c <-lm(lnreturnvalue ~ time_group*store_group*fproduct_category + avg_female + avg_age + avg_income + 
                  avg_homeowner + avg_residency + avg_childowner, data=Online_daily_prod)

lm6d <-lm(lnreturnvalue ~ time_group*store_group*fproduct_category + avg_female + avg_age + avg_income + 
                  avg_homeowner  + avg_childowner, data=Online_daily_prod)

anova(lm6c, lm6d, test='Chisq')
# insignificant, without avg_residency is better(lm6d)
lm6e <- lm(lnreturnvalue ~ time_group*store_group*fproduct_category + avg_female + avg_age + avg_income + 
                   avg_homeowner  + avg_childowner + lnsalesvalue, data=Online_daily_prod)

anova(lm6d, lm6e, test='Chisq') # withlnsalesvalue is better (6e)
stargazer(lm6e, type='text',title='regression result',
          column.labels = 'lm6e', df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))

#### Heteroskedasticity

bptest(lm6e)  #significant 
gqtest(lm6e)  # insignificant

HWrobstder6d <- sqrt(diag(vcovHC(lm6e, type='HC1')))
stargazer(lm6e,lm6e, type='text', title='Q6- Impact on Online return value across products',
          se=list(NULL, HWrobstder6d),
          column.labels = c('Normal SE', 'HW-robust SE'), df=F, digits=2, star.cutoffs = c(0.05,0.01,0.001))


##### Marginal Effects
meffects6c <- ggpredict(lm6e, terms=c('time_group','store_group','fproduct_category'))

ggplot(meffects6c, aes(x,predicted, color=group)) + geom_line(size=1.3)+
        xlab('Time') + ylab('ln(Online_Returnvalue)') + 
        scale_color_discrete(labels=c('Policy changed', 'Policy unchanged'))+ 
        scale_x_continuous(breaks = c(0,1), labels = c('BefOct', 'AftOct'))+
        facet_wrap(~facet,ncol=4)



online_returnvalue_pc <- list()
for (n in 1:21) {
        online_returnvalue_pc[[n]] <- subset(Online_daily_prod, Online_daily_prod$fproduct_category==n) 
}

model_online_returnvalue_pc<-list()
for (m in c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,20,21)) {
        model_online_returnvalue_pc[[m]] <- lm( 
                lnreturnvalue ~ time_group*store_group
                + avg_female 
                + avg_age 
                + avg_income 
                + avg_homeowner 
                + avg_childowner 
                + lnsalesvalue
                , data=online_returnvalue_pc[[m]] 
        )
}



for (n in c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,20,21)) {
        stargazer(model_online_returnvalue_pc[[n]],type="text",digits=2,titile=paste("model_return_value_pc",n,"Results"))
}



########
# Online Return Quantity
########
stargazer(Online_daily_prod, type='text', title='descriptive analysis',
          digits=2, median=TRUE, iqr=TRUE)

### Multicollinearity
df6c <- Online_daily_prod[c('salesquantity','store_group','time_group', 'avg_female','avg_age','avg_income',
                            'avg_homeowner','avg_residency','avg_childowner')]
cor(df6a)


############# Poisson
poisson6e <- glm(returnquantity ~ time_group*store_group*fproduct_category + avg_female + avg_age + avg_income + 
                         avg_homeowner + avg_childowner, data=Online_daily_prod, family='poisson')

poisson6f <- glm(returnquantity ~ time_group*store_group*fproduct_category + avg_female + avg_age + avg_income + 
                        avg_homeowner + avg_childowner + avg_residency, data=Online_daily_prod, family='poisson')
lrtest(poisson6e, poisson6f) 
# significant ==> with avg_residency is better(poisson6f)
poisson6g <- glm(returnquantity ~ time_group*store_group*fproduct_category + avg_female + avg_age + avg_income + 
                        avg_homeowner + avg_childowner + avg_residency + lnsalesquantity, data=Online_daily_prod, family='poisson')

lrtest(poisson6g, poisson6f) # significant ==> with log(salesquantity+1) is better(poisson6g)

stargazer(poisson6g, type='text', title='Q6-returnquantity across products', 
          column.labels =c('Poisson-IRR'),
          apply.coef = exp, t.auto = F, p.auto = F,
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))


#### Heteroskedasticity
bptest(poisson6g)  #significant 
gqtest(poisson6g)  # significant

HWrobstder6f <- sqrt(diag(vcovHC(poisson6g, type='HC1')))
stargazer(poisson6g,poisson6g, type='text', title='Q6- Impact on Online return quantity across products',
          apply.coef = exp, t.auto = F, p.auto = F,
          se=list(NULL, HWrobstder6e),
          column.labels = c('Normal SE', 'HW-robust SE'), df=F, digits=2, star.cutoffs = c(0.05,0.01,0.001))


##### Model Fit
poisson6h <- glm(salesquantity ~ 1, data=Online_daily_prod, family='poisson')
lrtest(poisson6g, poisson6h)
# significant ==> poisson does not fit the model



########### Negative Binomial

negbin6f <- glm.nb(returnquantity ~ time_group*store_group*fproduct_category + avg_female + avg_age + avg_income + 
                           avg_homeowner + avg_childowner + avg_residency, data=Online_daily_prod)

negbin6g <- glm.nb(returnquantity ~ time_group*store_group*fproduct_category + avg_female + avg_age + avg_income + 
                           avg_homeowner + avg_childowner , data=Online_daily_prod)
lrtest(negbin6f, negbin6g) # insignificant without avg_residency is better(negbin6g)

negbin6h <- glm.nb(returnquantity ~ time_group*store_group*fproduct_category+ avg_female+ avg_age + avg_income + 
                           avg_homeowner + avg_childowner+lnsalesquantity, data=Online_daily_prod)

lrtest(negbin6g, negbin6h) # significant ==> with log(salesquantity+1) is better (negbin6h)



### Heteroskedasticity
bptest(negbin6h)  #significant 
gqtest(negbin6h)  # significant

HWrobstder6g <- sqrt(diag(vcovHC(negbin6h, type='HC1')))
stargazer(negbin6h,negbin6h, type='text', title='Q6- Impact on Online return quantity across products-NB',
          apply.coef = exp, t.auto = F, p.auto = F,
          se=list(NULL, HWrobstder6g),
          column.labels = c('Normal SE', 'HW-robust SE'), df=F, digits=2, star.cutoffs = c(0.05,0.01,0.001))


#### Model Fit
negbin6i <- glm.nb(returnquantity~1, data=Online_daily_prod)
lrtest(negbin6i, negbin6h)
# significant, NB fit the model

#### Choose model (poisson & negative binomial)
lrtest(negbin6h, poisson6g)
# significant ==> NB is better


##### Marginal Effects

meffects6d <- ggpredict(negbin6h, terms=c('time_group','store_group','fproduct_category'))

ggplot(meffects6d, aes(x,predicted, color=group)) + geom_line(size=1.3)+
        xlab('Time') + ylab('Online_ReturnQuantity(predicted)') + 
        scale_color_discrete(labels=c('Policy changed', 'Policy unchanged'))+ 
        scale_x_continuous(breaks = c(0,1), labels = c('BefOct', 'AftOct'))+
        facet_wrap(~facet,ncol=4)



online_returnquantity_pc <- list()
for (n in c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,20,21)) {
        online_returnquantity_pc[[n]] <- subset(Online_daily_prod, Online_daily_prod$fproduct_category==n) 
}

model_online_returnquantity_pc<-list()
for (m in c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,20,21)) {
        model_online_returnquantity_pc[[m]] <- glm.nb( 
                returnquantity ~ time_group*store_group
                + avg_female 
                + avg_age 
                + avg_income 
                + avg_homeowner 
                + avg_childowner
                + lnreturnquantity
                , data=online_returnquantity_pc[[m]] 
        )
}


for (n in c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,20,21)) {
        stargazer(model_online_returnquantity_pc[[n]],type="text",titile=paste("model_online_returnquantity_pc",n,"Results"),
                  apply.coef = exp, t.auto = F, p.auto = F, 
                  digits=2,df=FALSE,star.cutoffs = c(0.05,0.01,0.001))
}
