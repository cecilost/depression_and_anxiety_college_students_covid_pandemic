
# Script for the manuscript entitled: 
# "A longitudinal study on depression and anxiety in college students during the first 106-days of the lengthy Argentinean quarantine for the COVID-19 pandemic"
# López Steinmetz, L. C., Godoy, J. C. & Fong, S. B. (currently under review).

####################################################################################
####################################################################################
############################### METHOD #############################################
####################################################################################
####################################################################################

# Use the sheet named as "sheet" from the Excel file
dateData<-read.table("clipboard",header=TRUE, encoding = "Latin 1", dec=",", sep="\t")


######  Section: METHOD. Sub-tile > Sample and procedure

table(dateData$quarantinesubperiod)
# quar first quar second  quar third 
# 866         298         328 
prop.table(table(dateData$quarantinesubperiod))*100
# quar first quar second  quar third 
# 58.04290    19.97319    21.98391

table(dateData$sex)
# man woman 
# 232  1260
prop.table(table(dateData$sex))*100
# man   woman 
# 15.5496 84.4504 

dateData$age[dateData$age >= "24"] <- "adult"
dateData$age[dateData$age < "24"] <- "young"
table(dateData$age)
# adult young 
# 534   958
prop.table(table(dateData$age))*100
# adult    young 
# 35.79088 64.20912 

table(dateData$mentdishist)
#   no  yes 
# 1099  393 
prop.table(table(dateData$mentdishist))*100
#  no      yes 
# 73.65952 26.34048

table(dateData$suic)
#   no  yes 
# 754 738 
prop.table(table(dateData$suic))*100
#  no      yes 
# 50.53619 49.46381


######  Section: METHOD. Sub-tile > Data analysis

# Testing Skewness and Kurtosis. Criteria: range of acceptable values -1 to +1 for Skewness and -3 to +3 for Kurtosis (Brown, 2006) or near to.
# Reference: Brown, T.A., 2006. Confirmatory factor analysis for applied research. Guilford Press, New York.

library(moments)

# DEPRESSION
# Time 1:
skewness(dateData$ADEPRESSION)
# skewness ADEPRESSION = 0.595362
kurtosis(dateData$ADEPRESSION) 
# kurtosis ADEPRESSION = 2.826949
# Time 2:
skewness(dateData$BDEPRESSION)
# skewness BDEPRESSION = 0.8791074
kurtosis(dateData$BDEPRESSION) 
# kurtosis BDEPRESSION = 3.300738


# ANXIETY-TRAIT
# Time 1:
skewness(dateData$AANXIETY)
# skewness AANXIETY = -0.1033472
kurtosis(dateData$AANXIETY) 
# kurtosis AANXIETY = 2.270577
# Time 2:
skewness(dateData$BANXIETY)
# skewness BANXIETY = 0.0004807362
kurtosis(dateData$BANXIETY) 
# kurtosis BANXIETY = 2.249405

#################################################################################################
#################################################################################################
###################################### RESULTS ##################################################
#################################################################################################
#################################################################################################

##################################################################################################
#################################### PERCENTAGES #################################################

percent<-dateData

# TIME 1: DEPRESSION
percent$ADEPRESSION[percent$ADEPRESSION > "20"] <- "depressed"
percent$ADEPRESSION[percent$ADEPRESSION <= "20"] <- "no depressed"

prop.table(table(percent$ADEPRESSION[percent$quarantinesubperiod=="quar first"],percent$quarantinesubperiod[percent$quarantinesubperiod=="quar first"]))*100
#               quar first quar second quar third
# depressed      58.66051     0.00000    0.00000
# no depressed   41.33949     0.00000    0.00000

prop.table(table(percent$ADEPRESSION[percent$quarantinesubperiod=="quar second"],percent$quarantinesubperiod[percent$quarantinesubperiod=="quar second"]))*100
#               quar first quar second quar third
# depressed       0.00000    60.40268    0.00000
# no depressed    0.00000    39.59732    0.00000

prop.table(table(percent$ADEPRESSION[percent$quarantinesubperiod=="quar third"],percent$quarantinesubperiod[percent$quarantinesubperiod=="quar third"]))*100
#                quar first quar second quar third
# depressed        0.0000      0.0000    55.4878
# no depressed     0.0000      0.0000    44.5122


# TIME 2: DEPRESSION
percent$BDEPRESSION[percent$BDEPRESSION > "20"] <- "depressed"
percent$BDEPRESSION[percent$BDEPRESSION <= "20"] <- "no depressed"

prop.table(table(percent$BDEPRESSION[percent$quarantinesubperiod=="quar first"],percent$quarantinesubperiod[percent$quarantinesubperiod=="quar first"]))*100
#               quar first quar second quar third
# depressed      54.61894     0.00000    0.00000
# no depressed   45.38106     0.00000    0.00000

prop.table(table(percent$BDEPRESSION[percent$quarantinesubperiod=="quar second"],percent$quarantinesubperiod[percent$quarantinesubperiod=="quar second"]))*100
#               quar first quar second quar third
# depressed       0.00000    62.41611    0.00000
# no depressed    0.00000    37.58389    0.00000

prop.table(table(percent$BDEPRESSION[percent$quarantinesubperiod=="quar third"],percent$quarantinesubperiod[percent$quarantinesubperiod=="quar third"]))*100
#               quar first quar second quar third
# depressed       0.00000     0.00000   57.62195
# no depressed    0.00000     0.00000   42.37805


# TIME 1: ANXIETY
percent$AANXIETY[percent$AANXIETY > "30"] <- "anxious"
percent$AANXIETY[percent$AANXIETY <= "30"] <- "no anxious"

prop.table(table(percent$AANXIETY[percent$quarantinesubperiod=="quar first"],percent$quarantinesubperiod[percent$quarantinesubperiod=="quar first"]))*100
#             quar first quar second quar third
# anxious      52.54042     0.00000    0.00000
# no anxious   47.45958     0.00000    0.00000

prop.table(table(percent$AANXIETY[percent$quarantinesubperiod=="quar second"],percent$quarantinesubperiod[percent$quarantinesubperiod=="quar second"]))*100
#              quar first quar second quar third
# anxious       0.00000    55.03356    0.00000
# no anxious    0.00000    44.96644    0.00000

prop.table(table(percent$AANXIETY[percent$quarantinesubperiod=="quar third"],percent$quarantinesubperiod[percent$quarantinesubperiod=="quar third"]))*100
#              quar first quar second quar third
# anxious       0.00000     0.00000   53.04878
# no anxious    0.00000     0.00000   46.95122


# TIME 2: ANXIETY
percent$BANXIETY[percent$BANXIETY > "30"] <- "anxious"
percent$BANXIETY[percent$BANXIETY <= "30"] <- "no anxious"

prop.table(table(percent$BANXIETY[percent$quarantinesubperiod=="quar first"],percent$quarantinesubperiod[percent$quarantinesubperiod=="quar first"]))*100
#             quar first quar second quar third
# anxious      50.23095     0.00000    0.00000
# no anxious   49.76905     0.00000    0.00000

prop.table(table(percent$BANXIETY[percent$quarantinesubperiod=="quar second"],percent$quarantinesubperiod[percent$quarantinesubperiod=="quar second"]))*100
#              quar first quar second quar third
# anxious       0.00000    54.36242    0.00000
# no anxious    0.00000    45.63758    0.00000

prop.table(table(percent$BANXIETY[percent$quarantinesubperiod=="quar third"],percent$quarantinesubperiod[percent$quarantinesubperiod=="quar third"]))*100
#              quar first quar second quar third
# anxious       0.00000     0.00000   49.08537
# no anxious    0.00000     0.00000   50.91463


# DEPRESSION
cor.test(dateData$ADEPRESSION, dateData$BDEPRESSION,exact = F,method = "pearson",conf.level=0.95)
# Pearson's product-moment correlation
# data:  dateData$ADEPRESSION and dateData$BDEPRESSION
# t = 41.711, df = 1490, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.7096243 0.7565134
# sample estimates:
#  cor 
# 0.7339421 

library(ggplot2)
# Fig. 2a.
# Scatterplot with the 2D density estimation. FIRST MEASUREMENT AND FOLLOW-UP: DEPRESSION
# COLOR VERSION
depr <- ggplot(dateData, aes(x=ADEPRESSION, y=BDEPRESSION)) +
  geom_point()
depr + stat_density_2d(aes(fill = ..level..), alpha = 0.8, geom="polygon", contour_var = "ndensity") +
  scale_fill_gradient(low="blue", high="red") + labs(x = "Depression (1st measurement)", y = "Depression (follow-up)") + theme(axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold"))

# BLACK & WHITE VERSION
# Scatterplot with the 2D density estimation. FIRST MEASUREMENT AND FOLLOW-UP: DEPRESSION
deprbw <- ggplot(dateData, aes(x=ADEPRESSION, y=BDEPRESSION)) +
  geom_point()
deprbw + stat_density_2d(aes(fill = ..level..), alpha = 0.9, geom="polygon", contour_var = "ndensity") +
  scale_fill_gradient(low="gray54", high="black") + labs(x = "Depression (1st measurement)", y = "Depression (follow-up)") + theme(axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold"))


# ANXIETY
cor.test(dateData$AANXIETY, dateData$BANXIETY,exact = F,method = "pearson",conf.level=0.95)
# Pearson's product-moment correlation
# data:  dateData$AANXIETY and dateData$BANXIETY
# t = 55.832, df = 1490, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.8054279 0.8383100
# sample estimates:
#  cor 
# 0.8225553

# Fig. 2b.
# Scatterplot with the 2D density estimation. FIRST MEASUREMENT AND FOLLOW-UP: ANXIETY
# COLOR VERSION
anx <- ggplot(dateData, aes(x=AANXIETY, y=BANXIETY)) +
  geom_point()
anx + stat_density_2d(aes(fill = ..level..), alpha = 0.8, geom="polygon", contour_var = "ndensity") +
  scale_fill_gradient(low="blue", high="red") + labs(x = "Anxiety (1st measurement)", y = "Anxiety (follow-up)") + theme(axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold"))

# BLACK & WHITE VERSION
anxbw <- ggplot(dateData, aes(x=AANXIETY, y=BANXIETY)) +
  geom_point()
anxbw + stat_density_2d(aes(fill = ..level..), alpha = 0.9, geom="polygon", contour_var = "ndensity") +
  scale_fill_gradient(low="gray54", high="black") + labs(x = "Anxiety (1st measurement)", y = "Anxiety (follow-up)") + theme(axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold"))



##################################################################################################
########################### MIXED DESIGN MODELLING ###############################################

# We have followed the steps indicated in Field et al. (2012) for carrying out these analyses.
# Rference: Field, A., Miles, J., Field, Z., 2012. Discovering statistics using R. SAGE, London.

##################################################################################################
######################## DEPRESSION (outcome variable) ###########################################
##################################################################################################

# DEPRESSION: SCORES IN TIMES 1 AND 2 (within), QUARANTINE SUB-PERIODS (between), SEX (between), AGE, (between) MENTDISHIST (between), and SUIC (between) #############################

####### Preparing the data

# Use the sheet named as "depressionData" from the Excel file
deprData<-read.table("clipboard",header=TRUE, encoding = "Latin 1", dec=",", sep="\t")

deprData$age[deprData$age >= "24"] <- "adult"
deprData$age[deprData$age < "24"] <- "young"

# To convert the format of the data frame into the long format
library(reshape)
myData<-melt(deprData, id = c("participant","quarantinesubperiod","sex","age","mentdishist","suic"), measured = c("ADEPRESSION","BDEPRESSION"))

# To rename variables:
names(myData)<-c("participant", "quarantinesubperiod","sex","age","mentdishist","suic", "groups", "scores") # "groups" refers to the repeated-measures variable

# This creates a variable period in the dataframe myData:
myData$period<-gl(2, 1492, labels = c("first", "second"))
# We created 2 sets of 1492 scores, the labels option then specifies the names to attach to these 2 sets, which correspond to the levels of "period" (time 1 and time 2).

# To make it clearer that there are two observations for each participant we have sorted the data by participant by executing:
myData<-myData[order(myData$participant),]

# Descriptive statistics:
library(pastecs)
by(myData$scores, list(myData$period, myData$quarantinesubperiod,myData$sex,myData$age,myData$mentdishist, myData$suic), stat.desc, basic = FALSE)

by(myData$scores, list(myData$period), stat.desc, basic = FALSE)
mean(myData$scores)
library("plotrix")
std.error(myData$scores)

by(myData$scores, list(myData$period, myData$quarantinesubperiod), stat.desc, basic = FALSE)
by(myData$scores, list(myData$quarantinesubperiod), stat.desc, basic = FALSE)

by(myData$scores, list(myData$period, myData$sex), stat.desc, basic = FALSE)
by(myData$scores, list(myData$sex), stat.desc, basic = FALSE)

by(myData$scores, list(myData$period, myData$age), stat.desc, basic = FALSE)
by(myData$scores, list(myData$age), stat.desc, basic = FALSE)

by(myData$scores, list(myData$period, myData$mentdishist), stat.desc, basic = FALSE)
by(myData$scores, list(myData$mentdishist), stat.desc, basic = FALSE)

by(myData$scores, list(myData$period, myData$suic), stat.desc, basic = FALSE)
by(myData$scores, list(myData$suic), stat.desc, basic = FALSE)



####### Mixed designs as a GLM

# Setting contrasts
quarfirstvsquarsecond<-c(0,1,0) # this compares the quarantine first (or baseline) with the quarantine second
quarfirstvsquarthird<-c(0,0,1) # this compares the quarantine first (or baseline) with the quarantine third
contrasts(myData$quarantinesubperiod)<-cbind(quarfirstvsquarsecond, quarfirstvsquarthird)
myData$quarantinesubperiod # To check we setted the contrasts correctly 

# Building the model
library(nlme)

# Baseline model (model 1)
baseline<-lme(scores ~ 1, random = ~1|participant/period, data = myData, method = "ML")

# To see the overall effect of each main effect and interaction we added them to the model one at a time:

# Model 2
periodM<-update(baseline, .~. + period) 

# Model 3
quarantineM<-update(periodM, .~. + quarantinesubperiod)

# Model 4
sexM<-update(quarantineM, .~. + sex)

# Model 5
ageM<-update(sexM, .~. + age)

# Model 6
mentdishistM<-update(ageM, .~. + mentdishist)

# Model 7
suicM<-update(mentdishistM, .~. + suic)

# To see interactions:

# Model 8
period_quarantine<-update(suicM, .~. + period:quarantinesubperiod,control = lmeControl(opt = "optim"))

# Model 9
period_sex<-update(period_quarantine, .~. + period:sex,control = lmeControl(opt = "optim"))

# Model 10
period_age<-update(period_sex, .~. + period:age,control = lmeControl(opt = "optim"))

# Model 11
period_mentdishist<-update(period_age, .~. + period:mentdishist,control = lmeControl(opt = "optim"))

# Model 12
period_suic<-update(period_mentdishist, .~. + period:suic,control = lmeControl(opt = "optim"))

# Model 13
per_quar_sex<-update(period_suic, .~. + period:quarantinesubperiod:sex)

# Model 14
per_quar_age<-update(per_quar_sex, .~. + period:quarantinesubperiod:age)

# Model 15
per_quar_ment<-update(per_quar_age, .~. + period:quarantinesubperiod:mentdishist)

# Model 16
per_quar_suic<-update(per_quar_ment, .~. + period:quarantinesubperiod:suic)

# Model 17
per_sex_age<-update(per_quar_suic, .~. + period:sex:age)

# Model 18
per_sex_ment<-update(per_sex_age, .~. + period:sex:mentdishist)

# Model 19
per_sex_suic<-update(per_sex_ment, .~. + period:sex:suic)

# Model 20
per_age_ment<-update(per_sex_suic, .~. + period:age:mentdishist)

# Model 21
per_age_suic<-update(per_age_ment, .~. + period:age:suic)

# Model 22
per_ment_suic<-update(per_age_suic, .~. + period:mentdishist:suic)

# Model 23
per_sex_age_ment<-update(per_ment_suic, .~. + period:sex:age:mentdishist)

# Model 24
per_sex_age_suic<-update(per_sex_age_ment, .~. + period:sex:age:suic)

# To compare these models we listed them in the order in which we want them compared in the anova() function
anova(baseline,periodM,quarantineM,sexM,ageM,mentdishistM,suicM,period_quarantine,period_sex,period_age,period_mentdishist,period_suic,per_quar_sex,per_quar_age,per_quar_ment,per_quar_suic,per_sex_age,per_sex_ment,per_sex_suic,per_age_ment,per_age_suic,per_ment_suic,per_sex_age_ment,per_sex_age_suic)
#                     Model df      AIC      BIC    logLik     Test   L.Ratio p-value
# baseline               1  4 21990.02 22014.02 -10991.01                           
# periodM                2  5 21989.05 22019.06 -10989.53   1 vs 2   2.96358  0.0852
# quarantineM            3  7 21991.58 22033.59 -10988.79   2 vs 3   1.46707  0.4802
# sexM                   4  8 21971.17 22019.18 -10977.59   3 vs 4  22.41221  <.0001
# ageM                   5  9 21963.78 22017.79 -10972.89   4 vs 5   9.39482  0.0022
# mentdishistM           6 10 21878.38 21938.39 -10929.19   5 vs 6  87.39935  <.0001
# suicM                  7 11 21697.17 21763.19 -10837.59   6 vs 7 183.20339  <.0001
# period_quarantine      8 13 21684.58 21762.59 -10829.29   7 vs 8  16.59460  0.0002 ### THIS IS THE MODEL FOR DEPRESSION
# period_sex             9 14 21686.56 21770.58 -10829.28   8 vs 9   0.01952  0.8889
# period_age            10 15 21688.19 21778.21 -10829.10  9 vs 10   0.37008  0.5430
# period_mentdishist    11 16 21687.92 21783.94 -10827.96 10 vs 11   2.27178  0.1317
# period_suic           12 17 21689.92 21791.93 -10827.96 11 vs 12   0.00343  0.9533
# per_quar_sex          13 21 21694.54 21820.56 -10826.27 12 vs 13   3.38014  0.4963
# per_quar_age          14 25 21692.62 21842.65 -10821.31 13 vs 14   9.91218  0.0419
# per_quar_ment         15 29 21693.09 21867.12 -10817.55 14 vs 15   7.53357  0.1102
# per_quar_suic         16 33 21697.54 21895.58 -10815.77 15 vs 16   3.54466  0.4711
# per_sex_age           17 35 21699.56 21909.59 -10814.78 16 vs 17   1.98885  0.3699
# per_sex_ment          18 37 21700.41 21922.45 -10813.21 17 vs 18   3.14467  0.2076
# per_sex_suic          19 39 21703.93 21937.97 -10812.97 18 vs 19   0.47993  0.7867
# per_age_ment          20 41 21704.95 21950.99 -10811.47 19 vs 20   2.98593  0.2247
# per_age_suic          21 43 21707.49 21965.53 -10810.74 20 vs 21   1.45838  0.4823
# per_ment_suic         22 45 21708.46 21978.50 -10809.23 21 vs 22   3.03240  0.2195
# per_sex_age_ment      23 47 21711.69 21993.74 -10808.84 22 vs 23   0.76570  0.6819
# per_sex_age_suic      24 49 21715.04 22009.09 -10808.52 23 vs 24   0.65209  0.7218


# We can see the model parameters by executing:
summary(period_quarantine) 
# Here is a fragment of the summary:
# Fixed effects: scores ~ period + quarantinesubperiod + sex + age + mentdishist + suic + period:quarantinesubperiod 
#                                                           Value Std.Error   DF   t-value p-value
# (Intercept)                                           10.546368 0.7931079 1489 13.297519  0.0000
# periodsecond                                           0.254042 0.2895067 1489  0.877498  0.3804
# quarantinesubperiodquarfirstvsquarsecond               1.279862 0.7189888 1485  1.780086  0.0753
# quarantinesubperiodquarfirstvsquarthird                2.436832 0.6938018 1485  3.512288  0.0005 # do not interpret this because interaction is meaningful
# sexwoman                                               2.413158 0.7056239 1485  3.419893  0.0006 ###
# ageyoung                                               1.787253 0.5335638 1485  3.349651  0.0008 ###
# mentdishistyes                                         3.880390 0.5960220 1485  6.510482  0.0000 ###
# suicyes                                                7.310812 0.5245168 1485 13.938185  0.0000 ###
# periodsecond:quarantinesubperiodquarfirstvsquarsecond -0.703706 0.5721724 1489 -1.229884  0.2189
# periodsecond:quarantinesubperiodquarfirstvsquarthird  -2.250993 0.5523620 1489 -4.075213  0.0000 ###

# Correlation: 
#                                                        (Intr) prdscn qrntnsbprdqrfrstvsqrs qrntnsbprdqrfrstvsqrt sexwmn ageyng mntdsh suicys prdscnd:qrntnsbprdqrfrstvsqrs
# periodsecond                                          -0.183                                                                                                             
# quarantinesubperiodquarfirstvsquarsecond              -0.220  0.201                                                                                                      
# quarantinesubperiodquarfirstvsquarthird               -0.284  0.209  0.265                                                                                               
# sexwoman                                              -0.687  0.000 -0.051                 0.020                                                                         
# ageyoung                                              -0.395  0.000  0.052                 0.022                -0.067                                                   
# mentdishistyes                                        -0.119  0.000 -0.007                 0.015                -0.060  0.097                                            
# suicyes                                               -0.225  0.000  0.017                 0.051                -0.056 -0.061 -0.235                                     
# periodsecond:quarantinesubperiodquarfirstvsquarsecond  0.092 -0.506 -0.398                -0.106                 0.000  0.000  0.000  0.000                              
# periodsecond:quarantinesubperiodquarfirstvsquarthird   0.096 -0.524 -0.106                -0.398                 0.000  0.000  0.000  0.000  0.265  

# Standardized Within-Group Residuals:
#  Min          Q1         Med          Q3         Max 
# -1.56070687 -0.21297185 -0.03567619  0.18738947  1.84373854 

# Number of Observations: 2984
# Number of Groups: 
#           participant period %in% participant 
#                  1492                    2984 


# To see the 95% CI:
intervals(period_quarantine, which = "fixed")
# Approximate 95% confidence intervals
# Fixed effects:
#                                                           lower       est.      upper
# (Intercept)                                            8.9932491 10.5463678 12.0994864
# periodsecond                                          -0.3128903  0.2540416  0.8209735
# quarantinesubperiodquarfirstvsquarsecond              -0.1281146  1.2798620  2.6878386
# quarantinesubperiodquarfirstvsquarthird                1.0781780  2.4368315  3.7954850
# sexwoman                                               1.0313536  2.4131578  3.7949621
# ageyoung                                               0.7423889  1.7872526  2.8321163
# mentdishistyes                                         2.7132164  3.8803903  5.0475642
# suicyes                                                6.2836650  7.3108122  8.3379594
# periodsecond:quarantinesubperiodquarfirstvsquarsecond -1.8241735 -0.7037060  0.4167615
# periodsecond:quarantinesubperiodquarfirstvsquarthird  -3.3326662 -2.2509928 -1.1693194
# attr(,"label")
# [1] "Fixed effects:"

# Calculating effect sizes
library(DSUR.noof)
# We got effect sizes of meaningful predictors by executing: rcontrast(t,df)

# sexwoman
rcontrast(3.419893,1485)
# [1] "r =  0.0883986774352474"

# ageyoung
rcontrast(3.349651,1485)
# [1] "r =  0.0865967930335736"

# mentdishistyes
rcontrast(6.510482,1485)
# [1] "r =  0.166586071384292"

# suicyes
rcontrast(13.938185,1485)
# [1] "r =  0.340130495520768"

# periodsecond:quarantinesubperiodquarfirstvsquarthird
rcontrast(-4.075213,1489)
# [1] "r =  0.105025425167657"



##################################################################################################
######################## ANXIETY-TRAIT (outcome variable) ########################################
##################################################################################################

# ANXIETY-TRAIT: SCORES IN TIMES 1 AND 2 (within), QUARANTINE SUB-PERIODS (between), SEX (between), AGE, (between) MENTDISHIST (between), and SUIC (between) #############################

####### Preparing the data

# Use the sheet named as "anxietyData" from the Excel file
anxData<-read.table("clipboard",header=TRUE, encoding = "Latin 1", dec=",", sep="\t")

anxData$age[anxData$age >= "24"] <- "adult"
anxData$age[anxData$age < "24"] <- "young"

# To convert the format of the dta frame into the long format
myData<-melt(anxData, id = c("participant","quarantinesubperiod","sex","age","mentdishist","suic"), measured = c("AANXIETY","BANXIETY"))

# To rename variables:
names(myData)<-c("participant", "quarantinesubperiod","sex","age","mentdishist","suic", "groups", "scores") # "groups" refers to the repeated-measures variable

# This creates a variable period in the dataframe myData:
myData$period<-gl(2, 1492, labels = c("first", "second"))
# We created 2 sets of 1492 scores, the labels option then specifies the names to attach to these 2 sets, which correspond to the levels of "period" (time 1 and time 2).

# To make it clearer that there are two observations for each participant we have sorted the data by participant by executing:
myData<-myData[order(myData$participant),]

# Descriptive statistics:
by(myData$scores, list(myData$period, myData$quarantinesubperiod,myData$sex,myData$age,myData$mentdishist, myData$suic), stat.desc, basic = FALSE)

by(myData$scores, list(myData$period), stat.desc, basic = FALSE)
mean(myData$scores)
std.error(myData$scores)

by(myData$scores, list(myData$period, myData$quarantinesubperiod), stat.desc, basic = FALSE)
by(myData$scores, list(myData$quarantinesubperiod), stat.desc, basic = FALSE)

by(myData$scores, list(myData$period, myData$sex), stat.desc, basic = FALSE)
by(myData$scores, list(myData$sex), stat.desc, basic = FALSE)

by(myData$scores, list(myData$period, myData$age), stat.desc, basic = FALSE)
by(myData$scores, list(myData$age), stat.desc, basic = FALSE)

by(myData$scores, list(myData$period, myData$mentdishist), stat.desc, basic = FALSE)
by(myData$scores, list(myData$mentdishist), stat.desc, basic = FALSE)

by(myData$scores, list(myData$period, myData$suic), stat.desc, basic = FALSE)
by(myData$scores, list(myData$suic), stat.desc, basic = FALSE)



####### Mixed designs as a GLM

# Setting contrasts
quarfirstvsquarsecond<-c(0,1,0) # this compares the quarantine first (or baseline) with the quarantine second
quarfirstvsquarthird<-c(0,0,1) # this compares the quarantine first (or baseline) with the quarantine third
contrasts(myData$quarantinesubperiod)<-cbind(quarfirstvsquarsecond, quarfirstvsquarthird)
myData$quarantinesubperiod # To check we setted the contrasts correctly 

# Building the model

# Baseline model (model 1)
baseline<-lme(scores ~ 1, random = ~1|participant/period, data = myData, method = "ML")

# To see the overall effect of each main effect and interaction we added them to the model one at a time:

# Model 2
periodM<-update(baseline, .~. + period) 

# Model 3
quarantineM<-update(periodM, .~. + quarantinesubperiod)

# Model 4
sexM<-update(quarantineM, .~. + sex)

# Model 5
ageM<-update(sexM, .~. + age)

# Model 6
mentdishistM<-update(ageM, .~. + mentdishist)

# Model 7
suicM<-update(mentdishistM, .~. + suic)

# To see interactions:

# Model 8
period_quarantine<-update(suicM, .~. + period:quarantinesubperiod,control = lmeControl(opt = "optim"))

# Model 9
period_sex<-update(period_quarantine, .~. + period:sex,control = lmeControl(opt = "optim"))

# Model 10
period_age<-update(period_sex, .~. + period:age,control = lmeControl(opt = "optim"))

# Model 11
period_mentdishist<-update(period_age, .~. + period:mentdishist,control = lmeControl(opt = "optim"))

# Model 12
period_suic<-update(period_mentdishist, .~. + period:suic,control = lmeControl(opt = "optim"))

# Model 13
per_quar_sex<-update(period_suic, .~. + period:quarantinesubperiod:sex)

# Model 14
per_quar_age<-update(per_quar_sex, .~. + period:quarantinesubperiod:age)

# Model 15
per_quar_ment<-update(per_quar_age, .~. + period:quarantinesubperiod:mentdishist)

# Model 16
per_quar_suic<-update(per_quar_ment, .~. + period:quarantinesubperiod:suic)

# Model 17
per_sex_age<-update(per_quar_suic, .~. + period:sex:age)

# Model 18
per_sex_ment<-update(per_sex_age, .~. + period:sex:mentdishist)

# Model 19
per_sex_suic<-update(per_sex_ment, .~. + period:sex:suic)

# Model 20
per_age_ment<-update(per_sex_suic, .~. + period:age:mentdishist)

# Model 21
per_age_suic<-update(per_age_ment, .~. + period:age:suic)

# Model 22
per_ment_suic<-update(per_age_suic, .~. + period:mentdishist:suic)

# Model 23
per_sex_age_ment<-update(per_ment_suic, .~. + period:sex:age:mentdishist)

# Model 24
per_sex_age_suic<-update(per_sex_age_ment, .~. + period:sex:age:suic)

# To compare these models we listed them in the order in which we want them compared in the anova() function
anova(baseline,periodM,quarantineM,sexM,ageM,mentdishistM,suicM,period_quarantine,period_sex,period_age,period_mentdishist,period_suic,per_quar_sex,per_quar_age,per_quar_ment,per_quar_suic,per_sex_age,per_sex_ment,per_sex_suic,per_age_ment,per_age_suic,per_ment_suic,per_sex_age_ment,per_sex_age_suic)
#                     Model df      AIC      BIC    logLik     Test   L.Ratio p-value
# baseline               1  4 21634.07 21658.08 -10813.04                           
# periodM                2  5 21613.70 21643.70 -10801.85   1 vs 2  22.37511  <.0001 # do not interpret this because is meaningful in the interaction
# quarantineM            3  7 21616.62 21658.62 -10801.31   2 vs 3   1.08185  0.5822
# sexM                   4  8 21575.38 21623.39 -10779.69   3 vs 4  43.23256  <.0001
# ageM                   5  9 21570.23 21624.24 -10776.11   4 vs 5   7.15405  0.0075
# mentdishistM           6 10 21477.07 21537.08 -10728.54   5 vs 6  95.15580  <.0001
# suicM                  7 11 21278.10 21344.11 -10628.05   6 vs 7 200.97678  <.0001
# period_quarantine      8 13 21270.71 21348.72 -10622.35   7 vs 8  11.38646  0.0034 ### THIS IS THE MODEL FOR ANXIETY-TRAIT
# period_sex             9 14 21272.65 21356.66 -10622.32   8 vs 9   0.06300  0.8018
# period_age            10 15 21274.24 21364.26 -10622.12  9 vs 10   0.40352  0.5253
# period_mentdishist    11 16 21275.88 21371.89 -10621.94 10 vs 11   0.36760  0.5443
# period_suic           12 17 21277.70 21379.72 -10621.85 11 vs 12   0.17477  0.6759
# per_quar_sex          13 21 21285.27 21411.29 -10621.64 12 vs 13   0.43012  0.9799
# per_quar_age          14 25 21285.71 21435.74 -10617.86 13 vs 14   7.55765  0.1092
# per_quar_ment         15 29 21289.47 21463.50 -10615.73 14 vs 15   4.24667  0.3737
# per_quar_suic         16 33 21292.59 21490.62 -10613.29 15 vs 16   4.87822  0.3000
# per_sex_age           17 35 21294.70 21504.73 -10612.35 16 vs 17   1.89215  0.3883
# per_sex_ment          18 37 21293.21 21515.24 -10609.60 17 vs 18   5.49113  0.0642
# per_sex_suic          19 39 21295.60 21529.64 -10608.80 18 vs 19   1.60343  0.4486
# per_age_ment          20 41 21296.89 21542.93 -10607.44 19 vs 20   2.71360  0.2575
# per_age_suic          21 43 21300.20 21558.24 -10607.10 20 vs 21   0.68908  0.7085
# per_ment_suic         22 45 21304.00 21574.05 -10607.00 21 vs 22   0.19766  0.9059
# per_sex_age_ment      23 47 21306.22 21588.27 -10606.11 22 vs 23   1.77748  0.4112
# per_sex_age_suic      24 49 21307.30 21601.35 -10604.65 23 vs 24   2.92516  0.2316


# We can see the model parameters by executing:
summary(period_quarantine) 
# Here is a fragment of the summary:
# Fixed effects: scores ~ period + quarantinesubperiod + sex + age + mentdishist + suic + period:quarantinesubperiod 
#                                                           Value Std.Error   DF   t-value p-value
# (Intercept)                                           20.324775 0.8179576 1489 24.848200  0.0000
# periodsecond                                          -0.398383 0.2429467 1489 -1.639798  0.1013
# quarantinesubperiodquarfirstvsquarsecond               0.977068 0.7251328 1485  1.347434  0.1780
# quarantinesubperiodquarfirstvsquarthird                1.371675 0.6997134 1485  1.960338  0.0501 # do not interpret this because interaction is meaningful
# sexwoman                                               3.998114 0.7319573 1485  5.462223  0.0000 ###
# ageyoung                                               1.632492 0.5534761 1485  2.949525  0.0032 ###
# mentdishistyes                                         4.231609 0.6182652 1485  6.844326  0.0000 ###
# suicyes                                                7.967192 0.5440914 1485 14.643112  0.0000 ###
# periodsecond:quarantinesubperiodquarfirstvsquarsecond -0.725778 0.4801526 1489 -1.511556  0.1309
# periodsecond:quarantinesubperiodquarfirstvsquarthird  -1.534543 0.4635282 1489 -3.310572  0.0010 ###

# Correlation: 
#                                                       (Intr) prdscn qrntnsbprdqrfrstvsqrs qrntnsbprdqrfrstvsqrt sexwmn ageyng mntdsh suicys prdscnd:qrntnsbprdqrfrstvsqrs
# periodsecond                                          -0.149                                                                                                             
# quarantinesubperiodquarfirstvsquarsecond              -0.215  0.168                                                                                                      
# quarantinesubperiodquarfirstvsquarthird               -0.281  0.174  0.265                                                                                               
# sexwoman                                              -0.691  0.000 -0.053                 0.021                                                                         
# ageyoung                                              -0.397  0.000  0.054                 0.022                -0.067                                                   
# mentdishistyes                                        -0.120  0.000 -0.007                 0.015                -0.060  0.097                                            
# suicyes                                               -0.226  0.000  0.017                 0.052                -0.056 -0.061 -0.235                                     
# periodsecond:quarantinesubperiodquarfirstvsquarsecond  0.075 -0.506 -0.331                -0.088                 0.000  0.000  0.000  0.000                              
# periodsecond:quarantinesubperiodquarfirstvsquarthird   0.078 -0.524 -0.088                -0.331                 0.000  0.000  0.000  0.000  0.265        

# Standardized Within-Group Residuals:
#  Min           Q1          Med           Q3          Max 
# -1.771071839 -0.227788992  0.003193551  0.231798181  1.820794252 

# Number of Observations: 2984
# Number of Groups: 
#             participant period %in% participant 
#                    1492                    2984

# To see the 95% CI:
intervals(period_quarantine, which = "fixed")
# Approximate 95% confidence intervals
# Fixed effects:
#                                                           lower       est.      upper
# (Intercept)                                           18.722993640 20.3247746 21.92655559
# periodsecond                                          -0.874138275 -0.3983834  0.07737153
# quarantinesubperiodquarfirstvsquarsecond              -0.442939586  0.9770685  2.39707657
# quarantinesubperiodquarfirstvsquarthird                0.001445066  1.3716750  2.74190501
# sexwoman                                               2.564741491  3.9981139  5.43148624
# ageyoung                                               0.548634358  1.6324917  2.71634913
# mentdishistyes                                         3.020876474  4.2316086  5.44234065
# suicyes                                                6.901712363  7.9671920  9.03267172
# periodsecond:quarantinesubperiodquarfirstvsquarsecond -1.666045648 -0.7257777  0.21449024
# periodsecond:quarantinesubperiodquarfirstvsquarthird  -2.442256372 -1.5345435 -0.62683054
# attr(,"label")
# [1] "Fixed effects:"

# Calculating effect sizes
# We got effect sizes of meaningful predictors by executing: rcontrast(t,df)

# period (second)
rcontrast(-1.639798,1489)
# [1] "r =  0.0424571871259571"

# sexwoman
rcontrast(5.462223,1485)
# [1] "r =  0.140341668351773"

# ageyoung
rcontrast(2.949525,1485)
# [1] "r =  0.0763168497513388"

# mentdishistyes
rcontrast(6.844326,1485)
# [1] "r =  0.174873218826131"

# suicyes
rcontrast(14.643112,1485)
# [1] "r =  0.355208155462878"

# periodsecond:quarantinesubperiodquarfirstvsquarthird
rcontrast(-3.310572,1489)
# [1] "r =  0.0854797444410431"


####################################################################################
####################################################################################
################################# END ##############################################
####################################################################################
####################################################################################

