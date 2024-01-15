library(tidyverse)
library(dplyr)
library(readxl)
library(plyr)
library(ggplot2)
library(magrittr)
#Installing older version so as to get IK bandwidths. Has older version of rdplot. Get kink and bw estimates, revert to new.
#packageurl <- "https://cran.r-project.org/src/contrib/Archive/rdrobust/rdrobust_0.80.tar.gz"
#install.packages(packageurl, repos=NULL, type="source")
library(rdrobust)
library(rddensity)
library(lmtest)
library(sandwich)
library(ivreg)



#####Construct workable dataset
#Select relevant variables
dfinvestigate <- tkh_gmc_final2 %>% select(spell_start_date, spell_end_date, uib_days_appointed, uib_start_date, uib_end_date, uib_end_date_actual, uib_first_daily_rate_start, uib_first_daily_rate_end,
                                           uib_second_daily_rate, previousdayilyearnings, maxbaas, age_group, gendermale, education, previousemploymentfield, isspellendreasonemployed, red_ben, red_ben_payment_date)
#103,458 observations

#fix typo
dfinvestigate$previousdailyearnings <- dfinvestigate$previousdayilyearnings
dfinvestigate <- dfinvestigate %>% select(-previousdayilyearnings)

#Delete unknown spell end dates and censored data
dfinvestigate <- dfinvestigate[!is.na(dfinvestigate$spell_end_date),]
#97,737 observations

#Keep only if reentering employment
dfinvestigate <- dfinvestigate %>% filter(dfinvestigate$isspellendreasonemployed == 1)
#84,253

###Fix missing UI end date. I think NA for uib_end_date_actual if uib_end_date(maximum time) is reached, so I have an overcomplicated equation.
#If spell_end_date later than uib_end_date, set uib_end_date_actual to uib_end_date
#If uib_end_date later than spell_end_date, set uib_end_date_actual to spell_end_date
dfinvestigate <- dfinvestigate  %>% mutate(uib_end_date_actual = ifelse(!is.na(uib_end_date_actual), uib_end_date_actual, ifelse(spell_end_date >= uib_end_date, uib_end_date, spell_end_date))) %>% 
  mutate(uib_end_date_actual = as.Date(uib_end_date_actual, origin = "1970-01-01")) 


###Create Unemployment and UI duration
dfinvestigate$Uduration <- dfinvestigate$spell_end_date - dfinvestigate$spell_start_date + 1
dfinvestigate$UIBduration <- dfinvestigate$uib_end_date_actual - dfinvestigate$uib_start_date + 1
#Assign difftime values to numeric 
dfinvestigate$Uduration <- as.numeric(dfinvestigate$Uduration)
dfinvestigate$UIBduration <- as.numeric(dfinvestigate$UIBduration)

##### 8 observations have the actual end date before the start date. They registered for unemployment, and found a job before they received any benefits. Drop
dfinvestigate <- dfinvestigate %>% filter(dfinvestigate$UIBduration >= 1)
#84,245
#Create log of duration
dfinvestigate$logduration <- log(dfinvestigate$Uduration)


####Doing to same with redundancy
####The date of redundancy payments are random. Sometimes before entry to unemployment, sometimes after UI payments end, usually in the middle somewhere.
dfinvestigate <- dfinvestigate %>% mutate(UIBdurationred = ifelse(is.na(red_ben_payment_date), uib_end_date_actual - uib_start_date +1, uib_end_date_actual- red_ben_payment_date + 1))
dfinvestigate$UIBdurationred <- as.numeric(dfinvestigate$UIBdurationred)

###Create average UI payments for each worker
#Test to find first time 2020 reform affects workers
test <- dfinvestigate
test$result = ifelse(test$uib_first_daily_rate_start != 
                       test$uib_first_daily_rate_end, 'Yes', 'No')
test <- test %>% select(uib_start_date, uib_end_date_actual, result)
test <- test[(which(test$result == 'Yes')),]
#2020-04-24 uib_start_date
#2020-08-01 uib_start_date not affected
###Including April 24th, only 99 days between August 1st. August 1st is the 100th day, so UIB start at 4-24 gives you 1 day of 60%.

#Count # of days between 2020-04-24 and 2020-08-01 for UIB start date
dfinvestigate <- dfinvestigate %>% mutate(reformlimbodays = ifelse(uib_start_date >= '2020-08-01', 0, ifelse(uib_start_date <= '2020-04-23', 0, as.Date('2020-08-01') - uib_start_date)))
#Add each day of different UI payments, divide by total UI days.
dfinvestigate <- dfinvestigate %>% mutate(AvgUIB = ifelse(reformlimbodays <= 0, 
                                                          ifelse(UIBduration > 100, 
                                                                 ((100*uib_first_daily_rate_start)+(UIBduration - 100)*uib_second_daily_rate)/UIBduration, 
                                                                 uib_first_daily_rate_start), 
                                                          ifelse(UIBduration > 100, 
                                                                 (reformlimbodays * uib_first_daily_rate_start + (100 - reformlimbodays) * uib_first_daily_rate_end + (UIBduration - 100) * uib_second_daily_rate)/UIBduration, 
                                                                 ifelse(UIBduration >= reformlimbodays, 
                                                                        ((reformlimbodays * uib_first_daily_rate_start) + (UIBduration - reformlimbodays) * uib_first_daily_rate_end)/UIBduration, 
                                                                        uib_first_daily_rate_start))))             
###Do the same with redundancy
dfinvestigate <- dfinvestigate %>% mutate(AvgUIBred = ifelse(red_ben <= 0, 
                                                             ifelse(UIBduration > 100, 
                                                                    ((100*uib_first_daily_rate_start)+(UIBduration - 100)*uib_second_daily_rate)/UIBduration, 
                                                                    uib_first_daily_rate_start),
                                                             ifelse(UIBduration > 100,
                                                                    ((100*uib_first_daily_rate_start)+(UIBduration - 100)*uib_second_daily_rate + red_ben)/UIBduration,
                                                                    uib_first_daily_rate_start + (red_ben/UIBduration))))
###Create max potential UI
dfinvestigate <- dfinvestigate %>% mutate(potentialUIB = ifelse(uib_days_appointed == 180, (100*uib_first_daily_rate_start + 80*uib_second_daily_rate)/180,
                                                                ifelse(uib_days_appointed == 270, (100*uib_first_daily_rate_start + 170*uib_second_daily_rate)/270,
                                                                       (100*uib_first_daily_rate_start + 260*uib_second_daily_rate)/360)))



###Create log of AvgUIB
dfinvestigate <- dfinvestigate %>% mutate(logUI = log(AvgUIB))
dfinvestigate <- dfinvestigate %>% mutate(logUIred = log(AvgUIBred))
dfinvestigate <- dfinvestigate %>% mutate(logPotUI = log(potentialUIB))


##### Deciding to not keep partial reform days
###Divide into years
dfin2017 <- dfinvestigate[dfinvestigate$uib_start_date >= "2017-01-01"
                          & dfinvestigate$uib_start_date < "2018-01-01", ]
dfin2017$year = 2017
dfin2018 <- dfinvestigate[dfinvestigate$uib_start_date >= "2018-01-01"
                          & dfinvestigate$uib_start_date < "2019-01-01", ]
dfin2018$year = 2018
dfin2019 <- dfinvestigate[dfinvestigate$uib_start_date >= "2019-01-01"
                          & dfinvestigate$uib_start_date < "2020-01-01", ]
dfin2019$year = 2019
dfin2020 <- dfinvestigate[dfinvestigate$uib_start_date >= "2020-01-01"
                          & dfinvestigate$uib_start_date < "2021-01-01", ]
dfin2020$year = 2020

#Giving the first 50 days of partial reform to the pre-reform stage
#dfin2020pre <- dfin2020[dfin2020$uib_start_date >= "2020-01-01"
#                             & dfin2020$uib_start_date < "2020-06-13", ]
#dfin2020post <- dfin2020[dfin2020$uib_start_date >= "2020-06-13"
#                          & dfin2020$uib_start_date < "2021-01-01", ]

#Exclude partial days
dfin2020pre <- dfin2020[dfin2020$uib_start_date >= "2020-01-01"
                        & dfin2020$uib_start_date < "2020-04-24", ]
dfin2020post <- dfin2020[dfin2020$uib_start_date >= "2020-08-01"
                         & dfin2020$uib_start_date < "2021-01-01", ]


dfin2021 <- dfinvestigate[dfinvestigate$uib_start_date >= "2021-01-01"
                          & dfinvestigate$uib_start_date < "2022-01-01", ]
dfin2021$year = 2021
dfin2022 <- dfinvestigate[dfinvestigate$uib_start_date >= "2022-01-01"
                          & dfinvestigate$uib_start_date < "2023-01-01", ]
dfin2022$year = 2022

##### Deflate by CPI
#####
## 100euros in 2017 = 103.4(2018), 105.8(2019), 105.3(2020), 110.2(2021), 131.6(2022)
dfin2017 <- dfin2017 %>% mutate(AvgUIBCPI = AvgUIB/1)
dfin2018 <- dfin2018 %>% mutate(AvgUIBCPI = AvgUIB/1.034)
dfin2019 <- dfin2019 %>% mutate(AvgUIBCPI = AvgUIB/1.058)
dfin2020pre <- dfin2020pre %>% mutate(AvgUIBCPI = AvgUIB/1.053)
dfin2020post <- dfin2020post %>% mutate(AvgUIBCPI = AvgUIB/1.053)
dfin2021 <- dfin2021 %>% mutate(AvgUIBCPI = AvgUIB/1.102)
dfin2022 <- dfin2022 %>% mutate(AvgUIBCPI = AvgUIB/1.316)

dfin2017 <- dfin2017 %>% mutate(logUICPI = log(AvgUIBCPI))
dfin2018 <- dfin2018 %>% mutate(logUICPI = log(AvgUIBCPI))
dfin2019 <- dfin2019 %>% mutate(logUICPI = log(AvgUIBCPI))
dfin2020pre <- dfin2020pre %>% mutate(logUICPI = log(AvgUIBCPI))
dfin2020post <- dfin2020post %>% mutate(logUICPI = log(AvgUIBCPI))
dfin2021 <- dfin2021 %>% mutate(logUICPI = log(AvgUIBCPI))
dfin2022 <- dfin2022 %>% mutate(logUICPI = log(AvgUIBCPI))

#####
#####

###Division of yearly data into Bottom and Top kinks centered around median wage
BK2017 <- filter(dfin2017, dfin2017$previousdailyearnings < median(dfin2017$previousdailyearnings))
TK2017 <- filter(dfin2017, dfin2017$previousdailyearnings >= median(dfin2017$previousdailyearnings))
BK2018 <- filter(dfin2018, dfin2018$previousdailyearnings < median(dfin2018$previousdailyearnings))
TK2018 <- filter(dfin2018, dfin2018$previousdailyearnings >= median(dfin2018$previousdailyearnings))
BK2019 <- filter(dfin2019, dfin2019$previousdailyearnings < median(dfin2019$previousdailyearnings))
TK2019 <- filter(dfin2019, dfin2019$previousdailyearnings >= median(dfin2019$previousdailyearnings))
## Only keep TK2018 obs starting in June 1 due to censoring
TK2018June <- TK2018 %>% filter(TK2018$uib_start_date >= "2018-06-01") #3,495 obs, lost 2,761

BK2020 <- filter(dfin2020, dfin2020$previousdailyearnings < median(dfin2020$previousdailyearnings))
TK2020 <- filter(dfin2020, dfin2020$previousdailyearnings >= median(dfin2020$previousdailyearnings))
BK2020pre <- filter(dfin2020pre, dfin2020pre$previousdailyearnings < median(dfin2020pre$previousdailyearnings))
TK2020pre <- filter(dfin2020pre, dfin2020pre$previousdailyearnings >= median(dfin2020pre$previousdailyearnings))
BK2020post <- filter(dfin2020post, dfin2020post$previousdailyearnings < median(dfin2020post$previousdailyearnings))
TK2020post <- filter(dfin2020post, dfin2020post$previousdailyearnings >= median(dfin2020post$previousdailyearnings))

BK2021 <- filter(dfin2021, dfin2021$previousdailyearnings < median(dfin2021$previousdailyearnings))
TK2021 <- filter(dfin2021, dfin2021$previousdailyearnings >= median(dfin2021$previousdailyearnings))
BK2022 <- filter(dfin2022, dfin2022$previousdailyearnings < median(dfin2022$previousdailyearnings))
TK2022 <- filter(dfin2022, dfin2022$previousdailyearnings >= median(dfin2022$previousdailyearnings))


#####Statistics

##Pre-reform     #Wage to meet Min Kink  #Wage to meet Max Kink  #Median Wage  #Observations  #Total
#2017             14.34 * 0.5 = 7.17      86.94 * 0.5 = 43.47     27.94        11,695         46,013
#2018             15.66 * 0.5 = 7.83      93.30 * 0.5 = 46.65     31.675       12,514
#2019             16.66 * 0.5 = 8.33      99.75 * 0.5 = 49.88     33.22        13,989
#2020             18    * 0.5 = 9        106.86 * 0.5 = 53.43     32.31         7,815

##Post-reform
#2020             18    * 0.5 = 9        106.86 * 0.6 = 64.12     35.525        9,652         29,514
#2021             19.46 * 0.5 = 9.73     112.17 * 0.6 = 67.3      35.29        14,663
#2022             19.46 * 0.5 = 9.73     119.49 * 0.6 = 71.69     38.45         5,199         75,527 Together

##After the 20202 reform, 60% of past wages given(first 100 days), so wages < min. wage are at the kink level
#2020             15     * 0.6 = 9
#2021             16.217 * 0.6 = 9.73
#2022             16.217 * 0.6 = 9.73


###Give Distance of Wage from Kink
#2017: lowest is 7.17   highest is 43.47
#2018: lowest is 7.83   highest is 46.65
#2019: lowest is 8.33   highest is 49.88
#2020: lowest is 9      highest is 64.12 POST reform    highest is 53.43 PRE reform
#2021: lowest is 9.73   highest is 67.3
#2022: lowest is 9.73   highest is 71.69

##
BK2017$Wagedist <- BK2017$previousdailyearnings - 14.34
TK2017$Wagedist <- TK2017$previousdailyearnings - TK2017$maxbaas
BK2018$Wagedist <- BK2018$previousdailyearnings - 15.66
TK2018$Wagedist <- TK2018$previousdailyearnings - TK2018$maxbaas
TK2018June$Wagedist <- TK2018June$previousdailyearnings - TK2018June$maxbaas
BK2019$Wagedist <- BK2019$previousdailyearnings - 16.66
TK2019$Wagedist <- TK2019$previousdailyearnings - TK2019$maxbaas


##Give -18 prior to reform for Bottom Kink, then - 15
BK2020 <- BK2020 %>% mutate(Wagedist = ifelse(uib_start_date < '2020-04-24' ,previousdailyearnings - 18, previousdailyearnings- 15))
TK2020$Wagedist <- TK2020$previousdailyearnings - TK2020$maxbaas

BK2020pre <- BK2020pre %>% mutate(Wagedist = ifelse(uib_start_date < '2020-04-24' ,previousdailyearnings - 18, previousdailyearnings- 15))
TK2020pre$Wagedist <- TK2020pre$previousdailyearnings - TK2020pre$maxbaas
BK2020post$Wagedist <- BK2020post$previousdailyearnings - 15
TK2020post$Wagedist <- TK2020post$previousdailyearnings - TK2020post$maxbaas

BK2021$Wagedist <- BK2021$previousdailyearnings - 16.217
TK2021$Wagedist <- TK2021$previousdailyearnings - TK2021$maxbaas
BK2022$Wagedist <- BK2022$previousdailyearnings - 16.217
TK2022$Wagedist <- TK2022$previousdailyearnings - TK2022$maxbaas

##### Deflate by CPI
#####
## 100euros in 2017 = 103.4(2018), 105.8(2019), 105.3(2020), 110.2(2021), 131.6(2022)
BK2017 <- BK2017 %>% mutate(WagedistCPI = Wagedist/1)
TK2017 <- TK2017 %>% mutate(WagedistCPI = Wagedist/1)
BK2018 <- BK2018 %>% mutate(WagedistCPI = Wagedist/1.034)
TK2018June <- TK2018June %>% mutate(WagedistCPI = Wagedist/1.034)
BK2019 <- BK2019 %>% mutate(WagedistCPI = Wagedist/1.058)
TK2019 <- TK2019 %>% mutate(WagedistCPI = Wagedist/1.058)
BK2020pre <- BK2020pre %>% mutate(WagedistCPI = Wagedist/1.053)
TK2020pre <- TK2020pre %>% mutate(WagedistCPI = Wagedist/1.053)

BK2020post <- BK2020post %>% mutate(WagedistCPI = Wagedist/1.053)
TK2020post <- TK2020post %>% mutate(WagedistCPI = Wagedist/1.053)
BK2021 <- BK2021 %>% mutate(WagedistCPI = Wagedist/1.102)
TK2021 <- TK2021 %>% mutate(WagedistCPI = Wagedist/1.102)
BK2022 <- BK2022 %>% mutate(WagedistCPI = Wagedist/1.316)
TK2022 <- TK2022 %>% mutate(WagedistCPI = Wagedist/1.316)

#####
#####



###Combine Data into Pre-Reform Bottom and Top kink sets(2) and Post-reform sets(2), total(4)
BKprereform <- bind_rows(BK2017, BK2018, BK2019, BK2020pre)
TKprereform <- bind_rows(TK2017, TK2018, TK2019, TK2020pre)
TKprereformNoCens <- bind_rows(TK2018June, TK2019, TK2020pre)

BKpostreform <- bind_rows(BK2020post, BK2021, BK2022)
TKpostreform <- bind_rows(TK2020post, TK2021, TK2022)

BKwhole <- bind_rows(BK2017, BK2018, BK2019, BK2020, BK2021, BK2022)
TKwhole <- bind_rows(TK2017, TK2018, TK2019, TK2020, TK2021, TK2022)



#####Create dummy variable for treatment status for rdrobust
BKprereform <- BKprereform %>% mutate(treatment = ifelse(Wagedist >= 0, 1, 0))
BKpostreform <- BKpostreform %>% mutate(treatment = ifelse(Wagedist >= 0, 1, 0))
TKprereformNoCens <- TKprereformNoCens %>% mutate(treatment = ifelse(Wagedist >= 0, 0, 1))
TKpostreform <- TKpostreform %>% mutate(treatment = ifelse(Wagedist >= 0, 0, 1))


###First stage (centered wages on AvgUI). Quick visual analysis for sharp vs fuzzy design.
ggplot(data=BKwhole, mapping = aes(x = Wagedist, y = AvgUIB, colour = year))+ 
  geom_point(alpha=0.25) +
  labs(x = "Daily Wage Relative to Floor", y = 'Average Daily UIB', title = 'Figure 1: Daily UIB Benefits Bottom Kink') +
  ylim(5, 25)


ggplot(data=TKwhole, mapping = aes(x = Wagedist, y = AvgUIB, colour = year))+ 
  geom_point(alpha=0.25) + xlim(NA, 200) +
  labs(x = "Daily Wage Relative to Ceiling", y = 'Average Daily UIB', title = 'Figure 2: Daily UIB Benefits Top Kink')




##### Distribution of Wagedist
#x=Wagedist, y= density
hist(BKprereform$Wagedist, freq=F, breaks=100) #spike at 0, minimum wage
hist(BKpostreform$Wagedist, freq=F, breaks=100) #spike a little after 0, minimum wage
hist(TKprereform$Wagedist, freq=F, breaks=200, xlim=c(-100,200))
hist(TKpostreform$Wagedist, freq=F, breaks=200, xlim=c(-100,200))

hist(TKprereformNoCens$Wagedist, freq=F, breaks=200, xlim=c(-100,200))



###Summary Statistics
#Average daily earnings, AvgUI benefit, unemployment duration, UI duration,
#max UI duration, fraction exhausted benefits, gender, average age, fraction higher education,
#percentages for industry
##                 BKprereform   BKpostreform  TKprereformNoCens    TKpostreform
#observations
#Wagedist          3.62(6.78)    7.23(7.49)    -38.93(33.81)  -47.51(33.55)   mean(BKprereform$Wagedist)
#Wage              19.65(6.88)   23.05(7.48)   61.18(33.51)    64.21(33.65)   mean(BKprereform$previousdailyearnings)
#AvgUI Ben.        10.12(2.33)   13.27(3.32)   26.95(9.89)    32.92(12.65)    mean(BKprereform$AvgUIB)
#Avg U dur.        184(177) days 182(137) days 219(179) days  196(138) days   mean(BKprereform$Uduration)
#Avg UI dur.       131(94) days  141(92) days  163(107) days  158(101) days   mean(BKprereform$UIBduration)
#% 180             54.71%        51.99%        33.40%         30.85%
#% 270             24.15%        25.58%        28.05%         28.45%
#% 360             21.14%        22.42%        38.54%         40.69%
count(BKprereform$uib_days_appointed)
count(BKpostreform$uib_days_appointed)
count(TKprereformNoCens$uib_days_appointed)
count(TKpostreform$uib_days_appointed)
#Avg max U d.      240(72) days  243(73) days  275(76) days   279(76) days    mean(BKprereform$uib_days_appointed)
#% Male            35.36%        36.52%        54.45%         52.75%          mean(BKprereform$gendermale)
#% Primary         16.65%        17.92%        7.62%          8.45%           count(BKprereform$education)
#% Secondary       53.96%        54.38%        37.83%         40.28%
#% Tertiary        28.57%        26.62%        52.76%         49.33%
#% Unknown         0.82%         1.08%         1.78%          1.93%
count(BKprereform$education)
count(BKpostreform$education)
count(TKprereformNoCens$education)
count(TKpostreform$education)
#% 15-30           26.13%        29.72%        17.73%         17.54%
#% 30-50           47.24%        45.47%        60.07%         60%
#% 50-65           26.63%        24.81%        22.20%         22.45%
count(BKprereform$age_group)
count(BKpostreform$age_group)
count(TKprereformNoCens$age_group)
count(TKpostreform$age_group)
#% Agriculture     2.4%          2.67%         1.38%          1.65%
#% Bus. Serv.      18.45%        17.47%        37.34%         36.93%
#% Construction    9.47%         9.51%         9.90%          10.28%
#% EduHealthSocPub 7.91%         7.56%         8.03%          9.16%
#% Industry        18.25%        14.05%        20.28%         14.92%
#% Pers. Serv.     19.87%        25.79%        8.89%          11.76%
#% Retail          15.72%        14.80%        6.38%          7.54%
#% Transport       4.08%         4.08%         5.09%          4.92%
#% Other           3.83%         4.08%         2.70%          2.81%
count(BKprereform$previousemploymentfield)
count(BKpostreform$previousemploymentfield)
count(TKprereformNoCens$previousemploymentfield)
count(TKpostreform$previousemploymentfield)
#% Exhausted Ben.  27.25%        27.75%        32.09%         26.21% (TKpostreform censored, would be higher)
BKprereform %>% mutate(Exhaust = ifelse(uib_end_date == uib_end_date_actual, 1, 0)) %>% summarize(mean = mean(Exhaust))
BKpostreform %>% mutate(Exhaust = ifelse(uib_end_date == uib_end_date_actual, 1, 0)) %>% summarize(mean = mean(Exhaust))
TKprereformNoCens %>% mutate(Exhaust = ifelse(uib_end_date == uib_end_date_actual, 1, 0)) %>% summarize(mean = mean(Exhaust))
TKpostreform %>% mutate(Exhaust = ifelse(uib_end_date == uib_end_date_actual, 1, 0)) %>% summarize(mean = mean(Exhaust))


##### McCrary density test with rddensity
rdplotdensity(rdd = rddensity(BKprereform$Wagedist, c = 0),
              X = BKprereform$Wagedist, type = "both", 
              xlabel = "Daily Wage Relative to Floor", ylabel = "Frequency", title = "Figure A1: Wage Density for Bottom Kink Prereform")
summary(rddensity(BKprereform$Wagedist, c=0, massPoints=F))

rdplotdensity(rdd = rddensity(TKprereformNoCens$Wagedist, c = 0),
              X = TKprereformNoCens$Wagedist, type = "both",
              xlabel = "Daily Wage Relative to Ceiling", ylabel = "Frequency", title = "Figure A4: Wage Density for TK Prereform (June 2018-2020)")
summary(rddensity(TKprereformNoCens$Wagedist,c=0, massPoints=F))
rdplotdensity(rdd = rddensity(TKprereform$Wagedist, c = 0),
              X = TKprereform$Wagedist, type = "both",
              xlabel = "Daily Wage Relative to Ceiling", ylabel = "Frequency", title = "Figure A3: Wage Density for Top Kink Prereform (2017-2020)")
summary(rddensity(TKprereform$Wagedist,c=0, massPoints=F))

rdplotdensity(rdd = rddensity(BKpostreform$Wagedist, c = 0),
              X = BKpostreform$Wagedist, type = "both",
              xlabel = "Daily Wage Relative to Floor", ylabel = "Frequency", title = "Figure A2: Wage Density for Bottom Kink Postreform")
summary(rddensity(BKpostreform$Wagedist, c=0, massPoints=F))

rdplotdensity(rdd = rddensity(TKpostreform$Wagedist, c = 0),
              X = TKpostreform$Wagedist, type = "both",
              xlabel = "Daily Wage Relative to Ceiling", ylabel = "Frequency", title = "Figure A5: Wage Density for Top Kink Postreform")
summary(rddensity(TKpostreform$Wagedist, c=0, massPoints=F))


summary(rddensity(BKprereform$Wagedist, massPoints=F)) #Low p-value, significant jump
summary(rddensity(TKprereform$Wagedist, massPoints=F)) #Low p-value, significant jump
summary(rddensity(TKprereformNoCens$Wagedist, massPoints=F))

summary(rddensity(BKpostreform$Wagedist, massPoints=F)) #High p-value, no stat. sign. jump
summary(rddensity(TKpostreform$Wagedist, massPoints=F)) #High p-value, no stat. sign. jump


##### Covariate Smoothness Analysis

### Gender Covariate Test
rdplot(BKprereform$gendermale, BKprereform$Wagedist, c=0,p=2,
       y.label="% Male", x.label = "Daily Wage Relative to Floor", title = "Figure A6: Distribution of Gender, Bottom Kink Prereform")
rdplot(TKprereformNoCens$gendermale, TKprereformNoCens$Wagedist, c=0,p=2,x.lim=c(NA, 200),
       y.label="% Male", x.label = "Daily Wage Relative to Ceiling", title = "Figure A7: Distribution of Gender, Top Kink Prereform")
rdplot(BKpostreform$gendermale, BKpostreform$Wagedist, c=0,p=2,
       y.label="% Male", x.label = "Daily Wage Relative to Floor", title = "Figure A8: Distribution of Gender, Bottom Kink Postreform")
rdplot(TKpostreform$gendermale, TKpostreform$Wagedist, c=0,p=2,x.lim=c(NA, 200),
       y.label="% Male", x.label = "Daily Wage Relative to Ceiling", title = "Figure A9: Distribution of Gender, Top Kink Postreform")

### Blue collar Covariate Test (agriculture, construction, industry)
BKprereform <- BKprereform %>% mutate(Blue = ifelse(previousemploymentfield == "Agriculture", 1, 
                                                    ifelse(previousemploymentfield == "Construction", 1, 
                                                           ifelse(previousemploymentfield == "Industry", 1, 0))))
BKpostreform <- BKpostreform %>% mutate(Blue = ifelse(previousemploymentfield == "Agriculture", 1, 
                                                      ifelse(previousemploymentfield == "Construction", 1, 
                                                             ifelse(previousemploymentfield == "Industry", 1, 0))))
TKprereformNoCens <- TKprereformNoCens %>% mutate(Blue = ifelse(previousemploymentfield == "Agriculture", 1, 
                                                            ifelse(previousemploymentfield == "Construction", 1, 
                                                                   ifelse(previousemploymentfield == "Industry", 1, 0))))
TKpostreform <- TKpostreform %>% mutate(Blue = ifelse(previousemploymentfield == "Agriculture", 1, 
                                                      ifelse(previousemploymentfield == "Construction", 1, 
                                                             ifelse(previousemploymentfield == "Industry", 1, 0))))

rdplot(BKprereform$Blue, BKprereform$Wagedist, c=0,p=2,
       y.label="% Bluecollar", x.label = "Daily Wage Relative to Floor", title = "Figure A10: % of Bluecollar, Bottom Kink Prereform")
rdplot(TKprereformNoCens$Blue, TKprereformNoCens$Wagedist, c=0,p=2,x.lim=c(NA, 200),
       y.label="% Bluecollar", x.label = "Daily Wage Relative to Ceiling", title = "Figure A11: % of Bluecollar, Top Kink Prereform")
rdplot(BKpostreform$Blue, BKpostreform$Wagedist, c=0,p=2,
       y.label="% Bluecollar", x.label = "Daily Wage Relative to Floor", title = "Figure A12: % of Bluecollar, Bottom Kink Postreform")
rdplot(TKpostreform$Blue, TKpostreform$Wagedist, c=0,p=2, x.lim=c(NA, 200),
       y.label="% Bluecollar", x.label = "Daily Wage Relative to Ceiling", title = "Figure A13: % of Bluecollar, Top Kink Postreform")

### Age Covariate Test
BKprereform <- BKprereform %>% mutate(age = ifelse(age_group == "[15,30)", 22.5, 
                                                   ifelse(age_group == "[30,50)", 40, 
                                                          ifelse(age_group == "[50,65]", 56.5, 0))))
BKpostreform <- BKpostreform %>% mutate(age = ifelse(age_group == "[15,30)", 22.5, 
                                                     ifelse(age_group == "[30,50)", 40, 
                                                            ifelse(age_group == "[50,65]", 56.5, 0))))
TKprereformNoCens <- TKprereformNoCens %>% mutate(age = ifelse(age_group == "[15,30)", 22.5, 
                                                           ifelse(age_group == "[30,50)", 40, 
                                                                  ifelse(age_group == "[50,65]", 56.5, 0))))
TKpostreform <- TKpostreform %>% mutate(age = ifelse(age_group == "[15,30)", 22.5, 
                                                     ifelse(age_group == "[30,50)", 40, 
                                                            ifelse(age_group == "[50,65]", 56.5, 0))))
rdplot(BKprereform$age, BKprereform$Wagedist,p=2,
       y.label = "Age", x.label = "Daily Wage Relative to Floor", 
       title = "Figure A18: Avg Age, Prereform Bottom Kink")
rdplot(TKprereformNoCens$age, TKprereformNoCens$Wagedist,p=2,x.lim=c(NA, 200),
       y.label = "Age", x.label = "Daily Wage Relative to Ceiling", 
       title = "Figure A19: Avg Age, Prereform Top Kink")
rdplot(BKpostreform$age, BKpostreform$Wagedist,p=2,
       y.label = "Age", x.label = "Daily Wage Relative to Floor", 
       title = "Figure A20: Avg Age, Postreform Bottom Kink")
rdplot(TKpostreform$age, TKpostreform$Wagedist,p=2,x.lim=c(NA, 200),
       y.label = "Age", x.label = "Daily Wage Relative to Ceiling", 
       title = "Figure A21: Avg Age, Postreform Top Kink")

### Education Covariate test (primary = 9 years, secondary = 3 years, tertiary = 2 years)
BKprereform <- BKprereform %>% mutate(Edu = ifelse(education == "Primary level", 9, 
                                                   ifelse(education == "Secondary level", 12, 
                                                          ifelse(education == "Tertiary level", 15, 0))))
BKprereformEdu <- BKprereform %>% filter(!education == "Unknown")
TKprereformNoCens <- TKprereformNoCens %>% mutate(Edu = ifelse(education == "Primary level", 9, 
                                                           ifelse(education == "Secondary level", 12, 
                                                                  ifelse(education == "Tertiary level", 15, 0))))
TKprereformEdu <- TKprereformNoCens %>% filter(!education == "Unknown")
BKpostreform <- BKpostreform %>% mutate(Edu = ifelse(education == "Primary level", 9, 
                                                     ifelse(education == "Secondary level", 12, 
                                                            ifelse(education == "Tertiary level", 15, 0))))
BKpostreformEdu <- BKpostreform %>% filter(!education == "Unknown")
TKpostreform <- TKpostreform %>% mutate(Edu = ifelse(education == "Primary level", 9, 
                                                     ifelse(education == "Secondary level", 12, 
                                                            ifelse(education == "Tertiary level", 15, 0))))
TKpostreformEdu <- TKpostreform %>% filter(!education == "Unknown")

rdplot(BKprereformEdu$Edu, BKprereformEdu$Wagedist, c=0,
       y.label="Years of education", x.label = "Daily Wage Relative to Floor", title = "Figure A14: Avg Education Length, Bottom Kink Prereform")
rdplot(TKprereformEdu$Edu, TKprereformEdu$Wagedist, c=0,x.lim=c(NA, 200),
       y.label="Years of education", x.label = "Daily Wage Relative to Ceiling", title = "Figure A15: Avg Education Length, Top Kink Prereform")
rdplot(BKpostreformEdu$Edu, BKpostreformEdu$Wagedist, c=0, 
       y.label="Years of education", x.label = "Daily Wage Relative to Floor", title = "Figure A16: Avg Education Length, Bottom Kink Postreform")
rdplot(TKpostreformEdu$Edu, TKpostreformEdu$Wagedist, c=0,x.lim=c(NA, 200),
       y.label="Years of education", x.label = "Daily Wage Relative to Ceiling", title = "Figure A17: Avg Education Length, Top Kink Postreform")


### Max UI Duration Covariate
rdplot(BKprereform$uib_days_appointed, BKprereform$Wagedist,
       y.label = "Maximum days of UI", x.label = "Daily Wage Relative to Floor", 
       title = "Figure A22: Maximum UI Days Prereform Bottom Kink")
rdplot(TKprereformNoCens$uib_days_appointed, TKprereformNoCens$Wagedist, c=0,x.lim=c(NA, 200),
       y.label = "Maximum days of UI", x.label = "Daily Wage Relative to Ceiling", 
       title = "Figure A23: Maximum UI Days Prereform Top Kink")
rdplot(BKpostreform$uib_days_appointed, BKpostreform$Wagedist,
       y.label = "Maximum days of UI", x.label = "Daily Wage Relative to Floor", 
       title = "Figure A24: Maximum UI Days Postreform Bottom Kink")
rdplot(TKpostreform$uib_days_appointed, TKpostreform$Wagedist, c=0,x.lim=c(NA, 200),
       y.label = "Maximum days of UI", x.label = "Daily Wage Relative to Ceiling", 
       title = "Figure A25: Maximum UI Days Postreform Top Kink")



##### rdplot kink graphs

rdplot(BKprereform$AvgUIB, BKprereform$Wagedist,p=2,
       y.label = "Average UI benefits", x.label = "Daily Wage Relative to Floor", 
       title = "Figure 3: Daily UIB Benefits Prereform Bottom Kink")
rdplot(BKprereform$logduration, BKprereform$Wagedist,p=2,
       y.label = "Log Unemployment Duration", x.label = "Daily Wage Relative to Floor", 
       title = "Figure 7: Length of Unemployment Prereform Bottom Kink")

rdplot(TKprereformNoCens$AvgUIB, TKprereformNoCens$Wagedist,p=2,x.lim=c(NA, 200),
       y.label = "Average UI benefits", x.label = "Daily Wage Relative to Ceiling", 
       title = "Figure 5: Daily UIB Benefits Prereform Top Kink")
rdplot(TKprereformNoCens$logduration, TKprereformNoCens$Wagedist,p=2,x.lim=c(NA, 200),
       y.label = "Log Unemployment Duration", x.label = "Daily Wage Relative to Ceiling", 
       title = "Figure 9: Length of Unemployment Prereform Top Kink")

rdplot(BKpostreform$AvgUIB, BKpostreform$Wagedist,p=2,
       y.label = "Average UI benefits", x.label = "Daily Wage Relative to Floor", 
       title = "Figure 4: Daily UIB Benefits Postreform Bottom Kink")
rdplot(BKpostreform$logduration, BKpostreform$Wagedist,p=2,
       y.label = "Log Unemployment Duration", x.label = "Daily Wage Relative to Floor", 
       title = "Figure 8: Length of Unemployment Postreform Bottom Kink")

rdplot(TKpostreform$AvgUIB, TKpostreform$Wagedist,p=2,x.lim=c(NA, 200),
       y.label = "Average UI benefits", x.label = "Daily Wage Relative to Ceiling", 
       title = "Figure 6: Daily UIB Benefits Postreform Top Kink")
rdplot(TKpostreform$logduration, TKpostreform$Wagedist,p=2,x.lim=c(NA, 200),
       y.label = "Log Unemployment Duration", x.label = "Daily Wage Relative to Floor", 
       title = "Figure 10: Length of Unemployment Postreform Top Kink")



##### Kink Estimates

###rdrobust with IK
rdrobust(BKprereform$logUI, BKprereform$Wagedist, c = 0, deriv=TRUE, p = 1, 
         kernel = "uniform", all = TRUE, bwselect = "IK")
rdrobust(BKprereform$logduration, BKprereform$Wagedist, c = 0, deriv=TRUE, p=2,q=0, 
         kernel = "uniform", all = TRUE, bwselect = "IK") 
rdrobust(BKprereform$logUI, BKprereform$Wagedist, c = 0, deriv=TRUE, p = 2,q=0, 
         kernel = "uniform", all = TRUE, bwselect = "CCT")
rdrobust(BKprereform$logduration, BKprereform$Wagedist, c = 0, deriv=TRUE, p=1, 
         kernel = "uniform", all = TRUE, bwselect = "CCT") #2.1598 linear bw, 5.3014 quad bw

rdrobust(TKprereformNoCens$logUI, TKprereformNoCens$Wagedist, c = 0, deriv=TRUE, p=2,q=0,
         kernel = "uniform", all = TRUE, bwselect = "CCT") 
rdrobust(TKprereformNoCens$logduration, TKprereformNoCens$Wagedist, c = 0, deriv=TRUE, p=2,q=0,
         kernel = "uniform", all = TRUE, bwselect = "CCT") #14.7281 linear bw, 26.8652 quad bw

rdrobust(BKpostreform$logUI, BKpostreform$Wagedist, c = 0, deriv=TRUE, p=2,q=0,
         kernel = "uniform", all = TRUE, bwselect = "CCT") 
rdrobust(BKpostreform$logduration, BKpostreform$Wagedist, c = 0, deriv=TRUE, p=1,
         kernel = "uniform", all = TRUE, bwselect = "CCT") #2.3964 linear bw, 3.7161 quad bw

rdrobust(TKpostreform$logUI, TKpostreform$Wagedist, c = 0, deriv=TRUE, p=2,q=0,
         kernel = "uniform", all = TRUE, bwselect = "CCT") 
rdrobust(TKpostreform$logduration, TKpostreform$Wagedist, c = 0, deriv=TRUE, p=1,
         kernel = "uniform", all = TRUE, bwselect = "CCT") #14.492 linear bw, 19.2076 quad bw

### rdrobust version without IK
##Prereform
#summary(rdrobust(BKprereform$logUI, BKprereform$Wagedist, c = 0, deriv=TRUE, p = 1, 
#                 kernel = "uniform", masspoints = "off", all = TRUE)) #1.274
#summary(rdrobust(BKprereform$logduration, BKprereform$Wagedist, c = 0, deriv=TRUE, p = 2, 
#                 kernel = "uniform", masspoints = "off", all = TRUE)) #2.287 linear bw, 3.737 quad bw
#
#
#summary(rdrobust(TKprereformNoCens$logUI, TKprereformNoCens$Wagedist, c = 0, deriv=TRUE, p = 1, 
#                 kernel = "uniform", masspoints = "off", all = TRUE)) #7.94
##summary(rdrobust(TKprereformNoCens$logduration, TKprereformNoCens$Wagedist, c = 0, deriv=TRUE, p = 1, 
#                 kernel = "uniform", masspoints = "off", all = TRUE)) #17.428 linear bw, 26.111 quad bw
#
#
##Postreform
#summary(rdrobust(BKpostreform$logUI, BKpostreform$Wagedist, c = 0, deriv=TRUE, p = 2, 
#                 kernel = "uniform", masspoints = "off", all = TRUE))
#summary(rdrobust(BKpostreform$logduration, BKpostreform$Wagedist, c = 0, deriv=TRUE, p = 1, 
#                 kernel = "uniform", masspoints = "off", all = TRUE)) #2.32 linear bw, 3.694 quad bw
#
#
#summary(rdrobust(TKpostreform$logUI, TKpostreform$Wagedist, c = 0, deriv=TRUE, p = 2, 
#                 kernel = "uniform", masspoints = "off", all = TRUE))
#summary(rdrobust(TKpostreform$logduration, TKpostreform$Wagedist, c = 0, deriv=TRUE, p = 2, 
#                 kernel = "uniform", masspoints = "off", all = TRUE)) #15.48 linear bw, 19.217 quad bw


## Not Used ##
##### For Elasticity and Kink graphs with only relevant obs
##With chosen bandwidth from Log duration. Same bw for both for elasticity.
# Linear bw
#BKprereformFirst <- BKprereform %>% filter(between(Wagedist, -2.287, 2.287))
#rdplot(BKprereformFirst$AvgUIB, BKprereformFirst$Wagedist, p=1, 
#       x.label= "Daily Wage Relative to Floor", y.label = "Average UIB Benefits", title = "Relevant Daily UIB Benefits Prereform Bottom Kink")
#BKprereformReduced <- BKprereform %>% filter(between(Wagedist, -2.287, 2.287))
#rdplot(BKprereformReduced$logduration, BKprereformReduced$Wagedist, p=1,
#       x.label = "Daily Wage Relative to Floor", y.label = "Log Unemployment Duration", title = "Relevant Length of Unemployment Prereform Bottom Kink")
#
#TKprereformFirst <- TKprereformNoCens %>% filter(between(Wagedist, -17.428, 17.428))
#rdplot(TKprereformFirst$AvgUIB, TKprereformFirst$Wagedist, p=2,
#       x.label = "Daily Wage Relative to Ceiling", y.label = "Average UIB Benefits", title = "Relevant Daily UIB Benefits Prereform Top Kink")
#TKprereformReduced <- TKprereformNoCens %>% filter(between(Wagedist, -17.428, 17.428))
#rdplot(TKprereformReduced$logduration, TKprereformReduced$Wagedist, p=2,
#       x.label = "Daily Wage Relative to Ceiling", y.label = "Log Unemployment Duration", title = "Relevant Length of Unemployment Prereform Top Kink")
#
#
#BKpostreformFirst <- BKpostreform %>% filter(between(Wagedist, -2.32, 2.32))
#rdplot(BKpostreformFirst$AvgUIB, BKpostreformFirst$Wagedist, p=2, 
#       x.label= "Daily Wage Relative to Floor", y.label = "Average UIB Benefits", title = "Relevant Daily UIB Benefits Postreform Bottom Kink")
#BKpostreformReduced <- BKpostreform %>% filter(between(Wagedist, -2.32, 2.32))
#rdplot(BKpostreformReduced$logduration, BKpostreformReduced$Wagedist, p=2,
#       x.label = "Daily Wage Relative to Floor", y.label = "Log Unemployment Duration", title = "Relevant Length of Unemployment Postreform Bottom Kink")
#
#TKpostreformFirst <- TKpostreform %>% filter(between(Wagedist, -15.48, 15.48))
#rdplot(TKpostreformFirst$AvgUIB, TKpostreformFirst$Wagedist, p=2,
#       x.label = "Daily Wage Relative to Ceiling", y.label = "Average UIB Benefits", title = "Relevant Daily UIB Benefits Postreform Top Kink")
#TKpostreformReduced <- TKpostreform %>% filter(between(Wagedist, -15.48, 15.48))
#rdplot(TKpostreformReduced$logduration, TKpostreformReduced$Wagedist, p=2,
#       x.label = "Daily Wage Relative to Ceiling", y.label = "Log Unemployment Duration", title = "Relevant Length of Unemployment Postreform Top Kink")



##### Elasticity
## Set bandwidth limits to data
##Linear
BKprereformFirst <- BKprereform %>% filter(between(Wagedist, -2.1598, 2.1598))
TKprereformFirst <- TKprereformNoCens %>% filter(between(Wagedist, -14.7281 , 14.7281 ))
BKpostreformFirst <- BKpostreform %>% filter(between(Wagedist, -2.3964, 2.3964))
TKpostreformFirst <- TKpostreform %>% filter(between(Wagedist, -14.492, 14.492))

##Main test
ivBKpre <- ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = BKprereformFirst)
ivTKpre <- ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = TKprereformFirst)
ivBKpost <- ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = BKpostreformFirst)
ivTKpost <- ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = TKpostreformFirst)
#white errors
round(tail(coeftest(ivBKpre, vcov=vcovHC(ivBKpre, type="HC0"))[,4], 1),3)
round(tail(coeftest(ivTKpre, vcov=vcovHC(ivTKpre, type="HC0"))[,4], 1),3)
round(tail(coeftest(ivBKpost, vcov=vcovHC(ivBKpost, type="HC0"))[,4], 1),3)
round(tail(coeftest(ivTKpost, vcov=vcovHC(ivTKpost, type="HC0"))[,4], 1),3)

summary(ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = BKprereformFirst))
summary(ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = TKprereformFirst))
summary(ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = BKpostreformFirst))
summary(ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = TKpostreformFirst))


#tail(summary(ivTKpre)$coefficient[,1],1)
#tail(summary(ivTKpre)$coefficient[,1],1) + 1.96*tail(coeftest(ivTKpre, vcov=vcovHC(ivTKpre, type="HC0"))[,2], 1)
#tail(summary(ivTKpre)$coefficient[,1],1) - 1.96*tail(coeftest(ivTKpre, vcov=vcovHC(ivTKpre, type="HC0"))[,2], 1)







#####Redundancy test
#####
ivBKprered <- ivreg(logduration ~ Wagedist + logUIred | Wagedist + Wagedist*treatment, data = BKprereformFirst)
ivTKprered <- ivreg(logduration ~ Wagedist + logUIred | Wagedist + Wagedist*treatment, data = TKprereformFirst)
ivBKpostred <- ivreg(logduration ~ Wagedist + logUIred | Wagedist + Wagedist*treatment, data = BKpostreformFirst)
ivTKpostred <- ivreg(logduration ~ Wagedist + logUIred | Wagedist + Wagedist*treatment, data = TKpostreformFirst)

summary(ivreg(logduration ~ Wagedist + logUIred | Wagedist + Wagedist*treatment, data = BKprereformFirst))
summary(ivreg(logduration ~ Wagedist + logUIred | Wagedist + Wagedist*treatment, data = TKprereformFirst))
summary(ivreg(logduration ~ Wagedist + logUIred | Wagedist + Wagedist*treatment, data = BKpostreformFirst))
summary(ivreg(logduration ~ Wagedist + logUIred | Wagedist + Wagedist*treatment, data = TKpostreformFirst))

round(tail(coeftest(ivBKprered, vcov=vcovHC(ivBKprered, type="HC0"))[,4], 1),3)
round(tail(coeftest(ivTKprered, vcov=vcovHC(ivTKprered, type="HC0"))[,4], 1),3)
round(tail(coeftest(ivBKpostred, vcov=vcovHC(ivBKpostred, type="HC0"))[,4], 1),3)
round(tail(coeftest(ivTKpostred, vcov=vcovHC(ivTKpostred, type="HC0"))[,4], 1),3)
#####
#####


##### Looking at the effect of duration on the results
#####
## Divide existing datasets by duration
BKprereform180 <- BKprereform %>% filter(BKprereform$uib_days_appointed == 180)
BKprereform270 <- BKprereform %>% filter(BKprereform$uib_days_appointed == 270)
BKprereform360 <- BKprereform %>% filter(BKprereform$uib_days_appointed == 360)

TKprereformNoCens180 <- TKprereformNoCens %>% filter(TKprereformNoCens$uib_days_appointed == 180)
TKprereformNoCens270 <- TKprereformNoCens %>% filter(TKprereformNoCens$uib_days_appointed == 270)
TKprereformNoCens360 <- TKprereformNoCens %>% filter(TKprereformNoCens$uib_days_appointed == 360)

BKpostreform180 <- BKpostreform %>% filter(BKpostreform$uib_days_appointed == 180)
BKpostreform270 <- BKpostreform %>% filter(BKpostreform$uib_days_appointed == 270)
BKpostreform360 <- BKpostreform %>% filter(BKpostreform$uib_days_appointed == 360)

TKpostreform180 <- TKpostreform %>% filter(TKpostreform$uib_days_appointed == 180)
TKpostreform270 <- TKpostreform %>% filter(TKpostreform$uib_days_appointed == 270)
TKpostreform360 <- TKpostreform %>% filter(TKpostreform$uib_days_appointed == 360)

## CCT Kink estimates for optimal bandwidth
summary(rdrobust(BKprereform180$logUI, BKprereform180$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) 
summary(rdrobust(BKprereform180$logduration, BKprereform180$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) # 2.485 linear bw
summary(rdrobust(BKprereform270$logUI, BKprereform270$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) 
summary(rdrobust(BKprereform270$logduration, BKprereform270$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) # 3.307 linear bw
summary(rdrobust(BKprereform360$logUI, BKprereform360$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) 
summary(rdrobust(BKprereform360$logduration, BKprereform360$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) # 2.427 linear bw

summary(rdrobust(TKprereformNoCens180$logUI, TKprereformNoCens180$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) 
summary(rdrobust(TKprereformNoCens180$logduration, TKprereformNoCens180$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) # 16.468 linear bw
summary(rdrobust(TKprereformNoCens270$logUI, TKprereformNoCens270$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) 
summary(rdrobust(TKprereformNoCens270$logduration, TKprereformNoCens270$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) # 13.660 linear bw
summary(rdrobust(TKprereformNoCens360$logUI, TKprereformNoCens360$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) 
summary(rdrobust(TKprereformNoCens360$logduration, TKprereformNoCens360$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) #19.979 linear bw

summary(rdrobust(BKpostreform180$logUI, BKpostreform180$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) 
summary(rdrobust(BKpostreform180$logduration, BKpostreform180$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) # 3.094 linear bw
summary(rdrobust(BKpostreform270$logUI, BKpostreform270$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) 
summary(rdrobust(BKpostreform270$logduration, BKpostreform270$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) # 2.209 linear bw
summary(rdrobust(BKpostreform360$logUI, BKpostreform360$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) 
summary(rdrobust(BKpostreform360$logduration, BKpostreform360$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) # 2.551 linear bw

summary(rdrobust(TKpostreform180$logUI,TKpostreform180$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) 
summary(rdrobust(TKpostreform180$logduration, TKpostreform180$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) # 15.408 linear bw
summary(rdrobust(TKpostreform270$logUI,TKpostreform270$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) 
summary(rdrobust(TKpostreform270$logduration, TKpostreform270$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) # 16.391 linear bw
summary(rdrobust(TKpostreform360$logUI,TKpostreform360$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) 
summary(rdrobust(TKpostreform360$logduration, TKpostreform360$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) # 16.738 linear bw

## Bandwidth
BKprereform180First <- BKprereform180 %>% filter(between(Wagedist, -2.485, 2.485))
BKprereform270First <- BKprereform270 %>% filter(between(Wagedist, -3.307, 3.307))
BKprereform360First <- BKprereform360 %>% filter(between(Wagedist, -2.427, 2.427))

TKprereformNoCens180First <- TKprereformNoCens180 %>% filter(between(Wagedist, -16.468, 16.468))
TKprereformNoCens270First <- TKprereformNoCens270 %>% filter(between(Wagedist, -13.660, 13.660))
TKprereformNoCens360First <- TKprereformNoCens360 %>% filter(between(Wagedist, -19.979, 19.979))

BKpostreform180First <- BKpostreform180 %>% filter(between(Wagedist, -3.094, 3.094))
BKpostreform270First <- BKpostreform270 %>% filter(between(Wagedist, -2.209, 2.209))
BKpostreform360First <- BKpostreform360 %>% filter(between(Wagedist, -2.551, 2.551))

TKpostreform180First <- TKpostreform180 %>% filter(between(Wagedist, -15.408, 15.408))
TKpostreform270First <- TKpostreform270 %>% filter(between(Wagedist, -16.391, 16.391))
TKpostreform360First <- TKpostreform360 %>% filter(between(Wagedist, -16.738, 16.738))

## Elasticity estimate
ivBKpre180 <- ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = BKprereform180First)
ivBKpre270 <- ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = BKprereform270First)
ivBKpre360 <- ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = BKprereform360First)

ivTKpre180 <- ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = TKprereformNoCens180First)
ivTKpre270 <- ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = TKprereformNoCens270First)
ivTKpre360 <- ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = TKprereformNoCens360First)

ivBKpost180 <- ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = BKpostreform180First)
ivBKpost270 <- ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = BKpostreform270First)
ivBKpost360 <- ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = BKpostreform360First)

ivTKpost180 <- ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = TKpostreform180First)
ivTKpost270 <- ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = TKpostreform270First)
ivTKpost360 <- ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = TKpostreform360First)

#white errors
round(tail(coeftest(ivBKpre180, vcov=vcovHC(ivBKpre180, type="HC0"))[,4], 1),3)
round(tail(coeftest(ivBKpre270, vcov=vcovHC(ivBKpre270, type="HC0"))[,4], 1),3)
round(tail(coeftest(ivBKpre360, vcov=vcovHC(ivBKpre360, type="HC0"))[,4], 1),3)

round(tail(coeftest(ivTKpre180, vcov=vcovHC(ivTKpre180, type="HC0"))[,4], 1),3)
round(tail(coeftest(ivTKpre270, vcov=vcovHC(ivTKpre270, type="HC0"))[,4], 1),3)
round(tail(coeftest(ivTKpre360, vcov=vcovHC(ivTKpre360, type="HC0"))[,4], 1),3)

round(tail(coeftest(ivBKpost180, vcov=vcovHC(ivBKpost180, type="HC0"))[,4], 1),3)
round(tail(coeftest(ivBKpost270, vcov=vcovHC(ivBKpost270, type="HC0"))[,4], 1),3)
round(tail(coeftest(ivBKpost360, vcov=vcovHC(ivBKpost360, type="HC0"))[,4], 1),3)

round(tail(coeftest(ivTKpost180, vcov=vcovHC(ivTKpost180, type="HC0"))[,4], 1),3)
round(tail(coeftest(ivTKpost270, vcov=vcovHC(ivTKpost270, type="HC0"))[,4], 1),3)
round(tail(coeftest(ivTKpost360, vcov=vcovHC(ivTKpost360, type="HC0"))[,4], 1),3)

summary(ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = BKprereform180First))
summary(ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = BKprereform270First))
summary(ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = BKprereform360First))

summary(ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = TKprereformNoCens180First))
summary(ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = TKprereformNoCens270First))
summary(ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = TKprereformNoCens360First))

summary(ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = BKpostreform180First))
summary(ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = BKpostreform270First))
summary(ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = BKpostreform360First))

summary(ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = TKpostreform180First))
summary(ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = TKpostreform270First))
summary(ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = TKpostreform360First))
#####
#####


##### Creating different estimate of UI: max mean potential UI.
#####
## Kink estimates for bandwidth
#Prereform
summary(rdrobust(BKprereform$logPotUI, BKprereform$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) 
summary(rdrobust(BKprereform$logduration, BKprereform$Wagedist, c = 0, deriv=TRUE, p = 2, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) #2.287 linear bw, 3.737 quad bw
summary(rdrobust(TKprereformNoCens$logPotUI, TKprereformNoCens$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) 
summary(rdrobust(TKprereformNoCens$logduration, TKprereformNoCens$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) #17.428 linear bw, 26.111 quad bw

#Postreform
summary(rdrobust(BKpostreform$logPotUI, BKpostreform$Wagedist, c = 0, deriv=TRUE, p = 2, 
                 kernel = "uniform", masspoints = "off", all = TRUE))
summary(rdrobust(BKpostreform$logduration, BKpostreform$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) #2.32 linear bw, 3.694 quad bw
summary(rdrobust(TKpostreform$logPotUI, TKpostreform$Wagedist, c = 0, deriv=TRUE, p = 2, 
                 kernel = "uniform", masspoints = "off", all = TRUE))
summary(rdrobust(TKpostreform$logduration, TKpostreform$Wagedist, c = 0, deriv=TRUE, p = 2, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) #15.48 linear bw, 19.217 quad bw

## Bandwidth
BKprereformPotFirst <- BKprereform %>% filter(between(Wagedist, -2.287, 2.287))
TKprereformPotFirst <- TKprereformNoCens %>% filter(between(Wagedist, -17.428, 17.428))
BKpostreformPotFirst <- BKpostreform %>% filter(between(Wagedist, -2.32, 2.32))
TKpostreformPotFirst <- TKpostreform %>% filter(between(Wagedist, -15.48, 15.48))

## Elasticity estimate
ivBKprePot <- ivreg(logduration ~ Wagedist + logPotUI | Wagedist + Wagedist*treatment, data = BKprereformPotFirst)
ivTKprePot <- ivreg(logduration ~ Wagedist + logPotUI | Wagedist + Wagedist*treatment, data = TKprereformPotFirst)
ivBKpostPot <- ivreg(logduration ~ Wagedist + logPotUI | Wagedist + Wagedist*treatment, data = BKpostreformPotFirst)
ivTKpostPot <- ivreg(logduration ~ Wagedist + logPotUI | Wagedist + Wagedist*treatment, data = TKpostreformPotFirst)

#white errors
round(tail(coeftest(ivBKprePot, vcov=vcovHC(ivBKprePot, type="HC0"))[,4], 1),3)
round(tail(coeftest(ivTKprePot, vcov=vcovHC(ivTKprePot, type="HC0"))[,4], 1),3)
round(tail(coeftest(ivBKpostPot, vcov=vcovHC(ivBKpostPot, type="HC0"))[,4], 1),3)
round(tail(coeftest(ivTKpostPot, vcov=vcovHC(ivTKpostPot, type="HC0"))[,4], 1),3)

summary(ivreg(logduration ~ Wagedist + logPotUI | Wagedist + Wagedist*treatment, data = BKprereformPotFirst))
summary(ivreg(logduration ~ Wagedist + logPotUI | Wagedist + Wagedist*treatment, data = TKprereformPotFirst))
summary(ivreg(logduration ~ Wagedist + logPotUI | Wagedist + Wagedist*treatment, data = BKpostreformPotFirst))
summary(ivreg(logduration ~ Wagedist + logPotUI | Wagedist + Wagedist*treatment, data = TKpostreformPotFirst))
#####
#####

##### Test by year to control for year effects and inflation differences
#####
## Divide existing datasets by year
BKprereform2017 <- BKprereform %>% filter(BKprereform$year == 2017)
BKprereform2018 <- BKprereform %>% filter(BKprereform$year == 2018)
BKprereform2019 <- BKprereform %>% filter(BKprereform$year == 2019)
BKprereform2020 <- BKprereform %>% filter(BKprereform$year == 2020)

# No 2017 for prereform TK data
TKprereformNoCens2018 <- TKprereformNoCens %>% filter(TKprereformNoCens$year == 2018)
TKprereformNoCens2019 <- TKprereformNoCens %>% filter(TKprereformNoCens$year == 2019)
TKprereformNoCens2020 <- TKprereformNoCens %>% filter(TKprereformNoCens$year == 2020)

BKpostreform2020 <- BKpostreform %>% filter(BKpostreform$year == 2020)
BKpostreform2021 <- BKpostreform %>% filter(BKpostreform$year == 2021)
BKpostreform2022 <- BKpostreform %>% filter(BKpostreform$year == 2022)

TKpostreform2020 <- TKpostreform %>% filter(TKpostreform$year == 2020)
TKpostreform2021 <- TKpostreform %>% filter(TKpostreform$year == 2021)
TKpostreform2022 <- TKpostreform %>% filter(TKpostreform$year == 2022)


## CCT Kink estimates for optimal bandwidth
summary(rdrobust(BKprereform2017$logUI, BKprereform2017$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) 
summary(rdrobust(BKprereform2017$logduration, BKprereform2017$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) # 1.867 linear bw
summary(rdrobust(BKprereform2018$logUI, BKprereform2018$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) 
summary(rdrobust(BKprereform2018$logduration, BKprereform2018$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) # 2.395 linear bw
summary(rdrobust(BKprereform2019$logUI, BKprereform2019$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) 
summary(rdrobust(BKprereform2019$logduration, BKprereform2019$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) # 2.236 linear bw
summary(rdrobust(BKprereform2020$logUI, BKprereform2020$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) 
summary(rdrobust(BKprereform2020$logduration, BKprereform2020$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) #2.588 linear bw

# No 2017 for prereform TK data
summary(rdrobust(TKprereformNoCens2018$logUI, TKprereformNoCens2018$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) 
summary(rdrobust(TKprereformNoCens2018$logduration, TKprereformNoCens2018$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) # 13.493 linear bw
summary(rdrobust(TKprereformNoCens2019$logUI, TKprereformNoCens2019$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) 
summary(rdrobust(TKprereformNoCens2019$logduration, TKprereformNoCens2019$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) # 13.385 linear bw
summary(rdrobust(TKprereformNoCens2020$logUI, TKprereformNoCens2020$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) 
summary(rdrobust(TKprereformNoCens2020$logduration, TKprereformNoCens2020$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) # 24.702 linear bw

summary(rdrobust(BKpostreform2020$logUI, BKpostreform2020$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) 
summary(rdrobust(BKpostreform2020$logduration, BKpostreform2020$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) # 2.641 linear bw
summary(rdrobust(BKpostreform2021$logUI, BKpostreform2021$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) 
summary(rdrobust(BKpostreform2021$logduration, BKpostreform2021$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) # 1.995 linear bw. Significant at 5% level
summary(rdrobust(BKpostreform2022$logUI, BKpostreform2022$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) 
summary(rdrobust(BKpostreform2022$logduration, BKpostreform2022$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) # 2.704 linear bw

summary(rdrobust(TKpostreform2020$logUI, TKpostreform2020$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) 
summary(rdrobust(TKpostreform2020$logduration, TKpostreform2020$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) # 13.609 linear bw
summary(rdrobust(TKpostreform2021$logUI, TKpostreform2021$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) 
summary(rdrobust(TKpostreform2021$logduration, TKpostreform2021$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) # 19.682 linear bw
summary(rdrobust(TKpostreform2022$logUI, TKpostreform2022$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) 
summary(rdrobust(TKpostreform2022$logduration, TKpostreform2022$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) # 16.424 linear bw

## Bandwidth
BKprereform2017First <- BKprereform2017 %>% filter(between(Wagedist, -1.867, 1.867))
BKprereform2018First <- BKprereform2018 %>% filter(between(Wagedist, -2.395, 2.395))
BKprereform2019First <- BKprereform2019 %>% filter(between(Wagedist, -2.236, 2.236))
BKprereform2020First <- BKprereform2020 %>% filter(between(Wagedist, -2.588, 2.588))

TKprereformNoCens2018First <- TKprereformNoCens2018 %>% filter(between(Wagedist, -13.493, 13.493))
TKprereformNoCens2019First <- TKprereformNoCens2019 %>% filter(between(Wagedist, -13.385, 13.385))
TKprereformNoCens2020First <- TKprereformNoCens2020 %>% filter(between(Wagedist, -24.702, 24.702))

BKpostreform2020First <- BKpostreform2020 %>% filter(between(Wagedist, -2.641, 2.641))
BKpostreform2021First <- BKpostreform2021 %>% filter(between(Wagedist, -1.995, 1.995))
BKpostreform2022First <- BKpostreform2022 %>% filter(between(Wagedist, -2.704, 2.704))

TKpostreform2020First <- TKpostreform2020 %>% filter(between(Wagedist, -13.609, 13.609))
TKpostreform2021First <- TKpostreform2021 %>% filter(between(Wagedist, -19.682, 19.682))
TKpostreform2022First <- TKpostreform2022 %>% filter(between(Wagedist, -16.424, 16.424))

## Elasticity estimate
ivBKpre2017 <- ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = BKprereform2017First)
ivBKpre2018 <- ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = BKprereform2018First)
ivBKpre2019 <- ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = BKprereform2019First)
ivBKpre2020 <- ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = BKprereform2020First)

ivTKpre2018 <- ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = TKprereformNoCens2018First)
ivTKpre2019 <- ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = TKprereformNoCens2019First)
ivTKpre2020 <- ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = TKprereformNoCens2020First)

ivBKpost2020 <- ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = BKpostreform2020First)
ivBKpost2021 <- ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = BKpostreform2021First)
ivBKpost2022 <- ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = BKpostreform2022First)

ivTKpost2020 <- ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = TKpostreform2020First)
ivTKpost2021 <- ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = TKpostreform2021First)
ivTKpost2022 <- ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = TKpostreform2022First)

#white errors
round(tail(coeftest(ivBKpre2017, vcov=vcovHC(ivBKpre2017, type="HC0"))[,4], 1),3)
round(tail(coeftest(ivBKpre2018, vcov=vcovHC(ivBKpre2018, type="HC0"))[,4], 1),3)
round(tail(coeftest(ivBKpre2019, vcov=vcovHC(ivBKpre2019, type="HC0"))[,4], 1),3)
round(tail(coeftest(ivBKpre2020, vcov=vcovHC(ivBKpre2020, type="HC0"))[,4], 1),3)

round(tail(coeftest(ivTKpre2018, vcov=vcovHC(ivTKpre2018, type="HC0"))[,4], 1),3)
round(tail(coeftest(ivTKpre2019, vcov=vcovHC(ivTKpre2019, type="HC0"))[,4], 1),3)
round(tail(coeftest(ivTKpre2020, vcov=vcovHC(ivTKpre2020, type="HC0"))[,4], 1),3)

round(tail(coeftest(ivBKpost2020, vcov=vcovHC(ivBKpost2020, type="HC0"))[,4], 1),3)
round(tail(coeftest(ivBKpost2021, vcov=vcovHC(ivBKpost2021, type="HC0"))[,4], 1),3)
round(tail(coeftest(ivBKpost2022, vcov=vcovHC(ivBKpost2022, type="HC0"))[,4], 1),3)

round(tail(coeftest(ivTKpost2020, vcov=vcovHC(ivTKpost2020, type="HC0"))[,4], 1),3)
round(tail(coeftest(ivTKpost2021, vcov=vcovHC(ivTKpost2021, type="HC0"))[,4], 1),3)
round(tail(coeftest(ivTKpost2022, vcov=vcovHC(ivTKpost2022, type="HC0"))[,4], 1),3)

summary(ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = BKprereform2017First))
summary(ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = BKprereform2018First))
summary(ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = BKprereform2019First))
summary(ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = BKprereform2020First))

summary(ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = TKprereformNoCens2018First))
summary(ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = TKprereformNoCens2019First))
summary(ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = TKprereformNoCens2020First))

summary(ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = BKpostreform2020First))
summary(ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = BKpostreform2021First))
summary(ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = BKpostreform2022First))

summary(ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = TKpostreform2020First))
summary(ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = TKpostreform2021First))
summary(ivreg(logduration ~ Wagedist + logUI | Wagedist + Wagedist*treatment, data = TKpostreform2022First))
#####
#####

##### CPI deflation
#####
##Prereform bandwidth
summary(rdrobust(BKprereform$logUICPI, BKprereform$WagedistCPI, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) 
summary(rdrobust(BKprereform$logduration, BKprereform$WagedistCPI, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) #2.134 linear bw


summary(rdrobust(TKprereformNoCens$logUICPI, TKprereformNoCens$WagedistCPI, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) 
summary(rdrobust(TKprereformNoCens$logduration, TKprereformNoCens$WagedistCPI, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) #16.646 linear bw


#Postreform bandwidth
summary(rdrobust(BKpostreform$logUICPI, BKpostreform$WagedistCPI, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE))
summary(rdrobust(BKpostreform$logduration, BKpostreform$WagedistCPI, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) #2.208 linear bw


summary(rdrobust(TKpostreform$logUICPI, TKpostreform$WagedistCPI, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE))
summary(rdrobust(TKpostreform$logduration, TKpostreform$WagedistCPI, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) #14.116 linear bw

##
BKprereformFirstCPI <- BKprereform %>% filter(between(Wagedist, -2.134, 2.134))
TKprereformFirstCPI <- TKprereformNoCens %>% filter(between(Wagedist, -16.646 , 16.646 ))
BKpostreformFirstCPI <- BKpostreform %>% filter(between(Wagedist, -2.208, 2.208))
TKpostreformFirstCPI <- TKpostreform %>% filter(between(Wagedist, -14.116, 14.116))

##Main test
ivBKpreCPI <- ivreg(logduration ~ WagedistCPI + logUICPI | WagedistCPI + WagedistCPI*treatment, data = BKprereformFirstCPI)
ivTKpreCPI <- ivreg(logduration ~ WagedistCPI + logUICPI | WagedistCPI + WagedistCPI*treatment, data = TKprereformFirstCPI)
ivBKpostCPI <- ivreg(logduration ~ WagedistCPI + logUICPI | WagedistCPI + WagedistCPI*treatment, data = BKpostreformFirstCPI)
ivTKpostCPI <- ivreg(logduration ~ WagedistCPI + logUICPI | WagedistCPI + WagedistCPI*treatment, data = TKpostreformFirstCPI)
#white errors
round(tail(coeftest(ivBKpreCPI, vcov=vcovHC(ivBKpreCPI, type="HC0"))[,4], 1),3)
round(tail(coeftest(ivTKpreCPI, vcov=vcovHC(ivTKpreCPI, type="HC0"))[,4], 1),3)
round(tail(coeftest(ivBKpostCPI, vcov=vcovHC(ivBKpostCPI, type="HC0"))[,4], 1),3)
round(tail(coeftest(ivTKpostCPI, vcov=vcovHC(ivTKpostCPI, type="HC0"))[,4], 1),3)

summary(ivreg(logduration ~ WagedistCPI + logUICPI | WagedistCPI + WagedistCPI*treatment, data = BKprereformFirstCPI))
summary(ivreg(logduration ~ WagedistCPI + logUICPI | WagedistCPI + WagedistCPI*treatment, data = TKprereformFirstCPI))
summary(ivreg(logduration ~ WagedistCPI + logUICPI | WagedistCPI + WagedistCPI*treatment, data = BKpostreformFirstCPI))
summary(ivreg(logduration ~ WagedistCPI + logUICPI | WagedistCPI + WagedistCPI*treatment, data = TKpostreformFirstCPI))
#####
#####

##### BK2021 analysis
#####

#Figure A26
rdplot(BK2021$AvgUIB, BK2021$Wagedist,p=2,
       y.label = "Average UI benefits", x.label = "Daily Wage Relative to Floor", 
       title = "Figure A26: Daily UIB Benefits 2021 Bottom Kink", masspoints=0)
rdplot(BKpostreform2021$logduration, BKpostreform2021$Wagedist,p=2,
       y.label = "Log Unemployment Duration", x.label = "Daily Wage Relative to Floor", 
       title = "Figure A27: Length of Unemployment 2021 Bottom Kink")

summary(rdrobust(BKpostreform2021$logUI, BKpostreform2021$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) 
summary(rdrobust(BKpostreform2021$logduration, BKpostreform2021$Wagedist, c = 0, deriv=TRUE, p = 1, 
                 kernel = "uniform", masspoints = "off", all = TRUE)) # 1.995 linear bw. Significant at 5% level

rdplot(BKpostreform2021$gendermale, BKpostreform2021$Wagedist, c=0,p=2,
       y.label="% Male", x.label = "Daily Wage Relative to Floor", title = "Figure A28: Distribution of Gender, 2021 Bottom Kink ")
rdplot(BKpostreform2021$Blue, BKpostreform2021$Wagedist, c=0,p=2,
       y.label="% Bluecollar", x.label = "Daily Wage Relative to Floor", title = "Figure A29: % of Bluecollar, 2021 Bottom Kink ")
rdplot(BKpostreform2021$Edu, BKpostreform2021$Wagedist, c=0, 
       y.label="Years of education", x.label = "Daily Wage Relative to Floor", title = "Figure A30: Avg Education Length, 2021 BK")
rdplot(BKpostreform2021$age, BKpostreform2021$Wagedist,p=2,
       y.label = "Age", x.label = "Daily Wage Relative to Floor", 
       title = "Figure A31: Avg Age, 2021 Bottom Kink")
rdplot(BKpostreform2021$uib_days_appointed, BKpostreform2021$Wagedist,
       y.label = "Maximum days of UI", x.label = "Daily Wage Relative to Floor", 
       title = "Figure A32: Maximum UI Days 2021 Bottom Kink")

#####
#####
