library(benford.analysis)
library(BenfordTests)
library(tidyverse)
library(BeyondBenford)
library(eforensics)
library(openxlsx)
library(gtrendsR)
library(reshape2)
library(dplyr)
library(benford.analysis)
library(foreach)
library(doParallel)

source('dig.distjd.R')
# read data
GA_activevoters <- read.xlsx("https://sos.ga.gov/admin/uploads/Active_Voters_by_Race_Gender_as_of_November_1_2020.xlsx", sheet = 'ACT_VOTERSBY_RACE_AND_GENDER', rows = 9:168, )
GA_activevoters$TOTAL.VOTERS <- as.integer(GA_activevoters$TOTAL.VOTERS)

countypop = read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv", header = T)
# filter out columns and state aggregate data
countypop_filter <- countypop %>% filter(COUNTY != 0) %>% select(c(STNAME, CTYNAME, CENSUS2010POP))
## get just data for GA
countypop_GA <- countypop %>% filter(STNAME == 'Georgia' & COUNTY != 0)
# perform benford analysis
AllUSCounties <- dig.distjd(countypop_filter$CENSUS2010POP, dig = 1, label = 'All US Counties') 
AllUSCounties$data$Digit <- as.integer(AllUSCounties$data$Digit)
ggplot(AllUSCounties$data, aes(x = Digit, y = Freq.Prob, fill = Distribution)) + geom_bar( stat = 'identity', position = 'dodge') + theme_minimal() 
  
GA_voters_benford <-benford(GA_activevoters$TOTAL.VOTERS, number.of.digits = 1)
plot(GA_voters_benford)
# plot results
plot(trends)

county_populations <- read.csv('county_population.csv', header = T, stringsAsFactors = F)

## view a historgram of those data 
ggplot(county_populations, aes(x = CENSUS2010POP)) + geom_histogram(boundary = 0) + theme_minimal() +labs(x = 'Value', y = 'Frequency of Observation\n(out of 3142 US counties)')





data2020 <- read.csv('2020GA.csv', stringsAsFactors = F)
data2020$BIDEN.VOTES <- gsub(',', '\\', data2020$BIDEN.VOTES)
data2020$BIDEN.VOTES <- as.integer(data2020$BIDEN.VOTES)
data2020$TRUMP.VOTES <- gsub(',' , '\\', data2020$TRUMP.VOTES)
data2020$TRUMP.VOTES <- as.integer(data2020$TRUMP.VOTES)

Bid<- dig.distjd(data2020$BIDEN.VOTES, dig = 1, main = "Comparison of Benford's Law (1st digit) to Biden Votes\n(159 counties)", label = 'Biden' )


plot(Benf.val(1:9, dig = 2)*159)

data <- read.csv('countypres_2000-2016.csv')

GA2016Trump <- filter(data, year == 2016 & state_po == 'GA' & candidate == 'Donald Trump')
digit.distr(GA2016Trump$candidatevotes, dig = 2)

GA2016total <- benford(filter(data, year == 2016 & state_po == 'GA')$totalvotes, number.of.digits = 1)
plot(GA2016total)
chi2(GA2016Trump$candidatevotes, dig = 2, pval = 1)

GA2016Trump <-  benford(filter(data, year == 2016 & state_po == 'GA' & candidate == 'Donald Trump')$candidatevotes, number.of.digits = 1)
plot(GA2016Trump)

GA2016Clinton <-  benford(filter(data, year == 2016 & state_po == 'GA' & candidate == 'Hillary Clinton')$candidatevotes, number.of.digits = 1)
plot(GA2016Clinton)

GA2020Biden <- benford(data2020$BIDEN.VOTES, number.of.digits = 1)
plot(GA2020Biden)

GA2020Trump <- benford(data2020$TRUMP.VOTES, number.of.digits = 1)
plot(GA2020Trump)


cl <-makeCluster(8)
registerDoParallel(cl)

MAD <- foreach(i = seq(10, 5000, 10), .combine = rbind, .packages = c('foreach', 'benford.analysis')) %dopar% {
 
  inside <- foreach (j = 1:30, .combine = rbind) %do% {
    
    Bdat <-data.frame(A = runif(i, 0, 100000), B = runif(i), C = rpois(i, 300), D = rexp(i, 1/200), E = rnorm(i, 100000, 1000), 
                      FF = exp(runif(i, 0, 100)), G = 2^runif(i, 0, 30), H = runif(i, 0,100), I = rnbinom(i, 5, .3), J = rgamma(i, 2.4, 5))
    
    Bdat$Benford <- apply(Bdat, 1, prod)
    BenfordPerfect <- benford(Bdat$Benford, number.of.digits = 1)
    
    data.frame(n = i, MAD = BenfordPerfect$MAD)
    
  }
   
}

stopCluster(cl)

write.csv(MAD, 'MAD.csv', row.names = F)

MAD <- read.csv('MAD.csv')

ggplot(MAD, aes(x = n, y = MAD)) + geom_point(alpha = .04 ) + geom_hline(yintercept = .015, colour = 'red') +
  geom_vline(xintercept =  159) + theme_minimal() +
  annotate('label',x = 3000, y = .018, label = 'MAD = .015, Report non-conformity above this line') +
  annotate('label', x = 2000, y = .08, label = 'n = 159, the number of counties in GA') + 
  annotate('segment', x = 600, y = .08, xend = 159, yend = .08, arrow = arrow(length = unit(0.02, "npc") ))



Bdat <-data.frame(A = runif(5000, 0, 100000), B = runif(5000), C = rpois(5000, 300), D = rexp(5000, 1/200), E = rnorm(5000, 100000, 1000), 
           FF = exp(runif(5000, 0, 100)), G = 2^runif(5000, 0, 30), H = runif(5000, 0,100), I = rnbinom(5000, 5, .3), J = rgamma(5000, 2.4, 5))

Bdat$Benford <- apply(Bdat, 1, prod)

BenfordPerfect <- benford(sample(Bdat$Benford, 2000, replace = F) , number.of.digits = 1)
BenfordPerfect

#all 159 of them. Your sample size is 159 (that's how many rows of data you copied from politico, one row for each county). Did you know that the first digit distribution of the population of GA counties also does not conform to BL? Neither does the number of active voters in GA by county on Nov 1, 2020. Did you know that if you look at the same data for all the presidential elections 