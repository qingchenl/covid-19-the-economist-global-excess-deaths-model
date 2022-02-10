###corresponds to 1_excess_deaths_global_estimates_data_generation.R

## characterising excess_deaths_source ##
str(excess_deaths_source)
# 11896 observations total between 21-Dec-2019 and 31-Jan-2022
country_freqs <- table(excess_deaths_source$country)
sort(country_freqs) #range from 16 to 218
length(country_freqs) #109 countries

summary(excess_deaths_source$covid_deaths_per_100k) #range from -5 to 208
# negatives?
neg_covid_deaths <- excess_deaths_source[excess_deaths_source$covid_deaths_per_100k < 0,]
print(neg_covid_deaths, n = 24)
# each observation consists of two otherwise identical entries differing by only date (start, end)
# this is a little strange, will structure be cleaned later?
# population isn't being revised for deaths, how might this affect interpretation of the data?


## is excess deaths per 100k bimodal?
with(excess_deaths_source, plot(population, excess_deaths_per_100k))
with(excess_deaths_source, plot(population, abs(excess_deaths_per_100k)))


deaths_dev <- abs(excess_deaths_source$excess_deaths_per_100k)
lm_deaths_dev <- lm(deaths_dev ~ excess_deaths_source$population)
summary(lm_deaths_dev)

#observation: the extremes of excess deaths per 100k are dominated by small countries
#possibly indicating large variance in the null model explaining a large part of deviation
#want to pool data within countries to mitigate that somewhat

total_excess_deaths_per_100k <- list()
var_excess_deaths_per_100k <- list()
var_deaths_per_100k <- list()
pops <- list()
pops_range <- list()
for(country in unique(excess_deaths_source$country)) {
	total_excess_deaths_per_100k[[country]] <- sum(excess_deaths_source$excess_deaths_per_100k[excess_deaths_source$country == country])
	var_excess_deaths_per_100k[[country]] <- var(excess_deaths_source$excess_deaths_per_100k[excess_deaths_source$country == country])
	#var_deaths_per_10k <- var(excess_deaths_source$total_deaths)
	pops[[country]] <- mean(excess_deaths_source$population[excess_deaths_source$country == country])
	pops_range[[country]] <- range(excess_deaths_source$population[excess_deaths_source$country == country])
}

# check if pops are actually treated as constant throughout
ssq_pr <- 0
for (r in pops_range) {
	ssq_pr <- (r[2] - r[1])^2
}
ssq_pr
# confirmed, can use pops as denominator at this stage

hist(as.numeric(total_excess_deaths_per_100k), nclass = 50)
hist(as.numeric(var_excess_deaths_per_100k), nclass = 50)

plot(as.numeric(pops), as.numeric(var_excess_deaths_per_100k))

##what is the population range again?
with(excess_deaths_source, summary(population))
with(excess_deaths_source, boxplot(log(population)))


## characterising daily_excess_deaths ##

str(daily_excess_deaths_raw) #67673 observations - data being interpolated
length(unique(daily_excess_deaths_raw$iso3c)) # 106 countries

daily_excess_deaths
#67608 observations
