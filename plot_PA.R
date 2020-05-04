# Create plots of various COVID-19 stats for all PA counties.
#
# Last modified 3 May 2020 by Rob Nicholas <rob.nicholas@gmail.com>.

# Clear the environment and close all existing plots.
rm(list=ls())
graphics.off()

# Read in US data from JHU dataset. Make sure you've first done a 'git pull'
# to grab the most recent version (updated multiple times daily).
ts_path <- '../COVID-19/csse_covid_19_data/csse_covid_19_time_series/'
us_cases <- read.csv(paste(ts_path,'time_series_covid19_confirmed_US.csv',sep=""))
us_deaths <- read.csv(paste(ts_path,'time_series_covid19_deaths_US.csv',sep=""))



# Identify and select those rows with PA data (mostly counties).
# We assume that the structure of the datasets for cases and deaths is identical,
# i.e. the locations match 1:1.
subset_rows <- which(us_cases[,7] == "Pennsylvania")
subset_ids <- us_cases[subset_rows,6]
subset_cases <- us_cases[subset_rows,-(1:11)]
subset_deaths <- us_deaths[subset_rows,-(1:12)]
subset_population <- us_deaths[subset_rows,12]

# Aggregate data for the whole state.
aggregate_cases <- colSums(subset_cases)
aggregate_deaths <- colSums(subset_deaths)
aggregate_population <- sum(subset_population)
l <- length(aggregate_cases)

dtd_aggregate_cases <- aggregate_cases[2:l] - aggregate_cases[1:l-1]
dtd_aggregate_deaths <- aggregate_deaths[2:l] - aggregate_deaths[1:l-1]

# Plot state cases and deaths.
plot(dtd_aggregate_cases[-(1:40)], xlab="days since 3/1/2020", ylab="new cases", main="COVID-19 in Pennsylvania", pch=16, col="blue", type="o")
grid()
x11()
plot(dtd_aggregate_deaths[-(1:40)], xlab="days since 3/1/2020", ylab="deaths", main="COVID-19 in Pennsylvania", pch=16, col="dark red", type="o")
grid()

# Plot Centre County cases and deaths.
locality_row <- which(subset_ids == "Centre")
locality_cases <- as.vector(t(subset_cases[locality_row,]))
dtd_locality_cases <- locality_cases[2:l] - locality_cases[1:l-1]
locality_deaths <- as.vector(t(subset_deaths[locality_row,]))
dtd_locality_deaths <- locality_deaths[2:l] - locality_deaths[1:l-1]

x11()
plot(dtd_locality_cases[-(1:40)], xlab="days since 3/1/2020", ylab="new cases", main="COVID-19 in Centre County", pch=16, col="blue", type="o")
grid()
x11()
plot(dtd_locality_deaths[-(1:40)], xlab="days since 3/1/2020", ylab="deaths", main="COVID-19 in Centre County", pch=16, col="dark red", type="o")
grid()

