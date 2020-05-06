# Create plots of various COVID-19 stats for all PA counties.
#
# Last modified 3 May 2020 by Rob Nicholas <rob.nicholas@gmail.com>.

# Clear the environment and close all existing plots.
rm(list=ls())
graphics.off()

# Number of days at the beginning of timeseries to skip when plotting.
dskip <- 40

# Define a function for a running mean of a vector. This is a "centered"
# mean with a default window of 7. If you change the window length, you'll
# want this to be an odd number (though this isn't strictly enforced).
csmooth <- function( ts, nn=7 )
{
  cx <- c(0,cumsum(ts))
  cx_smooth <- ( cx[(nn+1):length(cx)] - cx[1:(length(cx) - nn)] ) / nn
  cts <- vector( NA, length(ts) )
  nns <- floor(nn/2)
  cts[nns+1,length(ts)-nns]
  return(cts)
}

# Calculate position-to-position changes in the values of a vector. Used to
# determine new daily cases and deaths since the JHU data are cumulative.
uncum <- function( ts )
{
  uts <- ts[2,length(ts)] - ts[1:length(ts)-1]
  uts <- c(ts[1],uts)
  return(uts)
}

# Define a function for a running mean of a vector. This is a "trailing"
# mean with a default window of 7.
tsmooth <- function( ts, nn=7 )
{
  tts <- rep(NA,length(ts))
  
  return(tts)
}


# Read in US data from JHU dataset. Make sure you've first done a 'git pull'
# to grab the most recent version (updated multiple times daily).
ts_path <- '../COVID-19/csse_covid_19_data/csse_covid_19_time_series/'
us_cases <- read.csv(paste(ts_path,'time_series_covid19_confirmed_US.csv',sep=""))
us_deaths <- read.csv(paste(ts_path,'time_series_covid19_deaths_US.csv',sep=""))

# Extract list of states, territories, and cruise ships (!) loop over each.
state_list <- unique( us_cases[,7] )
for( state in state_list )
{

  # Identify and select those rows with data for locations in the state or
  # territory (usually counties).  We assume that the structure of the
  # datasets for cases and deaths is identical, i.e.  the locations match
  # 1:1, but this is not strictly true: the "deaths" database includes one
  # additional field (population), which is inserted at column 12.
  state_rows <- which( us_cases[,7] == state )
  state_ids <- us_cases[state_rows,6]
  state_cases <- us_cases[state_rows,-(1:11)]
  state_deaths <- us_deaths[state_rows,-(1:12)]
  state_population <- us_deaths[state_rows,12]

  # Aggregate data for the whole state.
  cum_state_cases <- colSums(state_cases)
  cum_state_deaths <- colSums(state_deaths)
  cum_state_population <- sum(state_population)
  l <- length(cum_state_cases)

  # Plot state cases.
  new_state_cases <- uncum(cum_state_cases)
  plot( new_state_cases[-(1:dskip)], xlab="days since 3/1/2020", ylab="new cases", main="COVID-19 Cases in", pch=16, col="blue", type="p" )
  grid()
  lines( csmooth(new_state_cases)[-(1:(dskip)], col="blue" )

  # Plot state deaths.
  new_state_deaths <- uncum(cum_state_deaths)
  plot( new_state_deaths[-(1:dskip)], xlab="days since 3/1/2020", ylab="deaths", main="COVID-19 Deaths in", pch=16, col="red", type="p" )
  grid()
  lines( csmooth(new_state_deaths)[-(1:(dskip)], col="red" )
}