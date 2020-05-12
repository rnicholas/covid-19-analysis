# Create plots of various COVID-19 stats for all PA counties.
#
# Last modified 8 May 2020 by Rob Nicholas <rob.nicholas@gmail.com>.

# Clear the environment and close all existing plots.
rm(list=ls())
graphics.off()

# Number of days at the beginning of timeseries to skip when plotting.
dskip <- 40

# "States" to exclude.
excluded_states <- c("American Samoa","Guam","Northern Mariana Islands","Diamond Princess","Grand Princess")
excluded_localities <- c("Out of","Unassigned","Bear River","TriCounty","Weber-Morgan","Central Utah","Southeast Utah","Southwest Utah")


# Function to write text into the corner of a plot, inside the plot frame.
# Defaults to upper left but position can be specified.
ctext <- function(text, location="topleft")
{
  legend(location,legend=text, bty ="n", pch=NA) 
}


# Define a function for a running mean of a vector.  This is a "centered"
# mean with a default window of 7.  If you change the window length, you'll
# want this to be an odd number (though this isn't strictly enforced).
#
# !! PLEASE DO NOT USE THIS FUNCTION -- IT IS NOT CORRECTLY IMPLEMENTED !!
#
csmooth <- function( ts, nn=7 )
{
  cx <- c(0,cumsum(as.vector(ts)))
  cx_smooth <- ( cx[(nn+1):length(cx)] - cx[1:(length(cx) - nn)] ) / nn
  cts <- rep(NA,length(ts))
  nns <- floor(nn/2)
  cts[(nns+1):(length(ts)-nns)] <- cx_smooth
return(cts)
}

# Define a function for a running mean of a vector. This is a "trailing"
# mean with a default window of 7.
tsmooth <- function( ts, nn=7 )
{
  tx <- c(0,cumsum(as.vector(ts)))
  tx_smooth <- ( tx[(nn+1):length(tx)] - tx[1:(length(tx) - nn)] ) / nn
  tts <- rep(NA,length(ts))
  tts[nn:length(ts)] <- tx_smooth
return(tts)
}

# Calculate position-to-position changes in the values of a vector.  Used to
# determine new daily cases and deaths since the JHU data are cumulative. 
# Negative values are set to zero; this allows us to ignore some apparent
# errors in the dataset where cumulative cases or deaths occasionally
# decrease on a given day.
uncum <- function( ts )
{
  uts <- ts[2:length(ts)] - ts[1:length(ts)-1]
  uts <- c(ts[1],uts)
  uts[uts<0] <- 0
  return(uts)
}

# Vector grep: Extend basic usage of grep function to allow a string vector rather than
# just a single string to be passed as the pattern.
vgrep <- function( vp, xx, ignore_case=FALSE, invert_match=FALSE, return_vector=FALSE )
{
  mpos <- NULL
  for( sp in vp)
  {
    mpos <- c( mpos, grep( sp, xx, ignore.case=ignore_case ) )
  }
  if( invert_match )
  {
    vpos <- 1:length(xx)
    vpos <- vpos[ !vpos %in% mpos ]
  }
  else { vpos <- mpos }
  if( return_vector ){ vgrep_ret <- xx[vpos] }
  else { vgrep_ret <- vpos }
  return(vgrep_ret)
}


# Read in US data from JHU dataset. Make sure you've first done a 'git pull'
# to grab the most recent version (updated multiple times daily).
ts_path <- '../COVID-19/csse_covid_19_data/csse_covid_19_time_series/'
us_cases <- read.csv(paste(ts_path,'time_series_covid19_confirmed_US.csv',sep=""))
us_deaths <- read.csv(paste(ts_path,'time_series_covid19_deaths_US.csv',sep=""))

# Extract list of states, territories, and cruise ships (!) loop over each.
state_list <- unique( us_cases[,7] )
state_list <- state_list[ !(state_list %in% excluded_states) ]
##
## a shorter list of states for testing
## state_list <- c("Kansas","Pennsylvania")
##
pdf("us_covid_atlas.pdf")
for( state in state_list )
{

  print("")
  print(state)
  print("===============")
  
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
  day <- seq( from=as.Date("2020-01-22"), by="days", length.out=l )
  begday <- format(as.Date(day[dskip]),"%B %-e")
  repday <- format(as.Date(day[l]),"%B %-e")
  
  # Plot state cases.
  new_state_cases <- uncum(cum_state_cases)
  # rtrend <- 100*lm( new_state_cases[(l-13):l] ~ seq(1,14) )$coefficients[2]*14/mean(new_state_cases[(l-20):(l-14)])
  twca <- sum(new_state_cases[(l-13):l])/(cum_state_population/100000)
  plot( day[-(1:dskip)], new_state_cases[-(1:dskip)], xlab="", ylab="new cases", main=paste(state,": COVID-19 Cases as of ",day[l],sep=""), pch=16, col="blue", type="p" )
  grid()
  lines( day[-(1:dskip)], tsmooth(new_state_cases)[-(1:(dskip))], col="blue" )
  #ctext(paste(sprintf("%+0.1f",rtrend),"%",sep=""))
  ctext(paste(sprintf("%0.1f",twca)," cases per 100k over past 14 days\n\n",format(cum_state_cases[l],big.mark=",")," total cases since March 1st\npopulation ",format(cum_state_population,big.mark=","),sep=""))
  
  # Plot state deaths.
  new_state_deaths <- uncum(cum_state_deaths)
  # rtrend <- 100*lm( new_state_deaths[(l-13):l] ~ seq(1,14) )$coefficients[2]*14/mean(new_state_deaths[(l-20):(l-14)])
  twda <- sum(new_state_deaths[(l-13):l])/(cum_state_population/100000)
  plot( day[-(1:dskip)], new_state_deaths[-(1:dskip)], xlab="", ylab="deaths", main=paste(state,": COVID-19 Deaths as of ",day[l],sep=""), pch=16, col="red", type="p" )
  grid()
  lines( day[-(1:dskip)], tsmooth(new_state_deaths)[-(1:(dskip))], col="red" )
  #ctext(paste(sprintf("%+0.1f",rtrend),"%",sep=""))
  # ctext(paste(sprintf("%0.1f",twda),"deaths per 100k over past 14 days"))
  ctext(paste(sprintf("%0.1f",twda)," deaths per 100k over past 14 days\n\n",format(cum_state_deaths[l],big.mark=",")," total deaths since March 1st\npopulation ",format(cum_state_population,big.mark=","),sep=""))
  
  # States generally have separate rows for each county or parish, plus rows for "Out of XX"
  # and "Unassigned"; thus anything with more than 3 rows has counties or other localities
  # which we plot separately here. The "Out of XX" and "Unassigned" categories are all zeros
  # for many states and are typically small otherwise, so we don't bother to plot (but they
  # WERE included in the state totals above).
  #
  if( length(state_ids) > 3 )
  {
    locality_list <- state_ids[ vgrep( excluded_localities, state_ids, invert_match=TRUE ) ]
    # We need to use vgrep() instead of the following because it doesn't match substrings.
    # locality_list <- state_ids[ !(state_ids %in% excluded_localities) ]
    #
    for(locality in locality_list)
    {
      locality_row <- which( locality_list == locality )
      print(paste(locality,", ",state,sep=""))

      locality_cases <- unlist(state_cases[locality_row,])
      locality_deaths <- unlist(state_deaths[locality_row,])
      locality_population <- state_population[locality_row]
      
      # Plot locality cases.
      new_locality_cases <- uncum(locality_cases)
      twca <- sum(new_locality_cases[(l-13):l])/(locality_population/100000)
      plot( day[-(1:dskip)], new_locality_cases[-(1:dskip)], xlab="", ylab="new cases", main=paste(locality,", ",state,": COVID-19 Cases as of ",day[l],sep=""), pch=16, col="blue", type="p" )
      grid()
      lines( day[-(1:dskip)], tsmooth(new_locality_cases)[-(1:(dskip))], col="blue" )
      #ctext(paste(sprintf("%0.1f",twca),"cases per 100k over past 14 days"))
      ctext(paste(sprintf("%0.1f",twca)," cases per 100k over past 14 days\n\n",format(locality_cases[l],big.mark=",")," total cases since March 1st\npopulation ",format(locality_population,big.mark=","),sep=""))
      
      # Plot locality deaths.
      new_locality_deaths <- uncum(locality_deaths)
      twda <- sum(new_locality_deaths[(l-13):l])/(locality_population/100000)
      plot( day[-(1:dskip)], new_locality_deaths[-(1:dskip)], xlab="", ylab="deaths", main=paste(locality,", ",state,": COVID-19 Deaths as of ",day[l],sep=""), pch=16, col="red", type="p" )
      grid()
      lines( day[-(1:dskip)], tsmooth(new_locality_deaths)[-(1:(dskip))], col="red" )
      #ctext(paste(sprintf("%0.1f",twda),"deaths per 100k over past 14 days"))
      ctext(paste(sprintf("%0.1f",twda)," deaths per 100k over past 14 days\n\n",format(locality_deaths[l],big.mark=",")," total deaths since March 1st\npopulation ",format(locality_population,big.mark=","),sep=""))
      
    }  
  }  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
}
