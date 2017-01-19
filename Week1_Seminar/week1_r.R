setwd("~/Desktop/STAT_450/lab")

library(gapminder)

head(gapminder)
tail(gapminder)

# structure of the data set
str(gapminder)  
# 1704 rows and 6 columns
# 6 dimentional 
dim(gapminder)
nrow(gapminder)
ncol(gapminder)

gDat <- gapminder
#there is only one type data (vector, list...) in matrix; that's why we use data frame
gDat.mat <- as.matrix(gDat)
head(gDat.mat)
str(gDat.mat)

#extract data 
gDat[, c(3,4)]
gDat[1,]
gDat[1:50, c(3,4)]
gDat[gDat$country =="Canada",]

min(gDat$year) #1952
max(gDat$year) #2007
#extract Canada and lifeExp & year 
a <- gDat[gDat$country == "Canada", c("lifeExp", "year") ]
plot(lifeExp~ year, data = a, type = "l")

b <- gDat[gDat$country == "Rwanda", c("lifeExp", "year") ]
plot(lifeExp~ year, data = b, type = "l")

#Q: rate of change of population in Europe with respect to Americas
# plot it & variablity & limitation of the results 

# create a function to compute the rate of change
deltaCompute <- function(x){
  x.lag <- Hmisc::Lag(x) # what does the function lag() do?
  #Compute a lagged version of a time series, shifting the time base back by a given number of observations.
  x.diff <- x - x.lag
  x.delta <- x.diff/x.lag
  
  return(x.delta)
}

# extract only the data we need
gDat.europe <- gDat[gDat$continent == "Europe",c("country", "year", 'pop')]

# use our function to compute the delta of pop
gDat.europe.delta <- deltaCompute(gDat.europe$pop)

# combine into original gDat.europe data.frame
gDat.europe.df <- data.frame(gDat.europe, Delta = gDat.europe.delta)

# plot results
plot(Delta ~ ., 
     data = gDat.europe.df[-1,], 
     main = "Annual Change of population for Europe",
     type = "l",
     col = "blue")

# extract only the data we need
gDat.americas <- gDat[gDat$continent == "Americas",c("country", 'year', 'pop')]

# use our function to compute the delta of pop
gDat.americas.delta <- deltaCompute(as.numeric(gDat.americas$pop))

# combine into original gDat.europe data.frame
gDat.americas.df <- data.frame(gDat.americas, Delta = gDat.americas.delta)

# plot results
plot(Delta ~ ., 
     data = gDat.americas.df[-1,], 
     main = "Annual Change of population for Americas",
     type = "l",
     col = "red")



