Aerial Intel - data challenge
-----------------------------

The objective is to create an accurate model to predict wheat yield in some counties in the US. The counties are located in the states of Washington, Oklahoma, Kansas, Texas and Montana. Two years of data are available. Each datasets contains over 170,000 observations of 23 variables (the variables dates, county name and state are not taken into account). Most of the variable are climatic (temperature, precipitations, pressure etc). We will encourage simpler models that give slightly less accurate predictions but are easier to interpret.

Exploratory data analysis
-------------------------

``` r
datadir = "../data/"
  
# load the data
df2013 = read.csv(paste(datadir,"wheat-2013-supervised.csv",sep=""),header = TRUE)
df2014 = read.csv(paste(datadir,"wheat-2014-supervised.csv",sep=""),header = TRUE)
StatesList = unique(df2013[2])

# clean the data
toDrop = c("CountyName","State","Date")
df2013 = df2013[ , !(names(df2013) %in% toDrop)]
df2014 = df2014[ , !(names(df2014) %in% toDrop)]

toDrop = c("precipTypeIsOther")
df2013 = df2013[ , !(names(df2013) %in% toDrop)]
df2014 = df2014[ , !(names(df2014) %in% toDrop)]

# look for missing data - using summary
missing2013 = df2013[!complete.cases(df2013),]
missing2014 = df2014[!complete.cases(df2014),]

# get ride of these lines - more can be done
df2013 = na.omit(df2013)
df2014 = na.omit(df2014)

library(corrplot)
```

    ## Warning: package 'corrplot' was built under R version 3.3.2

``` r
c2013 <- cor(df2013)
c2014 <- cor(df2014)

corrplot(c2013,method="pie",type="lower")
```

![](readme_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
corrplot(c2014,method="pie",type="lower")
```

![](readme_files/figure-markdown_github/unnamed-chunk-1-2.png)

Multiple linear regression with lasso
-------------------------------------

Timelime
--------

``` r
summary(df2013)
```

    ##     Latitude       Longitude       apparentTemperatureMax
    ##  Min.   :27.80   Min.   :-120.91   Min.   :-39.97        
    ##  1st Qu.:34.17   1st Qu.:-101.29   1st Qu.: 37.81        
    ##  Median :36.82   Median : -99.13   Median : 58.86        
    ##  Mean   :37.53   Mean   :-100.88   Mean   : 54.82        
    ##  3rd Qu.:38.95   3rd Qu.: -97.35   3rd Qu.: 73.09        
    ##  Max.   :48.90   Max.   : -94.61   Max.   :177.32        
    ##  apparentTemperatureMin   cloudCover         dewPoint     
    ##  Min.   :-58.42         Min.   :0.00000   Min.   :-36.09  
    ##  1st Qu.: 14.31         1st Qu.:0.00000   1st Qu.: 19.60  
    ##  Median : 26.55         Median :0.01000   Median : 27.85  
    ##  Mean   : 27.90         Mean   :0.07158   Mean   : 29.71  
    ##  3rd Qu.: 42.18         3rd Qu.:0.09000   3rd Qu.: 38.88  
    ##  Max.   : 77.18         Max.   :1.00000   Max.   : 75.18  
    ##     humidity      precipIntensity   precipIntensityMax precipProbability
    ##  Min.   :0.0800   Min.   :0.00000   Min.   :0.00000    Min.   :0.0000   
    ##  1st Qu.:0.4700   1st Qu.:0.00000   1st Qu.:0.00000    1st Qu.:0.0000   
    ##  Median :0.6000   Median :0.00000   Median :0.00000    Median :0.0000   
    ##  Mean   :0.5941   Mean   :0.00116   Mean   :0.01065    Mean   :0.1337   
    ##  3rd Qu.:0.7200   3rd Qu.:0.00020   3rd Qu.:0.00280    3rd Qu.:0.0900   
    ##  Max.   :1.0000   Max.   :0.15290   Max.   :2.05490    Max.   :0.9600   
    ##  precipAccumulation precipTypeIsRain precipTypeIsSnow     pressure     
    ##  Min.   : 0.00000   Min.   :0.0000   Min.   :0.00000   Min.   : 942.5  
    ##  1st Qu.: 0.00000   1st Qu.:0.0000   1st Qu.:0.00000   1st Qu.:1011.2  
    ##  Median : 0.00000   Median :0.0000   Median :0.00000   Median :1016.7  
    ##  Mean   : 0.05755   Mean   :0.2108   Mean   :0.09046   Mean   :1017.1  
    ##  3rd Qu.: 0.00000   3rd Qu.:0.0000   3rd Qu.:0.00000   3rd Qu.:1022.9  
    ##  Max.   :19.48700   Max.   :1.0000   Max.   :1.00000   Max.   :1048.1  
    ##  temperatureMax   temperatureMin     visibility      windBearing   
    ##  Min.   :-22.00   Min.   :-39.79   Min.   : 0.600   Min.   :  0.0  
    ##  1st Qu.: 43.33   1st Qu.: 23.42   1st Qu.: 9.180   1st Qu.:127.0  
    ##  Median : 58.86   Median : 33.24   Median : 9.890   Median :192.0  
    ##  Mean   : 57.54   Mean   : 34.38   Mean   : 9.285   Mean   :191.1  
    ##  3rd Qu.: 73.09   3rd Qu.: 46.05   3rd Qu.:10.000   3rd Qu.:275.0  
    ##  Max.   :105.20   Max.   : 77.18   Max.   :10.000   Max.   :359.0  
    ##    windSpeed           NDVI        DayInSeason         Yield      
    ##  Min.   : 0.040   Min.   :117.0   Min.   :  0.00   Min.   : 9.00  
    ##  1st Qu.: 4.760   1st Qu.:137.9   1st Qu.: 46.00   1st Qu.:17.30  
    ##  Median : 7.670   Median :147.2   Median : 93.00   Median :31.10  
    ##  Mean   : 8.438   Mean   :146.3   Mean   : 92.63   Mean   :31.44  
    ##  3rd Qu.:11.530   3rd Qu.:152.9   3rd Qu.:139.00   3rd Qu.:43.10  
    ##  Max.   :31.730   Max.   :206.0   Max.   :185.00   Max.   :72.20
