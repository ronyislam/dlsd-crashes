install.packages("pacman")
library(pacman)
p_load(rgeos
       , rgdal
       , devtools
       , viridis
       , rgeos
       , colorspace
       , RSocrata
       , tidyverse
       , ggplot2
       , lubridate
       , rtweet
       , leaflet
       , leaflegend
       , webshot2)
install_github("r-spatial/sf", configure.args = "--with-proj-lib=/usr/local/lib/")
p_load(sf)

startDate<-ymd(today(tz = "America/Chicago")-1)
endDate<-ymd(today(tz = "America/Chicago")-0)
dateQueryString <- paste0("crash_date between '",startDate, "' and '", endDate,"'")
streetFilter<- "and street_name like '%LAKE SHORE DR%'"

##Authenticate Twitter
auth <- rtweet_bot(
  api_key       = Sys.getenv("TWITTER_API_KEY"),
  api_secret    = Sys.getenv("TWITTER_API_KEY_SECRET"),
  access_token  = Sys.getenv("TWITTER_ACCESS_TOKEN"),
  access_secret = Sys.getenv("TWITTER_ACCESS_TOKEN_SECRET")
)

##Check if Data is ready
sleep <- function(x){
  Sys.sleep(x)
}

numberOfChecks <- 0
dataReadyFlag <- FALSE

checkIfDataRefreshed <- function(dateQueryString){
  chicagoCrashPeople <- read.socrata(paste0("https://data.cityofchicago.org/resource/u6pd-qa9d.json?$where=",dateQueryString))
  chicagoCrashCrashes <- read.socrata(paste0("https://data.cityofchicago.org/resource/85ca-t3if.json?$where=",dateQueryString))
  if(nrow(chicagoCrashCrashes)<10 | nrow(chicagoCrashPeople)<10){
    dataReadyFlag <- FALSE
    if(numberOfChecks == 0){
      auth_as(auth)
      ## post reply
      delayTweet<-post_tweet("Beep Boop Bop, I'm still waiting for The City to upload data. Please hold.")
      numberOfChecks <- numberOfChecks +1 
    }else{
      numberOfChecks <- numberOfChecks +1 
    }
    message(paste0("DATA IS NOT READY YET! As of ",now(tz='America/Chicago')))
    sleep(900)
    checkIfDataRefreshed(dateQueryString)
  }else{
    if(numberOfChecks > 0){
      ##DELETE data check tweet
      auth_as(auth)
      my_timeline <- get_my_timeline()
      ## ID for destruction
      destroy_id <- my_timeline$id_str[1]
      post_destroy(destroy_id)
    }
    dataReadyFlag <- TRUE
    return(dataReadyFlag)
  }
}

dataReadyFlag<-checkIfDataRefreshed(dateQueryString)

if(dataReadyFlag == TRUE){
  chicagoCrashPeople <- read.socrata(paste0("https://data.cityofchicago.org/resource/u6pd-qa9d.json?$where=",dateQueryString))
  chicagoCrashCrashes <- read.socrata(paste0("https://data.cityofchicago.org/resource/85ca-t3if.json?$where=",dateQueryString,streetFilter))
  
}else{
  dataReadyFlag<-checkIfDataRefreshed(dateQueryString)
}


chicagoCrashCrashes <- chicagoCrashCrashes %>% 
  filter(!grepl("LAKE SHORE DR E", street_name, fixed = TRUE)) %>%
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude)) %>%
  mutate(injury_super_class = if_else(most_severe_injury == 'NO INDICATION OF INJURY' | is.na(most_severe_injury), 'No Injury', 
                                      if_else(most_severe_injury == 'NONINCAPACITATING INJURY' | most_severe_injury =='REPORTED, NOT EVIDENT', 'Injury',
                                              if_else(most_severe_injury =='INCAPACITATING INJURY', 'Severe Injury', 'Fatal'))))


moreDLSDcrashes <- read.socrata(paste0("https://data.cityofchicago.org/resource/85ca-t3if.json?$where=street_name like '%LAKE SHORE DR%'")) %>% 
  filter(!grepl("LAKE SHORE DR E", street_name, fixed = TRUE))

dlsdCrashStatsYearMonthDay <- moreDLSDcrashes %>% 
  group_by(as.Date(crash_date)) %>%
  count(as.Date(crash_date)) %>%
  rename(crashes = n, crash_date = "as.Date(crash_date)") %>%
  filter(!is.na(crash_date)) %>%
  filter(crash_date>='2017-10-01')

dlsdCrashStatsYearMonth <- moreDLSDcrashes %>% 
  group_by(month(crash_date), year(crash_date)) %>%
  count(month(crash_date)) %>%
  rename(crashes = n, month = "month(crash_date)", year = "year(crash_date)") %>%
  filter(!is.na(month)) %>%
  mutate(year_month = ymd(paste0(year,'-',month,'-01'))) %>%
  filter(year_month>='2017-10-01')

dlsdCrashStatsDOY <- moreDLSDcrashes %>% 
  filter(crash_date>=ymd('2018-01-01'), crash_date<=ymd('2021-12-31')) %>%
  group_by(month(crash_date), day(crash_date)) %>%
  count(day(crash_date)) %>%
  rename(crashes = n, month = "month(crash_date)", day = "day(crash_date)") %>%
  filter(!is.na(month)) %>%
  mutate(day_month = (paste0(month,"-",day)))
  
dateDiff<-ymd(today())-ymd('2018-01-01')
as.numeric(dateDiff, units = "days")
avgCrashesPerDay<-round(sum(dlsdCrashStatsYearMonth$crashes)/as.numeric(dateDiff, units = "days"))

allDates<-as.data.frame(table(seq(ymd('2018-01-01'),ymd(today()-2), by = "day"), dnn = list("crash_date"))) %>% select(crash_date) %>%
  mutate(crash_date = ymd(crash_date))

allCrashesDates <- allDates %>%
  left_join(dlsdCrashStatsYearMonthDay)

allCrashesDates %>% filter(is.na(crashes))

lastDayNoCrashOnDLSD <- last(allCrashesDates$crash_date[is.na(allCrashesDates$crashes)], order_by = allCrashesDates$crash_date[is.na(allCrashesDates$crashes)])


daysSinceLastNoCrash <- as.numeric(ymd(today()-1) - lastDayNoCrashOnDLSD , units = "days")

ggplot(data=dlsdCrashStatsDOY, aes(x=day_month, y=crashes)) +
  geom_bar(stat="identity")

dlsdCrashStatsDOY[dlsdCrashStatsDOY$crashes == max(dlsdCrashStatsDOY$crashes),]


chicagoCrash <- chicagoCrashCrashes %>%
  left_join(chicagoCrashPeople)


crashesPersonInjury <- chicagoCrash %>%
  filter(injury_super_class != 'No Injury' & injury_super_class != 'Fatal') %>%
  count(person_type, injury_super_class) %>%
  rename(Injuries = n)

## Define globals
LEAFLET_TILES <- "CartoDB.DarkMatter"

##==============================================================================
## DOWNLOAD DATA
##==============================================================================

## Contained code to download Chicago's wards
shpCityWards <- local({
  cur <- getwd()
  on.exit(setwd(cur))
  
  tmp <- tempfile(fileext = ".zip")
  setwd(dirname(tmp))
  # url <- "https://data.cityofchicago.org/api/geospatial/sp34-6z76?method=export&format=Shapefile"
  url <- "https://data.cityofchicago.org/api/geospatial/sp34-6z76?method=export&format=GeoJSON"
  download.file(url, destfile = tmp)
  shp <- rgdal::readOGR(basename(tmp), stringsAsFactors = FALSE)
  shp
})

streetLines <- local({
  cur <- getwd()
  on.exit(setwd(cur))
  
  tmp <- tempfile(fileext = ".zip")
  setwd(dirname(tmp))
  # url <- "https://data.cityofchicago.org/api/geospatial/sp34-6z76?method=export&format=Shapefile"
  url <- "https://data.cityofchicago.org/api/geospatial/6imu-meau?method=export&format=GeoJSON"
  download.file(url, destfile = tmp)
  shp <- rgdal::readOGR(basename(tmp), stringsAsFactors = FALSE)
  shp
})


streetLinesDLSD <- streetLines[streetLines$street_nam == 'LAKE SHORE',]
streetLinesDLSD <- streetLinesDLSD[streetLinesDLSD$class == 1,]
## Generate city outline
shpCityOutline <- rgeos::gUnaryUnion(as(shpCityWards, "SpatialPolygons"))



getSize <- function(crashesWard) {
  sapply(crashesWard$injury_super_class, function(injury_super_class) {
    if(injury_super_class == 'No Injury') {
      .2
    } else if(injury_super_class == 'Fatal') {
      1
    } else if(injury_super_class == 'Severe Injury') {
      .6
    } else {
      .4
    } })
}


getColor <- function(chicagoCrashCrashes) {
  sapply(chicagoCrashCrashes$injury_super_class, function(injury_super_class) {
    if(injury_super_class == 'No Injury') {
      "yellow"
    } else if(injury_super_class == 'Fatal') {
      "aquamarine"
    } else {
      "red"
    } })
}


orderedInjuries <- c('No Injury', 'Injury', 'Severe Injury', 'Fatal')
factorPal <- colorFactor('BrBG', domain = orderedInjuries, ordered = TRUE)


crashCoordsMap <- leaflet(chicagoCrashCrashes, options = leafletOptions(zoomControl = FALSE,
                                                                        zoomSnap = .2,
                                                                        zoomDelta = .5)) %>%
  addProviderTiles(LEAFLET_TILES) %>%
  addPolylines(data = streetLinesDLSD, color = "white", weight = 3, smoothFactor = 4) %>%
  addSymbolsSize(values = ~getSize(chicagoCrashCrashes),
                 lat = ~latitude,
                 lng = ~longitude,
                 shape = c('circle'),
                 color = ~factorPal(injury_super_class),
                 opacity=1,
                 baseSize=7) %>%
  addLegendFactor(title = "Most Severe Injury", pal = factorPal, shape = "circle",values =  ~factor(orderedInjuries, levels = orderedInjuries), position = 'topright')

crashCoordsMap

htmlwidgets::saveWidget(widget = crashCoordsMap, file = "maps/temp/crashCoordsMap/map.html", selfcontained = FALSE)
webshot2::webshot(url = "maps/temp/crashCoordsMap/map.html", file = paste0("maps/","crashCoordsMap", "-", startDate,".png"), 
                  delay = 1,
                  zoom = 2)

firstTweetImg <- paste0(getwd(),"/maps/","crashCoordsMap", "-", startDate,".png")

numberOfPeople <- nrow(chicagoCrash)
numberOfCrashes <- nrow(chicagoCrashCrashes)
numberOfPeopleInjured <- sum(crashesPersonInjury$Injuries)

if(numberOfPeopleInjured==1){
  peopleInjuredText <- " There was 1 person injured."
}else if(numberOfPeopleInjured>1){
  peopleInjuredText <- paste0(" There were ",numberOfPeopleInjured, " people injured.")
}else{
  peopleInjuredText <- " Thankfully, no one was reported injured."
}

if(numberOfCrashes==1){
  numberOfCrashesText <- paste0("Was there a traffic crash on DLSD on ",startDate,"?\n\nYes.\n\nThere was one crash.",peopleInjuredText)
}else if(numberOfCrashes>1){
  if(numberOfCrashes>avgCrashesPerDay){
    higherThanAverage<-paste0(" This is ", round((numberOfCrashes/avgCrashesPerDay)*100), "% higher than average.")
    numberOfCrashesText <- paste0("Was there a traffic crash on DLSD on ",startDate,"?\n\nYes.\n\nThere were ", numberOfCrashes," crashes.",
                                  higherThanAverage,
                                  peopleInjuredText)
  }else{
    numberOfCrashesText <- paste0("Was there a traffic crash on DLSD on ",startDate,"?\n\nYes.\n\nThere were ", numberOfCrashes," crashes.",
                                  peopleInjuredText)
  }
  
    
}else{
  numberOfCrashesText<- paste0("Was there a traffic crash on DLSD on ",startDate,"?\n\nNo.\n\nThe last time this happened was ",daysSinceLastNoCrash, " days ago.")
}
numberOfCrashesText 


post_tweet(numberOfCrashesText, media = firstTweetImg,
           media_alt_text = "Map of all reported traffic crash on DuSable Lake Shore Drive on given date.")
