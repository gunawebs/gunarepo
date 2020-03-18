library(tidyverse)
library(lubridate)
library(ggpubr)

intCovidDf <- function(){
  caseDatLoc <- "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
  caseTibble <<- read_csv(caseDatLoc)
}

plotCountryData<-function(country="India"){
  p1 <- caseTibble%>%filter(`Country/Region`==country)%>%select(-`Province/State`,-Lat,-Long)%>%
    rename(Country=`Country/Region`)%>%
    gather("Date", "NumCases", -Country)%>%
    mutate(Date=mdy(Date))%>%
    ggplot(aes(x=Date, y = NumCases)) + geom_line()+geom_point(color="red")+ggtitle(paste0("Total Covid-19 Cases: ", country))

  p2 <- caseTibble%>%filter(`Country/Region`==country)%>%select(-`Province/State`,-Lat,-Long)%>%
    rename(Country=`Country/Region`)%>%
    gather("Date", "NumCases", -Country)%>%
    mutate(Date=mdy(Date))%>%
    ggplot(aes(x=Date, y = NumCases)) + geom_line()+geom_point(color="blue")+ggtitle(paste0("Total Covid-19 Cases: (Log Scale) ", country))+
    scale_y_continuous(trans='log10')

  ggarrange(p1, p2, ncol = 1)

}
