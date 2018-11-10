---
title: 'Homework 06: Data wrangling wrap up'
author: Stefanie Lumnitz
date: "09 November, 2018"   
output:
  html_document:
    keep_md: true
    toc: true
    toc_depth: 2
    theme: readable
---


### Load required packages


```r
library(singer)
library(leaflet)
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(revgeo))
suppressPackageStartupMessages(library(kableExtra))
library(stringr)
```


# Exercise 4: Geospatial R and the `singer` data

### Task

The `singer_location` dataframe in the `singer` package contains geographical information stored in two different formats: 

1. as a (dirty!) variable named `city`; 
2. as a latitude / longitude pair (stored in `latitude`, `longitude` respectively). 

I am trying to clean up the `city` variable containing multiple NA values, by adding addresses in the same format to each observation respectively. I will herefore use a technique called reverse geocoding:

> **Reverse geocoding** is the process of converting geographic coordinates into a human-readable address.

### Tools

The exercise suggests to use:

> **ggmap::revgeocode**: reverse geocodes a longitude/latitude location using Google Maps. Note that in most cases by using this function you are agreeing to the Google Maps API Terms of Service at https://developers.google.com/maps/terms.

Suggestiing to use additional resources:
[**Google Maps API**](https://developers.google.com/maps/documentation/geocoding/start?csw=1)

Since I am using the daily limit of this very same required API key I decided to not use `revgeocode` and Google maps for this problem. I discovered a second package called [`revgeo`](https://github.com/mhudecheck/revgeo):

> **revgeo::revgeo**: Enables the use of the Photon geocoder for OpenStreetMap, Google Maps, and Bing to reverse geocode coordinate pairs. Photon allows for unlimited geocode queries, while Google Maps and Bing provide a little more information for 'out of the way' locations. Google Maps and Bing require an API key, and Google Maps limits users to 2,500 free queries a day.

I will test if this package and the freely available `Photon API` is sufficient for my purpose.

### Singer dataset:

The singer dataset can be loaded and installed as follows:


```r
## install singer
# install.packages("devtools")
# devtools::install_github("JoeyBernhardt/singer")
## load singer
```

Let's assess first what the singer dataset contains, before starting to manipulate it:


```r
glimpse(singer_locations)
```

```
## Observations: 10,100
## Variables: 14
## $ track_id           <chr> "TRWICRA128F42368DB", "TRXJANY128F42246FC",...
## $ title              <chr> "The Conversation (Cd)", "Lonely Island", "...
## $ song_id            <chr> "SOSURTI12A81C22FB8", "SODESQP12A6D4F98EF",...
## $ release            <chr> "Even If It Kills Me", "The Duke Of Earl", ...
## $ artist_id          <chr> "ARACDPV1187FB58DF4", "ARYBUAO1187FB3F4EB",...
## $ artist_name        <chr> "Motion City Soundtrack", "Gene Chandler", ...
## $ year               <int> 2007, 2004, 1998, 1995, 1968, 2006, 2003, 2...
## $ duration           <dbl> 170.4485, 106.5530, 527.5947, 695.1179, 237...
## $ artist_hotttnesss  <dbl> 0.6410183, 0.3937627, 0.4306226, 0.3622792,...
## $ artist_familiarity <dbl> 0.8230522, 0.5700167, 0.5039940, 0.4773099,...
## $ latitude           <dbl> NA, 41.88415, 40.71455, NA, 42.33168, 40.99...
## $ longitude          <dbl> NA, -87.63241, -74.00712, NA, -83.04792, -7...
## $ name               <chr> NA, "Gene Chandler", "Paul Horn", NA, "Doro...
## $ city               <chr> NA, "Chicago, IL", "New York, NY", NA, "Det...
```

In order to make things a little more clear, let's only look at a subset of `singer_locations`, including the variables `artist`, `latitude`, `longitude`, `city` and for fun later on `hottness` and `familiarity`.


```r
geo_singer <- singer_locations %>% 
  select(artist = artist_name,
         latitude = latitude,
         longitude = longitude,
         city_singer = city,
         hottness = artist_hotttnesss,
         familiarity = artist_familiarity)
```

Furthermore, the task's description gives a hint that not all of the observations have latitude and longitude values. I will drop all observations with NA values in latitude and longitude, since this is the main required input for our `revgeo()` function. Additionally, I will remove artists that can be found twice in the dataset, since I am mainly interested in an artists hotness, not in the unreadable track ID.


```r
geo_singer_clean <- geo_singer %>% 
  distinct(artist, .keep_all = TRUE) %>% # remove duplicate artist names
  na.omit(cols=c("latitude", "longitude")) # remove all rows with NA values in lat or lon
geo_singer_clean %>% 
  head(10) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
```

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> artist </th>
   <th style="text-align:right;"> latitude </th>
   <th style="text-align:right;"> longitude </th>
   <th style="text-align:left;"> city_singer </th>
   <th style="text-align:right;"> hottness </th>
   <th style="text-align:right;"> familiarity </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Gene Chandler </td>
   <td style="text-align:right;"> 41.88415 </td>
   <td style="text-align:right;"> -87.63241 </td>
   <td style="text-align:left;"> Chicago, IL </td>
   <td style="text-align:right;"> 0.3937627 </td>
   <td style="text-align:right;"> 0.5700167 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Paul Horn </td>
   <td style="text-align:right;"> 40.71455 </td>
   <td style="text-align:right;"> -74.00712 </td>
   <td style="text-align:left;"> New York, NY </td>
   <td style="text-align:right;"> 0.4306226 </td>
   <td style="text-align:right;"> 0.5039940 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Dorothy Ashby </td>
   <td style="text-align:right;"> 42.33168 </td>
   <td style="text-align:right;"> -83.04792 </td>
   <td style="text-align:left;"> Detroit, MI </td>
   <td style="text-align:right;"> 0.4107520 </td>
   <td style="text-align:right;"> 0.5303468 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Barleyjuice </td>
   <td style="text-align:right;"> 40.99471 </td>
   <td style="text-align:right;"> -77.60454 </td>
   <td style="text-align:left;"> Pennsylvania </td>
   <td style="text-align:right;"> 0.3762635 </td>
   <td style="text-align:right;"> 0.5412950 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Madlib </td>
   <td style="text-align:right;"> 34.20034 </td>
   <td style="text-align:right;"> -119.18044 </td>
   <td style="text-align:left;"> Oxnard, CA </td>
   <td style="text-align:right;"> 0.5339732 </td>
   <td style="text-align:right;"> 0.7640263 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Seeed's Pharaoh Riddim Feat. General Degree </td>
   <td style="text-align:right;"> 50.73230 </td>
   <td style="text-align:right;"> 7.10169 </td>
   <td style="text-align:left;"> Bonn </td>
   <td style="text-align:right;"> 0.4800612 </td>
   <td style="text-align:right;"> 0.3086738 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Keali'i Reichel </td>
   <td style="text-align:right;"> 19.59009 </td>
   <td style="text-align:right;"> -155.43414 </td>
   <td style="text-align:left;"> Hawaii </td>
   <td style="text-align:right;"> 0.3640586 </td>
   <td style="text-align:right;"> 0.5649717 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Little Feat </td>
   <td style="text-align:right;"> 34.05349 </td>
   <td style="text-align:right;"> -118.24532 </td>
   <td style="text-align:left;"> Los Angeles, CA </td>
   <td style="text-align:right;"> 0.4791875 </td>
   <td style="text-align:right;"> 0.6749188 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Joan Baez </td>
   <td style="text-align:right;"> 40.57250 </td>
   <td style="text-align:right;"> -74.15400 </td>
   <td style="text-align:left;"> Staten Island, NY </td>
   <td style="text-align:right;"> 0.4552892 </td>
   <td style="text-align:right;"> 0.7322613 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 31Knots </td>
   <td style="text-align:right;"> 45.51179 </td>
   <td style="text-align:right;"> -122.67563 </td>
   <td style="text-align:left;"> Portland, OR </td>
   <td style="text-align:right;"> 0.3993194 </td>
   <td style="text-align:right;"> 0.6199906 </td>
  </tr>
</tbody>
</table>


```r
mysummary_table <- function(df) {
  tab <- data.frame(
    "rows" = nrow(df))
  return(tab)
}
```


```r
mysummary_table(geo_singer) 
```

```
##    rows
## 1 10100
```

```r
mysummary_table(geo_singer_clean)
```

```
##   rows
## 1 2988
```

After a clean-up, we have 2988 from 10100 observations left.

## Reverse geocoding with revgeo()

We can now feed in latitude and longitude coordinates to retrieve addresses using `revgeo`:


```r
# geo_singer_clean$revgeo <-
#   map2(geo_singer_clean$longitude, geo_singer_clean$latitude, revgeo)
```

Unfortunately 2988 observations or Photon requests are too many to handle on my laptlop. After the code ran for a while, my RStudio and my laptop crash. The last messages showing in the console includes, which is an indication for the code running, but taking too much processing power or temporary memory to execute all lines:

```
[1] "Getting geocode data from Photon: http://photon.komoot.de/reverse?lon=-0.12714&lat=51.50632"
[1] "Getting geocode data from Photon: http://photon.komoot.de/reverse?lon=-74.00712&lat=40.71455"
[1] "Getting geocode data from Photon: http://photon.komoot.de/reverse?lon=-1.14392&lat=52.94922"
[1] "Getting geocode data from Photon: http://photon.komoot.de/reverse?lon=-46.71173&lat=-23.6361"
[1] "Getting geocode data from Photon: http://photon.komoot.de/reverse?lon=-74.21612&lat=40.81741"
```

To avoid crashing my whole computer we have to subset the dataframe and the requests further. How about we subset the dataframe according to the hotness levels of artists. We can then do reverse geocoding and check where the hottest artists in the world live. We need to use `purrr::map_df` for processing, because `revgeo` returns a list and we would like to directly see our results in a new table later on:



```r
hot_or_not <- function(x=0, y=1){
  # filter hotness
  singer_hot_or_not <- geo_singer_clean %>% 
    filter(hottness > x & hottness < y)
  # extract revgeo cities, retreived as df
  cities <- map2_df(singer_hot_or_not$longitude, singer_hot_or_not$latitude,
               ~ revgeo(.x, .y, output='hash', item=c('city', 'state', 'country')))
  # combine dfs
  singer_city <- cbind(singer_hot_or_not, cities)
  return(singer_city)
}
```

Let's try to retrieve reverse geocodes for the hottest (>0.8) and not so hot (<0.2) artists:


```r
hottest_artists <- hot_or_not(x=0.8)
```

```
## [1] "Getting geocode data from Photon: http://photon.komoot.de/reverse?lon=-83.62758&lat=32.83968"
## [1] "Getting geocode data from Photon: http://photon.komoot.de/reverse?lon=-74.29504&lat=40.23447"
## [1] "Getting geocode data from Photon: http://photon.komoot.de/reverse?lon=-118.24532&lat=34.05349"
## [1] "Getting geocode data from Photon: http://photon.komoot.de/reverse?lon=-77.029&lat=38.8991"
## [1] "Getting geocode data from Photon: http://photon.komoot.de/reverse?lon=-59.59895&lat=13.11199"
## [1] "Getting geocode data from Photon: http://photon.komoot.de/reverse?lon=-122.42005&lat=37.77916"
## [1] "Getting geocode data from Photon: http://photon.komoot.de/reverse?lon=-59.59895&lat=13.11199"
```

```r
not_so_hot_artists <- hot_or_not(y=0.2)
```

```
## [1] "Getting geocode data from Photon: http://photon.komoot.de/reverse?lon=-0.12714&lat=51.50632"
## [1] "Getting geocode data from Photon: http://photon.komoot.de/reverse?lon=-93.19547&lat=39.12026"
## [1] "Getting geocode data from Photon: http://photon.komoot.de/reverse?lon=-74.00712&lat=40.71455"
## [1] "Getting geocode data from Photon: http://photon.komoot.de/reverse?lon=-74.72671&lat=40.14323"
## [1] "Getting geocode data from Photon: http://photon.komoot.de/reverse?lon=13.37698&lat=52.51607"
## [1] "Getting geocode data from Photon: http://photon.komoot.de/reverse?lon=-74.00712&lat=40.71455"
## [1] "Getting geocode data from Photon: http://photon.komoot.de/reverse?lon=-74.00712&lat=40.71455"
## [1] "Getting geocode data from Photon: http://photon.komoot.de/reverse?lon=-92.41952&lat=30.62981"
## [1] "Getting geocode data from Photon: http://photon.komoot.de/reverse?lon=-119.27023&lat=37.27188"
## [1] "Getting geocode data from Photon: http://photon.komoot.de/reverse?lon=-122.27302&lat=37.80506"
## [1] "Getting geocode data from Photon: http://photon.komoot.de/reverse?lon=-42.9212&lat=-22.06716"
## [1] "Getting geocode data from Photon: http://photon.komoot.de/reverse?lon=-119.27023&lat=37.27188"
## [1] "Getting geocode data from Photon: http://photon.komoot.de/reverse?lon=-78.64267&lat=35.78551"
## [1] "Getting geocode data from Photon: http://photon.komoot.de/reverse?lon=-77.029&lat=38.8991"
## [1] "Getting geocode data from Photon: http://photon.komoot.de/reverse?lon=-73.94888&lat=40.65507"
## [1] "Getting geocode data from Photon: http://photon.komoot.de/reverse?lon=-0.88132&lat=41.65173"
## [1] "Getting geocode data from Photon: http://photon.komoot.de/reverse?lon=-90.07771&lat=29.95369"
```


```r
kable(hottest_artists) %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
```

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> artist </th>
   <th style="text-align:right;"> latitude </th>
   <th style="text-align:right;"> longitude </th>
   <th style="text-align:left;"> city_singer </th>
   <th style="text-align:right;"> hottness </th>
   <th style="text-align:right;"> familiarity </th>
   <th style="text-align:left;"> city </th>
   <th style="text-align:left;"> state </th>
   <th style="text-align:left;"> country </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Jason Aldean </td>
   <td style="text-align:right;"> 32.83968 </td>
   <td style="text-align:right;"> -83.62758 </td>
   <td style="text-align:left;"> Macon, GA </td>
   <td style="text-align:right;"> 0.8492910 </td>
   <td style="text-align:right;"> 0.8069525 </td>
   <td style="text-align:left;"> Macon </td>
   <td style="text-align:left;"> Georgia </td>
   <td style="text-align:left;"> United States of America </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Bruce Springsteen </td>
   <td style="text-align:right;"> 40.23447 </td>
   <td style="text-align:right;"> -74.29504 </td>
   <td style="text-align:left;"> Freehold, NJ </td>
   <td style="text-align:right;"> 0.8075871 </td>
   <td style="text-align:right;"> 0.8236881 </td>
   <td style="text-align:left;"> City Not Found </td>
   <td style="text-align:left;"> New Jersey </td>
   <td style="text-align:left;"> United States of America </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Taylor Swift </td>
   <td style="text-align:right;"> 34.05349 </td>
   <td style="text-align:right;"> -118.24532 </td>
   <td style="text-align:left;"> California - LA </td>
   <td style="text-align:right;"> 0.8724472 </td>
   <td style="text-align:right;"> 0.8897492 </td>
   <td style="text-align:left;"> Los Angeles </td>
   <td style="text-align:left;"> California </td>
   <td style="text-align:left;"> United States of America </td>
  </tr>
  <tr>
   <td style="text-align:left;"> T.I. </td>
   <td style="text-align:right;"> 38.89910 </td>
   <td style="text-align:right;"> -77.02900 </td>
   <td style="text-align:left;"> Washington </td>
   <td style="text-align:right;"> 0.8728389 </td>
   <td style="text-align:right;"> 0.8426731 </td>
   <td style="text-align:left;"> Washington </td>
   <td style="text-align:left;"> District of Columbia </td>
   <td style="text-align:left;"> United States of America </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Rihanna </td>
   <td style="text-align:right;"> 13.11199 </td>
   <td style="text-align:right;"> -59.59895 </td>
   <td style="text-align:left;"> St Michael, Barbados </td>
   <td style="text-align:right;"> 0.9082026 </td>
   <td style="text-align:right;"> 0.9178639 </td>
   <td style="text-align:left;"> City Not Found </td>
   <td style="text-align:left;"> State Not Found </td>
   <td style="text-align:left;"> Barbados </td>
  </tr>
  <tr>
   <td style="text-align:left;"> The Killers </td>
   <td style="text-align:right;"> 37.77916 </td>
   <td style="text-align:right;"> -122.42005 </td>
   <td style="text-align:left;"> St. Louis, MO </td>
   <td style="text-align:right;"> 0.8195883 </td>
   <td style="text-align:right;"> 0.9184518 </td>
   <td style="text-align:left;"> San Francisco </td>
   <td style="text-align:left;"> California </td>
   <td style="text-align:left;"> United States of America </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Rihanna / Slash </td>
   <td style="text-align:right;"> 13.11199 </td>
   <td style="text-align:right;"> -59.59895 </td>
   <td style="text-align:left;"> St Michael, Barbados </td>
   <td style="text-align:right;"> 0.9082026 </td>
   <td style="text-align:right;"> 0.9178639 </td>
   <td style="text-align:left;"> City Not Found </td>
   <td style="text-align:left;"> State Not Found </td>
   <td style="text-align:left;"> Barbados </td>
  </tr>
</tbody>
</table>



```r
kable(not_so_hot_artists) %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
```

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> artist </th>
   <th style="text-align:right;"> latitude </th>
   <th style="text-align:right;"> longitude </th>
   <th style="text-align:left;"> city_singer </th>
   <th style="text-align:right;"> hottness </th>
   <th style="text-align:right;"> familiarity </th>
   <th style="text-align:left;"> city </th>
   <th style="text-align:left;"> state </th>
   <th style="text-align:left;"> country </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Chris Smither </td>
   <td style="text-align:right;"> 51.50632 </td>
   <td style="text-align:right;"> -0.12714 </td>
   <td style="text-align:left;"> London, England </td>
   <td style="text-align:right;"> 0.1289231 </td>
   <td style="text-align:right;"> 0.5996790 </td>
   <td style="text-align:left;"> London </td>
   <td style="text-align:left;"> England </td>
   <td style="text-align:left;"> United Kingdom </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Bob James And Earl Klugh </td>
   <td style="text-align:right;"> 39.12026 </td>
   <td style="text-align:right;"> -93.19547 </td>
   <td style="text-align:left;"> Marshall, MO </td>
   <td style="text-align:right;"> 0.1434462 </td>
   <td style="text-align:right;"> 0.3038551 </td>
   <td style="text-align:left;"> Marshall </td>
   <td style="text-align:left;"> Missouri </td>
   <td style="text-align:left;"> United States of America </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Glampire </td>
   <td style="text-align:right;"> 40.71455 </td>
   <td style="text-align:right;"> -74.00712 </td>
   <td style="text-align:left;"> NY - New York City </td>
   <td style="text-align:right;"> 0.1825540 </td>
   <td style="text-align:right;"> 0.4150899 </td>
   <td style="text-align:left;"> New York City </td>
   <td style="text-align:left;"> New York </td>
   <td style="text-align:left;"> United States of America </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Joe Zangie </td>
   <td style="text-align:right;"> 40.14323 </td>
   <td style="text-align:right;"> -74.72671 </td>
   <td style="text-align:left;"> New Jersey </td>
   <td style="text-align:right;"> 0.1565355 </td>
   <td style="text-align:right;"> 0.4616134 </td>
   <td style="text-align:left;"> Fieldsboro </td>
   <td style="text-align:left;"> New Jersey </td>
   <td style="text-align:left;"> United States of America </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Empty Trash </td>
   <td style="text-align:right;"> 52.51607 </td>
   <td style="text-align:right;"> 13.37698 </td>
   <td style="text-align:left;"> Berlin </td>
   <td style="text-align:right;"> 0.1908414 </td>
   <td style="text-align:right;"> 0.6001140 </td>
   <td style="text-align:left;"> Berlin </td>
   <td style="text-align:left;"> Berlin </td>
   <td style="text-align:left;"> Germany </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mani Spinx </td>
   <td style="text-align:right;"> 40.71455 </td>
   <td style="text-align:right;"> -74.00712 </td>
   <td style="text-align:left;"> New York </td>
   <td style="text-align:right;"> 0.1758856 </td>
   <td style="text-align:right;"> 0.5127965 </td>
   <td style="text-align:left;"> New York City </td>
   <td style="text-align:left;"> New York </td>
   <td style="text-align:left;"> United States of America </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hazmat Modine </td>
   <td style="text-align:right;"> 40.71455 </td>
   <td style="text-align:right;"> -74.00712 </td>
   <td style="text-align:left;"> NY - New York City </td>
   <td style="text-align:right;"> 0.1727403 </td>
   <td style="text-align:right;"> 0.5246648 </td>
   <td style="text-align:left;"> New York City </td>
   <td style="text-align:left;"> New York </td>
   <td style="text-align:left;"> United States of America </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Steve Riley &amp; The Mamou Playboys </td>
   <td style="text-align:right;"> 30.62981 </td>
   <td style="text-align:right;"> -92.41952 </td>
   <td style="text-align:left;"> Mamou, LA </td>
   <td style="text-align:right;"> 0.1831825 </td>
   <td style="text-align:right;"> 0.3919282 </td>
   <td style="text-align:left;"> Mamou </td>
   <td style="text-align:left;"> Louisiana </td>
   <td style="text-align:left;"> United States of America </td>
  </tr>
  <tr>
   <td style="text-align:left;"> David &amp; Steve Gordon </td>
   <td style="text-align:right;"> 37.27188 </td>
   <td style="text-align:right;"> -119.27023 </td>
   <td style="text-align:left;"> California </td>
   <td style="text-align:right;"> 0.1876763 </td>
   <td style="text-align:right;"> 0.3872426 </td>
   <td style="text-align:left;"> City Not Found </td>
   <td style="text-align:left;"> California </td>
   <td style="text-align:left;"> United States of America </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Russel Garcia </td>
   <td style="text-align:right;"> 37.80506 </td>
   <td style="text-align:right;"> -122.27302 </td>
   <td style="text-align:left;"> Oakland, CA </td>
   <td style="text-align:right;"> 0.1857795 </td>
   <td style="text-align:right;"> 0.3715440 </td>
   <td style="text-align:left;"> Oakland </td>
   <td style="text-align:left;"> California </td>
   <td style="text-align:left;"> United States of America </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Novos Baianos </td>
   <td style="text-align:right;"> -22.06716 </td>
   <td style="text-align:right;"> -42.92120 </td>
   <td style="text-align:left;"> RJ, BR </td>
   <td style="text-align:right;"> 0.0142683 </td>
   <td style="text-align:right;"> 0.6001378 </td>
   <td style="text-align:left;"> City Not Found </td>
   <td style="text-align:left;"> State Not Found </td>
   <td style="text-align:left;"> Country Not Found </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Graham Central Station </td>
   <td style="text-align:right;"> 37.27188 </td>
   <td style="text-align:right;"> -119.27023 </td>
   <td style="text-align:left;"> California </td>
   <td style="text-align:right;"> 0.1451572 </td>
   <td style="text-align:right;"> 0.4887419 </td>
   <td style="text-align:left;"> City Not Found </td>
   <td style="text-align:left;"> California </td>
   <td style="text-align:left;"> United States of America </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Dog Faced Gods </td>
   <td style="text-align:right;"> 35.78551 </td>
   <td style="text-align:right;"> -78.64267 </td>
   <td style="text-align:left;"> Raleigh North Carolina USA </td>
   <td style="text-align:right;"> 0.1884867 </td>
   <td style="text-align:right;"> 0.3441308 </td>
   <td style="text-align:left;"> Raleigh </td>
   <td style="text-align:left;"> North Carolina </td>
   <td style="text-align:left;"> United States of America </td>
  </tr>
  <tr>
   <td style="text-align:left;"> The Gena Rowlands Band </td>
   <td style="text-align:right;"> 38.89910 </td>
   <td style="text-align:right;"> -77.02900 </td>
   <td style="text-align:left;"> Washington D.C. </td>
   <td style="text-align:right;"> 0.1816108 </td>
   <td style="text-align:right;"> 0.4640791 </td>
   <td style="text-align:left;"> Washington </td>
   <td style="text-align:left;"> District of Columbia </td>
   <td style="text-align:left;"> United States of America </td>
  </tr>
  <tr>
   <td style="text-align:left;"> All About Chad </td>
   <td style="text-align:right;"> 40.65507 </td>
   <td style="text-align:right;"> -73.94888 </td>
   <td style="text-align:left;"> Brooklyn, NY </td>
   <td style="text-align:right;"> 0.1810007 </td>
   <td style="text-align:right;"> 0.3247319 </td>
   <td style="text-align:left;"> New York City </td>
   <td style="text-align:left;"> New York </td>
   <td style="text-align:left;"> United States of America </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Grossomodo </td>
   <td style="text-align:right;"> 41.65173 </td>
   <td style="text-align:right;"> -0.88132 </td>
   <td style="text-align:left;"> Zaragoza </td>
   <td style="text-align:right;"> 0.1697319 </td>
   <td style="text-align:right;"> 0.4388982 </td>
   <td style="text-align:left;"> Zaragoza </td>
   <td style="text-align:left;"> Aragon </td>
   <td style="text-align:left;"> Spain </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Steady Mobb'n </td>
   <td style="text-align:right;"> 29.95369 </td>
   <td style="text-align:right;"> -90.07771 </td>
   <td style="text-align:left;"> New Orleans, LA </td>
   <td style="text-align:right;"> 0.1873645 </td>
   <td style="text-align:right;"> 0.4706478 </td>
   <td style="text-align:left;"> New Orleans </td>
   <td style="text-align:left;"> Louisiana </td>
   <td style="text-align:left;"> United States of America </td>
  </tr>
</tbody>
</table>

However, sometimes the `revgeo` package has trouble identifying a city at all and only returns a not found statement. 

## Performance of `revgeo()`

Let's try and find out whether the original `city` values correspond to the retrieved information using the `not_so_hot_artists`.

The main problems here are, first, that our original city column contains information in different formats. "NY - New York City" for example is differently formatted to "Marshall, MO". Second, that our retrieved information is separated in diferent columns like `city` and `state`.

We will therefore:

1. Transform state names into their full names in `singer_city`,
2. unite city and state names into one column for `revgeo_city`
3. and try to find at least one match intersecting word in both, the original and revgeo created city names.


```r
# defined replacement values
replacements <- c("NY" = "New York", "CA" = "California", "LA" = "Louisiana", "MO" = "Missouri", "D.C." = "District Of Columbia")

# replace abbreviations by full name
singer_city <- not_so_hot_artists$city_singer %>%
  map(str_replace_all, replacements) %>% 
  str_split(pattern = boundary("word"))

# combine `city` and `state` in one column
revgeo_city <- not_so_hot_artists %>%
  unite("revgeo_city", c("city", "state"), sep=" ") %>% 
  select("revgeo_city") %>% 
  str_split(pattern = boundary("word"))
```

```
## Warning in stri_split_boundaries(string, n = n, simplify = simplify,
## opts_brkiter = opts(pattern)): argument is not an atomic vector; coercing
```

```r
## find intersection and check if there is at least one match
correct <- map2(singer_city, revgeo_city, ~intersect(.x, .y)) %>% 
  map(function(l) {
    return(length(l) >= 1)
  })

# show results
cbind(not_so_hot_artists$city_singer, not_so_hot_artists$city, not_so_hot_artists$state, correct) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
```

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">  </th>
   <th style="text-align:left;">  </th>
   <th style="text-align:left;">  </th>
   <th style="text-align:left;"> correct </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> London, England </td>
   <td style="text-align:left;"> London </td>
   <td style="text-align:left;"> England </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Marshall, MO </td>
   <td style="text-align:left;"> Marshall </td>
   <td style="text-align:left;"> Missouri </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NY - New York City </td>
   <td style="text-align:left;"> New York City </td>
   <td style="text-align:left;"> New York </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> New Jersey </td>
   <td style="text-align:left;"> Fieldsboro </td>
   <td style="text-align:left;"> New Jersey </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Berlin </td>
   <td style="text-align:left;"> Berlin </td>
   <td style="text-align:left;"> Berlin </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> New York </td>
   <td style="text-align:left;"> New York City </td>
   <td style="text-align:left;"> New York </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NY - New York City </td>
   <td style="text-align:left;"> New York City </td>
   <td style="text-align:left;"> New York </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mamou, LA </td>
   <td style="text-align:left;"> Mamou </td>
   <td style="text-align:left;"> Louisiana </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> California </td>
   <td style="text-align:left;"> City Not Found </td>
   <td style="text-align:left;"> California </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Oakland, CA </td>
   <td style="text-align:left;"> Oakland </td>
   <td style="text-align:left;"> California </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RJ, BR </td>
   <td style="text-align:left;"> City Not Found </td>
   <td style="text-align:left;"> State Not Found </td>
   <td style="text-align:left;"> FALSE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> California </td>
   <td style="text-align:left;"> City Not Found </td>
   <td style="text-align:left;"> California </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Raleigh North Carolina USA </td>
   <td style="text-align:left;"> Raleigh </td>
   <td style="text-align:left;"> North Carolina </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Washington D.C. </td>
   <td style="text-align:left;"> Washington </td>
   <td style="text-align:left;"> District of Columbia </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Brooklyn, NY </td>
   <td style="text-align:left;"> New York City </td>
   <td style="text-align:left;"> New York </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Zaragoza </td>
   <td style="text-align:left;"> Zaragoza </td>
   <td style="text-align:left;"> Aragon </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> New Orleans, LA </td>
   <td style="text-align:left;"> New Orleans </td>
   <td style="text-align:left;"> Louisiana </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
</tbody>
</table>

We can see that all names matched, except of one that wasnot found.


## Visualization

We can use `leaflet` to visualize our results. 

Resources:

- [Leaflet for R tutorial](https://rstudio.github.io/leaflet/)


```r
map_hottest_artists <- hottest_artists %>%  
  leaflet()  %>%   
  addTiles() %>% 
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>% # choose mapbackground
  addMarkers(popup = ~artist, label = ~hottness) %>% # add a marker
  addMiniMap() # add an overview map
```

```
## Assuming "longitude" and "latitude" are longitude and latitude, respectively
```

```r
map_hottest_artists
```

<!--html_preserve--><div id="htmlwidget-e8983c63ee58c9f42c8a" style="width:672px;height:480px;" class="leaflet html-widget"></div>
<script type="application/json" data-for="htmlwidget-e8983c63ee58c9f42c8a">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addTiles","args":["//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap<\/a> contributors, <a href=\"http://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA<\/a>"}]},{"method":"addProviderTiles","args":["Esri.NatGeoWorldMap",null,null,{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addMarkers","args":[[32.83968,40.23447,34.05349,38.8991,13.11199,37.77916,13.11199],[-83.62758,-74.29504,-118.24532,-77.029,-59.59895,-122.42005,-59.59895],null,null,null,{"interactive":true,"draggable":false,"keyboard":true,"title":"","alt":"","zIndexOffset":0,"opacity":1,"riseOnHover":false,"riseOffset":250},["Jason Aldean","Bruce Springsteen","Taylor Swift","T.I.","Rihanna","The Killers","Rihanna / Slash"],null,null,null,[0.849291004465,0.807587096405,0.872447223164,0.872838920624,0.908202619208,0.819588282229,0.908202619208],{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]},{"method":"addMiniMap","args":[null,null,"bottomright",150,150,19,19,-5,false,false,false,false,false,false,{"color":"#ff7800","weight":1,"clickable":false},{"color":"#000000","weight":1,"clickable":false,"opacity":0,"fillOpacity":0},{"hideText":"Hide MiniMap","showText":"Show MiniMap"},[]]}],"limits":{"lat":[13.11199,40.23447],"lng":[-122.42005,-59.59895]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


## Reflection

Due to the format in which `revgeo` returns the downloaded data, I had difficulties to add the `possibly()` check to `hot_or_not()` function. First, I had to come up with a workaround to add the retrieved `revgeo` data found in a list in a new column. Using `map2_df` and `cbind` helped in unpacking the nested list and binding the two dataframes. However, this trick in the end hindered me in using `possibly()` to accound for erroneous reverse geocodes. `possibly()` is used defining the parameter `otherwise`. A default value to use when an error occurs, this however did not play out well with me generating a dataframe with three columns. Therefore the use of possibly needs more time to be integrated in my function.



# Exercise 1: Character data adn Regular Expressions

This task is quite long and contains a lot of detail.
I would like to point out that I used 

* the `testthat` package to check certain issues and questions and
* that I wrote a couple of functions allowing ti iterate more quickly through the exercises.


### Task
Read and work the exercises in the [Strings chapter](https://r4ds.had.co.nz/strings.html) or R for Data Science.


## 14.2.5 Exercises: String basics

### **In code that doesn't use stringr, you'll often see `paste()` and `paste0()`. What's the difference between the two functions? What stringr function are they equivalent to? How do the functions differ in their handling of NA?**

Sinply check the documentation for both functions typing `?*function*` in the console.

> `paste()` and `paste0()`: Concatenate vectors after converting to character.

* `paste (..., sep = " ", collapse = NULL)`

* `paste0(..., collapse = NULL)`


```r
paste("Stefanie", "Lumnitz")
```

```
## [1] "Stefanie Lumnitz"
```

```r
paste0("Stefanie", "Lumnitz")
```

```
## [1] "StefanieLumnitz"
```

The function `paste()` separates strings by spaces by default, while `paste0()` does not separate strings with spaces by default. Otherwise they are equivalent.
  
They are equivalent to the `str_c` function.


```r
str_c("Stefanie", "Lumnitz")
```

```
## [1] "StefanieLumnitz"
```

`str_c()` does not separate strings with spaces by default either. It's default is closer to `paste0`

* `str_c(..., sep = "", collapse = NULL)`

Let's check how they handle NA values:


```r
str_c("Stefanie", NA)
```

```
## [1] NA
```

```r
paste("Stefanie", NA)
```

```
## [1] "Stefanie NA"
```

```r
paste0("Stefanie", NA)
```

```
## [1] "StefanieNA"
```

Both functions handle NA values differently. `str_c` propagates NA. If any value is missing, it returns a missing value. `paste` on the other hand converts NA into a string `"NA"` and treats it like any other character vector.


### **In your own words, describe the difference between the sep and collapse arguments to str_c().**


```r
name <- c("Stefanie", "Lumnitz")
info <- c("first", "last")
str_c(name, info, sep= "")
```

```
## [1] "Stefaniefirst" "Lumnitzlast"
```

```r
str_c(name, info, sep= "_", collapse = " ")
```

```
## [1] "Stefanie_first Lumnitz_last"
```

`sep` is the argument to use to insert a string between arguments `'_'`. If `collapse` is used, it is the string that separates element pairs of two or more input vectors and all input vectors are merged to a character vector of lenth 1. 


### **Use str_length() and str_sub() to extract the middle character from a string. What will you do if the string has an even number of characters?**

> `str_length()` returns the number of "code points", in a string. One code point usually corresponds to one character, but not always. For example, an u with a umlaut might be represented as a single character or as the combination a u and an umlaut. (Since I am German an umlaut is important in my language and should not be neglected)

> `str_sub` will recycle all arguments to be the same length as the longest argument. If any arguments are of length 0, the output will be a zero length character vector.

Option one extracts the middle character and the first character to the left of the middle in case our string is uneven:


```r
numbers <- c("1", "123", "1234", "12345", "123456")

length <- str_length(numbers)
left <- ceiling(length / 2)
str_sub(numbers, left, left)
```

```
## [1] "1" "2" "2" "3" "3"
```

Option two extracts the middle character and the first character to the right of the middle in case our string is uneven:


```r
length <- str_length(numbers)
right <- floor(length / 2)
str_sub(numbers, right, right)
```

```
## [1] ""  "1" "2" "2" "3"
```

We can see that option one is the method that always returns a character. DUe to the use of `floor()` option two misses a character if the input character has length 1.

We can also make a function out of this:


```r
middle <- function(x, left_or_right=left){
  length <- str_length(x)
  right <- floor(length / 2)
  left <- ceiling(length / 2)
  middle_str <- str_sub(x, left_or_right, left_or_right)
}
```


```r
(middle(numbers, left_or_right=right))
```

```
## [1] ""  "1" "2" "2" "3"
```

### **What does `str_wrap()` do? When might you want to use it?**

> str_wrap(string, width = 80, indent = 0, exdent = 0)

We can use `str_wrap()` to fit text within a certain width. This is for example useful in order to process ones R package documentation according to [Google's R styleguide](https://google.github.io/styleguide/Rguide.xml#linelength). 

> The maximum line length is 80 characters.


```r
long_documentation <- "long() is a wrapper of stats::reshape() that takes the data from a wide format to a long format. It can also handle unbalanced data (where some measures have different number of time points)."
cat(long_documentation)
```

```
## long() is a wrapper of stats::reshape() that takes the data from a wide format to a long format. It can also handle unbalanced data (where some measures have different number of time points).
```

```r
cat(str_wrap(long_documentation, width=80), sep = "\n")
```

```
## long() is a wrapper of stats::reshape() that takes the data from a wide format
## to a long format. It can also handle unbalanced data (where some measures have
## different number of time points).
```


### **What does str_trim() do? What’s the opposite of str_trim()?**

The function `str_trim()` delets starting or/and trailing whitespaces from a string.


```r
string_with_spaces = "  Hello    World!  "
str_trim(string_with_spaces)
```

```
## [1] "Hello    World!"
```

```r
str_trim(string_with_spaces, side = "left")
```

```
## [1] "Hello    World!  "
```

```r
str_trim(string_with_spaces, side = "right")
```

```
## [1] "  Hello    World!"
```

`str_squish()` also reduces repeated whitespace inside a string.


```r
str_squish(string_with_spaces)
```

```
## [1] "Hello World!"
```

`str_pad()` adds whitespaces and is the opposite of `str_trim()`.


```r
str_pad(string_with_spaces, 5, side="both")
```

```
## [1] "  Hello    World!  "
```


### **Write a function that turns (e.g.) a vector c("a", "b", "c") into the string a, b, and c. Think carefully about what it should do if given a vector of length 0, 1, or 2.**

If the final strings are a, b and c our function needs to handle four issues:

1. *length of vector is 0*: we return an empty string (""); 
2. *length of vector is 1*: we return the original vector ("a");
3. *length of vector is 2*: we return both elements separated by an "and" ("a and b").
4. *length of vector is >2*: we return all but the last element separated by commas and the last element separated by an "and" ("a, b and c").

Let's get started:


```r
vector_to_string <- function(x) {
  l <- length(x)
  if (l == 0) {
    ""
  } else if (l == 1) {
    x
  } else if (l == 2) {
    str_c(x[[1]], "and", x[[2]], sep = " ")
  } else {
    first_chr <- str_c(x[seq_len(l-1)], collapse = ", ") # handle all but last
    new_string <- str_c(first_chr, x[length(x)], sep = " and ")
  }
}
```

Let's use `testthat` package to test our function.


```r
suppressPackageStartupMessages(library(testthat))
```



```r
v0 <- c()
v1 <- c("a")
v2 <- c("a", "b")
v4 <- c ("a", "b", "c", "d")

test_that("function vector_to_string() is wrong", {
  expect_equal(vector_to_string(v0), "")
  expect_equal(vector_to_string(v1), "a")
  expect_equal(vector_to_string(v2), "a and b")
  expect_equal(vector_to_string(v4), "a, b, c and d")
})

(vector_to_string(v4))
```

```
## [1] "a, b, c and d"
```

All the above tests have passed.




## 14.3.1.1 Exercises: Basic matches

### **Explain why each of these strings don’t match a \: "\", "\\", "\\\".**

* "\" will escape the next character in an R string. Escape character meaning we are undoing the "special" meaning of what follows after. E.g. if we want to match a dot, we need to use `\.` since a simple `.` has the special meaning to match a character.

*  "\\" will resolve to \ in the regular expression. This will escape the next character in the regular expression.

* "\\\" will escape an escaped character, since the first two backslashes resolve to a literal backslash in the regular expression and the third will escape the next character.

> To match a literal `\` we need four backslashes `\\\\`!


### **How would you match the sequence "'\\?**

Since "'" and "\\" need to be escaped in regex, we need to use "\\'\\\\" to match it.


```r
str_view("\"'\\", "\"'\\\\")
```

<!--html_preserve--><div id="htmlwidget-de8035b5fdaea7d34d2a" style="width:960px;height:100%;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-de8035b5fdaea7d34d2a">{"x":{"html":"<ul>\n  <li><span class='match'>\"'\\<\/span><\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


### **What patterns will the regular expression \\..\\..\\.. match? How would you represent it as a string?**

Regex "\\..\\..\\.." will match any pattern that is a dot followed by any character, repeated three times. Here is an example, "a", "b", or "c" can be any other characters.


```r
test_pattern <- c(".a.b.c", ".a.b", ".....")
str_view(test_pattern, c("\\..\\..\\.."))
```

<!--html_preserve--><div id="htmlwidget-3180a1355d1cc1844ac8" style="width:960px;height:100%;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-3180a1355d1cc1844ac8">{"x":{"html":"<ul>\n  <li><span class='match'>.a.b.c<\/span><\/li>\n  <li>.a.b<\/li>\n  <li>.....<\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->



## 14.3.2.1 Exercises: Anchors

### **How would you match the literal string "\$^\$"?**

We need to escape "\$" and "^" in regex, "\\\\\$\\\\^\\\\\$" is a suitable pattern.


```r
test_pattern <- c("$^$", "ab$^$sfas")
str_view(test_pattern, pattern = "^\\$\\^\\$$")
```

<!--html_preserve--><div id="htmlwidget-d738bb2f0b388917a28c" style="width:960px;height:100%;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-d738bb2f0b388917a28c">{"x":{"html":"<ul>\n  <li><span class='match'>$^$<\/span><\/li>\n  <li>ab$^$sfas<\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

### **Given the corpus of common words in stringr::words, create regular expressions that find all words that:**

**Start with “y”.**


```r
str_view(words, pattern = "^y", match = TRUE)
```

<!--html_preserve--><div id="htmlwidget-9ee4eb58781121ca0002" style="width:960px;height:100%;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-9ee4eb58781121ca0002">{"x":{"html":"<ul>\n  <li><span class='match'>y<\/span>ear<\/li>\n  <li><span class='match'>y<\/span>es<\/li>\n  <li><span class='match'>y<\/span>esterday<\/li>\n  <li><span class='match'>y<\/span>et<\/li>\n  <li><span class='match'>y<\/span>ou<\/li>\n  <li><span class='match'>y<\/span>oung<\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


**End with “x”**


```r
str_view(words, "x$", match = TRUE)
```

<!--html_preserve--><div id="htmlwidget-1698a70c8456973803bc" style="width:960px;height:100%;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-1698a70c8456973803bc">{"x":{"html":"<ul>\n  <li>bo<span class='match'>x<\/span><\/li>\n  <li>se<span class='match'>x<\/span><\/li>\n  <li>si<span class='match'>x<\/span><\/li>\n  <li>ta<span class='match'>x<\/span><\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

**Are exactly three letters long. (Don’t cheat by using str_length()!)**


```r
str_view(words, "^...$", match = TRUE)
```

<!--html_preserve--><div id="htmlwidget-d4c2fadefd19a670d097" style="width:960px;height:100%;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-d4c2fadefd19a670d097">{"x":{"html":"<ul>\n  <li><span class='match'>act<\/span><\/li>\n  <li><span class='match'>add<\/span><\/li>\n  <li><span class='match'>age<\/span><\/li>\n  <li><span class='match'>ago<\/span><\/li>\n  <li><span class='match'>air<\/span><\/li>\n  <li><span class='match'>all<\/span><\/li>\n  <li><span class='match'>and<\/span><\/li>\n  <li><span class='match'>any<\/span><\/li>\n  <li><span class='match'>arm<\/span><\/li>\n  <li><span class='match'>art<\/span><\/li>\n  <li><span class='match'>ask<\/span><\/li>\n  <li><span class='match'>bad<\/span><\/li>\n  <li><span class='match'>bag<\/span><\/li>\n  <li><span class='match'>bar<\/span><\/li>\n  <li><span class='match'>bed<\/span><\/li>\n  <li><span class='match'>bet<\/span><\/li>\n  <li><span class='match'>big<\/span><\/li>\n  <li><span class='match'>bit<\/span><\/li>\n  <li><span class='match'>box<\/span><\/li>\n  <li><span class='match'>boy<\/span><\/li>\n  <li><span class='match'>bus<\/span><\/li>\n  <li><span class='match'>but<\/span><\/li>\n  <li><span class='match'>buy<\/span><\/li>\n  <li><span class='match'>can<\/span><\/li>\n  <li><span class='match'>car<\/span><\/li>\n  <li><span class='match'>cat<\/span><\/li>\n  <li><span class='match'>cup<\/span><\/li>\n  <li><span class='match'>cut<\/span><\/li>\n  <li><span class='match'>dad<\/span><\/li>\n  <li><span class='match'>day<\/span><\/li>\n  <li><span class='match'>die<\/span><\/li>\n  <li><span class='match'>dog<\/span><\/li>\n  <li><span class='match'>dry<\/span><\/li>\n  <li><span class='match'>due<\/span><\/li>\n  <li><span class='match'>eat<\/span><\/li>\n  <li><span class='match'>egg<\/span><\/li>\n  <li><span class='match'>end<\/span><\/li>\n  <li><span class='match'>eye<\/span><\/li>\n  <li><span class='match'>far<\/span><\/li>\n  <li><span class='match'>few<\/span><\/li>\n  <li><span class='match'>fit<\/span><\/li>\n  <li><span class='match'>fly<\/span><\/li>\n  <li><span class='match'>for<\/span><\/li>\n  <li><span class='match'>fun<\/span><\/li>\n  <li><span class='match'>gas<\/span><\/li>\n  <li><span class='match'>get<\/span><\/li>\n  <li><span class='match'>god<\/span><\/li>\n  <li><span class='match'>guy<\/span><\/li>\n  <li><span class='match'>hit<\/span><\/li>\n  <li><span class='match'>hot<\/span><\/li>\n  <li><span class='match'>how<\/span><\/li>\n  <li><span class='match'>job<\/span><\/li>\n  <li><span class='match'>key<\/span><\/li>\n  <li><span class='match'>kid<\/span><\/li>\n  <li><span class='match'>lad<\/span><\/li>\n  <li><span class='match'>law<\/span><\/li>\n  <li><span class='match'>lay<\/span><\/li>\n  <li><span class='match'>leg<\/span><\/li>\n  <li><span class='match'>let<\/span><\/li>\n  <li><span class='match'>lie<\/span><\/li>\n  <li><span class='match'>lot<\/span><\/li>\n  <li><span class='match'>low<\/span><\/li>\n  <li><span class='match'>man<\/span><\/li>\n  <li><span class='match'>may<\/span><\/li>\n  <li><span class='match'>mrs<\/span><\/li>\n  <li><span class='match'>new<\/span><\/li>\n  <li><span class='match'>non<\/span><\/li>\n  <li><span class='match'>not<\/span><\/li>\n  <li><span class='match'>now<\/span><\/li>\n  <li><span class='match'>odd<\/span><\/li>\n  <li><span class='match'>off<\/span><\/li>\n  <li><span class='match'>old<\/span><\/li>\n  <li><span class='match'>one<\/span><\/li>\n  <li><span class='match'>out<\/span><\/li>\n  <li><span class='match'>own<\/span><\/li>\n  <li><span class='match'>pay<\/span><\/li>\n  <li><span class='match'>per<\/span><\/li>\n  <li><span class='match'>put<\/span><\/li>\n  <li><span class='match'>red<\/span><\/li>\n  <li><span class='match'>rid<\/span><\/li>\n  <li><span class='match'>run<\/span><\/li>\n  <li><span class='match'>say<\/span><\/li>\n  <li><span class='match'>see<\/span><\/li>\n  <li><span class='match'>set<\/span><\/li>\n  <li><span class='match'>sex<\/span><\/li>\n  <li><span class='match'>she<\/span><\/li>\n  <li><span class='match'>sir<\/span><\/li>\n  <li><span class='match'>sit<\/span><\/li>\n  <li><span class='match'>six<\/span><\/li>\n  <li><span class='match'>son<\/span><\/li>\n  <li><span class='match'>sun<\/span><\/li>\n  <li><span class='match'>tax<\/span><\/li>\n  <li><span class='match'>tea<\/span><\/li>\n  <li><span class='match'>ten<\/span><\/li>\n  <li><span class='match'>the<\/span><\/li>\n  <li><span class='match'>tie<\/span><\/li>\n  <li><span class='match'>too<\/span><\/li>\n  <li><span class='match'>top<\/span><\/li>\n  <li><span class='match'>try<\/span><\/li>\n  <li><span class='match'>two<\/span><\/li>\n  <li><span class='match'>use<\/span><\/li>\n  <li><span class='match'>war<\/span><\/li>\n  <li><span class='match'>way<\/span><\/li>\n  <li><span class='match'>wee<\/span><\/li>\n  <li><span class='match'>who<\/span><\/li>\n  <li><span class='match'>why<\/span><\/li>\n  <li><span class='match'>win<\/span><\/li>\n  <li><span class='match'>yes<\/span><\/li>\n  <li><span class='match'>yet<\/span><\/li>\n  <li><span class='match'>you<\/span><\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

**Have seven letters or more.**


```r
str_view(stringr::words, ".......", match = TRUE)
```

<!--html_preserve--><div id="htmlwidget-62c74fa67f836de9199e" style="width:960px;height:100%;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-62c74fa67f836de9199e">{"x":{"html":"<ul>\n  <li><span class='match'>absolut<\/span>e<\/li>\n  <li><span class='match'>account<\/span><\/li>\n  <li><span class='match'>achieve<\/span><\/li>\n  <li><span class='match'>address<\/span><\/li>\n  <li><span class='match'>adverti<\/span>se<\/li>\n  <li><span class='match'>afterno<\/span>on<\/li>\n  <li><span class='match'>against<\/span><\/li>\n  <li><span class='match'>already<\/span><\/li>\n  <li><span class='match'>alright<\/span><\/li>\n  <li><span class='match'>althoug<\/span>h<\/li>\n  <li><span class='match'>america<\/span><\/li>\n  <li><span class='match'>another<\/span><\/li>\n  <li><span class='match'>apparen<\/span>t<\/li>\n  <li><span class='match'>appoint<\/span><\/li>\n  <li><span class='match'>approac<\/span>h<\/li>\n  <li><span class='match'>appropr<\/span>iate<\/li>\n  <li><span class='match'>arrange<\/span><\/li>\n  <li><span class='match'>associa<\/span>te<\/li>\n  <li><span class='match'>authori<\/span>ty<\/li>\n  <li><span class='match'>availab<\/span>le<\/li>\n  <li><span class='match'>balance<\/span><\/li>\n  <li><span class='match'>because<\/span><\/li>\n  <li><span class='match'>believe<\/span><\/li>\n  <li><span class='match'>benefit<\/span><\/li>\n  <li><span class='match'>between<\/span><\/li>\n  <li><span class='match'>brillia<\/span>nt<\/li>\n  <li><span class='match'>britain<\/span><\/li>\n  <li><span class='match'>brother<\/span><\/li>\n  <li><span class='match'>busines<\/span>s<\/li>\n  <li><span class='match'>certain<\/span><\/li>\n  <li><span class='match'>chairma<\/span>n<\/li>\n  <li><span class='match'>charact<\/span>er<\/li>\n  <li><span class='match'>Christm<\/span>as<\/li>\n  <li><span class='match'>colleag<\/span>ue<\/li>\n  <li><span class='match'>collect<\/span><\/li>\n  <li><span class='match'>college<\/span><\/li>\n  <li><span class='match'>comment<\/span><\/li>\n  <li><span class='match'>committ<\/span>ee<\/li>\n  <li><span class='match'>communi<\/span>ty<\/li>\n  <li><span class='match'>company<\/span><\/li>\n  <li><span class='match'>compare<\/span><\/li>\n  <li><span class='match'>complet<\/span>e<\/li>\n  <li><span class='match'>compute<\/span><\/li>\n  <li><span class='match'>concern<\/span><\/li>\n  <li><span class='match'>conditi<\/span>on<\/li>\n  <li><span class='match'>conside<\/span>r<\/li>\n  <li><span class='match'>consult<\/span><\/li>\n  <li><span class='match'>contact<\/span><\/li>\n  <li><span class='match'>continu<\/span>e<\/li>\n  <li><span class='match'>contrac<\/span>t<\/li>\n  <li><span class='match'>control<\/span><\/li>\n  <li><span class='match'>convers<\/span>e<\/li>\n  <li><span class='match'>correct<\/span><\/li>\n  <li><span class='match'>council<\/span><\/li>\n  <li><span class='match'>country<\/span><\/li>\n  <li><span class='match'>current<\/span><\/li>\n  <li><span class='match'>decisio<\/span>n<\/li>\n  <li><span class='match'>definit<\/span>e<\/li>\n  <li><span class='match'>departm<\/span>ent<\/li>\n  <li><span class='match'>describ<\/span>e<\/li>\n  <li><span class='match'>develop<\/span><\/li>\n  <li><span class='match'>differe<\/span>nce<\/li>\n  <li><span class='match'>difficu<\/span>lt<\/li>\n  <li><span class='match'>discuss<\/span><\/li>\n  <li><span class='match'>distric<\/span>t<\/li>\n  <li><span class='match'>documen<\/span>t<\/li>\n  <li><span class='match'>economy<\/span><\/li>\n  <li><span class='match'>educate<\/span><\/li>\n  <li><span class='match'>electri<\/span>c<\/li>\n  <li><span class='match'>encoura<\/span>ge<\/li>\n  <li><span class='match'>english<\/span><\/li>\n  <li><span class='match'>environ<\/span>ment<\/li>\n  <li><span class='match'>especia<\/span>l<\/li>\n  <li><span class='match'>evening<\/span><\/li>\n  <li><span class='match'>evidenc<\/span>e<\/li>\n  <li><span class='match'>example<\/span><\/li>\n  <li><span class='match'>exercis<\/span>e<\/li>\n  <li><span class='match'>expense<\/span><\/li>\n  <li><span class='match'>experie<\/span>nce<\/li>\n  <li><span class='match'>explain<\/span><\/li>\n  <li><span class='match'>express<\/span><\/li>\n  <li><span class='match'>finance<\/span><\/li>\n  <li><span class='match'>fortune<\/span><\/li>\n  <li><span class='match'>forward<\/span><\/li>\n  <li><span class='match'>functio<\/span>n<\/li>\n  <li><span class='match'>further<\/span><\/li>\n  <li><span class='match'>general<\/span><\/li>\n  <li><span class='match'>germany<\/span><\/li>\n  <li><span class='match'>goodbye<\/span><\/li>\n  <li><span class='match'>history<\/span><\/li>\n  <li><span class='match'>holiday<\/span><\/li>\n  <li><span class='match'>hospita<\/span>l<\/li>\n  <li><span class='match'>however<\/span><\/li>\n  <li><span class='match'>hundred<\/span><\/li>\n  <li><span class='match'>husband<\/span><\/li>\n  <li><span class='match'>identif<\/span>y<\/li>\n  <li><span class='match'>imagine<\/span><\/li>\n  <li><span class='match'>importa<\/span>nt<\/li>\n  <li><span class='match'>improve<\/span><\/li>\n  <li><span class='match'>include<\/span><\/li>\n  <li><span class='match'>increas<\/span>e<\/li>\n  <li><span class='match'>individ<\/span>ual<\/li>\n  <li><span class='match'>industr<\/span>y<\/li>\n  <li><span class='match'>instead<\/span><\/li>\n  <li><span class='match'>interes<\/span>t<\/li>\n  <li><span class='match'>introdu<\/span>ce<\/li>\n  <li><span class='match'>involve<\/span><\/li>\n  <li><span class='match'>kitchen<\/span><\/li>\n  <li><span class='match'>languag<\/span>e<\/li>\n  <li><span class='match'>machine<\/span><\/li>\n  <li><span class='match'>meaning<\/span><\/li>\n  <li><span class='match'>measure<\/span><\/li>\n  <li><span class='match'>mention<\/span><\/li>\n  <li><span class='match'>million<\/span><\/li>\n  <li><span class='match'>ministe<\/span>r<\/li>\n  <li><span class='match'>morning<\/span><\/li>\n  <li><span class='match'>necessa<\/span>ry<\/li>\n  <li><span class='match'>obvious<\/span><\/li>\n  <li><span class='match'>occasio<\/span>n<\/li>\n  <li><span class='match'>operate<\/span><\/li>\n  <li><span class='match'>opportu<\/span>nity<\/li>\n  <li><span class='match'>organiz<\/span>e<\/li>\n  <li><span class='match'>origina<\/span>l<\/li>\n  <li><span class='match'>otherwi<\/span>se<\/li>\n  <li><span class='match'>paragra<\/span>ph<\/li>\n  <li><span class='match'>particu<\/span>lar<\/li>\n  <li><span class='match'>pension<\/span><\/li>\n  <li><span class='match'>percent<\/span><\/li>\n  <li><span class='match'>perfect<\/span><\/li>\n  <li><span class='match'>perhaps<\/span><\/li>\n  <li><span class='match'>photogr<\/span>aph<\/li>\n  <li><span class='match'>picture<\/span><\/li>\n  <li><span class='match'>politic<\/span><\/li>\n  <li><span class='match'>positio<\/span>n<\/li>\n  <li><span class='match'>positiv<\/span>e<\/li>\n  <li><span class='match'>possibl<\/span>e<\/li>\n  <li><span class='match'>practis<\/span>e<\/li>\n  <li><span class='match'>prepare<\/span><\/li>\n  <li><span class='match'>present<\/span><\/li>\n  <li><span class='match'>pressur<\/span>e<\/li>\n  <li><span class='match'>presume<\/span><\/li>\n  <li><span class='match'>previou<\/span>s<\/li>\n  <li><span class='match'>private<\/span><\/li>\n  <li><span class='match'>probabl<\/span>e<\/li>\n  <li><span class='match'>problem<\/span><\/li>\n  <li><span class='match'>proceed<\/span><\/li>\n  <li><span class='match'>process<\/span><\/li>\n  <li><span class='match'>produce<\/span><\/li>\n  <li><span class='match'>product<\/span><\/li>\n  <li><span class='match'>program<\/span>me<\/li>\n  <li><span class='match'>project<\/span><\/li>\n  <li><span class='match'>propose<\/span><\/li>\n  <li><span class='match'>protect<\/span><\/li>\n  <li><span class='match'>provide<\/span><\/li>\n  <li><span class='match'>purpose<\/span><\/li>\n  <li><span class='match'>quality<\/span><\/li>\n  <li><span class='match'>quarter<\/span><\/li>\n  <li><span class='match'>questio<\/span>n<\/li>\n  <li><span class='match'>realise<\/span><\/li>\n  <li><span class='match'>receive<\/span><\/li>\n  <li><span class='match'>recogni<\/span>ze<\/li>\n  <li><span class='match'>recomme<\/span>nd<\/li>\n  <li><span class='match'>relatio<\/span>n<\/li>\n  <li><span class='match'>remembe<\/span>r<\/li>\n  <li><span class='match'>represe<\/span>nt<\/li>\n  <li><span class='match'>require<\/span><\/li>\n  <li><span class='match'>researc<\/span>h<\/li>\n  <li><span class='match'>resourc<\/span>e<\/li>\n  <li><span class='match'>respect<\/span><\/li>\n  <li><span class='match'>respons<\/span>ible<\/li>\n  <li><span class='match'>saturda<\/span>y<\/li>\n  <li><span class='match'>science<\/span><\/li>\n  <li><span class='match'>scotlan<\/span>d<\/li>\n  <li><span class='match'>secreta<\/span>ry<\/li>\n  <li><span class='match'>section<\/span><\/li>\n  <li><span class='match'>separat<\/span>e<\/li>\n  <li><span class='match'>serious<\/span><\/li>\n  <li><span class='match'>service<\/span><\/li>\n  <li><span class='match'>similar<\/span><\/li>\n  <li><span class='match'>situate<\/span><\/li>\n  <li><span class='match'>society<\/span><\/li>\n  <li><span class='match'>special<\/span><\/li>\n  <li><span class='match'>specifi<\/span>c<\/li>\n  <li><span class='match'>standar<\/span>d<\/li>\n  <li><span class='match'>station<\/span><\/li>\n  <li><span class='match'>straigh<\/span>t<\/li>\n  <li><span class='match'>strateg<\/span>y<\/li>\n  <li><span class='match'>structu<\/span>re<\/li>\n  <li><span class='match'>student<\/span><\/li>\n  <li><span class='match'>subject<\/span><\/li>\n  <li><span class='match'>succeed<\/span><\/li>\n  <li><span class='match'>suggest<\/span><\/li>\n  <li><span class='match'>support<\/span><\/li>\n  <li><span class='match'>suppose<\/span><\/li>\n  <li><span class='match'>surpris<\/span>e<\/li>\n  <li><span class='match'>telepho<\/span>ne<\/li>\n  <li><span class='match'>televis<\/span>ion<\/li>\n  <li><span class='match'>terribl<\/span>e<\/li>\n  <li><span class='match'>therefo<\/span>re<\/li>\n  <li><span class='match'>thirtee<\/span>n<\/li>\n  <li><span class='match'>thousan<\/span>d<\/li>\n  <li><span class='match'>through<\/span><\/li>\n  <li><span class='match'>thursda<\/span>y<\/li>\n  <li><span class='match'>togethe<\/span>r<\/li>\n  <li><span class='match'>tomorro<\/span>w<\/li>\n  <li><span class='match'>tonight<\/span><\/li>\n  <li><span class='match'>traffic<\/span><\/li>\n  <li><span class='match'>transpo<\/span>rt<\/li>\n  <li><span class='match'>trouble<\/span><\/li>\n  <li><span class='match'>tuesday<\/span><\/li>\n  <li><span class='match'>underst<\/span>and<\/li>\n  <li><span class='match'>univers<\/span>ity<\/li>\n  <li><span class='match'>various<\/span><\/li>\n  <li><span class='match'>village<\/span><\/li>\n  <li><span class='match'>wednesd<\/span>ay<\/li>\n  <li><span class='match'>welcome<\/span><\/li>\n  <li><span class='match'>whether<\/span><\/li>\n  <li><span class='match'>without<\/span><\/li>\n  <li><span class='match'>yesterd<\/span>ay<\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


## 14.3.3.1 Exercises: Character classes and alternatives

### **Create regular expressions to find all words that:**

**Words starting with vowels**

Since the `words` library is quite long, we will create a subset fromnow on and continue all examplkes with the subset:


```r
# create subset of randomly selected words
random_words <- runif(100, 1, length(words))
subset_words <- words %>% 
  `[`(random_words) %>% 
  sort()
```



```r
str_view(subset_words, "^[aeiou]", match = TRUE)
```

<!--html_preserve--><div id="htmlwidget-98f02c1ccd32e7df66e8" style="width:960px;height:100%;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-98f02c1ccd32e7df66e8">{"x":{"html":"<ul>\n  <li><span class='match'>a<\/span>bout<\/li>\n  <li><span class='match'>a<\/span>bout<\/li>\n  <li><span class='match'>a<\/span>bsolute<\/li>\n  <li><span class='match'>a<\/span>dd<\/li>\n  <li><span class='match'>a<\/span>dd<\/li>\n  <li><span class='match'>a<\/span>pply<\/li>\n  <li><span class='match'>a<\/span>rea<\/li>\n  <li><span class='match'>e<\/span>lect<\/li>\n  <li><span class='match'>e<\/span>xample<\/li>\n  <li><span class='match'>e<\/span>ye<\/li>\n  <li><span class='match'>i<\/span>nclude<\/li>\n  <li><span class='match'>i<\/span>ndustry<\/li>\n  <li><span class='match'>i<\/span>nterest<\/li>\n  <li><span class='match'>o<\/span>perate<\/li>\n  <li><span class='match'>o<\/span>perate<\/li>\n  <li><span class='match'>o<\/span>r<\/li>\n  <li><span class='match'>o<\/span>r<\/li>\n  <li><span class='match'>o<\/span>rganize<\/li>\n  <li><span class='match'>u<\/span>se<\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

**That only contain consonants. (Hint: thinking about matching “not”-vowels.)**


```r
str_view(subset_words, "^[^aeiou]+$", match=TRUE)
```

<!--html_preserve--><div id="htmlwidget-f300b279bc0044e26eff" style="width:960px;height:100%;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-f300b279bc0044e26eff">{"x":{"html":"<ul>\n  <li><span class='match'>mrs<\/span><\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

**End with ed, but not with eed.**


```r
test_case <- c("bed", "beed")
str_view(test_case, "^ed$|[^e]ed$", match = TRUE)
```

<!--html_preserve--><div id="htmlwidget-e7f6465137d6b6691c5b" style="width:960px;height:100%;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-e7f6465137d6b6691c5b">{"x":{"html":"<ul>\n  <li><span class='match'>bed<\/span><\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

**End with ing or ise.**


```r
str_view(subset_words, "i(ng|se)$", match = TRUE)
```

<!--html_preserve--><div id="htmlwidget-df14d95483d3edb3d19c" style="width:960px;height:100%;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-df14d95483d3edb3d19c">{"x":{"html":"<ul>\n  <li>s<span class='match'>ing<\/span><\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


### **Empirically verify the rule "i before e except after c".**

To solve this question, it means we need to ensure:

- The number of "ie" and the number of "cei" are high enough.
- The number of "ei" and the number of "cie" are low enough.

Therefore, we try the following to regexes.


```r
x <- sum(str_detect(words, "(cei|[^c]ie)"))
y <- sum(str_detect(words, "(cie|[^c]ei)"))
x
```

```
## [1] 14
```

```r
y
```

```
## [1] 3
```

```r
test_that("The rule is not correct", {
  expect_more_than(as.double(x), as.double(y))
})
```

The test passes and the the rule is empirically verified.

### **Is "q" always followed by a "u"?**



```r
str_view(stringr::words, "q[^u]", match = TRUE)
```

<!--html_preserve--><div id="htmlwidget-22e33c5e3523f32c8fae" style="width:960px;height:100%;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-22e33c5e3523f32c8fae">{"x":{"html":"<ul>\n  <li><\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

The result is empty, so the statement is true for the `words` dataset.

### **Write a regular expression that matches a word if it's probably written in British English, not American English.**

SInce my mother tongue is German and I am neither an expert in British nor in American english, I will only test specific cases:

1. “ou” instead of “o” in British English
2. British English ends in ise instead of ize


```r
test_words <- c("colour", "color", "honour", "honor", "labor", "labour", "vectorize", "vectorise")

str_view(test_words, pattern = "ou|ise$", match = TRUE)
```

<!--html_preserve--><div id="htmlwidget-02595a44c73004fda906" style="width:960px;height:100%;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-02595a44c73004fda906">{"x":{"html":"<ul>\n  <li>col<span class='match'>ou<\/span>r<\/li>\n  <li>hon<span class='match'>ou<\/span>r<\/li>\n  <li>lab<span class='match'>ou<\/span>r<\/li>\n  <li>vector<span class='match'>ise<\/span><\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


### **Create a regular expression that will match telephone numbers as commonly written in your country.**

The telephone numbers in German commpnly follow the rule: "+49 111 1111111". Therefore we can use the following regex to detect it.


```r
test_telephone_numbers <- c("+1 (778) 069 2357", "+49 222 4433666", "+852 2309 9667", "+49 222 4455556")

str_view(test_telephone_numbers, pattern = "\\+49 [0-9]{3} [0-9]{7}", match=TRUE)
```

<!--html_preserve--><div id="htmlwidget-38bcc9d8171198d89f72" style="width:960px;height:100%;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-38bcc9d8171198d89f72">{"x":{"html":"<ul>\n  <li><span class='match'>+49 222 4433666<\/span><\/li>\n  <li><span class='match'>+49 222 4455556<\/span><\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->




## 14.3.4.1 Exercises: Repetition


### **Describe the equivalents of `?`, `+`, `*` in {m,n} form.**

 Pattern | {m,n} | Meaning
---------|-------|--------
 ? | {0,1} | Match at most 1
 + | {1,} | Match 1 or more
 * | {0,} | Match 0 or more


### **Describe in words what these regular expressions match: (read carefully to see if I'm using a regular expression or a string that defines a regular expression.)**

- `^.*$` matches any string.
- `"\\{.+\\}"` matches any string with curly braces surrounding at least one character.
- `\d{4}-\d{2}-\d{2}` matches a string that looks like dddd-dd-dd, where d is a digit. For example a date.
- `"\\\\{4}"` is `\\{4}` and will match four backslashes.


### **Create regular expressions to find all words that:**

**Start with three consonants**


```r
str_view(subset_words, "^[^aeiou]{3}", match = TRUE)
```

<!--html_preserve--><div id="htmlwidget-5a0653eadf3ae8ee9649" style="width:960px;height:100%;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-5a0653eadf3ae8ee9649">{"x":{"html":"<ul>\n  <li><span class='match'>mrs<\/span><\/li>\n  <li><span class='match'>str<\/span>ike<\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

**Have three or more vowels in a row.**


```r
str_view(words, pattern = "[aeoiu]{3,}", match = TRUE)
```

<!--html_preserve--><div id="htmlwidget-1090e4fe7f6081570cf0" style="width:960px;height:100%;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-1090e4fe7f6081570cf0">{"x":{"html":"<ul>\n  <li>b<span class='match'>eau<\/span>ty<\/li>\n  <li>obv<span class='match'>iou<\/span>s<\/li>\n  <li>prev<span class='match'>iou<\/span>s<\/li>\n  <li>q<span class='match'>uie<\/span>t<\/li>\n  <li>ser<span class='match'>iou<\/span>s<\/li>\n  <li>var<span class='match'>iou<\/span>s<\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

**Have two or more vowel-consonant pairs in a row.**


```r
str_view(subset_words, "([aeiou][^aeiou]){2,}", match=TRUE)
```

<!--html_preserve--><div id="htmlwidget-7811cc2a66cc4a156c1a" style="width:960px;height:100%;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-7811cc2a66cc4a156c1a">{"x":{"html":"<ul>\n  <li>abs<span class='match'>olut<\/span>e<\/li>\n  <li>b<span class='match'>ehin<\/span>d<\/li>\n  <li>d<span class='match'>ecid<\/span>e<\/li>\n  <li><span class='match'>elec<\/span>t<\/li>\n  <li><span class='match'>exam<\/span>ple<\/li>\n  <li>fr<span class='match'>iday<\/span><\/li>\n  <li>int<span class='match'>eres<\/span>t<\/li>\n  <li><span class='match'>operat<\/span>e<\/li>\n  <li><span class='match'>operat<\/span>e<\/li>\n  <li>org<span class='match'>aniz<\/span>e<\/li>\n  <li>p<span class='match'>ower<\/span><\/li>\n  <li>pr<span class='match'>esen<\/span>t<\/li>\n  <li>s<span class='match'>atur<\/span>day<\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->




## 14.3.5.1 Exercises: Grouping and backreferences

### **Describe, in words, what these expressions will match:**

* `(.)\1\1` matches the same character appearing three times in a row, "bbb".
* `"(.)(.)\\2\\1"` matches two characters followed by the same pair of characters in reversed order, "baab".
* `(..)\1` matches any two characters repeated in teh same order, "abab".
* `"(.).\\1.\\1"` matches a string with five characters: an original character followed by any character, the original character again, any other character, the original character a third time, "babdb", "a1a2a".
* `"(.)(.)(.).*\\3\\2\\1"` matches a set of three characters, followed by zero or more characters of any kind followed by the same three characters but in reverse order "xyzzyx", "zyx1xyz".


### **Construct regular expressions to match words that:**

**Start and end with the same character.**


```r
str_view(subset_words, "^(.)((.*\\1$)|\\1?$)", match = TRUE)
```

<!--html_preserve--><div id="htmlwidget-9cf58cb45faec999a416" style="width:960px;height:100%;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-9cf58cb45faec999a416">{"x":{"html":"<ul>\n  <li><span class='match'>area<\/span><\/li>\n  <li><span class='match'>example<\/span><\/li>\n  <li><span class='match'>eye<\/span><\/li>\n  <li><span class='match'>high<\/span><\/li>\n  <li><span class='match'>stairs<\/span><\/li>\n  <li><span class='match'>treat<\/span><\/li>\n  <li><span class='match'>treat<\/span><\/li>\n  <li><span class='match'>trust<\/span><\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

**Contain a repeated pair of letters (e.g. "church" contains "ch" repeated twice.)**


```r
str_view(subset_words, "(..).*\\1", match = TRUE)
```

<!--html_preserve--><div id="htmlwidget-a697732fa1edd143d030" style="width:960px;height:100%;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-a697732fa1edd143d030">{"x":{"html":"<ul>\n  <li>c<span class='match'>ondition<\/span><\/li>\n  <li><span class='match'>decide<\/span><\/li>\n  <li>l<span class='match'>ondon<\/span><\/li>\n  <li>p<span class='match'>ressure<\/span><\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

**Contain one letter repeated in at least three places (e.g. "eleven" contains three "e"s.)**


```r
str_view(words, pattern = "(.).*\\1.*\\1", match = TRUE)
```

<!--html_preserve--><div id="htmlwidget-82624d4c637e540958bb" style="width:960px;height:100%;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-82624d4c637e540958bb">{"x":{"html":"<ul>\n  <li>a<span class='match'>pprop<\/span>riate<\/li>\n  <li><span class='match'>availa<\/span>ble<\/li>\n  <li>b<span class='match'>elieve<\/span><\/li>\n  <li>b<span class='match'>etwee<\/span>n<\/li>\n  <li>bu<span class='match'>siness<\/span><\/li>\n  <li>d<span class='match'>egree<\/span><\/li>\n  <li>diff<span class='match'>erence<\/span><\/li>\n  <li>di<span class='match'>scuss<\/span><\/li>\n  <li><span class='match'>eleve<\/span>n<\/li>\n  <li>e<span class='match'>nvironmen<\/span>t<\/li>\n  <li><span class='match'>evidence<\/span><\/li>\n  <li><span class='match'>exercise<\/span><\/li>\n  <li><span class='match'>expense<\/span><\/li>\n  <li><span class='match'>experience<\/span><\/li>\n  <li><span class='match'>indivi<\/span>dual<\/li>\n  <li>p<span class='match'>aragra<\/span>ph<\/li>\n  <li>r<span class='match'>eceive<\/span><\/li>\n  <li>r<span class='match'>emembe<\/span>r<\/li>\n  <li>r<span class='match'>eprese<\/span>nt<\/li>\n  <li>t<span class='match'>elephone<\/span><\/li>\n  <li>th<span class='match'>erefore<\/span><\/li>\n  <li>t<span class='match'>omorro<\/span>w<\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

## 14.4.2 Exercises: Detect matches

### **For each of the following challenges, try solving it by using both a single regular expression, and a combination of multiple str_detect() calls.**

**Find all words that start or end with x.**


```r
words[str_detect(words, "^x|x$")]
```

```
## [1] "box" "sex" "six" "tax"
```


```r
start <- str_detect(words, "^x")
end <- str_detect(words, "x$")
words[start | end]
```

```
## [1] "box" "sex" "six" "tax"
```

**Find all words that start with a vowel and end with a consonant.**


```r
str_view(subset_words, "^[aeiou].*[^aeiou]$", match=TRUE)
```

<!--html_preserve--><div id="htmlwidget-49c42140f5a1514ca5b8" style="width:960px;height:100%;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-49c42140f5a1514ca5b8">{"x":{"html":"<ul>\n  <li><span class='match'>about<\/span><\/li>\n  <li><span class='match'>about<\/span><\/li>\n  <li><span class='match'>add<\/span><\/li>\n  <li><span class='match'>add<\/span><\/li>\n  <li><span class='match'>apply<\/span><\/li>\n  <li><span class='match'>elect<\/span><\/li>\n  <li><span class='match'>industry<\/span><\/li>\n  <li><span class='match'>interest<\/span><\/li>\n  <li><span class='match'>or<\/span><\/li>\n  <li><span class='match'>or<\/span><\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->



```r
start <- str_detect(subset_words, "^[aeiou]")
end <- str_detect(subset_words, "[^aeiou]$")
subset_words[start | end]
```

```
##  [1] "about"     "about"     "absolute"  "add"       "add"      
##  [6] "apply"     "area"      "back"      "behind"    "bet"      
## [11] "blow"      "board"     "box"       "boy"       "card"     
## [16] "condition" "corner"    "corner"    "cross"     "cup"      
## [21] "dear"      "dear"      "dinner"    "elect"     "example"  
## [26] "eye"       "farm"      "for"       "friday"    "glass"    
## [31] "hand"      "high"      "include"   "industry"  "interest" 
## [36] "laugh"     "leg"       "london"    "lot"       "lot"      
## [41] "many"      "market"    "match"     "miss"      "monday"   
## [46] "mrs"       "operate"   "operate"   "or"        "or"       
## [51] "organize"  "past"      "pick"      "power"     "present"  
## [56] "press"     "quarter"   "region"    "run"       "saturday" 
## [61] "sing"      "sister"    "small"     "sound"     "stairs"   
## [66] "stop"      "suggest"   "term"      "traffic"   "treat"    
## [71] "treat"     "trust"     "use"       "watch"     "wind"     
## [76] "worry"     "worry"
```


**Are there any words that contain at least one of each different vowel?**

We can use multiple `str_detect()` calls, one pattern for each vowel to check:


```r
contains_vowel <- function(x) {
  vowels <- c("a", "e", "i", "o", "u")
  check <- as.logical(seq(from = 1, to = 1, along.with = x))
  for (chr in vowels) {
    check <- check & str_detect(x, pattern = chr)
  }
  return(x[check])
}
```



```r
test_that("function contains_vowel() does not work", {
  test_string <- "aberixowu"
  expect_equal(test_string, contains_vowel(test_string))
})
```


```r
contains_vowel(subset_words)
```

```
## character(0)
```
There are no matching words.


**What word has the highest number of vowels? What word has the highest proportion of vowels? (Hint: what is the denominator?)**


```r
num_vowels <- str_count(words, "[aeiou]")
words[which(num_vowels == max(num_vowels))]
```

```
## [1] "appropriate" "associate"   "available"   "colleague"   "encourage"  
## [6] "experience"  "individual"  "television"
```

There are 8 words with 5 vowels each.


```r
prop_vowels <- str_count(words, "[aeiou]") / str_length(words)
words[which(prop_vowels == max(prop_vowels))]
```

```
## [1] "a"
```

The result is reasonable because the word "a" is 100% vowel.




## 14.4.3.1 Exercises: Extract matches

### **In the previous example, you might have noticed that the regular expression matched “flickered”, which is not a colour. Modify the regex to fix the problem.**

"flickered" matched "red", because the characters of the colour are contained by the word. TO avoid this we would like to detect colours as single words only. We can achieve this by using `\b` to indicate a word boundary:



```r
colours <- c("red", "orange", "yellow", "green", "blue", "purple")
colour_match <- str_c("\\b(", str_c(colours, collapse = "|"), ")\\b")
str_view_all(sentences, colour_match, match = TRUE)
```

<!--html_preserve--><div id="htmlwidget-81798ed4a8da39d605f0" style="width:960px;height:100%;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-81798ed4a8da39d605f0">{"x":{"html":"<ul>\n  <li>Glue the sheet to the dark <span class='match'>blue<\/span> background.<\/li>\n  <li>Two <span class='match'>blue<\/span> fish swam in the tank.<\/li>\n  <li>A wisp of cloud hung in the <span class='match'>blue<\/span> air.<\/li>\n  <li>Leaves turn brown and <span class='match'>yellow<\/span> in the fall.<\/li>\n  <li>The spot on the blotter was made by <span class='match'>green<\/span> ink.<\/li>\n  <li>The sofa cushion is <span class='match'>red<\/span> and of light weight.<\/li>\n  <li>The sky that morning was clear and bright <span class='match'>blue<\/span>.<\/li>\n  <li>A <span class='match'>blue<\/span> crane is a tall wading bird.<\/li>\n  <li>It is hard to erase <span class='match'>blue<\/span> or <span class='match'>red<\/span> ink.<\/li>\n  <li>The lamp shone with a steady <span class='match'>green<\/span> flame.<\/li>\n  <li>The box is held by a bright <span class='match'>red<\/span> snapper.<\/li>\n  <li>The houses are built of <span class='match'>red<\/span> clay bricks.<\/li>\n  <li>The <span class='match'>red<\/span> tape bound the smuggled food.<\/li>\n  <li>Hedge apples may stain your hands <span class='match'>green<\/span>.<\/li>\n  <li>The plant grew large and <span class='match'>green<\/span> in the window.<\/li>\n  <li>The <span class='match'>purple<\/span> tie was ten years old.<\/li>\n  <li>Bathe and relax in the cool <span class='match'>green<\/span> grass.<\/li>\n  <li>The lake sparkled in the <span class='match'>red<\/span> hot sun.<\/li>\n  <li>Mark the spot with a sign painted <span class='match'>red<\/span>.<\/li>\n  <li>The couch cover and hall drapes were <span class='match'>blue<\/span>.<\/li>\n  <li>A man in a <span class='match'>blue<\/span> sweater sat at the desk.<\/li>\n  <li>The small <span class='match'>red<\/span> neon lamp went out.<\/li>\n  <li>Paint the sockets in the wall dull <span class='match'>green<\/span>.<\/li>\n  <li>Wake and rise, and step into the <span class='match'>green<\/span> outdoors.<\/li>\n  <li>The <span class='match'>green<\/span> light in the brown box flickered.<\/li>\n  <li>Tear a thin sheet from the <span class='match'>yellow<\/span> pad.<\/li>\n  <li>The sky in the west is tinged with <span class='match'>orange<\/span> <span class='match'>red<\/span>.<\/li>\n  <li>The <span class='match'>red<\/span> paper brightened the dim stage.<\/li>\n  <li>The big <span class='match'>red<\/span> apple fell to the ground.<\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


### **From the Harvard sentences data, extract the following words.**

**The first word from each sentence.**

We can use `str_extract` if we define a word to be any continous set of letters:

> `str_extract()`: Extract matching patterns from a string. 


```r
str_extract(sentences, "[a-zA-Z]+") %>% head()
```

```
## [1] "The"   "Glue"  "It"    "These" "Rice"  "The"
```

**All words ending in `ing`.**

`str_extract_all()` tries to extract all matches from a string, and put them into a vector.


```r
ing_words <- str_extract_all(sentences, pattern = "\\b[a-zA-Z]+ing\\b", simplify = TRUE)
ing_words[ing_words != ""] %>% 
  head()
```

```
## [1] "spring"  "evening" "morning" "winding" "living"  "king"
```

**All plurals.**

We simplify the definition of all plurals to be all words ending in an “s” and with more than three characters (to remove “as”, “is”, “gas”).


```r
unique(unlist(str_extract_all(sentences, "\\b[A-Za-z]{3,}s\\b"))) %>%
  head()
```

```
## [1] "planks" "days"   "bowls"  "lemons" "makes"  "hogs"
```

This unfortunately also includes verbs like `makes`.




## 14.4.4 Exercises: Grouped Matches

### **Find all words that come after a "number" like "one", "two", "three" etc. Pull out both the number and the word.**


```r
numwords <- "(one|two|three|four|five|six|seven|eight|nine|ten) +(\\S+)"
sentences[str_detect(sentences, numwords)] %>%
  str_extract(numwords)
```

```
##  [1] "ten served"    "one over"      "seven books"   "two met"      
##  [5] "two factors"   "one and"       "three lists"   "seven is"     
##  [9] "two when"      "one floor."    "ten inches."   "one with"     
## [13] "one war"       "one button"    "six minutes."  "ten years"    
## [17] "one in"        "ten chased"    "one like"      "two shares"   
## [21] "two distinct"  "one costs"     "ten two"       "five robins." 
## [25] "four kinds"    "one rang"      "ten him."      "three story"  
## [29] "ten by"        "one wall."     "three inches"  "ten your"     
## [33] "six comes"     "one before"    "three batches" "two leaves."
```

### **Find all contractions. Separate out the pieces before and after the apostrophe.**


```r
contraction <- "([A-Za-z]+)'([A-Za-z]+)"
sentences %>%
  `[`(str_detect(sentences, contraction)) %>%
  str_extract(contraction)
```

```
##  [1] "It's"       "man's"      "don't"      "store's"    "workmen's" 
##  [6] "Let's"      "sun's"      "child's"    "king's"     "It's"      
## [11] "don't"      "queen's"    "don't"      "pirate's"   "neighbor's"
```




## 14.4.5 Exercises: Replacing Matches

### **Replace all forward slashes in a string with backslashes.**


```r
test_string <- c("2018/November/09")
writeLines(str_replace_all(test_string, "\\/", "\\\\"))
```

```
## 2018\November\09
```

### **Implement a simple version of `str_to_lower()` using `replace_all()`.**


```r
test_strings <- c("HELLO", "WorLd")
str_to_lower(test_strings)
```

```
## [1] "hello" "world"
```

```r
str_replace_all(test_strings, c("A"="a", "B"="b", "C"="c", "D"="d", "E"="e", "F"="f", "G"="g", "H"="h", "I"="i", "J"="j", "K"="k", "L"="l", "M"="m", "N"="n", "O"="o", "P"="p", "Q"="q", "R"="r", "S"="s", "T"="t", "U"="u", "V"="v", "W"="w", "X"="x", "Y"="y", "Z"="z"))
```

```
## [1] "hello" "world"
```

### **Switch the first and last letters in words. Which of those strings are still words?**

We use `str_replace()` to switch the lettersand `intersect()` to see which words are still correct.


```r
switch <- swapped <- str_replace_all(words, "^([A-Za-z])(.*)([a-z])$", "\\3\\2\\1")

head(intersect(words, switch), 10)
```

```
##  [1] "a"       "america" "area"    "dad"     "dead"    "deal"    "dear"   
##  [8] "depend"  "dog"     "educate"
```





## 14.4.6 Exercises: Splitting


### **Split up a string like "apples, pears, and bananas" into individual components.**



```r
test_string <- c("apples, pears, and bananas")
str_split(test_string, ", +(and +)?")[[1]]
```

```
## [1] "apples"  "pears"   "bananas"
```


### **Why is it better to split up by `boundary("word")` than " "?**

Let's try to use " " to split.


```r
test_string <- c("apples, pears, and bananas")

str_split(test_string, " ")
```

```
## [[1]]
## [1] "apples," "pears,"  "and"     "bananas"
```

```r
str_split(test_string, boundary("word"))
```

```
## [[1]]
## [1] "apples"  "pears"   "and"     "bananas"
```

As shown, `boundary("word")` makes sure only words are selected, without other characters like `","`.


### **What does splitting with an empty string ("") do? Experiment, and then read the documentation.**


```r
test_string <- c("apples, pears, and bananas")
str_split(test_string, "")[[1]]
```

```
##  [1] "a" "p" "p" "l" "e" "s" "," " " "p" "e" "a" "r" "s" "," " " "a" "n"
## [18] "d" " " "b" "a" "n" "a" "n" "a" "s"
```

Based on the results and the documentation:

> An empty pattern, '', is equivalent to `boundary('character')`'

an empty string splits a string into individual characters.


## 14.5.1. Exercises: Other types of patterns

### **How would you find all strings containing `\` with `regex()` vs. with `fixed()`?**

Using `fixed()` has the benefit that we only need to add escape characters for regex.


```r
test_string <- c("2018\\November")
str_view(test_string, pattern = "\\\\")
```

<!--html_preserve--><div id="htmlwidget-f94e84202b63304be776" style="width:960px;height:100%;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-f94e84202b63304be776">{"x":{"html":"<ul>\n  <li>2018<span class='match'>\\<\/span>November<\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

```r
str_view(test_string, pattern = fixed("\\"))
```

<!--html_preserve--><div id="htmlwidget-b5379aea8829642b9c4c" style="width:960px;height:100%;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-b5379aea8829642b9c4c">{"x":{"html":"<ul>\n  <li>2018<span class='match'>\\<\/span>November<\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


### **What are the five most common words in `sentences`?**

We first try to get all words in `sentences` using `str_extract_all()` with `boundary("word")`. Then we can convert the results into a tibble and use techniques discribes in STAT 545A to tidy up the data and find the most common words.


```r
word_list <- sentences %>% 
  str_extract_all(boundary("word"), simplify = TRUE) %>% 
  str_to_lower()

tibble(words = word_list) %>% 
  count(words, sort = TRUE) %>%
  head() %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
```

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> words </th>
   <th style="text-align:right;"> n </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 2892 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> the </td>
   <td style="text-align:right;"> 751 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> a </td>
   <td style="text-align:right;"> 202 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> of </td>
   <td style="text-align:right;"> 132 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> to </td>
   <td style="text-align:right;"> 123 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> and </td>
   <td style="text-align:right;"> 118 </td>
  </tr>
</tbody>
</table>

`the` is the most common word.

## 14.7.1 Exercises: stringi


```r
library(stringi)
```


### **Find the stringi functions that:**

**Count the number of words.**

We can use `stri_count_words()`:


```r
test_string <- "How many words does this sentence have?"

test_that("The number of words is not correct", {
  expect_equal(stri_count_words(test_string), 7)
})
```

**Find duplicated strings**

We can use two functions for this problem:

* `stri_duplicated()` determines which strings in a character vector are duplicates of other elements.
* `stri_duplicated_any()` determines if there are any duplicated strings in a character vector.

We write a function using these two functions, so it can output duplicated strings if they exist.

**Generate random text.**

We can use the following two functions to generate random texts:

* `stri_rand_lipsum()` generates lorem ipsum text
* `stri_rand_strings()` generates random strings
* `stri_rand_shuffle()` randomly shuffles the code points (characters) in the text.


```r
stri_rand_lipsum(1)
```

```
## [1] "Lorem ipsum dolor sit amet, mollis sit, sed dictumst nullam sed. Mauris sodales aliquet cras et ut. Volutpat cras porttitor est ligula sed ipsum inceptos fringilla a. Class torquent tellus id. In felis per hac neque ante tempor, turpis in netus diam. Ut, ultricies nec erat suscipit id ut, justo et maximus, mus. Faucibus elit sapien dapibus aliquet interdum malesuada et. Tristique volutpat lacus sit, auctor sociosqu euismod quis, viverra sit dui nisl. Pellentesque vestibulum nisl augue, ipsum in, turpis scelerisque id pulvinar eu. Eget nascetur elementum, aliquam vestibulum? Tristique consectetur tempor diam lectus venenatis. Ultrices morbi mauris vehicula sed netus. Sociis pellentesque arcu eu amet nisl sit vestibulum ad quam aenean mauris lobortis? Himenaeos libero nec velit amet maximus vestibulum sollicitudin. Ut dui bibendum, adipiscing ac sit suspendisse."
```

### **How do you control the language that stri_sort() uses for sorting?**

According to the documentation we can use either:

* `stri_sort(..., locale = ...)` or
* `stri_sort(..., opts_collator=stri_opts_collator(locale = ...))`

