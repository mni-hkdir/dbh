# dbh

Get data from dbh api as r dataframe
## Getting started

#### Prerequisites
In order to use code you need to install R and R studio and the following R packages
rjson,
jsonlite,
httr,
magrittr,
stringr,
tidyverse

#### Brief description of file organization
Consists of three R scripts

*  DBH_api_tabell.R : allow us to send query to dbh api and get data as R dataframe or to get data from a bulk data from dbh api-stage

* DBH_metadata.R: get metadata for each table

* DBH_token.R : get token  

### Running the tests
It is necessary to run all R scripts.
For token users it necessary to get token before start using function dbh_api_token(brukernavn, passord)

* Get R data frame from bulk data 

dbh_tabell(tabell_id)

* Get R data frame using query 

dbh_tabell(902, filters=list("Årstall"=c("between", c("2016", "2019")), "Institusjonskode"=c("1120")))

* Metadata information

to get metadata you need to choose option meta=TRUE in function dbh_tabell


Author: Marija Ninic-norsk senter for forsknigsdata
