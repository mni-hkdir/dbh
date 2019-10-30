## Håndterer token-innlogging ----

# Globale variabler for nåværende token og utløpstid
dbh_api_token_utløpstid <- Sys.time()
dbh_api_token_innhold <- ""

# Henter nytt token
#' Title
#'
#' @param brukernavn 
#' @param passord 
#'
#' @return
#' @export
#'
#' @examples
dbh_api_token_hent_nytt <- function(brukernavn, passord) {
  res <- 
    httr::POST(url = "https://sso.nsd.no/oauth/token",
      httr::authenticate(user = brukernavn,
        password = passord),
      body = list(grant_type = "client_credentials"),
      encode = "form") 
  res <- httr::content(res, as = "text") 
  res <- jsonlite::fromJSON(res)
  return(res$access_token)
}


# Returnerer nåværende token fra global variabel, eller henter nytt token hvis
# utløpt
#' Title
#'
#' @param brukernavn 
#' @param passord 
#'
#' @return
#' @export
#'
#' @examples
dbh_api_token <- function(brukernavn="", passord="" ){
  t <- Sys.time()
  if (t >= dbh_api_token_utløpstid) {
    purrr::walk2(
      stringr::str_c("dbh_api_token_",
        c("utløpstid", "innhold")),
      list(t + 3600,
        dbh_api_token_hent_nytt(brukernavn, passord)),
      assign,
      env = .GlobalEnv
    )
  }
  return(dbh_api_token_innhold)
}
