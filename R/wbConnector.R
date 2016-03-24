#'Connect to West Brook sql database
#'
#'Creates two links to the West Brook database one for RPostgreSQL and one for dplyr
#'@return \code{con} A link to the database for use with RPostgreSQL functions
#'@return \code{conDplyr} A link to the database for use with dplyr functions
wbConnector<-function(usr=NULL,pass=NULL){
  #!/usr/bin/r -vi
  if(is.null(usr)){
  #obtain credentials interactively from the user
  usr<-readline("Enter postgres username: ")
  # cat("\nEnter postgres password (not stored in history, console will be cleared upon entry)")
  pass<-readline("Enter postgres password: ")
  }
  credentials<-list(drv="PostgreSQL",
                    host="osensei.cns.umass.edu",
                    user=usr,
                    password=pass,
                    dbname="westbrook")
  con<<-do.call(RPostgreSQL::dbConnect,credentials)
  conDplyr<<-do.call(src_postgres,credentials)
  cat("\014")
}

#'Reconnect to the West Brook database
#'
#'Tests the connection to the database and runs wbConnect() if the connection has not been established or has expired
#'@return A connection to the West Brook database
#'@export
reconnect<-function(usr=NULL,pass=NULL){
  if(!exists("con")){wbConnector()} else {
    if(class(con)!="PostgreSQLConnection"){wbConnector()} else{
      if(class(try(RPostgreSQL::dbGetQuery(con,""),silent=T))=="try-error"|
         any(suppressWarnings(class(try(tbl(conDplyr,"yoy_bins"),silent=T))=="try-error"))){
        wbConnector(usr=usr,pass=pass)
      }
    }
  }
}
