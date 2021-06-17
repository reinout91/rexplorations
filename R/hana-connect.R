library(DBI)
library(RJDBC)
library(data.table)

credentials <- strsplit(as.character(read.table('auth.txt')[[1]]),'XXXXX')
user$username <- credentials[[1]][2]
user$password <- credentials[[1]][3]

driver <- RJDBC::JDBC(driverClass = "com.sap.db.jdbc.Driver",
                      classPath = "/data/share/R/library/alliander.common.r/java/ngdbc.jar",
                      identifier.quote = "\"")

connection_database <-  RJDBC::dbConnect(drv = driver,
                                         "jdbc:sap://dpl.hec.alliander.local:30241/?sessionVariable:APPLICATION=RSTUDIO&sessionVariable:communicationTimeout=7200000",
                                         username,
                                         password)

query <- "SELECT TO_DATE(DATUM_TIJD) as DATUM, DATUM_TIJD, ASSETID, JAAR, SCENARIO_ID, SCENARIO, BELASTING FROM \"_SYS_BIC\".\"IT.Specifiek.Keten.NetPrognose.QV/NP_OUT_OS_PROFIELEN\" LIMIT 2"

# Import data

a <- as.data.table(DBI::dbFetch(
  RJDBC::dbSendQuery(conn = connection_database,
                     statement = query)))

RJDBC::dbDisconnect(connection_database)