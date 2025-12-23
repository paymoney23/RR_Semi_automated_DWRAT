# Last Updated By: Payman Alemi on 12/22/2025

# PURPOSE: ---------------------------------------------------------------------
# This script tests the CalWATRS flat files that the Data Governance Team (DGT)
# has loaded into Snowflake


require(odbc)
require(DBI)

sf_con <- DBI::dbConnect(drv = odbc::odbc(), 
               dsn = "snowflake", 
               server = "gb51005.west-us-2.azure.snowflakecomputing.com", 
               Trusted_Connection = "True", 
               authenticator = "externalbrowser",
               database = "SWRCB_INTERNAL_TEST",
               schema = "WR_CALWATRS_FLATFILES",
               role = "SWRCB_INTERNAL_TEST_WR_CALWATRS_FLATFILES_R_ACROLE",
               warehouse = "WR_WH")


# Send a SQL Query to Snowflake
query <- dbSendQuery(sf_con,  
                    "SELECT DISTINCT * FROM SWRCB_INTERNAL_TEST.WR_CALWATRS_FLATFILES.ANNUAL_REPORTS")

# Load the result
res <-dbFetch(query)
              
colnames(res)