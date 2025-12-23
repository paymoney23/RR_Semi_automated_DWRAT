# Last Updated By: Payman Alemi on 12/22/2025

# PURPOSE: ---------------------------------------------------------------------
# This script tests the CalWATRS flat files that the Data Governance Team (DGT)
# has loaded into Snowflake


# Load Libraries----
require(odbc)
require(DBI)
require(tidyverse)
require(sf)
require(openxlsx)
require(mapview)
require(lwgeom)
require(httr)
require(data.table)
require(odbc)
require(DBI)
require(readxl)
require(janitor)
require(writexl)

# Connect to Snowflake----
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
AR_flat_file <- dbFetch(query)

# Export a subset of AR_flat_file as a CSV for inspection
ar_subset <- head(AR_flat_file, 100)
write.csv(x = ar_subset, 
          file = "OutputData/ar_subset.csv",
          row.names = FALSE)

# Whittle to Russian River Rights-----

# Load the RR MDT_2017_2024
rr_mdt = read.csv(file ="C:/Users/palemi/Water Boards/Supply and Demand Assessment - Documents/DWRAT/SDU_Runs/Demand_Datasets/RR_2017_2024_MDT_2025-04-04.csv")

# Inner Join RR Water Rights to res
rr_ar_subset <- rr_mdt %>%
  select(APPLICATION_NUMBER) %>% 
  inner_join(AR_flat_file,
             by = c("APPLICATION_NUMBER" = "ANNUALREPORT_WR_WATERRIGHTID__C"))    


# Export RR Annual Report Subset to Excel for inspection----

# Set the file path
excel_path = "OutputData/rr_ar_wb_inspection.xlsx"

# Create the workbook
rr_ar_wb <- createWorkbook()

# Add the 1st sheet
addWorksheet(wb = rr_ar_wb,
             sheetName = "rr_ar_subset")

# Write the data to the 1st sheet as a table
writeDataTable(wb = rr_ar_wb,
               sheet = "rr_ar_subset",
               x = rr_ar_subset,
               tableName = "rr_ar_subset_table",
               tableStyle = "TableStyleMedium9",
               startCol = 1,
               startRow = 1)
               

# Save the workbook
saveWorkbook(wb = rr_ar_wb,
             file = excel_path,
             overwrite = TRUE)
