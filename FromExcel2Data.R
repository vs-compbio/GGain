## merge SWELT data 
library(readxl)
library(purrr)
library(dplyr)

# define path for the folder where all excel files are located
fileLocation<-"."

# create a list of all excel files in the above file location
excelFilesList<-list.files(fileLocation, pattern = "\\.xls[x]?$", full.names = T)

# check file names in the above list
print(excelFilesList)

# list of dataframes from all sheets
df_allSheets<-map(excelFilesList, function(file){
  sheets <-excel_sheets(file)
  map(sheets, function(sheet){
    read_excel(path = file, sheet = sheet)
    })
}) %>% 
  flatten()



# Check the absence of a specific column name in the data frame list created 
sapply(df_allSheets, function(df) {
  if ("PREID" %in% names(df)) {
    class(df[["PREID"]])
  } else {
    NA_character_
  }
})




