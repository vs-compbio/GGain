## merge SWELT data 
library(readxl)
library(purrr)
library(dplyr)

setwd("C:/Users/singhvis/OneDrive - Oregon State University/GGainProject_24/GGain/SWELT/sweltFinal2/")

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
  if ("YEAR" %in% names(df)) {
    class(df[["YEAR"]])
  } else {
    NA_character_
  }
})

# row bind all dfs
df_excels<-bind_rows(df_allSheets)

# import csvs
getwd()
csvFilePath<-"./"
csv_17to23<-list.files(csvFilePath, pattern = "\\.csv$", full.names = T)
print(csv_17to23)

csv_df_17to23<-lapply(csv_17to23, read.csv) %>% 
  bind_rows()


# verticaly join two DFs
EliteData_full<-bind_rows(csv_df_17to23, df_excels)             ### Full data RAW without outlier removal






