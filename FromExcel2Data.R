## merge SWELT data 
library(readxl)
library(purrr)
library(dplyr)
install.packages("ggblend")
library(ggblend)


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

# row bind all dfs (1997-2016)
df_excels<-bind_rows(df_allSheets)

# import CSVs (data from 2017-2023)
getwd()
csvFilePath<-"./"
csv_17to23<-list.files(csvFilePath, pattern = "\\.csv$", full.names = T)
print(csv_17to23)

csv_df_17to23<-lapply(csv_17to23, read.csv) %>% 
  bind_rows()


# verticaly join two DFs
EliteData_full<-bind_rows(csv_df_17to23, df_excels)             ### Full data RAW without outlier removal
# remove data where the yield value is less than 1
EliteData_full <- EliteData_full%>% 
  filter(!(YIELD < 1))

##########################################################################################

####### Clean the Elite trial data (tukeys )

# tukey's fences test

tukey_outliers_elite_k3 <- EliteData_full %>%
  group_by(LOCATION, YEAR) %>%
  mutate(
    Q1 = quantile(YIELD, 0.25),
    Q3 = quantile(YIELD, 0.75),
    IQR = Q3 - Q1,
    lower_fence = Q1 - 3 * IQR,
    upper_fence = Q3 +  3* IQR,
    is_outlier = ifelse(YIELD < lower_fence | YIELD > upper_fence, TRUE, FALSE)
  ) %>%
  filter(is_outlier == TRUE)  # Keep only outliers for inspection

getwd()
write.csv(tukey_outliers_elite_k3, "tukeys_outliers_Elite_k3.csv")



