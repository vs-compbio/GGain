## merge SWELT data 
library(readxl)
library(purrr)
library(dplyr)
# install.packages("ggblend")
library(ggblend)
# install.packages("EnvStats")  
library(EnvStats)
library(purrr)
install.packages("remotes")
remotes::install_github("etnite/bwardr", force = TRUE)
library(bwardr)
library(lme4)

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

# tukey's fences test.........................................Method 1

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

# Rosner's test........................................................Method 2

eliteOut_ros2 <- EliteData_full %>%
  group_by(LOCATION, YEAR) %>%
  summarise(
    #rosner_result <- rosnerTest(nonNormFuldata$YIELD, k = 30)
    rosner_outliers = list(rosnerTest(YIELD, k = 10)),  # Adjust 'k' for max number of outliers
    .groups = "drop"
  )

# Extract results
results_Ros_elite <- eliteOut_ros2 %>%
  mutate(
    summary = map(rosner_outliers, ~ .x$all.stats),  # Detailed results
    significant_outliers = map(rosner_outliers, ~ .x$all.stats[.x$all.stats$Outlier == TRUE, ])
  )


# Unnest outliers
all_outliers_Ros_elite <- results_Ros_elite %>%
  select(LOCATION, YEAR, significant_outliers) %>%
  unnest(cols = c(significant_outliers))
write.csv(all_outliers_Ros_elite, "rosner_outlier_elite.csv")

###########################################################################################

## Estimate heritability of each trial and make a list



# Define model
modelFormula<- YIELD ~ (1|PREID) + (1|BLOC) 

geno_term     <- "PREID"
# Define a function to make things easier

H2_eachtrial <-function(data_provided, Yr, Loc){
  result<-tryCatch({
    m1 <-lmer(modelFormula , data = data_provided)
    H2ResCullis <-Cullis_H2(m1, geno_label = geno_term)$H2
    tibble(TrailHeritability= H2ResCullis)
  }, error = function(e) tibble(TrailHeritability = NA_real_))
  
  result %>% mutate(Year = Yr, Location = Loc, .before=1)
}



## estimate hetibility of each trial

heritability_elteTrials_clean <-clean_elite2 %>%
  group_by(YEAR, LOCATION) %>% 
  group_modify(~H2_eachtrial(.x, .y$YEAR, .y$LOCATION)) %>% 
  ungroup()


#### Remove low heritability trials
# try to remove traisl with no values and then run heritabilty again
clean_elite <- EliteData_full %>%                                # your original data
  group_by(YEAR, LOCATION) %>%                        # each trial = 1 group
  filter(any(!is.na(YIELD) & YIELD != "")) %>%        # keep only if ≥1 real YIELD
  ungroup()

clean_elite2<-clean_elite %>% 
  filter(!(YEAR ==2008))

