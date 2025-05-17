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

######################################################################################################

## Estimate heritability of each trial and make a list



# Define model
modelFormula<- YIELD ~ (1|PREID)  

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

heritability_elteTrials_clean3 <-clean_elite3 %>%
  group_by(YEAR, LOCATION) %>% 
  group_modify(~H2_eachtrial(.x, .y$YEAR, .y$LOCATION)) %>% 
  ungroup()

heritability_elteTrials_clean4 <-clean_elite4 %>%
  group_by(YEAR, LOCATION) %>% 
  group_modify(~H2_eachtrial(.x, .y$YEAR, .y$LOCATION)) %>% 
  ungroup()


# ### heritbility of troubling trials
# # Corvallis 2023
# cor23<-clean_elite4 %>% 
#   filter(YEAR == 2023 & LOCATION == "Corvallis")
# 
# heritability_cor23 <-cor23 %>%
#   group_by(YEAR, LOCATION) %>% 
#   group_modify(~H2_eachtrial(.x, .y$YEAR, .y$LOCATION)) %>% 
#   ungroup()
# 
# mo1 <-lmer(modelFormula , data = cor23)
# H2ResCullis <-Cullis_H2(mo1, geno_label = geno_term)$H2
#######################################################################################################
# correct the labels 
EliteData_full <-EliteData_full %>% 
  mutate(
    PREID = recode(PREID,
                   "ARS99124" ="ARS99123",
                   "ARS99125" ="ARS99123",
                   "BRUNDAGE97" ="BRUNDAGE96",
                   "BRUNDAGE98" ="BRUNDAGE96",
                   "IDO621" ="IDO620",
                   "IDO622" ="IDO620",
                   "WESTBRD529" ="WESTBRD528",
                   "WESTBRD530" ="WESTBRD528",
                   )
  )
  

#### Remove low heritability trials
# try to remove traisl with no values and then run heritabilty again
clean_elite <- EliteData_full %>%                                # your original data
  group_by(YEAR, LOCATION) %>%                        # each trial = 1 group
  filter(any(!is.na(YIELD) & YIELD != "")) %>%        # keep only if ≥1 real YIELD
  ungroup()

# Removing the year 2008 because it doesn't has info on PREID

clean_elite2 <-clean_elite %>% 
  filter(!(YEAR == 2008))


# remove less tha one rep geno
# find out which PREIDs have just one rep
rep_table_elite <- clean_elite2 %>%                       
  group_by(YEAR, LOCATION, PREID) %>%      
  summarise(
    n_reps = n(),                             # row-count = # of replicates
    .groups = "drop"                          # return an un-grouped tibble
  )

# remove single rep PREIDs
clean_elite3 <- clean_elite2 %>%            # your full data frame
  group_by(YEAR, LOCATION, PREID) %>%       # one group = one trial × genotype
  filter(n() > 1) %>%                       # keep only groups with ≥ 2 rows
  ungroup()


#####################################################################################
## plot trial means of trials
# trial means
trialMeans <- clean_elite3 %>%            # your cleaned data set
  group_by(YEAR, LOCATION) %>%             # one group = one trial
  summarise(
    trialMeans = mean(YIELD, na.rm = TRUE),# mean across all plots in that trial
    .groups    = "drop"
  )

# remove Lexington 2023 data due to abnormally low yield
clean_elite3 <-clean_elite3 %>% 
  filter(!(YEAR ==2023 & LOCATION == "Lexington"))
# remove Dufur, Madsen, SouthValley
clean_elite3 <-clean_elite3 %>% 
  filter(!(LOCATION == "Dufur"|LOCATION == "Madsen"|LOCATION == "SouthValley" ))

# Correct the location names 
clean_elite3 <-clean_elite3 %>% 
  mutate(
    LOCATION = recode(LOCATION,
                      "ARLINGTON" = "Arlington",
                      "CONDON" = "Condon",
                      "CORVALLIS" = "Corvallis",
                      "HERMISTON" = "Hermiston",
                      "KASEBERG" = "Kaseberg",
                      "Kfall" = "Kfalls",
                      "La Grande" = "LaGrande",
                      "LAGRANDE" = "LaGrande",
                      "LEXINGTON" = "Lexington",
                      "MADRAS" = "Madras",
                      "MFH2O" = "MiltonFreewater",
                      "Milton-Freewater" = "MiltonFreewater",
                      "Milton Freewater" = "MiltonFreewater",
                      "MORO" = "Moro",
                      "N. Valley" = "NorthValley",
                      "N.Valley" = "NorthValley",
                      "N.VALLEY" = "NorthValley",
                      "North Valley" = "NorthValley",
                      "NV" = "NorthValley",
                      "ONTARIO" = "Ontario",
                      "PENDLETON" = "Pendleton",
                      "Hermison" = "Hermiston"
      
    )
  )

###############################################################################
###  Clean the PREIDs
# find unique PREIDs
uniquePREIDs<-clean_elite4 %>%
  distinct(PREID)
  
### Clean PREID
library(stringr)

clean_elite3 <-clean_elite3 %>% 
  mutate(
    PREID = str_replace(PREID, "^OR\\s+", "OR")   # delete space(s) after “OR”
  )
  
  
  
clean_elite4 <-clean_elite3 %>% 
  mutate(
    PREID = recode(PREID,
                      "ARSC96059-1*" = "ARSC96059-1",
                      "ARS97135-9*" = "ARS97135-9",
                      "BRUNDAGE96" = "Brundage 96",
                      "BRUNDAGE 96" = "Brundage 96",
                      "CHUKAR" = "Chukar",
                      "BZ 6W99-456*" = "BZ 6W99-456",
                      "CODA" = "Coda",
                      "DUNE" = "Dune",
                      "FINCH" = "Finch",
                      "GENE" = "Gene",
                      "MFH2O" = "MiltonFreewater",
                      "ID99-419*" = "ID99-419",
                      "ID99-435*" = "ID99-435",
                      "MADSEN" = "Madsen",
                      "MASAMI" = "Masami",
                      "MOHLER" = "Mohler",
                      "OR2020787*" = "OR2020787",
                      "OR2030238*" = "OR2030238",
                      "OR2030239*" = "OR2030239",
                      "OR2030411*" = "OR2030411",
                      "OR2030554*" = "OR2030554",
                      "ORH010837*" = "ORH010837",
                   "ORI2042037*" = "ORI2042037",
                   "ROD" = "Rod",
                   
                   "Rod/Tubbs06" = "Rod&Tubbs06",
                   "SIMON" = "Simon",
                   "STEPHENS" = "Stephens",
                   "TUBBS" = "Tubbs",
                   "TUBBS-06" = "Tubbs-06",
                   "TUBBS-06*" = "Tubbs-06",
                   "UI Magic" = "UI-Magic",
                   "WA8134" = "WA 8134",
                   "WEATHERFORD" = "Weatherford",
                   "WESTBRED 528" = "Westbred 528",
                   "WESTBRD528" = "Westbred 528",
                   "ORCF-101" = "ORCF 101",
                   "ORCF-102" = "ORCF 102"
                      
    )
  )

# find unique PREIDs
uniquePREIDs_2<-clean_elite4 %>%
  distinct(PREID)
write.csv(uniquePREIDs_2, "uniquePREIDEliteFinal.csv")

########################################################################################
#Remove trials with heritabilit values less than 0.2

low_h2<-heritability_elteTrials_clean4 %>% 
  filter(is.finite(TrailHeritability) & TrailHeritability <0.2) %>% 
  select(YEAR, LOCATION)
         
clean_elite5 <- clean_elite4 %>% 
  anti_join(low_h2, by = c("YEAR", "LOCATION"))


#### save the clean and final data
write.csv(clean_elite5, "clean_elite5_Final_May15.csv")
  
#############################################################################



