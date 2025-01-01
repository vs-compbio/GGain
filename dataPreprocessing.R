## data pre-processing

# merge files and save...

# merge data from individual years and merge files to one and save it 
library(dplyr)
library(purrr)

list<-list.files("./data_yearwise/", pattern = ".csv$", full.names = TRUE) # list the file path
listdf<-lapply(list, read.csv) # make list of dataframes after reading csv files from the above path

# resolve error due to ID column's varying data type: make ID as.character uniformly
listdf<-lapply(listdf, function(df){
  if ("ID" %in% names(df)){
    df$ID <- as.character(df$ID)
  }
  df
  })

# append dataframes vertically
dataFull<-reduce(listdf, bind_rows) 
dataFull<-dataFull[,-c(2,10)] # remove unnecessary columns

write.csv(dataFull, "dataFullJan1.csv", row.names = FALSE) # write the datafile as csv
j
## Explore the data .................................
# 1. check missing values  
# 2. distributions 
# 3. find out outliers 
# 4. find out leverage points
# 5. find the genotypes with just one replicate and consider removing it
# 6. Extract checks and see their distribution across years
# 7. mean of each trial, plot it.
# 8. estimate heritability of each trial
# 9. Box plot of each trial to outliers
# 10. Plot location-wise heritability of each trial
# 11. identify locations for ggain
# 12. extract year of release of each genotype
# 13. estimate ggain location wise
# 14. estimate ggain overall
##### OBTAIN OR numbers of potential checks. 



