## ##############################     GGain Elite trials  ######################
library(tidyr)
library(ggplot2)
library(viridis)
library(dplyr)
library(stringr)
## import elite trail clean data 
dataElite<-read.csv("clean_elite5_Final_May15.csv")

# Sections
  # 1. Check how mny env to go ahead with
  # 1.2 Plot checkerboard to display trial distribution with time (to be included in draft)
  # 2. Check distribution of trials for each selected location and overall
  # 3. Model and find predicted values for each location and overall; plot ggain simultaneously
  # 4.  

################################################################################## Plot trial distro: 
#                                               upto line 126
## 1. datagrid and locplot :elite
dataGrid<-dataElite %>% 
  complete(LOCATION, YEAR)

df_counts <- dataElite %>%                     # your raw observations
  group_by(LOCATION, YEAR) %>%          # one group = one trial
  summarise(
    n_genotypes = n_distinct(PREID), # count unique entries
    .groups = "drop"                    # return an ordinary data frame
  )

locPlotElite<-  df_counts %>% 
    ggplot(aes(x = LOCATION,
               y = factor(YEAR),
               fill = n_genotypes)) +
    geom_tile(colour = "white", linewidth = 1.25, width = 1) +
    scale_fill_viridis(name = "# Genotypes",
                       na.value = "grey90",
                       limits = c(0, 80),            # optional – clip scale to this range
                       breaks = c(0, 10, 20, 30, 40, 50, 60, 80) ) +
    scale_x_discrete(expand = c(0, 0))+
    
    labs(x = "Location", y = "Year") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size =18, vjust = 0.4,color ="black"),
          axis.text.y = element_text(hjust = 1, size =16, color ="black"),
          axis.title.x = element_text(size =18,face = "bold",color ="black"),
          axis.title.y = element_text(size =18, face = "bold", margin = margin(r = 15),color ="black" )
          )+
    coord_fixed(ratio = 0.9)+
  theme(
    legend.text  = element_text(colour = "black"),   # legend tick labels
    legend.title = element_text(colour = "black")    # legend title
  )


ggsave("locPlotElite_cleanOnly_May16_2.tiff", width = 12, height = 12, units = "in", dpi = 600)

##### Make the above plot for the full data without removal of trials.................................
## 1. datagrid and locplot :elite
dataGridRaw<-EliteData_full %>% 
  complete(LOCATION, YEAR)

EliteData_fullLocC <-EliteData_full %>% 
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



df_counts_raw <- EliteData_fullLocC %>%                     # your raw observations
  group_by(LOCATION, YEAR) %>%          # one group = one trial
  summarise(
    n_genotypes = n_distinct(PREID), # count unique entries
    .groups = "drop"                    # return an ordinary data frame
  )



locPlotElite<-  df_counts_raw %>% 
  ggplot(aes(x = LOCATION,
             y = factor(YEAR),
             fill = n_genotypes)) +
  geom_tile(colour = "white", linewidth = 1.25, width = 1) +
  scale_fill_viridis(name = "# Genotypes",
                     na.value = "grey90",limits = c(0, 80),            # optional – clip scale to this range
                     breaks = c(0, 10, 20, 30, 40, 50, 60, 80) ) +
  scale_x_discrete(expand = c(0, 0))+
  
  labs(x = "Location", y = "Year") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size =18, vjust = 0.4,color ="black"),
        axis.text.y = element_text(hjust = 1, size =16, color ="black"),
        axis.title.x = element_text(size =18,face = "bold",color ="black"),
        axis.title.y = element_text(size =18, face = "bold", margin = margin(r = 15),color ="black" )
  )+
  coord_fixed(ratio = 0.9)+
  theme(
    legend.text  = element_text(colour = "black"),   # legend tick labels
    legend.title = element_text(colour = "black")    # legend title
  )


ggsave("locPlotElite_RAW_May16.tiff", width = 12, height = 12, units = "in", dpi = 600)

###################################################################################################
#############   Check Continuity
checkPlotElite <- dataElite %>%                       # <- your full trial data
  filter(!str_starts(PREID, "OR")) %>%      # ① remove "OR…" entries
  distinct(PREID, YEAR)  

checkPlotEliteM4Y<-checkPlotElite %>% 
  group_by(PREID) %>% 
  filter(n_distinct(YEAR)>=5) %>% 
  ungroup()

plot_df <- checkPlotEliteM4Y %>% 
  group_by(PREID) %>% 
  mutate(n_years = n_distinct(YEAR)) %>%   # how many distinct years for this PREID
  ungroup()



library(RColorBrewer)
library(scales) 

ggplot(plot_df, 
       aes(x = factor(YEAR), 
           y = PREID, 
           fill = n_years)) +
  geom_tile(width = 0.90,
            height = 0.75,
            color = "black",
            linewidth = 0.70 ) +    
  scale_fill_viridis_c(                     
    #name = "Entry",                         
    option = "viridis",
    direction  = -1,
  ) +
  labs(x = "Year",
       y = "Cultivars")+
  scale_x_discrete(expand = c(0, 0)) +
  theme_minimal(base_size = 22) +
  theme(
    axis.text.x  = element_text(angle = 90, hjust = 1,  vjust = 0.4,color = "black"),
    axis.text.y  = element_text(color = "black"),
    axis.title.x = element_text(margin = margin(t = 12), color = "black",face = "bold"),
    axis.title.y = element_text(margin = margin(r = 10), color = "black",face = "bold"),
    legend.position = "none"
  )+coord_fixed(ratio = 0.4)
  
ggsave("checkDistrobutionElite_May16.tiff", width = 16, height = 10, units = "in", dpi = 600)

###################################################################################################

####################  Fit models overall and location-wise




