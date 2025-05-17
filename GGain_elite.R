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
########## Shared vs unique entries
df_uniqueGenYr <- dataElite %>%                       # <- your multiyear, multilocation data
  distinct(PREID, YEAR)    
library(tibble) 
incidenceMatr <- df_uniqueGenYr %>%
  mutate(present = 1) %>%                    # marker column
  pivot_wider(names_from = YEAR,
              values_from = present,
              values_fill = 0) %>%           # missing → 0
  column_to_rownames("PREID") %>%
  as.matrix()  


year_Matr <- t(incidenceMatr) %*% incidenceMatr
yrs <- sort(unique(df_uniqueGenYr$YEAR))
year_Matr <- year_Matr[ as.character(yrs), as.character(yrs) ]
# heatmap
df_htmp<-as.data.frame(as.table(year_Matr))

ggplot(df_htmp, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile(colour = "white", linewidth = 0.25) +
  # ---- Optional: print the counts in each cell ------------------------------
  geom_text(aes(label = Freq), colour = "black", size = 5.5) +
  scale_fill_gradientn(
    colours   = viridis(256, option = "cividis", direction = -1),
    name      = "# Genotypes\n(Freq)",  # legend title
    limits    = c(0, max(df_htmp$Freq)), # keeps the colour bar consistent
    na.value  = "grey90"
  ) +
  labs(x = "Year", y = "Year") +
  scale_x_discrete(expand = c(0, 0), position = "top") +  # put x-axis on top
  scale_y_discrete(expand = c(0, 0)) +
  coord_fixed() +                                         # square cells
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x  = element_text(size = 16,angle = 90, hjust = 0, vjust = 0.5, colour = "black"),
    axis.text.y  = element_text(size = 16,angle = 0, hjust = 0, vjust = 0.5, colour = "black"),
    axis.title.x  = element_text(margin = margin(t = 18),size = 20,colour = "black"),
    axis.title.y  = element_text(margin = margin(r = 12),size = 20, colour = "black"),
    panel.grid   = element_blank()
  )
ggsave("Common&UniqueEntries_Elite_May16.tiff", width = 12, height = 12, units = "in", dpi = 600)

#####################################################################################################
##############      Heatmap or lineplot for heritability
ggplot(heritability_elteTrials_clean4, aes(x = LOCATION, y = factor(YEAR), fill = TrailHeritability)) +
  geom_tile(colour = "white", linewidth = 0.2) +
  scale_fill_viridis_c(name = "Heritability", option = "plasma") +
  labs(x = "Location", y = "Year") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  coord_fixed() +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# regression lines
# only selected location
library(broom)
heritability_elteTrials_clean4SEL<-heritability_elteTrials_clean4 %>% 
  filter(LOCATION %in% c("Corvallis", "Moro", "Pendleton", "Kaseberg", "Lexington", "Hermiston"))

# Ls for each loc, slope, and 95% CI
slopes <- heritability_elteTrials_clean4SEL %>%                     
  group_by(LOCATION) %>%
  summarise(beta = coef(lm(TrailHeritability ~ YEAR))[2],    
            .groups = "drop")


annot_df <- heritability_elteTrials_clean4SEL %>%                   # LOCATION, YEAR, h2
  group_by(LOCATION) %>%
  slice_min(order_by = YEAR, n = 1) %>%       # earliest year instead of latest
  left_join(slopes, by = "LOCATION") %>%
  mutate(
    label = sprintf("β = %.3f", beta),        # slope only
    YEAR  = YEAR - 0.3                        # nudge a bit left of the first point
  )


## Line plot of heritability with year (regression)
ggplot(heritability_elteTrials_clean4SEL, aes(x = YEAR,
                        y = TrailHeritability,
                        colour = LOCATION)) +
  geom_point(size = 2, alpha = .7) +           # the raw points
  geom_smooth(method = "lm",                   #   + straight-line fit
              se = F,  
              level  = 0.9,
              alpha  = 0.01,
              linewidth = 2) +                 # line thickness
  scale_colour_viridis_d(name = "Location",option = "mako",begin  = 0.15,          # skip the first 15 %  (lightest)
                         end    = .90) +
  labs(x = "Year",
       y = "Heritability (H²)") +
   geom_text(data = annot_df,
            aes(x = YEAR, y = TrailHeritability, label = label, colour = LOCATION),
            hjust = 0,            # left-align to the x position
            vjust = 0.5,
            size  = 6,            # ≈ 8 pt; shrink if the plot is crowded
            show.legend = T) +
  theme(
    axis.text.x  = element_text(size = 18,angle = 0, hjust = 0.5, vjust = 1, colour = "black"),
    axis.text.y  = element_text(size = 18,angle = 0, hjust = 0, vjust = 0.5, colour = "black"),
    axis.title.x  = element_text(margin = margin(t = 18),size = 20,colour = "black"),
    axis.title.y  = element_text(margin = margin(r = 12),size = 20, colour = "black"),
    panel.grid   = element_blank()
  ) +
  theme(
    legend.title = element_text(size = 16, face = "bold"),  # title font
    legend.text  = element_text(size = 16)                  # entry labels
  )

ggsave("Heritability_RegressionTime_Elite_May16.tiff", width = 10, height = 8, units = "in", dpi = 600)


###################################################################################################################
####################  Fit models overall and location-wise
library(asreml)

dataElite_ggF<-dataElite %>% 
  mutate(
    across(
      c(YEAR, PREID, LOCATION), as.factor
    )
  )
str(dataElite)
model_full_elite <- asreml(fixed = YIELD ~ YEAR + PREID,
                           random = ~ LOCATION + YEAR:LOCATION +
                             YEAR:PREID + LOCATION:PREID,
                           residual = ~units,
                           data = dataElite_ggF)
predictions_full_elite <- predict(model_full_elite, classify = "PREID")

