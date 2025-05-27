library(dplyr)
library(agriutilities)

# QaulityGGAin
dataElite<-read.csv("../CLEANESTData_May25/cleanData_Elite_May25.csv")
YrRel<-read.csv("../YrOr.csv")
# select protein 
dataPro<-dataElite %>% 
  group_by(YEAR, LOCATION) %>% 
  filter(any(!is.na(PRO))) %>% 
  ungroup() %>% 
  filter(PRO >=5)
dataHT<-dataElite %>% 
  group_by(YEAR, LOCATION) %>% 
  filter(any(!is.na(HT))) %>% 
  ungroup() %>% 
  filter(HT >=20)
dataMOI<-dataElite %>% 
  group_by(YEAR, LOCATION) %>% 
  filter(any(!is.na(MOI))) %>% 
  ungroup() %>% 
  filter(MOI >=6)

dataTWT<-dataElite %>% 
  group_by(YEAR, LOCATION) %>% 
  filter(any(!is.na(TWT))) %>% 
  ungroup() %>% 
  filter(TWT >=40)

dataHD<-dataElite %>% 
  group_by(YEAR, LOCATION) %>% 
  filter(any(!is.na(HD))) %>% 
  ungroup() %>% 
  filter(HD >=100)


########### 

library(asreml)
# modelling Absolute yield and plotting
View(dataHT)
str(dataHT)
dataHT<-dataHT %>% 
  mutate(
    YEAR = factor(YEAR),
    PREID = factor(PREID),
  ) 

model_HT <-asreml(fixed = HT ~ YEAR + PREID  ,
                                   random = ~ PREID:YEAR + BLOC,
                                   residual =~dsum(~units | YEAR),
                                   workspace = "12gb",
                                   tol       = 1e-09,
                                   data = dataHT
)

predictions_HT <- predict(model_HT, classify = "PREID", pworkspace = "12gb")$pval
write.csv(predictions_HT,"predictions_Elite_HT.csv", row.names = F)

# import the file with Yearof OR and merge the preicted file 
#YrOr<-read.csv("YrOr.csv")
#...........for predictions_full_eliteResC5
dataFinalMod_HT <-predictions_HT %>% 
  inner_join(YrRel,  by= "PREID") %>% 
  filter(!(PREID == "Stephens")) 


FinalModel_HT<-lm(predicted.value ~ YEAROr, data = dataFinalMod_HT)
parameters_gg(FinalModel_HT, trait = "predicted.value")
# plot it (Run full from the fesit line below until the ggsave) ...................
coefficnt<-summary(FinalModel_HT)$coefficients
beta =coefficnt["YEAROr", "Estimate"]
p_val =coefficnt["YEAROr", "Pr(>|t|)"]
anno<-sprintf("β = %.3f\n p-val= %.3g", beta, p_val)

View(dataFinalMod_HT)
write.csv(dataFinalMod_HT, "dataFinalMod_HT_Elite_AbsComPlot.csv", row.names = F)
ggplot(dataFinalMod_HT, aes(x =YEAROr, y = predicted.value)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE, linewidth=3) +
  # geom_text_repel(
  #   aes(label = PREID),
  #   size        = 5,       # text size
  #   max.overlaps= Inf,     # show all labels
  #   box.padding = 0.2      # space around text
  # ) +
  annotate(
    "text",
    x    = min(dataFinalMod_HT$YEAROr),           # left edge
    y    = max(dataFinalMod_HT$predicted.value),          # top edge
    label = anno,
    hjust = 0,                          # left-justify text
    vjust = 1,                          # top-justify text
    size  = 10                          # adjust as needed
  ) +
  theme_minimal() +
  theme(
    axis.title    = element_text(size = 26, colour = "black"),  # axis titles
    axis.text     = element_text(size = 22, colour = "black")   # tick labels
  ) +
  labs(
    x = "Year of Release",
    y = "Height(cm)"
  )
ggsave("Elite_HT.tiff", height = 10, width = 12, units = "in", dpi = 1200)


plot(model_MOI$varcomp.trace)
tr(model_MOI)
