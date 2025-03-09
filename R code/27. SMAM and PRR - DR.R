################################################################################
# Article: Multimorbidity parity progression ratio in Europe
# Title:   Compute SMAM and parity progression ratio 
# Authors: Daniel,
# Data:    SHARE W1
################################################################################

# ---------------------------------------------------------------------------- #
#  0. Working directory and Packages
# ---------------------------------------------------------------------------- #

# To clear everything in R, before start the analysis and open functions
rm(list = ls())
pacman::p_load(here)
source(here::here("R Code/00. Packages and Functions.R"))

# ---------------------------------------------------------------------------- #
#  1. Open data
# ---------------------------------------------------------------------------- #

# ----------------------
#   SHARE wave 1
# ----------------------

SHARE_W1_pop <- read_stata("Data/FinalSHARE/SHARE_W1_analysis.dta")


# Weights
SHARE_W1_analysis <- read_stata("Data/FinalSHARE/SHARE_W1_Pop_diseases.dta")


SHARE_W1_analysis_2 <- SHARE_W1_analysis %>% 
  filter(!is.na(age_cat)) %>% 
  mutate(Year=2004,
         age_groups = ifelse(age_cat>=80,80, age_cat),
         number_diseases = ifelse(is.na(number_diseases),0, number_diseases)) %>% 
  rename(Pop = cciw_w1,
         Wave = wave,
         sex = gender,
         Num_diseases = number_diseases)

SHARE_W1_analysis_2$sex <- factor(SHARE_W1_analysis_2$sex, levels = c(1,2),
                                  labels = c("Males","Females"))

SHARE_W1_analysis_2$country <- factor(SHARE_W1_analysis_2$country,
                                      levels = c(11,12,13,14,15,
                                                 16,17,18,19,
                                                 20,23,25,28,29,
                                                 30,31,32,33,34,35,
                                                 47,48,
                                                 51,53,55,57,59,
                                                 61,63),
                                      labels = c("Austria","Germany","Sweden","Netherlands","Spain",
                                                 "Italy","France", "Denmark","Greece",
                                                 "Switzerland", "Belgium","Israel","Czech Republic","Poland",
                                                 "Ireland","Luxembourg","Hungary","Portugal","Slovenia","Estonia",
                                                 "Croatia","Lithuania",
                                                 "Bulgaria","Cyprus","Finland","Latvia","Malta",
                                                 "Romania","Slovakia"))




# ----------------------
#   SHARE wave 7
# ----------------------

SHARE_W7_pop <- read_stata("Data/FinalSHARE/SHARE_W7_analysis.dta")


# Weights
SHARE_W7_analysis <- read_stata("Data/FinalSHARE/SHARE_W7_Pop_diseases.dta")


SHARE_W7_analysis_2 <- SHARE_W7_analysis %>% 
  filter(!is.na(age_cat)) %>% 
  mutate(Year=2017,
         age_groups = ifelse(age_cat>=80,80, age_cat),
         number_diseases = ifelse(is.na(number_diseases),0, number_diseases)) %>% 
  rename(Pop = cciw_w7,
         Wave = wave,
         sex = gender,
         Num_diseases = number_diseases)

SHARE_W7_analysis_2$sex <- factor(SHARE_W7_analysis_2$sex, levels = c(1,2),
                                  labels = c("Males","Females"))

SHARE_W7_analysis_2$country <- factor(SHARE_W7_analysis_2$country,
                                      levels = c(11,12,13,14,15,
                                                 16,17,18,19,
                                                 20,23,25,28,29,
                                                 30,31,32,33,34,35,
                                                 47,48,
                                                 51,53,55,57,59,
                                                 61,63),
                                      labels = c("Austria","Germany","Sweden","Netherlands","Spain",
                                                 "Italy","France", "Denmark","Greece",
                                                 "Switzerland", "Belgium","Israel","Czech Republic","Poland",
                                                 "Ireland","Luxembourg","Hungary","Portugal","Slovenia","Estonia",
                                                 "Croatia","Lithuania",
                                                 "Bulgaria","Cyprus","Finland","Latvia","Malta",
                                                 "Romania","Slovakia"))



Original_countries <- c("Austria","Germany","Sweden","Netherlands","Spain",
                        "Italy","France", "Denmark","Greece",
                        "Switzerland", "Belgium","Israel")

# ----------------------
#   SHARE combined
# ----------------------

# Same regions as Zazueta et al. 2024

SHARE_W1_W7_analysis <- SHARE_W1_analysis_2 %>% 
  rbind(SHARE_W7_analysis_2) %>% 
  mutate(Region = case_when(country=="Denmark" | country=="Finland" |
                              country=="Sweden" | country=="Norway" ~ 1,
                            country=="Austria" | country=="Belgium" |
                              country=="France" | country=="Germany" |
                              country=="Ireland" | country=="Netherlands" |
                              country=="Switzerland" ~  2,
                            country=="Greece" | country=="Italy" | country=="Luxembourg" | 
                              country=="Portugal" | country=="Spain" ~ 3,
                            country=="Bulgaria" | country=="Czech Republic" |
                              country=="Hungary" | country=="Poland" |
                              country=="Slovakia" | country=="Slovenia" ~ 4,
                            country=="Belarus" | country=="Estonia" |
                              country=="Latvia" | country=="Lithuania" |
                              country=="Romania" |
                              country=="Russia" | country=="Ukraine"  ~  5,
                            country=="Malta" | country=="Croatia" |
                              country=="Cyprus" | country=="Israel" ~ 6))

SHARE_W1_W7_analysis$Region <- factor(SHARE_W1_W7_analysis$Region, 
                                      levels = c(1,2,3,4,5,6),
                                      labels = c("Nordic countries",
                                                 "Western European countries",
                                                 "Southern European countries",
                                                 "Central Eastern European countries",
                                                 "Former Soviet Republics",
                                                 "Baltic + Israel"))



SHARE_W1_W7_Age_dis <- SHARE_W1_W7_analysis %>% 
  group_by(country, sex, Year, Wave, age_groups) %>% 
  summarize(Pop=sum(Pop)) %>% 
  group_by(country, sex, Year, Wave) %>% 
  mutate(Total = sum(Pop),
         Distri = round(Pop/Total*100,2),
         Wave = ifelse(Year==2004,"Wave 1 (2004)","Wave 7 (2017)"),
         Label = paste(country, sex, sep = " - "))



Figure_Age_dis <- ggplot(SHARE_W1_W7_Age_dis, mapping = aes(x=age_groups,
                                                               y=Distri,
                                                          color=factor(Wave),
                                                          group=factor(Wave))) +
  geom_line(size=1.4) +
  facet_wrap(.~Label, scales = "free") +
  #coord_flip() +
  scale_color_manual(values = c("indianred3", "lightblue4")) +
  theme_classic() +
  theme(text = element_text(size = 16), 
        legend.position = "bottom",
        #legend.position=c(.95, 0.15),
        legend.background = element_rect(fill="transparent", 
                                         size=1, linetype="solid"),
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        axis.text.x = element_text(angle = 0, vjust = 0.9, hjust=.50),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(1,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  labs(#title = "B) Contribution to stagnation",
    color="Period",
    fill="Period",
    y="Single Mean Age at one disease",
    x="Country")
Figure_Age_dis




# ---------------------------------------------------------------------------- #
#  2. Single Mean Age
# ---------------------------------------------------------------------------- #

SHARE_W1_W7_sensitivityanalysis <- SHARE_W1_W7_analysis %>% 
  filter(country %in% Original_countries)

# ---------------------------------------------------------------------------- #
#  2.1 Single Mean Age by country
# ---------------------------------------------------------------------------- #

# Single mean age at disease
SMA_disease_country <- SHARE_W1_W7_analysis %>% 
  mutate(Disease_free = ifelse(Num_diseases==1,"Disease","No_disease")) %>% 
  group_by(Wave, Year, country, sex,  age_groups, Disease_free) %>% 
  summarize(Pop = sum(Pop)) %>% 
  arrange(Wave, Year, country, sex,  age_groups) %>% 
  pivot_wider(names_from = Disease_free,
              values_from = Pop) %>% 
  mutate(Disease = ifelse(is.na(Disease),0,Disease),
         No_disease = ifelse(is.na(No_disease),0,No_disease),
         Prop_nodisease = No_disease/(Disease+ No_disease),
         AP_x_i = Prop_nodisease*5,
         Never_diseas_90 = case_when(age_groups==80 ~ Prop_nodisease,
                                     age_groups!=80  ~ 0),
         Disease_90 = 1 - Never_diseas_90) %>% 
  group_by(Wave,Year,  country, sex) %>% 
  mutate(Perons_year =  50 + sum(AP_x_i),
         Perons_year_90 = 80*Never_diseas_90) %>% 
  filter(age_groups==80) %>% 
  mutate(SMADisease = (Perons_year-Perons_year_90)/Disease_90) %>% 
  dplyr::select(Wave, Year,  country, sex, SMADisease)


# Single mean age at multimorbidity
SMA_Multimorbidity_country <- SHARE_W1_W7_analysis %>% 
  mutate(Disease_free = ifelse(Num_diseases>=2,"Disease","No_disease")) %>% 
  group_by(Wave, Year, country, sex,  age_groups, Disease_free) %>% 
  summarize(Pop = sum(Pop)) %>% 
  arrange(Wave, Year, country, sex,  age_groups) %>% 
  pivot_wider(names_from = Disease_free,
              values_from = Pop) %>% 
  mutate(Disease = ifelse(is.na(Disease),0,Disease),
         No_disease = ifelse(is.na(No_disease),0,No_disease),
         Prop_nodisease = No_disease/(Disease+ No_disease),
         AP_x_i = Prop_nodisease*5,
         Never_diseas_90 = case_when(age_groups==80 ~ Prop_nodisease,
                                     age_groups!=80  ~ 0),
         Disease_90 = 1 - Never_diseas_90) %>% 
  group_by(Wave,Year,  country, sex) %>% 
  mutate(Perons_year =  50 + sum(AP_x_i),
         Perons_year_90 = 80*Never_diseas_90) %>% 
  filter(age_groups==80) %>% 
  mutate(SMA_Multimorbidity = (Perons_year-Perons_year_90)/Disease_90) %>% 
  dplyr::select(Wave, Year,  country, sex, SMA_Multimorbidity)


SMA_Multimorbidity_country_sensitviity <- SHARE_W1_W7_sensitivityanalysis %>% 
  mutate(Disease_free = ifelse(Num_diseases>=2,"Disease","No_disease")) %>% 
  group_by(Wave, Year, country, sex,  age_groups, Disease_free) %>% 
  summarize(Pop = sum(Pop)) %>% 
  arrange(Wave, Year, country, sex,  age_groups) %>% 
  pivot_wider(names_from = Disease_free,
              values_from = Pop) %>% 
  mutate(Disease = ifelse(is.na(Disease),0,Disease),
         No_disease = ifelse(is.na(No_disease),0,No_disease),
         Prop_nodisease = No_disease/(Disease+ No_disease),
         AP_x_i = Prop_nodisease*5,
         Never_diseas_90 = case_when(age_groups==80 ~ Prop_nodisease,
                                     age_groups!=80  ~ 0),
         Disease_90 = 1 - Never_diseas_90) %>% 
  group_by(Wave,Year,  country, sex) %>% 
  mutate(Perons_year =  50 + sum(AP_x_i),
         Perons_year_90 = 80*Never_diseas_90) %>% 
  filter(age_groups==80) %>% 
  mutate(SMA_Multimorbidity = (Perons_year-Perons_year_90)/Disease_90) %>% 
  dplyr::select(Wave, Year,  country, sex, SMA_Multimorbidity)



# ---------------------------------------------------------------------------- #
#  2.2 Single Mean Age by region
# ---------------------------------------------------------------------------- #


# Single mean age at disease
SMA_disease_Region <- SHARE_W1_W7_analysis %>% 
  mutate(Disease_free = ifelse(Num_diseases==1,"Disease","No_disease")) %>% 
  group_by(Wave, Year, Region, sex,  age_groups, Disease_free) %>% 
  summarize(Pop = sum(Pop)) %>% 
  arrange(Wave, Year, Region, sex,  age_groups) %>% 
  pivot_wider(names_from = Disease_free,
              values_from = Pop) %>% 
  mutate(Disease = ifelse(is.na(Disease),0,Disease),
         No_disease = ifelse(is.na(No_disease),0,No_disease),
         Prop_nodisease = No_disease/(Disease+ No_disease),
         AP_x_i = Prop_nodisease*5,
         Never_diseas_90 = case_when(age_groups==80 ~ Prop_nodisease,
                                     age_groups!=80  ~ 0),
         Disease_90 = 1 - Never_diseas_90) %>% 
  group_by(Wave,Year,  Region, sex) %>% 
  mutate(Perons_year =  50 + sum(AP_x_i),
         Perons_year_90 = 80*Never_diseas_90) %>% 
  filter(age_groups==80) %>% 
  mutate(SMADisease = (Perons_year-Perons_year_90)/Disease_90) %>% 
  dplyr::select(Wave, Year,  Region, sex, SMADisease)


# Single mean age at Multimorbidity
SMA_Multimorbidity_Region <- SHARE_W1_W7_analysis %>% 
  mutate(Disease_free = ifelse(Num_diseases>=2,"Disease","No_disease")) %>% 
  group_by(Wave, Year, Region, sex,  age_groups, Disease_free) %>% 
  summarize(Pop = sum(Pop)) %>% 
  arrange(Wave, Year, Region, sex,  age_groups) %>% 
  pivot_wider(names_from = Disease_free,
              values_from = Pop) %>% 
  mutate(Disease = ifelse(is.na(Disease),0,Disease),
         No_disease = ifelse(is.na(No_disease),0,No_disease),
         Prop_nodisease = No_disease/(Disease+ No_disease),
         AP_x_i = Prop_nodisease*5,
         Never_diseas_90 = case_when(age_groups==80 ~ Prop_nodisease,
                                     age_groups!=80  ~ 0),
         Disease_90 = 1 - Never_diseas_90) %>% 
  group_by(Wave,Year,  Region, sex) %>% 
  mutate(Perons_year =  50 + sum(AP_x_i),
         Perons_year_90 = 80*Never_diseas_90) %>% 
  filter(age_groups==80) %>% 
  mutate(SMA_Multimorbidity = (Perons_year-Perons_year_90)/Disease_90) %>% 
  dplyr::select(Wave, Year,  Region, sex, SMA_Multimorbidity)


# Single mean age at Multimorbidity
SMA_Multimorbidity_Region_sensitivity <- SHARE_W1_W7_sensitivityanalysis %>% 
  mutate(Disease_free = ifelse(Num_diseases>=2,"Disease","No_disease")) %>% 
  group_by(Wave, Year, Region, sex,  age_groups, Disease_free) %>% 
  summarize(Pop = sum(Pop)) %>% 
  arrange(Wave, Year, Region, sex,  age_groups) %>% 
  pivot_wider(names_from = Disease_free,
              values_from = Pop) %>% 
  mutate(Disease = ifelse(is.na(Disease),0,Disease),
         No_disease = ifelse(is.na(No_disease),0,No_disease),
         Prop_nodisease = No_disease/(Disease+ No_disease),
         AP_x_i = Prop_nodisease*5,
         Never_diseas_90 = case_when(age_groups==80 ~ Prop_nodisease,
                                     age_groups!=80  ~ 0),
         Disease_90 = 1 - Never_diseas_90) %>% 
  group_by(Wave,Year,  Region, sex) %>% 
  mutate(Perons_year =  50 + sum(AP_x_i),
         Perons_year_90 = 80*Never_diseas_90) %>% 
  filter(age_groups==80) %>% 
  mutate(SMA_Multimorbidity = (Perons_year-Perons_year_90)/Disease_90) %>% 
  dplyr::select(Wave, Year,  Region, sex, SMA_Multimorbidity)


# ---------------------------------------------------------------------------- #
#  2.2 Single Mean Age by region
# ---------------------------------------------------------------------------- #


# Single mean age at disease
SMA_disease_Europe <- SHARE_W1_W7_analysis %>% 
  mutate(Disease_free = ifelse(Num_diseases==1,"Disease","No_disease")) %>% 
  group_by(Wave, Year, sex,  age_groups, Disease_free) %>% 
  summarize(Pop = sum(Pop)) %>% 
  arrange(Wave, Year,  sex,  age_groups) %>% 
  pivot_wider(names_from = Disease_free,
              values_from = Pop) %>% 
  mutate(Disease = ifelse(is.na(Disease),0,Disease),
         No_disease = ifelse(is.na(No_disease),0,No_disease),
         Prop_nodisease = No_disease/(Disease+ No_disease),
         AP_x_i = Prop_nodisease*5,
         Never_diseas_90 = case_when(age_groups==80 ~ Prop_nodisease,
                                     age_groups!=80  ~ 0),
         Disease_90 = 1 - Never_diseas_90) %>% 
  group_by(Wave,Year,  sex) %>% 
  mutate(Perons_year =  50 + sum(AP_x_i),
         Perons_year_90 = 80*Never_diseas_90) %>% 
  filter(age_groups==80) %>% 
  mutate(SMADisease = (Perons_year-Perons_year_90)/Disease_90) %>% 
  dplyr::select(Wave, Year,  sex, SMADisease)


# Single mean age at Multimorbidity
SMA_Multimorbidity_Europe <- SHARE_W1_W7_analysis %>% 
  mutate(Disease_free = ifelse(Num_diseases>=2,"Disease","No_disease")) %>% 
  group_by(Wave, Year,  sex,  age_groups, Disease_free) %>% 
  summarize(Pop = sum(Pop)) %>% 
  arrange(Wave, Year,  sex,  age_groups) %>% 
  pivot_wider(names_from = Disease_free,
              values_from = Pop) %>% 
  mutate(Disease = ifelse(is.na(Disease),0,Disease),
         No_disease = ifelse(is.na(No_disease),0,No_disease),
         Prop_nodisease = No_disease/(Disease+ No_disease),
         AP_x_i = Prop_nodisease*5,
         Never_diseas_90 = case_when(age_groups==80 ~ Prop_nodisease,
                                     age_groups!=80  ~ 0),
         Disease_90 = 1 - Never_diseas_90) %>% 
  group_by(Wave,Year,   sex) %>% 
  mutate(Perons_year =  50 + sum(AP_x_i),
         Perons_year_90 = 80*Never_diseas_90) %>% 
  filter(age_groups==80) %>% 
  mutate(SMA_Multimorbidity = (Perons_year-Perons_year_90)/Disease_90) %>% 
  dplyr::select(Wave, Year,  sex, SMA_Multimorbidity)


# Single mean age at Multimorbidity
SMA_Multimorbidity_Europe_sensitivity <- SHARE_W1_W7_sensitivityanalysis %>% 
  mutate(Disease_free = ifelse(Num_diseases>=2,"Disease","No_disease")) %>% 
  group_by(Wave, Year,  sex,  age_groups, Disease_free) %>% 
  summarize(Pop = sum(Pop)) %>% 
  arrange(Wave, Year,  sex,  age_groups) %>% 
  pivot_wider(names_from = Disease_free,
              values_from = Pop) %>% 
  mutate(Disease = ifelse(is.na(Disease),0,Disease),
         No_disease = ifelse(is.na(No_disease),0,No_disease),
         Prop_nodisease = No_disease/(Disease+ No_disease),
         AP_x_i = Prop_nodisease*5,
         Never_diseas_90 = case_when(age_groups==80 ~ Prop_nodisease,
                                     age_groups!=80  ~ 0),
         Disease_90 = 1 - Never_diseas_90) %>% 
  group_by(Wave,Year,   sex) %>% 
  mutate(Perons_year =  50 + sum(AP_x_i),
         Perons_year_90 = 80*Never_diseas_90) %>% 
  filter(age_groups==80) %>% 
  mutate(SMA_Multimorbidity = (Perons_year-Perons_year_90)/Disease_90) %>% 
  dplyr::select(Wave, Year,  sex, SMA_Multimorbidity)



# -----------------------
# Combine Single mean ages
# -----------------------

Single_Mean_Age_datasets <- SMA_disease_country %>% 
  left_join(SMA_Multimorbidity_country) %>% 
  mutate(Year = round(Year,0),
         Wave = ifelse(Year==2004,"Wave 1 (2004)","Wave 7 (2017)"),
         Region = case_when(country=="Denmark" | country=="Finland" |
                              country=="Sweden" | country=="Norway" ~ 1,
                            country=="Austria" | country=="Belgium" |
                              country=="France" | country=="Germany" |
                              country=="Ireland" | country=="Netherlands" |
                              country=="Switzerland" ~  2,
                            country=="Greece" | country=="Italy" | country=="Luxembourg" | 
                              country=="Portugal" | country=="Spain" ~ 3,
                            country=="Bulgaria" | country=="Czech Republic" |
                              country=="Hungary" | country=="Poland" |
                              country=="Slovakia" | country=="Slovenia" ~ 4,
                            country=="Belarus" | country=="Estonia" |
                              country=="Latvia" | country=="Lithuania" |
                              country=="Romania" |
                              country=="Russia" | country=="Ukraine"  ~  5,
                            country=="Malta" | country=="Croatia" |
                              country=="Cyprus" | country=="Israel" ~ 6),
         Label = paste(Wave, sex, sep = " - ")) 


Single_Mean_Age_datasets$Region <- factor(Single_Mean_Age_datasets$Region, 
                                          levels = c(1,2,3,4,5,6),
                                          labels = c("Nordic countries",
                                                     "Western European countries",
                                                     "Southern European countries",
                                                     "Central Eastern European countries",
                                                     "Former Soviet Republics",
                                                     "Baltic + Israel"))




Figure_SMA_D <- ggplot(Single_Mean_Age_datasets, mapping = aes(x=reorder(country, -SMADisease),
                                                               y=SMADisease)) +
  #geom_bar(position = "dodge", stat = "identity") +
  geom_point(size=3)+
  facet_wrap(.~Label, scales = "free") +
  coord_flip() +
  ylim(50,65)+
  scale_color_manual(values = c("indianred3", "lightblue4")) +
  theme_classic() +
  theme(text = element_text(size = 16), 
        legend.position = "bottom",
        #legend.position=c(.95, 0.15),
        legend.background = element_rect(fill="transparent", 
                                         size=1, linetype="solid"),
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        axis.text.x = element_text(angle = 0, vjust = 0.9, hjust=.50),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(1,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  labs(#title = "B) Contribution to stagnation",
    color="Period",
    fill="Period",
    y="Single Mean Age at one disease",
    x="Country")
Figure_SMA_D
#ggsave(filename = "Figure 1 SHARE.JPG",
#       path= "Figures/Main figures/", 
#       dpi = 300, width = 8, height = 7,
#       bg = "transparent")




Figure_SMA_Multy <- ggplot(Single_Mean_Age_datasets, mapping = aes(x=reorder(country,-SMA_Multimorbidity),
                                                                  y=SMA_Multimorbidity,
                                                                  color=sex)) +
  #geom_bar(position = "dodge", stat = "identity") +
  geom_point(size=3) +
  facet_wrap(.~Label, scales = "free_y") +
  force_panelsizes(cols =c(2,2),
                   rows = c(1,2)) +
  coord_flip() +
  #ylim(50,65)+
  scale_color_manual(values = c("lightblue4","indianred")) +
  geom_hline(data=filter(Single_Mean_Age_datasets,  Label=="Wave 1 (2004) - Males"),
             aes(yintercept =  65.43670), linetype="dashed", color="black", size=1, alpha=.5) + # Males 2004
  geom_hline(data=filter(Single_Mean_Age_datasets,  Label=="Wave 1 (2004) - Females"),
             aes(yintercept =  65.22499), linetype="dashed", color="black", size=1, alpha=.5) + # Females 2004
  geom_hline(data=filter(Single_Mean_Age_datasets,  Label=="Wave 7 (2017) - Males"),
             aes(yintercept =  65.93010), linetype="dashed", color="black", size=1, alpha=.5) + # Males 2017
  geom_hline(data=filter(Single_Mean_Age_datasets,  Label=="Wave 7 (2017) - Females"),
             aes(yintercept =  66.86798), linetype="dashed", color="black", size=1, alpha=.5) + # Females 2017
  scale_y_continuous(breaks = c(seq(50,80,5))) +
  theme_classic() +
  theme_bw()+
  theme(text = element_text(size = 16), 
        legend.position = "",
        #legend.position=c(.30, 0.55),
        legend.background = element_rect(fill="transparent", 
                                         size=1, linetype="solid"),
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        axis.text.x = element_text(angle = 0, vjust = 0.9, hjust=.50),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(1,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  labs(#title = "B) Contribution to stagnation",
    color="",
    fill="",
    y="Single Mean Age at Multimorbidity",
    x="Country")
Figure_SMA_Multy
ggsave(filename = "Figure 1.JPG",
       path= "Figures/DR/Main/", 
       dpi = 300, width = 8, height = 8,
       bg = "transparent")



# ---------------------------------------------------------------------------- #
# Parity Progression
# ---------------------------------------------------------------------------- #

PPR_analysis_country <- SHARE_W1_W7_analysis %>% 
  filter(age_groups>=50) %>% 
  arrange(Wave, Year, country, sex,  age_groups, Num_diseases) %>% 
  group_by(Wave, Year, country, sex,  age_groups) %>% 
  mutate(Acu_pop = rev(cumsum(rev(Pop))),
         Pop_Multimorbidity = sum(Pop*Num_diseases),
         Total_Pop_upto_age_x = sum(Pop),
         Average_parity = round(Pop_Multimorbidity/Total_Pop_upto_age_x,2),
         Prop_at_least_1 = Acu_pop/Total_Pop_upto_age_x,
         Prop_at_least_1_n1 = lead(Prop_at_least_1),
         Parity_progression_ratio = Prop_at_least_1_n1/Prop_at_least_1,
         Year = round(Year,0),
         Wave = ifelse(Wave==1, "Wave 1 (2004)","Wave 7 (2017)"),
         Label = paste(Wave,sex, sep = " - ")) 


PPR_analysis_Europe <- SHARE_W1_W7_analysis %>% 
  group_by(Wave, Year, sex,  age_groups, Num_diseases) %>% 
  summarize(Pop=sum(Pop)) %>% 
  filter(age_groups>=50) %>% 
  arrange(Wave, Year, sex,  age_groups, Num_diseases) %>% 
  group_by(Wave, Year, sex,  age_groups) %>% 
  mutate(Acu_pop = rev(cumsum(rev(Pop))),
         Pop_Multimorbidity = sum(Pop*Num_diseases),
         Total_Pop_upto_age_x = sum(Pop),
         Average_parity = round(Pop_Multimorbidity/Total_Pop_upto_age_x,2),
         Prop_at_least_1 = Acu_pop/Total_Pop_upto_age_x,
         Prop_at_least_1_n1 = lead(Prop_at_least_1),
         Parity_progression_ratio = Prop_at_least_1_n1/Prop_at_least_1,
         Year = round(Year,0),
         country= "Europe",
         Wave = ifelse(Wave==1, "Wave 1 (2004)","Wave 7 (2017)"),
         Label = paste(Wave,sex, sep = " - "))




# ---------------------------------------------------------------------------- #
# Figure 2.Parity progression diseases
# ---------------------------------------------------------------------------- #


Data_PPR_Europe <- PPR_analysis_Europe %>% 
  dplyr::select(Wave, Year, sex,  age_groups,  Num_diseases, Parity_progression_ratio) %>% 
  rename(Parity_progression_ratio_Europe = Parity_progression_ratio)

Data_PPR_country <- PPR_analysis_country %>% 
  dplyr::select(Wave, Year, sex,  age_groups, country, Num_diseases, Parity_progression_ratio) 


Data_fig_3 <- Data_PPR_country %>% 
  left_join(Data_PPR_Europe) 

Data_fig_3_2017 <- Data_fig_3 %>% 
  filter(Wave=="Wave 7 (2017)" & sex=="Males") %>% 
  filter(Num_diseases<=4) %>% 
  filter(!is.na(Parity_progression_ratio))


Data_fig_3_2017_Europe <- Data_PPR_Europe %>% 
  #filter(Wave=="Wave 7 (2017)") %>% 
  filter(Num_diseases<=4) %>% 
  mutate(Age_cat = case_when(age_groups==50 ~ "50-54",
                             age_groups==55 ~ "55-59",
                             age_groups==60 ~ "60-64",
                             age_groups==65 ~ "65-69",
                             age_groups==70 ~ "70-74",
                             age_groups==75 ~ "75-79",
                             age_groups==80 ~ "80+"))

Data_fig_3_2017_Europe$Num_diseases  <- factor(Data_fig_3_2017_Europe$Num_diseases,
                                             levels = c(seq(0,4,1)),
                                             labels = c("From 0 to 1", "From 1 to 2",
                                                        "From 2 to 3", "From 3 to 4",
                                                        "From 4 to 5"))

Figure_2 <- ggplot(Data_fig_3_2017_Europe) +
  #geom_point(aes(x=age_groups, y = Parity_progression_ratio, color=factor(country))) +
  #geom_line(aes(x=age_groups, y = Parity_progression_ratio, group=factor(country), color=factor(country))) +
  geom_point(aes(x=age_groups, y = Parity_progression_ratio_Europe, color=sex), size=3) +
  geom_line(aes(x=age_groups, y = Parity_progression_ratio_Europe, linetype=Wave, color=sex), size=1.3)  +
  
  facet_wrap(.~Num_diseases, ncol = 2) +
  scale_color_manual(values = c("indianred3", "lightblue4")) +
  scale_x_continuous(breaks = c(seq(50,80,5))) +
  geom_hline(yintercept = 0.5, linetype="dashed") +
  theme_classic() +
  theme(text = element_text(size = 20), 
        #legend.position = "",
        legend.position=c(.75, 0.12),
        legend.background = element_rect(fill="transparent", 
                                         size=1, linetype="solid"),
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        axis.text.x = element_text(angle = 0, vjust = 0.9, hjust=0.5),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(1,"cm"),
        legend.title = element_text(color = "Black", size = 18),
        legend.text = element_text(color = "Black", size = 18)) +
  guides(color=guide_legend(ncol=1)) +
  labs(#title = "A) Males after 2010",
       #caption="Source data: ONS Longitudinal Study, Statistics Finland & Turin Longitudinal Study",
       linetype = "Wave & Year",
       color = "Sex",
       y="Parity progresion ratio",
       x="Age groups")
Figure_2
ggsave(filename = "Figure 2.JPG",
       path= "Figures/DR/Main/", 
       dpi = 300, width = 8, height = 8,
       bg = "transparent")



# ---------------------------------------------------------------------------- #
# Figure 3. Average number of diseases
# ---------------------------------------------------------------------------- #

Data_Average_parity_Europe <- PPR_analysis_Europe %>% 
  filter(Num_diseases==0) %>% 
  dplyr::select(Wave, Year, sex,  age_groups,  Average_parity) %>% 
  rename(Average_parity_Europe = Average_parity)

Data_Average_parity_country <- PPR_analysis_country %>% 
  filter(Num_diseases==0) %>% 
  dplyr::select(Wave, Year, sex,  age_groups, country, Label, Average_parity) 


Data_Figure_Average_parity <- Data_Average_parity_country %>% 
  left_join(Data_Average_parity_Europe)

Data_Figure_Average_parity$age_groups <- factor(Data_Figure_Average_parity$age_groups,
                                                levels = c(seq(50,80,5)),
                                                labels = c("50-54", "55-59",
                                                           "60-64", "65-69",
                                                           "70-74", "75-79",
                                                           "80+"))


Figure_3_males <- ggplot(Data_Figure_Average_parity) +
  geom_point(aes(x=age_groups, y = Average_parity, color=factor(country)), size=3) +
  geom_line(aes(x=age_groups, y = Average_parity, group=factor(country), color=factor(country)), size=1.3) +
  geom_point(aes(x=age_groups, y = Average_parity_Europe, color="Europe"), size=3) +
  geom_line(aes(x=age_groups, y = Average_parity_Europe, group="Europe", color="Europe"), size=1.3) +
  facet_wrap(.~Label, ncol = 2) +
  scale_color_manual(values = c(rep("grey80",28),"indianred")) +
  scale_y_continuous(breaks = c(seq(0,3,.5))) +
  geom_hline(yintercept = 1, linetype="dashed") +
  theme_classic() +
  theme(text = element_text(size = 20), 
        legend.position = "",
        #legend.position=c(.85, 0.05),
        legend.background = element_rect(fill="transparent", 
                                         size=1, linetype="solid"),
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        axis.text.x = element_text(angle = 0, vjust = 0.9, hjust=0.5),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(1,"cm"),
        legend.title = element_text(color = "Black", size = 18),
        legend.text = element_text(color = "Black", size = 18)) +
  guides(color=guide_legend(ncol=5)) +
  labs(#title = "A) Males",
    #caption="Source data: ONS Longitudinal Study, Statistics Finland & Turin Longitudinal Study",
    color = "Countries",
    y="Average number of diseases",
    x="Age groups")
Figure_3_males
ggsave(filename = "Figure 3.JPG",
       path= "Figures/DR/Main/", 
       dpi = 300, width = 11, height = 10,
       bg = "transparent")



################################################################################
################################################################################
#
#     SUPPLEMENTARY FIGURES
#
################################################################################
################################################################################

SMA_Multimorbidity_Europe_sensitivity

# -----------------------
# Combine Single mean ages
# -----------------------

SMAM_sensitivity <- SMA_Multimorbidity_country_sensitviity %>% 
  mutate(Year = round(Year,0),
         Wave = ifelse(Year==2004,"Wave 1 (2004)","Wave 7 (2017)"),
         Label = paste(Wave, sex, sep = " - ")) 


Figure_SMA_Multy_sensitivity <- ggplot(SMAM_sensitivity, mapping = aes(x=reorder(country,-SMA_Multimorbidity),
                                                                   y=SMA_Multimorbidity,
                                                                   color=sex)) +
  #geom_bar(position = "dodge", stat = "identity") +
  geom_point(size=3) +
  facet_wrap(.~Label, scales = "free_y") +
  #force_panelsizes(cols =c(2,2),
  #                 rows = c(1,2)) +
  coord_flip() +
  #ylim(50,65)+
  scale_color_manual(values = c("lightblue4","indianred")) +
  geom_hline(data=filter(SMAM_sensitivity,  Label=="Wave 1 (2004) - Males"),
             aes(yintercept =  65.43670), linetype="dashed", color="black", size=1, alpha=.5) + # Males 2004
  geom_hline(data=filter(SMAM_sensitivity,  Label=="Wave 1 (2004) - Females"),
             aes(yintercept =  65.22499), linetype="dashed", color="black", size=1, alpha=.5) + # Females 2004
  geom_hline(data=filter(SMAM_sensitivity,  Label=="Wave 7 (2017) - Males"),
             aes(yintercept =  66.35323), linetype="dashed", color="black", size=1, alpha=.5) + # Males 2017
  geom_hline(data=filter(SMAM_sensitivity,  Label=="Wave 7 (2017) - Females"),
             aes(yintercept =  68.47144), linetype="dashed", color="black", size=1, alpha=.5) + # Females 2017
  scale_y_continuous(breaks = c(seq(50,80,5))) +
  theme_classic() +
  theme_bw()+
  theme(text = element_text(size = 16), 
        legend.position = "",
        #legend.position=c(.30, 0.55),
        legend.background = element_rect(fill="transparent", 
                                         size=1, linetype="solid"),
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        axis.text.x = element_text(angle = 0, vjust = 0.9, hjust=.50),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(1,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  labs(#title = "B) Contribution to stagnation",
    color="",
    fill="",
    y="Single Mean Age at Multimorbidity",
    x="Country")
Figure_SMA_Multy_sensitivity
ggsave(filename = "Figure S1 - Sensitivity.JPG",
       path= "Figures/DR/Supplementary/", 
       dpi = 300, width = 8, height = 8,
       bg = "transparent")


# ---------------------------------------------------------------------------- #
# Figure S1. Average number of diseases males wave 1
# ---------------------------------------------------------------------------- #


Data_Figure_Average_parity_long <- Data_Figure_Average_parity %>% 
  pivot_longer(!c(Wave, Year, sex, age_groups, country, Label),
               values_to = "Average_parity",
               names_to = "Region")

Data_Figure_Average_males_Wave_1 <- Data_Figure_Average_parity_long %>% 
  filter(sex=="Males" & Wave=="Wave 1 (2004)")

Figure_S1 <- ggplot(Data_Figure_Average_males_Wave_1) +
  geom_point(aes(x=age_groups, y = Average_parity,  group=Region, color=Region), size=3) +
  geom_line(aes(x=age_groups, y = Average_parity, group=Region, color=Region), size=1.3) +
  facet_wrap(.~country, ncol = 4) +
  scale_color_manual(values = c("grey80","indianred")) +
  scale_y_continuous(breaks = c(seq(0,3,.5))) +
  geom_hline(yintercept = 1, linetype="dashed") +
  theme_classic() +
  theme(text = element_text(size = 20), 
        legend.position = "",
        #legend.position=c(.85, 0.05),
        legend.background = element_rect(fill="transparent", 
                                         size=1, linetype="solid"),
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=0.5),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(1,"cm"),
        legend.title = element_text(color = "Black", size = 18),
        legend.text = element_text(color = "Black", size = 18)) +
  guides(color=guide_legend(ncol=5)) +
  labs(title = "Wave 1 (2004) - Males",
    #caption="Source data: ONS Longitudinal Study, Statistics Finland & Turin Longitudinal Study",
    color = "Countries",
    y="Average number of diseases",
    x="Age groups")
Figure_S1
ggsave(filename = "Figure S1.JPG",
       path= "Figures/DR/Supplementary/", 
       dpi = 300, width = 11, height = 10,
       bg = "transparent")


Data_Figure_Average_females_Wave_1 <- Data_Figure_Average_parity_long %>% 
  filter(sex=="Females" & Wave=="Wave 1 (2004)")

Figure_S2 <- ggplot(Data_Figure_Average_females_Wave_1) +
  geom_point(aes(x=age_groups, y = Average_parity,  group=Region, color=Region), size=3) +
  geom_line(aes(x=age_groups, y = Average_parity, group=Region, color=Region), size=1.3) +
  facet_wrap(.~country, ncol = 4) +
  scale_color_manual(values = c("grey80","indianred")) +
  scale_y_continuous(breaks = c(seq(0,3,.5))) +
  geom_hline(yintercept = 1, linetype="dashed") +
  theme_classic() +
  theme(text = element_text(size = 20), 
        legend.position = "",
        #legend.position=c(.85, 0.05),
        legend.background = element_rect(fill="transparent", 
                                         size=1, linetype="solid"),
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=0.5),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(1,"cm"),
        legend.title = element_text(color = "Black", size = 18),
        legend.text = element_text(color = "Black", size = 18)) +
  guides(color=guide_legend(ncol=5)) +
  labs(title = "Wave 1 (2004) - Females",
       #caption="Source data: ONS Longitudinal Study, Statistics Finland & Turin Longitudinal Study",
       color = "Countries",
       y="Average number of diseases",
       x="Age groups")
Figure_S2
ggsave(filename = "Figure S2.JPG",
       path= "Figures/DR/Supplementary/", 
       dpi = 300, width = 11, height = 10,
       bg = "transparent")


Data_Figure_Average_males_Wave_7 <- Data_Figure_Average_parity_long %>% 
  filter(sex=="Males" & Wave=="Wave 7 (2017)")

Figure_S3 <- ggplot(Data_Figure_Average_males_Wave_7) +
  geom_point(aes(x=age_groups, y = Average_parity,  group=Region, color=Region), size=3) +
  geom_line(aes(x=age_groups, y = Average_parity, group=Region, color=Region), size=1.3) +
  facet_wrap(.~country, ncol = 4) +
  scale_color_manual(values = c("grey80","indianred")) +
  scale_y_continuous(breaks = c(seq(0,3,.5))) +
  geom_hline(yintercept = 1, linetype="dashed") +
  theme_classic() +
  theme(text = element_text(size = 20), 
        legend.position = "",
        #legend.position=c(.85, 0.05),
        legend.background = element_rect(fill="transparent", 
                                         size=1, linetype="solid"),
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=0.5),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(1,"cm"),
        legend.title = element_text(color = "Black", size = 18),
        legend.text = element_text(color = "Black", size = 18)) +
  guides(color=guide_legend(ncol=5)) +
  labs(title = "Wave 7 (2017) - Males",
       #caption="Source data: ONS Longitudinal Study, Statistics Finland & Turin Longitudinal Study",
       color = "Countries",
       y="Average number of diseases",
       x="Age groups")
Figure_S3
ggsave(filename = "Figure S3.JPG",
       path= "Figures/DR/Supplementary/", 
       dpi = 300, width = 11, height = 13,
       bg = "transparent")

Data_Figure_Average_females_Wave_7 <- Data_Figure_Average_parity_long %>% 
  filter(sex=="Females" & Wave=="Wave 7 (2017)")

Figure_S4 <- ggplot(Data_Figure_Average_females_Wave_7) +
  geom_point(aes(x=age_groups, y = Average_parity,  group=Region, color=Region), size=3) +
  geom_line(aes(x=age_groups, y = Average_parity, group=Region, color=Region), size=1.3) +
  facet_wrap(.~country, ncol = 4) +
  scale_color_manual(values = c("grey80","indianred")) +
  scale_y_continuous(breaks = c(seq(0,3,.5))) +
  geom_hline(yintercept = 1, linetype="dashed") +
  theme_classic() +
  theme(text = element_text(size = 20), 
        legend.position = "",
        #legend.position=c(.85, 0.05),
        legend.background = element_rect(fill="transparent", 
                                         size=1, linetype="solid"),
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=0.5),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(1,"cm"),
        legend.title = element_text(color = "Black", size = 18),
        legend.text = element_text(color = "Black", size = 18)) +
  guides(color=guide_legend(ncol=5)) +
  labs(title = "Wave 7 (2017) - Females",
       #caption="Source data: ONS Longitudinal Study, Statistics Finland & Turin Longitudinal Study",
       color = "Countries",
       y="Average number of diseases",
       x="Age groups")
Figure_S4
ggsave(filename = "Figure S4.JPG",
       path= "Figures/DR/Supplementary/", 
       dpi = 300, width = 11, height = 13,
       bg = "transparent")

