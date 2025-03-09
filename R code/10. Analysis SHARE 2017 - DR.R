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
#   1.1 Prepare data SHARE wave 7
# ----------------------

# Prepare data
SHARE_W7_pop <- read_stata("Data/FinalSHARE/SHARE_W7_analysis.dta")

# Country's label
SHARE_W7_pop$country <- factor(SHARE_W7_pop$country,
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

# 
SHARE_W7_analysis <- SHARE_W7_pop %>% 
  mutate(Pop = 1,
         sex = ifelse(gender==1,"Males","Females"),
         age_groups = case_when(age2017>=50 & age2017<=54 ~ 50,
                             age2017>=55 & age2017<=59 ~ 55,
                             age2017>=60 & age2017<=64 ~ 60,
                             age2017>=65 & age2017<=69 ~ 65,
                             age2017>=70 & age2017<=74 ~ 70,
                             age2017>=75 & age2017<=79 ~ 75,
                             age2017>=80 & age2017<=84 ~ 80,
                             age2017>=85 & age2017<=89 ~ 85,
                             age2017>=90 & age2017<=110 ~ 90,
                             age2017<50 ~999),
         Num_diseases = Heart_attack_dico + HB_pressure_or_hypertension_dico + 
           HB_cholesterol_dico + Stroke_dico + Diabetes_dico + Chronic_lung_dico + 
           Arthritis_dico + Cancer_dico) %>% 
  filter(country!="Israel") %>% 
  filter(!is.na(cciw_w7)) %>% 
  mutate(ID_sample = paste(psu,ssu, sep = " "),
         Final_stratum = paste(stratum1,stratum2, sep = " "),
         Multimorbidity =  case_when(Num_diseases==0 ~ 0,
                                     Num_diseases==1 ~ 1,
                                     Num_diseases>=2 ~ 2)) %>% 
  dplyr::select(country, ID_sample, psu,ssu, cciw_w7, stratum1, stratum2,Final_stratum, sex, age2017,Multimorbidity, age_groups, Num_diseases, Pop)






# ----------------------
#   1.2 Population weighted data
# ----------------------

options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")


SHARE_svydesing_2017 <-  svydesign(id = ~psu+ssu, weights = ~cciw_w7, strata = ~stratum1+stratum2, 
                                 nest = TRUE, survey.lonely.psu = "adjust", data = SHARE_W7_analysis)


SHARE_pop_2017 <-  svyby(~Pop,~sex+Num_diseases+age_groups, design = SHARE_svydesing_2017, FUN = svytotal)

SHARE_pop_2017 <- SHARE_pop_2017 %>% 
  mutate(Wave = 7,
         Year = 2017)

# ---------------------------------------------------------------------------- #
#  2. Analysis
# ---------------------------------------------------------------------------- #

Mean_diseases <-  svyby(~age2017,~sex+Multimorbidity, design = SHARE_svydesing_2017, FUN = svymean)

Mean_diseases_wider <- Mean_diseases %>% 
  dplyr::select(-se) %>% 
  pivot_wider(names_from = sex,
              values_from = age2017)



# Single mean age at multimorbidity
SMA_Multimorbidity_country <- SHARE_pop_2017 %>% 
  mutate(Disease_free = ifelse(Num_diseases>=2,"Disease","No_disease")) %>% 
  group_by(Wave, Year, sex,  age_groups, Disease_free) %>% 
  summarize(Pop = sum(Pop)) %>% 
  arrange(Wave, Year,  sex,  age_groups) %>% 
  pivot_wider(names_from = Disease_free,
              values_from = Pop) %>% 
  mutate(Disease = ifelse(is.na(Disease),0,Disease),
         No_disease = ifelse(is.na(No_disease),0,No_disease),
         Prop_nodisease = No_disease/(Disease+ No_disease),
         AP_x_i = Prop_nodisease*5,
         Never_diseas_90 = case_when(age_groups==90 ~ Prop_nodisease,
                                     age_groups!=90  ~ 0),
         Disease_90 = 1 - Never_diseas_90) %>% 
  group_by(Wave,Year,   sex) %>% 
  mutate(Perons_year =  50 + sum(AP_x_i),
         Perons_year_90 = 90*Never_diseas_90) %>% 
  filter(age_groups==90) %>% 
  mutate(SMA_Multimorbidity = (Perons_year-Perons_year_90)/Disease_90) %>% 
  dplyr::select(Wave, Year,  sex, SMA_Multimorbidity)


# Parity Progression ratio
PPR_analysis_Europe <- SHARE_pop_2017 %>% 
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
#  3. Figures
# ---------------------------------------------------------------------------- #

# -------------------
# Figure 1
# -------------------

Data_Figure_1 <- SMA_Multimorbidity_country 

Figure_1 <- ggplot(Data_Figure_1, mapping = aes(x=sex, y=SMA_Multimorbidity,
                                                color=sex,
                                                fill=sex)) +
  geom_bar(position = "dodge", stat = "identity") +
  
  #facet_wrap(.~Num_diseases, ncol = 2) +
  scale_color_manual(values = c("indianred3", "lightblue4")) +
  #scale_x_continuous(breaks = c(seq(50,90,5))) +
  #geom_hline(yintercept = 0.5, linetype="dashed") +
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
    linetype = "Sex",
    color = "Sex",
    y="Singulate Mean-Age at Multimorbidity",
    x="Sex")
Figure_1



# -------------------
# Figure 2
# -------------------

Data_Figure_2 <- PPR_analysis_Europe %>% 
  filter(Num_diseases<=4)


Data_Figure_2$Num_diseases  <- factor(Data_Figure_2$Num_diseases,
                                               levels = c(seq(0,4,1)),
                                               labels = c("From 0 to 1", "From 1 to 2",
                                                          "From 2 to 3", "From 3 to 4",
                                                          "From 4 to 5"))



Figure_2 <- ggplot(Data_Figure_2) +
  #geom_point(aes(x=age_groups, y = Parity_progression_ratio, color=factor(country))) +
  #geom_line(aes(x=age_groups, y = Parity_progression_ratio, group=factor(country), color=factor(country))) +
  geom_point(aes(x=age_groups, y = Parity_progression_ratio, color=sex), size=3) +
  geom_line(aes(x=age_groups, y = Parity_progression_ratio, color=sex), size=1.3)  +
  
  facet_wrap(.~Num_diseases, ncol = 2) +
  scale_color_manual(values = c("indianred3", "lightblue4")) +
  scale_x_continuous(breaks = c(seq(50,90,5))) +
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

# -------------------
# Figure 3
# -------------------

Data_Figure_3 <- PPR_analysis_Europe

#Data_Figure_3$age_groups <- factor(Data_Figure_3$age_groups,
#                                   levels = c(seq(50,90,5)),
#                                   labels = c("50-54", "55-59",
#                                              "60-64", "65-69",
#                                              "70-74", "65-69",
#                                              "80-84", "85-89",
#                                              "90+"))


Figure_3_males <- ggplot(Data_Figure_3) +
  geom_point(aes(x=age_groups, y = Average_parity, color=sex), size=3) +
  geom_line(aes(x=age_groups, y = Average_parity, color=sex), size=1.3, alpha=0.7) +
  geom_text(aes(x=age_groups, y = Average_parity,color=sex,
                label = round(Average_parity,2),
                vjust =-1.9)) +
  facet_wrap(.~sex, ncol = 2) +
  #scale_color_manual(values = c(rep("grey80",28),"indianred")) +
  scale_color_manual(values = c("indianred3", "lightblue4")) +
  ylim(0.5,2) +
  scale_x_continuous(breaks = seq(50,90,5)) +
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
       dpi = 300, width = 8, height = 6,
       bg = "transparent")






