################################################################################
# Article: Multimorbidity parity progression ratio in Mexico
# Title:   20. Prevalence of diseases
# Authors: Daniel,
# Data:    MHAS, CRELES, SHARE, HRS & LASI
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

SHARE_prevalences_countries <- read_excel("Demographic Research/Excel/Prevalences diseases countries.xlsx")

SHARE_prevalences_Europe <- read_excel("Demographic Research/Excel/Prevalences diseases Europe.xlsx")


# --------------------------------------------
#  2.2.3. Combine weighted data
# --------------------------------------------

Data_Europe <- SHARE_prevalences_Europe %>% 
  mutate(Diseases_label = case_when(Disease=="Arthritis" ~ "Arthritis",
                                    Disease=="Cancer" ~ "Cancer",
                                    Disease=="Chronic_lung" ~ "Chroninc lung diseases",
                                    Disease=="Diabetes" ~ "Diabetes",
                                    Disease=="HB_cholesterol" ~ "High-blood cholesterol",
                                    Disease=="HB_Hypertension" ~ "High-blood hypertension",
                                    Disease=="Heart_attack" ~ "Heart attack",
                                    Disease=="Stroke" ~ "Stroke"),
         sexo = ifelse(Sex=="Males",1,2))


Data_Figure_A1 <- SHARE_prevalences_countries %>% 
  mutate(Diseases_label = case_when(Disease=="Arthritis" ~ "Arthritis",
                                    Disease=="Cancer" ~ "Cancer",
                                    Disease=="Chronic_lung" ~ "Chroninc lung diseases",
                                    Disease=="Diabetes" ~ "Diabetes",
                                    Disease=="HB_cholesterol" ~ "High-blood cholesterol",
                                    Disease=="HB_Hypertension" ~ "High-blood hypertension",
                                    Disease=="Heart_attack" ~ "Heart attack",
                                    Disease=="Stroke" ~ "Stroke"),
         sexo = ifelse(Sex=="Males",1,2))

# -------------------------
#. Figure A1
# -------------------------

SHARE_prevalences_W1 <- Data_Figure_A1 %>% 
  filter(Wave==1)


SHARE_prevalences_W1$sexo <- factor(SHARE_prevalences_W1$sexo,
                                   levels = c(1,2),
                                   labels = c("Males","Females"))

Figure_A1 <- ggplot(SHARE_prevalences_W1, mapping = aes(x=reorder(Diseases_label,Mean*100),
                                                        y=Mean*100,
                                                        color= factor(sexo),
                                                        fill = factor(sexo))) +
  geom_bar(position = "dodge", stat = "identity", 
           alpha=0.7) +
  geom_errorbar(aes(ymin=Low*100, ymax=Upper*100), width=.2,
                position=position_dodge(.9)) +
  facet_wrap(.~Country, ncol = 3) +
  coord_flip() +
  theme_classic() +
  theme_bw() +
  scale_fill_manual(values = c("lightblue","indianred")) +
  scale_color_manual(values = c("black","black")) +
  scale_y_continuous(breaks = c(seq(0,60,10))) +
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
    color="Sex",
    fill="Sex",
    y="Prevalence (%) - weighted",
    x="Diseases")
Figure_A1
ggsave(filename = "Figure A1.JPG",
       path= "Figures/DR/Appendix/", 
       dpi = 300, width = 8, height = 8,
       bg = "transparent")


# -------------------------
#. Figure A2
# -------------------------

SHARE_prevalences_W7 <- Data_Figure_A1 %>% 
  filter(Wave==7)


SHARE_prevalences_W7$sexo <- factor(SHARE_prevalences_W7$sexo,
                                    levels = c(1,2),
                                    labels = c("Males","Females"))

Figure_A2 <- ggplot(SHARE_prevalences_W7, mapping = aes(x=reorder(Diseases_label,Mean*100),
                                                        y=Mean*100,
                                                        color= factor(sexo),
                                                        fill = factor(sexo))) +
  geom_bar(position = "dodge", stat = "identity", 
           alpha=0.7) +
  geom_errorbar(aes(ymin=Low*100, ymax=Upper*100), width=.2,
                position=position_dodge(.9)) +
  facet_wrap(.~Country, ncol = 3) +
  coord_flip() +
  theme_classic() +
  theme_bw() +
  scale_fill_manual(values = c("lightblue","indianred")) +
  scale_color_manual(values = c("black","black")) +
  scale_y_continuous(breaks = c(seq(0,60,10))) +
  theme(text = element_text(size = 14), 
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
    color="Sex",
    fill="Sex",
    y="Prevalence (%) - weighted",
    x="Diseases")
Figure_A2
ggsave(filename = "Figure A2.JPG",
       path= "Figures/DR/Appendix/", 
       dpi = 300, width = 8, height = 11,
       bg = "transparent")



# -------------------------
#. Figure A2
# -------------------------

Data_Europe$sexo <- factor(Data_Europe$sexo,
                                    levels = c(1,2),
                                    labels = c("Males","Females"))

Data_Europe$Wave <- factor(Data_Europe$Wave,
                           levels = c(1,7),
                           labels = c("Wave 1 (2004)","Wave 7 (2014)"))


Figure_A3 <- ggplot(Data_Europe, mapping = aes(x=reorder(Diseases_label,Mean*100),
                                                        y=Mean*100,
                                                        color= factor(Wave),
                                                        fill = factor(Wave))) +
  geom_bar(position = "dodge", stat = "identity", 
           alpha=0.7) +
  geom_errorbar(aes(ymin=Low*100, ymax=Upper*100), width=.2,
                position=position_dodge(.9)) +
  facet_wrap(.~sexo, ncol = 3) +
  coord_flip() +
  theme_classic() +
  theme_bw() +
  scale_fill_manual(values = c("lightblue","indianred")) +
  scale_color_manual(values = c("black","black")) +
  scale_y_continuous(breaks = c(seq(0,60,10))) +
  theme(text = element_text(size = 14), 
        #legend.position = "bottom",
        legend.position=c(.30, 0.20),
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
    color="Wave",
    fill="Wave",
    y="Prevalence (%) - weighted",
    x="Diseases")
Figure_A3
ggsave(filename = "Figure A3.JPG",
       path= "Figures/DR/Appendix/", 
       dpi = 300, width = 8, height = 6,
       bg = "transparent")
