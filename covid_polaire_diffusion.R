
#######################
###### Packages #######
#######################

# Installation des packages (à ne faire qu'une seule fois)
install.packages(c("tidyverse","doBy","ggplot2","gganimate","broom","tibble","dplyr",
                   "lubridate","stringr","av","glue"))

# Chargement des packages
library(tidyverse)
library(doBy)
library(ggplot2)
library(gganimate)
library(broom)
library(tibble)
library(dplyr)
library(lubridate)
library(stringr)
library(av)
library(glue)

#########################
### Traitements covid ###
#########################

# Source file : https://statbel.fgov.be/sites/default/files/files/opendata/bevolking/TF_DEATHS.zip
TF_DEATHS <- read.csv("FOLDER/TF_DEATHS.txt", sep=";")
# Source file : https://statbel.fgov.be/sites/default/files/files/opendata/deathday/DEMO_DEATH_OPEN.zip
TF_DEATHS_2020 <- read.csv("FOLDER/DEMO_DEATH_OPEN.txt", sep=";")

TF_DEATHS$DT_DTH <- as.Date(TF_DEATHS$DT_DTH,format = "%d/%m/%Y")
TF_DEATHS_2020$DT_DATE <- as.Date(TF_DEATHS_2020$DT_DATE,format = "%d/%m/%Y")

TF_DEATHS_2020 <- TF_DEATHS_2020[format(TF_DEATHS_2020$DT_DATE, format = "%Y") == "2020",]
TF_DEATHS_2020 <- summaryBy(MS_NUM_DEATH ~ DT_DATE, data=TF_DEATHS_2020,FUN=sum,keep.names = TRUE)

TF_DEATHS_2020$DT_DTH <- TF_DEATHS_2020$DT_DATE
TF_DEATHS_2020$MS_NUM_DEATHS <- TF_DEATHS_2020$MS_NUM_DEATH

TF_DEATHS_2020 <- subset(TF_DEATHS_2020,select = c(DT_DTH,MS_NUM_DEATHS))

TF_DEATHS <- rbind(TF_DEATHS,TF_DEATHS_2020)

TF_DEATHS$DT_DAY <- format(TF_DEATHS$DT_DTH, format = "%d")
TF_DEATHS$DT_MONTH <- format(TF_DEATHS$DT_DTH, format = "%m")
TF_DEATHS$DT_YEAR <- format(TF_DEATHS$DT_DTH, format = "%Y")
TF_DEATHS$DT_DTH_YEAR <- as.Date(format(TF_DEATHS$DT_DTH, format = "%d/%m"),format = "%d/%m")

#####################
### Graph basique ###
#####################

ggplot()+
  aes(x = DT_DTH_YEAR, y = MS_NUM_DEATHS)+
  ylim(0, max(TF_DEATHS$MS_NUM_DEATHS))+
  xlab("Jour")+
  ylab("Nombre de décès")+
  ggtitle("")+
  labs(title = "Nombre quotidien de décès en Belgique (2009-2020)",
       subtitle = "Année 2020 en rouge",
       caption = "Data source: open Data Statbel ; sur une idée de visualisation de Baptiste Coulmont")+
  geom_line(data = subset(TF_DEATHS,DT_YEAR == "2009"))+
  geom_line(data = subset(TF_DEATHS,DT_YEAR == "2010"))+
  geom_line(data = subset(TF_DEATHS,DT_YEAR == "2011"))+
  geom_line(data = subset(TF_DEATHS,DT_YEAR == "2012"))+
  geom_line(data = subset(TF_DEATHS,DT_YEAR == "2013"))+
  geom_line(data = subset(TF_DEATHS,DT_YEAR == "2014"))+
  geom_line(data = subset(TF_DEATHS,DT_YEAR == "2015"))+
  geom_line(data = subset(TF_DEATHS,DT_YEAR == "2016"))+
  geom_line(data = subset(TF_DEATHS,DT_YEAR == "2017"))+
  geom_line(data = subset(TF_DEATHS,DT_YEAR == "2018"))+
  geom_line(data = subset(TF_DEATHS,DT_YEAR == "2019"))+
  geom_line(data = subset(TF_DEATHS,DT_YEAR == "2020"),col=2)+
  coord_polar()+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line("grey70"),
        panel.grid.minor = element_line("grey70")
  )

ggplot()+
  aes(x = DT_DTH_YEAR, y = MS_NUM_DEATHS)+
  ylim(0, max(TF_DEATHS$MS_NUM_DEATHS))+
  xlab("Jour")+
  ylab("Nombre de décès")+
  ggtitle("")+
  labs(title = "Nombre quotidien de décès en Belgique (2009-2020)",
       subtitle = "Année 2020 en rouge",
       caption = "Data source: open Data Statbel")+
  geom_line(data = subset(TF_DEATHS,DT_YEAR == "2009"))+
  geom_line(data = subset(TF_DEATHS,DT_YEAR == "2010"))+
  geom_line(data = subset(TF_DEATHS,DT_YEAR == "2011"))+
  geom_line(data = subset(TF_DEATHS,DT_YEAR == "2012"))+
  geom_line(data = subset(TF_DEATHS,DT_YEAR == "2013"))+
  geom_line(data = subset(TF_DEATHS,DT_YEAR == "2014"))+
  geom_line(data = subset(TF_DEATHS,DT_YEAR == "2015"))+
  geom_line(data = subset(TF_DEATHS,DT_YEAR == "2016"))+
  geom_line(data = subset(TF_DEATHS,DT_YEAR == "2017"))+
  geom_line(data = subset(TF_DEATHS,DT_YEAR == "2018"))+
  geom_line(data = subset(TF_DEATHS,DT_YEAR == "2019"))+
  geom_line(data = subset(TF_DEATHS,DT_YEAR == "2020"),col=2)+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line("grey70"),
        panel.grid.minor = element_line("grey70")
  )

###########################################
# ajout des informations pour l'animation #
###########################################

TF_DEATHS <- subset(TF_DEATHS,as.numeric(TF_DEATHS$DT_YEAR) >= 2001)

dc <- as_tibble(TF_DEATHS)
dc$jour <- as.numeric(dc$DT_DAY)
dc$mois <- as.numeric(dc$DT_MONTH)
dc$annee <- as.numeric(dc$DT_YEAR)
dc$date_fictive <- dc$DT_DTH_YEAR

dc <- orderBy(~DT_DTH,data=dc)
dc$N <- dc$MS_NUM_DEATHS

# correction année et mois pour les données de 2020
dc <- dc %>%
  mutate(jour = ifelse(annee==2020, day(date_fictive),jour),
         mois = ifelse(annee==2020, month(date_fictive),mois))

# ralentir 2020
# 4 fois plus lent
dc <- bind_rows( dc %>% filter(annee != 2020) %>% mutate(repetition = 0),
                 dc %>% filter(annee == 2020) %>% mutate(repetition = 1),
                 dc %>% filter(annee == 2020) %>% mutate(repetition = 2),
                 dc %>% filter(annee == 2020) %>% mutate(repetition = 3),
                 dc %>% filter(annee == 2020) %>% mutate(repetition = 4) ) %>%
  arrange(annee,mois,jour) %>%
  mutate(groupage = as.numeric(str_sub(annee,3,4))  ) %>%
  group_by(annee) %>%
  mutate(numero_frame = row_number()) %>%
  ungroup() %>%
  mutate(numero_frame = 366*(groupage-1) + numero_frame) %>%
  mutate(texte = annee)

# pour éviter que le label de l'année bouge dans tous les sens
# on le trace sur la courbe "loess"
dc <- dc %>%
  left_join( dc %>% group_by(annee) %>% 
               do(augment(loess(N ~ numero_frame, .,span=.1))) %>% 
               ungroup() %>%
               select(annee,numero_frame,.fitted) ,
             by=c("annee","numero_frame"))

######################
# Paramètrage langue #
######################

title_fr <- "Nombre quotidien de décès en Belgique, 2001-2020"
subtitle_fr <- "En rouge, l’année 2020, en gris, les années 2001 à 2019."
sources_fr <- "Sources : Statbel, Open data du nombre de décès par jour | Graphique : T. Delclite (sur une idée de B. Coulmont)"

title_nl <- "Dagelijks aantal sterfgevallen in België, 2001-2020"
subtitle_nl <- "In het rood, het jaar 2020, in het grijs, de jaren 2001 tot 2019."
sources_nl <- "Bronnen: Statbel, Open data van het aantal sterfgevallen per dag | Grafiek: T. Delclite (gebaseerd op een idee van B. Coulmont)"

title_de <- "Tägliche Zahl der Todesfälle in Belgien, 2001-2020"
subtitle_de <- "In roter Farbe das Jahr 2020, in grauer Farbe die Jahre 2001 bis 2019."
sources_de <- "Quellen: Statbel, Open Data zur Anzahl der Todesfälle pro Tag | Grafik: T. Delclite (nach einer Idee von B. Coulmont)"

title_en <- "Daily number of deaths in Belgium, 2001-2020"
subtitle_en <- "In red, the year 2020, in grey, the years 2001 to 2019."
sources_en <- "Sources : Statbel, Open data on the number of deaths per day | Graph: T. Delclite (idea of B. Coulmont)"

###################
# graphique animé #
###################

graph_anime <- function(langue,fps,nframes=nrow(dc),dir_sauv = "")
{
  if (langue == "fr"){
    title = title_fr
    subtitle = subtitle_fr
    sources <- sources_fr
  }
  if (langue == "nl"){
    title = title_nl
    subtitle = subtitle_nl
    sources <- sources_nl
  }
  if (langue == "de"){
    title = title_de
    subtitle = subtitle_de
    sources <- sources_de
  }
  if (langue == "en"){
    title = title_en
    subtitle = subtitle_en
    sources <- sources_en
  }
  
  p <- dc %>%
    mutate(couleur = case_when(annee == 2020 ~ "orangered",TRUE ~ "gray"),
           couleur_texte = ifelse(annee<2020,"black","orangered"),
           transparence = case_when(annee %in% c(2020) ~ 1,TRUE ~ 0.5)
    ) %>%
    ggplot(aes(date_fictive, N, group = annee, 
               color=I(couleur), alpha=I(transparence)))+
    geom_line(size=0.5, aes(group=annee)) +
    geom_point(color="black") +
    geom_text(aes(date_fictive,.fitted,
                  group=annee,
                  label = texte,
                  color = I(couleur_texte)),
              size=6,nudge_x = 25, alpha=1 ) +
    coord_polar() +
    scale_x_date(date_breaks = "1 month", date_labels = "%b", expand=expansion(add=c(0,-26))) +
    scale_y_continuous(limits = c(0,800),
                       breaks = c(0,200,400,600,800),
                       minor_breaks = NULL
    ) +
    labs(title = title,
         subtitle = subtitle,
         y=NULL,x=NULL,
         caption = sources) +
    theme(plot.title.position="plot",
          plot.title = element_text(size = 12, face="bold"),
          plot.subtitle = element_text(size = 10),
          plot.caption = element_text(size = 8, face="italic"),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line("grey70"),
          panel.grid.minor = element_line("grey70")) +
    transition_reveal(along = numero_frame, keep_last = FALSE)
  
  # Animation
  if (dir_sauv == ""){
    animate(p, nframes = nframes,fps=fps,width=800,height=800)
  }else{
    animate(p , nframes= nframes, fps= fps,
            width=800,height=800, end_pause = 200,
            res = 130,
            renderer = av_renderer(dir_sauv, codec = "libx264"))    
  }
}

# Graph de test
graph_anime(langue="fr",fps=1,nframes=25)
graph_anime(langue="nl",fps=1,nframes=25)
graph_anime(langue="de",fps=1,nframes=25)
graph_anime(langue="en",fps=1,nframes=25)

# Sauvegarde des videos
graph_anime(langue="fr",fps=100,dir_sauv = "FOLDER/covid-deces-W48-fr.mp4")
graph_anime(langue="nl",fps=100,dir_sauv = "FOLDER/covid-deces-W48-nl.mp4")
graph_anime(langue="de",fps=100,dir_sauv = "FOLDER/covid-deces-W48-de.mp4")
graph_anime(langue="en",fps=100,dir_sauv = "FOLDER/covid-deces-W48-en.mp4")


