library(readxl)
library(plyr)
Stats_modifs <- read_excel("C:/Users/thomas.delclite/OneDrive - GCloud Belgium/Statbel/Projets/Visualisation R/treemap EBM/EBM 2018.xlsx")

colnames(Stats_modifs)
# "Catégorie"   "Modalité"    "Professions" "Nombre" 
# [1] "COICOP"                                                          
# [2] "Libellés"                                                        
# [3] "Nombre de ménages (échantillon)"                                 
# [4] "Nombre de ménages (population)"                                  
# [5] "Dépenses moyennes pour la totalité des ménages (par an en euros)"
# [6] "Dépenses moyennes par personne et par an (€)"                    
# [7] "Dépenses moyennes par unité de consommation et par an (€)"  

colnames(Stats_modifs) <- c("COICOP","label","n_echan","n_pop","depenses_moy_tot","depenses_moy_pp","depenses_moy_UC")

# Créer les différents niveaux de nomenclature COICOP

Stats_modifs$COICOP_N1 <- substr(Stats_modifs$COICOP,1,1)
Stats_modifs$COICOP_N2 <- substr(Stats_modifs$COICOP,1,2)
Stats_modifs$COICOP_N3 <- substr(Stats_modifs$COICOP,1,3)
Stats_modifs$COICOP_N4 <- substr(Stats_modifs$COICOP,1,4)
Stats_modifs$COICOP_N5 <- substr(Stats_modifs$COICOP,1,5)
Stats_modifs$COICOP_N6 <- substr(Stats_modifs$COICOP,1,6)
Stats_modifs$COICOP_N7 <- substr(Stats_modifs$COICOP,1,7)

# Récupérer les labels des COICOP
nchar(Stats_modifs$COICOP)
for (i in 1:7){
  temp <- Stats_modifs[nchar(Stats_modifs$COICOP) == i,c('COICOP','label','depenses_moy_tot')]
  temp$label <- paste(temp$label," (",round(temp$depenses_moy_tot,-1),"€)",sep="")
  temp <- rename(temp,replace= c("label"= paste("label_N",i,sep="")))
  temp <- temp[,-3]
  Stats_modifs <- merge(Stats_modifs,temp,by.x=paste("COICOP_N",i,sep=""),by.y="COICOP",all.x = TRUE)
}

library(treemap)

treemap(Stats_modifs,
        index="label_N6",
        vSize="depenses_moy_tot",
        type="index"
)

# Trop détaillé, mais joli :)

treemap(subset(Stats_modifs,!is.na(label_N6)),
        index=c(paste("label_N",c(1:6),sep="")),
        vSize="depenses_moy_tot",
        type="index"
)

# Moche, mais complet

# devtools::install_github("timelyportfolio/d3treeR")
library(d3treeR)

p <-  treemap(subset(Stats_modifs,!is.na(label_N6)),
              index=c(paste("label_N",c(2:6),sep="")),
              vSize="depenses_moy_tot",
              type="index",
              palette = "Set2",
              align.labels=list(
                c("center", "center"), 
                c("right", "bottom")
              ),
              overlap.labels=0.1,
              inflate.labels = F,
              eval.labels = FALSE,
              title = "Dépenses moyennes des ménages belges par COICOP, enquête EBM 2018"
)

inter <- d3tree2(
             p
      ,  rootname = "Dépenses moyennes des ménages" )

inter
# save the widget
library(htmlwidgets)
setwd("C:/Users/thomas.delclite/OneDrive - GCloud Belgium/Statbel/Projets/Visualisation R/treemap EBM")
saveWidget(inter, file=paste0( getwd(), "/interactiveTreemap.html"))
