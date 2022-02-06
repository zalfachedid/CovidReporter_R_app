

#==============================================================================#
# ------------------------- Installation librairies ---------------------------#
#==============================================================================#

#install.packages("rvest")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("leaflet")
#install.packages("sf")
#install.packages("stringr")
#install.packages("readr")
#install.packages("tmap")

library(ggplot2)
library(rvest)
library(dplyr)
library(shiny)
library(stringr)
library(readr)
library(leaflet)
library(sf)


#==============================================================================#
# ---------------------------------- Functions --------------------------------#
#==============================================================================#

# -- Variable locale de couleur -- #
#Palette générale#
palette_gen <- c("#233D4D", # Bleu foncé
             "#FE7F2D", # Orange
             "#FCCA46", # Jaune foncé
             "#A1C181", # Vert 
             "#619B8A", # Turquois
             "#FFFCF2", # Blanc floral
             "#231F20") # Raisin noir
#Palette service#
# -- hosp (bleu)
palette_01 <- c("#D9ED92","#99D98C","#52B69A","#168AAD","#233D4D","#03045E")
# -- rea (orange)
palette_02 <- c("#FFBA08","#FE7F2D","#DC2F02","#9D0208","#6A040F","#370617")
# -- HospConv (jaune)
palette_03 <- c("#FAD643","#FCCA46","#C9A227","#A47E1B","#805B10","#76520E")
# -- SSR_USLD (vert)
palette_04 <- c("#DBEAD7","#A1C181","#7CB36B","#558745","#446C37","#33512A")
# -- autres (Turquois)
palette_05 <- c("#99E2B4","#67B99A","#619B8A","#358F80","#14746F","#036666")





# Affiche_all_hosp_periode ----------------------------------------------------#
Affiche_all_hosp_periode <- function(dataframe, deb_date = as.Date("2019-12-31"), end_date = as.Date("2100-12-31")) {
  sprintf("Period of analysis: %s -> %s",as.character.Date(deb_date),as.character.Date(end_date))
  #Filter part#
  df_filtered<-dataframe %>% filter((as.Date(jour) <= end_date) & (as.Date(jour) >= deb_date))
  date_min = min(as.character.Date(df_filtered$jour))
  date_max = max(as.character.Date(df_filtered$jour))
  #Graphic part#
  # Construction du titre
  title_label = paste("Nombre d'hospitalisation du au Covid19 dans les territoires francais\n du",
                      as.character.Date(date_min),
                      " au ",
                      as.character.Date(date_max))
  # Initialisation du graphique
  graph <- ggplot(df_filtered, aes(x=as.Date(jour))) + 
    
    # Edition du titre
    ggtitle(label = title_label, subtitle = "Répartition des hospitalisations par service")+
    
    # Edition des paramètres titre/legend
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(color = "#364167", hjust = 0.5),
      legend.position="bottom"
    )+
    # Edition des labels (ordonnées/abscisse)
    xlab("Date") + ylab("Nombre d'hospitalisation") +
    
    # Affichage des areas plot consernant les différents services
    geom_area(aes(y = rea+HospConv+SSR_USLD+autres, color = "reanimation", fill = "reanimation"), alpha=0.4)+ 
    geom_area(aes(y = hosp, color = "total", fill = "total"), alpha=0.1)+ 
    geom_area(aes(y = HospConv+SSR_USLD+autres, color = "conventionnelle", fill = "conventionnelle"), alpha=0.3)+
    geom_area(aes(y = SSR_USLD+autres, color = "SSR_ou_USLD", fill = "SSR_ou_USLD"), alpha=0.4)+
    geom_area(aes(y = autres, color = "autres", fill = "autres"), alpha=0.5)+
    
    # Paramétrage des couleurs des area plot et de la légende
    scale_color_manual("Hospitalisation", values=c(total=palette_gen[1],
                                                   reanimation =palette_gen[2],
                                                   conventionnelle = palette_gen[3],
                                                   SSR_ou_USLD = palette_gen[4],
                                                   autres = palette_gen[5])) +  
    scale_fill_manual("Hospitalisation", values=c(total=palette_gen[1],
                                                  reanimation =palette_gen[2],
                                                  conventionnelle = palette_gen[3],
                                                  SSR_ou_USLD = palette_gen[4],
                                                  autres = palette_gen[5]))
  
  return(graph)
  
}


# Affiche_dc_periode ----------------------------------------------------------#

Affiche_dc_periode <- function(dataframe, deb_date = as.Date("2019-12-31"), end_date = as.Date("2100-12-31")) {
  # Permet d'afficher sous forme de line plot la le nombre de décès dans une période donnée 
  sprintf("Period of analysis: %s -> %s",as.character.Date(deb_date),as.character.Date(end_date))
  #Filter part#
  df_filtered <- dataframe %>% filter((as.Date(jour) <= end_date) & (as.Date(jour) >= deb_date))
  date_min = min(as.character.Date(df_filtered$jour))
  date_max = max(as.character.Date(df_filtered$jour))
  #     print(as.character.Date(date_min))
  #Graphic part#
  # Area plot
  title_label = paste("Nombre de deces du au Covid19 dans les territoires francais\n du",
                      as.character.Date(date_min),
                      " au ",
                      as.character.Date(date_max))
  
  graph <- ggplot(df_filtered,
         aes(x=as.Date(jour))) + 
    #     
    ggtitle(label = title_label)+
    theme(
      plot.title = element_text(size = 14,
                                face = "bold",
                                hjust = 0.5),
      panel.grid.major.y = element_line(color = "#BBC5C0",
                                        size = 0.5))+
    xlab("Date") + ylab("Nombre de deces") +
    #     
    geom_area(aes(y = dc), fill = palette_gen[6], 
              color = palette_gen[7], alpha=0.2)
  
  return(graph)
}



# Filtre_departement ----------------------------------------------------------#

Filtre_departement <- function(dataframe, liste_dep = "all") {
  
  nb_dep = length(liste_region)
  
  #Print part of argument#
  if (liste_dep == "all"){
    print("On prend toutes les départements de france")
    df_filtered <- dataframe
  }else{
    if (nb_dep > 1){
      print("Sélection des départements:")
      for(dep in liste_dep){
        print(as.character(dep))
      }
    }else{
      print(paste("Sélection des départements: ",as.character(liste_dep)))
    }
    df_filtered <- dataframe %>% filter(as.character(dataframe$dep) %in% liste_dep)
  }
  
  #On regroupe les données par leur date#
  df_filtered <- df_filtered %>% filter(df_filtered$sexe == 0) %>% group_by(jour) %>% summarise(
    hosp = sum(hosp),
    rea = sum(rea),
    HospConv = sum(HospConv),
    SSR_USLD = sum(SSR_USLD),
    autres = sum(autres),
    dc = sum(dc)
  )
  df_filtered[is.na(df_filtered)] <- 0
  df_filtered
  
  #Return dataframe filtered#
  return(df_filtered)
}



# Affiche_hosp_periode --------------------------------------------------------#

Affiche_hosp_periode <- function(dataframe, deb_date = as.Date("2019-12-31"), end_date = as.Date("2100-12-31"),secteur_str = "Reanimation" ) {
  sprintf("Period of analysis: %s -> %s",as.character.Date(deb_date),as.character.Date(end_date))
  
  #Filter part#
  df_filtered<-dataframe %>% filter((as.Date(jour) <= end_date) & (as.Date(jour) >= deb_date))
  date_min = min(as.character.Date(df_filtered$jour))
  date_max = max(as.character.Date(df_filtered$jour))
  
  print(secteur_str)
  #traitement des entr?e ( secteur )#
  if (secteur_str == "Autres") {secteur <- df_filtered$autres}
  else if(secteur_str =="Hospitalisation"){ secteur <- df_filtered$hosp}
  else if(secteur_str =="Reanimation"){ secteur <- df_filtered$rea}
  else if(secteur_str == "Conventionnelle"){ secteur <- df_filtered$HospConv}
  else if(secteur_str == "SSR_USLD"){secteur <- df_filtered$SSR_USLD}
  
  
  
  #Graphic part#
  # Construction du titre
  title_label = paste("Nombre de personne affecte au service '",secteur_str,"' dans les territoires francais\n du",
                      as.character.Date(date_min),
                      " au ",
                      as.character.Date(date_max))
  # Initialisation du graphique
  graph <- ggplot() +
    
    # Edition du titre
    ggtitle(label = title_label)+
    
    # # Edition des paramètres titre/legend
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(color = "#364167", hjust = 0.5),
      legend.position="bottom"
    )+
    # Edition des labels (ordonnées/abscisse)
    xlab("Date") + ylab("Nombre d'hospitalisation") +
    
    # Affichage des areas plot consernant les différents services
    geom_area(aes(x= as.Date(df_filtered$jour) ,y = secteur, color = secteur_str, fill = secteur_str), alpha=0.4)+ 
    
    # Paramétrage des couleurs des area plot et de la légende
    scale_color_manual("Hospitalisation", values=c(Hospitalisation=palette_gen[1],
                                                   Reanimation =palette_gen[2],
                                                   Conventionnelle = palette_gen[3],
                                                   SSR_USLD = palette_gen[4],
                                                   Autres = palette_gen[5])) +  
    scale_fill_manual("Hospitalisation", values=c(Hospitalisation=palette_gen[1],
                                                  Reanimation =palette_gen[2],
                                                  Conventionnelle = palette_gen[3],
                                                  SSR_USLD = palette_gen[4],
                                                  Autres = palette_gen[5]))
  
  return(graph)
  
}

# Affiche_carte_france --------------------------------------------------------#

Affiche_carte_france <- function(dataframe, deb_date = as.Date("2019-12-31"), end_date = as.Date("2100-12-31"),secteur_str = "Hospitalisation" ) {
  sprintf("Period of analysis: %s -> %s",as.character.Date(deb_date),as.character.Date(end_date))
  
  #Filter part1 (jour)#
  df_filtered<-dataframe %>% filter((as.Date(jour) <= end_date) & (as.Date(jour) >= deb_date))
  date_min = min(as.character.Date(df_filtered$jour))
  date_max = max(as.character.Date(df_filtered$jour))
  
  #Filter part2 (selection du service)#
  if (secteur_str == "Autres") {
    secteur <- df_filtered %>% select(jour,dep,autres) %>% rename(service=autres)
    pal_serv <- palette_05
    }
  else if(secteur_str =="Hospitalisation"){ 
    secteur <- df_filtered %>% select(jour,dep,hosp) %>% rename(service=hosp)
    pal_serv <- palette_01
    }
  else if(secteur_str =="Reanimation"){
    secteur <- df_filtered %>% select(jour,dep,rea) %>% rename(service=rea)
    pal_serv <- palette_02
    }
  else if(secteur_str == "Conventionnelle"){ 
    secteur <- df_filtered %>% select(jour,dep,HospConv) %>% rename(service=HospConv)
    pal_serv <- palette_03
    }
  else if(secteur_str == "SSR_USLD"){
    secteur <- df_filtered %>% select(jour,dep,SSR_USLD) %>% rename(service=SSR_USLD)
    pal_serv <- palette_04
    }
  
  #Addition des nombre de personnes affectées aux différents services sur la plage de temps d'entrée#
  df_grp <- secteur %>% group_by(dep) %>% summarise(service = sum(service)) %>% rename(code_insee=dep, nb_pers = service)
  df_grp[is.na(df_grp)] <- 0
  df_grp$nb_pers <- df_grp$nb_pers/1000
  
  
  # Construction du fond de la carte interactive
  carte_dep <- st_read("data/departements.shp")
  carte_dep
  carte_dep <- carte_dep %>% mutate(nom = str_conv(nom, "utf-8"))
  # Arrangement par code_insee = code des départements
  carte_dep <- carte_dep %>% arrange(code_insee)
  # Correction des code_insee
  carte_dep$code_insee[70] <- "69"
  carte_dep$code_insee[71] <- "71"
  
  # Jointure à gauche
  carte_dep <- carte_dep %>% left_join(df_grp, by = "code_insee")
  carte_dep
  
  pal <- colorBin(
    palette = pal_serv,
    domain = carte_dep$nb_pers,
    bin = 6,
    pretty=FALSE
  )
  
  map_dep <- leaflet() %>%
    addTiles() %>%
    addProviderTiles("Esri.WorldGrayCanvas")%>%
    # polygone des regions
    addPolygons(
      data = carte_dep, 
      label = ~nom,
      weight = 1,
      # popup = ~paste0("Densité : ", round(density), " hab/km2"), 
      fill = TRUE, 
      # Application de la fonction palette_gen
      # fillColor = ~pal(nb_pers),
      fillColor = pal_serv,
      fillOpacity = 0.8,
      highlightOptions = highlightOptions(color = "white", weight = 1)) %>%
    addLegend(
      title = paste0("Nombre de personne en '",secteur_str,"'(/1000pers)"),
      pal = pal, values = carte_dep$nb_pers) %>%
      setView(lat = "47.803530", lng = "7.828293", zoom = 5)
    
  return(map_dep)
  
}





# Filtre_periode_pred --------------------------------------------------------#

Filtre_periode_pred <- function(dataframe, choix = "Global") {
  
  # Gestion du choix d'entrée
  if(choix == "Global"){
    date_min = as.Date(min(as.character.Date(dataframe$jour)))
    date_max = as.Date(max(as.character.Date(dataframe$jour)))
  }
  else if(choix == "Ce mois"){
    Today <- max(as.character.Date(dataframe$jour))
    Mois_crt <- as.numeric(substr(Today,start=6,stop=7))
    if (Mois_crt>11){
      Mois_sv = "01"
    }
    else{
      Mois_sv <- Mois_crt+1
      if (Mois_sv<10){
        Mois_sv <- paste0("0",as.character(Mois_sv))
      }else{
        Mois_sv <- as.character(Mois_sv)
      }
    }
    date_min <- as.Date(paste0(substr(Today,start=1,stop=7),"-01"),format="%Y-%m-%d")
    date_max <- as.Date(paste0(substr(Today,start=1,stop=5),Mois_sv,"-01"),format="%Y-%m-%d")
    # date_min <- as.Date(format(as.Date(Today),"%Y-%m-01"))
    # date_max <- as.Date(format(as.Date(Today),"%Y-%m-31"))
  }
  else if(choix == "2022"){
    date_min = as.Date("2022-01-01")
    date_max = as.Date("2023-01-01")    
  }
  else if(choix == "2021"){
    date_min = as.Date("2021-01-01")
    date_max = as.Date("2022-01-01")     
  }
  else if(choix == "2020"){
    date_min = as.Date("2020-01-01")
    date_max = as.Date("2021-01-01")     
  }
  
  df_filtered <- dataframe %>% filter((as.Date(jour) < date_max) & (as.Date(jour) >= date_min))
  df_filtered[is.na(df_filtered)] <- 0
  
  #Return dataframe filtered#
  return(df_filtered)
}
