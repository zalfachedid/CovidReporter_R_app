#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#==============================================================================#
# ------------------------- Installation librairies ---------------------------#
#==============================================================================#

#install.packages("rvest")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("shiny")
#install.packages("leaflet")
#install.packages("sf")
#install.packages("stringr")
#install.packages("readr")

library(ggplot2)
library(rvest)
library(dplyr)
library(shiny)
library(leaflet)
library(sf)
library(stringr)
library(readr)
library(plotly)

#==============================================================================#
# ------------------------ Importing R function file --------------------------#
#==============================================================================#

source("function_file.R")


#==============================================================================#
# ---------------------------------- Serveur ----------------------------------#
#==============================================================================#

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Create a Progress object
  progress <- shiny::Progress$new()
  # Make sure it closes when we exit this reactive, even if there's an error
  on.exit(progress$close())
  
  progress$set(message = "Chargement des données", value = 0)
  
  # Increment the progress bar, and update the detail text.
  progress$inc(1/10, detail = "Web scrapping sur data.gouv ...")
  #==============================================================================#
  # ------------------------------- Web Scrapping -------------------------------#
  #==============================================================================#
  
  #================= Données hospitalières =======================#
  url <- "https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/"
  download.file(url = url, "datagouv.html")
  html <- read_html("datagouv.html")
  html_c <- html_children(html)
  html_wanted <- html %>% html_nodes('.card-body')
  pos <- Position(x = html_wanted, f = function(x){ grepl("donnees-hospitalieres-covid19",html_text(x))})
  html_wanted2 <- html_wanted[pos] %>% html_nodes('a') %>% html_attr('href')
  l_html = length(html_wanted2)
  link <- html_wanted2[l_html-1]
  
  # Increment the progress bar, and update the detail text.
  progress$inc(1/5, detail = "Lecture des tables de données hospitalières ...")
  df_hebdo_covid <- read.csv(link, sep=";")
  #head(df_hebdo_covid,10)
  
  
  #================= Données vaccinales =======================#
  progress$inc(1/10, detail = "Web scrapping sur data.gouv ...")
  url <- "https://www.data.gouv.fr/fr/datasets/donnees-relatives-aux-personnes-vaccinees-contre-la-covid-19-1/#resources"
  download.file(url = url, "datagouv.html")
  html <- read_html("datagouv.html")
  html_c <- html_children(html)
  html_wanted <- html %>% html_nodes('.card-body')
  pos <- Position(x = html_wanted, f = function(x){ grepl("vacsi12-reg",html_text(x))})
  html_wanted2 <- html_wanted[pos] %>% html_nodes('a') %>% html_attr('href')
  l_html = length(html_wanted2)
  link <- html_wanted2[l_html-1]
  
  progress$inc(1/10, detail = "Lecture des tables de données hospitalières ...")
  vaccin_covid <- read.csv(link, sep=";")
  
  
  #==============================================================================#
  # ------------------------- Traitement des données --------------------------- #
  #==============================================================================#
  # 1- Regroupement des données par jour sur l'ensemble du territoire français --#
  #    ps: Ici sexe=0 signifie que l'on fait aucune distinction de genre
  
  # Increment the progress bar, and update the detail text.
  progress$inc(1/5, detail = "Traitement des données vaccinales...")
  
  
  vaccin_covid <- vaccin_covid %>% group_by(jour) %>% summarise(
    n_cum_complet = sum(n_cum_complet),
    n_cum_dose1 = sum(n_cum_dose1),
    n_cum_rappel = sum(n_cum_rappel),
    n_complet= sum(n_complet),
    n_dose1=sum(n_dose1),
    n_rappel=sum(n_rappel)
  )
  
  progress$inc(1/5, detail = "Traitement des données hospitalières...")
  
  df_data_fr <- df_hebdo_covid %>% filter(df_hebdo_covid$sexe == 0) %>% group_by(jour) %>% summarise(
    hosp = sum(hosp),
    rea = sum(rea),
    HospConv = sum(HospConv),
    SSR_USLD = sum(SSR_USLD),
    autres = sum(autres),
    dc = sum(dc)
  )
  df_data_fr[is.na(df_data_fr)] <- 0
  df_data_fr
  
  # 1.2 - On sépare les données par années pour afficher certains graphiques--#
  df_data_fr_2020<-df_data_fr %>% filter((as.Date(jour) < as.Date("2021-01-01")) & (as.Date(jour) > as.Date("2019-12-31")))
  df_data_fr_2021<-df_data_fr %>% filter((as.Date(jour) < as.Date("2022-01-01")) & (as.Date(jour) > as.Date("2020-12-31"))) 
  df_data_fr_2022<-df_data_fr %>% filter((as.Date(jour) < as.Date("2023-01-01")) & (as.Date(jour) > as.Date("2021-12-31"))) 
  
  
  # 1(bis)- Regroupement des données par jour et par departement sur l'ensemble du territoire français --#
  #    ps: Ici sexe=0 signifie que l'on fait aucune distinction de genre
  df_data_dep <- df_hebdo_covid %>% filter(df_hebdo_covid$sexe == 0) %>% group_by(jour,dep) %>% summarise(
    hosp = sum(hosp),
    rea = sum(rea),
    HospConv = sum(HospConv),
    SSR_USLD = sum(SSR_USLD),
    autres = sum(autres),
    dc = sum(dc)
  )
  df_data_dep[is.na(df_data_dep)] <- 0
  df_data_dep
  
  
  
  # 2 - Construction des textes généraux ------------------------------------#
  
  Titre_nb_hosp_001 <- "Nombre de personne en hospitalisation:"
  Titre_nb_rea_001 <- "Nombre de personne en réanimation:"
  Titre_nb_dc_001 <- "Nombre de décès liee au Covid19:"
  
  Titre_nb_hosp_002 <- "Nombre d'hospitalisation:"
  Titre_nb_rea_002 <- "Bilan total de personne en réanimation:"
  Titre_nb_dc_002 <- "Bilan total de décès liée au Covid19:"
  
  Titre_nb_hosp_003 <- Titre_nb_hosp_002
  Titre_nb_rea_003 <- Titre_nb_rea_002
  Titre_nb_dc_003 <- Titre_nb_dc_002
  
  Titre_nb_hosp_004 <- Titre_nb_hosp_002
  Titre_nb_rea_004 <- Titre_nb_rea_002
  Titre_nb_dc_004 <- Titre_nb_dc_002
  
  Titre_nb_hosp_005 <- Titre_nb_hosp_002
  Titre_nb_rea_005 <- Titre_nb_rea_002
  Titre_nb_dc_005 <- Titre_nb_dc_002
  
  Today <- max(as.character.Date(df_data_fr$jour))
  Yesterday <- paste(format(as.Date(Today),"%Y-%m"),sep="")
  Titre_gen_recap_001 <- paste("Aujourd'hui :", Today )
  Titre_gen_recap_002 <- paste("Ce mois :", toupper(format(as.Date(Today),"%B")) )
  Titre_gen_recap_003 <- paste("Bilan de l'année 2022 :")
  Titre_gen_recap_004 <- paste("Bilan de l'année 2021 :")
  Titre_gen_recap_005 <- paste("Bilan de l'année 2020 :")
  
  # A ce jour
  nb_hosp_001 <- df_data_fr %>% filter(df_data_fr$jour == Today) %>% select(hosp) %>% as.character()
  nb_rea_001 <- df_data_fr %>% filter(df_data_fr$jour == Today) %>% select(rea) %>% as.character()
  nb_dc_001 <- df_data_fr %>% filter(df_data_fr$jour == Today) %>% select(dc)
  nb_dc_001 <- (nb_dc_001 - df_data_fr$dc[length(df_data_fr$dc)-1]) %>% as.character()
  
  # Ce mois
  nb_hosp_002 <- df_data_fr %>% filter(format(as.Date(df_data_fr$jour),"%Y-%m") == format(as.Date(Today),"%Y-%m")) %>% select(hosp) %>% sum() %>% as.character()
  nb_rea_002 <- df_data_fr %>% filter(format(as.Date(df_data_fr$jour),"%Y-%m") == format(as.Date(Today),"%Y-%m")) %>% select(rea) %>% sum() %>% as.character()
  nb_dc_002 <- df_data_fr %>% filter(format(as.Date(df_data_fr$jour),"%Y-%m") == format(as.Date(Today),"%Y-%m")) 
  nb_dc_002 <- (nb_dc_002$dc[length(nb_dc_002$dc)] - nb_dc_002$dc[1]) %>% as.character()
  
  # 2022
  nb_hosp_003 <- sum(df_data_fr_2022$hosp) %>% as.character()
  nb_rea_003 <- sum(df_data_fr_2022$rea) %>% as.character()
  nb_dc_003 <- (df_data_fr_2022$dc[length(df_data_fr_2022$dc)] - df_data_fr_2022$dc[1]) %>% as.character()
  
  # 2021
  nb_hosp_004 <- sum(df_data_fr_2021$hosp) %>% as.character()
  nb_rea_004 <- sum(df_data_fr_2021$rea) %>% as.character()
  nb_dc_004 <- (df_data_fr_2021$dc[length(df_data_fr_2022$dc)] - df_data_fr_2021$dc[1]) %>% as.character()
  
  # 2020
  nb_hosp_005 <- sum(df_data_fr_2020$hosp) %>% as.character()
  nb_rea_005 <- sum(df_data_fr_2021$rea) %>% as.character()
  nb_dc_005 <- (df_data_fr_2020$dc[length(df_data_fr_2020$dc)] - df_data_fr_2020$dc[1]) %>% as.character()
  
  # Données vaccinales
  nbr_vacc_complet2<- sum(vaccin_covid$n_complet)
  nbr_vacc_dose12<- sum(vaccin_covid$n_dose1)
  nbr_vacc_rappel2<- sum(vaccin_covid$n_rappel)
  
  #==============================================================================#
  # ----------------------------------- Output ----------------------------------#
  #==============================================================================#
  
  # Increment the progress bar, and update the detail text.
  progress$inc(1/10, detail = "Génération de la page bilan ...")
  # 2 - Construction des textes -----------------------------------------------#
  # == # Toutes pages
  output$Titre_nb_hosp_001 <- renderText(Titre_nb_hosp_001)
  output$Titre_nb_rea_001 <- renderText(Titre_nb_rea_001)
  output$Titre_nb_dc_001 <- renderText(Titre_nb_dc_001)
  
  output$Titre_nb_hosp_002 <- renderText(Titre_nb_hosp_002)
  output$Titre_nb_rea_002 <- renderText(Titre_nb_rea_002)
  output$Titre_nb_dc_002 <- renderText(Titre_nb_dc_002)
  
  output$Titre_nb_hosp_003 <- renderText(Titre_nb_hosp_003)
  output$Titre_nb_rea_003 <- renderText(Titre_nb_rea_003)
  output$Titre_nb_dc_003 <- renderText(Titre_nb_dc_003)
  
  output$Titre_nb_hosp_004 <- renderText(Titre_nb_hosp_004)
  output$Titre_nb_rea_004 <- renderText(Titre_nb_rea_004)
  output$Titre_nb_dc_004 <- renderText(Titre_nb_dc_004)
  
  output$Titre_nb_hosp_005 <- renderText(Titre_nb_hosp_005)
  output$Titre_nb_rea_005 <- renderText(Titre_nb_rea_005)
  output$Titre_nb_dc_005 <- renderText(Titre_nb_dc_005)
  
  
  # == # Page bilan
  output$Titre_gen_recap_001 <- renderText(Titre_gen_recap_001)
  output$Titre_gen_recap_002 <- renderText(Titre_gen_recap_002)
  output$Titre_gen_recap_003 <- renderText(Titre_gen_recap_003)
  output$Titre_gen_recap_004 <- renderText(Titre_gen_recap_004)
  output$Titre_gen_recap_005 <- renderText(Titre_gen_recap_005)
  
  output$nb_hosp_001 <- renderText(nb_hosp_001)
  output$nb_rea_001 <- renderText(nb_rea_001)
  output$nb_dc_001 <- renderText(nb_dc_001)
  
  output$nb_hosp_002 <- renderText(nb_hosp_002)
  output$nb_rea_002 <- renderText(nb_rea_002)
  output$nb_dc_002 <- renderText(nb_dc_002)
  
  output$nb_hosp_003 <- renderText(nb_hosp_003)
  output$nb_rea_003 <- renderText(nb_rea_003)
  output$nb_dc_003 <- renderText(nb_dc_003)
  
  output$nb_hosp_004 <- renderText(nb_hosp_004)
  output$nb_rea_004 <- renderText(nb_rea_004)
  output$nb_dc_004 <- renderText(nb_dc_004)
  
  output$nb_hosp_005 <- renderText(nb_hosp_005)
  output$nb_rea_005 <- renderText(nb_rea_005)
  output$nb_dc_005 <- renderText(nb_dc_005)
  # == # Page parametrage
  
  # == # Page features
  
  # 3 - Construction des graphiques  ------------------------------------------#
  # == # Page bilan
  output$graph_gen_hosp_001 <- renderPlot({Affiche_all_hosp_periode(Filtre_periode_pred(df_data_fr,input$choix_bilan_01))})
  output$graph_gen_dc_001 <- renderPlot({Affiche_dc_periode(Filtre_periode_pred(df_data_fr,input$choix_bilan_02))})
  
  # == # Page parametrage
  # Increment the progress bar, and update the detail text.
  progress$inc(1/10, detail = "Génération de la page parametrable ...")
  
  date1 <-reactive({input$DateRange[1]})
  date2 <-reactive({input$DateRange[2]})
  output$graph_mod_hosp_001 <- renderPlot({Affiche_hosp_periode(df_data_fr,date1(),date2(),input$choix_graphe)})
  output$graph_mod_dc_001 <- renderPlot({Affiche_dc_periode(df_data_fr,date1(),date2())})
  output$map_mod_serv_001 <- renderLeaflet({Affiche_carte_france(df_data_dep,date1(),date2(),input$choix_graphe)})
  
  # == # Page vaccin
  output$cas_positive <- renderValueBox({
    valueBox(
      value = format(nbr_vacc_complet2, big.mark=","),
      subtitle = "Total des vaccination complètes",
      color = "aqua")
  })
  
  output$nbr_de_deces <- renderValueBox({
    valueBox(
      value = format(nbr_vacc_dose12, big.mark=","),
      subtitle = "Total des vaccinations avec une seule dose",
      color = "aqua")
  })
  
  output$nbr_d_hosp <- renderValueBox({
    valueBox(
      value = format(nbr_vacc_rappel2, big.mark=","),
      subtitle = "Total des rappel de vaccination",
      color = "aqua")
  })
  
  output$vaccin_comp <- renderPlotly({
    plot_ly(data = vaccin_covid, type = 'scatter', mode = 'lines')%>%
      add_trace(x = ~jour, y = ~n_cum_complet,name="vaccin")%>%
      layout(yaxis=list(title= "Nombre de vaccination complète"))
  }
  )
  output$vaccin_dose1 <- renderPlotly({
    plot_ly(data = vaccin_covid, type = 'scatter', mode = 'lines')%>%
      add_trace(x = ~jour, y = ~n_cum_dose1,name="vaccin")%>%
      layout(yaxis=list(title= "Nombre de 1ière dose"))
  })
  output$vaccin_rappel <- renderPlotly({
    plot_ly(data = vaccin_covid, type = 'scatter', mode = 'lines')%>%
      add_trace(x = ~jour, y = ~n_cum_rappel,name="vaccin")%>%
      layout(yaxis=list(title= "Nombre de dose de rappel"))
  })
  
  # 3 - Construction des tableaux  ------------------------------------------#
  output$CovidData_gen <-renderDataTable({df_data_fr})
  
  # 4 - Boutons de téléchargement -------------------------------------------#
  #Bouton de téléchargement de la base de donnée générale#
  output$downloadData_001 <- downloadHandler(
    filename = function() {
      paste("donnees_hospitalieres_covid19", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(df_data_fr, file, row.names = TRUE)
    }
  )
  progress$set(message = "Terminé !", value = 1)
  progress$close()
})
