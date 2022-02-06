#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#==============================================================================#
# -------------------------------- librairies ---------------------------------#
#==============================================================================#
library(shiny)
library(colourpicker)
library(leaflet)
library(shinydashboard)
library(plotly)


#==============================================================================#
# ------------------------------------ UI -------------------------------------#
#==============================================================================#

# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    ####################
    #--dashboardHeader--------------------------
    dashboardHeader(
      title = "Covid Reporter"
    ),
    ####################
    #--dashboardSidebar-------------------------
    dashboardSidebar(
      sidebarMenu(
                  menuItem("Bilan Général", tabName = "bilangeneral", icon = icon("universal-access")),
                  menuItem("Bilan Détaillé", tabName = "bilandetaille",icon = icon("chart-area")),
                  menuItem("Bilan de vaccination", tabName = "Bilandevaccination",icon = icon("file-invoice"))
                )
      
    ),
    ####################
    #--dashboardBody----------------------------
    dashboardBody(
                  tabItems(
                            tabItem(tabName = "bilangeneral",
                                                fluidRow(column(width = 4,
                                                        h3("CHIFFRES CLEFS",align = "center"),
                                                        #--# Side panel (bouton de parametrage #-----------------------------#)
        
                                                          
                                                          box(
                                                              title = textOutput(outputId = "Titre_gen_recap_001"), width = 12, solidHeader = TRUE,
                                                              textOutput(outputId = "Titre_nb_hosp_001"),
                                                              textOutput(outputId = "nb_hosp_001"),
                                                              textOutput(outputId = "Titre_nb_rea_001"),
                                                              textOutput(outputId = "nb_rea_001"),
                                                              textOutput(outputId = "Titre_nb_dc_001"),
                                                              textOutput(outputId = "nb_dc_001"),
                                                             ),
                                                          box(
                                                            title = textOutput(outputId = "Titre_gen_recap_002"), width = 12, solidHeader = TRUE,
                                                            textOutput(outputId = "Titre_nb_hosp_002"),
                                                            textOutput(outputId = "nb_hosp_002"),
                                                            textOutput(outputId = "Titre_nb_rea_002"),
                                                            textOutput(outputId = "nb_rea_002"),
                                                            textOutput(outputId = "Titre_nb_dc_002"),
                                                            textOutput(outputId = "nb_dc_002"),
                                                          ),
                                                          box(
                                                            title = textOutput(outputId = "Titre_gen_recap_003"), width = 12, solidHeader = TRUE,
                                                            textOutput(outputId = "Titre_nb_hosp_003"),
                                                            textOutput(outputId = "nb_hosp_003"),
                                                            textOutput(outputId = "Titre_nb_rea_003"),
                                                            textOutput(outputId = "nb_rea_003"),
                                                            textOutput(outputId = "Titre_nb_dc_003"),
                                                            textOutput(outputId = "nb_dc_003"),
                                                          ),
                                                          box(
                                                            title = textOutput(outputId = "Titre_gen_recap_004"), width = 12, solidHeader = TRUE,
                                                            textOutput(outputId = "Titre_nb_hosp_004"),
                                                            textOutput(outputId = "nb_hosp_004"),
                                                            textOutput(outputId = "Titre_nb_rea_004"),
                                                            textOutput(outputId = "nb_rea_004"),
                                                            textOutput(outputId = "Titre_nb_dc_004"),
                                                            textOutput(outputId = "nb_dc_004"),
                                                          ),
                                                          box(
                                                            title = textOutput(outputId = "Titre_gen_recap_005"), width = 12, solidHeader = TRUE,
                                                            textOutput(outputId = "Titre_nb_hosp_005"),
                                                            textOutput(outputId = "nb_hosp_005"),
                                                            textOutput(outputId = "Titre_nb_rea_005"),
                                                            textOutput(outputId = "nb_rea_005"),
                                                            textOutput(outputId = "Titre_nb_dc_005"),
                                                            textOutput(outputId = "nb_dc_005"),
                                                          ),
                                                        ),
                                                      column(width = 7,
        
                                                        #--# Main panel (Affichage graphique #-------------------------------#)
                                                        h1("BILAN GÉNÉRAL",align="center"),
                                                        tabsetPanel(
                                                                    tabPanel("Hospitalisation", radioButtons("choix_bilan_01","Type de bilan", c("Global","Ce mois","2022","2021","2020"), selected="Global",inline = TRUE),
                                                                                                plotOutput(outputId = "graph_gen_hosp_001",
                                                                                                                      width = "800px",
                                                                                                                      height = "500px")
                  
                                                                             ),
                                                                    tabPanel("Décès", radioButtons("choix_bilan_02","Type de bilan", c("Global","Ce mois","2022","2021","2020"), selected="Global",inline = TRUE),
                                                                                               plotOutput(outputId = "graph_gen_dc_001",
                                                                                                                      width = "800px",
                                                                                                                      height = "500px")
                                                                             ),
                                                                    tabPanel("Données",
                                                                             fluidRow(
                                                                                       column(width = 9,dataTableOutput(outputId ="CovidData_gen")),
                                                                                       column(width = 2,downloadButton("downloadData_001", "Download"))
                                                                                       )
                                                                            )
                                                                    ) #End tabsetPanel
        
                                                        )# end column
                                              ) # end FluidRow
                            ),
                            
                            tabItem(tabName = "bilandetaille",
                                    fluidRow(column(width = 4,
                                                    h3("Parametres",align = "center"),
                                                    box(
                                                      title = "Période d'étude:", background = "blue", width = 12,
                                                      
                                                      dateRangeInput("DateRange",label = NULL,
                                                                     start  = "2020-03-19",
                                                                     end    = as.character.Date(Sys.Date()),
                                                                     min    = "2020-03-18",
                                                                     max    = as.character.Date(Sys.Date())
                                                                    ),
                                                    
                                                        ),
                                                    box(
                                                        title = "Sélection du secteur:", background = "light-blue", width = 12,
                                                        radioButtons("choix_graphe",label = NULL, c("Hospitalisation","Reanimation","Conventionnelle","SSR_USLD","Autres"), selected="Reanimation"),
                                                       )
                                                  ),
                                              column(width = 7,
                                                     h1("GRAPHIQUES",align="center"),
                                                     tabsetPanel(
                                                                 tabPanel("Hospitalisations", plotOutput(outputId = "graph_mod_hosp_001",
                                                                                                         width = "800px",
                                                                                                         height = "500px")
                                                                          ), 
                                                                 tabPanel("Décès",plotOutput(outputId = "graph_mod_dc_001",
                                                                                             width = "800px",
                                                                                             height = "500px")
                                                                          ),
                                                                 tabPanel("Carte", leafletOutput("map_mod_serv_001") )
                                                                 ) #End tabsetPanel
                                                                                      
                                                  )#End column
                                              )#End FluidRow
                                    ),#End tabItem
                            tabItem(
                                      tabName = "Bilandevaccination",
                                      fluidRow(
                                                valueBoxOutput("nbr_de_deces"),
                                                valueBoxOutput("cas_positive"),
                                                valueBoxOutput("nbr_d_hosp"),
                                                h1("GRAPHIQUES DE VACCINATION",align="center"),
                                                tabsetPanel(
                                                            tabPanel("1ière dose",
                                                                     plotlyOutput("vaccin_dose1")
                                                                    ),
                                                            tabPanel("Vaccination complète",
                                                                     plotlyOutput("vaccin_comp")
                                                            ),
                                                            tabPanel("Dose de rappel",
                                                                     plotlyOutput("vaccin_rappel")
                                                                    )
                                                            )#End tabsetPanel
                                              )#end fluidRow
                                   )# End tabItem
                        )# End tabItems
        )# End dashboardBody
  )# End dashboard
)# End shinyUI
