##============================================================================##
##===============================scrap========================================##
##============================================================================##


url <- "https://www.data.gouv.fr/fr/datasets/donnees-relatives-aux-resultats-des-tests-virologiques-covid-19/"
download.file(url = url, "datagouv.html")
html <- read_html("datagouv.html")
html_c <- html_children(html)
html_wanted <- html %>% html_nodes('.card-body')
pos <- Position(x = html_wanted, f = function(x){ grepl("sp-pos-quot-dep",html_text(x))})
html_wanted2 <- html_wanted[pos] %>% html_nodes('a') %>% html_attr('href')
l_html = length(html_wanted2)
link <- html_wanted2[l_html-1]

cas_pos_covid <- read.csv(link, sep=";") 

##============================================================================##
##================================scrap=======================================##
##============================================================================##

##============================================================================##
##============================changement dataset==============================##
##============================================================================##


cas_pos_covid <- rename(cas_pos_covid,department_code=dep,date=jour,cas_positive=P,testes=T,age_group=cl_age90,population=pop)
head(cas_pos_covid,10)

cas_pos_covid_pargroupe <- aggregate(x= cas_pos_covid$cas_positive, by = list(cas_pos_covid$age_group), FUN = sum)
cas_pos_covid_pargroupe <- rename(cas_pos_covid_pargroupe,age_group=Group.1, cas_pos=x)
cas_pos_covid_pargroupe

cas_pos_covid_pardep <- aggregate(x= cas_pos_covid$cas_positive, by = list(cas_pos_covid$department_code), FUN = sum)
cas_pos_covid_pardep <- rename(cas_pos_covid_pardep,departement=Group.1, cas_pos=x)
cas_pos_covid_pardep

cas_pos_covid_parjour <- aggregate(x= cas_pos_covid$cas_positive, by = list(cas_pos_covid$date), FUN = sum)
cas_pos_covid_parjour <- rename(cas_pos_covid_parjour,jour=Group.1, cas_pos=x)
cas_pos_covid_parjour

##============================================================================##
##============================changement dataset==============================##
##============================================================================##

library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)

# data("iris")

ui <- dashboardPage(
  dashboardHeader(title = "Example Module"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Table",
                         tabName = "table",
                         icon = icon("table")
                )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "table",
        box(title = "",
            width = 8,
            plotlyOutput("sepal_petal"),
            br(),
            br(),
            plotlyOutput("correlation_plot")
        )
      )
    )
  )
)
server <- function(input, output, session) {
  
  values <- reactiveValues(selected_species = NULL)
  
  output$sepal_petal <- renderPlotly({
    df_subset <- cas_pos_covid_pargroupe %>%
      group_by(age_group) %>%
      summarise(cor_sepal = cor(Sepal.Length, Sepal.Width))
    
    plot_ly(source = "plot_correlation") %>%
      add_trace(data = df_subset,
                x = ~age_group,
                y = ~ cas_pos,
                type = "bar") %>%
      layout(title = "Correlation Sepal Length/Width",
             yaxis = list(title = "Correlation")) %>%
      event_register("plotly_click")
  })
  
  observeEvent(event_data(event = "plotly_click",
                          source = "plot_correlation"), {
                            clicked <- event_data(event = "plotly_click",
                                                  source = "plot_correlation")
                            if (!is.null(clicked)) {
                              values$selected_species <- clicked$x
                            }
                          })
  
  output$correlation_plot <- renderPlotly({
    
    validate(need(values$selected_species, message = "click on a bar"))
    
    df_susbet <- iris %>%
      filter(Species == values$selected_species)
    
    plot_ly() %>%
      add_trace(data = df_susbet,
                x = ~Sepal.Length,
                y = ~ Sepal.Width,
                type = "scatter") %>%
      layout(title = paste0("Scatterplot ", values$selected_species))
  })
}

shinyApp(ui = ui, server = server,  options = list(display.mode = "showcase"))
