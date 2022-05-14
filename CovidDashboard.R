library(ggplot2)
library(scales)
library(tidyverse)
library(plotly)
library(gt)





#------------------------------------------------------------------------------

# creating text table for Death percentage overall

death_percentage <- read.csv("https://raw.githubusercontent.com/AreninCodes/ShinyApp/main/death_percentage.csv")
death_table <- death_percentage %>% 
  gt() %>%
  tab_options(
    table.background.color = "grey",
    column_labels.background.color = "darkcyan",
    heading.background.color = "white"
  ) %>%
  tab_header(
    title = "Global Number"
  ) %>%
  cols_label(
    total_cases = "Total Cases",
    total_deaths = "Total Deaths",
    DeathPercentage = "Death Percentage"
  ) %>%
  fmt_number(columns = DeathPercentage, decimals = 2) %>%
  fmt_number(columns = total_cases, decimals = 0, sep_mark = ",") %>%
  fmt_number(columns = total_deaths, decimals = 0,sep_mark = ",") %>%
  cols_width(
    total_cases ~ px(150),
    total_deaths ~ px(150),
    DeathPercentage ~ px(150)
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) 

death_table



#------------------------------------------------------------------------------

# create bar plot for total death count
total_death_count <- read.csv("https://raw.githubusercontent.com/AreninCodes/ShinyApp/main/total_death_count.csv")
total_death_count_highlight <- data.frame(total_death_count,
                                          highlight= c(1,0,0,0,0,0)
                                          ) 

totalcount <- 
  ggplot(total_death_count_highlight, aes(x = reorder(continent, -TotalDeathCount), y = TotalDeathCount, fill = highlight)) +
    geom_bar(
      stat = "identity",
      position = "dodge",
      width = 0.7
    ) +
    geom_text(
      aes(label = scales::comma(TotalDeathCount)),
      vjust = -0.4,
      size = 4,
      position = position_dodge(0.9)
    ) +
    labs(title = "Total Death Count Per Continent") +
    xlab("Continent") +
    ylab(NULL) +
    scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3)) +
    theme(
      axis.text.x = element_text(face="bold", color="black"),
      axis.text.y = element_text(face="bold", color="black"),
      axis.line = element_line(color = "black", size = 0.5),
      legend.position = "none"

    )


totalcount


#------------------------------------------------------------------------------

# create heatmap for Percent Populated Infected
percent_populated_infected <- read.csv("https://raw.githubusercontent.com/AreninCodes/ShinyApp/main/percent_populated_infected.csv")

world <- map_data("world")
colnames(percent_populated_infected) <- c('region', 'Population', 'HighestInfectionCount', 'PercentPopulationInfected')
percent_populated_infected[1, "region"] <- "Faroe Islands"
percent_populated_infected[48, "region"] <- "USA"
percent_populated_infected[217, "region"] <- "Democratic Republic of the Congo"
MergedCountries <- inner_join(percent_populated_infected, world, by = 'region')


world_map <- 
  ggplot() + 
    geom_polygon( 
      data=MergedCountries, 
      aes(
        x=long, 
        y=lat, 
        group=group, 
        fill = PercentPopulationInfected, 
        color = "red"
        ), 
      color="white", 
      size = 0.2
    ) +
    labs(fill = "Percent Populated Infected") +
    xlab(NULL) +
    ylab(NULL) +
    scale_fill_viridis_c(direction = -1, option = "rocket", trans = "sqrt") +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank()
    )


world_map



#------------------------------------------------------------------------------

# create line plot and forecast for specific countries percent population infected
library(lubridate)

specific_percent_population_infected <- read.csv("https://raw.githubusercontent.com/AreninCodes/ShinyApp/main/specific_percent_population_infected.csv")
specific_percent_population_infected$date <- mdy(specific_percent_population_infected$date)
specific_percent_population_infected <- transform(specific_percent_population_infected, PercentPopulationInfected = as.numeric(PercentPopulationInfected))

countrytrends <-  
  ggplotly(
  ggplot(specific_percent_population_infected, aes(date, PercentPopulationInfected, color = Location)) +
    geom_line() +
    scale_x_date(date_breaks = "4 month", date_labels =  "%b %Y")
)
countrytrends




#------------------------------------------------------------------------------
# create dashboard
library(shiny)
library(shinydashboard)


ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "Covid Data"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              fluidRow(
                box(
                  title = "Percent Population Infected",
                  background = "blue",
                  solidHeader = TRUE,
                  plotlyOutput("correlation_plot2")
                ),
                
                box(
                  title = "Total Death Count Per Continent",
                  background = "blue",
                  solidHeader = TRUE,
                  plotOutput("bars"))


              ),
              
              fluidRow(
                box(
                  selectInput(
                    "countries2",
                    "Country Seleciton",
                    choices = c("All", unique(specific_percent_population_infected$Location)),
                    selected = 'All'
                  )
                )
              ),
              
              # info boxes
              fluidRow(
                infoBoxOutput("casesBox"),
                infoBoxOutput("deathsBox"),
                infoBoxOutput("percentBox")
              ),
              
              # heatmap
              fluidRow(
                box(
                  title = "Heatmap of Percent Infected Per Country",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("plot")
                ),
                
                box(
                  column(
                    width=12,
                    tags$head(tags$script('!function(d,s,id){var js,fjs=d.getElementsByTagName(s)    [0],p=/^http:/.test(d.location)?\'http\':\'https\';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+"://platform.twitter.com/widgets.js";fjs.parentNode.insertBefore(js,fjs);}}(document,"script","twitter-wjs");')),
                    column(
                      width=12,
                      box(
                        width = 12,
                        a("Covid Updates", class="twitter-timeline", href = "https://twitter.com/CDCgov")
                      )
                    )
                  )
                )

              )
              
              
              
              
      )
      
    )
    
  ),
)


server <- function(input, output) {
  
  #----- Dashboard ------
  
  # line plot render
  output$correlation_plot2 <- renderPlotly({
    data <- specific_percent_population_infected
    if (input$countries2 != "All") {
      data <- data[data$Location == input$countries2,]
    }

    ggplotly(
      ggplot(data, aes(date, PercentPopulationInfected, group = Location, color = Location)) +
        geom_line() +
        scale_x_date(date_breaks = "4 month", date_labels =  "%b %Y")
    )
  })
  
  # bar output
  output$bars <- renderPlot({
    totalcount
  })

  
  # info boxes render
  
  output$casesBox <- renderInfoBox({
    infoBox(
      "Total Cases", "474,599,600", icon = icon("flag", lib = "glyphicon"),
      color = "red", fill = TRUE
    )
  })
  
  output$deathsBox <- renderInfoBox({
    infoBox(
      "Total Deaths", "6,070,116", icon = icon("globe", lib = "glyphicon"),
      color = "red", fill = TRUE
    )
  })
  
  output$percentBox <- renderInfoBox({
    infoBox(
      "Death Percentage", "1.28%", icon = icon("warning-sign", lib = "glyphicon"),
      color = "red", fill = TRUE
    )
  })
  
  # heatmap render
  output$plot <- renderPlot({
    world_map
  })
}

shinyApp(ui, server)










