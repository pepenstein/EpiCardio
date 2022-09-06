# Packages ----
library(shiny)
library(shinydashboard)
library(shinyEffects)
library(shinyWidgets)
library(bs4Dash)
library(numform)
library(tidyverse)
library(echarts4r)
library(echarts4r.maps)
library(echarts4r.assets)
library(readxl)
library(DT)
library(lubridate)
options(scipen = 999, encoding = "latin1")

# DATA ----

# * Completed database ----

db <- read_excel("db_cvd.xlsx")
code <- read_delim("CODE.txt", 
                   delim = ";", escape_double = FALSE, trim_ws = TRUE)

db$Cause <- factor(db$Cause, levels = c(1,2,3,4,5,6,7,8,9),
                   labels = code$Causes)


prevalences <- data.frame()
for(i in 1:1782){
  prop <- prop.test(x = db$N[i], n = db$Population[i], alternative = "two.sided",
                    conf.level = 0.95)
  final <- round(data.frame(Rate = prop$estimate, Rate_lower = prop$conf.int[1],
                            Rate_upper = prop$conf.int[2])*100000,1)
  prevalences <- rbind.data.frame(prevalences, final)
}


db <- cbind.data.frame(db, prevalences)

# * Mortality by sex ----

both <- db %>% 
  filter(Cause == "Cardiovascular diseases",
         Age == "All Ages",
         Sex == "Both") %>% 
  arrange(Year)  %>% 
  as.data.frame()

female <- db %>% 
  filter(Cause == "Cardiovascular diseases",
         Age == "All Ages",
         Sex == "Female") %>% 
  arrange(Year) %>% 
  as.data.frame()
male <- db %>% 
  filter(Cause == "Cardiovascular diseases",
         Age == "All Ages",
         Sex == "Male") %>% 
  arrange(Year) %>% 
  as.data.frame()



# * Moratality by province ----
provincias <- read_excel("causa_provincias.xlsx")

provincias <- provincias %>% 
  pivot_longer(names_to = "Province", values_to = "Total", cols = 2:17) %>% 
  arrange(Year) %>% as.data.frame()


# * Annualized change ratio by sex ----

acr <- read.csv("acr.csv") %>% as.data.frame()

# * Annualized change ratio by province ----

acr_province <- read.csv("provinces_final.csv") %>% as.data.frame()

# * Forecast data ----

forecast <- read.csv("forecast.csv") %>% as.data.frame()

forecast <- forecast[c(1:11, 22:26), 5:6]
forecast$Serie <- c(rep("Actual", 11), rep("Estimated", 5))
forecast$Deaths <- round(forecast$Deaths)
forecast$Year <- as.Date(forecast$Year, "%Y-%m-%d") %>% year() %>% as.character()


# UI Shiny Dashboard ----

ui <- dashboardPage(
  dark = NULL,
  header = dashboardHeader(title = HTML("<head>
    <link href='https://unpkg.com/boxicons@2.1.2/css/boxicons.min.css' rel='stylesheet'>
    <style>
        .logo-content .logo {
    color: white;
    display: flex;
    height: 50px;
    width: 100%;
    align-items: center;
}

.logo-content .logo i {
    font-size: 26px;
    margin-right: 10px;   
}

.logo-content .logo .logo_name {
    font-size: 20px;
    font-weight: 400;
    overflow: hidden
}

.layout-fixed.sidebar-mini.sidebar-collapse .wrapper .main-sidebar.sidebar-dark-primary.elevation-4.shiny-bound-input .brand-link .logo-content{
  width: 60px;
  height: 60px;
  background: #FFF url('https://avatars.githubusercontent.com/u/112302277?s=400&u=cbd35fa32b91217fb520bde438550648ea09cf7c&v=4');
  background-size: cover;
  background-position:center;
  background-repeat:no-repeat;
}

.layout-fixed.sidebar-mini.sidebar-collapse .wrapper .main-sidebar.sidebar-dark-primary.elevation-4.shiny-bound-input .brand-link .logo-content .logo_name{
display: none;
}

.layout-fixed.sidebar-mini.sidebar-collapse .wrapper .main-sidebar.sidebar-dark-primary.elevation-4.shiny-bound-input .brand-link .logo-content .bx-bar-chart-square {
    display: none;
}
    </style>
    <title>Document</title>
</head>
<div class='logo-content'>
    <div class='logo'>
        <i class='bx bx-bar-chart-square'></i>
        <div class='logo_name'>EpiCardio</div>
    </div>
</div>"),
                           tags$style(".inner{font-size:200%;color:white;face:bold}"),
                           tags$style(".row{display:flex;place-items:center}"),
                           tags$style("#header_text{font-size:200%}"),
                           controlbarIcon = icon("th", lib = "glyphicon"),
                           rightUi = (dropdownMenu(type = "messages",
                                        messageItem(from = "José A. Ávila Cabreja",
                                                    message = "Puede enviar sugerencias a mi correo"
                                                    ))
                                      )
                           ),
  sidebar = dashboardSidebar(
    bs4SidebarUserPanel(name = tags$a(href = "mailto:javilacabreja@gmail.com","José A. Ávila Cabreja"), image = "https://media-exp1.licdn.com/dms/image/C4E03AQFn3EdRI1ihBw/profile-displayphoto-shrink_400_400/0/1640650841613?e=1667433600&v=beta&t=LqpqRj_jmdC_Dss8hUP1Z_zPnQqXqmsWSXWjFGfflGY"),
    sidebarMenu(id = "sidebarid",
                menuItem("Home", tabName = "home", icon = icon("fas fa-house")),
                menuItem("General", tabName = "general", icon = icon("fas fa-rectangle-list")),
                menuItem("Rate of changes", tabName = "changes", icon = icon("transfer",lib = "glyphicon")),
                menuItem("Forecasts", tabName = "forecasts", icon = icon("chart-line"))
    )
  ),
  body = dashboardBody(
    tabItems(
      # * First Tab Information ---- 
      tabItem("home",
              tags$h1("EpiCardio v.1.0")
              ),
      #  * Second tab with general information about the time period ----
      tabItem("general",
              fluidRow(id = "sexos",
                  column(valueBoxOutput("both_mortality_box", width = 10), width = 4),
                  column(valueBoxOutput("female_mortality_box", width = 10), width = 4),
                  column(valueBoxOutput("male_mortality_box", width = 10), width = 4)
                  ),
              box(
                title = " Mortality of cardiovascular diseases by sex, age and causes",
                solidHeader = TRUE,
                status = "primary",
                icon = icon("signal", lib = "glyphicon"),
                width = 11.5,
                fluidRow(
                  column(6,
                         selectInput("cause1", "Causes:",
                                     choices = unique(db$Cause))),
                  column(6,
                         selectInput("medida1", "Measure:", choices = c("Total", "Rate"))),
                  br(),
                  column(12,
                  echarts4rOutput("plot1", height = 600))),
                footer = HTML("<p style='font-size:100%'><i>Note:</i> Rate by 100 000 persons.</p>")
                ),
              box(
                title = " Trends in mortality for cardiovascular diseases between 2010 and 2020",
                solidHeader = TRUE,
                status = "warning",
                icon = icon("random", lib = "glyphicon"),
                width = 11.5,
                fluidRow(
                  column(4,
                         selectInput("cause0", "Causes:",
                                     choices = unique(db$Cause))),
                  column(4,
                         selectInput("age0", "Age:",
                                     choices = unique(db$Age))),
                  column(4,
                         selectInput("medida0", "Measure:", choices = c("Total", "Rate"))),
                  br(),
                  column(12,
                         echarts4rOutput("plot0", height = 600))),
                  footer =  HTML("<p style='font-size:100%'><i>Note:</i> Rate by 100 000 persons.</p>")
              ),
              box(title = " Mortality of cardiovascular diseases by province",
                  solidHeader = TRUE,
                  status = "danger",
                  icon = icon("globe", lib = "glyphicon"),
                  width = 11.5,
                  echarts4rOutput("map", height = 600)
                    )
              ),
      # * Third tab with annualized rate of change by cause, age, sex and province ----
      tabItem("changes",
              box(
                title = "Annualised rate of change in mortality of cardiovascular diseases by sex  2010-2020",
                solidHeader = TRUE,
                status = "lightblue",
                icon = icon("transfer", lib = "glyphicon"),
                width = 11.5,
                fluidRow(class = "rates",
                  column(6,
                         selectInput("cause2", "Causes:",
                                     choices = unique(db$Cause))),
                  column(6,
                         selectInput("sex2", "Sex:",
                                     choices = unique(db$Sex))),
                  column(12,
                         textOutput("header_text")),
                  br(),
                  column(4,
                         dataTableOutput("rate_change_table", width = 400)),
                  column(8,
                         echarts4rOutput("plot2", height = 460))),
                  footer = HTML("<p style='font-size:100%'><i>Note:</i> Data are reported in percentage.</p>")
              ),
              box(
                title = "Annualised rate of change in mortality of cardiovascular diseases by province 2010-2020",
                solidHeader = TRUE,
                status = "indigo",
                icon = icon("transfer", lib = "glyphicon"),
                width = 11.5,
                footer =  fluidRow(
                 column(12, HTML("<p style='font-size:100%'><i>Note:</i> Data are reported in percentage.</p>")),
                 column(12, paste(acr_province$Provinces, ": ", unique(provincias$Province), collapse = ", "))),
                echarts4rOutput("plot3", height = 800)
              )
              ),
      # * Fourth tab with forecasting ----
      tabItem("forecasts",
              box(
                title = "Cardiovascular disease mortality in both sexes in the next 5 years",
                solidHeader = TRUE,
                status = "navy",
                icon = icon("chart-line"),
                width = 11.5,
                echarts4rOutput("forecast_plot")
              )
              ))
    
  ),
  controlbar = dashboardControlbar(skinSelector(), width = 250),
  title = "EpiCardio"
  )

# SERVER ----

server <- function(input, output, session){
  
  
  # * Value boxes ----
  output$both_mortality_box <- renderValueBox({
    valueBox(value = f_comma(sum(both$N)), subtitle = "2010-2020", icon = icon("fas fa-heart"),
             width = 6, color = "orange", footer = "Both sex")
  })
    
  output$female_mortality_box <- renderValueBox({
    valueBox(value = f_comma(sum(female$N)), subtitle = paste(round(sum(female$N)/sum(both$N)*100), "%"), icon = icon("fas fa-female"),
             width = 6, color = "purple", footer = "Female")
  })
  
  output$male_mortality_box <- renderValueBox({
    valueBox(value = f_comma(sum(male$N)), subtitle = paste(round(sum(male$N)/sum(both$N)*100), "%"), icon = icon("fas fa-male"),
               width = 6, color = "primary", footer = "Male")
  })
  
  
  # * PLOT1 ----
  
  output$plot1 <- renderEcharts4r({
    if(input$medida1=="Total"){
      db %>% 
        filter(Cause == input$cause1,
               Age != "All Ages") %>% 
        group_by(Sex) %>% 
        e_charts(Age) %>% 
        e_bar(N) %>% 
        e_color(color = c("#FD7E14", "#6F42C1", "#007BFF")) %>% 
        e_title("Mortality of cardiovascular diseases by cause, age and sex 2010-2020",
                "Data extracted from the Statistical Yearbook on Health 2010-2020") %>% 
        e_tooltip(trigger = "item") %>% 
        e_legend(center=0, top = 570) %>% 
        e_show_loading()
    } else {
      db %>% 
        filter(Cause == input$cause1,
               Age != "All Ages") %>% 
        group_by(Sex) %>% 
        e_charts(Age) %>% 
        e_bar(Rate) %>% 
        e_color(color = c("#FD7E14", "#6F42C1", "#007BFF")) %>% 
        e_title(text = "Mortality of cardiovascular diseases by cause, age and sex 2010-2020",
                subtext =  "Data extracted from the Statistical Yearbook on Health 2010-2020") %>% 
        e_tooltip(trigger = "item") %>% 
        e_legend(center=0, top = 570) %>% 
        e_show_loading()
    }
  })
  
  # * PLOT2 ---- 
  
  trends_table <- reactive({
    db %>% 
      filter(Cause == input$cause0,
             Age == input$age0) %>%
      arrange(Year) %>% 
      mutate(Year2 = as.character(Year))
  })
  

  output$plot0 <- renderEcharts4r({
    if(input$medida0=="Total"){
      trends_table() %>% 
        group_by(Sex) %>% 
        e_charts(Year2) %>% 
        e_line(N) %>% 
        e_color(color = c("#FD7E14", "#6F42C1", "#007BFF")) %>%
        e_title("Trends in mortality of cardiovascular diseases by cause, age and sex 2010-2020",
                "Data extracted from the Statistical Yearbook on Health 2010-2020") %>% 
        e_tooltip(trigger = "axis") %>% 
        e_legend(center=0, top = 570)%>% 
        e_show_loading()
    } else {
      trends_table() %>% 
        group_by(Sex) %>% 
        e_charts(Year2) %>% 
        e_line(Rate) %>% 
        e_color(color = c("#FD7E14", "#6F42C1", "#007BFF")) %>%
        e_title("Trends in mortality of cardiovascular diseases by cause, age and sex 2010-2020",
                "Data extracted from the Statistical Yearbook on Health 2010-2020") %>% 
        e_tooltip(trigger = "axis") %>% 
        e_legend(center=0, top = 570) %>% 
        e_show_loading()
    }
  })
  
  # * MAP PLOT3 ----
  output$map <- renderEcharts4r({
    provincias %>% 
      group_by(Year) %>% 
      e_charts(Province, timeline = TRUE) %>% 
      em_map("Cuba") %>% 
      e_map(Total, "Cuba") %>% 
      e_visual_map(Total) %>% 
      e_tooltip(trigger = "item", formatter = htmlwidgets::JS("
      function(params){
        return('<strong>' + params.name + ':' +'</strong><br /> ' + params.value + ' deaths')}")) %>% 
      e_timeline_opts(axis_type = "time") %>% 
      e_title("Mortality of cardiovascular diseases in Cuba by province 2010-2020",
              "Data extracted from the Statistical Yearbook on Health 2010-2020") %>% 
      e_show_loading()
  })
  
  sex <- reactive({
    if(input$sex2=="Both"){paste("both sexes")
    } else {
        if(input$sex2=="Female"){paste("females")
        } else {
            if(input$sex2=="Male"){paste("males")}
          }
    }
  })
  
  
  # * TABLE-PLOT4 ----
  output$header_text <- renderText({
    paste("Annualised rate of change for", tolower(input$cause2),  "mortality for", sex(), "(%)")
    })
  
  rate_change_table <- reactive({
    acr %>% 
      filter(Sex == input$sex2,
             Cause == input$cause2) %>% 
      select(Age, ARC, Lower,  Upper) %>% 
      mutate(color = if_else(ARC > 0, "#dc3545", "#5470C6")) %>% 
      as.data.frame()
  })
  



  output$rate_change_table <- renderDataTable({
    datatable(rate_change_table()[,1:4], filter = "none",
              options = list(paging=FALSE, searching=FALSE, info = FALSE, ordering = FALSE),
              selection = "none",
              rownames = FALSE,
              style = "bootstrap4")
  })
  
  output$plot2 <- renderEcharts4r({
    rate_change_table() %>% 
      e_charts(Age) %>% 
      e_bar(ARC, legend = FALSE) %>%
      e_add_nested("itemStyle", color) %>%
      e_error_bar(lower = Lower, upper = Upper,
                  itemStyle = list(borderWidth = 2)
                  ) %>% 
      e_show_loading()
  })
  
  
  # * PLOT5 ----
  
  output$plot3 <- renderEcharts4r({
    acr_province %>% 
      e_charts(Provinces) %>% 
      e_line(ARC, legend = FALSE) %>% 
      e_error_bar(lower = Lower, upper = Upper,
                  itemStyle = list(borderWidth = 2)) %>% 
      e_x_axis(axisLabel = list(interval = 0, rotate = 45)) %>% 
      e_title("Annualised rate of change in mortality of cardiovascular diseases by province 2010-2020",
              "Data extracted from the Statistical Yearbook on Health 2010-2020") %>% 
      e_labels() %>% 
      e_show_loading()
  })
  
  # * FORECAST PLOT ----
  output$forecast_plot <- renderEcharts4r({
    forecast %>% 
      group_by(Serie) %>% 
      e_charts(Year) %>% 
      e_bar(Deaths, stack = "grp") %>% 
      e_color(color = c("#5470C6","#dc3545")) %>% 
      e_tooltip(trigger = "item")
    
  })
  
  
  session$onSessionEnded(stopApp)
}

# SHINY APP ----

shinyApp(ui = ui, server = server)
