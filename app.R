library(shiny);library(dplyr);library(tidyr);library(DT); library(ggplot2); library(shinycssloaders)

df <- read.csv("matchData_2020.csv", header=T, stringsAsFactors = F)
states <- levels(factor(df$State))
years <- levels(factor(df$Year))
specialties <- levels(factor(df$simpleSpecialty))

ui <- navbarPage("Match Data",
  header = 
    div(
      tags$head(
        includeHTML("google-analytics.js"),
        includeCSS("closableFooterCSS.css")
        ),
      includeHTML("closableFooterHTML.html"),
      style="margin-left:2%; margin-right:2%;",
      titlePanel("Fellowship Match Data"),
      div(style="border:solid; background-color:#f5f7fa; border-radius: 25px",
          column(4,selectizeInput(inputId = "specialty",
                      label = "Choose specialty",
                      choices = c("All specialties", sort(specialties)),
                      selected = "All specialties")),      
          column(4,selectInput(inputId = "state",
                      label = "Choose state",
                      choices = c("All states", states),
                      selected = "All states")),
          column(4,selectInput(inputId = "year",
                      label = "Choose year",
                      choices = c("All years",years),
                      selected = "2020")),
      fluidRow(
        column(6, align = "center", checkboxInput("SOAP", label = "Only show programs with SOAP positions?", value = FALSE)),
        column(6, align = "center", downloadButton('downloadData', 'Download filtered data'))
        )
      ),
      br()
  ),
  footer = div(column(12,
      br(),
      p("Source:", a(href="https://mk0nrmpcikgb8jxyd19h.kinstacdn.com/wp-content/uploads/2019/02/SMS_Program_Results_2015_2019.pdf", 
                     "NRMP 2019-2015 Main Match Results"), align = "right"),
      p("Source:", a(href="https://mk0nrmp3oyqui6wqfm.kinstacdn.com/wp-content/uploads/2020/02/Program-Results-SMS-2016-2020.pdf", 
                     "NRMP 2020-2016 Main Match Results"), align = "right")
  )),
  tabPanel("Data Browser",
    div(DT::dataTableOutput("mytable"))
    ),
  tabPanel("Plots",
    div(style="border:solid; border-radius: 25px; border-width: 1px",
      fluidRow(h4(textOutput("countText"), align = "center")),
      fluidRow(style="height=25%;",
        column(12, withSpinner(plotOutput("countPlot")) )
      ),
      fluidRow(br())
    ),
    div(fluidRow(br())),
    div(style="border:solid; border-radius: 25px; border-width: 1px",
      fluidRow(h4(textOutput("rateText"), align = "center")),
      fluidRow(style="height=25%;",
        column(12, withSpinner(plotOutput("ratePlot")) )
      ),
      fluidRow(br())
    ),
    includeHTML("closableFooterJS.html")
  )
)

server <- function(input, output) {

  
  ##DATA TABLE DISPLAY##
  data.browse <- reactive({
    if (input$state != "All states") {df <- df %>% filter(State == input$state)} 
    if (input$year != "All years") {df <- df %>% filter(Year == input$year)}
    if (input$specialty != "All specialties"){df <- df %>% filter(simpleSpecialty == input$specialty)}
    else {df <- df %>% select(-simpleSpecialty) %>% distinct()}
    df <- df %>% spread(Stat, value, fill = 0) %>% select(Program, Specialty, Code, State, City, Year, Quota, Matched, SOAP)
    if (input$SOAP == TRUE) {df <- df %>% filter(SOAP > 0)}
    df
  })
  
  output$mytable = DT::renderDataTable({data.browse()})
  
  output$downloadData <- downloadHandler(
    filename = "filteredMatchData.csv",
    content = function(file) {
      write.csv(data.browse(), file,row.names = FALSE)
    }
  )
  
  
  ##PLOT - POSITION COUNTS OVER TIME##
  output$countText <- renderText({paste("Displaying stats for", input$specialty, "in", input$state)}) 
  data.count <- reactive({
    if (input$state != "All states") {df <- df %>% filter(State == input$state)} 
    if (input$specialty != "All specialties"){df <- df %>% filter(simpleSpecialty == input$specialty)}
    else {df <- df %>% select(-simpleSpecialty) %>% distinct()}
    df <- df %>% mutate(Stat = factor(Stat, levels=c("Quota", "Matched", "SOAP")))
    df
  })
  output$countPlot <- renderPlot({
    data.count() %>% 
      ggplot(aes(x = Year, y = value)) + 
      stat_summary(fun.y = sum, geom = "path", aes(color = Stat), size = 2) +
      stat_summary(fun.y = sum, geom = "point", size = 3) +
      facet_wrap(~Stat, scales = "free") +
      theme_bw() +
      scale_color_brewer(palette = "Set1") +
      theme(legend.position = "none",
            axis.title = element_blank(),
            axis.text.x = element_text(angle = 90, vjust = 0.5),
            axis.text = element_text(size = 12),
            strip.text = element_text(size = 14, face = "bold"))
  })
  
  
  ##PLOT - MATCH RATE OVER TIME##
  output$rateText <- renderText({paste("Pre-SOAP fill rates for ", input$state, "in", input$year)})
  data.rate <- reactive({
    if (input$state != "All states") {df <- df %>% filter(State == input$state)} 
    if (input$year != "All years") {df <- df %>% filter(Year == input$year)}
    df <- df %>% 
      spread(Stat, value, fill = 0) %>%
      group_by(simpleSpecialty) %>%
      summarise(Quota = sum(Quota), Matched = sum(Matched)) %>%
      filter(Quota != 0) %>%
      mutate(PercentMatched = Matched/Quota) %>%
      arrange(desc(PercentMatched)) %>%
      mutate(simpleSpecialty = factor(simpleSpecialty, levels = simpleSpecialty),
             simpleSpecialty = droplevels(simpleSpecialty))
    df
  })
  output$ratePlot <- renderPlot({
    data.rate() %>% {
      ggplot(., aes(x = simpleSpecialty, y = PercentMatched)) +
      geom_bar(stat = "identity") +
      theme_bw() +
      coord_cartesian(ylim=c(min(.$PercentMatched, na.rm = T), 1.0)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
            axis.text = element_text(size = 12)) +
      labs(x = "",
           y = "Percentage of matched positions \n prior to SOAP")
      }
  })
}

shinyApp(ui, server)