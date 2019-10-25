## AutoBOD Shiny App
##
## 27 September 2019
setwd("C:/Users/TSQ/Desktop/Daniel Lowenstein/AutoBOD")

#install these packages if you haven't already
library(shiny)
library(tidyverse)
library(shinyWidgets)


ui <- fluidPage(theme = "bootstrap.css",
                tabsetPanel(
                  tabPanel(
                    title = "Choose a File",
                  sidebarLayout(
                    sidebarPanel( 
                      fileInput("file", h3("Select an AutoBOD .csv file",
                                           accept = c(".csv"))),
                      checkboxInput("header", "Check if there are column names in your source file.", FALSE),
                      numericInput("first_row", label = h3("On what row does your data begin?"), value = 1)
                     
                      ),
                    mainPanel(tableOutput("values")
                              
                  )
                
                )
            ), 
            tabPanel(
              plotOutput('plot', height = "800px"),
              title = "O2 Conc (umol/L) vs. Time",
              br(),
            tags$hr(),
            fluidRow(
              column(3,
                 uiOutput("bottle_input")),
              column(9,
                uiOutput("time_range")))
            
                
            )
  )
)

# Define server logic ----
server <- function(input, output) {
  library(tidyverse)
  library(grid)
  
  autoBOD_file <- reactive({
    req(input$file)
    read.csv(input$file$datapath, header = TRUE,
             sep = ",")
   
  })
  
  # show the head of the file we import to make sure the format is right
  output$values <- renderTable({
    
    req(input$file)
    
    bottles <- autoBOD_file()
    
    if(input$header == FALSE){
      colnames(bottles) <- c("amplitude", "phase", "airTemp", "oxygen", "error", "encoder", "bottle", "sample", "Date", "Time", "IRdetT", "IRBotT")}
    
    if(input$first_row != 1){
      bottles <- bottles[-c(1:(input$first_row - 1)),]
    }
    
    bottles$phase <- sapply(bottles$phase, function(x) as.numeric(as.character(x)))
    bottles$airTemp <- sapply(bottles$airTemp, function(x) as.numeric(as.character(x)))
    
    # need to fix as.POSIXct -- no idea why it's not working
    bottles <- bottles %>% mutate(new_phase = phase/100, 
                                  airsat = calc_air_sat(phase = new_phase, IRBotT = IRBotT), 
                                  o2conc = calc_o2_conc(airsat = airsat, IRBotT = IRBotT), 
                                  o2conc_umol_L = o2conc*31.25, datetime = as.POSIXct(lubridate::mdy_hms(paste(Date, Time, sep = " "))), 
                                  unclassdatetime = as.POSIXct(datetime))
    
    return(head(bottles))
    
  })

  output$bottle_input <- renderUI({
    req(autoBOD_file())
    bottles <- autoBOD_file()
    
    if(input$first_row != 1){
      bottles <- bottles[-c(1:(input$first_row - 1)),]
    }
    if(input$header == FALSE){
      colnames(bottles) <- c("amplitude", "phase", "airTemp", "oxygen", "error", "encoder", "bottle", "sample", "Date", "Time", "IRdetT", "IRBotT")}
    
    pickerInput(
      inputId = "bottle_choice",
      label = "Choose bottle",
      choices = c("All", c(1:12)),
      multiple = FALSE, 
      selected = "All"
    )
  })
  
  output$time_range <- renderUI({
    req(autoBOD_file())
    bottles <- autoBOD_file()
    
    if(input$header == FALSE){
      colnames(bottles) <- c("amplitude", "phase", "airTemp", "oxygen", "error", "encoder", "bottle", "sample", "Date", "Time", "IRdetT", "IRBotT")}
    
    if(input$first_row != 1){
      bottles <- bottles[-c(1:(input$first_row - 1)),]
    }
    
    bottles$phase <- sapply(bottles$phase, function(x) as.numeric(as.character(x)))
    bottles$airTemp <- sapply(bottles$airTemp, function(x) as.numeric(as.character(x)))
    
    # need to fix as.POSIXct -- no idea why it's not working
    bottles <- bottles %>% mutate(new_phase = phase/100, 
                                  airsat = calc_air_sat(phase = new_phase, IRBotT = IRBotT), 
                                  o2conc = calc_o2_conc(airsat = airsat, IRBotT = IRBotT), 
                                  o2conc_umol_L = o2conc*31.25, datetime = as.POSIXct(lubridate::mdy_hms(paste(Date, Time, sep = " "))), 
                                  unclassdatetime = as.POSIXct(datetime))
    
    validate(
      need(input$bottle_choice, 'Please select at least one bottle')
    )
    
    bottle_plot <- bottles
    
    # subset for bottle choice - if you're not
    if("All"%in%input$bottle_choice!=TRUE){
      bottle_plot <- bottle_plot %>% filter(bottle == input$bottle_choice)
    }
    
    min_time <- as.POSIXct(min(as.numeric(bottle_plot$datetime)), origin = "1970-01-01 00:00.00 UTC")
    max_time <- as.POSIXct(max(as.numeric(bottle_plot$datetime)), origin = "1970-01-01 00:00.00 UTC")
    sliderInput(inputId = "range", 
                label = "Time Range", 
                min = min_time, max = max_time, 
                value = c(min_time, max_time), step = 60)

  })

  
  output$plot <- renderPlot({
    req(input$file)
    
    validate(
      need(input$bottle_choice, 'Please select at least one bottle')
    )

    bottles <- autoBOD_file()

    if(input$header == FALSE){
      colnames(bottles) <- c("amplitude", "phase", "airTemp", "oxygen", "error", "encoder", "bottle", "sample", "Date", "Time", "IRdetT", "IRBotT")}
    
    if(input$first_row != 1){
      bottles <- bottles[-c(1:(input$first_row - 1)),]
    }
    
    bottles$phase <- sapply(bottles$phase, function(x) as.numeric(as.character(x)))
    bottles$airTemp <- sapply(bottles$airTemp, function(x) as.numeric(as.character(x)))

    # need to fix as.POSIXct -- no idea why it's not working
    bottles <- bottles %>% mutate(new_phase = phase/100,
                                  airsat = calc_air_sat(phase = new_phase, IRBotT = IRBotT),
                                  o2conc = calc_o2_conc(airsat = airsat, IRBotT = IRBotT),
                                  o2conc_umol_L = o2conc*31.25, datetime = as.POSIXct(lubridate::mdy_hms(paste(Date, Time, sep = " "))),
                                  unclassdatetime = as.POSIXct(datetime),
                                  bottle_number = as.factor(bottle))%>% filter(bottle_number == 1 |
                                                                                 bottle_number == 2 |
                                                                                 bottle_number == 3 |
                                                                                 bottle_number == 4 |
                                                                                 bottle_number == 5 |
                                                                                 bottle_number == 6 |
                                                                                 bottle_number == 7 |
                                                                                 bottle_number == 8 |
                                                                                 bottle_number == 9 |
                                                                                 bottle_number == 10 |
                                                                                 bottle_number == 11 |
                                                                                 bottle_number == 12)



    bottle_plot <- bottles 
    
    bottle_plot <- bottle_plot %>% filter(unclassdatetime >= as.POSIXct(input$range[1]), unclassdatetime <= as.POSIXct(input$range[2]))
    o2_vs_time <- ggplot(bottles, aes(x = datetime, y = o2conc_umol_L, color = bottle_number))+
      geom_point()+
      scale_color_manual(values = c("1" = "darkorange1", 
                                    "2" = "darkgreen", 
                                    "3" = "palegreen", 
                                    "4" = "orchid", 
                                    "5" = "orangered2",
                                    "6" = "steelblue4",
                                    "7" = "skyblue",
                                    "8" = "violetred",
                                    "9" = "goldenrod",
                                    "10" = "cyan",
                                    "11" = "blue",
                                    "12" = "blueviolet"))+
      guides(colour = guide_legend(override.aes = list(size=10)))

    # subset for bottle choice - if you're not
    if("All"%in%input$bottle_choice!=TRUE){
      bottle_plot <- bottle_plot %>% filter(bottle == input$bottle_choice)
      bottles <- bottles %>% filter(bottle == input$bottle_choice)
      
      monte_carlo_estimates <- Monte_Carlo_Slope_Sim(Bottle = bottle_plot)
      grob <- grobTree(textGrob(label = paste0("Number of Pairs = ", monte_carlo_estimates$Number_of_Pairs, ", ",
                                               "Mean dO2 per hour = ", round(monte_carlo_estimates$Avg_dO2_umol_hour, digits = 4), ", ",
                                               "Standard Error =", round(monte_carlo_estimates$Slope_SE, digits = 4)), 
                                x=0.4,  
                                y=0.95, 
                                hjust=0,
                                gp=gpar(col="red", fontsize=13, fontface="italic")))
      o2_vs_time <- ggplot()+
        geom_point(data = bottles, aes(x = datetime, y = o2conc_umol_L))+
        geom_point(data = bottle_plot, aes(x = datetime, y = o2conc_umol_L, color = "red"))+
        annotation_custom(grob)
        # annotate(geom = "text",
        #          color = "red",
        #          y = 248,
        #          x = as.POSIXct(mean(c(as.numeric(input$range[1]), as.numeric(input$range[2]))), origin = "1970-01-01 00:00.00 UTC"),
        #          label = paste0("Number of Pairs = ", monte_carlo_estimates$Number_of_Pairs, ", ",
        #                         "Mean dO2 per hour = ", round(monte_carlo_estimates$Avg_dO2_umol_hour, digits = 4), ", ",
        #                         "Slope Standard Error =", round(monte_carlo_estimates$Slope_SE, digits = 4)))

    }
    
    o2_vs_time
    
  })

    
}



# Run the app ----
shinyApp(ui = ui, server = server)



