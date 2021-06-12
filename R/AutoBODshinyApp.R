## AutoBOD Shiny App
# Daniel Lowenstein

# comments/description to come



#' Title
#'
#' @return
#' @export
#'
#' @examples
AutoBOD_app <- function(){
  
  ui <- shiny::fluidPage(theme = "bootstrap.css",
                         tabsetPanel(
                           tabPanel(
                             title = "Choose a File",
                             sidebarLayout(
                               sidebarPanel(
                                 shinyFilesButton("GetFile", "Choose a log file" ,
                                                  title = "Please select a log file:", multiple = FALSE,
                                                  buttonType = "default", class = NULL),
                                 checkboxInput("header", "Check if there are already column names in your source file.", FALSE),
                                 numericInput("first_row", label = h3("On what row does your data begin?"), value = 1)
                                 
                               ),
                               mainPanel(tableOutput("values")
                                         
                               )
                               
                             )
                           ),
                           tabPanel(
                             plotOutput('plot', height = "600px"),
                             title = "O2 Conc (umol/L) vs. Time",
                             br(),
                             tags$hr(),
                             fluidRow(
                               column(3,
                                      uiOutput("bottle_input")),
                               column(4,
                                      uiOutput("time_range"),
                                      checkboxInput('y_auto', 'Auto Y-Axis',value = TRUE),
                                      sliderInput('y_axis', "Y-Axis Limits", min = 0, max = 500, value = c(0, 500), step = 10, round = 1)),
                               column(3, 
                                      actionButton("refresh", "Update")))
                             
                             
                             
                           )
                         )
  )
  
  # Define server logic ----
  server <- function(input, output) {
    
    volumes <- getVolumes()
    
    autoBOD_file <- reactiveValues(path = NULL)
    
    observe({
      shinyFileChoose(input, "GetFile", roots = volumes)
      
      if (!is.null(input$GetFile)) {
        file_selected <- parseFilePaths(volumes, input$GetFile)
        autoBOD_file$path <- as.character(file_selected$datapath)
        req(autoBOD_file$path)
        autoBOD_file$data <- read.table(autoBOD_file$path, fill = TRUE)
      }
    })
    
    observeEvent(input$refresh, {
      req(autoBOD_file$path)
      autoBOD_file$data <- read.table(autoBOD_file$path, fill = TRUE)
      
    })
    
    
    
    
    # show the head of the file we import to make sure the format is right
    output$values <- shiny::renderTable({
      
      req(autoBOD_file$data)
      
      bottles <- autoBOD_file$data
      
      if(input$header == FALSE){
        colnames(bottles) <- c("amplitude", "phase",  "oxygen", "error", "encoder", "bottle", "sample", "Date", "Time", "IRdetT", "IRBotT", "mystery_column", "Corrective_Steps", "Serial_Number")} #"IR_det_RAW", "IR_bot_RAW",airTemp no longer reported
      
      if(input$first_row != 1){
        bottles <- bottles[-c(1:(input$first_row - 1)),]
      }
      
      bottles$phase <- sapply(bottles$phase, function(x) as.numeric(as.character(x)))
      #bottles$airTemp <- sapply(bottles$airTemp, function(x) as.numeric(as.character(x))) #airTemp no longer reported in autoBOD output
      
      bottles <- bottles %>% dplyr::mutate(new_phase = phase/100,
                                           airsat = calc_air_sat(phase = new_phase, IRBotT = IRBotT),
                                           o2conc = calc_o2_conc(airsat = airsat, IRBotT = IRBotT),
                                           o2conc_umol_L = o2conc*31.25, datetime = as.POSIXct(lubridate::mdy_hms(paste(Date, Time, sep = " "))),
                                           unclassdatetime = as.POSIXct(datetime))
      
      return(head(bottles))
      
    })
    
    output$bottle_input <- shiny::renderUI({
      shiny::req(autoBOD_file$data)
      bottles <- autoBOD_file$data
      
      if(input$first_row != 1){
        bottles <- bottles[-c(1:(input$first_row - 1)),]
      }
      if(input$header == FALSE){
        colnames(bottles) <- c("amplitude", "phase",  "oxygen", "error", "encoder", "bottle", "sample", "Date", "Time", "IRdetT", "IRBotT", "mystery_column", "Corrective_Steps", "Serial_Number")} #"IR_det_RAW", "IR_bot_RAW", "airTemp" no longer reported
      
      shinyWidgets::pickerInput(
        inputId = "bottle_choice",
        label = "Choose bottle",
        choices = c("All", c(1:12)),
        multiple = FALSE,
        selected = "All"
      )
    })
    
    output$time_range <- shiny::renderUI({
      shiny::req(autoBOD_file$data)
      bottles <- autoBOD_file$data
      
      if(input$header == FALSE){
        colnames(bottles) <- c("amplitude", "phase",  "oxygen", "error", "encoder", "bottle", "sample", "Date", "Time", "IRdetT", "IRBotT", "mystery_column", "Corrective_Steps", "Serial_Number")} #"IR_det_RAW", "IR_bot_RAW", airTemp no longer reported
      
      if(input$first_row != 1){
        bottles <- bottles[-c(1:(input$first_row - 1)),]
      }
      
      bottles$phase <- sapply(bottles$phase, function(x) as.numeric(as.character(x)))
      #bottles$airTemp <- sapply(bottles$airTemp, function(x) as.numeric(as.character(x)))
      
      bottles <- bottles %>% mutate(new_phase = phase/100,
                                    airsat = calc_air_sat(phase = new_phase, IRBotT = IRBotT),
                                    o2conc = calc_o2_conc(airsat = airsat, IRBotT = IRBotT),
                                    o2conc_umol_L = o2conc*31.25, datetime = as.POSIXct(lubridate::mdy_hms(paste(Date, Time, sep = " "))),
                                    unclassdatetime = as.POSIXct(datetime))
      
      shiny::validate(
        need(input$bottle_choice, 'Please select at least one bottle')
      )
      
      bottle_plot <- bottles
      
      # subset for bottle choice
      if("All"%in%input$bottle_choice!=TRUE){
        bottle_plot <- bottle_plot %>% filter(bottle == input$bottle_choice)
      }
      
      min_time <- as.POSIXct(min(as.numeric(bottle_plot$datetime)), origin = "1970-01-01 00:00.00 UTC")
      max_time <- as.POSIXct(max(as.numeric(bottle_plot$datetime)), origin = "1970-01-01 00:00.00 UTC")
      shiny::sliderInput(inputId = "range",
                         label = "Time Range",
                         min = min_time, max = max_time,
                         value = c(min_time, max_time), step = 60)
      
    })
    
    
    output$plot <- renderPlot({
      req(autoBOD_file$data)
      
      shiny::validate(
        need(input$bottle_choice, 'Please select at least one bottle')
      )
      
      bottles <- autoBOD_file$data
      
      if(input$header == FALSE){
        colnames(bottles) <- c("amplitude", "phase",  "oxygen", "error", "encoder", "bottle", "sample", "Date", "Time", "IRdetT", "IRBotT", "mystery_column", "Corrective_Steps", "Serial_Number")} #"IR_det_RAW", "IR_bot_RAW", "airTemp", no longer reported
      
      if(input$first_row != 1){
        bottles <- bottles[-c(1:(input$first_row - 1)),]
      }
      
      bottles$phase <- sapply(bottles$phase, function(x) as.numeric(as.character(x)))
      #bottles$airTemp <- sapply(bottles$airTemp, function(x) as.numeric(as.character(x)))
      
      # need to fix as.POSIXct -- no idea why it's not working
      bottles <- bottles %>% dplyr::mutate(new_phase = phase/100,
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
      
      bottle_plot <- bottle_plot %>% dplyr::filter(unclassdatetime >= as.POSIXct(input$range[1]), unclassdatetime <= as.POSIXct(input$range[2]))
      o2_vs_time <- ggplot2::ggplot(bottles, aes(x = datetime, y = o2conc_umol_L, color = bottle_number))+
        geom_point(size = 5)+
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
        guides(colour = guide_legend(override.aes = list(size=10)))+
        theme(axis.text.x = element_text(angle = 90, size = 15, family = "Times"),
              axis.text.y = element_text(size = 15, family = "Times"),
              axis.title.x = element_text(size = 20, family = "Times"),
              axis.title.y = element_text(size = 20, family = "Times"),
              panel.background = element_rect(fill = "white", colour = "black", size = 0.5, 
                                              linetype = "solid"),
              panel.grid.major = element_line(size = 0.5, linetype = "solid", color = "gray"),
              legend.text = element_text(size = 20, family = "Times"))+
        xlab("Date/Time")+
        ylab("O2 Conc. (umol/L)")+
        labs(color = "Bottle")
      
      # subset for bottle choice - if you're not
      if("All"%in%input$bottle_choice!=TRUE){
        bottle_plot <- bottle_plot %>% dplyr::filter(bottle == input$bottle_choice)
        bottles <- bottles %>% dplyr::filter(bottle == input$bottle_choice)
        
        monte_carlo_estimates <- Monte_Carlo_Slope_Sim(Bottle = bottle_plot)
        BOD_Five_Day <- as.numeric(monte_carlo_estimates$Avg_dO2_umol_hour)*0.032*120 # umol per hour * 0.032 mg/umol * 120 hours
        
        grob <- grid::grobTree(textGrob(label = paste0("Number of Pairs = ", monte_carlo_estimates$Number_of_Pairs, ", ",
                                                       "Mean dO2 per hour = ", round(monte_carlo_estimates$Avg_dO2_umol_hour, digits = 4), ", ",
                                                       "Standard Error =", round(monte_carlo_estimates$Slope_SE, digits = 4), 
                                                       "\n", 
                                                       "Predicted 5 Day BOD (mg/L) = ",
                                                       round(BOD_Five_Day, digits = 4)),
                                        x=0.3,
                                        y=0.9,
                                        hjust=0,
                                        gp=gpar(col="red", fontsize=25, fontface="italic")))
        o2_vs_time <- ggplot2::ggplot()+
          geom_point(data = bottles, aes(x = datetime, y = o2conc_umol_L), size = 5)+
          geom_point(data = bottle_plot, aes(x = datetime, y = o2conc_umol_L, color = "red"), size = 5)+
          annotation_custom(grob)+
          theme(axis.text.x = element_text(angle = 90, size = 15),
                axis.text.y = element_text(size = 15),
                axis.title.x = element_text(size = 20),
                axis.title.y = element_text(size = 20),
                panel.background = element_rect(fill = "white", colour = "black", size = 0.5, 
                                                linetype = "solid"),
                panel.grid.major = element_line(size = 0.5, linetype = "solid", color = "gray"),
                legend.text = element_text(size = 20))+
          xlab("Date/Time")+
          ylab("O2 Conc. (umol/L)")+
          labs(color = "Bottle")
        # annotate(geom = "text",
        #          color = "red",
        #          y = 248,
        #          x = as.POSIXct(mean(c(as.numeric(input$range[1]), as.numeric(input$range[2]))), origin = "1970-01-01 00:00.00 UTC"),
        #          label = paste0("Number of Pairs = ", monte_carlo_estimates$Number_of_Pairs, ", ",
        #                         "Mean dO2 per hour = ", round(monte_carlo_estimates$Avg_dO2_umol_hour, digits = 4), ", ",
        #                         "Slope Standard Error =", round(monte_carlo_estimates$Slope_SE, digits = 4)))
        
      }
      
      if(input$y_auto==FALSE){
        o2_vs_time <- o2_vs_time + ylim(c(input$y_axis[1],input$y_axis[2]))
      }
      
      o2_vs_time
      
    })
    
    
  }
  
  
  
  # Run the app ----
  shiny::shinyApp(ui = ui, server = server)
  
}
