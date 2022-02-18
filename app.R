
############################## This is the code for the Wetland conservation cost Tool ###############################################
# The code has been structured into 3 main parts

#__________ Part A : libraries
#These are the R packages used for the App: they will be installed on the Shinyapp.io server  

library(shinythemes)       # appearance of the app
library(shinyWidgets)      #custom widgets to enhance shiny applications
library(shiny)             # main shiny tool
library(DT)                # Data table package 
library(tidyverse)         # contains several functions for data wrangling and plotting
library(shinydashboard)
#library(flexdashboard)
library(plotly)            #adding interactivity to ggplots with the ggplotly
library(rmarkdown)         #for creating documents
library(htmltools)         # for accessing the user-guide
#library(markdown)
library(triangle)          # creating triangular distribution
library(splitstackshape)   # for creating fields on the landscape (specifically expand the rows in the tables based on proportion of cultivated crops and desired number of field)
library(Rmisc)             # for estimating confidence interval bands


#___________ Part B: Custom defined functions
#options(warn=-1)
#_______1. Thus script contains functions that are used to estimate the annual net returns of crop production and uncertainties around the estimates
source("functions.R") 

#_______2. User Data Editing function: This function enables the user to edit crop production information table
modDtUi <- function(id){ # UI module
  ns = NS(id)
  DT::dataTableOutput(ns('x1'))
}

modDt <-  function(input, output, session, data){ # Server module
  
  output$x1 <- DT::renderDataTable(data, selection = 'none', editable = TRUE, server = TRUE)
  proxy <- dataTableProxy('x1', session = session)
  
  updatedData <- eventReactive(input$x1_cell_edit, {
    info = input$x1_cell_edit
    if (!is.null(info)) {
      str(info)
      data[info$row, info$col] <<- DT::coerceValue(as.double(info$value),
                                                   data[info$row, info$col])
    }
    data
  }, ignoreNULL = FALSE)
  
  return(updatedData)
}

#___________ Part C: UI phase of the App_________________________________________________________________
#. This is what drives what is seen on the App by the user

ui <- 
  
  fluidPage(                                                                             # FluidPage function is used to 
    
    useShinydashboard(), # added this
    
    div(style="padding: 1px 0px; width: '100%'",
        titlePanel(
          title="", windowTitle="Wetlands"
        )
    ),
    theme = shinythemes::shinytheme("flatly"),

    navbarPage(                                                                           # The navbarPage function produces the multi-tab sections of the App
      
      title=div(h1("Wetland Conservation Tool")),
      
      tabPanel(h3("Tool Overview"),                                                        # This is the first section of the App: the overview section where provides summarized information on the App
               fluidRow(
                 column(1), 
                 column(10, htmltools::includeMarkdown("overview.Rmd")),
                 column(1)
               )),

      tabPanel(h3("Field level Analysis"),                                                # This is the field analysis tab of the App
               fluidRow(
                 tags$div(
                   
                   # The sidebarPanel function contains the user- input widgets which are grouped into 5 main sections wrapped in the dropdownButton function.
                   sidebarPanel(                                                          
                   
                   tags$h4(p("You can alter the outputs of this tool but changing the levels of the variables which are grouped in the grey buttons below.
                            Please click on a grey button to reveal the variables!", style = "color:black")),
                                 tags$a("Click here to get the User-guide for more information", href="userguide.pdf"),
                   
                   column(12,                                                             # The input-widgets are defined in column orientation in the sidebarPanel  
                          fluidRow(                                                        # The fluidRow functions creates a row orientation in the App
                            br(),
                            br(),
                            hr(),
                            tags$h4(p("1. Wetland characteristics", style = "color:black")),
                            tags$h4(tags$em("Variables: Wetland area, number of wetlands, wetland location, delayed seeding, upland-wetland area % yield difference,
                                            drainage cost")),
                            
                            dropdownButton(
                              tags$h3("List of Input"),
                              
                              numericInput("wetlandacre", p("How many acres of wetland do you have on your field?", style ="color: black"), 
                                           value = 0, min = 0.05, max = 37, step = 1),
                              sliderInput("num_wetland", p("How many wetlands do you have on your field, if any?", style ="color: black"), min = 0, max = 15, value = 0, step = 1),
                              sliderInput("delayedseeding", p("How many years out of 10 do you experience delayed seeding on drained wetland areas?", 
                                                              style ="color: black"), min = 0, max = 10, value = 0, step = 1),
                              selectInput("uplandyd_greater", p("Is upland crop yield greater than yield on drained wetland?", 
                                                                style ="color: black"), choices = c("greater","equal", "lesser"), selected = "equal"),
                              sliderInput("ydiff", "What is the % difference in upland crop yield and drained wetland crop yield", min = 0, max = 50, value = 0, step = 10),
                              
                              selectInput("wetland_interference", p("Does wetlands on your field interfere with farming operation?", 
                                                                style ="color: black"), choices = c("No", "Yes"), selected = "No"),
                              numericInput("drainage_cost", p("What is the annualized cost of draining an acre of wetland on your field?",
                                                              style ="color: black"), value = 600, min = 0, max = 2000, step = 1) 
                            ),
                            hr(),
                            tags$h4(p("2. Crop production variables.", style = "color:black")),                                     
                            tags$h4(tags$em("Variables: Soil zone, crop price, crop yield, production cost, cultivated crops")),
                            
                            dropdownButton(
                              
                              br(),
                              selectInput("soilzone", p("Select Soil Zone?", style ="color: black"), choices = c("black", "brown", "darkbrown"), selected = c("black"),
                                          multiple = TRUE),
                              br(),
                              selectInput("Cultivated_crops", p("Select the crops you cultivate on your field?", style ="color: black"), 
                                          choices = c("Feed Barley", "Malt Barley", "Yellow Peas", "Soybean", "Canola", "Corn", "Flax", "Spring Wheat", "Oats", "Fallow"),
                                          selected = c("Canola", "Spring Wheat"),
                                          multiple = TRUE),
                              br(),
                              tags$h4(tags$em("Edit selected table below with your own data.")),
                              modDtUi("editable_fl")),                                                       # the modDtUi function displays the editable crop production information
                            hr(),
                            br(),
                            tags$h4(p("3. Discounting information.", style = "color:black")),
                            tags$h4(tags$em("Variables: Discount factor, planning horizon")),
                            
                            dropdownButton(
                              tags$h3("List of Input"),
                              numericInput("planning_horizon", p("How many years do you plan to operate your farm?", style ="color: black"), 
                                           value = 50, min = 2, max = 200, step = 1),
                              numericInput("discount_rate", p("Select discount rate (%)?", style ="color: black"), value = 7, min = 1, max = 100, step = 1)
                              
                              
                            ),
                            hr(),
                            tags$h4(p("4.  Uncertainty using the triangular distribution.", style = "color:black")),
                            tags$h4(tags$em("Variables: Number of simulations. yield, crop price, drainage cost and production cost variability")),
                            
                            dropdownButton(
                              tags$h3("List of Input"),
                              numericInput("number_simulation", p("How many iterations should the simulation run?",
                                                                  style ="color: black"), value = 100, min = 50, max = 100000, step = 100),
                              sliderInput("yd_variability", p("Range of crop yield variability (%):", style ="color: black"), min = 0, max = 50, value = 0, step = 10),
                              sliderInput("cp_variability", p("Range of crop price variability (%):", style ="color: black"), min = 0, max = 50, value = 0, step = 10),
                              sliderInput("pc_variability", p("Range of production cost variability (%):", style ="color: black"), min = 0, max = 50, value = 0, step = 10),
                              sliderInput("dc_variability", p("Range of drainage cost variability (%):", style ="color: black"), min = 0, max = 50, value = 0, step = 10)),
                            hr(),
                            tags$h4(p("5.  Download simulated data.", style = "color:black")),
                            dropdownButton(
                              tags$h3("List of Input"),
                              
                               # Button
                              downloadButton("downloadData", "Download Simulated Table as CSV")
                            )))),
                   
                   # The mainPanel is the output display phase of the App
                   
                   mainPanel(                                                                                  
                   
                   column(12, 
                          br(),
                          fluidRow(
                            column(12, 
                                   
                                   tags$h4(tags$b(p("Summary of Key Wetland Information", style = "color:black"))),
                                   DT::dataTableOutput('wetland_summary')
                                   )),
                          br(),
                          fluidRow(
                            tags$h4(tags$b(p("Distribution of annual net returns of field with intact and field with drained wetland areas", style = "color:black"))),
                            plotlyOutput("box_plot", height = '600px', width = 'auto'),
                            br(),
                            br(),
                            tags$h4(tags$b(p("Probability density plot of annual net returns of the difference between field with intact and field with drained wetland areas", style = "color:black"))),
                            plotlyOutput("probability_plot", height = '600px', width = 'auto'),
                            br()
                          )
                   ))))),
      tabPanel(h3("Landscape level Analysis"),                                                        # The landscape tab of the app
               
               fluidRow(
                 
                 sidebarPanel(
                 
                 column(12,
                        
                        br(),
                        br(),
                        tags$h4(p("1. Wetland characteristics", style = "color:black")),
                        
                        
                        numericInput("wetlandacre1", p("How many acres of wetland do you have on your field?", style ="color: black"), 
                                     value = 0, min = 0.05, max = 37, step = 1),
                        sliderInput("num_wetland1", p("How many wetlands do you have on your field, if any?", style ="color: black"), min = 0, max = 15, value = 0, step = 1),
                        sliderInput("delayedseeding1", p("How many years out of 10 do you experience delayed seeding on drained wetland areas?", 
                                                         style ="color: black"), min = 0, max = 10, value = 0, step = 1),
                        selectInput("uplandyd_greater1", p("Is upland crop yield greater than yield on drained wetland?", 
                                                           style ="color: black"), choices = c("greater","equal", "lesser"), selected = "equal"),
                        sliderInput("ydiff1", "What is the % difference in upland crop yield and drained wetland crop yield", min = 0, max = 50, value = 0, step = 10),
                        
                        selectInput("wetland_interference1", p("Does wetlands on your field interfere with farming operation?", 
                                                           style ="color: black"), choices = c("No", "Yes"), selected = "No"),
                        numericInput("drainage_cost1", p("What is the annualized cost of draining an acre of wetland on your field?",
                                                         style ="color: black"), value = 600, min = 0, max = 2000, step = 1),
                        hr(),
                        hr(),
                        numericInput("conservation_budget", p("Wetland conservation budget to conserve wetland areas in the landscape?",
                                                              style ="color: black"), value = 100000, min = 1000, max = 1000000000, step = 1000),
                        numericInput("policy", p("Pencentage of highest quality fields to conserve wetlands?",
                                                 style ="color: black"), value = 25, min = 1, max = 100, step = 5))),
                 mainPanel(
                 column(11, 
                        DT::dataTableOutput("wetland_summary1")),
                 
                 column(1)
               )))
      
      )
    )

#___________ Part D: The back-end phase of the App: where all the computations are done_________________________________________________________________


server <- function(input, output) {                                                   

#_________ Crop production guide information and display
  
  fieldData <- read_csv("landsape_cropdata11.csv")                                   # Read the crop production information guide data
  
    editable_fl <- reactive({                                                        # the reactive function allows the creation of a custom crop production data based on user-defined soil zone and crops
    
    fieldData_fl <- fieldData %>% 
      dplyr::filter(soil_zone %in% input$soilzone & crop %in% input$Cultivated_crops)
    
    callModule(modDt,"editable_fl", data = fieldData_fl)
  }) 

#________Nuisance cost calculation
    
   ioc <- read.csv("ioc.csv") %>% select(-X)                                         # Read data the has information on input cost (seed, seed treatment, fertilizer use, and pesticide use)
                                                                                     # and machinery operating cost (machinery fuel and repairs)
   
    nc_df <- reactive({

     ioc_select <- ioc %>% 
      dplyr::filter(soil_zone %in% input$soilzone & crop %in% input$Cultivated_crops) %>%
      summarise(input_cost = sum(input_cost),
               mach_cost = sum(machinery_oc)) %>%
     mutate(nuisance_factor = case_when(                                             # nuisance factors are selected (from Cortus et al. (2011) Table 3) based on number of wetlands
                                    (input$num_wetland >= 1 & input$num_wetland <=3) ~ 0.08,
                                    (input$num_wetland >  3 & input$num_wetland <=6) ~ 0.09,
                                    (input$num_wetland >= 6 & input$num_wetland <=9) ~ 0.11,
                                    input$num_wetland > 9 ~ 0.14
                               ),
           input_waste_factor = 0.1,                                                #input waste factor are assumed to be 10% of input cost as farmer applies inputs around wetland areas
           nuisance_cost_mach = nuisance_factor * mach_cost,                        # nuisance cost associated with machinery operation: nuisance factor times machinery operating cost
           nuisance_cost_input = input_waste_factor * input_cost,                   # nuisance cost associated with input wastage (overlap): input waste factor times input cost
           Efficiency_loss = round(nuisance_cost_mach + nuisance_cost_input,0)) %>% select(Efficiency_loss)  # Efficiency loss is the sum of the two above
    
     return(ioc_select)
    
     })
  # Creating the main table that will be used for the main analysis from user applied information

  df_farmacre_wl <- reactive({
    
    nc <- nc_df()[1,]                                                               # Nuisance cost estimate
    
    
    editable_fl()() %>%                                                             # the editable_fl()() is a reactive table that is the product of user-selected crop production information
      dplyr::mutate(prod_cost = variable_cost + fixed_cost,                         # total production cost
                    wetland_interference = as.numeric(ifelse(input$wetland_interference == "No", 0,1)),   #binary variables which is 1 if wetlands interfere with farming operations and 0 otherwise
                    number_wetlands = input$num_wetland,                                                  
                    wetland_acre = as.numeric(input$wetlandacre),                                          
                    nuisance_cost= as.numeric(ifelse(wetland_acre==0 |input$num_wetland==0 | wetland_interference==0, 0, nc*(0.5*wetland_acre))),   #nuisance cost = 0 if wetland area or number of wetlands or wetland interference are = 0 otherwise estimated nuisance cost which applies to half wetland area
                    num_crops = length(input$Cultivated_crops),
                    proportion_cultivated = 1/num_crops,                             # proportion cultivated apportions a proportion of each crop to an area of field or wetland area
                    farm_acre = 160 - wetland_acre,                                  # land available for cultivation on the field with drained wetland areas                            
                    farm_acre_wl = 160,                               
                    delayed_seeding = 1 - as.numeric(input$delayedseeding/10),       # delayed seeding: if it is 0 out of 100 then the full yield/acre ((1-0)*yield) will apply on the wetland area
                    discount_rate = as.numeric(input$discount_rate/100),             # discount factor for net present estimate
                    planning_horizon = as.numeric(input$planning_horizon),           # planning horizon for net present estimate
                    dr_cost = as.numeric(input$drainage_cost),                       # drainage cost
                    simulations = as.numeric(input$number_simulation),               # the number of times to estimate annual net returns
                    yd_variability = input$yd_variability,                           # the variability variables drives the uncertainty around the annual net returns
                    yieldsimulation = as.numeric(ifelse(yd_variability > 0, 1,0)),
                    min_yield = as.numeric(avg_yield - (avg_yield*(100 -(input$yd_variability))/100)),
                    max_yield = as.numeric(avg_yield + (avg_yield*(100 +(input$yd_variability))/100)),
                    cp_variability = input$cp_variability, 
                    pricesimulation = as.numeric(ifelse(cp_variability > 0, 1,0)),
                    min_crop_price = as.numeric(avg_crop_price - (avg_crop_price*(100 -(input$cp_variability))/100)),
                    max_crop_price = as.numeric(avg_crop_price + (avg_crop_price*(100 +(input$cp_variability))/100)),
                    dc_variability = input$dc_variability, 
                    drainagecostsimulation = as.numeric(ifelse(dc_variability > 0, 1,0)),
                    min_drainage_cost = as.numeric(dr_cost - (dr_cost*(100 -(input$dc_variability))/100)),
                    max_drainage_cost = as.numeric(dr_cost + (dr_cost*(100 +(input$dc_variability))/100)),
                    pc_variability = input$pc_variability, 
                    productioncostsimulation = as.numeric(ifelse(pc_variability > 0, 1,0)),
                    min_prod_cost = as.numeric(prod_cost - (prod_cost*(100 -(input$pc_variability))/100)),
                    max_prod_cost = as.numeric(prod_cost + (prod_cost*(100 +(input$pc_variability))/100)),
                    upland_wtland_yd = case_when(                                 # This is where the yield differences between upland area and drained area is defined
                                input$uplandyd_greater == "greater" ~ 1,
                                input$uplandyd_greater == "equal" ~ 2,
                                input$uplandyd_greater == "lesser" ~ 3,
                              ),
                    yd_diff = case_when(                                          # yield difference factor based on upland_wtland_yd. This will be used to adjust drained wetland areas crop yield
                      upland_wtland_yd == 1  ~ as.numeric(-1*(input$ydiff*avg_yield)/100),
                      upland_wtland_yd == 3  ~ as.numeric((input$ydiff*avg_yield)/100),
                      upland_wtland_yd == 2  ~ as.numeric(0)
                          ),
                    yd_diff = ifelse(wetland_acre==0, 0, yd_diff)                # we specify that yield difference factor must be equal to 0 if there are no wetlands on the field
                    ) 
    
  
  })
  
  ############################################################# Main App outputs
  
  #____Wetland summary table output
  output$wetland_summary <- DT::renderDataTable({
    
    sum <- df_farmacre_wl() %>% select(number_wetlands, wetland_acre) 
    DT::datatable(sum[1,],  options = list(dom = "t", columnDefs = list(list(className = 'dt-center', targets = "_all"))), rownames = FALSE)
  })

  
  #______Annual net return estimates table using functions stored in the functions script
  wlfl_df <- reactive({
    
    df_flwl <- df_farmacre_wl()
    npv_1 <- simulated_npv(df=df_flwl) 
    
    return(npv_1)
    
  })
  

  #______Functions to download simulated data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("simulated_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(wlfl_df(), file, row.names = FALSE)
    }
  )
  
  #____Annual net return box plot output
  
  output$box_plot<- renderPlotly({
    
    fldf <- wlfl_df() %>% dplyr::mutate(npv = as.integer(npv_fl), Label = "Field with intact wetland area") %>% dplyr::select(npv, Label)
    wlfldf <- wlfl_df() %>% dplyr::mutate(npv = as.integer(npv_flwl), Label = "Field with drained wetland area") %>% dplyr::select(npv, Label)
    
    df_all <- wlfldf %>% rbind(fldf)
    
    ggplotly(ggplot(df_all, aes(x=Label, y=npv, fill=Label)) + geom_boxplot()  +
               labs(
                 x ="", y="$/Acre/Year") +
               theme_bw() +
               theme(text = element_text(size=14, colour="dark green")) +
               theme(legend.position="none"))
  })
  
  #____Probability density plot output
  
  output$probability_plot <- renderPlotly({
    
    wldf <- wlfl_df() %>% mutate(npv = as.integer(npv_wl)) %>% dplyr::select(npv)
    
    ggplotly(ggplot(wldf, aes(npv)) + 
               geom_density(alpha=.2, fill="#FF6666") +
               labs(
                 x ="Annual Net Returns ($/Acre/Yr) ", y="Probability") + 
               theme_bw() +
               theme(text = element_text(size=14, colour="dark green")))
  })
  

######################################### Landscape analysis section  
  
  
  df_landscape <- reactive({
    
    nc <- nc_df()[1,] #%>% filter(implement_width == input$implement_width) %>% select(input$num_wetland)
    
    
    A <- editable_fl()() %>%
      dplyr::mutate(prod_cost = variable_cost + fixed_cost,
                    num_crops = length(input$Cultivated_crops),
                    proportion_cultivated = 1/num_crops,
                    number_fields = 20,
                    expand_factor = as.integer(proportion_cultivated * number_fields),
                    wetland_interference = as.numeric(ifelse(input$wetland_interference1 == "No", 0,1)),
                    number_wetlands = input$num_wetland1,
                    wetland_acre = as.numeric(input$wetlandacre1),
                    loss = as.numeric(ifelse(wetland_acre==0 |input$num_wetland1==0, 0, nc*0.5)),
                    nuisance_cost = as.numeric(ifelse(wetland_interference==0,  0, loss))
                    
                    ) 
    
    B <- expandRows(A, "expand_factor")
    return(B)
    
  })
    
    
  output$wetland_summary1 <- DT::renderDataTable({
    
    sum <- df_farmacre_wl() %>% select(number_wetlands, wetland_acre) #%>% rename(Number_wetlands = number_wetlands, Wetland_acreage = wetland_acre)
    DT::datatable(df_landscape(),  options = list(dom = "t", columnDefs = list(list(className = 'dt-center', targets = "_all"))), rownames = FALSE)
  })
}

shinyApp(ui = ui, server = server)