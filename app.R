
############################## This is the code for the Wetland conservation cost Tool ###############################################
#https://rstudio.github.io/packrat/walkthrough.html
# The code has been structured into 3 main parts

#__________ Part A : libraries
#options(repos = "https://cran.rstudio.com")

#if (!require(pacman)) {
  #install.packages("pacman")
 # library(pacman)}

# Load Packages
#p_load(shinythemes, shinyWidgets,shiny,DT,tidyverse,shinydashboard,flexdashboard,plotly,htmltools,triangle,splitstackshape, lattice, Rmisc,packrat,shinythemes)

library(shinythemes)
library(shinyWidgets)
library(shiny)
library(DT)
library(tidyverse)
library(shinydashboard)
library(flexdashboard)
library(plotly)
library(rmarkdown)
library(htmltools)
library(markdown)
library(triangle)
library(Rmisc)
#___________ Part B: Custom defined functions
#options(warn=-1)
#_______1. Thus script contains functions that are used to estimate the annual net returns of crop production and uncertainties around the estimates
source("functions_npv_field.R") 

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
  
  fluidPage( 
    
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
            column(10, htmltools::includeMarkdown("overview.Rmd")),                   # IncludeMarkdown function allows us to use markdown to write the overview section
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
                                sliderInput("num_wetland", p("How many wetlands do you have on your field, if any?", style ="color: black"), min = 0, max = 15, value = 2, step = 1),
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
                              tags$h4(p("4.  Uncertainty using best fit empirical distributions of real data (except drainage cost and production cost which varies around the mean by +/- 30% of the mean).", style = "color:black")),
                              tags$h4(tags$em("Variables: Number of simulations. yield, crop price, drainage cost and production cost variability")),
                              
                              dropdownButton(
                                tags$h3("List of Input"),
                                numericInput("number_simulation", p("How many iterations should the simulation run?",
                                                                    style ="color: black"), value = 100, min = 50, max = 100000, step = 100),
                                #sliderInput("yd_variability", p("Range of crop yield variability (%):", style ="color: black"), min = 0, max = 50, value = 0, step = 10),
                                selectInput("yd_var", p("Do you want to put variability around yield?", 
                                                                      style ="color: black"), choices = c("No", "Yes"), selected = "No"),
                                selectInput("cp_var", p("Do you want to put variability around crop_price?", 
                                                                 style ="color: black"), choices = c("No", "Yes"), selected = "No"),
                                selectInput("pc_var", p("Do you want to put variability around production_cost?", 
                                                                 style ="color: black"), choices = c("No", "Yes"), selected = "No"),
                                selectInput("dc_var", p("Do you want to put variability around drainage cost?", 
                                                                 style ="color: black"), choices = c("No", "Yes"), selected = "No")),
                                #sliderInput("cp_variability", p("Range of crop price variability (%):", style ="color: black"), min = 0, max = 50, value = 0, step = 10),
                                #sliderInput("pc_variability", p("Range of production cost variability (%):", style ="color: black"), min = 0, max = 50, value = 0, step = 10),
                                #sliderInput("dc_variability", p("Range of drainage cost variability (%):", style ="color: black"), min = 0, max = 50, value = 0, step = 10)),
                              
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
                                     DT::dataTableOutput('wetland_summary'),
                                     DT::dataTableOutput("wetland_summary2")
                                     
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
                   
                   tags$h4(p("You can alter the outputs of this tool but changing the levels of the variables which are grouped in the grey buttons below.
                            Please click on a grey button to reveal the variables!", style = "color:black")),
                   # tags$a("Click here to get the User-guide for more information", href="userguide.pdf"),
                   
                   column(12,
                          
                          br(),
                          tags$h4(p("1. Wetland characteristics", style = "color:black")),
                          tags$h4(tags$em("Variables: Number of fields, wetland areas, number of wetlands, delayed seeding, upland-wetland area yield difference,
                                                    proportion of wetlands interfering with farm operations, drainage cost")),
                          dropdownButton(
                            numericInput("num_fields", p("What is the number of fields in the landscape?",
                                                         style ="color: black"), value = 100, min = 1, max = 20000, step = 100),
                            selectInput("havewl", p("Do you have wetlands on the fields in the landscape?", 
                                                    style ="color: black"), choices = c("No", "Yes"), selected = "Yes"),
                            sliderInput("min_wetlandacre", p("What is the minimum wetland acreage on the fields in the landscape?", style ="color: black"), 
                                        value = 0, min = 0, max = 10, step = 0.5),
                            sliderInput("max_wetlandacre", p("What is the maximum wetland acreage on the fields in the landscape?:",
                                                             style ="color: black"), value = 37, min = 10.5, max = 100, step = 10),
                            sliderInput("min_num_wetland", p("What is the minimum number of wetlands on the fields in the landscape?", style ="color: black"), 
                                        value = 0, min = 0, max = 10, step = 0.5),
                            sliderInput("max_num_wetland", p("What is the maximum number of wetlands on the fields in the landscape?:",
                                                             style ="color: black"), value = 4, min = 10.5, max = 100, step = 10),
                            sliderInput("delayedseeding1", p("How many years out of 10 do you experience delayed seeding on drained wetland areas?", 
                                                             style ="color: black"), min = 0, max = 10, value = 0, step = 1),
                            selectInput("uplandyd_greater1", p("Is upland crop yield greater than yield on drained wetland?", 
                                                               style ="color: black"), choices = c("greater","equal", "lesser"), selected = "equal"),
                            sliderInput("ydiff1", "What is the % difference in upland crop yield and drained wetland crop yield", min = 0, max = 50, value = 0, step = 10),
                            
                            sliderInput("prop_margin", p("Proportion of wetland areas in the landscape that interferes with the operationn of farm machinery:", 
                                                         style ="color: black"), min = 0.01, max = 1, value = 0.6, step = 0.2),
                            sliderInput("min_drainage_cost", p("What is the the minimum annualized cost of draining an acre of wetland on your fields in the landscape?", 
                                                               style ="color: black"), value = 200, min = 0, max = 599, step = 50),
                            sliderInput("max_drainage_cost", p("What is the the maximum annualized cost of draining an acre of wetland on your fields in the landscape?:",
                                                               style ="color: black"), value = 600, min = 10, max = 3000, step = 100)
                          ),
                          hr(),
                          br(),
                          tags$h4(p("2. Crop production variables.", style = "color:black")),                                     
                          tags$h4(tags$em("Variables: Soil zone, crop price, crop yield, production cost, cultivated crops")),
                          dropdownButton(
                            selectInput("soilzone1", p("Select Soil Zone?", style ="color: black"), choices = c("black", "brown", "darkbrown"), selected = c("black"),
                                        multiple = TRUE),
                            br(),
                            selectInput("Cultivated_crops1", p("Select the crops you cultivate on your field?", style ="color: black"), 
                                        choices = c("Feed Barley", "Malt Barley", "Yellow Peas", "Soybean", "Canola", "Corn", "Flax", "Spring Wheat", "Oats", "Fallow"),
                                        selected = c("Canola", "Spring Wheat"),
                                        multiple = TRUE),
                            br(),
                            tags$h4(tags$em("Edit selected table below with your own data.")),
                            modDtUi("editable_fl2")
                          ),
                          hr(),
                          br(),
                          tags$h4(p("3. Discounting Information", style = "color:black")),  
                          tags$h4(tags$em("Variables: Discount factor, planning horizon")),
                          dropdownButton(
                            numericInput("planning_horizon1", p("How many years do you plan to operate your farm?", style ="color: black"), 
                                         value = 50, min = 2, max = 200, step = 1),
                            numericInput("discount_rate1", p("Select discount rate (%)?", style ="color: black"), value = 7, min = 1, max = 100, step = 1)
                          ),
                          hr(),
                          br(),
                          tags$h4(p("4. Policy Questions.", style = "color:black")),   
                          tags$h4(tags$em("Variables: Conservation budget, percentage of highest quality fields to conserve wetlands")),
                          dropdownButton(
                            numericInput("conservation_budget", p("Wetland conservation budget to conserve wetland areas in the landscape?",
                                                                  style ="color: black"), value = 100000, min = 1000, max = 1000000000, step = 1000),
                            numericInput("policy", p("Pencentage of highest quality fields to conserve wetlands?",
                                                     style ="color: black"), value = 25, min = 1, max = 100, step = 5)
                          )
                   )),
                 mainPanel(
                   column(11, 
                          #DT::dataTableOutput("wetland_summary1"),
                          tags$h4(tags$b(p("Distribution of annual net returns of fields with intact and fields with drained wetland areas", style = "color:green"))),
                          plotlyOutput("box_plot1", height = '600px', width = 'auto'),
                          tags$h4(tags$b(p("Probability density plot of annual net returns of the difference between fields with intact and fields with drained wetland areas", style = "color:green"))),
                          plotlyOutput("probability_plot1", height = '600px', width = 'auto'),
                          tags$h4(p("Wetland acre supply curve.", style = "color:green")),
                          plotlyOutput("wl_supplycurve", height = '600px', width = 'auto'),
                          br(),
                          br(),
                          br(),
                          tags$h4(p("Policy application of table.", style = "color:green")),
                          DT::dataTableOutput("conservation"),
                          br(),
                          tags$h4(p(tags$b(tags$u("Notes", style = "color:black")))),
                          tags$h4(p("1. Wetland drainage supply curve shows the the cumulative acreage of wetland areas that could be cultivated given various levels of
                               net present benefit of wetland drainage.", style = "color:black")),
                          tags$h4(p("2. Conserve_wl denotes the total wetland acreage in the landscape that could be conserved with a wetland conservation budget.", style = "color:black")),
                          tags$h4(p("2. Conserve_wl_best_fields denotes the total cost ($/year) of conserving wetland areas in the highest quality fields in the landscape.", style = "color:black"))),
                   
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
  
  ioc <- read.csv("ioc.csv") # %>% select(-X)
  
  nc_df <- reactive({
    
    ioc_select <- ioc %>% 
      dplyr::filter(soil_zone %in% input$soilzone & crop %in% input$Cultivated_crops) %>%
      dplyr::summarise(input_cost = sum(input_cost),
                       mach_cost = sum(machinery_oc)) %>%
      dplyr::mutate(nuisance_factor = case_when(
        (input$num_wetland >= 1 & input$num_wetland <=3) ~ 0.08,
        (input$num_wetland >  3 & input$num_wetland <=6) ~ 0.09,
        (input$num_wetland >= 6 & input$num_wetland <=9) ~ 0.11,
        input$num_wetland > 9 ~ 0.14
      ),
      input_waste_factor = 0.1,
      nuisance_cost_mach = nuisance_factor * mach_cost,
      nuisance_cost_input = input_waste_factor * input_cost,
      Efficiency_loss = round(nuisance_cost_mach + nuisance_cost_input,0)) %>% select(Efficiency_loss)
    
    return(ioc_select)
    
  })
  # Creating the main table that will be used for the main analysis from user applied information
  
  df_farmacre_wl <- reactive({
    
    nc <- nc_df()[1,] #%>% filter(implement_width == input$implement_width) %>% select(input$num_wetland)
    
    editable_fl()()%>%                                                             # the editable_fl()() is a reactive table that is the product of user-selected crop production information
      dplyr::mutate(prod_cost = other_variable_cost + fixed_cost + (fertilizer_nitrogen+phosphorus+sulphur_others+herbicides+insecticides+fungicides),  # total production cost
                    wetland_interference = as.numeric(ifelse(input$wetland_interference == "No", 0,1)),   #binary variables which is 1 if wetlands interfere with farming operations and 0 otherwise
                    number_wetlands = input$num_wetland,                                                  
                    wetland_acre = as.numeric(input$wetlandacre),                                          
                    loss = as.numeric(ifelse(wetland_acre==0 |input$num_wetland==0, 0, nc*0.5)),
                    nuisance_cost = as.numeric(ifelse(wetland_interference==0,  0, loss)),
                    num_crops = length(input$Cultivated_crops),
                    proportion_cultivated = 1/num_crops,                             # proportion cultivated apportions a proportion of each crop to an area of field or wetland area
                    farm_acre = 160 - wetland_acre,                                  # land available for cultivation on the field with drained wetland areas                            
                    farm_acre_wl = 160,                               
                    delayed_seeding = 1 - as.numeric(input$delayedseeding/10),       # delayed seeding: if it is 0 out of 100 then the full yield/acre ((1-0)*yield) will apply on the wetland area
                    discount_rate = as.numeric(input$discount_rate/100),             # discount factor for net present estimate
                    planning_horizon = as.numeric(input$planning_horizon),           # planning horizon for net present estimate
                    dr_cost = as.numeric(input$drainage_cost),                       # drainage cost
                    simulations = as.numeric(input$number_simulation),               # the number of times to estimate annual net returns
                    yieldsimulation1 = as.numeric(ifelse(input$yd_var == "Yes",1,0)),
                    pricesimulation1 = as.numeric(ifelse(input$cp_var == "Yes",1,0)),
                    drainagecostsimulation1 = as.numeric(ifelse(input$dc_var == "Yes", 1,0)),
                    productioncostsimulation1 = as.numeric(ifelse(input$pc_var == "Yes", 1,0)),
                    upland_wtland_yd = case_when(                                 # This is where the yield differences between upland area and drained area is defined
                      input$uplandyd_greater == "greater" ~ 1,
                      input$uplandyd_greater == "equal" ~ 2,
                      input$uplandyd_greater == "lesser" ~ 3
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
    
    sum <- df_farmacre_wl() %>% dplyr::select(number_wetlands, wetland_acre) 
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
    
    wldf <- wlfl_df() %>% dplyr::mutate(npv = as.integer(npv_wl)) %>% dplyr::select(npv)
    
    ggplotly(ggplot(wldf, aes(npv)) + 
               geom_density(alpha=.2, fill="#FF6666") +
               labs(
                 x ="Annual Net Returns ($/Acre/Yr) ", y="Probability") + 
               theme_bw() +
               theme(text = element_text(size=14, colour="dark green")))
  })
  
  
  ######################################### Landscape analysis section
  #_________ Crop production guide information and display
  
  fieldData <- read_csv("landsape_cropdata11.csv")                                   # Read the crop production information guide data
  
  editable_fl2 <- reactive({                                                        # the reactive function allows the creation of a custom crop production data based on user-defined soil zone and crops
    fieldData_fl <- fieldData %>% 
      dplyr::filter(soil_zone %in% input$soilzone1 & crop %in% input$Cultivated_crops1)
    callModule(modDt,"editable_fl2", data = fieldData_fl)
  }) 
  
  #________Nuisance cost calculation
  
  
  source("function_npv_landscape.R")
  source("function_num_fields.R")  #it sources the function that is used to create proportions of number of crops that will be cultivated
  #Its arguments are number of intended crops to cultivated and intended number of fields in the landscape
  source("nuisance_cost.R")
  source("totalcost.R")
  
  #________Nuisance cost calculation
  
  ioc <- read.csv("ioc.csv") #%>% dplyr::select(-X)                                         # Read data the has information on input cost (seed, seed treatment, fertilizer use, and pesticide use)
  # and machinery operating cost (machinery fuel and repairs)
  
  nc_df1 <- reactive({
    
    nc <-  ioc %>% dplyr::filter(soil_zone %in% input$soilzone1 & crop %in% input$Cultivated_crops1)
    
    num_row <- nrow(nc)
    
    nc1 <- nc %>% 
      dplyr::mutate(
        wetland_acre = round(runif(num_row, input$min_wetlandacre, input$max_wetlandacre), 2),
        num_wl = as.integer(round(runif(num_row, input$min_num_wetland, input$max_num_wetland), 2)),
        wetland_interference = sample(0:1, num_row, replace=T, prob=c(input$prop_margin, 1-input$prop_margin)),
        iwf = 0.1,
        nuisance = as.numeric(round(unlist(pmap_dbl(list(num_wl, iwf),nuisance_cost)),2)),
        nuisance2 = round(nuisance * wetland_interference * (0.5*wetland_acre), 2)
      ) %>% dplyr::select(nuisance2)
    
    return(nc1)
    
  })
  
  df_landscape <- reactive({
    
    set.seed(1)
    
    ioc <- read.csv("ioc.csv") #%>% dplyr::select(-X)                                         # Read data the has information on input cost (seed, seed treatment, fertilizer use, and pesticide use)
    
    #nc <- nc_df1() %>% pluck(1) #%>% filter(implement_width == input$implement_width) %>% select(input$num_wetland)
    
    #nc = 10
    #this data comes from the selected data in the field analysis
    num_row1 <- nrow(editable_fl2()())
    
    A1 <-  ioc %>% dplyr::filter(soil_zone %in% input$soilzone1 & crop %in% input$Cultivated_crops1) 
    num_row1 <- nrow(A1)
    
    A2 <- A1 %>% dplyr::mutate(
      num_wl = as.integer(round(runif(num_row1, input$min_num_wetland, input$max_num_wetland), 2)),
      iwf = 0.1
    ) 
    
    A3 <- nuisance_cost(df=A2) 
    
    A4 <- cbind(editable_fl2()(), A3)
    
    A <- A4 %>% 
      dplyr::mutate(prod_cost = other_variable_cost + fixed_cost + (fertilizer_nitrogen+phosphorus+sulphur_others+herbicides+insecticides+fungicides),
                    num_crops = length(input$Cultivated_crops1),
                    proportion = 1/num_crops,
                    #planning_horizon = as.numeric(input$planning_horizon1),           # planning horizon for net present estimate
                    #discount_rate = as.numeric(input$discount_rate1/100),             # discount factor for net present estimate
                    delayed_seeding = 1 - as.numeric(input$delayedseeding1/10),       # delayed seeding: if it is 0 out of 100 then the full yield/acre ((1-0)*yield) will apply on the wetland area
                    wetland_acre = round(runif(num_row1, input$min_wetlandacre, input$max_wetlandacre), 2),
                    num_wetlands = as.integer(round(runif(num_row1, input$min_num_wetland, input$max_num_wetland), 2)),
                    wetland_interference = sample(0:1, num_row1, replace=T, prob=c(input$prop_margin, input$prop_margin))
      ) 
    # Using the landscape_df() function to create the proportion of fields in the landscape that will be used for cultivating a specific crop
    B <- landscape_df(df = A, input$num_fields)
    
    #Creating other variables that will be needed in the annual net present value functions
    num_row <- nrow(B)
    set.seed = 100
    C <- B %>% dplyr::mutate(
      
      upland_wtland_yd = case_when(                                 # This is where the yield differences between upland area and drained area is defined
        input$uplandyd_greater1 == "greater" ~ 1,
        input$uplandyd_greater1 == "equal" ~ 2,
        input$uplandyd_greater1 == "lesser" ~ 3
      ),
      yd_diff = case_when(                                          # yield difference factor based on upland_wtland_yd. This will be used to adjust drained wetland areas crop yield
        upland_wtland_yd == 1  ~ as.numeric(-1*(input$ydiff1*avg_yield)/100),
        upland_wtland_yd == 3  ~ as.numeric((input$ydiff1*avg_yield)/100),
        upland_wtland_yd == 2  ~ as.numeric(0)
      ),
      yd_diff = ifelse(wetland_acre==0, 0, yd_diff),                # we specify that yield difference factor must be equal to 0 if there are no wetlands on the field
      yield_wl = yd_diff +  yield,
      
      production_cost = case_when(
        crop == "Canola" ~ round(runif(1, totalcost(df = fieldData, soil = input$soilzone1, croptype = "Canola")*0.7,
                                       totalcost(df = fieldData, soil = input$soilzone1, croptype = "Canola")*1.3), 2),  
        crop == "Feed Barley" ~ round(runif(1, totalcost(df = fieldData, soil = input$soilzone1, croptype = "Feed Barley")*0.7,
                                            totalcost(df = fieldData, soil = input$soilzone1, croptype = "Feed Barley")*1.3), 2),
        crop == "Malt Barley" ~ round(runif(1, totalcost(df = fieldData, soil = input$soilzone1, croptype = "Malt Barley")*0.7,
                                            totalcost(df = fieldData, soil = input$soilzone1, croptype = "Malt Barley")*1.3), 2),
        crop == "Yellow Peas" ~ round(runif(1, totalcost(df = fieldData, soil = input$soilzone1, croptype = "Yellow Peas")*0.7,
                                            totalcost(df = fieldData, soil = input$soilzone1, croptype = "Yellow Peas")*1.3), 2),
        crop == "Soybean" ~ round(runif(1, totalcost(df = fieldData, soil = input$soilzone1, croptype = "Soybean")*0.7,
                                        totalcost(df = fieldData, soil = input$soilzone1, croptype = "Soybean")*1.3), 2),
        crop == "Corn" ~ round(runif(1, totalcost(df = fieldData, soil = input$soilzone1, croptype = "Corn")*0.7,
                                     totalcost(df = fieldData, soil = input$soilzone1, croptype = "Corn")*1.3), 2),
        crop == "Flax" ~ round(runif(1, totalcost(df = fieldData, soil = input$soilzone1, croptype = "Flax")*0.7,
                                     totalcost(df = fieldData, soil = input$soilzone1, croptype = "Flax")*1.3), 2),
        crop == "Spring Wheat" ~ round(runif(1, totalcost(df = fieldData, soil = input$soilzone1, croptype = "Spring Wheat")*0.7,
                                             totalcost(df = fieldData, soil = input$soilzone1, croptype = "Spring Wheat")*1.3), 2),
        crop == "Oats" ~ round(runif(1, totalcost(df = fieldData, soil = input$soilzone1, croptype = "Oats")*0.7,
                                     totalcost(df = fieldData, soil = input$soilzone1, croptype = "Oats")*1.3), 2),
        crop == "Fallow" ~ 0
      ),
      nowetland = as.numeric(ifelse(input$havewl == "No", 0, 1)),
      wetland_acre = as.numeric(ifelse(nowetland == 0, 0, round(runif(num_row, input$min_wetlandacre, input$max_wetlandacre), 2))),
      wetland_interference = sample(0:1, num_row, replace=T, prob=c(input$prop_margin, input$prop_margin)),
      nuisance_cost = Efficiency_loss * wetland_interference * (0.5*wetland_acre),
      farm_acre = 160,
      farm_acre_wl = 160 - wetland_acre,
      drainage_cost = ifelse(nowetland == 0, 0, round(runif(num_row, input$min_drainage_cost, input$max_drainage_cost), 2)),
      
      # The NPB_prop_fl is used to estimate the annual net returns (with intact wetland areas) and also estimate the uncertainties around for certain variables if needed              
      npv_fl = as.numeric(round(unlist(pmap_dbl(list(crop_price, yield,production_cost, nuisance_cost, farm_acre, farm_acre_wl, 
                                                     discount_rate=input$discount_rate1/100, planning_horizon=input$planning_horizon1),NPB_prop_fl_ls)),2)),
      
      # The NPB_prop_flwl is used to estimate the annual net returns (with drained wetland areas) and also estimate the uncertainties around for certain variables if needed              
      npv_flwl = as.numeric(round(unlist(pmap_dbl(list(crop_price, yield,  yield_wl, production_cost, farm_acre, 
                                                       farm_acre_wl, wetland_acre, delayed_seeding, drainage_cost,  discount_rate=input$discount_rate1/100, planning_horizon=input$planning_horizon1),NPB_prop_flwl_ls)),2)),
      
      # We estimate the difference between the annual net returns of fields with drained wetland areas and fields with intact wetland areas. It is also wetland conservation cost            
      npv_wl = as.numeric(npv_flwl - npv_fl)
      
    ) %>% dplyr::select(crop, yield, crop_price, yd_diff, production_cost, drainage_cost, wetland_acre, wetland_interference, nuisance_cost, npv_fl, npv_flwl, npv_wl)
    
    return(C)
    
  })
  
  
  output$wetland_summary1 <- DT::renderDataTable({
    
    #sum <- df_farmacre_wl() %>% dplyr::select(number_wetlands, wetland_acre) #%>% rename(Number_wetlands = number_wetlands, Wetland_acreage = wetland_acre)
    DT::datatable(df_landscape())
  })
  
  output$wetland_summary2 <- DT::renderDataTable({
    
    #sum <- df_farmacre_wl() %>% dplyr::select(number_wetlands, wetland_acre) #%>% rename(Number_wetlands = number_wetlands, Wetland_acreage = wetland_acre)
    # DT::datatable(nc_df())
  })
  #____Annual net return box plot output
  
  output$box_plot1 <- renderPlotly({
    
    fldf <- df_landscape() %>% dplyr::mutate(npv = npv_fl, Label = "Field with intact wetland area") %>% dplyr::select(npv, Label)
    wlfldf <- df_landscape() %>% dplyr::mutate(npv = npv_flwl, Label = "Field with drained wetland area") %>% dplyr::select(npv, Label)
    
    df_all <- wlfldf %>% rbind(fldf)
    
    ggplotly(ggplot(df_all, aes(x=Label, y=npv, fill=Label)) + geom_boxplot()  +
               labs(
                 x ="", y="$/Acre/Year") +
               theme_bw() +
               theme(text = element_text(size=14, colour="dark green")) +
               theme(legend.position="none"))
  })
  
  #____Probability density plot output
  
  output$probability_plot1 <- renderPlotly({
    
    # wldf <- df_farmacre_wl() %>% mutate(npv = npv_wl) %>% dplyr::select(npv)
    
    ggplotly(ggplot(df_landscape(), aes(x=npv_wl)) + 
               geom_density(alpha=.2, fill="#FF6666") +
               labs(
                 x ="Annual Net Returns ($/Acre/Yr) ", y="Probability") + 
               theme_bw() +
               theme(text = element_text(size=14, colour="dark green")))
  })
  
  # __________________________________________ Landscape_____________________________
  
  output$wl_supplycurve <- renderPlotly({
    
    wldf <- df_landscape() %>% dplyr::select(npv_wl, wetland_acre)
    
    df_ss <- wldf %>% 
      dplyr::arrange(npv_wl) %>% 
      dplyr::mutate(wl_cumsum = round(cumsum(wetland_acre),0)) %>% dplyr::select(wetland_acre, wl_cumsum, npv_wl)
    
    ci <- data.frame(CI(df_ss$npv_wl, ci = 0.95))
    ci_upper <- ci["upper", ]
    ci_mean <- ci["mean", ]
    
    ci_gap = ci_upper - ci_mean 
    
    df_ss1 <- df_ss %>%
      dplyr::mutate(cilower = round(npv_wl - ci_gap,0),
                    ciupper = round(npv_wl + ci_gap,0)) 
    
    ggplotly(ggplot(data = df_ss1) +
               geom_line(aes(x = wl_cumsum, y = npv_wl), color = "dark green", size = 0.4) +
               geom_ribbon(aes(ymin= cilower, ymax= ciupper, x= wl_cumsum),fill="darkblue", linetype=2, alpha=0.1) +
               labs(#title = "Wetland Area Supply Curve",
                 x = "Total Wetland Area (Acres)",
                 y= "Annual Net Returns ($/Acre/Year)") +
               theme_bw() +
               theme(text = element_text(size=18, colour="dark green")))
  })
  
  policy_df <- reactive({
    
    df_landscape() %>% dplyr::select(npv_wl, wetland_acre, crop, yield) %>% dplyr::mutate(budget = input$conservation_budget, topn = input$policy/100, rownum = input$num_fields)
    
  })
  
  output$conservation <- DT::renderDataTable({
    
    #cost to preserve wetland areas in the top % of highest quality fields
    nrow_extract <- policy_df()[1, "topn"]
    rows <- policy_df()[1, "rownum"]
    
    ext_num <- as.integer((nrow_extract*rows))
    
    d <- policy_df() %>% dplyr::arrange(dplyr::desc(yield)) %>% top_n(ext_num,npv_wl) 
    #sum(d$npv_wl)
    
    #conservation budget
    budget1 <- policy_df()[, "budget"]
    d2 <- policy_df() %>% plyr::arrange(npv_wl) %>% dplyr::mutate(npv_sum = cumsum(npv_wl)) %>% dplyr::filter(npv_sum <= budget1)
    sum(d2$wetland_acre)
    
    #policy table
    policy <- data.frame(Conserve_wl =  as.integer(sum(d2$wetland_acre)), 
                         Conserve_wl_best_fields = as.integer(sum(d$npv)))
    
    DT::datatable(policy, rownames =FALSE, options = list(dom ="t"))
  })
  
  
}



shinyApp(ui = ui, server = server)