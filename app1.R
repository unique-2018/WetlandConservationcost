
############################## This is the code for the Wetland conservation cost Tool ###############################################
#https://rstudio.github.io/packrat/walkthrough.html
# The code has been structured into 3 main parts

#__________ Part A : libraries: All the R packages I will need to run this App. They will be installed in the cloud server
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
options(warn=-1)
#_______1. Thus script contains functions that are used to estimate the annual net returns of crop production and uncertainties around the estimates
source("functions_npv_field.R")    # : It is used to calculate the annualized net returns (ANR)of wetland drainage for the field
source("function_npv_landscape.R") #:This function is used to calculate ANR of drained wetlands in the landscape
source("function_landscape_df.R")  # It is used to generate the fields in the landscape as well as crop yield and price distribution.
source("nuisance_cost.R")          #: it is used to calculate the Efficiency loss of using farm machinery around wetlands 
source("totalcost.R")              #: It is used to calculate the total cost of production for a crop in a specific soil zone and province

#_______2. User Data Editing function: This function enables the user to edit crop production information table.
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
    
    navbarPage(                                            # The navbarPage function produces the multi-tab sections of the App
      
      title=div(h1("Wetland Conservation Tool")),
      
      tabPanel(h3("Tool Overview"),                         # This is the first section of the App: the overview section where provides summarized information on the App
         fluidRow(
            column(1), 
            column(10, htmltools::includeMarkdown("overview.Rmd")),    # IncludeMarkdown function allows us to use markdown to write the overview section
            column(1)
       )),
      
      tabPanel(h3("Field level Analysis"),    # This is the field analysis tab of the App
               fluidRow(
                 tags$div(
                   
                   # The sidebarPanel function contains the user- input widgets which are grouped into 5 main sections wrapped in the dropdownButton function.
                   sidebarPanel(                                                          
                     
                     tags$h4(p("You can alter the outputs of this tool but changing the levels of the variables which are grouped in the grey buttons below.
                            Please click on a grey button to reveal the variables!", style = "color:black")),
                     tags$a("Click here to get the User-guide for more information", href="userguide.pdf"),
                     
                     column(12,          # The input-widgets are defined in column orientation in the sidebarPanel  
                            fluidRow(    # The fluidRow functions creates a row orientation in the App
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
                              tags$h4(tags$em("Variables: Province, soil zone, crop price, crop yield, production cost, cultivated crops")),
                              
                              dropdownButton(
                                br(),
                                selectInput("province", p("Select Province?", style ="color: black"), choices = c("Alberta", "Manitoba", "Saskatchewan"), selected = c("Alberta"),
                                            multiple = FALSE),
                                br(),
                                selectInput("soilzone", p("Select Soil Zone?", style ="color: black"), choices = c("black", "brown", "darkbrown"), selected = c("black"),
                                            multiple = FALSE),
                                br(),
                                selectInput("Cultivated_crops", p("Select the crops you cultivate on your field?", style ="color: black"), 
                                            choices = c("Feed Barley", "Malt Barley", "Yellow Peas", "Canola","Spring Wheat", "Fallow"),
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
                              tags$h4(p("4.  Uncertainty (where variables vary around the mean by +/- 30% of the mean).", style = "color:black")),
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
                              br(),
                              tags$h4(p("5. Post Process Analysis.", style = "color:black")),
                              tags$h4(tags$em("Analyse changes in App outputs (landscape analysis) for percentage changes in crop yields, crop prices, production cost and drainage cost.")),
                              dropdownButton(
                                sliderInput("yd_percenchange1", p("What is expected percentage change in crop yields?", style ="color: black"), 
                                            value = 0, min = -100, max = 100, step = 25),
                                sliderInput("cp_percenchange1", p("What is expected percentage change in crop prices?", style ="color: black"), 
                                            value = 0, min = -100, max = 100, step = 25),
                                sliderInput("pc_percenchange1", p("What is expected percentage change in production cost?", style ="color: black"), 
                                            value = 0, min = -100, max = 100, step = 25),
                                sliderInput("dc_percenchange1", p("What is expected percentage change in drainage cost?", style ="color: black"), 
                                            value = 0, min = -100, max = 100, step = 25)
                                
                              ),
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
      
      tabPanel(h3("Landscape level Analysis"),          # The landscape tab of the app
               
               fluidRow(
                 
                 sidebarPanel(
                   
                   tags$h4(p("You can alter the outputs of this tool but changing the levels of the variables which are grouped in the grey buttons below.
                            Please click on a grey button to reveal the variables!", style = "color:black")),
                   # tags$a("Click here to get the User-guide for more information", href="userguide.pdf"),

                   column(12,
                          br(),
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
                          tags$h4(tags$em("Variables: Province, soil zone, crop price, crop yield, production cost, cultivated crops")),
                          dropdownButton(
                            selectInput("province1", p("Select Province?", style ="color: black"), choices = c("Alberta", "Manitoba", "Saskatchewan"), selected = c("Alberta"),
                                        multiple = FALSE),
                            br(),
                            selectInput("soilzone1", p("Select Soil Zone?", style ="color: black"), choices = c("black", "brown", "darkbrown"), selected = c("black"),
                                        multiple = FALSE),
                            br(),
                            selectInput("Cultivated_crops1", p("Select the crops you cultivate on your field?", style ="color: black"), 
                                        choices = c("Feed Barley", "Malt Barley", "Yellow Peas", "Canola","Spring Wheat", "Fallow"),
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
                          tags$h4(p(tags$b(tags$u("4. Post Process Analysis.", style = "color:black")))),
                          
                          tags$h4(tags$em("Estimates of wetland conservaton cost for the following scenarios:
                                          a) most valuable wetlands, b) valuable wetlands, c) less valuable wetlands,
                                          d) least valuable wetlands e) Wetlands in top 50% of fertile fields in the landscape and 
                                          f) Wetlands in top 50% of less fertile fields", style = "color:black")),
                          dropdownButton(    #The dropdownButton function enables me to group similar input widgets together
                            sliderInput("prop_A", p("Proportion of fields containing most valuable wetlands:", 
                                                    style ="color: black"), min = 0, max = 1, value = 0.6, step = 0.2),
                            sliderInput("prop_B", p("Proportion of fields containing valuable wetlands:", 
                                                    style ="color: black"), min = 0, max = 1, value = 0.6, step = 0.2),
                            sliderInput("prop_C", p("Proportion of fields containing valuable wetlands:", 
                                                     style ="color: black"), min = 0, max = 1, value = 0.6, step = 0.2),
                            sliderInput("prop_D", p("Proportion of fields containing least valuable wetlands:", 
                                                     style ="color: black"), min = 0, max = 1, value = 0.6, step = 0.2),
                            sliderInput("prop_fl", p("Proportion of fertile fields in the landscape:", 
                                                     style ="color: black"), min = 0, max = 1, value = 0.6, step = 0.2),
                            sliderInput("prop_lfl", p("Proportion of least fertile in the landscape:", 
                                                     style ="color: black"), min = 0, max = 1, value = 0.6, step = 0.2)
                          ),
                          hr(),
                          br(),
                          tags$h4(tags$em("Analyse changes in App outputs (landscape analysis) for changes in main variables..", style = "color:black")),
                          dropdownButton(
                            sliderInput("yd_percenchange", p("What is expected percentage change in crop yields?", style ="color: black"), 
                                        value = 0, min = -100, max = 100, step = 25),
                            sliderInput("cp_percenchange", p("What is expected percentage change in crop prices?", style ="color: black"), 
                                        value = 0, min = -100, max = 100, step = 25),
                            sliderInput("pc_percenchange", p("What is expected percentage change in production cost?", style ="color: black"), 
                                        value = 0, min = -100, max = 100, step = 25),
                            sliderInput("dc_percenchange", p("What is expected percentage change in drainage cost?", style ="color: black"), 
                                        value = 0, min = -100, max = 100, step = 25)
                          ),
                          hr(),
                          br(),
                          tags$h4(p("5.  Download Landscape-level Simulated Data.", style = "color:black")),
                          dropdownButton(
                            tags$h3("List of Input"),
                            
                            # Button
                            downloadButton("downloadData1", "Download Simulated Table as CSV")
                          )
                   )),
                 mainPanel(
                   column(11, 
                          #gt_output('table'),
                          tags$h4(tags$b(p("Distribution of annual net returns of fields with intact and fields with drained wetland areas", style = "color:green"))),
                          plotlyOutput("box_plot1", height = '600px', width = 'auto'),
                          tags$h4(tags$b(p("Probability density plot of annual net returns of the difference between fields with intact and fields with drained wetland areas", style = "color:green"))),
                          plotlyOutput("probability_plot1", height = '600px', width = 'auto'),
                          tags$h4(p("Wetland acre supply curve.", style = "color:green")),
                          plotlyOutput("wl_supplycurve", height = '600px', width = 'auto'),
                          tags$h5(p(tags$b(tags$u("Note", style = "color:black")))),
                          tags$h5(p("Wetland drainage supply curve shows the the cumulative acreage of wetland areas that could be cultivated given various levels of
                               net present benefit of wetland drainage.", style = "color:black")),
                          br(),
                          br(),
                          br(),
                          tags$h4(p("Economic Cost of Alberta's Wetland Policy.", style = "color:green")),
                          DT::dataTableOutput("Wetland_policy_alberta"),
                          tags$h5(p(tags$b(tags$u("Variable definitions", style = "color:black")))),
                          tags$h5(p("Totol_WA : Total Wetland Area (Acres); Percentage_PWCC : Percentage of fields with positive wetland conservation cost;
                          Mean_WCC : Mean wetland conservation cost ($); Standard Deviation: Standard deviation of wetland conservation cost ($)", style = "color:black"))),
                          
                   column(1)
                 )))
      
    )
  )

#___________ Part D: The back-end phase of the App: where all the computations are done_________________________________________________________________


server <- function(input, output) {                                                   
  
  #_________ Crop production guide information and display
  
  fieldData <- read_csv("landsape_cropdata11.csv")    # Read the crop production information guide data
  
  editable_fl <- reactive({                           # the reactive function allows the creation of a custom crop production data based on user-defined soil zone and crops
    
    fieldData_fl <- fieldData %>% 
      dplyr::filter(soil_zone %in% input$soilzone & crop %in% input$Cultivated_crops & province %in% input$province)
    
    callModule(modDt,"editable_fl", data = fieldData_fl)
  }) 
  
  ioc <- read.csv("ioc.csv") # The IOC data contains information on input cost and farm machinery cost(fuel and repairs)
  
  nc_df <- reactive({  # This is estimating the efficiency loss of wetlands on fields; it will be reactive based on
                       # the user-defined information. The calculation is based on Cortus et al. (2011 Table 3). 
    
    ioc_select <- ioc %>% 
      dplyr::filter(soil_zone %in% input$soilzone & crop %in% input$Cultivated_crops & province %in% input$province) %>%
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
      Efficiency_loss = round(nuisance_cost_mach + nuisance_cost_input,0)) %>% dplyr::select(Efficiency_loss)
    
    return(ioc_select)
    
  })
  
  # Creating the main table that will be used for the main analysis from user applied information
  df_farmacre_wl <- reactive({  # The data is created as a reactive data frame. it will change based on the values users select
    
    nc <- nc_df()[1,] #%>% filter(implement_width == input$implement_width) %>% select(input$num_wetland)
    
    editable_fl()()%>%                                                             # the editable_fl()() is a reactive table that is the product of user-selected crop production information
      dplyr::mutate(prod_cost = other_variable_cost + fixed_cost + fertilizer_cost + herb_insect_fung,  # total production cost
                    avg_yield = (100+input$yd_percenchange1)/100*avg_yield, # This is for the post-processing part, where we can calculate how our results will change when we 
                                                                            # change the main variables, in this case yield by a certain %. When the change is 0, we remain at the pre-change state (or original state)
                    avg_crop_price = (100+input$cp_percenchange1)/100*avg_crop_price,
                    prod_cost = (100+input$pc_percenchange1)/100*prod_cost,
                    wetland_interference = as.numeric(ifelse(input$wetland_interference == "No", 0,1)),   #binary variables which is 1 if wetlands interfere with farming operations and 0 otherwise
                    number_wetlands = input$num_wetland,                                                  
                    wetland_acre = as.numeric(input$wetlandacre),                                          
                    loss = as.numeric(ifelse(wetland_acre==0 |input$num_wetland==0, 0, nc*0.5)), # I am saying efficiency loss must be 0 if there are no wetlands or number of wetlands is 0.
                    nuisance_cost = as.numeric(ifelse(wetland_interference==0,  0, loss)),  # Also the efficiency loss must be zero if wetlands are at the margins of the field
                    num_crops = length(input$Cultivated_crops),   
                    proportion_cultivated = 1/num_crops,                             # proportion cultivated apportions a proportion of each crop to an area of field or wetland area
                    farm_acre = 160 - wetland_acre,                                  # land available for cultivation on the field with drained wetland areas                            
                    farm_acre_wl = 160,                               
                    delayed_seeding = 1 - as.numeric(input$delayedseeding/10),       # delayed seeding: if it is 0 out of 100 then the full yield/acre ((1-0)*yield) will apply on the wetland area
                    discount_rate = as.numeric(input$discount_rate/100),             # discount factor for net present estimate
                    planning_horizon = as.numeric(input$planning_horizon),           # planning horizon for net present estimate
                    dr_cost = as.numeric(input$drainage_cost),                       # drainage cost
                    dr_cost = (100+input$dc_percenchange1)/100*dr_cost,
                    simulations = as.numeric(input$number_simulation),               # the number of times to estimate annual net returns
                    yieldsimulation1 = as.numeric(ifelse(input$yd_var == "Yes",1,0)), # yieldsimulation turns allows us to add uncertainty to our results; the same logic applies to the other variables below
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
                 
                    #percenchange = ifelse(direction_change ==1, (100+input$percenchange1)/100, (100-input$percenchange1)/100),
                    
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
    npv_1 <- simulated_npv(df=df_flwl) #The simulated_npv function is store in the functions_npv_field script
    
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
  
  
  # _________________________________________________ Landscape analysis section __________________________________________
  #_________ Crop production guide information and display
  
  fieldData <- read_csv("landsape_cropdata11.csv")   # Read the crop production information guide data
  
  editable_fl2 <- reactive({                         # the reactive function allows the creation of a custom crop production data based on user-defined soil zone and crops
    fieldData_fl <- fieldData %>% 
      dplyr::filter(soil_zone %in% input$soilzone1 & crop %in% input$Cultivated_crops1 & province %in% input$province1)
    callModule(modDt,"editable_fl2", data = fieldData_fl)
  }) 
  
  #________Nuisance cost calculation

  ioc <- read.csv("ioc.csv") #%>% dplyr::select(-X)                                         # Read data the has information on input cost (seed, seed treatment, fertilizer use, and pesticide use)
  # and machinery operating cost (machinery fuel and repairs)
  
  nc_df1 <- reactive({
    
    nc <-  ioc %>% dplyr::filter(soil_zone %in% input$soilzone1 & crop %in% input$Cultivated_crops1 & province %in% input$province1)
    
    num_row <- nrow(nc)
    
    nc1 <- nc %>% 
      dplyr::mutate(
        wetland_acre = round(runif(num_row, input$min_wetlandacre, input$max_wetlandacre), 2),
        num_wl = as.integer(round(runif(num_row, input$min_num_wetland, input$max_num_wetland), 2)),
        wetland_interference = sample(1:0, num_row, replace=T, prob=c(input$prop_margin, 1-input$prop_margin)),
        iwf = 0.1,
        nuisance = as.numeric(round(unlist(pmap_dbl(list(num_wl, iwf),nuisance_cost)),2)),
        nuisance2 = round(nuisance * wetland_interference * (0.5*wetland_acre), 2)
      ) %>% dplyr::select(nuisance2)
    
    return(nc1)
    
  })
  
  df_landscape <- reactive({
    
    set.seed(1)
    
    ioc <- read.csv("ioc.csv") #%>% dplyr::select(-X)     # Read data the has information on input cost (seed, seed treatment, fertilizer use, and pesticide use)

        #this data comes from the selected data in the field analysis
    num_row1 <- nrow(editable_fl2()())
    
    A1 <-  ioc %>% dplyr::filter(soil_zone %in% input$soilzone1 & crop %in% input$Cultivated_crops1 & province %in% input$province1) 
    num_row1 <- nrow(A1)
    
    A2 <- A1 %>% dplyr::mutate(
      num_wl = as.integer(round(runif(num_row1, input$min_num_wetland, input$max_num_wetland), 2)),
      iwf = 0.1
    ) 
    
    A3 <- nuisance_cost(df=A2) 
    
    A4 <- cbind(editable_fl2()(), A3)
    
    A <- A4 %>% 
      dplyr::mutate(prod_cost = other_variable_cost + fixed_cost +  fertilizer_cost + herb_insect_fung,
                    num_crops = length(input$Cultivated_crops1),
                    proportion = 1/num_crops,
                    delayed_seeding = 1 - as.numeric(input$delayedseeding1/10),       # delayed seeding: if it is 0 out of 100 then the full yield/acre ((1-0)*yield) will apply on the wetland area
                    wetland_acre = round(runif(num_row1, input$min_wetlandacre, input$max_wetlandacre), 2),
                    num_wetlands = as.integer(round(runif(num_row1, input$min_num_wetland, input$max_num_wetland), 2)),
                    wetland_interference = sample(0:1, num_row1, replace=T, prob=c(input$prop_margin, (1-input$prop_margin))) #this creates the proportion of wetlands at the margins of the fields in the landscape
      ) 
    # Using the landscape_df() function to create the proportion of fields in the landscape that will be used for cultivating a specific crop
    source("function_landscape_df.R")  # It is used to generate the fields in the landscape as well as crop yield and price distribution.
    B <- landscape_df(df = A, input$num_fields)
    
    #Creating other variables that will be needed in the annual net present value functions
    num_row <- nrow(B)
    set.seed = 100
    C <- B %>% dplyr::mutate(
      yield = (100+input$yd_percenchange)/100*yield,
      crop_price = (100+input$cp_percenchange)/100*crop_price,
      upland_wtland_yd = case_when(  #This is where the yield differences between upland area and drained area is defined
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
        crop == "Canola" ~ round(runif(1, totalcost(df = fieldData, soil = input$soilzone1, croptype = "Canola", province = input$province1)*0.7,
                                       totalcost(df = fieldData, soil = input$soilzone1, croptype = "Canola", province = input$province1)*1.3), 2),  
        crop == "Feed Barley" ~ round(runif(1, totalcost(df = fieldData, soil = input$soilzone1, croptype = "Feed Barley", province = input$province1)*0.7,
                                            totalcost(df = fieldData, soil = input$soilzone1, croptype = "Feed Barley", province = input$province1)*1.3), 2),
        crop == "Malt Barley" ~ round(runif(1, totalcost(df = fieldData, soil = input$soilzone1, croptype = "Malt Barley", province = input$province1)*0.7,
                                            totalcost(df = fieldData, soil = input$soilzone1, croptype = "Malt Barley", province = input$province1)*1.3), 2),
        crop == "Yellow Peas" ~ round(runif(1, totalcost(df = fieldData, soil = input$soilzone1, croptype = "Yellow Peas", province = input$province1)*0.7,
                                            totalcost(df = fieldData, soil = input$soilzone1, croptype = "Yellow Peas", province = input$province1)*1.3), 2),
        crop == "Spring Wheat" ~ round(runif(1, totalcost(df = fieldData, soil = input$soilzone1, croptype = "Spring Wheat", province = input$province1)*0.7,
                                             totalcost(df = fieldData, soil = input$soilzone1, croptype = "Spring Wheat", province = input$province1)*1.3), 2),
        crop == "Fallow" ~ 0
      ),
      production_cost = (100+input$pc_percenchange)/100*production_cost,
      nowetland = as.numeric(ifelse(input$havewl == "No", 0, 1)),
      wetland_acre = as.numeric(ifelse(nowetland == 0, 0, round(runif(num_row, input$min_wetlandacre, input$max_wetlandacre), 2))),
      wetland_interference = sample(0:1, num_row, replace=T, prob=c(input$prop_margin, input$prop_margin)),
      nuisance_cost = Efficiency_loss * wetland_interference * (0.5*wetland_acre),
      farm_acre = 160,
      farm_acre_wl = 160 - wetland_acre,
      drainage_cost = ifelse(nowetland == 0, 0, round(runif(num_row, input$min_drainage_cost, input$max_drainage_cost), 2)),
      drainage_cost = (100+input$dc_percenchange)/100*drainage_cost,
      npv_fl = as.numeric(round(unlist(pmap_dbl(list(crop_price, yield,production_cost, nuisance_cost, farm_acre, farm_acre_wl, 
                                                     discount_rate=input$discount_rate1/100, planning_horizon=input$planning_horizon1),NPB_prop_fl_ls)),2)),
      npv_flwl = as.numeric(round(unlist(pmap_dbl(list(crop_price, yield,  yield_wl, production_cost, farm_acre, 
                                                       farm_acre_wl, wetland_acre, delayed_seeding, drainage_cost,  discount_rate=input$discount_rate1/100, planning_horizon=input$planning_horizon1),NPB_prop_flwl_ls)),2)),
      npv_wl = as.numeric(npv_flwl - npv_fl),
      class_a_wl = sample(1:0, num_row, replace=T, prob=c(input$prop_A, (1-input$prop_A))),
      class_b_wl = sample(1:0, num_row, replace=T, prob=c(input$prop_B, (1-input$prop_B))),
      class_c_wl = sample(1:0, num_row, replace=T, prob=c(input$prop_C, (1-input$prop_C))),
      class_d_wl = sample(1:0, num_row, replace=T, prob=c(input$prop_D, (1-input$prop_D)))
      
     ) 
    return(C)
    
  })
  
# Box plot of annualized net return estimates of fields with drained wetlands and fields with intact wetlands
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
  
  #____Probability density plot output of the difference in the estimates of fields with drained wetlands and fields with intact wetlands
  
  output$probability_plot1 <- renderPlotly({
    
    ggplotly(ggplot(df_landscape(), aes(x=npv_wl)) + 
               geom_density(alpha=.2, fill="#FF6666") +
               labs(
                 x ="Annual Net Returns ($/Acre/Yr) ", y="Probability") + 
               theme_bw() +
               theme(text = element_text(size=14, colour="dark green")))
  })
  
  # Wetland supply curve in the landscape
  
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
  
  # This is where we calculate the wetland conservation cost for different scenarios of wetland values in the landscape
  output$Wetland_policy_alberta <- DT::renderDataTable({
    
    #Class A Wetlands
    df_class_a <- df_landscape() %>% dplyr::filter(class_a_wl ==1) %>% dplyr::select(wetland_acre, npv_wl)
    total_wcc_a <- as.integer(sum(df_class_a$wetland_acre))
    mean__wcc_a <- as.integer(mean(df_class_a$npv_wl))
    sd__wcc_a <- as.integer(sd(df_class_a$npv_wl))
    pos_wcc_a <- as.integer(100*(nrow(df_class_a %>% dplyr::filter(npv_wl >=0))/nrow(df_class_a)))

    #Class B Wetlands
    df_class_b <- df_landscape() %>% dplyr::filter(class_b_wl ==1) %>% dplyr::select(wetland_acre, npv_wl)
    total_wcc_b <- as.integer(sum(df_class_b$wetland_acre))
    mean__wcc_b <- as.integer(mean(df_class_b$npv_wl))
    sd__wcc_b <- as.integer(sd(df_class_b$npv_wl))
    pos_wcc_b <- as.integer(100*(nrow(df_class_b %>% dplyr::filter(npv_wl >=0))/nrow(df_class_b)))
    
    
    #Class C Wetlands
    df_class_c <- df_landscape() %>% dplyr::filter(class_c_wl ==1) %>% dplyr::select(wetland_acre, npv_wl)
    total_wcc_c <- as.integer(sum(df_class_c$wetland_acre))
    mean__wcc_c <- as.integer(mean(df_class_c$npv_wl))
    sd__wcc_c <- as.integer(sd(df_class_c$npv_wl))
    pos_wcc_c <- as.integer(100*(nrow(df_class_c %>% dplyr::filter(npv_wl >=0))/nrow(df_class_c)))
    
    
    #Class D Wetlands
    df_class_d <- df_landscape() %>% dplyr::filter(class_d_wl ==1) %>% dplyr::select(wetland_acre, npv_wl)
    total_wcc_d <- as.integer(sum(df_class_d$wetland_acre))
    mean__wcc_d <- as.integer(mean(df_class_d$npv_wl))
    sd__wcc_d <- as.integer(sd(df_class_d$npv_wl))
    pos_wcc_d <- as.integer(100*(nrow(df_class_d %>% dplyr::filter(npv_wl >=0))/nrow(df_class_d)))
    
    
    final_df <- data.frame(
      Wetland_Class = c("Highest Valued Wetlands", " High Valued Wetlands", "Valued Wetlands", "Less Valued Wetlands"),
      Total_Wetland_Area = c(total_wcc_a, total_wcc_b, total_wcc_c, total_wcc_d),
      Percentage_PWCC = c(pos_wcc_a, pos_wcc_b, pos_wcc_c, pos_wcc_d),
      Mean_WCC = c(mean__wcc_a, mean__wcc_b, mean__wcc_c, mean__wcc_d),
      Standard_Deviation = c(sd__wcc_a, sd__wcc_b, sd__wcc_c, sd__wcc_d)
    )
    
    
    DT::datatable(final_df, rownames =FALSE, options = list(dom ="t"))
  })
  
  #______download Landscape-level simulated data
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste("simulated_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(df_landscape(), file, row.names = FALSE)
    }
  )
  
  
}



shinyApp(ui = ui, server = server)