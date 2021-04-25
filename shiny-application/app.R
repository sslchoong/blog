# Installing the required packages
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(olsrr)
library(MASS)
library(plm)
library(lmtest)
library(ggplot2)
library(ggstatsplot)
library(ExPanDaR)
library(corrplot)
library(DT)
library(kableExtra)
library(insight)
library(parameters)
library(ggpubr)
library(lubridate)
library(directlabels)
library(plotly)

# Loading the dataset
ghg_EU <- read_csv("data/GHGproj_data.csv")

# Data wrangling
ghg_df <- ghg_EU %>% 
    filter(!Country %in% c("European Union - 27 countries (from 2020)",                                           # remove regional levels
                           "European Union - 28 countries (2013-2020) and Iceland under the Kyoto Protocol", 
                           "European Union - 28 countries (2013-2020)")) %>% 
    mutate(Country = dplyr::recode(Country, "Germany (until 1990 former territory of the FRG)" = "Germany")) %>%  # recode Germany label
    rename("Greenhouse_Gases" = "GHG_emissions",                                                                  # rename variables
           "Carbon_Dioxide" = "CO2_emissions", 
           "Methane" = "CH4_emissions", 
           "Nitrous_Oxide" = "NO2_emissions", 
           "Carbon_Intensity" = "Carbon_intensity", 
           "Environmental_Taxes" = "Envt_taxes", 
           "Final_Energy_Intensity" = "Final_EI", 
           "Fuel_Mix" = "Fuel_mix", 
           "GDP" = "GDP", 
           "Heat_Pumps" = "Heat_pumps", 
           "Liquid_Biofuels" = "Liquid_biofuels", 
           "Renewable_Energy_Share" = "Renewables_share", 
           "Solar_Thermal" = "Solar_thermal"
    )

# Defining parameters
## Countries
countries <- c("Belgium", "Bulgaria", "Czechia", "Denmark", "Germany", "Estonia", "Ireland", "Greece", "Spain", "France", 
               "Croatia", "Italy", "Cyprus", "Latvia", "Lithuania", "Luxembourg", "Hungary", "Malta", "Netherlands", "Austria",                                                                        
               "Poland", "Portugal", "Romania", "Slovenia", "Slovakia", "Finland", "Sweden", "Iceland", "Liechtenstein", "Norway",                                                                         
               "Switzerland", "United Kingdom", "Turkey")


# Define UI for application that combine Shiny modules
ui <- fluidPage(theme = shinytheme("flatly"), 
                
                # ----- Navigation Bar ----- #
                navbarPage(title = "DEGGED", fluid = TRUE,
                           
                           # -------------------- Overview -------------------- #
                           tabPanel("Overview", icon = icon("info"), 
                                    fluidRow(
                                        column(5,
                                               mainPanel(img(src = "overview.png", width = 600))
                                               ), 
                                        column(7, 
                                               mainPanel(htmlOutput("shinydesc")))
                                    )), 
                           
                           # -------------------- Data -------------------- #
                           
                           tabPanel("Data Overview", icon = icon("database"), 
                                    sidebarLayout(
                                        sidebarPanel(
                                            # Selection of Year Range
                                            sliderInput(inputId = "eda_year", 
                                                        label = "Year of Analysis:", 
                                                        min = 2010, 
                                                        max = 2018, 
                                                        value = c(2010, 2018), 
                                                        sep = ""), 
                                            # Selection of Countries
                                            pickerInput(inputId = "eda_country", 
                                                        label = "Select Countries:", 
                                                        choices = countries, 
                                                        selected = unique(countries), 
                                                        options = list(`actions-box` = TRUE), 
                                                        multiple = TRUE), 
                                            actionButton("goButtonEDAdata", "Apply changes")
                                            ), 
                                        mainPanel(
                                            tabsetPanel(
                                                tabPanel("Data", br(), 
                                                         dataTableOutput("table")), 
                                                tabPanel("Missing Values", br(), 
                                                         fluidRow(
                                                             htmlOutput("missinglabel"), 
                                                             plotOutput("missingdat", height = "500px")))
                                        )))),
                           
                           # -------------------- EDA -------------------- #
                           navbarMenu("Exploratory Data Analysis", icon = icon("search"), 
                                      tabPanel("Descriptive Analysis", 
                                               sidebarLayout(
                                                   sidebarPanel(
                                                       sliderInput(inputId = "eda_year2", 
                                                                   label = "Year of Analysis:", 
                                                                   min = 2010, 
                                                                   max = 2018, 
                                                                   value = c(2010, 2018), 
                                                                   sep = ""), 
                                                       pickerInput(inputId = "eda_country2", 
                                                                   label = "Select Countries:", 
                                                                   choices = countries, 
                                                                   selected = unique(countries), 
                                                                   options = list(`actions-box` = TRUE), 
                                                                   multiple = TRUE), 
                                                       selectInput(inputId = "eda_variable2", 
                                                                   label = "Variables:", 
                                                                   choices = c("Greenhouse Gases" = "Greenhouse_Gases", 
                                                                               "Carbon Dioxide" = "Carbon_Dioxide", 
                                                                               "Methane" = "Methane", 
                                                                               "Nitrous Oxide" = "Nitrous_Oxide", 
                                                                               "Carbon Intensity" = "Carbon_Intensity", 
                                                                               "Environmental Taxes" = "Environmental_Taxes", 
                                                                               "Final Energy Intensity" = "Final_Energy_Intensity", 
                                                                               "Fuel Mix" = "Fuel_Mix", 
                                                                               "GDP" = "GDP", 
                                                                               "Heat Pumps" = "Heat_Pumps", 
                                                                               "Liquid Biofuels" = "Liquid_Biofuels", 
                                                                               "Renewable Energy Share" = "Renewable_Energy_Share", 
                                                                               "Solar Thermal" = "Solar_Thermal"), 
                                                                   selected = c("Greenhouse_Gases"), 
                                                                   multiple = TRUE, width = "100%"),
                                                       sliderInput(inputId = "nb_bins", 
                                                                   label = "(Histogram) No. of Bins:", 5, 30, 15), 
                                                       actionButton("goButtonEDAanalysis", "Apply changes")
                                                   ), 
                                               mainPanel(
                                                   tabsetPanel(
                                                       tabPanel("Summary Statistics", br(),
                                                                tableOutput("desc")), 
                                                       tabPanel("Histogram", br(),
                                                                fluidRow(
                                                                    htmlOutput("histolabel"), 
                                                                    plotlyOutput("histo", height = "600px")))
                                                   )
                                               )
                                               )),
                                      tabPanel("Time Trend", 
                                               sidebarLayout(
                                                   sidebarPanel(
                                                       sliderInput(inputId = "eda_year3", 
                                                                   label = "Year of Analysis:", 
                                                                   min = 2010, 
                                                                   max = 2018, 
                                                                   value = c(2010, 2018), 
                                                                   sep = "", ), 
                                                       pickerInput(inputId = "eda_country3", 
                                                                   label = "Select Countries:", 
                                                                   choices = countries, 
                                                                   selected = unique(countries), 
                                                                   options = list(`actions-box` = TRUE), 
                                                                   multiple = TRUE), 
                                                       selectInput(inputId = "eda_timetrend", 
                                                                   label = "Individual or Grouped:", 
                                                                   choices = c("Individual", "Grouped"), 
                                                                   selected = c("Individual"), width = "100%"),
                                                       conditionalPanel(condition = "input.eda_timetrend == 'Individual'", 
                                                                        selectInput("eda_variable3",
                                                                                    label = "Variable:", 
                                                                                    choices = c("Greenhouse Gases" = "Greenhouse_Gases", 
                                                                                                "Carbon Dioxide" = "Carbon_Dioxide", 
                                                                                                "Methane" = "Methane", 
                                                                                                "Nitrous Oxide" = "Nitrous_Oxide", 
                                                                                                "Carbon Intensity" = "Carbon_Intensity", 
                                                                                                "Environmental Taxes" = "Environmental_Taxes", 
                                                                                                "Final Energy Intensity" = "Final_Energy_Intensity", 
                                                                                                "Fuel Mix" = "Fuel_Mix", 
                                                                                                "GDP" = "GDP", 
                                                                                                "Heat Pumps" = "Heat_Pumps", 
                                                                                                "Liquid Biofuels" = "Liquid_Biofuels", 
                                                                                                "Renewable Energy Share" = "Renewable_Energy_Share", 
                                                                                                "Solar Thermal" = "Solar_Thermal"), 
                                                                                    selected = c("Greenhouse_Gases"), width = "100%")), 
                                                       actionButton("goButtonTimeT", "Apply changes")
                                                   ), 
                                                   mainPanel(
                                                       fluidRow(
                                                           htmlOutput("linelabel"), 
                                                           plotlyOutput("line", height = "600px"))
                                                   )
                                               )),
                                      tabPanel("Correlation Matrix", 
                                               sidebarLayout(
                                                   sidebarPanel(
                                                       selectInput(inputId = "eda_year4", 
                                                                   label = "Year of Analysis:", 
                                                                   choices = c("2010" = 2010,
                                                                               "2011" = 2011,
                                                                               "2012" = 2012,
                                                                               "2013" = 2013,
                                                                               "2014" = 2014,
                                                                               "2015" = 2015,
                                                                               "2016" = 2016,
                                                                               "2017" = 2017,
                                                                               "2018" = 2018),
                                                                   selected = 2010, width = "100%"), 
                                                       pickerInput(inputId = "eda_country4", 
                                                                   label = "Select Countries:", 
                                                                   choices = countries, 
                                                                   selected = unique(countries), 
                                                                   options = list(`actions-box` = TRUE), 
                                                                   multiple = TRUE), 
                                                       selectInput(inputId = "eda_variable4", 
                                                                   label = "Variables:", 
                                                                   choices = c("Greenhouse Gases" = "Greenhouse_Gases", 
                                                                               "Carbon Dioxide" = "Carbon_Dioxide", 
                                                                               "Methane" = "Methane", 
                                                                               "Nitrous Oxide" = "Nitrous_Oxide", 
                                                                               "Carbon Intensity" = "Carbon_Intensity", 
                                                                               "Environmental Taxes" = "Environmental_Taxes", 
                                                                               "Final Energy Intensity" = "Final_Energy_Intensity", 
                                                                               "Fuel Mix" = "Fuel_Mix", 
                                                                               "GDP" = "GDP", 
                                                                               "Heat Pumps" = "Heat_Pumps", 
                                                                               "Liquid Biofuels" = "Liquid_Biofuels", 
                                                                               "Renewable Energy Share" = "Renewable_Energy_Share", 
                                                                               "Solar Thermal" = "Solar_Thermal"), 
                                                                   selected = c("Greenhouse_Gases", "Carbon_Dioxide", "Methane",  
                                                                                "Nitrous_Oxide", "Carbon_Intensity", "Environmental_Taxes", 
                                                                                "Final_Energy_Intensity", "Fuel_Mix", "GDP", "Heat_Pumps", 
                                                                                "Liquid_Biofuels", "Renewable_Energy_Share", "Solar_Thermal"), 
                                                                   multiple = TRUE, width = "100%"),
                                                       selectInput(inputId = 'corrtype',
                                                                   label = 'Type of Correlation',
                                                                   choices = c('Parametric - Pearson' = 'parametric',
                                                                               'Non-Parametric - Spearman' = 'nonparametric',
                                                                               'Robust' = 'robust')),
                                                       selectInput(inputId = 'siglevel',
                                                                   label = '(Correlation Matrix) Significance Level',
                                                                   choices = c(0.01, 0.1, 0.5),
                                                                   selected = 0.5),
                                                       actionButton("goButtonCorr", "Apply changes")
                                                   ), 
                                                   mainPanel(
                                                       fluidRow(
                                                           htmlOutput("corrlabel"), 
                                                           plotOutput("corr", width = "800px", height = "700px"))
                                                   )
                                               )),
                                      tabPanel("Scatterplot", 
                                               sidebarLayout(
                                                   sidebarPanel(
                                                       sliderInput(inputId = "eda_year5", 
                                                                   label = "Year of Analysis:", 
                                                                   min = 2010, 
                                                                   max = 2018, 
                                                                   value = c(2010, 2018), 
                                                                   sep = ""), 
                                                       pickerInput(inputId = "eda_country5", 
                                                                   label = "Select Countries:", 
                                                                   choices = countries, 
                                                                   selected = unique(countries), 
                                                                   options = list(`actions-box` = TRUE), 
                                                                   multiple = TRUE), 
                                                       selectInput(inputId = "eda_xvariable5", 
                                                                   label = "X-Variable:", 
                                                                   choices = c("Greenhouse Gases" = "Greenhouse_Gases", 
                                                                               "Carbon Dioxide" = "Carbon_Dioxide", 
                                                                               "Methane" = "Methane", 
                                                                               "Nitrous Oxide" = "Nitrous_Oxide", 
                                                                               "Carbon Intensity" = "Carbon_Intensity", 
                                                                               "Environmental Taxes" = "Environmental_Taxes", 
                                                                               "Final Energy Intensity" = "Final_Energy_Intensity", 
                                                                               "Fuel Mix" = "Fuel_Mix", 
                                                                               "GDP" = "GDP", 
                                                                               "Heat Pumps" = "Heat_Pumps", 
                                                                               "Liquid Biofuels" = "Liquid_Biofuels", 
                                                                               "Renewable Energy Share" = "Renewable_Energy_Share", 
                                                                               "Solar Thermal" = "Solar_Thermal"), 
                                                                   selected = c("Greenhouse_Gases"), width = "100%"),
                                                       selectInput(inputId = "eda_yvariable5", 
                                                                   label = "Y-Variable:", 
                                                                   choices = c("Greenhouse Gases" = "Greenhouse_Gases", 
                                                                               "Carbon Dioxide" = "Carbon_Dioxide", 
                                                                               "Methane" = "Methane", 
                                                                               "Nitrous Oxide" = "Nitrous_Oxide", 
                                                                               "Carbon Intensity" = "Carbon_Intensity", 
                                                                               "Environmental Taxes" = "Environmental_Taxes", 
                                                                               "Final Energy Intensity" = "Final_Energy_Intensity", 
                                                                               "Fuel Mix" = "Fuel_Mix", 
                                                                               "GDP" = "GDP", 
                                                                               "Heat Pumps" = "Heat_Pumps", 
                                                                               "Liquid Biofuels" = "Liquid_Biofuels", 
                                                                               "Renewable Energy Share" = "Renewable_Energy_Share", 
                                                                               "Solar Thermal" = "Solar_Thermal"), 
                                                                   selected = c("Carbon_Intensity"), width = "100%"),
                                                       actionButton("goButtonBiVar", "Apply changes")
                                                   ), 
                                                   mainPanel(
                                                       fluidRow(
                                                           htmlOutput("biVarlabel"), 
                                                           plotlyOutput("biVar", height = "600px"))
                                                   )
                                               ))), 
                           
                           # -------------------- OLS -------------------- #
                           navbarMenu("Ordinary Least Square Regression", icon = icon("globe-europe"), 
                                      tabPanel("Variable Selection", 
                                               sidebarLayout(
                                                   sidebarPanel(
                                                       selectInput(inputId = "vs_dep", label = "Dependent Variable:", 
                                                                   choices = c("Greenhouse Gases" = "Greenhouse_Gases", 
                                                                               "Carbon Dioxide" = "Carbon_Dioxide", 
                                                                               "Methane" = "Methane", 
                                                                               "Nitrous Oxide" = "Nitrous_Oxide"), 
                                                                   selected = c("GHG_emissions"), width = "100%"),
                                                       selectInput(inputId = "vs_indep", label = "Independent Variables:", 
                                                                   choices = c("Carbon Intensity" = "Carbon_Intensity", 
                                                                               "Environmental Taxes" = "Environmental_Taxes", 
                                                                               "Final Energy Intensity" = "Final_Energy_Intensity", 
                                                                               "Fuel Mix" = "Fuel_Mix", 
                                                                               "GDP" = "GDP", 
                                                                               "Heat Pumps" = "Heat_Pumps", 
                                                                               "Liquid Biofuels" = "Liquid_Biofuels", 
                                                                               "Renewable Energy Share" = "Renewable_Energy_Share", 
                                                                               "Solar Thermal" = "Solar_Thermal"), 
                                                                   selected = c("GDP"),
                                                                   multiple = TRUE,
                                                                   width = "100%"), 
                                                       selectInput(inputId = "vs_yr", label = "Year of Analysis:",
                                                                   choices = c("2010" = 2010,
                                                                               "2011" = 2011,
                                                                               "2012" = 2012,
                                                                               "2013" = 2013,
                                                                               "2014" = 2014,
                                                                               "2015" = 2015,
                                                                               "2016" = 2016,
                                                                               "2017" = 2017,
                                                                               "2018" = 2018),
                                                                   selected = 2010, width = "100%"),
                                                       selectInput(inputId = "vs_methd", label = "Method:", 
                                                                   choices = c("Stepwise AIC forward" =  1,
                                                                               "Stepwise AIC backward" = 2,
                                                                               "Stepwise AIC both" = 3, 
                                                                               "Stepwise BIC forward" = 4,
                                                                               "Stepwise BIC backward" = 5,
                                                                               "Stepwise BIC both" = 6), 
                                                                   selected = 1,
                                                                   width = "100%"),          
                                                       actionButton("goButtonOLSvs", "Apply changes")
                                                   ), 
                                                   mainPanel(
                                                       tabsetPanel(
                                                           tabPanel("Summary", br(),
                                                                    fluidRow(
                                                                        htmlOutput("step_sum"),
                                                                        htmlOutput("step_sum2"),
                                                                        verbatimTextOutput("vs_sumresult"),
                                                                        htmlOutput("step_sum_interpret"),
                                                                        htmlOutput("step_sum3"),
                                                                        verbatimTextOutput("vs_sumresult2"),
                                                                        htmlOutput("step_sum2_interpret"))), 
                                                           tabPanel("Plot", br(),
                                                                    fluidRow(
                                                                    htmlOutput("step_plot"),
                                                                    plotOutput("vs_plotresult"),
                                                                    htmlOutput("step_plot_interpret"))),
                                                           tabPanel("Detailed Output", br(),
                                                                    verbatimTextOutput("vs_detailedresult"))
                                                       )
                                                   )
                                               )),
                                      tabPanel("Model Selected", 
                                               sidebarLayout(
                                                   sidebarPanel(
                                                       selectInput(inputId = "ms_dep", label = "Dependent Variable:", 
                                                                   choices = c("Greenhouse Gases" = "Greenhouse_Gases", 
                                                                               "Carbon Dioxide" = "Carbon_Dioxide", 
                                                                               "Methane" = "Methane", 
                                                                               "Nitrous Oxide" = "Nitrous_Oxide"), 
                                                                   selected = "GHG_emissions"), 
                                                       selectInput(inputId = "ms_indep", label = "Independent Variables:", 
                                                                   choices = c("Carbon Intensity" = "Carbon_Intensity", 
                                                                               "Environmental Taxes" = "Environmental_Taxes", 
                                                                               "Final Energy Intensity" = "Final_Energy_Intensity", 
                                                                               "Fuel Mix" = "Fuel_Mix", 
                                                                               "GDP" = "GDP", 
                                                                               "Heat Pumps" = "Heat_Pumps", 
                                                                               "Liquid Biofuels" = "Liquid_Biofuels", 
                                                                               "Renewable Energy Share" = "Renewable_Energy_Share", 
                                                                               "Solar Thermal" = "Solar_Thermal"), 
                                                                   selected = "GDP",
                                                                   multiple = TRUE),
                                                       selectInput(inputId = "ms_yr", label = "Year of Analysis:",
                                                                   choices = c("2010" = 2010,
                                                                               "2011" = 2011,
                                                                               "2012" = 2012,
                                                                               "2013" = 2013,
                                                                               "2014" = 2014,
                                                                               "2015" = 2015,
                                                                               "2016" = 2016,
                                                                               "2017" = 2017,
                                                                               "2018" = 2018),
                                                                   selected = 2010),
                                                       selectInput(inputId = "ms_hettest", label = "Heteroskedasticity Test:", 
                                                                   choices = c("Bvensch Pagan Test" = 1, 
                                                                               "Score test" = 2, 
                                                                               "F-test" = 3), 
                                                                   selected = 1),     
                                                       selectInput(inputId = "ms_resdiag", label = "Residual Diagnostics:", 
                                                                   choices = c("QQ-plot" = 1, 
                                                                               "Residual vs Fitted value" = 2,
                                                                               "Residual Histogram" = 3), 
                                                                   selected = 1), 
                                                       
                                                       selectInput(inputId = "ms_coldiag", label = "Collinear Diagnostics:", 
                                                                   choices = c("Collinear plot" = 1), 
                                                                   selected = 1), 
                                                       selectInput(inputId = "ms_fitass", label = "Model Fit Assessment:", 
                                                                   choices = c("Residual Fit Spreadplot" = 1, 
                                                                               "Observed vs Predicted plot" = 2), 
                                                                   selected = 1), 
                                                       actionButton("goButtonOLSms", "Apply changes")
                                                   ), 
                                                   mainPanel(
                                                       tabsetPanel(
                                                           tabPanel("Output (Table)", br(), 
                                                                    fluidRow(
                                                                    htmlOutput("fitted_lm"), br(), 
                                                                    verbatimTextOutput("ms_model"), br(), 
                                                                    htmlOutput("Regression_Summary"), br(), 
                                                                    verbatimTextOutput("ms_table"),
                                                                    htmlOutput("ms_model_interpret"))),
                                                           tabPanel("Output (Plot)", 
                                                                    fluidRow(
                                                                    htmlOutput("Coefficient_Estimates"), br(),
                                                                    plotOutput("ms_plot"),
                                                                    htmlOutput("ms_modelplot_interpret"))),
                                                           tabPanel("Heteroskedasticity Test", br(), 
                                                                    fluidRow(
                                                                    htmlOutput("ms_het"), br(),
                                                                    verbatimTextOutput("ms_hetresult"),
                                                                    htmlOutput("ms_het_interpret"))),
                                                           tabPanel("Residual Diagnostics", br(), 
                                                                    fluidRow(
                                                                    htmlOutput("ms_res"), br(),
                                                                    plotOutput("ms_resresult"),
                                                                    htmlOutput("ms_res_interpret"))),
                                                           tabPanel("Collinear Diagnostics", br(), 
                                                                    fluidRow(
                                                                    htmlOutput("ms_col"), br(),
                                                                    verbatimTextOutput("ms_colresult"),
                                                                    htmlOutput("ms_col_interpret"))),
                                                           tabPanel("Model Fit Assessment", br(), 
                                                                    fluidRow(
                                                                    htmlOutput("ms_fit"), br(),
                                                                    plotOutput("ms_fitassresult"),
                                                                    htmlOutput("ms_fit_interpret")))
                                                       )
                                                   )
                                               ))),
                           
                           # -------------------- PLM -------------------- #
                           navbarMenu("Panel Data Regression", icon = icon("table"),
                                      
                                      # ----- PLM Model Building ----- #
                                      tabPanel("Model Building", 
                                               sidebarLayout(
                                                   sidebarPanel(
                                                       # Selection of Year Range
                                                       sliderInput(inputId = "year", 
                                                                   label = "Year of Analysis:", 
                                                                   min = 2010, 
                                                                   max = 2018, 
                                                                   value = c(2010, 2018), 
                                                                   sep = ""), 
                                                       # Selection of Countries
                                                       pickerInput(inputId = "country", 
                                                                   label = "Select Countries:", 
                                                                   choices = countries, 
                                                                   selected = unique(countries), 
                                                                   options = list(`actions-box` = TRUE), 
                                                                   multiple = TRUE), 
                                                       # Selection of Dependent Variable
                                                       selectInput(inputId = "depVar", 
                                                                   label = "Dependent Variable:",
                                                                   choices = c("Greenhouse Gases" = "Greenhouse_Gases", 
                                                                               "Carbon Dioxide" = "Carbon_Dioxide", 
                                                                               "Methane" = "Methane", 
                                                                               "Nitrous Oxide" = "Nitrous_Oxide"),
                                                                   selected = c("GHG_emissions"),
                                                                   width = "100%"), 
                                                       # Selection of Independent Variables
                                                       selectInput(inputId = "indepVars", 
                                                                   label = "Independent Variables:",
                                                                   choices = c("Carbon Intensity" = "Carbon_Intensity", 
                                                                               "Environmental Taxes" = "Environmental_Taxes", 
                                                                               "Final Energy Intensity" = "Final_Energy_Intensity", 
                                                                               "Fuel Mix" = "Fuel_Mix", 
                                                                               "GDP" = "GDP", 
                                                                               "Heat Pumps" = "Heat_Pumps", 
                                                                               "Liquid Biofuels" = "Liquid_Biofuels", 
                                                                               "Renewable Energy Share" = "Renewable_Energy_Share", 
                                                                               "Solar Thermal" = "Solar_Thermal"),
                                                                   selected = c("GDP"),
                                                                   multiple = TRUE,
                                                                   width = "100%"),
                                                       # Selection of Model
                                                       selectInput(inputId = "plmModel", 
                                                                   label = "Model Type:", 
                                                                   choices = c("Fixed Effect" = "within", 
                                                                               "Random Effect" = "random"), 
                                                                   selected = c("within")), 
                                                       # Selection of Effect
                                                       selectInput(inputId = "plmEffect", 
                                                                   label = "Effects:", 
                                                                   choices = c("Individual" = "individual", 
                                                                               "Time" = "time", 
                                                                               "Two-ways" = "twoways"), 
                                                                   selected = c("individual")), 
                                                       conditionalPanel(condition = "input.plmModel == 'random'", 
                                                                        selectInput("plmRandMethod",
                                                                                    label = "Random Method:", 
                                                                                    choices = c("Swamy and Arora" = "swar", 
                                                                                                "Wallace and Hussain" = "walhus", 
                                                                                                "Amemiya" = "amemiya", 
                                                                                                "Nerlove" = "nerlove"), 
                                                                                    selected = c("swar"))), 
                                                       conditionalPanel(condition = "input.plmModel == 'random'", 
                                                                        selectInput("plmLMtype",
                                                                                    label = "(Random Effect) LM Test Type:", 
                                                                                    choices = c("Breush-Pagan" = "bp", 
                                                                                                "Honda" = "honda", 
                                                                                                "King and Wu" = "kw"), 
                                                                                    selected = c("bp"))), 
                                                       actionButton("goButtonPanel", "Apply changes")
                                                   ),
                                                   
                                                   mainPanel(
                                                       tabsetPanel(
                                                           tabPanel("Summary", br(), 
                                                                    fluidRow(
                                                                        htmlOutput("plmlabel_1"), 
                                                                        tableOutput("plmcoefficients"),
                                                                        htmlOutput("plminterpret_1"))), 
                                                           tabPanel("Plot", br(), 
                                                                    fluidRow(
                                                                        htmlOutput("plmlabel_2"), 
                                                                        plotOutput("plmplot"),
                                                                        htmlOutput("plminterpret_2"))), 
                                                           tabPanel("Detailed Output", br(), 
                                                                    fluidRow(
                                                                        verbatimTextOutput("plmstats"),
                                                                        htmlOutput("plminterpret_3"))), 
                                                           tabPanel("Model Fit Assessment", br(), 
                                                                    fluidRow(
                                                                        htmlOutput("plmhauslabel"), 
                                                                        htmlOutput("plmhausdesc"), br(),
                                                                        verbatimTextOutput("plmhausTestVars"),
                                                                        verbatimTextOutput("plmhausTest"), 
                                                                        htmlOutput("plmhausinterpret"), br(),
                                                                        
                                                                        htmlOutput("plmchowlabel"), 
                                                                        htmlOutput("plmchowdesc"), br(),
                                                                        verbatimTextOutput("plmchowTestVars"),
                                                                        verbatimTextOutput("plmchowTest"), 
                                                                        htmlOutput("plmchowinterpret"), br(),
                                                                        
                                                                        htmlOutput("plmLMlabel"),
                                                                        htmlOutput("plmLMdesc"), br(),
                                                                        verbatimTextOutput("plmLMVars"),
                                                                        verbatimTextOutput("plmLMTest"), 
                                                                        htmlOutput("plmLMinterpret")
                                                                        ))
                                                       ))
                                                   )), 
                                      
                                      # ----- Tests for PLM Assumptions ----- #
                                      
                                      tabPanel("Tests for Assumptions", 
                                               sidebarLayout(
                                                   sidebarPanel(
                                                       # Selection of Normality Tests
                                                       selectInput(inputId = "normTest", 
                                                                   label = "Normality Test",
                                                                   choices = c("Residual QQ-plot", "Residual vs Fitted Value Plot", "Residual Histogram"),
                                                                   selected = c("Residual QQ-plot"),
                                                                   width = "100%"), 
                                                       # Selection of Serial Correlation Tests
                                                       selectInput(inputId = "serialcorTest", 
                                                                   label = "Serial Correlation Test",
                                                                   choices = c("Unobserved Effect Test", "Locally Robust Test", "Breusch-Godfrey/Wooldridge Test"),
                                                                   selected = c("Unobserved Effect Test"),
                                                                   width = "100%"), 
                                                       # Selection of Heteroscedasticity Tests
                                                       selectInput(inputId = "heteroskTest", 
                                                                   label = "Heteroscedasticity Test",
                                                                   choices = c("Breusch-Pagan Test", "Residual vs Fitted Value Plot"),
                                                                   selected = c("Breusch-Pagan Test"),
                                                                   width = "100%"), 
                                                       actionButton("goButtonAssump", "Apply changes")     
                                                   ), 
                                                   
                                                   mainPanel(
                                                       tabsetPanel(
                                                           tabPanel("Normality Test", br(), 
                                                                    fluidRow(
                                                                        htmlOutput("plmnormdesc"), br(),
                                                                        htmlOutput("normlabel_1"), 
                                                                        plotOutput("plmnormTest"),
                                                                        htmlOutput("plmnorminterpret"))), 
                                                           tabPanel("Serial Correlation Test", br(), 
                                                                    fluidRow(
                                                                        htmlOutput("plmserialcordesc"), br(),
                                                                        verbatimTextOutput("plmserialCorVars"),
                                                                        verbatimTextOutput("plmserialcorTest"), 
                                                                        htmlOutput("plmserialcorinterpret"))), 
                                                           tabPanel("Heteroscedasticity Test", br(), 
                                                                    fluidRow(
                                                                        htmlOutput("plmheteroskdesc"), br(),
                                                                        verbatimTextOutput("plmheteroskVars"),
                                                                        verbatimTextOutput("plmheteroskTest"), 
                                                                        htmlOutput("plmheteroskinterpret1"), 
                                                                        plotOutput("plmheteroskPlot"), 
                                                                        htmlOutput("plmheteroskinterpret2")))
                                                    ))))
                                      )
                )
)

# Define server logic required to output results
server <- function(input, output, session) {
    
    # -------------------- OVERVIEW -------------------- #
    
    output$shinydesc <- renderUI({HTML(paste("<b>", "About", "</b><br><br>", 
                                             "This application explores the relationship between drivers and mitigation measures with greenhouse gas emission levels in the European Union countries. <br>", 
                                             "The application covers multiple components which include:", 
                                             "<ul><li> Data overview to explore dataset used and assess missing data </li>",
                                             "<li> Exploratory data analysis to understand the factors affecting greenhouse gas emissions </li>", 
                                             "<li> OLS regression to identify main factors contributing to greenhouse gas emissions from cross-sectional view </li>",
                                             "<li> Panel data greession to identify main factors contributing to greenhouse gas emissions from panel data view</li></ul>"
                                             ))})
    
    # -------------------- DATA -------------------- #
    
    # Dynamic filtering of dataframe
    data_ghg <- reactive({
        ghg_df %>%
            filter(Year %in% (input$eda_year[1]:input$eda_year[2]))%>%
            filter(Country %in% input$eda_country)
    })
    
    # Output filtered dataset
    output$table <- renderDataTable({
        input$goButtonEDAdata
        
        isolate({
        datatable(data = data_ghg(), extensions = "FixedHeader", 
                  options = list(pageLength = 20, scrollX = TRUE),
                  filter = "top")
        })
    })
    
    # Plot Missing Value Chart
    output$missinglabel <- renderUI({HTML(paste("<b>", "Percentage of Missing Data by Variable and Country", "</b>"))})
    
    output$missingdat <- renderPlot({
        input$goButtonEDAdata
        
        isolate({
            prepare_missing_values_graph(data_ghg(), ts_id = "Country")
        })
    })
    
    # -------------------- EDA -------------------- #
    
    # Dynamic filtering of dataframe
    eda_df <- reactive({
        
        ghg_df %>%
            filter(Year %in% (input$eda_year2[1]:input$eda_year2[2]))%>%
            filter(Country %in% input$eda_country2) %>% 
            dplyr::select(input$eda_variable2)
    })
    
    # Summary Statistics
    output$desc <- function(){
        input$goButtonEDAanalysis
        
        isolate({
        sumstats <- prepare_descriptive_table(eda_df())
        sumstats$kable_ret  %>%
            kable_styling("condensed", position = "center")
        })
    }
    
    # Facet Histograms
    output$histolabel <- renderUI({HTML(paste("<b>", "Histogram Distribution for Selected Variables", "</b>"))})
    output$histo <- renderPlotly({
        input$goButtonEDAanalysis
        
        isolate({
        hist_df <- ghg_df %>% 
            filter(Year %in% (input$eda_year2[1]:input$eda_year2[2]))%>%
            filter(Country %in% input$eda_country2) %>% 
            dplyr::select(input$eda_variable2)
        histplot <- hist_df %>%
                    gather() %>% 
                    ggplot(aes(value)) +
                    facet_wrap( ~ key, ncol=3, scales="free") +
                    geom_histogram(fill = "cornflowerblue", bins = input$nb_bins) 
        
        ggplotly(histplot)
        
        })
    })
    
    # Dynamic filtering of dataframe
    eda_trenddf <- reactive({
        ghg_df %>%
            filter(Year %in% (input$eda_year3[1]:input$eda_year3[2]))%>%
            filter(Country %in% input$eda_country3)
    })
    
    # Trend Line Graph
    output$linelabel <- renderUI({HTML(paste("<b>", "Time Trend for Variables", "</b><br>", 
                                             "Note: Reduce the number of countries selected to minimise the amount of overlaps"))})
    output$line <- renderPlotly({
        input$goButtonTimeT
        
        isolate({
        
        if (input$eda_timetrend == "Individual") {
            trend <- ggplot(eda_trenddf(), aes_string(x = "Year", y = input$eda_variable3, color="Country")) +
                geom_line() +
                scale_y_log10()
            
            ggplotly(trend)
        } else {
            grped <- prepare_trend_graph(df = eda_trenddf(), ts_id = "Year")
            grped$plot 
        }
            
        })
    })
    
    # Dynamic filtering of dataframe
    eda_corrdf <- reactive({
        input$goButtonCorr
        
        isolate({
        ghg_df %>%
            filter(Year == input$eda_year4) %>%
            filter(Country %in% input$eda_country4) %>% 
            dplyr::select(input$eda_variable4)
        })
    })
    
    # Correlation Plot
    output$corrlabel <- renderUI({HTML(paste("<b>", "Correlation Matrix for Selected Variables", "</b>"))})    
    output$corr <- renderPlot({
        input$goButtonCorr
        
        isolate({
        ggcorrmat(data = eda_corrdf(),
                  type = input$corrtype,
                  sig.level = input$siglevel)
        })
    })
    
    # Dynamic filtering of dataframe
    eda_biVardf <- reactive({
        input$goButtonBiVar
        
        isolate({
            ghg_df %>%
                filter(Year %in% (input$eda_year5[1]:input$eda_year5[2])) %>%
                filter(Country %in% input$eda_country5)
        })
    })
    
    ## Bivariate Plot
    output$biVarlabel <- renderUI({
        input$goButtonBiVar
        
        isolate({
            HTML(paste("<b>", "Scatterplot of", input$eda_xvariable5, "vs", input$eda_yvariable5, "</b>"))
            })
    })
    output$biVar <- renderPlotly({
        input$goButtonBiVar
        
        isolate({
        biVar <- prepare_scatter_plot(eda_biVardf(), x=input$eda_xvariable5, y=input$eda_yvariable5, color="Country", loess = 1)
        
        ggplotly(biVar)
        })
    })
    
    # -------------------- OLS -------------------- #
    
    # ----- OLS Variable Selection ----- #
    
    # Summary

    output$step_label <- renderUI({HTML(paste("<b>", "Purpose of Variable Selection:", "</b></br>", 
                                              "To identify the best subset of independent variables to include in the model, among all possible subsets of independent variables."))})
    
    output$step_sum <- renderUI({
        input$goButtonOLSvs
        
        isolate({
        if (input$vs_methd==1) {
            HTML(paste("In a step-wise AIC forward method, we can start with a single predictor and continue to add predictors based on AIC value until no variable is left."))
        }
        else if (input$vs_methd==2) {
            HTML(paste("In a step-wise AIC backwards method, we can start with the full list of predictors and remove predictors based on AIC value until only one variable is left."))
        }
        else if (input$vs_methd==3) {
            HTML(paste("In a step-wise AIC method (both forward & backward), we can start with a single predictor and continue to add/remove predictors based on BIC value until no variable is left."))
        }
        else if (input$vs_methd==4) {
            HTML(paste("In a step-wise AIC forward method, we can start with a single predictor and continue to add predictors based on BIC value until no variable is left."))
        }
        else if (input$vs_methd==5) {
            HTML(paste("In a step-wise AIC backwards method, we can start with the full list of predictors and remove predictors based on AIC value until only one variable is left."))
        }
        else if (input$vs_methd==6) {
            HTML(paste("In a step-wise BIC method (both forward & backward), we can start with a single predictor and continue to add/remove predictors based on BIC value until no variable is left."))
        }
        })
    })
    
    output$step_sum_interpret <- renderUI({
        HTML(paste("<b>", "How to interpret the output?", "</b>",
                   "<ul><li> The above output shows the change in criterion value after every step, which is useful for understanding the importance of the independent variable added (+)/removed (-) from the model. </li></ui>"))
    })
    
    output$step_sum2_interpret <- renderUI({
        HTML(paste("<b>", "How to interpret the output?", "</b>",
                   "<ul><li> If the estimate is positive, there is positive relationship between the independent variable and dependent variable. </ui>", 
                   "<li> The parameter estimate is statistically significant at 90% confidence interval (CI) if sig value (i.e. p-value) is <0.1 (95% CI: p-value <0.05 and 99% CI: p-value <0.01). </li></ui>"))
    })
    
    output$step_sum2 <- renderUI({
        HTML(paste("<b>", "ANOVA Summary - Steps taken in the search", 
                   sep="<br/>"))
    })
    
    output$step_sum3 <- renderUI({
        HTML(paste("<b>", "Regression Summary of the Final Model", 
                   sep="<br/>"))
    })
    
    vs_ghg <- reactive({
        df1 <- ghg_df %>% 
            filter(Year %in% input$vs_yr)
        df2 <- df1[c(input$vs_indep, input$vs_dep)]
        na.omit(df2)
        
    })
    
    output$vs_sumresult <- renderPrint({
        input$goButtonOLSvs
        
        isolate({
        fit1 <- lm(reformulate(input$vs_indep, input$vs_dep), data =vs_ghg())
        fit2 <- lm(reformulate("1", input$vs_dep), data =vs_ghg())
        
        if (input$vs_methd==1) {
            step.model <- stepAIC(fit2, direction = "forward", scope=list(upper=fit1,lower=fit2), trace = FALSE)
            step.model$anova
        }
        else if (input$vs_methd==2) {
            step.model <- stepAIC(fit1, direction = "backward", trace = FALSE)
            step.model$anova
        }
        else if (input$vs_methd==3) {
            step.model <- stepAIC(fit2, direction = "both", scope=list(upper=fit1,lower=fit2), trace = FALSE)
            step.model$anova
        }
        else if (input$vs_methd==4) {
            step.model <- stepAIC(fit2, k = log(nrow(vs_ghg())), direction = "forward", scope=list(upper=fit1,lower=fit2), trace = FALSE)
            step.model$anova
        }
        else if (input$vs_methd==5) {
            step.model <- stepAIC(fit1, k = log(nrow(vs_ghg())), direction = "backward", trace = FALSE)
            step.model$anova
        }
        else if (input$vs_methd==6) {
            step.model <- stepAIC(fit2, k = log(nrow(vs_ghg())), direction = "both", scope=list(upper=fit1,lower=fit2), trace = FALSE)
            step.model$anova
        }
        })
    })
    
    output$vs_sumresult2 <- renderPrint({
        input$goButtonOLSvs
        
        isolate({
        fit1 <- lm(reformulate(input$vs_indep, input$vs_dep), data =vs_ghg())
        fit2 <- lm(reformulate("1", input$vs_dep), data =vs_ghg())
        
        if (input$vs_methd==1) {
            step.model <- stepAIC(fit2, direction = "forward", scope=list(upper=fit1,lower=fit2), trace = FALSE)
            summary(step.model)
        }
        else if (input$vs_methd==2) {
            step.model <- stepAIC(fit1, direction = "backward", trace = FALSE)
            summary(step.model)
        }
        else if (input$vs_methd==3) {
            step.model <- stepAIC(fit2, direction = "both", scope=list(upper=fit1,lower=fit2), trace = FALSE)
            summary(step.model)
        }
        else if (input$vs_methd==4) {
            step.model <- stepAIC(fit2, k = log(nrow(vs_ghg())), direction = "forward", scope=list(upper=fit1,lower=fit2), trace = FALSE)
            summary(step.model)
        }
        else if (input$vs_methd==5) {
            step.model <- stepAIC(fit1, k = log(nrow(vs_ghg())), direction = "backward", trace = FALSE)
            summary(step.model)
        }
        else if (input$vs_methd==6) {
            step.model <- stepAIC(fit2, k = log(nrow(vs_ghg())), direction = "both", scope=list(upper=fit1,lower=fit2), trace = FALSE)
            summary(step.model)
        }
        })
    })
    
    # Plot
    
    output$step_plot <- renderUI({
        input$goButtonOLSvs
        
        isolate({
        if (input$vs_methd==1) {
            HTML(paste("<b>", "Stepwise AIC Forward Selection Plot:", "</b>",
                       sep="<br/>"))
        }
        else if (input$vs_methd==2) {
            HTML(paste("<b>", "Stepwise AIC Backward Elimination Plot:", "</b>",
                       sep="<br/>"))
        }
        else if (input$vs_methd==3) {
            HTML(paste("<b>", "Stepwise AIC Selection (Forward & Backward) Plot:", "</b>",
                       sep="<br/>"))
        }
        else if (input$vs_methd==4) {
            HTML(paste("<b>", "Stepwise BIC Forward Selection Plot:", "</b>",
                       sep="<br/>"))
        }
        else if (input$vs_methd==5) {
            HTML(paste("<b>", "Stepwise BIC Backward Elimination Plot:", "</b>",
                       sep="<br/>"))
        }
        else if (input$vs_methd==6) {
            HTML(paste("<b>", "Setpwise BIC Selection (Forward & Backward) Plot:", "</b>",
                       sep="<br/>"))
        }
        })
    })
    
    output$step_plot_interpret <- renderUI({
        HTML(paste("<b>", "How to interpret the output?", "</b>",
                   "<ul><li> X-axis refers to the step number, while y-axis refers to the criterion value. </li>",
                   "<li> The plot shows the change in criterion value after every step, which is useful for understanding the importance of the independent variable added/removed from the model. </li></ui>"))
    })
    
    output$vs_plotresult <- renderPlot({
        input$goButtonOLSvs
        
        isolate({
        fit1 <- lm(reformulate(input$vs_indep, input$vs_dep), data =vs_ghg())
        fit2 <- lm(reformulate("1", input$vs_dep), data =vs_ghg())
        
        if (input$vs_methd==1) {
            step.model <- stepAIC(fit2, direction = "forward", scope=list(upper=fit1,lower=fit2), trace = FALSE)
            k <- as.data.frame(step.model$anova$AIC)
            k$N_Step <- seq.int(nrow(k))
            k$AIC <- k$`step.model$anova$AIC`
            
            ggplot(data=k, aes(x=N_Step, y=AIC)) + 
                geom_point(color = "blue", size = 2) +
                geom_line(color = "blue") +
                xlab("Step number") + ylab("AIC value") 
        }
        else if (input$vs_methd==2) {
            step.model <- stepAIC(fit1, direction = "backward", trace = FALSE)
            k <- as.data.frame(step.model$anova$AIC)
            k$N_Step <- seq.int(nrow(k))
            k$AIC <- k$`step.model$anova$AIC`
            
            ggplot(data=k, aes(x=N_Step, y=AIC)) + 
                geom_point(color = "blue", size = 2) +
                geom_line(color = "blue") +
                xlab("Step number") + ylab("AIC value") 
        }
        else if (input$vs_methd==3) {
            step.model <- stepAIC(fit2, direction = "both", scope=list(upper=fit1,lower=fit2), trace = FALSE)
            k <- as.data.frame(step.model$anova$AIC)
            k$N_Step <- seq.int(nrow(k))
            k$AIC <- k$`step.model$anova$AIC`
            
            ggplot(data=k, aes(x=N_Step, y=AIC)) + 
                geom_point(color = "blue", size = 2) +
                geom_line(color = "blue") +
                xlab("Step number") + ylab("AIC value") 
        }
        else if (input$vs_methd==4) {
            step.model <- stepAIC(fit2, k = log(nrow(vs_ghg())), direction = "forward", scope=list(upper=fit1,lower=fit2), trace = FALSE)
            k <- as.data.frame(step.model$anova$AIC)
            k$N_Step <- seq.int(nrow(k))
            k$AIC <- k$`step.model$anova$AIC`
            
            ggplot(data=k, aes(x=N_Step, y=AIC)) + 
                geom_point(color = "blue", size = 2) +
                geom_line(color = "blue") +
                xlab("Step number") + ylab("AIC value") 
        }
        else if (input$vs_methd==5) {
            step.model <- stepAIC(fit1, k = log(nrow(vs_ghg())), direction = "backward", trace = FALSE)
            k <- as.data.frame(step.model$anova$AIC)
            k$N_Step <- seq.int(nrow(k))
            k$AIC <- k$`step.model$anova$AIC`
            
            ggplot(data=k, aes(x=N_Step, y=AIC)) + 
                geom_point(color = "blue", size = 2) +
                geom_line(color = "blue") +
                xlab("Step number") + ylab("AIC value") 
        }
        else if (input$vs_methd==6) {
            step.model <- stepAIC(fit2, k = log(nrow(vs_ghg())), direction = "both", scope=list(upper=fit1,lower=fit2), trace = FALSE)
            k <- as.data.frame(step.model$anova$AIC)
            k$N_Step <- seq.int(nrow(k))
            k$AIC <- k$`step.model$anova$AIC`
            
            ggplot(data=k, aes(x=N_Step, y=AIC)) + 
                geom_point(color = "blue", size = 2) +
                geom_line(color = "blue") +
                xlab("Step number") + ylab("AIC value") 
        }  
        })
    })
    
    # Detailed Output
    
    output$step_detailed <- renderUI({
        HTML(paste("<b>", "Detailed steps taken in the search - ANOVA", 
                   sep="<br/>"))
    })
    
    output$vs_detailedresult <- renderPrint({
        input$goButtonOLSvs
        
        isolate({
        fit1 <- lm(reformulate(input$vs_indep, input$vs_dep), data =vs_ghg())
        fit2 <- lm(reformulate("1", input$vs_dep), data =vs_ghg())
        
        if (input$vs_methd==1) {
            step.model <- stepAIC(fit2, direction = "forward", scope=list(upper=fit1,lower=fit2), trace = TRUE)
            step.model$anova
        }
        else if (input$vs_methd==2) {
            step.model <- stepAIC(fit1, direction = "backward", trace = TRUE)
            step.model$anova
        }
        else if (input$vs_methd==3) {
            step.model <- stepAIC(fit2, direction = "both", scope=list(upper=fit1,lower=fit2), trace = TRUE)
            step.model$anova
        }
        else if (input$vs_methd==4) {
            step.model <- stepAIC(fit2, k = log(nrow(vs_ghg())), direction = "forward", scope=list(upper=fit1,lower=fit2), trace = TRUE)
            step.model$anova
        }
        else if (input$vs_methd==5) {
            step.model <- stepAIC(fit1, k = log(nrow(vs_ghg())), direction = "backward", trace = TRUE)
            step.model$anova
        }
        else if (input$vs_methd==6) {
            step.model <- stepAIC(fit2, k = log(nrow(vs_ghg())), direction = "both", scope=list(upper=fit1,lower=fit2), trace = TRUE)
            step.model$anova
        }
        })
    })
    
    # ---------- OLS Model Selected ---------- #
    
    ms_ghg <- reactive({
        ghg_df %>% 
            filter(Year %in% input$ms_yr) 
    })
    
    # ----- OLS Regression Summary ----- #
    
    output$fitted_lm <- renderUI({HTML(paste("<b>", "The fitted linear model is:", "</b>"))})
    
    output$ms_model <- renderPrint({
        input$goButtonOLSms
        
        isolate({
        reformulate(input$ms_indep, input$ms_dep)
        })
    })
    
    output$Regression_Summary <- renderUI({
        HTML(paste("<b>", "Regression Summary", "</b>"))
    })
    
    output$ms_table <- renderPrint({
        input$goButtonOLSms
        
        isolate({
        ols_regress(reformulate(input$ms_indep, input$ms_dep),data = ms_ghg())
        })
    })
    
    output$ms_model_interpret <- renderUI({
        HTML(paste("<b>", "How to interpret the output?", "</b>",
                   "<ul><li> Adjusted R-square explains the degree to which independent variables explain the variation of the dependent variable, while taking into consideration the number of independent variables included in the model. It ranges from 0 to 1. A higher adjusted R-square indicates a better fit for the model. </li>",
                   "<li> If beta is positive, there is positive relationship between the predictor and dependent variable. </li>", 
                   "<li> The parameter estimate is statistically significant at 90% confidence interval (CI) if sig value (i.e. p-value) is <0.1 (95% CI: p-value <0.05 and 99% CI: p-value <0.01). </li></ul>"))
    })
    
    output$Coefficient_Estimates <- renderUI({
        HTML(paste("<b>", "A plot of the Coefficient Estimates",
                   " ",
                   sep="<br/>"))
    })
    
    output$ms_plot <- renderPlot({
        input$goButtonOLSms
        
        isolate({
        ms_lm_reg <- lm(reformulate(input$ms_indep, input$ms_dep),data = ms_ghg())
        ggcoefstats(x = ms_lm_reg, output = "plot") + 
            scale_y_discrete(labels=c("Carbon_intensity" = "Carbon intensity", 
                                      "Envt_taxes" = "Environmental taxes",
                                      "Final_EI" = "Final energy intensity", 
                                      "Fuel_mix" = "Fuel mix", 
                                      "Heat_pumps" = "Heat pumps", 
                                      "Liquid_biofuels" = "Liquid biofuels", 
                                      "Renewables_share" = "Renewable energy share", 
                                      "Solar_thermal" = "Solar thermal"))
        })
    })
    
    
    output$ms_modelplot_interpret <- renderUI({
        HTML(paste("<b>", "How to interpret the output?", "</b>",
                   "<ul><li> If beta is positive, there is positive relationship between the predictor and dependent variable. </li>", 
                   "<li> The parameter estimate is statistically significant at 90% confidence interval (CI) if sig value (i.e. p-value) is <0.1 (95% CI: p-value <0.05 and 99% CI: p-value <0.01). </li></ul>"))
    })
    
    # ----- Heteroskedasticity Test ----- #
    
    output$ms_het <- renderUI({
        HTML(paste("<b>", "Purpose of Heteroskedasticity test:", "</b><br>",
                   "OLS regression assumes that all residuals are drawn from a population that has a constant variance i.e. homoskedasticity. If the assumption is violated, there is heteroskedasticity problem and the estimators will be inefficient and hypothesis tests will be invalid. <br>",
                   "Hence, run the following test ..."))
    })
    
    output$ms_hetresult <- renderPrint({
        input$goButtonOLSms
        
        isolate({
        if(input$ms_hettest==1){
            ols_test_breusch_pagan(lm(reformulate(input$ms_indep, input$ms_dep),data = ms_ghg()))
        }
        else if (input$ms_hettest==2){
            ols_test_score(lm(reformulate(input$ms_indep, input$ms_dep),data = ms_ghg()))
        }
        else if (input$ms_hettest==3){
            ols_test_f(lm(reformulate(input$ms_indep, input$ms_dep),data = ms_ghg()))
        }
        })
    })
    
    output$ms_het_interpret <- renderUI({
        input$goButtonOLSms
        
        isolate({
        if(input$ms_hettest==1){
            HTML(paste("<b>", "How to interpret the output?", "</b>",
                       "<ul><li> If p-value is >0.05, cannot reject the null hypothesis and hence conclude that the variance is constant. </li></ul>"))
        }
        else if (input$ms_hettest==2){
            HTML(paste("<b>", "Notes:", "</b>",
                       "<ul><li> We can also test for heteroskedasticity under the assumption that the errors are independent and identically distributed (i.i.d.). </li></ul>", " ",
                       "<b>", "How to interpret the output?", "</b>",
                       "<ul><li> From either the Chi2 or p-value, users can determine if the null hypothesis is being rejected and hence if variance is homogenous. For example, if the p-value is >0.05, we cannot reject the null hypothesis and hence conclude that the variance is homogenous. </li></ul>"))
        }
        else if (input$ms_hettest==3){
            HTML(paste("<b>", "Notes:", "</b>",
                       "<ul><li> We can also test for heteroskedasticity under the assumption that the errors are independent and identically distributed (i.i.d.). </li></ul>", " ",
                       "<b>", "How to interpret the output?", "</b>",
                       "<ul><li> From the p-value, users can determine if the null hypothesis is being rejected and hence if variance is homogenous. For example, if the p-value is >0.05, we cannot reject the null hypothesis and hence conclude that the variance is homogenous. </li></ul>"))
        }
        })
        
    })
    
    # -----Residual Diagnostics ----- #
    
    output$ms_res <- renderUI({
        input$goButtonOLSms
        
        isolate({
        if(input$ms_resdiag==1){
            HTML(paste("<b>", "Purpose of Residual diagnostics:", "</b><br>",
                       "OLS regression assumes that the error has a normal distribution. <br>",
                       "Hence, to detect non-linearity, plot the QQ-plot."))
        }
        else if (input$ms_resdiag==2){
            HTML(paste("<b>", "Purpose of Residual diagnostics:", "</b><br>",
                       "OLS regression assumes that the error has a normal distribution.<br>",
                       "Hence, to detect non-linearity, unequal error variances, and outliers, a scatter plot with residual on the y-axis and fitted values on the x-axis can be plotted."))
        }
        else if (input$ms_resdiag==3){
            HTML(paste("<b>", "Purpose of Residual diagnostics:", "</b><br>",
                       "OLS regression assumes that the error has a normal distribution. <br>",
                       "A histogram can also be plotted with a single line of code using olsrr() package to detect if normality assumption has been violated."))
        }
        })
    })
    
    output$ms_resresult <- renderPlot({
        input$goButtonOLSms
        
        isolate({
        if(input$ms_resdiag==1){
            ols_plot_resid_qq(lm(reformulate(input$ms_indep, input$ms_dep),data = ms_ghg()))
        }
        else if (input$ms_resdiag==2){
            ols_plot_resid_fit(lm(reformulate(input$ms_indep, input$ms_dep),data = ms_ghg()))
        }
        else if (input$ms_resdiag==3){
            ols_plot_resid_hist(lm(reformulate(input$ms_indep, input$ms_dep),data = ms_ghg()))
        }
        })
    })
    
    output$ms_res_interpret <- renderUI({
        input$goButtonOLSms
        
        isolate({
        if(input$ms_resdiag==1){
            HTML(paste("<b>", "How to interpret the output?", "</b>",
                       "<ul><li> From the QQ-plot, if the data points do not fall near the diagonal line, it can be said that the normality assumption has been violated. </li></ul>"))
        }
        else if (input$ms_resdiag==2){
            HTML(paste("<b>", "How to interpret the output?", "</b>",
                       "<ul><li> If the residuals lies randomly around the residual = 0 line, it suggests that the assumption that the relationship is linear is reasonable. </li>",
                       "<li> If the residuals roughly form a horizontal band around the residual = 0 line, this suggests that the variances of the error terms are equal. </li>",
                       "<li> If no one residual stands out from the basic random pattern of residuals, this suggests that there are no outliers. </li></ul>"))
        }
        else if (input$ms_resdiag==3){
            HTML(paste("<b>", "How to interpret the output?", "</b>",
                       "<ul><li> A symmetric bell-shaped histogram which is evenly distributed around zero indicates that the normality assumption is likely to be true. </li></ul>"))
        }
        })
    })
    
    # ----- Collinear Diagnostics ----- #
    
    output$ms_col <- renderUI({
        HTML(paste("<b>", "Purpose of Collinear diagnostics:", "</b><br>",
                   "OLS assumes that variables are not collinear with each other. In the presence of multicollinearity, regression estimates are unstable and have high standard errors. <br>",
                   "Hence, run the following diagonstics ..."))
    })    
    
    output$ms_colresult <- renderPrint({
        input$goButtonOLSms
        
        isolate({
        if (length(as.list(input$ms_indep))>1){
            ols_coll_diag(lm(reformulate(input$ms_indep, input$ms_dep),data = ms_ghg()))
        }
        else {
            "Hello there.. the model contains only one independent variable. Please specify a model with at least 2 independent variables."
        }
        })
    })    
    
    output$ms_col_interpret <- renderUI({
        HTML(paste("<b>", "How to interpret the output?", "</b>",
                   "<ul><li> If the VIFs are less than 4, it means there is little sign of multicollinearity that requires correction. </li></ul>"))
    })
    
    # ----- Model Fit Assessment ----- #
    
    output$ms_fit <- renderUI({
        input$goButtonOLSms
        
        isolate({
        if(input$ms_fitass==1){
            HTML(paste("<b>", "Purpose of Model fit assessment:", "</b><br>",
                       "By plotting residual fit spread plot, we can easily see how much variation in the data is explained by the fit and how much remains in the residuals."))
        }
        else if (input$ms_fitass==2){
            HTML(paste("<b>", "Purpose of Model fit assessment:", "</b><br>",
                       "To access the fit of the model, we can plot the observed on the x-axis and fitted values on the y-axis."))
        }
        })
    })
    
    output$ms_fitassresult <- renderPlot({
        input$goButtonOLSms
        
        isolate({
        if(input$ms_fitass==1){
            ols_plot_resid_fit_spread(lm(reformulate(input$ms_indep, input$ms_dep),data = ms_ghg()))
        }
        else if (input$ms_fitass==2){
            ols_plot_obs_fit(lm(reformulate(input$ms_indep, input$ms_dep),data = ms_ghg()))
        }
        })
    })  
    
    output$ms_fit_interpret <- renderUI({
        input$goButtonOLSms
        
        isolate({
        if(input$ms_fitass==1){
            HTML(paste("<b>", "How to interpret the output?", "</b>",
                       "<ul><li> If the spread of the residuals is greater than the spread of the centered fit, the model is deemed as inappropriate and users should consider choosing other models (repeat the variable selection module). </li></ul>"))
        }
        else if (input$ms_fitass==2){
            HTML(paste("<b>", "How to interpret the output?", "</b>",
                       "<ul><li> A model is deemed as a good fit if the points are close to the diagonal line i.e. R-square will be higher. </li></ul>"))
        }
        })
    }) 
    
    
    # -------------------- PLM -------------------- #
    
    # ----- PLM Model Building ----- #
    
    # Dynamic filtering of dataframe
    ghg <- reactive({
        ghg_df %>%
            filter(Year %in% (input$year[1]:input$year[2]))%>%
            filter(Country %in% input$country)
    })
    
    # Running PLM regression
    plm_reg <- reactive({
        ghg_data <- ghg()
        plm(reformulate(input$indepVars, input$depVar),
            data = ghg_data,
            index = c("Country", "Year"),
            model = input$plmModel,
            effect = input$plmEffect, 
            random.method = input$plmRandMethod)
    })
    
    # Output table with PLM coefficients
    output$plmlabel_1 <- renderUI({HTML(paste("<b>", "Estimates Coefficients:", "</b>"))}) # Add table title
    
    output$plmcoefficients <- function() {
        input$goButtonPanel
        
        isolate({
            summary(plm_reg())[["coefficients"]] %>% 
                kbl(digits = 50, format.args = list(scientific = TRUE)) %>% 
                kable_styling(font_size = 14)
        })
    }
    
    output$plminterpret_1 <- renderUI({HTML(paste("<b>", "How to interpret the output?", "</b>", 
                                                  "<ul><li> The estimate represents the amount of change in the emission levels (in thousand tonnes) for every 1 unit of change in the independent variable. A positive estimate meant that emission levels increases for every unit increase of the independent variable while a negative estimate will result in a decrease in the emission levels for every unit increase. </li>
                                                <li> Standard error indicates the estimate of the standard deviation of the coefficients i.e. measure of precision of the regression coefficient. </li>
                                                <li> t-value is the coefficient divided by its standard error. If a coefficient is large relative to its standard error, then it is likely different from 0. </li>
                                                <li> Pr(>|t|) represents the p-value. The variable has a statistically significant influence on the dependent variable at 90% confidence interval (CI) if sig value (i.e. p-value) is <0.1 (95% CI: p-value <0.05 and 99% CI: p-value <0.01). </li></ul>"))})
    
    # Plotting of regression coefficients
    output$plmlabel_2 <- renderUI({HTML(paste("<b>", "Regression Coefficient Estimates Plot", "</b>"))}) # Add chart title
    
    output$plmplot <- renderPlot({
        input$goButtonPanel
        
        isolate({
            ggcoefstats(x = plm_reg(), output = "plot", palette = "Paired") + 
                scale_y_discrete(labels=c("Carbon_Intensity" = "Carbon Intensity", 
                                          "Environmental_Taxes" = "Environmental Taxes",
                                          "Final_Energy_Intensity" = "Final Energy Intensity", 
                                          "Fuel_Mix" = "Fuel Mix", 
                                          "Heat_Pumps" = "Heat Pumps", 
                                          "Liquid_Biofuels" = "Liquid Biofuels", 
                                          "Renewable_Energy_Share" = "Renewable Energy Share", 
                                          "Solar_Thermal" = "Solar Thermal"))
        })
    })
    
    output$plminterpret_2 <- renderUI({HTML(paste("<b>", "How to interpret the plot?", "</b>", 
                                                  "<ul><li> X-axis refers to the regression coefficient estimate for each independent variables, while y-axis refers to the number of independent variables included in the model. </li>
                                                <li> Position of each dot represents the coefficient estimate value, while the whisker line represents the standard error. Each dot is accompanied with statistical value of estimate (B-hat), t-statistics and p-value. </li>
                                                <li> Both AIC (akaike information criterion) and BIC (bayesian information criterion) can be used to determine the best subsets of independent variables and for model comparison. Lower AIC or BIC values indicate that the constructed model is more likely to be the true model. </li>
                                                <li> The plot gives a quick visual comparison of the relative values of the coefficient estimates. </li></ul>"))})
    
    # Output summary statistics from PLM regression
    output$plmstats <- renderPrint({
        input$goButtonPanel
        
        isolate({
            summary(plm_reg())
        })
    })
    
    output$plminterpret_3 <- renderUI({HTML(paste("<b>", "How to interpret the plot?", "</b><br>", 
                                                  "Apart from the statistics covered under Summary and Plot, the Detailed Output offers additional statistical information of the constructed model.", 
                                                  "<ul><li> Balanced panel means that there is value for all the independent variables for all selected countries across all selected years. Any missing data will result in an unbalanced panel. </li>
                                                <li> Overall model statistics is also included. R-Squared indicates the amount of variation in the dependent variable that is explained by the selected independent variables, while adjusted R-squared (preferred) accounts for the number of independent variables selected. A higher adjsuted R-squared values is preferred. </li>
                                                <li> All coefficients in the model are different from zero at 90% confidence interval (CI) if sig value (i.e. p-value) is <0.1 (95% CI: p-value <0.05 and 99% CI: p-value <0.01) </li></ul>"))})
    
    # ----- Model Fit Assessment ----- #
    
    # Hausman Test
    output$plmhauslabel <- renderUI({HTML(paste("<b>", "Hausman Test", "</b>"))})
    output$plmhausdesc <- renderUI({HTML(paste("To determine if ", "<u>", "fixed effect or random effect", "</u>", " is more appropriate"))})
    output$plmhausTestVars <- renderText({paste(input$depVar, "~", paste(input$indepVars, collapse = " + ")) })
    
    plm_FE <- reactive({
        ghg_data <- ghg()
        plm(reformulate(input$indepVars, input$depVar), data = ghg_data, index = c("Country", "Year"), model = "within", effect = input$plmEffect)
    })
    plm_RE <- reactive({
        ghg_data <- ghg()
        plm(reformulate(input$indepVars, input$depVar), data = ghg_data, index = c("Country", "Year"), model = "random", effect = input$plmEffect)
    })
    
    output$plmhausTest <- renderPrint({
        phtest(plm_FE(), plm_RE())
    })
    
    output$plmhausinterpret <- renderUI({HTML(paste("<b>", "How to interpret the output?", "</b><br>", 
                                                    "Fixed effect is more appropriate than random effect if p-value is <0.1 (95% CI: p-value <0.05 and 99% CI: p-value <0.01)"))})
    
    # Chow Test
    output$plmchowlabel <- renderUI({
        input$goButtonPanel
        
        isolate({
            if (input$plmModel == "within") {
                HTML(paste("<b>", "Chow Test of Poolability", "</b>"))
            }
        })
    })
    
    output$plmchowdesc <- renderUI({
        input$goButtonPanel
        
        isolate({
            if (input$plmModel == "within") {
                HTML(paste("To determine if ", "<u>", "pooled effect or fixed effect", "</u>", " is more appropriate <br>", 
                           "Note: Due to insufficient number of observations in the current dataset, the maximum number of x variables that can be included is 6 variables."))
            }
        })
    })
    
    output$plmchowTestVars <- renderText({
        input$goButtonPanel
        
        isolate({
            if (input$plmModel == "within") {
                paste(input$depVar, paste(input$indepVars, collapse = " + "), sep = " ~ ")
            }
        })
    })
    
    output$plmchowTest <- renderPrint({
        input$goButtonPanel
        
        isolate({
            if (input$plmModel == "within") {
                pooltest(reformulate(input$indepVars, input$depVar), data = ghg(), index = c("Country", "Year"), model = "within") 
            }
        })
    })
    
    output$plmchowinterpret <- renderUI({
        input$goButtonPanel
        
        isolate({
            if (input$plmModel == "within") {
                HTML(paste("<b>", "How to interpret the output?", "</b><br>", 
                           "Fixed effect is more appropriate than pooled effect if p-value is <0.1 (95% CI: p-value <0.05 and 99% CI: p-value <0.01)"))
            }
        })
    })
    
    # Lagrange Multiplier Test
    output$plmLMlabel <- renderUI({
        input$goButtonPanel
        
        isolate({
            if (input$plmModel == "random") {
                HTML(paste("<b>", "Lagrange Multiplier Test", "</b>"))
            }
        })
    })
    
    output$plmLMdesc <- renderUI({
        input$goButtonPanel
        
        isolate({
            if (input$plmModel == "random") {
                HTML(paste("To determine if ", "<u>", "pooled effect or random effect", "</u>", " is more appropriate"))
            }
        })
    })
    
    output$plmLMVars <- renderText({
        input$goButtonPanel
        
        isolate({
            if (input$plmModel == "random") {
                paste(input$depVar, "~", paste(input$indepVars, collapse = " + ")) 
            }
        })
    })
    
    plm_pooled <- reactive({
        ghg_data <- ghg()
        plm(reformulate(input$indepVars, input$depVar), data = ghg_data, index = c("Country", "Year"), model = "pooling")
    })
    
    output$plmLMTest <- renderPrint({
        input$goButtonPanel
        
        isolate({
            if (input$plmModel == "random") {
                plmtest(plm_pooled(), effect = input$plmEffect, type = input$plmLMtype)
            }
        })
    })
    
    output$plmLMinterpret <- renderUI({
        input$goButtonPanel
        
        isolate({
            if (input$plmModel == "random") {
                HTML(paste("<b>", "How to interpret the output?", "</b><br>", 
                           "Random effect is more appropriate than pooled effect if p-value is <0.1 (95% CI: p-value <0.05 and 99% CI: p-value <0.01)"))
            }
        })
    })
    
    # ----- Tests for PLM Assumptions ----- #
    
    # ----- Normality Test ----- #
    
    # Adding description for purpose of Normality Testing
    output$plmnormdesc <- renderUI({HTML(paste("<b>", "Assumption:", "</b>", "Regressions have errors that are normally distributed"))})
    
    # Creating dataframe containing plm redisuals and fitted values
    resfit_df <- reactive({
        data.frame(index = attr(resid(plm_reg()), "index"), 
                   residual = resid(plm_reg()),
                   fitted = fitted(plm_reg())) %>%
            rename(country = index.Country, year = index.Year)
    })
    
    ## Residual QQ Plot
    normqqplot <- reactive({
        ggplot(data = resfit_df(), aes(sample = residual)) +
            stat_qq(color = "cornflowerblue") +
            stat_qq_line(color = "darkslategray") +
            facet_wrap(~ country)
    })
    
    normqqInterpret <- HTML(paste("<b>", "How to interpret the plot?", "</b><br>", 
                                  "If the residual data points closely follow the straight diagonal line, the residuals are assumed to be normally distributed."))
    
    ##  Residual vs Fitted Value Plot
    normresfitplot <- reactive({
        ggplot(data = resfit_df()) + 
            geom_point(mapping = aes(x = fitted, y = residual), color = "cornflowerblue") + 
            geom_hline(yintercept = 0, color = "darkslategray")  + 
            facet_wrap(~ country)
    })
    
    normresfitInterpret <- HTML(paste("<b>", "How to interpret the plot?", "</b><br>", 
                                      "If the residual data points spread randomly around the zero line forming an approximate horizontal band and there is no visible outliers, the residuals are assumed to be normally distributed."))
    
    ## Residual Histogram
    normhistplot <- reactive({
        ggplot(data = resfit_df()) + 
            geom_histogram(mapping = aes(x = residual), fill = "cornflowerblue")  + 
            facet_wrap(~ country)
    })
    
    normhistInterpret <- HTML(paste("<b>", "How to interpret the plot?", "</b><br>", 
                                    "If the shape of the histogram is close to symmetrical (i.e. distribution follows a normal distribution curve), the residuals are assumed to be normally distributed."))
    
    # Selecting the Normality Test Plot to Output and the Respective Labels/ Interpretations
    normgraphInput <- reactive({
        switch (input$normTest,
                "Residual QQ-plot" = normqqplot(),
                "Residual vs Fitted Value Plot" = normresfitplot(),
                "Residual Histogram" = normhistplot()
        )
    })
    
    output$normlabel_1 <- renderUI({
        input$goButtonAssump
        
        isolate({
            if (input$normTest == "Residual QQ-plot") {
                HTML(paste("<b>", "Residual QQ Plot by Country", "</b>"))
            } else if (input$normTest == "Residual vs Fitted Value Plot") {
                HTML(paste("<b>", "Residual vs Fitted Value Plot by Country", "</b>"))
            } else {
                HTML(paste("<b>", "Residual Histogram by Country", "</b>"))
            }
        })
    })
    
    output$plmnormTest <- renderPlot({
        input$goButtonAssump
        
        isolate({
            normgraphInput()
        })
    })
    
    output$plmnorminterpret <- renderUI({
        input$goButtonAssump
        
        isolate({
            if (input$normTest == "Residual QQ-plot") {
                normqqInterpret
            } else if (input$normTest == "Residual vs Fitted Value Plot") {
                normresfitInterpret
            } else {
                normhistInterpret
            }
        })
    })
    
    # ----- Serial Correlation Test ----- #
    
    # Adding description for purpose of Serial Correlation Testing
    output$plmserialcordesc <- renderUI({HTML(paste("<b>", "Purpose:", "</b>", "Identify the presence of time-invariant error component, or typically the presence of idiosyncratic error terms"))})
    
    # Output variables for reference
    output$plmserialCorVars <- renderText({
        paste(input$depVar, paste(input$indepVars, collapse = " + "), sep = " ~ ")
    })
    
    # Output selected test for serial correlation
    output$plmserialcorTest <- renderPrint({
        input$goButtonAssump
        
        isolate({
            if (input$serialcorTest == "Unobserved Effect Test") {
                pwtest(reformulate(input$indepVars, input$depVar), data = ghg())     # Unobserved Effect Test
            } else if (input$serialcorTest == "Locally Robust Test") {
                pbsytest(reformulate(input$indepVars, input$depVar), data = ghg())   # Locally Robust Test
            } else {
                pbgtest(reformulate(input$indepVars, input$depVar), data = ghg())    # Breusch-Godfrey/Wooldridge Test
            }
        })
    })
    
    ## Unobserved Effect Test
    unobseffInterpret <- HTML(paste("<b>", "How to interpret the output?", "</b><br>", 
                                    "Covariance matrix of the residuals cannot be assumed to be equal to its diagonal (i.e, unobserved effect might exist) if p-value is <0.1 (95% CI: p-value <0.05 and 99% CI: p-value <0.01)"))
    ## Locally Robust Test
    localrobInterpret <- HTML(paste("<b>", "How to interpret the output?", "</b><br>", 
                                    "Idiosyncratic errors might violate normality and homoscedasticity assumption if p-value is <0.1 (95% CI: p-value <0.05 and 99% CI: p-value <0.01)"))
    ## Breusch-Godfrey/Wooldridge Test
    BGWTInterpret <- HTML(paste("<b>", "How to interpret the output?", "</b><br>", 
                                "Serial correlation  in idiosyncratic errors might exist if p-value is <0.1 (95% CI: p-value <0.05 and 99% CI: p-value <0.01)"))
    
    output$plmserialcorinterpret <- renderUI({
        input$goButtonAssump
        
        isolate({
            if (input$serialcorTest == "Unobserved Effect Test") {
                unobseffInterpret
            } else if (input$serialcorTest == "Locally Robust Test") {
                localrobInterpret
            } else {
                BGWTInterpret
            }
        })
    })
    
    # ----- Heteroscedasticity Test ----- #
    
    # Adding description for purpose of Serial Correlation Testing
    output$plmheteroskdesc <- renderUI({HTML(paste("<b>", "Assumption:", "</b>", "Homoscedasticity exists among the residuals (i.e. all residuals are drawn from a population with a constant variance)"))})
    
    # Output variables for reference
    output$plmheteroskVars <- renderText({
        input$goButtonAssump
        
        isolate({
            if (input$heteroskTest == "Breusch-Pagan Test") {
                paste(input$depVar, " ~ ", paste(input$indepVars, collapse = " + "), " + factor(Country)") 
            }
        })
    })
    
    # Breusch-Pagan Test for Heteroscedasticity
    output$plmheteroskTest <- renderPrint({
        input$goButtonAssump
        
        isolate({
            if (input$heteroskTest == "Breusch-Pagan Test") {
                bptest(formula(paste(input$depVar, " ~ ", paste(input$indepVars, sep = " + "), " + factor(Country)")), data = ghg(), studentize = FALSE)
            }
        })
    })
    BPTInterpret <- HTML(paste("<b>", "How to interpret the output?", "</b><br>", 
                               "Heteroscedasticity does not exist if p-value is <0.1 (95% CI: p-value <0.05 and 99% CI: p-value <0.01)"))
    
    # Residual vs Fitted Value Plot
    output$plmheteroskPlot <- renderPlot({
        input$goButtonAssump
        
        isolate({
            if (input$heteroskTest == "Residual vs Fitted Value Plot") {
                ggplot(data = resfit_df()) +
                    geom_point(mapping = aes(x = fitted, y = residual), color = "cornflowerblue") + 
                    geom_hline(yintercept = 0, color = "darkslategray") + 
                    ggtitle("Residual vs Fitted Value Plot") + 
                    theme(plot.title = element_text(size = 15, face = "bold"))}
        })
    })
    HetResFitInterpret <- HTML(paste("<b>", "How to interpret the plot?", "</b><br>", 
                                     "If the plot resembles a cone-shaped pattern (i.e. variance of the residuals increases as the fitted values increases), it indicates the presence of heteroskedasticity"))
    
    output$plmheteroskinterpret1 <- renderUI({
        input$goButtonAssump
        
        isolate({
            if (input$heteroskTest == "Breusch-Pagan Test") {
                BPTInterpret}
        })
    })
    
    output$plmheteroskinterpret2 <- renderUI({
        input$goButtonAssump
        
        isolate({
            if (input$heteroskTest == "Residual vs Fitted Value Plot") {
                HetResFitInterpret}
        })
    })
    
} 

# Run the application 
shinyApp(ui = ui, server = server)
