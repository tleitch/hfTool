library(shinydashboard)
library(DT)
library(shiny)
library(shinyWidgets)
library(rhandsontable)
library(quantmod)
library(PortfolioAnalytics)
library(ROI)
require(ROI.plugin.quadprog)
require(ROI.plugin.glpk)
library(Rglpk)
library(DEoptim)
library(quantmod)
library(downloader)
library(ahf)


shinyUI(dashboardPage(skin = "black" , 
                      dashboardHeader(title = "Portfolio Allocation Demo"),
                      
                      dashboardSidebar(
                        sidebarUserPanel("", img(src="carey.png",width="80%")),
                        br(),
                        sidebarMenu(
                          # menuItem("About", tabName = "about", icon = icon("book")),
                          menuItem("Section 1", tabName = "sec1", icon = icon("line-chart")),
                          menuItem("Section 2", tabName = "sec2", icon = icon("chart-pie")),
                          menuItem("Section 3", tabName = "sec3", icon = icon("pen-fancy")),
                          menuItem("Section 4", tabName = "sec4", icon = icon("flask")),
                          menuItem("Section 5", tabName = "sec5", icon = icon("chess-board")),
                          menuItem("Disclaimer", tabName = "discl", icon = icon("book"))
                          
                        )
                      ),
                      
                      dashboardBody(
                        tabItems(
                          
                          
                          ####ABOUT PAGE
                          tabItem(tabName = "about",
                                  fluidRow(column(12,h2("About the Application"))),
                                  
                                  fluidRow(column(12,
                                                  div(br(),br(),
                                                      p("Decription for this application if needed")
                                                  )))
                          ),
                          
                          ##### Legal Disclaimer Page 
                          # tabItem(tabName = "discl", div(htmlOutput("disclaimer"))),
                          tabItem(tabName = "discl",
                                  fluidRow(column(12,h2("Disclaimer"))),
                                  
                                  fluidRow(column(12,
                                                  div(br(),br(),
                                                      p("Disclaimer if needed")
                                                  )))
                          ),
                          
                          #### Section 1
                          tabItem(tabName = "sec1",
                                  headerPanel("Calculation of Performance Measures"),
                                  sidebarLayout(
                                    sidebarPanel(
                                      checkboxGroupInput("show_vars", "Columns:",
                                                         names(edhec2), selected = names(edhec2)[1:6]),
                                      sliderInput("year",
                                                  "Select Viewing Years:",
                                                  min = 1997,  max = 2019, value = c(2004,2012)),
                                      
                                      selectInput("criteria", "Assessment Criteria:",
                                                  choices = c("Sharpe Ratio", "Sortino Ratio", "CAPM beta","Kurtosis",
                                                              "Skewness","Calmar Ratio","Jensen’s alpha","Omega ratio","Bull beta","Bear beta"))
                                    ),
                                    mainPanel(
                                      tabsetPanel(
                                        id = 'dataset',
                                        tabPanel("edhec2", DT::dataTableOutput("mytable1"))
                                      ),
                                      
                                      h4("Criteria Values"),
                                      verbatimTextOutput("cvalues")
                                    )
                                  )
                          ),
                          
                          
                          #### Section 2
                          tabItem(tabName = "sec2",
                                  fluidRow(column(12, h3("Rule Based Trading: Mean reversion"), align = "center")),
                                  br(),br(),
                                  # fluidRow(column(12, verbatimTextOutput("tttest"))
                                  # ),
                                  # fluidRow(column(12, verbatimTextOutput("tttest2"))
                                  # ),
                                  br(),

                                  
                                  br()
                          ),
                          
                          
                          #### Section 3
                          tabItem(tabName = "sec3",
                                  fluidRow(column(12, h3("Factor Investing : Simple Factors "), align = "center")),
                                  br(),br(),
                                  fluidRow(
                                    # column(1),
                                    column(12, h4("Construct your portfolio:"), align = "left")),
                                  br(),
                                  fluidRow(
                                    # column(1),
                                    column(4, br(), br(), br(),
                                           wellPanel(div(rHandsontableOutput("asset_sec3"))
                                           )
                                    ),
                                    column(4, DTOutput('factors_sec3')),
                                    column(4,
                                           h4("Some descriptions"),
                                           br(),
                                           p("All tickers from Yahoo finance and FRED are valid."),
                                           br(),
                                           p("fast and slow SMA are set to....."),
                                           br(),
                                           p("stop loss for long and short position are set to..."),
                                           br(),
                                           p("Click the botton below to fetch real-time data:"),
                                           br(),
                                           actionBttn("fetch_sec3", label = "FetchData", color = "primary"),
                                           actionBttn("backtest_go3", label = "Backtest", color = "primary")
                                           
                                    )
                                  ),
                                  fluidRow(column(3, pickerInput(inputId = "model_sec3",
                                                                 label = "Select a pre-set model",
                                                                 choices = c("User Define", "CAPM", "FF3"))
                                  ),
                                  
                                  column(3, textAreaInput("leverage",
                                                          "Set the leverage ratio:",
                                                          "0.99,2",
                                                          height = "40px",
                                                          resize = "none"))
                                  # column(3, awesomeCheckbox(inputId = "hide_sec3",
                                  #                           label = "Hide unselected factors", 
                                  #                           value = FALSE
                                  # ))
                                  ),
                                  fluidRow(column(12,
                                                  # verbatimTextOutput("tttest"),
                                                  div(sliderTextInput(
                                                    inputId = "date_range_sec3", label = h4("Time interval:"), width = "80%",
                                                    choices = date_choices, selected = range(date_choices),
                                                    grid = TRUE, dragRange = FALSE
                                                  ), align = "center"))
                                  ),
                                  # fluidRow(column(12, DTOutput('factors_sec3'))),
                                  # fluidRow(column(12, verbatimTextOutput("tttest"))
                                  # ),
                                  # fluidRow(column(12, verbatimTextOutput("tttest2"))
                                  # ),
                                  
                                  fluidRow(column(12, plotOutput("bt_sec3"))
                                  ),
                                  # br(), br(),
                                  # fluidRow(
                                  #   # column(1),
                                  #   column(12, h4("Performance analysis for the entire portfolio and each asset inside:"), align = "left")),
                                  # br(),
                                  # fluidRow(column(12, rHandsontableOutput("performance_sec3"), align = "left")
                                  # ),
                                  # br(), br(),
                                  # fluidRow(
                                  #   # column(1),
                                  #   column(12, h4("Performance analysis for the entire portfolio and each asset inside:"), align = "left")),
                                  # br(),
                                  # fluidRow(column(12, rHandsontableOutput("performance_sec1"), align = "left")
                                  # ),
                                  
                                  br()
                          ),
                          
                          
                          
                          #### Section 4
                          
                          tabItem(tabName = "sec4",
                                  fluidRow(column(12, h3("Factor Investing: Factors with Momentum"), align = "center")),
                                  br(),br(),
                                  fluidRow(column(3, textAreaInput("back_4",
                                                                   "Set the performance calculation period:",
                                                                   "60",
                                                                   height = "40px",
                                                                   resize = "none")),
                                           column(3, textAreaInput("hold_4",
                                                                   "Set the holding period:",
                                                                   "30",
                                                                   height = "40px",
                                                                   resize = "none")),
                                           column(3, textAreaInput("num_4",
                                                                   "Set how many factors to be held:",
                                                                   "3",
                                                                   height = "40px",
                                                                   resize = "none")),
                                           column(3, actionBttn("backtest_go4", label = "Backtest", color = "primary"))
                                           ),
                                  fluidRow(column(12,
                                                  # verbatimTextOutput("tttest"),
                                                  div(sliderTextInput(
                                                    inputId = "date_range_sec4", label = h4("Time interval:"), width = "80%",
                                                    choices = date_choices, selected = range(date_choices),
                                                    grid = TRUE, dragRange = FALSE
                                                  ), align = "center"))
                                  ),
                                  
                                  # fluidRow(column(12, verbatimTextOutput("tttest"))
                                  # ),
                                  
                                  br(),
                                  fluidRow(column(12, plotOutput("bt_sec4"))
                                  ),
                                  
                                  br()
                          ),
                          
                          
                          #### Section 5
                          tabItem(tabName = "sec5",
                                  fluidRow(column(12, h3("Portfolio Analaytics"), align = "center")),
                                  br(),br(),
                                  # fluidRow(column(12, verbatimTextOutput("tttest"))
                                  # ),
                                  # fluidRow(column(12, verbatimTextOutput("tttest2"))
                                  # ),
                                  br(),br(),br(),br(),
                                  fluidRow(column(2),
                                           column(10, rHandsontableOutput("performance_sec5"), align = "left", style = "font-size:100%")
                                  ),
                                  # test
                                  br()
                          )
                          )
                        )
                      )
        )
