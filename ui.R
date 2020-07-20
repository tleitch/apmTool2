library(shinydashboard)
library(DT)
library(shiny)
library(shinyWidgets)
library(rhandsontable)
library(quantmod)

shinyUI(dashboardPage(skin = "black" ,
                      dashboardHeader(title = "Portfolio Allocation Demo"),
                      
                      dashboardSidebar(
                        sidebarUserPanel("", img(src="carey.png",width="80%")),
                        br(),
                        sidebarMenu(
                          menuItem("About", tabName = "about", icon = icon("book")),
                          menuItem("Theory", tabName = "theory", icon = icon("graduation-cap"),
                                   menuSubItem("Risk/Return Ratio", tabName = "theory_1"),
                                   menuSubItem("Optimal Portfolio", tabName = "theory_2"),
                                   menuSubItem("Performance Measures", tabName = "theory_3")
                          ),
                          menuItem("Backtest", tabName = "backtest", icon = icon("line-chart"),
                                   menuSubItem("Your Allocation", tabName = "user_port"),
                                   menuSubItem("Allocation Comparison", tabName = "opt_port"),
                                   menuSubItem("ALM Comparison Simulation", tabName = "sim_port")
                                   
                          ),
                          menuItem("Disclaimers", tabName = "discl", icon = icon("exclamation-triangle")))
                        
                      ),
                      
                      dashboardBody(
                        tabItems(
                          
                          
                          ####ABOUT PAGE
                          # tabItem(tabName = "about", fluidRow(column(6, htmlOutput("abt")))),
                          
                          ####ABOUT PAGE
                          tabItem(tabName = "about",
                                  fluidRow(column(3,h2("About the Application"))),
                                  
                                  fluidRow(column(6,
                                                  div(br(),br(),
                                                      p("This Shiny App was developed for the Advanced Portfolio Management course from Carey Business School of Johns Hopkins University."),
                                                      p("The application illustrates the key principles of portfolio optimization."),
                                                      p("In Theory section we talk about diversification and portfolio composition. Also, we introduce key performance measures that are later used in our backtesting."),
                                                      p("Backtesting section allows user to choose a portfolio comprised of up to 10 assets from quantmod package as well as to choose a desired rebalancing schedule. The resulting portfolio is compared to S&P500 performance and the performance of the portfolio consisting of 60% S&P500, 10% Treasury Bonds, and 30% Corporate Bonds as a proxy of typical 60/40 portfolio."),
                                                      p("The user can select a date range for which the backtesting is performed (don't forget to press Backtest button). On Allocation Comparison tab the user portfolio is compared to two optimal portfolios for the same date range: a portfolio with the same return and lower risk, and a portfolio with the same risk and higher return."),
                                                      p("Please be informed that information in this application is provided for illustrative purposes only and does not constitute a financial advice. For more information please see the Disclaimer."),
                                                      # br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                                                      p("The author of the first version of this application is Dr. Mikhail Stukalo, who has over 15 years of experience in financial markets."),
                                                      p("The author of current version is Qiang Sheng, who has solid background for quantitative finance, machine learning and algorithmic trading.")
                                                  )))
                          ),
                          
                          ##### Legal Disclaimer Page
                          tabItem(tabName = "discl", div(htmlOutput("disclaimer"))),
                          
                          
                          ####Risk/Return Page
                          tabItem(tabName = "theory_1",
                                  fluidPage(h1("Risk/Return Ratio"),
                                            p("In 1952 Harry Markowitz suggested that assets should be evaluated based on their risk/return ratio.
                                  For the purposes of this app, I look at the asset returns measured by corresponding indices in 1Q2000
                                 - 2Q2020. "),
                                            p("The assets are:"),
                                            p(em("Equities:")),
                                            tags$div(tags$ul(
                                              tags$li("S&P 500"),
                                              tags$li("MSCI Europian Stock Index"),
                                              tags$li("MSCI Emerging Market Stock Index"))
                                            ),
                                            p(em("Bonds:")),
                                            tags$div(tags$ul(
                                              tags$li("Barclays US Treasury Total Return Index"),
                                              tags$li("Barclays US Corporate Bonds Total Return Index")
                                            )
                                            ),
                                            p(em("Real Estate:")),
                                            tags$div(tags$ul(
                                              tags$li("Dow Jones Real Estate Index"))
                                            ),
                                            tabsetPanel(
                                              tabPanel("Whole Period", br(), plotlyOutput("graph1")),
                                              tabPanel("By Years",  plotlyOutput("graph2")),
                                              tabPanel("Compound Return",  plotlyOutput("graph3"))
                                            )
                                  )
                          ),
                          
                          #####Optimal potrfolio page
                          
                          tabItem(tabName = "theory_2",
                                  fluidPage(fluidRow(
                                    column(6,h1("Optimal portfolio"),
                                           p("Asset returns are not perferctly correlated. Therefore, we can combine assets into portfolios, and harverst
                                 the results of the diversification."),
                                           p("However, diversification is not limitless. For each expected risk there will be a portfolio with
                                 a maximum achievable return.The graph below shows risk/return profiles of simulated portfolios (gray) and
                                 a line (blue) depicting portfolios offering highest return for a given risk."),
                                           p("In Harry Markowitz (1952) framework, such line is called the Efficient Frontier. However, Markowitz' theory
                                 assumes that investors hold long-short portfolios. In our analysis, we limit ourselves to long-only portfolios,
                                 as it is the type of portfolios retail investors usually hold. Therefore, we will refer to portfolios on this line as
                                 'Optimal Portfolios', and the line itself as the 'Optimal Line'."),
                                           br(),
                                           plotlyOutput("graph4")
                                    )))
                          ),
                          
                          tabItem(tabName = "theory_3",
                                  fluidRow(column(8,div(htmlOutput("measures"))))
                          ),
                          
                          
                          
                          #####  HERE IS WHERE FUN BEGINS
                          #####
                          
                          #### Your allocation Page
                          tabItem(tabName = "user_port",
                                  
                                  fluidRow(div(column(6, h4("Select Portfolio Allocation:", align = "center")),
                                               column(2, h4("Modify Expected Return:", align = "center")),
                                               column(1, h4("Select Rebalance Schedule:", align = "left")),
                                               column(3, h4("Allocation", align = "center")))
                                  ),
                                  fluidRow(div(column(1),
                                               column(1, downloadButton("downloadData", "Download")),
                                               column(1, fileInput("p1upload", NULL,
                                                                   buttonLabel = "Upload",
                                                                   multiple = TRUE,
                                                                   accept = ".csv")),
                                               column(1, switchInput(inputId = "auto", label = "AUTO", value = TRUE,
                                                                     onLabel = "ON", offLabel = "OFF", size = "mini",
                                                                     width = "100%")),
                                               column(2, align="right", h5(textOutput("currentsum")))
                                               
                                  )),
                                  fluidRow(
                                    column(1, align="left",
                                           # textAreaInput("pp1", label = NULL, "SPY", height = "40px", resize = "none"),
                                           # textAreaInput("pp2", label = NULL, "PRESX", height = "40px", resize = "none"),
                                           # textAreaInput("pp3", label = NULL, "", height = "40px", resize = "none"),
                                           # textAreaInput("pp4", label = NULL, "", height = "40px", resize = "none"),
                                           # textAreaInput("pp5", label = NULL, "", height = "40px", resize = "none"),
                                           # textAreaInput("pp6", label = NULL, "", height = "40px", resize = "none")
                                           
                                           textAreaInput("pp1", label = NULL, "SPY", height = "40px", resize = "none"),
                                           textAreaInput("pp2", label = NULL, "PRESX", height = "40px", resize = "none"),
                                           textAreaInput("pp3", label = NULL, "EEM", height = "40px", resize = "none"),
                                           textAreaInput("pp4", label = NULL, "DGS10", height = "40px", resize = "none"),
                                           textAreaInput("pp5", label = NULL, "LQD", height = "40px", resize = "none"),
                                           textAreaInput("pp6", label = NULL, "IYR", height = "40px", resize = "none")
                                    ),
                                    column(2, align="left",
                                           uiOutput("p1ui"),
                                           uiOutput("p2ui"),
                                           uiOutput("p3ui"),
                                           uiOutput("p4ui"),
                                           uiOutput("p5ui"),
                                           uiOutput("p6ui")
                                    ),
                                    column(1, align="left",
                                           # textAreaInput("pp7", label = NULL, "", height = "40px", resize = "none"),
                                           # textAreaInput("pp8", label = NULL, "", height = "40px", resize = "none"),
                                           # textAreaInput("pp9", label = NULL, "", height = "40px", resize = "none"),
                                           # textAreaInput("pp10", label = NULL, "", height = "40px", resize = "none"),
                                           # textAreaInput("pp12", label = NULL, "", height = "40px", resize = "none"),
                                           # textAreaInput("pp11", label = NULL, "", height = "40px", resize = "none")
                                           
                                           textAreaInput("pp7", label = NULL, "PSP", height = "40px", resize = "none"),
                                           textAreaInput("pp8", label = NULL, "DFGBX", height = "40px", resize = "none"),
                                           textAreaInput("pp9", label = NULL, "", height = "40px", resize = "none"),
                                           textAreaInput("pp10", label = NULL, "", height = "40px", resize = "none"),
                                           textAreaInput("pp11", label = NULL, "", height = "40px", resize = "none"),
                                           textAreaInput("pp12", label = NULL, "", height = "40px", resize = "none")
                                    ),
                                    column(2, align="left",
                                           uiOutput("p7ui"),
                                           uiOutput("p8ui"),
                                           uiOutput("p9ui"),
                                           uiOutput("p10ui"),
                                           uiOutput("p11ui"),
                                           uiOutput("p12ui")
                                    ),
                                    column(2, rHandsontableOutput("table5")),
                                    
                                    column(1, align="left",
                                           fluidRow(
                                             radioButtons(inputId="rebalance",
                                                          label=NULL,
                                                          choices=c("Monthly","Quarterly", "Annually", "Never"),
                                                          selected = "Never")),
                                           fluidRow(
                                             br(),
                                             actionBttn("update", label = "FetchData", color = "primary"),
                                             hr(),
                                             actionBttn("go", label = "Backtest", color = "primary")
                                           )),
                                    column(3,
                                           div(plotlyOutput("graph5"), align = "center", style = "height:250px"))),
                                  
                                  fluidRow(column(12,
                                                  # verbatimTextOutput("tttest"),
                                                  div(sliderTextInput(
                                                    inputId = "date_range", label = h4("Time interval:"), width = "80%",
                                                    choices = date_choices, selected = range(date_choices),
                                                    grid = TRUE, dragRange = FALSE
                                                  ), align = "center"))
                                  ),
                                  fluidRow(column(6, h4("Compound Return", align="center")),
                                           column(6, h4("Performance Measures", align="center"))),
                                  fluidRow(column(6, div(plotlyOutput("graph6"), align="center")),
                                           column(6, div(tableOutput("bt_table1"), align="center"))
                                  )
                          ),
                          
                          ####Allocation Comparison Page
                          tabItem(tabName = "opt_port",
                                  fluidRow(column(4, h4("Your Allocation", align="center")),
                                           column(4, h4("Similar Return", align="center")),
                                           column(4, h4("Similar Risk", align="center"))
                                  ),
                                  fluidRow(column(4,
                                                  br(),br(),
                                                  div(plotlyOutput("graph7"), align="center")),
                                           column(4,
                                                  br(),br(),
                                                  div(plotlyOutput("graph8"), align="center")),
                                           column(4,
                                                  br(),br(),
                                                  div(plotlyOutput("graph9"), align="center"))
                                  ),
                                  fluidRow(column(6, h4("Compound Return", align = "center")),
                                           column(6, h4("Performance Measures", align="center"))
                                  ),
                                  fluidRow(column(6, div(plotlyOutput("graph10"), allign = "center")),
                                           column(6, div(br(),tableOutput("bt_table2"), align="center"))
                                  )
                          ),
                          
                          ###ALM Comparison Page
                          tabItem(tabName = "sim_port",
                                  fluidRow(column(9, h4("Simulations", align="center")),
                                           column(3, h4("Liability Cashflow", align="center"))),
                                  
                                  fluidRow(
                                    column(9, wellPanel(
                                      fluidRow(column(3,
                                                      strong("Historical annually log return (%)"),
                                                      verbatimTextOutput("simu11")),
                                               column(3,
                                                      strong("Expected annually log return (%)"),
                                                      verbatimTextOutput("simu12")
                                                      # numericInput("simu12", label = "Expected annually log return (%)",
                                                      #                  "")
                                               ),
                                               column(1),
                                               column(1,downloadButton("p3download", "Download"), allign = "center"),
                                               column(1),
                                               column(1,fileInput("p3upload", NULL,
                                                                  buttonLabel = "Upload",
                                                                  multiple = TRUE,
                                                                  accept = ".csv"), allign = "right")
                                      ),
                                      div(plotOutput("graph11_2"), allign = "center"),
                                      br(),
                                      
                                      div(plotOutput("graph11"), allign = "center"),
                                      
                                      # fluidRow(column(3,
                                      #                 strong("Historical annually log return (%)"),
                                      #                 verbatimTextOutput("simu21")),
                                      #          column(3, numericInput("simu22", label = "Expected annually log return (%)",
                                      #                                 ""))
                                      # ),
                                      div(plotOutput("graph12"), allign = "center"),
                                      # fluidRow(column(3,
                                      #                 strong("Historical annually log return (%)"),
                                      #                 verbatimTextOutput("simu31")),
                                      #          column(3, numericInput("simu32", label = "Expected annually log return (%)",
                                      #                                 ""))
                                      # ),
                                      
                                      
                                      div(plotOutput("graph13"), allign = "center"))
                                    )
                                    
                                    ,
                                    column(3,
                                           wellPanel(
                                             div(
                                               # verbatimTextOutput("debug"),
                                               selectInput("simuWay", "Choose a scenario:",
                                                           c("default", "Recently Retired", "Pre Retired", "Couple and Young Kids", "Cook County","Custom"),
                                                           selected = "default"),
                                               actionBttn("go2", label = "Run Sim", color = "primary"),
                                               # actionBttn("getAlm", label = "Retrieve Custom ALM", color = "primary"),
                                               # actionBttn("saveAlm", label = "Save Custom ALM", color = "primary"),
                                               align = "left"),
                                             br(),
                                             div(
                                               fluidRow(column(4, rHandsontableOutput("table3"), align="left"),
                                                        column(1, rHandsontableOutput("table4"), align="center"))
                                             )
                                           )
                                    )
                                  )
                          )
                        )
                      )
)
)
