library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(shinythemes)
library(plotly)

navbarPage(theme = shinytheme("cerulean"),"Power analysis for log linear growth models",
           tabPanel("Two levels: measurements in clients",
                    fluidRow(
                      column(width=3,
                             box(title = "Measurement occasions", width = NULL, solidHeader = FALSE, status = "primary",
                                 textInput('timepoints', 'Enter time points (in increasing order and comma separated)', "0,35,91,365")
                             ),
                             
                             box(title="Sample size",width=NULL,solidHeader = FALSE, status = "primary",
                                 sliderInput("N", "Specify minimum and maximum number of clients", min = 2, max = 2000, step = 2, round = FALSE, value = c(2,500))
                             ),
                             box(
                               title = "Type I error rate and type of test", width = NULL, solidHeader = FALSE, status = "primary",
                               numericInput("alpha", "Type I error rate (alpha)", 0.05, min = 0, max = 1),
                               selectInput("testtype", label = "Type of test", 
                                           choices = list("One-sided" = 1, "Two-sided" = 2), 
                                           selected = 1))
                      )             ,
                      column(width=3,
                             box(
                               title = "User-specified a priori values of regression coefficients", width = NULL, solidHeader = FALSE, status = "primary",
                               
                               span(HTML(
                                 "<b>Give a priori estimate of mean baseline score in control condition</b>"),
                                 div(style = "display:inline-block;",
                                     title = "User-specified a priori estimate for the mean score at time point zero in the control condition. An estimate may follow from the literature, expert opinion or expectations.",
                                     icon("info-circle")))
                               ,
                               numericInput("beta0.c", "", value=69.9,label=NULL),
                               
                               span(HTML(
                                 "<b>Give a priori estimate of mean baseline score in intervention condition</b>"),
                                 div(style = "display:inline-block;",
                                     title = "User-specified a priori estimate for the mean score at time point zero in the intervention condition. An estimate may follow from the literature, expert opinion or expectations.",
                                     icon("info-circle")))
                               ,
                               numericInput("beta0.i", "", value=71.9,label=NULL),
                               
                               
                               span(HTML(
                                 "<b>Give a priori estimate of mean growth rate in control condition</b>"),
                                 div(style = "display:inline-block;",
                                     title = "User-specified a priori estimate for the mean growth rate in the control condition. An estimate may follow from the literature, expert opinion or expectations.",
                                     icon("info-circle")))
                               ,
                               numericInput("beta1.c", "", value=-7.7,label=NULL),
                               
                               span(HTML(
                                 "<b>Give a priori estimate of mean growth rate in intervention condition</b>"),
                                 div(style = "display:inline-block;",
                                     title = "User-specified a priori estimate for the mean growth rate in the intervention condition. An estimate may follow from the literature, expert opinion or expectations.",
                                     icon("info-circle")))
                               ,
                               numericInput("beta1.i", "", value=-9.7,label=NULL)
                             ),
                             box(title="Mean response curves for control and intervention conditions", width = NULL, solidHeader = FALSE, status = "primary",
                                 plotOutput("responsecurves")
                             )
                      ),
                      column(width=3,
                             box(
                               title = "User-specified a priori values of (co-)variance components", width = NULL, solidHeader = FALSE, status = "primary",
                               span(HTML(
                                 "<b>Give a priori estimate of residual variance</b>"),
                                 div(style = "display:inline-block;",
                                     title = "User-specified a priori estimate for the variability in residual scores. An estimate may follow from the literature, expert opinion or expectations.",
                                     icon("info-circle")))
                               ,
                               numericInput("var.e", "", value=172.2,label=NULL),
                               
                               span(HTML(
                                 "<b>Give a priori estimate of variance in baseline scores</b>"),
                                 div(style = "display:inline-block;",
                                     title = "User-specified a priori estimate for the variability in scores at time point zero. An estimate may follow from the literature, expert opinion or expectations.",
                                     icon("info-circle")))
                               ,
                               numericInput("var.u0", "", value=237.1,label=NULL),
                               
                               
                               span(HTML(
                                 "<b>Give a priori estimate of variance in growth rate</b>"),
                                 div(style = "display:inline-block;",
                                     title = "User-specified a priori estimate for the variability in growth rates. An estimate may follow from the literature, expert opinion or expectations.",
                                     icon("info-circle")))
                               ,
                               numericInput("var.u1", "", value=14.6,label=NULL),
                               
                               span(HTML(
                                 "<b>Give a priori estimate of covariance between baseline scores and growth rate</b>"),
                                 div(style = "display:inline-block;",
                                     title = "User-specified a priori estimate for the association between scores at time point zero and growth rate. An estimate may follow from the literature, expert opinion or expectations.",
                                     icon("info-circle")))
                               ,
                               numericInput("covar.u01", "", value=18.3,label=NULL)
                             ),
                             box(title="Mean response curves and 95% intervals for control and intervention conditions", width = NULL, solidHeader = FALSE, status = "primary",
                                 plotOutput("boundarycurves")
                             )
                      ),
                      column(width=3,
                             h3('Power for time*treatment interaction'),
                             tabBox(
                               title = " ",width=NULL,  height = 550,
                               id = "tabset1", 
                               tabPanel("Graph", 
                                        plotOutput("powerplot")
                               ),
                               tabPanel("Table", 
                                        DT::dataTableOutput(outputId = "ResultsTable")
                               )
                             )),
                    )
           ),
           
           tabPanel("Three levels: measurements in clients in therapists",
                    fluidRow(
                      column(width=3,
                             box(title = "Measurement occasions", width = NULL, solidHeader = FALSE, status = "primary",
                                 textInput('timepoints3L', 'Enter time points (in increasing order and comma separated)', "0,35,91,365")
                             ),
                             
                             box(title="Sample size",width=NULL,solidHeader = FALSE, status = "primary",
                                 numericInput("K3L", "Give number of therapists", 54),
                                 sliderInput("N3L", "Specify minimum and maximum number of clients per therapist", min = 2, max = 50, step = 1, round = FALSE, value = c(2,25))
                             ),
                             box(
                               title = "Type I error rate and type of test", width = NULL, solidHeader = FALSE, status = "primary",
                               numericInput("alpha3L", "Type I error rate (alpha)", 0.05, min = 0, max = 1),
                               selectInput("testtype3L", label = "Type of test", 
                                           choices = list("One-sided" = 1, "Two-sided" = 2), 
                                           selected = 1))
                      )             ,
                      column(width=3,
                             box(
                               title = "User-specified a priori values of regression coefficients", width = NULL, solidHeader = FALSE, status = "primary",
                               span(HTML(
                                 "<b>Give a priori estimate of mean baseline score in control condition</b>"),
                                 div(style = "display:inline-block;",
                                     title = "User-specified a priori estimate for the mean score at time point zero in the control condition. An estimate may follow from the literature, expert opinion or expectations.",
                                     icon("info-circle")))
                               ,
                               numericInput("beta0.c3L", "", value=69.9,label=NULL),
                               
                               span(HTML(
                                 "<b>Give a priori estimate of mean baseline score in intervention condition</b>"),
                                 div(style = "display:inline-block;",
                                     title = "User-specified a priori estimate for the mean score at time point zero in the intervention condition. An estimate may follow from the literature, expert opinion or expectations.",
                                     icon("info-circle")))
                               ,
                               numericInput("beta0.i3L", "", value=71.9,label=NULL),
                               
                               
                               span(HTML(
                                 "<b>Give a priori estimate of mean growth rate in control condition</b>"),
                                 div(style = "display:inline-block;",
                                     title = "User-specified a priori estimate for the mean growth rate in the control condition. An estimate may follow from the literature, expert opinion or expectations.",
                                     icon("info-circle")))
                               ,
                               numericInput("beta1.c3L", "", value=-7.7,label=NULL),
                               
                               span(HTML(
                                 "<b>Give a priori estimate of mean growth rate in intervention condition</b>"),
                                 div(style = "display:inline-block;",
                                     title = "User-specified a priori estimate for the mean growth rate in the intervention condition. An estimate may follow from the literature, expert opinion or expectations.",
                                     icon("info-circle")))
                               ,
                               numericInput("beta1.i3L", "", value=-9.7,label=NULL)
                             ),
                             box(title="Mean response curves for control and intervention conditions", width = NULL, solidHeader = FALSE, status = "primary",
                                 plotOutput("responsecurves3L")
                             )
                      ),
                      column(width=3,
                             box(
                               title = "User-specified a priori values of (co-)variance components", width = NULL, solidHeader = FALSE, status = "primary",
                               
                               
                               
                               span(HTML(
                                 "<b>Give a priori estimate of residual variance</b>"),
                                 div(style = "display:inline-block;",
                                     title = "User-specified a priori estimate for the variability in residual scores. An estimate may follow from the literature, expert opinion or expectations.",
                                     icon("info-circle")))
                               ,
                               numericInput("var.e3L", "", value=172.1,label=NULL),
                               
                               span(HTML(
                                 "<b>Give a priori estimate of variance at client level in baseline scores</b>"),
                                 div(style = "display:inline-block;",
                                     title = "User-specified a priori estimate for the between-client variability in scores at time point zero. An estimate may follow from the literature, expert opinion or expectations.",
                                     icon("info-circle")))
                               ,
                               numericInput("var.u03L", "", value=237.1,label=NULL),
                               
                               
                               span(HTML(
                                 "<b>Give a priori estimate of variance at client level in growth rate</b>"),
                                 div(style = "display:inline-block;",
                                     title = "User-specified a priori estimate for the between-client variability in growth rates. An estimate may follow from the literature, expert opinion or expectations.",
                                     icon("info-circle")))
                               ,
                               numericInput("var.u13L", "", value=13.2,label=NULL),
                               
                               span(HTML(
                                 "<b>Give a priori estimate of covariance at client level between baseline scores and growth rate</b>"),
                                 div(style = "display:inline-block;",
                                     title = "User-specified a priori estimate for the client-level association between scores at time point zero and growth rate. An estimate may follow from the literature, expert opinion or expectations.",
                                     icon("info-circle")))
                               ,
                               numericInput("covar.u013L", "", value=18.2,label=NULL),
                               
                               
                               span(HTML(
                                 "<b>Give a priori estimate of variance at therapist level in baseline scores</b>"),
                                 div(style = "display:inline-block;",
                                     title = "User-specified a priori estimate for the between-therapist variability in scores at time point zero. An estimate may follow from the literature, expert opinion or expectations.",
                                     icon("info-circle")))
                               ,
                               numericInput("var.v03L", "", value=0,label=NULL),
                               
                               
                               span(HTML(
                                 "<b>Give a priori estimate of variance at therapist level in growth rate</b>"),
                                 div(style = "display:inline-block;",
                                     title = "User-specified a priori estimate for the between-therapist variability in growth rates. An estimate may follow from the literature, expert opinion or expectations.",
                                     icon("info-circle")))
                               ,
                               numericInput("var.v13L", "", value=1.4,label=NULL),
                               
                               span(HTML(
                                 "<b>Give a priori estimate of covariance at therapist level between baseline scores and growth rate</b>"),
                                 div(style = "display:inline-block;",
                                     title = "User-specified a priori estimate for the client-level association between scores at time point zero and growth rate. An estimate may follow from the literature, expert opinion or expectations.",
                                     icon("info-circle")))
                               ,
                               numericInput("covar.v013L", "", value=0,label=NULL)
                               
                               
                       
                             ),
                             box(title="Mean response curves and 95% intervals for control and intervention conditions", width = NULL, solidHeader = FALSE, status = "primary",
                                 plotOutput("boundarycurves3L")
                             )
                      ),
                      column(width=3,
                             h3('Power for time*treatment interaction'),
                             tabBox(
                               title = " ",width=NULL,  height = 550,
                               id = "tabset1", 
                               tabPanel("Graph", 
                                        plotOutput("powerplot3L")
                               ),
                               tabPanel("Table", 
                                        DT::dataTableOutput(outputId = "ResultsTable3L")
                                        )
                               )
                             )),
                    )
           )
           
