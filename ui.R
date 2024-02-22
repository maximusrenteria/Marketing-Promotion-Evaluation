#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(DT)
library(ggplot2)

# Define UI for application that draws a histogram
navbarPage("Marketing Promotion Analysis",

  
  tabPanel(("Data Visualization"),
           tabsetPanel(
             tabPanel("Data Set", DT::dataTableOutput("mytable")),
             tabPanel("Sales Distribution", plotOutput("plot1")),
             tabPanel("Market Size by Promotion", plotOutput("plot2")),
             tabPanel("Store Age Distribution", plotOutput("plot3"))
           )
           ),


  tabPanel("A/B Testing",
           tabsetPanel(
             tabPanel("ANOVA",
                      tabsetPanel(
                        tabPanel("Promotion Sales Variance", h3("Sales by Promotion Box Plot"), plotOutput("plot4"), h3("Sales by Promotion Dot Plot"), plotOutput("plot5")),
                        tabPanel("Levene's Test", verbatimTextOutput("LevenesTest")),
                        tabPanel("ANOVA Test", h3("One Way Method"), verbatimTextOutput("anovaowm"), h3("AOV"), verbatimTextOutput("anovaaov"), verbatimTextOutput("aovreport"))
                      )),
             tabPanel("T-test", h3("T-Test of Promo 1 and Promo 2"), verbatimTextOutput("t_test12"), 
                      h3("T-Test of Promo 2 and Promo 3"), verbatimTextOutput("t_test23"),
                      h3("T-Test of Promo 1 and Promo 3"), verbatimTextOutput("t_test13"), 
                      h3("Conclusion"), textOutput("conclusion"))
           )),
  
  
  tabPanel("Promotion Performance",
           tabsetPanel(
             tabPanel("Linear Regression", h3("Sales by Market Size, Age and Promotion"), verbatimTextOutput("regression")),
             tabPanel("Plots",
                      tabsetPanel(
                        tabPanel("by Promotion and Market Size",h3("Promotion 1"), plotOutput("plot6"), h3("Promotion 2"), plotOutput("plot7"), h3("Promotion 3"), plotOutput("plot8")),
                        tabPanel("by Promotion and Market ID/Location", h3("Promotion 1"), plotOutput("plot9"), h3("Promotion 2"), plotOutput("plot10"), h3("Promotion 3"), plotOutput("plot11")))
           ))
  )



  
)

