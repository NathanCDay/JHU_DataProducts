library(markdown)
library(shiny)


# Define UI for application that draws a histogram
shinyUI(navbarPage(title = "Dark_Nate",
                   tabPanel("Capital_Crime", icon = icon("gavel"),
                   sidebarLayout(
                       sidebarPanel(
                           h3("Controls:"),
                           selectizeInput("offense","Offense",
                                              choices = list("Choose 1 or more ..." = "", "HOMICIDE","SEX ABUSE","BURGLARY","ROBBERY","ASSAULT W/DANGEROUS WEAPON","ARSON"),
                                              multiple = T),
                           selectizeInput("weapon", "Weapon",
                                          choices = list("Choose 1 or more ..." = "", "KNIFE","OTHERS","GUN" ),
                                          multiple = T),
                           actionButton("crime_update", "Update", 
                                        icon = icon("globe"))),
                       mainPanel(
                           tabsetPanel(
                               tabPanel("Raw",
                                        h3("Murder Locations:"),
                                        leafletOutput("crime_raw")
                               ),
                               tabPanel("Clustered",
                                        h3("Murder Locations:"),
                                        leafletOutput("crime_cluster")
                               ),
                               tabPanel("Icons",
                                        h3("Murder Locations:"),
                                        leafletOutput("crime_icon")
                               )
                           )
                       )
                   )
                   ),
                   tabPanel("Bourbon", icon = icon("beer"),
                            sidebarLayout(
                                sidebarPanel(
                                    checkboxGroupInput("distilleries", "Distilleries",
                                                       choices = list("Buffalo Trace","Barton","Jim Beam","Maker's Mark",
                                                                      "Woodford Reserve","Bulleit","Jack Daniel's",
                                                                      "Heaven Hill","Four Roses","George Dickel","LDI (MGP)",
                                                                      "Wild Turkey","Willet")),
                                    actionButton("bourbon_update", "Distill")
                                ),
                            mainPanel(
                                h3("Distillery Locations:"),
                                leafletOutput("bourbon_map")
                                )
                            )
                    ),
                   tabPanel("Future_Work", icon = icon("calendar"),
                            mainPanel(
                                h3("Place Holder")
                            )),
                   theme = "yeti_bs.min.css"
))
