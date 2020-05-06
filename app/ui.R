#setwd('C:/AE/statistik_corey')
library(shiny)
library(shinydashboard)
library(plotly)

datum=c("05.05.2020", "04.05.2020", "03.05.2020", "02.05.2020", "01.05.2020", "30.04.2020", "29.04.2020", "28.04.2020", "27.04.2020", "26.04.2020", "25.04.2020", "24.04.2020", "23.04.2020", "22.04.2020", "21.04.2020", "20.04.2020", "19.04.2020", "18.04.2020","17.04.2020", "16.04.2020", "15.04.2020","14.04.2020", "13.04.2020", "12.04.2020", "11.04.2020", "10.04.2020", "09.04.2020", "08.04.2020", "07.04.2020", "06.04.2020")
chatbot=c("Corey", "Corey-IM", "Corey-Emmendingen", "Corey-Ravensburg", "Corey-Calw", "Corey-Esslingen", "Corey-Main-Tauber-Kreis", "Corey-Heilbronn", "Corey-Loerrach", "Corey-Reutlingen", "Corey-Heidenheim")
gesamt_an=125958
gesamt = 69977
gesamt_im=65495
gesamt_em=1624
gesamt_ra=374
gesamt_ca=1040
gesamt_ess=423
gesamt_mtk=1741
gesamt_he=332
gesamt_lo=113
gesamt_re=888
gesamt_hei=910









# Define UI for application that draws a histogram
shinyUI(dashboardPage(
    dashboardHeader(title = "Corey Statistik"),
    dashboardSidebar(sidebarMenu(id = "sbm",
                                 menuItem("Dashboard", tabName = "Dashboard", icon = icon("home")),
                                 
                                 menuItem("Allgemein", tabName = "Allgemein", icon = icon("line-chart"), startExpanded = TRUE),
                                 conditionalPanel(
                                     condition = "input.sbm == 'Allgemein'",
                                     selectInput("filterD", label = "Wählen Sie ein Datum",
                                                 choices =  datum), width = 3,
                                     selectInput("filterC", label = "Wählen Sie ein Chatbot",
                                                 choices =  chatbot), width = 3),
                                 menuItem("Feedback", tabName = "Feedback", icon = icon("pie-chart"), startExpanded = TRUE)
                                 
                                 
                     )
    ),#end of dashboardSidebar,
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "Dashboard",
                    
                    fluidRow(
                        column(width=12,
                               valueBox(gesamt_an, "Gesamtanzahl Anfragen Corey", color = "blue", width=2),
                               valueBox(gesamt, "Gesamtanzahl Besucher Corey", width=2),
                               valueBox(gesamt_im, "Gesamtanzahl Besucher Corey-IM", color = "orange", width=2),
                               valueBox(gesamt_em, "Gesamtanzahl Besucher Corey-Emmendingen", color = "green", width=2),
                               valueBox(gesamt_ra, "Gesamtanzahl Besucher Corey-Ravensburg", color = "red", width=2),
                               valueBox(gesamt_ca, "Gesamtanzahl Besucher Corey-Calw", color = "purple", width=2),
                               valueBox(gesamt_ess, "Gesamtanzahl Besucher Corey-Esslingen", color = "navy", width=2),
                               valueBox(gesamt_mtk, "Gesamtanzahl Besucher Corey-Main-Tauber-Kreis", color = "light-blue", width=2),
                               valueBox(gesamt_he, "Gesamtanzahl Besucher Corey-Heilbronn", color = "maroon", width=2),
                               valueBox(gesamt_lo, "Gesamtanzahl Besucher Corey-Lörrach", color = "fuchsia", width=2),
                               valueBox(gesamt_re, "Gesamtanzahl Besucher Corey-Reutlingen", color = "teal", width=2),
                               valueBox(gesamt_hei, "Gesamtanzahl Besucher Corey-Heidenheim", color = "aqua", width=2)
                               
                        )
                        
                    ),
                    fluidRow(
                        column(width=12,
                               box(
                                   title = "Top10 Themen",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   collapsible = TRUE,
                                   width = NULL,
                                   plotlyOutput("Plottop", height = 480)
                               )
                               
                        )
                        
                    ),
                    fluidRow(
                        column(width = 6, align = "center",
                        box(
                            title = "Statistiken-Besucheranzahl",
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            width = NULL,
                            plotlyOutput("Plotdash", height = 480)
                        )),
                        column(width = 6, align = "center",
                               box(
                                   title = "Statistiken-Antworten",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   collapsible = TRUE,
                                   width = NULL,
                                   plotlyOutput("Plotdashpos", height = 480)
                               ))
                    )
            ),
            tabItem(tabName = "Allgemein",
                    
                
                    fluidRow(
                        column(width = 6, align = "center",
                               box(
                                   title = "Allgemeiner Übersicht",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   collapsible = TRUE,
                                   width = NULL,
                                   plotlyOutput("Plotallgbar", height = 600)
                               )),
                        column(width = 6, align = "center",
                               box(
                                   title = "Statistiken-Antworten",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   collapsible = TRUE,
                                   width = NULL,
                                   plotlyOutput("Plotallgpie", height = 600)
                               ))
                    )
            ),
            tabItem(tabName = "Feedback",
                    
                    
                    fluidRow(
                        column(width = 1, align = "center"),
                        column(width = 10, align = "center",
                               box(
                                   title = "Anzahl von Feedbacks",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   collapsible = TRUE,
                                   width = NULL,
                                   plotlyOutput("Plotfeed", height = 600)
                               )),
                        column(width = 1, align = "center")
                    )
            )
            )
    )#end of dashboardBody
))
