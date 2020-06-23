#setwd('C:/AE/statistik_corey')
library(shiny)
library(shinydashboard)
library(plotly)

datum=c("22.06.2020", "21.06.2020", "20.06.2020", "19.06.2020", "18.06.2020", "17.06.2020", "16.06.2020", "15.06.2020", "14.06.2020", "13.06.2020", "12.06.2020", "11.06.2020", "10.06.2020", "09.06.2020", "08.06.2020", "07.06.2020", "06.06.2020", "05.06.2020", "04.06.2020", "03.06.2020", "02.06.2020", "01.06.2020", "31.05.2020", "30.05.2020", "29.05.2020", "28.05.2020", "27.05.2020", "26.05.2020", "25.05.2020", "24.05.2020", "23.05.2020", "22.05.2020", "21.05.2020", "20.05.2020", "19.05.2020", "18.05.2020", "17.05.2020", "16.05.2020", "15.05.2020", "14.05.2020", "13.05.2020", "12.05.2020", "11.05.2020", "10.05.2020", "09.05.2020", "08.05.2020", "07.05.2020", "06.05.2020", "05.05.2020", "04.05.2020", "03.05.2020", "02.05.2020", "01.05.2020", "30.04.2020", "29.04.2020", "28.04.2020", "27.04.2020", "26.04.2020", "25.04.2020", "24.04.2020", "23.04.2020", "22.04.2020", "21.04.2020", "20.04.2020", "19.04.2020", "18.04.2020","17.04.2020", "16.04.2020", "15.04.2020","14.04.2020", "13.04.2020", "12.04.2020", "11.04.2020", "10.04.2020", "09.04.2020", "08.04.2020", "07.04.2020", "06.04.2020")
chatbot=c("Corey", "Corey-IM", "Corey-Emmendingen", "Corey-Ravensburg", "Corey-Calw", "Corey-Esslingen", "Corey-Main-Tauber-Kreis", "Corey-Heilbronn", "Corey-Loerrach", "Corey-Reutlingen", "Corey-Heidenheim", "Corey-Karlsruhe", "Corey-Mannheim")
gesamt_an=486561
gesamt = 304101
gesamt_im=290714
gesamt_em=2425
gesamt_ra=692
gesamt_ca=2090
gesamt_ess=945
gesamt_mtk=3610
gesamt_he=974
gesamt_lo=433
gesamt_re=1826
gesamt_hei=2494
gesamt_ka=400
gesamt_ma=461




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
                               valueBox(gesamt_hei, "Gesamtanzahl Besucher Corey-Heidenheim", color = "aqua", width=2),
                               valueBox(gesamt_ka, "Gesamtanzahl Besucher Corey-Karlsruhe", color = "blue", width=2),
                               valueBox(gesamt_ma, "Gesamtanzahl Besucher Corey-Mannheim", color = "yellow", width=2)
                               
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
