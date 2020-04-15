#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    setwd('C:/AE/statistik_corey')
#

library(shiny)
library(plotly)
dash <- read.csv2("statistik_corey.csv", header = TRUE, sep= ";", dec=".")
trace_c <- dash[dash$Chatbot=="Corey", ]
trace_c_Datum<- trace_c$Datum
trace_c_Anzahl<- trace_c$Anzahl
trace_c_Positiv<- trace_c$Positiv
trace_c_Negativ<- trace_c$Negativ
trace_c_Prozent_Positiv<- trace_c$Prozent_Positiv
trace_c_Prozent_Negativ<- trace_c$Prozent_Negativ
trace_cim <- dash[dash$Chatbot=="Corey-IM", ]
trace_cim_Datum<- trace_cim$Datum
trace_cim_Anzahl<- trace_cim$Anzahl
trace_cim_Positiv<- trace_cim$Positiv
trace_cim_Negativ<- trace_cim$Negativ
trace_cim_Prozent_Positiv<- trace_cim$Prozent_Positiv
trace_cim_Prozent_Negativ<- trace_cim$Prozent_Negativ
trace_cem <- dash[dash$Chatbot=="Corey-Emmendingen", ]
trace_cem_Datum<- trace_cem$Datum
trace_cem_Anzahl<- trace_cem$Anzahl
trace_cem_Positiv<- trace_cem$Positiv
trace_cem_Negativ<- trace_cem$Negativ
trace_cem_Prozent_Positiv<- trace_cem$Prozent_Positiv
trace_cem_Prozent_Negativ<- trace_cem$Prozent_Negativ
trace_cra <- dash[dash$Chatbot=="Corey-Ravensburg", ]
trace_cra_Datum<- trace_cra$Datum
trace_cra_Anzahl<- trace_cra$Anzahl
trace_cra_Positiv<- trace_cra$Positiv
trace_cra_Negativ<- trace_cra$Negativ
trace_cra_Prozent_Positiv<- trace_cra$Prozent_Positiv
trace_cra_Prozent_Negativ<- trace_cra$Prozent_Negativ
trace_cca <- dash[dash$Chatbot=="Corey-Calw", ]
trace_cca_Datum<- trace_cca$Datum
trace_cca_Anzahl<- trace_cca$Anzahl
trace_cca_Positiv<- trace_cca$Positiv
trace_cca_Negativ<- trace_cca$Negativ
trace_cca_Prozent_Positiv<- trace_cca$Prozent_Positiv
trace_cca_Prozent_Negativ<- trace_cca$Prozent_Negativ
trace_cess <- dash[dash$Chatbot=="Corey-Esslingen", ]
trace_cess_Datum<- trace_cess$Datum
trace_cess_Anzahl<- trace_cess$Anzahl
trace_cess_Positiv<- trace_cess$Positiv
trace_cess_Negativ<- trace_cess$Negativ
trace_cess_Prozent_Positiv<- trace_cess$Prozent_Positiv
trace_cess_Prozent_Negativ<- trace_cess$Prozent_Negativ
trace_cmtk <- dash[dash$Chatbot=="Corey-Main-Tauber-Kreis", ]
trace_cmtk_Datum<- trace_cmtk$Datum
trace_cmtk_Anzahl<- trace_cmtk$Anzahl
trace_cmtk_Positiv<- trace_cmtk$Positiv
trace_cmtk_Negativ<- trace_cmtk$Negativ
trace_cmtk_Prozent_Positiv<- trace_cmtk$Prozent_Positiv
trace_cmtk_Prozent_Negativ<- trace_cmtk$Prozent_Negativ

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$Plotdash <- renderPlotly({
        
            
        datadash <- data.frame(trace_c_Datum, trace_c_Anzahl, trace_cim_Anzahl, trace_cem_Anzahl, trace_cra_Anzahl, trace_cca_Anzahl, trace_cess_Anzahl, trace_cmtk$Anzahl)
        a <- list(title="Besucheranzahl")
        b<- list(title="Datum")
        
        Plotdash <- plot_ly(datadash, x = ~trace_c_Datum) 
        Plotdash <- Plotdash %>% add_trace(y = ~trace_c_Anzahl, name = 'Corey',type = 'scatter',mode = 'lines+markers', line=list(width=4, shape = "spline")) 
        Plotdash <- Plotdash %>% add_trace(y = ~trace_cim_Anzahl, name = 'Corey-IM',type = 'scatter',mode = 'lines+markers', line=list(width=4, shape = "spline")) 
        Plotdash <- Plotdash %>% add_trace(y = ~trace_cem_Anzahl, name = 'Corey-Emmendingen',type = 'scatter',mode = 'lines+markers', line=list(width=4, shape = "spline"))
        Plotdash <- Plotdash %>% add_trace(y = ~trace_cra_Anzahl, name = 'Corey-Ravensburg',type = 'scatter',mode = 'lines+markers', line=list(width=4, shape = "spline"))
        Plotdash <- Plotdash %>% add_trace(y = ~trace_cca_Anzahl, name = 'Corey-Calw',type = 'scatter',mode = 'lines+markers', line=list(width=4, shape = "spline"))
        Plotdash <- Plotdash %>% add_trace(y = ~trace_cess_Anzahl, name = 'Corey-Esslingen',type = 'scatter',mode = 'lines+markers', line=list(width=4, shape = "spline"))
        Plotdash <- Plotdash %>% add_trace(y = ~trace_cmtk$Anzahl, name = 'Corey-Main-Tauber-Kreis',type = 'scatter',mode = 'lines+markers', line=list(width=4, shape = "spline"))
        Plotdash <- Plotdash %>%layout(paper_bgcolor='transparent',
                                       plot_bgcolor='transparent',
                                       yaxis = a, xaxis = b)
        
    })
    output$Plotdashpos <- renderPlotly({
        
        
        datadashpos <- data.frame(trace_c_Datum, trace_c_Prozent_Positiv, trace_c_Prozent_Negativ)
        a <- list(title="Prozent korrekt vs. falsch klassifizierte Fragen")
        b<- list(title="Datum")
        
        Plotdashpos <- plot_ly(datadashpos, x = ~trace_c_Datum) 
        Plotdashpos <- Plotdashpos %>% add_trace(y = ~trace_c_Prozent_Positiv, name = 'korrekt klassifiziert',type = 'bar') 
        Plotdashpos <- Plotdashpos %>% add_trace(y = ~trace_c_Prozent_Negativ, name = 'falsch klassifiziert',type = 'bar') 
        Plotdashpos <- Plotdashpos %>%layout(paper_bgcolor='transparent',
                                             plot_bgcolor='transparent',
                                             yaxis = a, xaxis = b)
        
    })
    
    output$Plotallgbar <- renderPlotly({
        
        if (input$filterD== "06.04.2020") {
            
            if (input$filterC== "Corey") {
            
        x<-c("06.04.2020")
        data <- data.frame(x, trace_c_Anzahl = trace_c_Anzahl[1], trace_c_Positiv = trace_c_Positiv[1], trace_c_Negativ = trace_c_Negativ[1])
        a <- list(title="Besucheranzahl")
        b<- list(title="Datum")
        
        Plotallgbar <- plot_ly(data, x = ~x) 
        Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_c_Anzahl, name = 'Besucheranzahl',type = 'bar') 
        Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_c_Positiv, name = 'korrekt beantwortet',type = 'bar')
        Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_c_Negativ, name = 'falsch beantwortet',type = 'bar') 
        Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                             plot_bgcolor='transparent',
                                             yaxis = a, xaxis = b)
        
            }
            else if (input$filterC== "Corey-IM") {
                
                x<-c("06.04.2020")
                data <- data.frame(x, trace_cim_Anzahl=trace_cim_Anzahl[1], trace_cim_Positiv=trace_cim_Positiv[1], trace_cim_Negativ=trace_cim_Negativ[1])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cim_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cim_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cim_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("06.04.2020")
                data <- data.frame(x, trace_cem_Anzahl=trace_cem_Anzahl[1], trace_cem_Positiv=trace_cem_Positiv[1], trace_cem_Negativ=trace_cem_Negativ[1])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cem_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cem_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cem_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("06.04.2020")
                data <- data.frame(x, trace_cra_Anzahl=trace_cra_Anzahl[1], trace_cra_Positiv=trace_cra_Positiv[1], trace_cra_Negativ=trace_cra_Negativ[1])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cra_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cra_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cra_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("06.04.2020")
                data <- data.frame(x, trace_cca_Anzahl=trace_cca_Anzahl[1], trace_cca_Positiv=trace_cca_Positiv[1], trace_cca_Negativ=trace_cca_Negativ[1])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cca_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cca_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cca_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("06.04.2020")
                data <- data.frame(x, trace_cess_Anzahl=trace_cess_Anzahl[1], trace_cess_Positiv=trace_cess_Positiv[1], trace_cess_Negativ=trace_cess_Negativ[1])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cess_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cess_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cess_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("06.04.2020")
                data <- data.frame(x, trace_cmtk_Anzahl=trace_cmtk_Anzahl[1], trace_cmtk_Positiv=trace_cmtk_Positiv[1], trace_cmtk_Negativ=trace_cmtk_Negativ[1])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cmtk_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cmtk_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cmtk_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            
        }
        else if (input$filterD== "07.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("07.04.2020")
                data <- data.frame(x, trace_c_Anzahl = trace_c_Anzahl[2], trace_c_Positiv = trace_c_Positiv[2], trace_c_Negativ = trace_c_Negativ[2])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_c_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_c_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_c_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-IM") {
                
                x<-c("07.04.2020")
                data <- data.frame(x, trace_cim_Anzahl=trace_cim_Anzahl[2], trace_cim_Positiv=trace_cim_Positiv[2], trace_cim_Negativ=trace_cim_Negativ[2])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cim_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cim_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cim_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("07.04.2020")
                data <- data.frame(x, trace_cem_Anzahl=trace_cem_Anzahl[2], trace_cem_Positiv=trace_cem_Positiv[2], trace_cem_Negativ=trace_cem_Negativ[2])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cem_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cem_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cem_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("07.04.2020")
                data <- data.frame(x, trace_cra_Anzahl=trace_cra_Anzahl[2], trace_cra_Positiv=trace_cra_Positiv[2], trace_cra_Negativ=trace_cra_Negativ[2])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cra_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cra_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cra_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("07.04.2020")
                data <- data.frame(x, trace_cca_Anzahl=trace_cca_Anzahl[2], trace_cca_Positiv=trace_cca_Positiv[2], trace_cca_Negativ=trace_cca_Negativ[2])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cca_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cca_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cca_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("07.04.2020")
                data <- data.frame(x, trace_cess_Anzahl=trace_cess_Anzahl[2], trace_cess_Positiv=trace_cess_Positiv[2], trace_cess_Negativ=trace_cess_Negativ[2])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cess_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cess_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cess_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("07.04.2020")
                data <- data.frame(x, trace_cmtk_Anzahl=trace_cmtk_Anzahl[2], trace_cmtk_Positiv=trace_cmtk_Positiv[2], trace_cmtk_Negativ=trace_cmtk_Negativ[2])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cmtk_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cmtk_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cmtk_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            
        }
        else if (input$filterD== "08.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("08.04.2020")
                data <- data.frame(x, trace_c_Anzahl = trace_c_Anzahl[3], trace_c_Positiv = trace_c_Positiv[3], trace_c_Negativ = trace_c_Negativ[3])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_c_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_c_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_c_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-IM") {
                
                x<-c("08.04.2020")
                data <- data.frame(x, trace_cim_Anzahl=trace_cim_Anzahl[3], trace_cim_Positiv=trace_cim_Positiv[3], trace_cim_Negativ=trace_cim_Negativ[3])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cim_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cim_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cim_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("08.04.2020")
                data <- data.frame(x, trace_cem_Anzahl=trace_cem_Anzahl[3], trace_cem_Positiv=trace_cem_Positiv[3], trace_cem_Negativ=trace_cem_Negativ[3])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cem_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cem_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cem_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("08.04.2020")
                data <- data.frame(x, trace_cra_Anzahl=trace_cra_Anzahl[3], trace_cra_Positiv=trace_cra_Positiv[3], trace_cra_Negativ=trace_cra_Negativ[3])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cra_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cra_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cra_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("08.04.2020")
                data <- data.frame(x, trace_cca_Anzahl=trace_cca_Anzahl[3], trace_cca_Positiv=trace_cca_Positiv[3], trace_cca_Negativ=trace_cca_Negativ[3])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cca_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cca_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cca_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("08.04.2020")
                data <- data.frame(x, trace_cess_Anzahl=trace_cess_Anzahl[3], trace_cess_Positiv=trace_cess_Positiv[3], trace_cess_Negativ=trace_cess_Negativ[3])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cess_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cess_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cess_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("08.04.2020")
                data <- data.frame(x, trace_cmtk_Anzahl=trace_cmtk_Anzahl[3], trace_cmtk_Positiv=trace_cmtk_Positiv[3], trace_cmtk_Negativ=trace_cmtk_Negativ[3])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cmtk_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cmtk_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cmtk_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            
        }
        else if (input$filterD== "09.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("09.04.2020")
                data <- data.frame(x, trace_c_Anzahl = trace_c_Anzahl[4], trace_c_Positiv = trace_c_Positiv[4], trace_c_Negativ = trace_c_Negativ[4])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_c_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_c_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_c_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-IM") {
                
                x<-c("09.04.2020")
                data <- data.frame(x, trace_cim_Anzahl=trace_cim_Anzahl[4], trace_cim_Positiv=trace_cim_Positiv[4], trace_cim_Negativ=trace_cim_Negativ[4])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cim_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cim_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cim_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("09.04.2020")
                data <- data.frame(x, trace_cem_Anzahl=trace_cem_Anzahl[4], trace_cem_Positiv=trace_cem_Positiv[4], trace_cem_Negativ=trace_cem_Negativ[4])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cem_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cem_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cem_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("09.04.2020")
                data <- data.frame(x, trace_cra_Anzahl=trace_cra_Anzahl[4], trace_cra_Positiv=trace_cra_Positiv[4], trace_cra_Negativ=trace_cra_Negativ[4])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cra_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cra_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cra_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("09.04.2020")
                data <- data.frame(x, trace_cca_Anzahl=trace_cca_Anzahl[4], trace_cca_Positiv=trace_cca_Positiv[4], trace_cca_Negativ=trace_cca_Negativ[4])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cca_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cca_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cca_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("09.04.2020")
                data <- data.frame(x, trace_cess_Anzahl=trace_cess_Anzahl[4], trace_cess_Positiv=trace_cess_Positiv[4], trace_cess_Negativ=trace_cess_Negativ[4])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cess_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cess_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cess_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("09.04.2020")
                data <- data.frame(x, trace_cmtk_Anzahl=trace_cmtk_Anzahl[4], trace_cmtk_Positiv=trace_cmtk_Positiv[4], trace_cmtk_Negativ=trace_cmtk_Negativ[4])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cmtk_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cmtk_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cmtk_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            
        }
        else if (input$filterD== "10.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("10.04.2020")
                data <- data.frame(x, trace_c_Anzahl = trace_c_Anzahl[5], trace_c_Positiv = trace_c_Positiv[5], trace_c_Negativ = trace_c_Negativ[5])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_c_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_c_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_c_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-IM") {
                
                x<-c("10.04.2020")
                data <- data.frame(x, trace_cim_Anzahl=trace_cim_Anzahl[5], trace_cim_Positiv=trace_cim_Positiv[5], trace_cim_Negativ=trace_cim_Negativ[5])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cim_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cim_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cim_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("10.04.2020")
                data <- data.frame(x, trace_cem_Anzahl=trace_cem_Anzahl[5], trace_cem_Positiv=trace_cem_Positiv[5], trace_cem_Negativ=trace_cem_Negativ[5])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cem_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cem_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cem_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("10.04.2020")
                data <- data.frame(x, trace_cra_Anzahl=trace_cra_Anzahl[5], trace_cra_Positiv=trace_cra_Positiv[5], trace_cra_Negativ=trace_cra_Negativ[5])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cra_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cra_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cra_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("10.04.2020")
                data <- data.frame(x, trace_cca_Anzahl=trace_cca_Anzahl[5], trace_cca_Positiv=trace_cca_Positiv[5], trace_cca_Negativ=trace_cca_Negativ[5])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cca_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cca_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cca_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("10.04.2020")
                data <- data.frame(x, trace_cess_Anzahl=trace_cess_Anzahl[5], trace_cess_Positiv=trace_cess_Positiv[5], trace_cess_Negativ=trace_cess_Negativ[5])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cess_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cess_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cess_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("10.04.2020")
                data <- data.frame(x, trace_cmtk_Anzahl=trace_cmtk_Anzahl[5], trace_cmtk_Positiv=trace_cmtk_Positiv[5], trace_cmtk_Negativ=trace_cmtk_Negativ[5])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cmtk_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cmtk_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cmtk_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            
        }
        else if (input$filterD== "11.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("11.04.2020")
                data <- data.frame(x, trace_c_Anzahl = trace_c_Anzahl[6], trace_c_Positiv = trace_c_Positiv[6], trace_c_Negativ = trace_c_Negativ[6])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_c_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_c_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_c_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-IM") {
                
                x<-c("11.04.2020")
                data <- data.frame(x, trace_cim_Anzahl=trace_cim_Anzahl[6], trace_cim_Positiv=trace_cim_Positiv[6], trace_cim_Negativ=trace_cim_Negativ[6])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cim_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cim_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cim_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("11.04.2020")
                data <- data.frame(x, trace_cem_Anzahl=trace_cem_Anzahl[6], trace_cem_Positiv=trace_cem_Positiv[6], trace_cem_Negativ=trace_cem_Negativ[6])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cem_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cem_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cem_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("11.04.2020")
                data <- data.frame(x, trace_cra_Anzahl=trace_cra_Anzahl[6], trace_cra_Positiv=trace_cra_Positiv[6], trace_cra_Negativ=trace_cra_Negativ[6])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cra_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cra_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cra_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("11.04.2020")
                data <- data.frame(x, trace_cca_Anzahl=trace_cca_Anzahl[6], trace_cca_Positiv=trace_cca_Positiv[6], trace_cca_Negativ=trace_cca_Negativ[6])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cca_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cca_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cca_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("11.04.2020")
                data <- data.frame(x, trace_cess_Anzahl=trace_cess_Anzahl[6], trace_cess_Positiv=trace_cess_Positiv[6], trace_cess_Negativ=trace_cess_Negativ[6])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cess_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cess_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cess_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("11.04.2020")
                data <- data.frame(x, trace_cmtk_Anzahl=trace_cmtk_Anzahl[6], trace_cmtk_Positiv=trace_cmtk_Positiv[6], trace_cmtk_Negativ=trace_cmtk_Negativ[6])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cmtk_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cmtk_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cmtk_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            
        }
        else if (input$filterD== "12.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("12.04.2020")
                data <- data.frame(x, trace_c_Anzahl = trace_c_Anzahl[7], trace_c_Positiv = trace_c_Positiv[7], trace_c_Negativ = trace_c_Negativ[7])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_c_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_c_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_c_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-IM") {
                
                x<-c("12.04.2020")
                data <- data.frame(x, trace_cim_Anzahl=trace_cim_Anzahl[7], trace_cim_Positiv=trace_cim_Positiv[7], trace_cim_Negativ=trace_cim_Negativ[7])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cim_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cim_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cim_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("12.04.2020")
                data <- data.frame(x, trace_cem_Anzahl=trace_cem_Anzahl[7], trace_cem_Positiv=trace_cem_Positiv[7], trace_cem_Negativ=trace_cem_Negativ[7])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cem_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cem_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cem_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("12.04.2020")
                data <- data.frame(x, trace_cra_Anzahl=trace_cra_Anzahl[7], trace_cra_Positiv=trace_cra_Positiv[7], trace_cra_Negativ=trace_cra_Negativ[7])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cra_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cra_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cra_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("12.04.2020")
                data <- data.frame(x, trace_cca_Anzahl=trace_cca_Anzahl[7], trace_cca_Positiv=trace_cca_Positiv[7], trace_cca_Negativ=trace_cca_Negativ[7])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cca_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cca_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cca_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("12.04.2020")
                data <- data.frame(x, trace_cess_Anzahl=trace_cess_Anzahl[7], trace_cess_Positiv=trace_cess_Positiv[7], trace_cess_Negativ=trace_cess_Negativ[7])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cess_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cess_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cess_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("12.04.2020")
                data <- data.frame(x, trace_cmtk_Anzahl=trace_cmtk_Anzahl[7], trace_cmtk_Positiv=trace_cmtk_Positiv[7], trace_cmtk_Negativ=trace_cmtk_Negativ[7])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cmtk_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cmtk_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cmtk_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            
        }
        else if (input$filterD== "13.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("13.04.2020")
                data <- data.frame(x, trace_c_Anzahl = trace_c_Anzahl[8], trace_c_Positiv = trace_c_Positiv[8], trace_c_Negativ = trace_c_Negativ[8])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_c_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_c_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_c_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-IM") {
                
                x<-c("13.04.2020")
                data <- data.frame(x, trace_cim_Anzahl=trace_cim_Anzahl[8], trace_cim_Positiv=trace_cim_Positiv[8], trace_cim_Negativ=trace_cim_Negativ[8])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cim_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cim_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cim_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("13.04.2020")
                data <- data.frame(x, trace_cem_Anzahl=trace_cem_Anzahl[8], trace_cem_Positiv=trace_cem_Positiv[8], trace_cem_Negativ=trace_cem_Negativ[8])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cem_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cem_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cem_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("13.04.2020")
                data <- data.frame(x, trace_cra_Anzahl=trace_cra_Anzahl[8], trace_cra_Positiv=trace_cra_Positiv[8], trace_cra_Negativ=trace_cra_Negativ[8])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cra_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cra_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cra_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("13.04.2020")
                data <- data.frame(x, trace_cca_Anzahl=trace_cca_Anzahl[8], trace_cca_Positiv=trace_cca_Positiv[8], trace_cca_Negativ=trace_cca_Negativ[8])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cca_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cca_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cca_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("13.04.2020")
                data <- data.frame(x, trace_cess_Anzahl=trace_cess_Anzahl[8], trace_cess_Positiv=trace_cess_Positiv[8], trace_cess_Negativ=trace_cess_Negativ[8])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cess_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cess_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cess_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("13.04.2020")
                data <- data.frame(x, trace_cmtk_Anzahl=trace_cmtk_Anzahl[8], trace_cmtk_Positiv=trace_cmtk_Positiv[8], trace_cmtk_Negativ=trace_cmtk_Negativ[8])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cmtk_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cmtk_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cmtk_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            
        }
        else if (input$filterD== "14.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("14.04.2020")
                data <- data.frame(x, trace_c_Anzahl = trace_c_Anzahl[9], trace_c_Positiv = trace_c_Positiv[9], trace_c_Negativ = trace_c_Negativ[9])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_c_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_c_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_c_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-IM") {
                
                x<-c("14.04.2020")
                data <- data.frame(x, trace_cim_Anzahl=trace_cim_Anzahl[9], trace_cim_Positiv=trace_cim_Positiv[9], trace_cim_Negativ=trace_cim_Negativ[9])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cim_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cim_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cim_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("14.04.2020")
                data <- data.frame(x, trace_cem_Anzahl=trace_cem_Anzahl[9], trace_cem_Positiv=trace_cem_Positiv[9], trace_cem_Negativ=trace_cem_Negativ[9])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cem_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cem_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cem_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("14.04.2020")
                data <- data.frame(x, trace_cra_Anzahl=trace_cra_Anzahl[9], trace_cra_Positiv=trace_cra_Positiv[9], trace_cra_Negativ=trace_cra_Negativ[9])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cra_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cra_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cra_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("14.04.2020")
                data <- data.frame(x, trace_cca_Anzahl=trace_cca_Anzahl[9], trace_cca_Positiv=trace_cca_Positiv[9], trace_cca_Negativ=trace_cca_Negativ[9])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cca_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cca_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cca_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("14.04.2020")
                data <- data.frame(x, trace_cess_Anzahl=trace_cess_Anzahl[9], trace_cess_Positiv=trace_cess_Positiv[9], trace_cess_Negativ=trace_cess_Negativ[9])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cess_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cess_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cess_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("14.04.2020")
                data <- data.frame(x, trace_cmtk_Anzahl=trace_cmtk_Anzahl[9], trace_cmtk_Positiv=trace_cmtk_Positiv[9], trace_cmtk_Negativ=trace_cmtk_Negativ[9])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cmtk_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cmtk_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cmtk_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            
        }
        else if (input$filterD== "15.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("15.04.2020")
                data <- data.frame(x, trace_c_Anzahl = trace_c_Anzahl[10], trace_c_Positiv = trace_c_Positiv[10], trace_c_Negativ = trace_c_Negativ[10])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_c_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_c_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_c_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-IM") {
                
                x<-c("15.04.2020")
                data <- data.frame(x, trace_cim_Anzahl=trace_cim_Anzahl[10], trace_cim_Positiv=trace_cim_Positiv[10], trace_cim_Negativ=trace_cim_Negativ[10])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cim_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cim_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cim_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("15.04.2020")
                data <- data.frame(x, trace_cem_Anzahl=trace_cem_Anzahl[10], trace_cem_Positiv=trace_cem_Positiv[10], trace_cem_Negativ=trace_cem_Negativ[10])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cem_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cem_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cem_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("15.04.2020")
                data <- data.frame(x, trace_cra_Anzahl=trace_cra_Anzahl[10], trace_cra_Positiv=trace_cra_Positiv[10], trace_cra_Negativ=trace_cra_Negativ[10])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cra_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cra_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cra_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("15.04.2020")
                data <- data.frame(x, trace_cca_Anzahl=trace_cca_Anzahl[10], trace_cca_Positiv=trace_cca_Positiv[10], trace_cca_Negativ=trace_cca_Negativ[10])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cca_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cca_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cca_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("15.04.2020")
                data <- data.frame(x, trace_cess_Anzahl=trace_cess_Anzahl[10], trace_cess_Positiv=trace_cess_Positiv[10], trace_cess_Negativ=trace_cess_Negativ[10])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cess_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cess_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cess_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("15.04.2020")
                data <- data.frame(x, trace_cmtk_Anzahl=trace_cmtk_Anzahl[10], trace_cmtk_Positiv=trace_cmtk_Positiv[10], trace_cmtk_Negativ=trace_cmtk_Negativ[10])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cmtk_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cmtk_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cmtk_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            
        }
        
    })
    
    output$Plotallgpie <- renderPlotly({
        
        if (input$filterD== "06.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_c_Prozent_Positiv = trace_c_Prozent_Positiv[1], trace_c_Prozent_Negativ = trace_c_Prozent_Negativ[1])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
            else if (input$filterC== "Corey-IM") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cim_Prozent_Positiv = trace_cim_Prozent_Positiv[1], trace_cim_Prozent_Negativ = trace_cim_Prozent_Negativ[1])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cem_Prozent_Positiv = trace_cem_Prozent_Positiv[1], trace_cem_Prozent_Negativ = trace_cem_Prozent_Negativ[1])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cra_Prozent_Positiv = trace_cra_Prozent_Positiv[1], trace_cra_Prozent_Negativ = trace_cra_Prozent_Negativ[1])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cca_Prozent_Positiv = trace_cca_Prozent_Positiv[1], trace_cca_Prozent_Negativ = trace_cca_Prozent_Negativ[1])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cess_Prozent_Positiv = trace_cess_Prozent_Positiv[1], trace_cess_Prozent_Negativ = trace_cess_Prozent_Negativ[1])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cmtk_Prozent_Positiv = trace_cmtk_Prozent_Positiv[1], trace_cmtk_Prozent_Negativ = trace_cmtk_Prozent_Negativ[1])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
        }
        else if (input$filterD== "07.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_c_Prozent_Positiv = trace_c_Prozent_Positiv[2], trace_c_Prozent_Negativ = trace_c_Prozent_Negativ[2])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
            else if (input$filterC== "Corey-IM") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cim_Prozent_Positiv = trace_cim_Prozent_Positiv[2], trace_cim_Prozent_Negativ = trace_cim_Prozent_Negativ[2])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cem_Prozent_Positiv = trace_cem_Prozent_Positiv[2], trace_cem_Prozent_Negativ = trace_cem_Prozent_Negativ[2])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cra_Prozent_Positiv = trace_cra_Prozent_Positiv[2], trace_cra_Prozent_Negativ = trace_cra_Prozent_Negativ[2])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cca_Prozent_Positiv = trace_cca_Prozent_Positiv[2], trace_cca_Prozent_Negativ = trace_cca_Prozent_Negativ[2])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cess_Prozent_Positiv = trace_cess_Prozent_Positiv[2], trace_cess_Prozent_Negativ = trace_cess_Prozent_Negativ[2])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cmtk_Prozent_Positiv = trace_cmtk_Prozent_Positiv[2], trace_cmtk_Prozent_Negativ = trace_cmtk_Prozent_Negativ[2])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
        }
        else if (input$filterD== "08.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_c_Prozent_Positiv = trace_c_Prozent_Positiv[3], trace_c_Prozent_Negativ = trace_c_Prozent_Negativ[3])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
            else if (input$filterC== "Corey-IM") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cim_Prozent_Positiv = trace_cim_Prozent_Positiv[3], trace_cim_Prozent_Negativ = trace_cim_Prozent_Negativ[3])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cem_Prozent_Positiv = trace_cem_Prozent_Positiv[3], trace_cem_Prozent_Negativ = trace_cem_Prozent_Negativ[3])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cra_Prozent_Positiv = trace_cra_Prozent_Positiv[3], trace_cra_Prozent_Negativ = trace_cra_Prozent_Negativ[3])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cca_Prozent_Positiv = trace_cca_Prozent_Positiv[3], trace_cca_Prozent_Negativ = trace_cca_Prozent_Negativ[3])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cess_Prozent_Positiv = trace_cess_Prozent_Positiv[3], trace_cess_Prozent_Negativ = trace_cess_Prozent_Negativ[3])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cmtk_Prozent_Positiv = trace_cmtk_Prozent_Positiv[3], trace_cmtk_Prozent_Negativ = trace_cmtk_Prozent_Negativ[3])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
        }
        else if (input$filterD== "09.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_c_Prozent_Positiv = trace_c_Prozent_Positiv[4], trace_c_Prozent_Negativ = trace_c_Prozent_Negativ[4])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
            else if (input$filterC== "Corey-IM") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cim_Prozent_Positiv = trace_cim_Prozent_Positiv[4], trace_cim_Prozent_Negativ = trace_cim_Prozent_Negativ[4])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cem_Prozent_Positiv = trace_cem_Prozent_Positiv[4], trace_cem_Prozent_Negativ = trace_cem_Prozent_Negativ[4])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cra_Prozent_Positiv = trace_cra_Prozent_Positiv[4], trace_cra_Prozent_Negativ = trace_cra_Prozent_Negativ[4])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cca_Prozent_Positiv = trace_cca_Prozent_Positiv[4], trace_cca_Prozent_Negativ = trace_cca_Prozent_Negativ[4])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cess_Prozent_Positiv = trace_cess_Prozent_Positiv[4], trace_cess_Prozent_Negativ = trace_cess_Prozent_Negativ[4])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cmtk_Prozent_Positiv = trace_cmtk_Prozent_Positiv[4], trace_cmtk_Prozent_Negativ = trace_cmtk_Prozent_Negativ[4])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
        }
        else if (input$filterD== "10.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_c_Prozent_Positiv = trace_c_Prozent_Positiv[5], trace_c_Prozent_Negativ = trace_c_Prozent_Negativ[5])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
            else if (input$filterC== "Corey-IM") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cim_Prozent_Positiv = trace_cim_Prozent_Positiv[5], trace_cim_Prozent_Negativ = trace_cim_Prozent_Negativ[5])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cem_Prozent_Positiv = trace_cem_Prozent_Positiv[5], trace_cem_Prozent_Negativ = trace_cem_Prozent_Negativ[5])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cra_Prozent_Positiv = trace_cra_Prozent_Positiv[5], trace_cra_Prozent_Negativ = trace_cra_Prozent_Negativ[5])
                data <- data.frame(x, y)
                
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cca_Prozent_Positiv = trace_cca_Prozent_Positiv[5], trace_cca_Prozent_Negativ = trace_cca_Prozent_Negativ[5])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cess_Prozent_Positiv = trace_cess_Prozent_Positiv[5], trace_cess_Prozent_Negativ = trace_cess_Prozent_Negativ[5])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cmtk_Prozent_Positiv = trace_cmtk_Prozent_Positiv[5], trace_cmtk_Prozent_Negativ = trace_cmtk_Prozent_Negativ[5])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
        }
        else if (input$filterD== "11.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_c_Prozent_Positiv = trace_c_Prozent_Positiv[6], trace_c_Prozent_Negativ = trace_c_Prozent_Negativ[6])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
            else if (input$filterC== "Corey-IM") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cim_Prozent_Positiv = trace_cim_Prozent_Positiv[6], trace_cim_Prozent_Negativ = trace_cim_Prozent_Negativ[6])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cem_Prozent_Positiv = trace_cem_Prozent_Positiv[6], trace_cem_Prozent_Negativ = trace_cem_Prozent_Negativ[6])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cra_Prozent_Positiv = trace_cra_Prozent_Positiv[6], trace_cra_Prozent_Negativ = trace_cra_Prozent_Negativ[6])
                data <- data.frame(x, y)
                
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cca_Prozent_Positiv = trace_cca_Prozent_Positiv[6], trace_cca_Prozent_Negativ = trace_cca_Prozent_Negativ[6])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cess_Prozent_Positiv = trace_cess_Prozent_Positiv[6], trace_cess_Prozent_Negativ = trace_cess_Prozent_Negativ[6])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cmtk_Prozent_Positiv = trace_cmtk_Prozent_Positiv[6], trace_cmtk_Prozent_Negativ = trace_cmtk_Prozent_Negativ[6])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
        }
        else if (input$filterD== "12.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_c_Prozent_Positiv = trace_c_Prozent_Positiv[7], trace_c_Prozent_Negativ = trace_c_Prozent_Negativ[7])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
            else if (input$filterC== "Corey-IM") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cim_Prozent_Positiv = trace_cim_Prozent_Positiv[7], trace_cim_Prozent_Negativ = trace_cim_Prozent_Negativ[7])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cem_Prozent_Positiv = trace_cem_Prozent_Positiv[7], trace_cem_Prozent_Negativ = trace_cem_Prozent_Negativ[7])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cra_Prozent_Positiv = trace_cra_Prozent_Positiv[7], trace_cra_Prozent_Negativ = trace_cra_Prozent_Negativ[7])
                data <- data.frame(x, y)
                
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cca_Prozent_Positiv = trace_cca_Prozent_Positiv[7], trace_cca_Prozent_Negativ = trace_cca_Prozent_Negativ[7])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cess_Prozent_Positiv = trace_cess_Prozent_Positiv[7], trace_cess_Prozent_Negativ = trace_cess_Prozent_Negativ[7])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cmtk_Prozent_Positiv = trace_cmtk_Prozent_Positiv[7], trace_cmtk_Prozent_Negativ = trace_cmtk_Prozent_Negativ[7])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
        }
        else if (input$filterD== "13.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_c_Prozent_Positiv = trace_c_Prozent_Positiv[8], trace_c_Prozent_Negativ = trace_c_Prozent_Negativ[8])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
            else if (input$filterC== "Corey-IM") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cim_Prozent_Positiv = trace_cim_Prozent_Positiv[8], trace_cim_Prozent_Negativ = trace_cim_Prozent_Negativ[8])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cem_Prozent_Positiv = trace_cem_Prozent_Positiv[8], trace_cem_Prozent_Negativ = trace_cem_Prozent_Negativ[8])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cra_Prozent_Positiv = trace_cra_Prozent_Positiv[8], trace_cra_Prozent_Negativ = trace_cra_Prozent_Negativ[8])
                data <- data.frame(x, y)
                
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cca_Prozent_Positiv = trace_cca_Prozent_Positiv[8], trace_cca_Prozent_Negativ = trace_cca_Prozent_Negativ[8])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cess_Prozent_Positiv = trace_cess_Prozent_Positiv[8], trace_cess_Prozent_Negativ = trace_cess_Prozent_Negativ[8])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cmtk_Prozent_Positiv = trace_cmtk_Prozent_Positiv[8], trace_cmtk_Prozent_Negativ = trace_cmtk_Prozent_Negativ[8])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
        }
        else if (input$filterD== "14.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_c_Prozent_Positiv = trace_c_Prozent_Positiv[9], trace_c_Prozent_Negativ = trace_c_Prozent_Negativ[9])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
            else if (input$filterC== "Corey-IM") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cim_Prozent_Positiv = trace_cim_Prozent_Positiv[9], trace_cim_Prozent_Negativ = trace_cim_Prozent_Negativ[9])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cem_Prozent_Positiv = trace_cem_Prozent_Positiv[9], trace_cem_Prozent_Negativ = trace_cem_Prozent_Negativ[9])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cra_Prozent_Positiv = trace_cra_Prozent_Positiv[9], trace_cra_Prozent_Negativ = trace_cra_Prozent_Negativ[9])
                data <- data.frame(x, y)
                
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cca_Prozent_Positiv = trace_cca_Prozent_Positiv[9], trace_cca_Prozent_Negativ = trace_cca_Prozent_Negativ[9])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cess_Prozent_Positiv = trace_cess_Prozent_Positiv[9], trace_cess_Prozent_Negativ = trace_cess_Prozent_Negativ[9])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cmtk_Prozent_Positiv = trace_cmtk_Prozent_Positiv[9], trace_cmtk_Prozent_Negativ = trace_cmtk_Prozent_Negativ[9])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
        }
        else if (input$filterD== "15.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_c_Prozent_Positiv = trace_c_Prozent_Positiv[10], trace_c_Prozent_Negativ = trace_c_Prozent_Negativ[10])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
            else if (input$filterC== "Corey-IM") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cim_Prozent_Positiv = trace_cim_Prozent_Positiv[10], trace_cim_Prozent_Negativ = trace_cim_Prozent_Negativ[10])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cem_Prozent_Positiv = trace_cem_Prozent_Positiv[10], trace_cem_Prozent_Negativ = trace_cem_Prozent_Negativ[10])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cra_Prozent_Positiv = trace_cra_Prozent_Positiv[10], trace_cra_Prozent_Negativ = trace_cra_Prozent_Negativ[10])
                data <- data.frame(x, y)
                
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cca_Prozent_Positiv = trace_cca_Prozent_Positiv[10], trace_cca_Prozent_Negativ = trace_cca_Prozent_Negativ[10])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cess_Prozent_Positiv = trace_cess_Prozent_Positiv[10], trace_cess_Prozent_Negativ = trace_cess_Prozent_Negativ[10])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cmtk_Prozent_Positiv = trace_cmtk_Prozent_Positiv[10], trace_cmtk_Prozent_Negativ = trace_cmtk_Prozent_Negativ[10])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
        }
    })
    
    output$Plotfeed <- renderPlotly({
        
        x=c("06.04.2020", "07.04.2020", "08.04.2020", "09.04.2020", "10.04.2020", "11.04.2020", "12.04.2020", "13.04.2020", "14.04.2020", "15.04.2020")
        y=c(119, 132, 120, 111, 94, 61, 45, 57, 170, 236)
        data <- data.frame(trace_c_Datum, trace_c_Prozent_Positiv, trace_c_Prozent_Negativ)
        a <- list(title="Gesamtanzahl der Feedbacks")
        b<- list(title="Datum")
        
        Plotfeed <- plot_ly(x = x) 
        Plotfeed <- Plotfeed %>% add_trace(y = y, name = 'Anzahl',type = 'bar') 
        Plotfeed <- Plotfeed %>%layout(paper_bgcolor='transparent',
                                             plot_bgcolor='transparent',
                                             yaxis = a, xaxis = b)
        
    })

})
