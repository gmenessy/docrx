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
trace_c_Datum <-as.Date(trace_c_Datum, format = "%d-%m-%y")
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
trace_che <- dash[dash$Chatbot=="Corey-Heilbronn", ]
trace_che_Datum<- trace_che$Datum
trace_che_Anzahl<- trace_che$Anzahl
trace_che_Positiv<- trace_che$Positiv
trace_che_Negativ<- trace_che$Negativ
trace_che_Prozent_Positiv<- trace_che$Prozent_Positiv
trace_che_Prozent_Negativ<- trace_che$Prozent_Negativ
trace_clo <- dash[dash$Chatbot=="Corey-Loerrach", ]
trace_clo_Datum<- trace_clo$Datum
trace_clo_Anzahl<- trace_clo$Anzahl
trace_clo_Positiv<- trace_clo$Positiv
trace_clo_Negativ<- trace_clo$Negativ
trace_clo_Prozent_Positiv<- trace_clo$Prozent_Positiv
trace_clo_Prozent_Negativ<- trace_clo$Prozent_Negativ
trace_cre <- dash[dash$Chatbot=="Corey-Reutlingen", ]
trace_cre_Datum<- trace_cre$Datum
trace_cre_Anzahl<- trace_cre$Anzahl
trace_cre_Positiv<- trace_cre$Positiv
trace_cre_Negativ<- trace_cre$Negativ
trace_cre_Prozent_Positiv<- trace_cre$Prozent_Positiv
trace_cre_Prozent_Negativ<- trace_cre$Prozent_Negativ
trace_chei <- dash[dash$Chatbot=="Corey-Heidenheim", ]
trace_chei_Datum<- trace_chei$Datum
trace_chei_Anzahl<- trace_chei$Anzahl
trace_chei_Positiv<- trace_chei$Positiv
trace_chei_Negativ<- trace_chei$Negativ
trace_chei_Prozent_Positiv<- trace_chei$Prozent_Positiv
trace_chei_Prozent_Negativ<- trace_chei$Prozent_Negativ

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$Plotdash <- renderPlotly({
        
            
        datadash <- data.frame(trace_c_Datum, trace_c_Anzahl, trace_cim_Anzahl, trace_cem_Anzahl, trace_cra_Anzahl, trace_cca_Anzahl, trace_cess_Anzahl, trace_cmtk$Anzahl, trace_che_Anzahl, trace_clo_Anzahl, trace_cre_Anzahl)
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
        Plotdash <- Plotdash %>% add_trace(y = ~trace_che$Anzahl, name = 'Corey-Heilbronn',type = 'scatter',mode = 'lines+markers', line=list(width=4, shape = "spline"))
        Plotdash <- Plotdash %>% add_trace(y = ~trace_clo$Anzahl, name = 'Corey-Lörrach',type = 'scatter',mode = 'lines+markers', line=list(width=4, shape = "spline"))
        Plotdash <- Plotdash %>% add_trace(y = ~trace_cre$Anzahl, name = 'Corey-Reutlingen',type = 'scatter',mode = 'lines+markers', line=list(width=4, shape = "spline"))
        Plotdash <- Plotdash %>% add_trace(y = ~trace_chei$Anzahl, name = 'Corey-Heidenheim',type = 'scatter',mode = 'lines+markers', line=list(width=4, shape = "spline"))
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("06.04.2020")
                data <- data.frame(x, trace_che_Anzahl=trace_che_Anzahl[1], trace_che_Positiv=trace_che_Positiv[1], trace_che_Negativ=trace_che_Negativ[1])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("06.04.2020")
                data <- data.frame(x, trace_clo_Anzahl=trace_clo_Anzahl[1], trace_clo_Positiv=trace_clo_Positiv[1], trace_clo_Negativ=trace_clo_Negativ[1])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("06.04.2020")
                data <- data.frame(x, trace_cre_Anzahl=trace_cre_Anzahl[1], trace_cre_Positiv=trace_cre_Positiv[1], trace_cre_Negativ=trace_cre_Negativ[1])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("06.04.2020")
                data <- data.frame(x, trace_chei_Anzahl=trace_chei_Anzahl[1], trace_chei_Positiv=trace_chei_Positiv[1], trace_chei_Negativ=trace_chei_Negativ[1])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            
        }
        else if (input$filterD== "11.05.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("11.05.2020")
                data <- data.frame(x, trace_c_Anzahl = trace_c_Anzahl[36], trace_c_Positiv = trace_c_Positiv[36], trace_c_Negativ = trace_c_Negativ[36])
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
                
                x<-c("11.05.2020")
                data <- data.frame(x, trace_cim_Anzahl=trace_cim_Anzahl[36], trace_cim_Positiv=trace_cim_Positiv[36], trace_cim_Negativ=trace_cim_Negativ[36])
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
                
                x<-c("11.05.2020")
                data <- data.frame(x, trace_cem_Anzahl=trace_cem_Anzahl[36], trace_cem_Positiv=trace_cem_Positiv[36], trace_cem_Negativ=trace_cem_Negativ[36])
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
                
                x<-c("11.05.2020")
                data <- data.frame(x, trace_cra_Anzahl=trace_cra_Anzahl[36], trace_cra_Positiv=trace_cra_Positiv[36], trace_cra_Negativ=trace_cra_Negativ[36])
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
                
                x<-c("11.05.2020")
                data <- data.frame(x, trace_cca_Anzahl=trace_cca_Anzahl[36], trace_cca_Positiv=trace_cca_Positiv[36], trace_cca_Negativ=trace_cca_Negativ[36])
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
                
                x<-c("11.05.2020")
                data <- data.frame(x, trace_cess_Anzahl=trace_cess_Anzahl[36], trace_cess_Positiv=trace_cess_Positiv[36], trace_cess_Negativ=trace_cess_Negativ[36])
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
                
                x<-c("11.05.2020")
                data <- data.frame(x, trace_cmtk_Anzahl=trace_cmtk_Anzahl[36], trace_cmtk_Positiv=trace_cmtk_Positiv[36], trace_cmtk_Negativ=trace_cmtk_Negativ[36])
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("11.05.2020")
                data <- data.frame(x, trace_che_Anzahl=trace_che_Anzahl[36], trace_che_Positiv=trace_che_Positiv[36], trace_che_Negativ=trace_che_Negativ[36])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("11.05.2020")
                data <- data.frame(x, trace_clo_Anzahl=trace_clo_Anzahl[36], trace_clo_Positiv=trace_clo_Positiv[36], trace_clo_Negativ=trace_clo_Negativ[36])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("11.05.2020")
                data <- data.frame(x, trace_cre_Anzahl=trace_cre_Anzahl[36], trace_cre_Positiv=trace_cre_Positiv[36], trace_cre_Negativ=trace_cre_Negativ[36])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("11.05.2020")
                data <- data.frame(x, trace_chei_Anzahl=trace_chei_Anzahl[36], trace_chei_Positiv=trace_chei_Positiv[36], trace_chei_Negativ=trace_chei_Negativ[36])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            
        }
        else if (input$filterD== "10.05.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("10.05.2020")
                data <- data.frame(x, trace_c_Anzahl = trace_c_Anzahl[35], trace_c_Positiv = trace_c_Positiv[35], trace_c_Negativ = trace_c_Negativ[35])
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
                
                x<-c("10.05.2020")
                data <- data.frame(x, trace_cim_Anzahl=trace_cim_Anzahl[35], trace_cim_Positiv=trace_cim_Positiv[35], trace_cim_Negativ=trace_cim_Negativ[35])
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
                
                x<-c("10.05.2020")
                data <- data.frame(x, trace_cem_Anzahl=trace_cem_Anzahl[35], trace_cem_Positiv=trace_cem_Positiv[35], trace_cem_Negativ=trace_cem_Negativ[35])
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
                
                x<-c("10.05.2020")
                data <- data.frame(x, trace_cra_Anzahl=trace_cra_Anzahl[35], trace_cra_Positiv=trace_cra_Positiv[35], trace_cra_Negativ=trace_cra_Negativ[35])
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
                
                x<-c("10.05.2020")
                data <- data.frame(x, trace_cca_Anzahl=trace_cca_Anzahl[35], trace_cca_Positiv=trace_cca_Positiv[35], trace_cca_Negativ=trace_cca_Negativ[35])
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
                
                x<-c("10.05.2020")
                data <- data.frame(x, trace_cess_Anzahl=trace_cess_Anzahl[35], trace_cess_Positiv=trace_cess_Positiv[35], trace_cess_Negativ=trace_cess_Negativ[35])
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
                
                x<-c("10.05.2020")
                data <- data.frame(x, trace_cmtk_Anzahl=trace_cmtk_Anzahl[35], trace_cmtk_Positiv=trace_cmtk_Positiv[35], trace_cmtk_Negativ=trace_cmtk_Negativ[35])
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("10.05.2020")
                data <- data.frame(x, trace_che_Anzahl=trace_che_Anzahl[35], trace_che_Positiv=trace_che_Positiv[35], trace_che_Negativ=trace_che_Negativ[35])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("10.05.2020")
                data <- data.frame(x, trace_clo_Anzahl=trace_clo_Anzahl[35], trace_clo_Positiv=trace_clo_Positiv[35], trace_clo_Negativ=trace_clo_Negativ[35])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("10.05.2020")
                data <- data.frame(x, trace_cre_Anzahl=trace_cre_Anzahl[35], trace_cre_Positiv=trace_cre_Positiv[35], trace_cre_Negativ=trace_cre_Negativ[35])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("10.05.2020")
                data <- data.frame(x, trace_chei_Anzahl=trace_chei_Anzahl[35], trace_chei_Positiv=trace_chei_Positiv[35], trace_chei_Negativ=trace_chei_Negativ[35])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            
        }
        else if (input$filterD== "09.05.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("09.05.2020")
                data <- data.frame(x, trace_c_Anzahl = trace_c_Anzahl[34], trace_c_Positiv = trace_c_Positiv[34], trace_c_Negativ = trace_c_Negativ[34])
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
                
                x<-c("09.05.2020")
                data <- data.frame(x, trace_cim_Anzahl=trace_cim_Anzahl[34], trace_cim_Positiv=trace_cim_Positiv[34], trace_cim_Negativ=trace_cim_Negativ[34])
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
                
                x<-c("09.05.2020")
                data <- data.frame(x, trace_cem_Anzahl=trace_cem_Anzahl[34], trace_cem_Positiv=trace_cem_Positiv[34], trace_cem_Negativ=trace_cem_Negativ[34])
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
                
                x<-c("09.05.2020")
                data <- data.frame(x, trace_cra_Anzahl=trace_cra_Anzahl[34], trace_cra_Positiv=trace_cra_Positiv[34], trace_cra_Negativ=trace_cra_Negativ[34])
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
                
                x<-c("09.05.2020")
                data <- data.frame(x, trace_cca_Anzahl=trace_cca_Anzahl[34], trace_cca_Positiv=trace_cca_Positiv[34], trace_cca_Negativ=trace_cca_Negativ[34])
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
                
                x<-c("09.05.2020")
                data <- data.frame(x, trace_cess_Anzahl=trace_cess_Anzahl[34], trace_cess_Positiv=trace_cess_Positiv[34], trace_cess_Negativ=trace_cess_Negativ[34])
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
                
                x<-c("09.05.2020")
                data <- data.frame(x, trace_cmtk_Anzahl=trace_cmtk_Anzahl[34], trace_cmtk_Positiv=trace_cmtk_Positiv[34], trace_cmtk_Negativ=trace_cmtk_Negativ[34])
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("09.05.2020")
                data <- data.frame(x, trace_che_Anzahl=trace_che_Anzahl[34], trace_che_Positiv=trace_che_Positiv[34], trace_che_Negativ=trace_che_Negativ[34])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("09.05.2020")
                data <- data.frame(x, trace_clo_Anzahl=trace_clo_Anzahl[34], trace_clo_Positiv=trace_clo_Positiv[34], trace_clo_Negativ=trace_clo_Negativ[34])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("09.05.2020")
                data <- data.frame(x, trace_cre_Anzahl=trace_cre_Anzahl[34], trace_cre_Positiv=trace_cre_Positiv[34], trace_cre_Negativ=trace_cre_Negativ[34])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("09.05.2020")
                data <- data.frame(x, trace_chei_Anzahl=trace_chei_Anzahl[34], trace_chei_Positiv=trace_chei_Positiv[34], trace_chei_Negativ=trace_chei_Negativ[34])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            
        }
        else if (input$filterD== "08.05.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("08.05.2020")
                data <- data.frame(x, trace_c_Anzahl = trace_c_Anzahl[33], trace_c_Positiv = trace_c_Positiv[33], trace_c_Negativ = trace_c_Negativ[33])
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
                
                x<-c("08.05.2020")
                data <- data.frame(x, trace_cim_Anzahl=trace_cim_Anzahl[33], trace_cim_Positiv=trace_cim_Positiv[33], trace_cim_Negativ=trace_cim_Negativ[33])
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
                
                x<-c("08.05.2020")
                data <- data.frame(x, trace_cem_Anzahl=trace_cem_Anzahl[33], trace_cem_Positiv=trace_cem_Positiv[33], trace_cem_Negativ=trace_cem_Negativ[33])
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
                
                x<-c("08.05.2020")
                data <- data.frame(x, trace_cra_Anzahl=trace_cra_Anzahl[33], trace_cra_Positiv=trace_cra_Positiv[33], trace_cra_Negativ=trace_cra_Negativ[33])
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
                
                x<-c("08.05.2020")
                data <- data.frame(x, trace_cca_Anzahl=trace_cca_Anzahl[33], trace_cca_Positiv=trace_cca_Positiv[33], trace_cca_Negativ=trace_cca_Negativ[33])
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
                
                x<-c("08.05.2020")
                data <- data.frame(x, trace_cess_Anzahl=trace_cess_Anzahl[33], trace_cess_Positiv=trace_cess_Positiv[33], trace_cess_Negativ=trace_cess_Negativ[33])
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
                
                x<-c("08.05.2020")
                data <- data.frame(x, trace_cmtk_Anzahl=trace_cmtk_Anzahl[33], trace_cmtk_Positiv=trace_cmtk_Positiv[33], trace_cmtk_Negativ=trace_cmtk_Negativ[33])
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("08.05.2020")
                data <- data.frame(x, trace_che_Anzahl=trace_che_Anzahl[33], trace_che_Positiv=trace_che_Positiv[33], trace_che_Negativ=trace_che_Negativ[33])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("08.05.2020")
                data <- data.frame(x, trace_clo_Anzahl=trace_clo_Anzahl[33], trace_clo_Positiv=trace_clo_Positiv[33], trace_clo_Negativ=trace_clo_Negativ[33])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("08.05.2020")
                data <- data.frame(x, trace_cre_Anzahl=trace_cre_Anzahl[33], trace_cre_Positiv=trace_cre_Positiv[33], trace_cre_Negativ=trace_cre_Negativ[33])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("08.05.2020")
                data <- data.frame(x, trace_chei_Anzahl=trace_chei_Anzahl[33], trace_chei_Positiv=trace_chei_Positiv[33], trace_chei_Negativ=trace_chei_Negativ[33])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Negativ, name = 'falsch beantwortet',type = 'bar') 
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("07.04.2020")
                data <- data.frame(x, trace_che_Anzahl=trace_che_Anzahl[2], trace_che_Positiv=trace_che_Positiv[2], trace_che_Negativ=trace_che_Negativ[2])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("07.04.2020")
                data <- data.frame(x, trace_clo_Anzahl=trace_clo_Anzahl[2], trace_clo_Positiv=trace_clo_Positiv[2], trace_clo_Negativ=trace_clo_Negativ[2])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("07.04.2020")
                data <- data.frame(x, trace_cre_Anzahl=trace_cre_Anzahl[2], trace_cre_Positiv=trace_cre_Positiv[2], trace_cre_Negativ=trace_cre_Negativ[2])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("07.04.2020")
                data <- data.frame(x, trace_chei_Anzahl=trace_chei_Anzahl[2], trace_chei_Positiv=trace_chei_Positiv[2], trace_chei_Negativ=trace_chei_Negativ[2])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Negativ, name = 'falsch beantwortet',type = 'bar') 
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("08.04.2020")
                data <- data.frame(x, trace_che_Anzahl=trace_che_Anzahl[3], trace_che_Positiv=trace_che_Positiv[3], trace_che_Negativ=trace_che_Negativ[3])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("08.04.2020")
                data <- data.frame(x, trace_clo_Anzahl=trace_clo_Anzahl[3], trace_clo_Positiv=trace_clo_Positiv[3], trace_clo_Negativ=trace_clo_Negativ[3])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("08.04.2020")
                data <- data.frame(x, trace_cre_Anzahl=trace_cre_Anzahl[3], trace_cre_Positiv=trace_cre_Positiv[3], trace_cre_Negativ=trace_cre_Negativ[3])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("08.04.2020")
                data <- data.frame(x, trace_chei_Anzahl=trace_chei_Anzahl[3], trace_chei_Positiv=trace_chei_Positiv[3], trace_chei_Negativ=trace_chei_Negativ[3])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Negativ, name = 'falsch beantwortet',type = 'bar') 
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("09.04.2020")
                data <- data.frame(x, trace_che_Anzahl=trace_che_Anzahl[4], trace_che_Positiv=trace_che_Positiv[4], trace_che_Negativ=trace_che_Negativ[4])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("09.04.2020")
                data <- data.frame(x, trace_clo_Anzahl=trace_clo_Anzahl[4], trace_clo_Positiv=trace_clo_Positiv[4], trace_clo_Negativ=trace_clo_Negativ[4])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("09.04.2020")
                data <- data.frame(x, trace_cre_Anzahl=trace_cre_Anzahl[4], trace_cre_Positiv=trace_cre_Positiv[4], trace_cre_Negativ=trace_cre_Negativ[4])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("09.04.2020")
                data <- data.frame(x, trace_chei_Anzahl=trace_chei_Anzahl[4], trace_chei_Positiv=trace_chei_Positiv[4], trace_chei_Negativ=trace_chei_Negativ[4])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Negativ, name = 'falsch beantwortet',type = 'bar') 
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("10.04.2020")
                data <- data.frame(x, trace_che_Anzahl=trace_che_Anzahl[5], trace_che_Positiv=trace_che_Positiv[5], trace_che_Negativ=trace_che_Negativ[5])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("10.04.2020")
                data <- data.frame(x, trace_clo_Anzahl=trace_clo_Anzahl[5], trace_clo_Positiv=trace_clo_Positiv[5], trace_clo_Negativ=trace_clo_Negativ[5])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("10.04.2020")
                data <- data.frame(x, trace_cre_Anzahl=trace_cre_Anzahl[5], trace_cre_Positiv=trace_cre_Positiv[5], trace_cre_Negativ=trace_cre_Negativ[5])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("10.04.2020")
                data <- data.frame(x, trace_chei_Anzahl=trace_chei_Anzahl[5], trace_chei_Positiv=trace_chei_Positiv[5], trace_chei_Negativ=trace_chei_Negativ[5])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Negativ, name = 'falsch beantwortet',type = 'bar') 
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("11.04.2020")
                data <- data.frame(x, trace_che_Anzahl=trace_che_Anzahl[6], trace_che_Positiv=trace_che_Positiv[6], trace_che_Negativ=trace_che_Negativ[6])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("11.04.2020")
                data <- data.frame(x, trace_clo_Anzahl=trace_clo_Anzahl[6], trace_clo_Positiv=trace_clo_Positiv[6], trace_clo_Negativ=trace_clo_Negativ[6])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("11.04.2020")
                data <- data.frame(x, trace_cre_Anzahl=trace_cre_Anzahl[6], trace_cre_Positiv=trace_cre_Positiv[6], trace_cre_Negativ=trace_cre_Negativ[6])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("11.04.2020")
                data <- data.frame(x, trace_chei_Anzahl=trace_chei_Anzahl[6], trace_chei_Positiv=trace_chei_Positiv[6], trace_chei_Negativ=trace_chei_Negativ[6])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Negativ, name = 'falsch beantwortet',type = 'bar') 
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("12.04.2020")
                data <- data.frame(x, trace_che_Anzahl=trace_che_Anzahl[7], trace_che_Positiv=trace_che_Positiv[7], trace_che_Negativ=trace_che_Negativ[7])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("12.04.2020")
                data <- data.frame(x, trace_clo_Anzahl=trace_clo_Anzahl[7], trace_clo_Positiv=trace_clo_Positiv[7], trace_clo_Negativ=trace_clo_Negativ[7])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("12.04.2020")
                data <- data.frame(x, trace_cre_Anzahl=trace_cre_Anzahl[7], trace_cre_Positiv=trace_cre_Positiv[7], trace_cre_Negativ=trace_cre_Negativ[7])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("12.04.2020")
                data <- data.frame(x, trace_chei_Anzahl=trace_chei_Anzahl[7], trace_chei_Positiv=trace_chei_Positiv[7], trace_chei_Negativ=trace_chei_Negativ[7])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Negativ, name = 'falsch beantwortet',type = 'bar') 
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("13.04.2020")
                data <- data.frame(x, trace_che_Anzahl=trace_che_Anzahl[8], trace_che_Positiv=trace_che_Positiv[8], trace_che_Negativ=trace_che_Negativ[8])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("13.04.2020")
                data <- data.frame(x, trace_clo_Anzahl=trace_clo_Anzahl[8], trace_clo_Positiv=trace_clo_Positiv[8], trace_clo_Negativ=trace_clo_Negativ[8])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("13.04.2020")
                data <- data.frame(x, trace_cre_Anzahl=trace_cre_Anzahl[8], trace_cre_Positiv=trace_cre_Positiv[8], trace_cre_Negativ=trace_cre_Negativ[8])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("13.04.2020")
                data <- data.frame(x, trace_chei_Anzahl=trace_chei_Anzahl[8], trace_chei_Positiv=trace_chei_Positiv[8], trace_chei_Negativ=trace_chei_Negativ[8])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Negativ, name = 'falsch beantwortet',type = 'bar') 
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("14.04.2020")
                data <- data.frame(x, trace_che_Anzahl=trace_che_Anzahl[9], trace_che_Positiv=trace_che_Positiv[9], trace_che_Negativ=trace_che_Negativ[9])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("14.04.2020")
                data <- data.frame(x, trace_clo_Anzahl=trace_clo_Anzahl[9], trace_clo_Positiv=trace_clo_Positiv[9], trace_clo_Negativ=trace_clo_Negativ[9])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("14.04.2020")
                data <- data.frame(x, trace_cre_Anzahl=trace_cre_Anzahl[9], trace_cre_Positiv=trace_cre_Positiv[9], trace_cre_Negativ=trace_cre_Negativ[9])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("14.04.2020")
                data <- data.frame(x, trace_chei_Anzahl=trace_chei_Anzahl[9], trace_chei_Positiv=trace_chei_Positiv[9], trace_chei_Negativ=trace_chei_Negativ[9])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Negativ, name = 'falsch beantwortet',type = 'bar') 
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("15.04.2020")
                data <- data.frame(x, trace_che_Anzahl=trace_che_Anzahl[10], trace_che_Positiv=trace_che_Positiv[10], trace_che_Negativ=trace_che_Negativ[10])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("15.04.2020")
                data <- data.frame(x, trace_clo_Anzahl=trace_clo_Anzahl[10], trace_clo_Positiv=trace_clo_Positiv[10], trace_clo_Negativ=trace_clo_Negativ[10])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("15.04.2020")
                data <- data.frame(x, trace_cre_Anzahl=trace_cre_Anzahl[10], trace_cre_Positiv=trace_cre_Positiv[10], trace_cre_Negativ=trace_cre_Negativ[10])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("15.04.2020")
                data <- data.frame(x, trace_chei_Anzahl=trace_chei_Anzahl[10], trace_chei_Positiv=trace_chei_Positiv[10], trace_chei_Negativ=trace_chei_Negativ[10])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            
        }
        else if (input$filterD== "16.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("16.04.2020")
                data <- data.frame(x, trace_c_Anzahl = trace_c_Anzahl[11], trace_c_Positiv = trace_c_Positiv[11], trace_c_Negativ = trace_c_Negativ[11])
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
                
                x<-c("16.04.2020")
                data <- data.frame(x, trace_cim_Anzahl=trace_cim_Anzahl[11], trace_cim_Positiv=trace_cim_Positiv[11], trace_cim_Negativ=trace_cim_Negativ[11])
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
                
                x<-c("16.04.2020")
                data <- data.frame(x, trace_cem_Anzahl=trace_cem_Anzahl[11], trace_cem_Positiv=trace_cem_Positiv[11], trace_cem_Negativ=trace_cem_Negativ[11])
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
                
                x<-c("16.04.2020")
                data <- data.frame(x, trace_cra_Anzahl=trace_cra_Anzahl[11], trace_cra_Positiv=trace_cra_Positiv[11], trace_cra_Negativ=trace_cra_Negativ[11])
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
                
                x<-c("16.04.2020")
                data <- data.frame(x, trace_cca_Anzahl=trace_cca_Anzahl[11], trace_cca_Positiv=trace_cca_Positiv[11], trace_cca_Negativ=trace_cca_Negativ[11])
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
                
                x<-c("16.04.2020")
                data <- data.frame(x, trace_cess_Anzahl=trace_cess_Anzahl[11], trace_cess_Positiv=trace_cess_Positiv[11], trace_cess_Negativ=trace_cess_Negativ[11])
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
                
                x<-c("16.04.2020")
                data <- data.frame(x, trace_cmtk_Anzahl=trace_cmtk_Anzahl[11], trace_cmtk_Positiv=trace_cmtk_Positiv[11], trace_cmtk_Negativ=trace_cmtk_Negativ[11])
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("16.04.2020")
                data <- data.frame(x, trace_che_Anzahl=trace_che_Anzahl[11], trace_che_Positiv=trace_che_Positiv[11], trace_che_Negativ=trace_che_Negativ[11])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("16.04.2020")
                data <- data.frame(x, trace_clo_Anzahl=trace_clo_Anzahl[11], trace_clo_Positiv=trace_clo_Positiv[11], trace_clo_Negativ=trace_clo_Negativ[11])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("16.04.2020")
                data <- data.frame(x, trace_cre_Anzahl=trace_cre_Anzahl[11], trace_cre_Positiv=trace_cre_Positiv[11], trace_cre_Negativ=trace_cre_Negativ[11])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("16.04.2020")
                data <- data.frame(x, trace_chei_Anzahl=trace_chei_Anzahl[11], trace_chei_Positiv=trace_chei_Positiv[11], trace_chei_Negativ=trace_chei_Negativ[11])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            
        }
        else if (input$filterD== "17.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("17.04.2020")
                data <- data.frame(x, trace_c_Anzahl = trace_c_Anzahl[12], trace_c_Positiv = trace_c_Positiv[12], trace_c_Negativ = trace_c_Negativ[12])
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
                
                x<-c("17.04.2020")
                data <- data.frame(x, trace_cim_Anzahl=trace_cim_Anzahl[12], trace_cim_Positiv=trace_cim_Positiv[12], trace_cim_Negativ=trace_cim_Negativ[12])
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
                
                x<-c("17.04.2020")
                data <- data.frame(x, trace_cem_Anzahl=trace_cem_Anzahl[12], trace_cem_Positiv=trace_cem_Positiv[12], trace_cem_Negativ=trace_cem_Negativ[12])
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
                
                x<-c("17.04.2020")
                data <- data.frame(x, trace_cra_Anzahl=trace_cra_Anzahl[12], trace_cra_Positiv=trace_cra_Positiv[12], trace_cra_Negativ=trace_cra_Negativ[12])
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
                
                x<-c("17.04.2020")
                data <- data.frame(x, trace_cca_Anzahl=trace_cca_Anzahl[12], trace_cca_Positiv=trace_cca_Positiv[12], trace_cca_Negativ=trace_cca_Negativ[12])
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
                
                x<-c("17.04.2020")
                data <- data.frame(x, trace_cess_Anzahl=trace_cess_Anzahl[12], trace_cess_Positiv=trace_cess_Positiv[12], trace_cess_Negativ=trace_cess_Negativ[12])
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
                
                x<-c("17.04.2020")
                data <- data.frame(x, trace_cmtk_Anzahl=trace_cmtk_Anzahl[12], trace_cmtk_Positiv=trace_cmtk_Positiv[12], trace_cmtk_Negativ=trace_cmtk_Negativ[12])
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("17.04.2020")
                data <- data.frame(x, trace_che_Anzahl=trace_che_Anzahl[12], trace_che_Positiv=trace_che_Positiv[12], trace_che_Negativ=trace_che_Negativ[12])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("17.04.2020")
                data <- data.frame(x, trace_clo_Anzahl=trace_clo_Anzahl[12], trace_clo_Positiv=trace_clo_Positiv[12], trace_clo_Negativ=trace_clo_Negativ[12])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("17.04.2020")
                data <- data.frame(x, trace_cre_Anzahl=trace_cre_Anzahl[12], trace_cre_Positiv=trace_cre_Positiv[12], trace_cre_Negativ=trace_cre_Negativ[12])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("17.04.2020")
                data <- data.frame(x, trace_chei_Anzahl=trace_chei_Anzahl[12], trace_chei_Positiv=trace_chei_Positiv[12], trace_chei_Negativ=trace_chei_Negativ[12])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            
        }
        else if (input$filterD== "18.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("18.04.2020")
                data <- data.frame(x, trace_c_Anzahl = trace_c_Anzahl[13], trace_c_Positiv = trace_c_Positiv[13], trace_c_Negativ = trace_c_Negativ[13])
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
                
                x<-c("18.04.2020")
                data <- data.frame(x, trace_cim_Anzahl=trace_cim_Anzahl[13], trace_cim_Positiv=trace_cim_Positiv[13], trace_cim_Negativ=trace_cim_Negativ[13])
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
                
                x<-c("18.04.2020")
                data <- data.frame(x, trace_cem_Anzahl=trace_cem_Anzahl[13], trace_cem_Positiv=trace_cem_Positiv[13], trace_cem_Negativ=trace_cem_Negativ[13])
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
                
                x<-c("18.04.2020")
                data <- data.frame(x, trace_cra_Anzahl=trace_cra_Anzahl[13], trace_cra_Positiv=trace_cra_Positiv[13], trace_cra_Negativ=trace_cra_Negativ[13])
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
                
                x<-c("18.04.2020")
                data <- data.frame(x, trace_cca_Anzahl=trace_cca_Anzahl[13], trace_cca_Positiv=trace_cca_Positiv[13], trace_cca_Negativ=trace_cca_Negativ[13])
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
                
                x<-c("18.04.2020")
                data <- data.frame(x, trace_cess_Anzahl=trace_cess_Anzahl[13], trace_cess_Positiv=trace_cess_Positiv[13], trace_cess_Negativ=trace_cess_Negativ[13])
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
                
                x<-c("18.04.2020")
                data <- data.frame(x, trace_cmtk_Anzahl=trace_cmtk_Anzahl[13], trace_cmtk_Positiv=trace_cmtk_Positiv[13], trace_cmtk_Negativ=trace_cmtk_Negativ[13])
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("18.04.2020")
                data <- data.frame(x, trace_che_Anzahl=trace_che_Anzahl[13], trace_che_Positiv=trace_che_Positiv[13], trace_che_Negativ=trace_che_Negativ[13])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("18.04.2020")
                data <- data.frame(x, trace_clo_Anzahl=trace_clo_Anzahl[13], trace_clo_Positiv=trace_clo_Positiv[13], trace_clo_Negativ=trace_clo_Negativ[13])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("18.04.2020")
                data <- data.frame(x, trace_cre_Anzahl=trace_cre_Anzahl[13], trace_cre_Positiv=trace_cre_Positiv[13], trace_cre_Negativ=trace_cre_Negativ[13])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("18.04.2020")
                data <- data.frame(x, trace_chei_Anzahl=trace_chei_Anzahl[13], trace_chei_Positiv=trace_chei_Positiv[13], trace_chei_Negativ=trace_chei_Negativ[13])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            
        }
        else if (input$filterD== "19.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("19.04.2020")
                data <- data.frame(x, trace_c_Anzahl = trace_c_Anzahl[14], trace_c_Positiv = trace_c_Positiv[14], trace_c_Negativ = trace_c_Negativ[14])
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
                
                x<-c("19.04.2020")
                data <- data.frame(x, trace_cim_Anzahl=trace_cim_Anzahl[14], trace_cim_Positiv=trace_cim_Positiv[14], trace_cim_Negativ=trace_cim_Negativ[14])
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
                
                x<-c("19.04.2020")
                data <- data.frame(x, trace_cem_Anzahl=trace_cem_Anzahl[14], trace_cem_Positiv=trace_cem_Positiv[14], trace_cem_Negativ=trace_cem_Negativ[14])
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
                
                x<-c("19.04.2020")
                data <- data.frame(x, trace_cra_Anzahl=trace_cra_Anzahl[14], trace_cra_Positiv=trace_cra_Positiv[14], trace_cra_Negativ=trace_cra_Negativ[14])
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
                
                x<-c("19.04.2020")
                data <- data.frame(x, trace_cca_Anzahl=trace_cca_Anzahl[14], trace_cca_Positiv=trace_cca_Positiv[14], trace_cca_Negativ=trace_cca_Negativ[14])
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
                
                x<-c("19.04.2020")
                data <- data.frame(x, trace_cess_Anzahl=trace_cess_Anzahl[14], trace_cess_Positiv=trace_cess_Positiv[14], trace_cess_Negativ=trace_cess_Negativ[14])
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
                
                x<-c("19.04.2020")
                data <- data.frame(x, trace_cmtk_Anzahl=trace_cmtk_Anzahl[14], trace_cmtk_Positiv=trace_cmtk_Positiv[14], trace_cmtk_Negativ=trace_cmtk_Negativ[14])
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("19.04.2020")
                data <- data.frame(x, trace_che_Anzahl=trace_che_Anzahl[14], trace_che_Positiv=trace_che_Positiv[14], trace_che_Negativ=trace_che_Negativ[14])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("19.04.2020")
                data <- data.frame(x, trace_clo_Anzahl=trace_clo_Anzahl[14], trace_clo_Positiv=trace_clo_Positiv[14], trace_clo_Negativ=trace_clo_Negativ[14])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("19.04.2020")
                data <- data.frame(x, trace_cre_Anzahl=trace_cre_Anzahl[14], trace_cre_Positiv=trace_cre_Positiv[14], trace_cre_Negativ=trace_cre_Negativ[14])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("19.04.2020")
                data <- data.frame(x, trace_chei_Anzahl=trace_chei_Anzahl[14], trace_chei_Positiv=trace_chei_Positiv[14], trace_chei_Negativ=trace_chei_Negativ[14])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            
        }
        else if (input$filterD== "20.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("20.04.2020")
                data <- data.frame(x, trace_c_Anzahl = trace_c_Anzahl[15], trace_c_Positiv = trace_c_Positiv[15], trace_c_Negativ = trace_c_Negativ[15])
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
                
                x<-c("20.04.2020")
                data <- data.frame(x, trace_cim_Anzahl=trace_cim_Anzahl[15], trace_cim_Positiv=trace_cim_Positiv[15], trace_cim_Negativ=trace_cim_Negativ[15])
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
                
                x<-c("20.04.2020")
                data <- data.frame(x, trace_cem_Anzahl=trace_cem_Anzahl[15], trace_cem_Positiv=trace_cem_Positiv[15], trace_cem_Negativ=trace_cem_Negativ[15])
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
                
                x<-c("20.04.2020")
                data <- data.frame(x, trace_cra_Anzahl=trace_cra_Anzahl[15], trace_cra_Positiv=trace_cra_Positiv[15], trace_cra_Negativ=trace_cra_Negativ[15])
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
                
                x<-c("20.04.2020")
                data <- data.frame(x, trace_cca_Anzahl=trace_cca_Anzahl[15], trace_cca_Positiv=trace_cca_Positiv[15], trace_cca_Negativ=trace_cca_Negativ[15])
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
                
                x<-c("20.04.2020")
                data <- data.frame(x, trace_cess_Anzahl=trace_cess_Anzahl[15], trace_cess_Positiv=trace_cess_Positiv[15], trace_cess_Negativ=trace_cess_Negativ[15])
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
                
                x<-c("20.04.2020")
                data <- data.frame(x, trace_cmtk_Anzahl=trace_cmtk_Anzahl[15], trace_cmtk_Positiv=trace_cmtk_Positiv[15], trace_cmtk_Negativ=trace_cmtk_Negativ[15])
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("20.04.2020")
                data <- data.frame(x, trace_che_Anzahl=trace_che_Anzahl[15], trace_che_Positiv=trace_che_Positiv[15], trace_che_Negativ=trace_che_Negativ[15])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("20.04.2020")
                data <- data.frame(x, trace_clo_Anzahl=trace_clo_Anzahl[15], trace_clo_Positiv=trace_clo_Positiv[15], trace_clo_Negativ=trace_clo_Negativ[15])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("20.04.2020")
                data <- data.frame(x, trace_cre_Anzahl=trace_cre_Anzahl[15], trace_cre_Positiv=trace_cre_Positiv[15], trace_cre_Negativ=trace_cre_Negativ[15])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("20.04.2020")
                data <- data.frame(x, trace_chei_Anzahl=trace_chei_Anzahl[15], trace_chei_Positiv=trace_chei_Positiv[15], trace_chei_Negativ=trace_chei_Negativ[15])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            
        }
        else if (input$filterD== "21.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("21.04.2020")
                data <- data.frame(x, trace_c_Anzahl = trace_c_Anzahl[16], trace_c_Positiv = trace_c_Positiv[16], trace_c_Negativ = trace_c_Negativ[16])
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
                
                x<-c("21.04.2020")
                data <- data.frame(x, trace_cim_Anzahl=trace_cim_Anzahl[16], trace_cim_Positiv=trace_cim_Positiv[16], trace_cim_Negativ=trace_cim_Negativ[16])
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
                
                x<-c("21.04.2020")
                data <- data.frame(x, trace_cem_Anzahl=trace_cem_Anzahl[16], trace_cem_Positiv=trace_cem_Positiv[16], trace_cem_Negativ=trace_cem_Negativ[16])
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
                
                x<-c("21.04.2020")
                data <- data.frame(x, trace_cra_Anzahl=trace_cra_Anzahl[16], trace_cra_Positiv=trace_cra_Positiv[16], trace_cra_Negativ=trace_cra_Negativ[16])
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
                
                x<-c("21.04.2020")
                data <- data.frame(x, trace_cca_Anzahl=trace_cca_Anzahl[16], trace_cca_Positiv=trace_cca_Positiv[16], trace_cca_Negativ=trace_cca_Negativ[16])
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
                
                x<-c("21.04.2020")
                data <- data.frame(x, trace_cess_Anzahl=trace_cess_Anzahl[16], trace_cess_Positiv=trace_cess_Positiv[16], trace_cess_Negativ=trace_cess_Negativ[16])
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
                
                x<-c("21.04.2020")
                data <- data.frame(x, trace_cmtk_Anzahl=trace_cmtk_Anzahl[16], trace_cmtk_Positiv=trace_cmtk_Positiv[16], trace_cmtk_Negativ=trace_cmtk_Negativ[16])
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("21.04.2020")
                data <- data.frame(x, trace_che_Anzahl=trace_che_Anzahl[16], trace_che_Positiv=trace_che_Positiv[16], trace_che_Negativ=trace_che_Negativ[16])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("21.04.2020")
                data <- data.frame(x, trace_clo_Anzahl=trace_clo_Anzahl[16], trace_clo_Positiv=trace_clo_Positiv[16], trace_clo_Negativ=trace_clo_Negativ[16])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("21.04.2020")
                data <- data.frame(x, trace_cre_Anzahl=trace_cre_Anzahl[16], trace_cre_Positiv=trace_cre_Positiv[16], trace_cre_Negativ=trace_cre_Negativ[16])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("21.04.2020")
                data <- data.frame(x, trace_chei_Anzahl=trace_chei_Anzahl[16], trace_chei_Positiv=trace_chei_Positiv[16], trace_chei_Negativ=trace_chei_Negativ[16])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            
        }
        else if (input$filterD== "22.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("22.04.2020")
                data <- data.frame(x, trace_c_Anzahl = trace_c_Anzahl[17], trace_c_Positiv = trace_c_Positiv[17], trace_c_Negativ = trace_c_Negativ[17])
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
                
                x<-c("22.04.2020")
                data <- data.frame(x, trace_cim_Anzahl=trace_cim_Anzahl[17], trace_cim_Positiv=trace_cim_Positiv[17], trace_cim_Negativ=trace_cim_Negativ[17])
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
                
                x<-c("22.04.2020")
                data <- data.frame(x, trace_cem_Anzahl=trace_cem_Anzahl[17], trace_cem_Positiv=trace_cem_Positiv[17], trace_cem_Negativ=trace_cem_Negativ[17])
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
                
                x<-c("22.04.2020")
                data <- data.frame(x, trace_cra_Anzahl=trace_cra_Anzahl[17], trace_cra_Positiv=trace_cra_Positiv[17], trace_cra_Negativ=trace_cra_Negativ[17])
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
                
                x<-c("22.04.2020")
                data <- data.frame(x, trace_cca_Anzahl=trace_cca_Anzahl[17], trace_cca_Positiv=trace_cca_Positiv[17], trace_cca_Negativ=trace_cca_Negativ[17])
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
                
                x<-c("22.04.2020")
                data <- data.frame(x, trace_cess_Anzahl=trace_cess_Anzahl[17], trace_cess_Positiv=trace_cess_Positiv[17], trace_cess_Negativ=trace_cess_Negativ[17])
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
                
                x<-c("22.04.2020")
                data <- data.frame(x, trace_cmtk_Anzahl=trace_cmtk_Anzahl[17], trace_cmtk_Positiv=trace_cmtk_Positiv[17], trace_cmtk_Negativ=trace_cmtk_Negativ[17])
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("22.04.2020")
                data <- data.frame(x, trace_che_Anzahl=trace_che_Anzahl[17], trace_che_Positiv=trace_che_Positiv[17], trace_che_Negativ=trace_che_Negativ[17])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("22.04.2020")
                data <- data.frame(x, trace_clo_Anzahl=trace_clo_Anzahl[17], trace_clo_Positiv=trace_clo_Positiv[17], trace_clo_Negativ=trace_clo_Negativ[17])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("22.04.2020")
                data <- data.frame(x, trace_cre_Anzahl=trace_cre_Anzahl[17], trace_cre_Positiv=trace_cre_Positiv[17], trace_cre_Negativ=trace_cre_Negativ[17])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("22.04.2020")
                data <- data.frame(x, trace_chei_Anzahl=trace_chei_Anzahl[17], trace_chei_Positiv=trace_chei_Positiv[17], trace_chei_Negativ=trace_chei_Negativ[17])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            
        }
        else if (input$filterD== "23.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("23.04.2020")
                data <- data.frame(x, trace_c_Anzahl = trace_c_Anzahl[18], trace_c_Positiv = trace_c_Positiv[18], trace_c_Negativ = trace_c_Negativ[18])
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
                
                x<-c("23.04.2020")
                data <- data.frame(x, trace_cim_Anzahl=trace_cim_Anzahl[18], trace_cim_Positiv=trace_cim_Positiv[18], trace_cim_Negativ=trace_cim_Negativ[18])
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
                
                x<-c("23.04.2020")
                data <- data.frame(x, trace_cem_Anzahl=trace_cem_Anzahl[18], trace_cem_Positiv=trace_cem_Positiv[18], trace_cem_Negativ=trace_cem_Negativ[18])
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
                
                x<-c("23.04.2020")
                data <- data.frame(x, trace_cra_Anzahl=trace_cra_Anzahl[18], trace_cra_Positiv=trace_cra_Positiv[18], trace_cra_Negativ=trace_cra_Negativ[18])
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
                
                x<-c("23.04.2020")
                data <- data.frame(x, trace_cca_Anzahl=trace_cca_Anzahl[18], trace_cca_Positiv=trace_cca_Positiv[18], trace_cca_Negativ=trace_cca_Negativ[18])
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
                
                x<-c("23.04.2020")
                data <- data.frame(x, trace_cess_Anzahl=trace_cess_Anzahl[18], trace_cess_Positiv=trace_cess_Positiv[18], trace_cess_Negativ=trace_cess_Negativ[18])
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
                
                x<-c("23.04.2020")
                data <- data.frame(x, trace_cmtk_Anzahl=trace_cmtk_Anzahl[18], trace_cmtk_Positiv=trace_cmtk_Positiv[18], trace_cmtk_Negativ=trace_cmtk_Negativ[18])
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("23.04.2020")
                data <- data.frame(x, trace_che_Anzahl=trace_che_Anzahl[18], trace_che_Positiv=trace_che_Positiv[18], trace_che_Negativ=trace_che_Negativ[18])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("23.04.2020")
                data <- data.frame(x, trace_clo_Anzahl=trace_clo_Anzahl[18], trace_clo_Positiv=trace_clo_Positiv[18], trace_clo_Negativ=trace_clo_Negativ[18])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("23.04.2020")
                data <- data.frame(x, trace_cre_Anzahl=trace_cre_Anzahl[18], trace_cre_Positiv=trace_cre_Positiv[18], trace_cre_Negativ=trace_cre_Negativ[18])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("23.04.2020")
                data <- data.frame(x, trace_chei_Anzahl=trace_chei_Anzahl[18], trace_chei_Positiv=trace_chei_Positiv[18], trace_chei_Negativ=trace_chei_Negativ[18])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            
        }
        else if (input$filterD== "24.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("24.04.2020")
                data <- data.frame(x, trace_c_Anzahl = trace_c_Anzahl[19], trace_c_Positiv = trace_c_Positiv[19], trace_c_Negativ = trace_c_Negativ[19])
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
                
                x<-c("24.04.2020")
                data <- data.frame(x, trace_cim_Anzahl=trace_cim_Anzahl[19], trace_cim_Positiv=trace_cim_Positiv[19], trace_cim_Negativ=trace_cim_Negativ[19])
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
                
                x<-c("24.04.2020")
                data <- data.frame(x, trace_cem_Anzahl=trace_cem_Anzahl[19], trace_cem_Positiv=trace_cem_Positiv[19], trace_cem_Negativ=trace_cem_Negativ[19])
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
                
                x<-c("24.04.2020")
                data <- data.frame(x, trace_cra_Anzahl=trace_cra_Anzahl[19], trace_cra_Positiv=trace_cra_Positiv[19], trace_cra_Negativ=trace_cra_Negativ[19])
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
                
                x<-c("24.04.2020")
                data <- data.frame(x, trace_cca_Anzahl=trace_cca_Anzahl[19], trace_cca_Positiv=trace_cca_Positiv[19], trace_cca_Negativ=trace_cca_Negativ[19])
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
                
                x<-c("24.04.2020")
                data <- data.frame(x, trace_cess_Anzahl=trace_cess_Anzahl[19], trace_cess_Positiv=trace_cess_Positiv[19], trace_cess_Negativ=trace_cess_Negativ[19])
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
                
                x<-c("24.04.2020")
                data <- data.frame(x, trace_cmtk_Anzahl=trace_cmtk_Anzahl[19], trace_cmtk_Positiv=trace_cmtk_Positiv[19], trace_cmtk_Negativ=trace_cmtk_Negativ[19])
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("24.04.2020")
                data <- data.frame(x, trace_che_Anzahl=trace_che_Anzahl[19], trace_che_Positiv=trace_che_Positiv[19], trace_che_Negativ=trace_che_Negativ[19])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("24.04.2020")
                data <- data.frame(x, trace_clo_Anzahl=trace_clo_Anzahl[19], trace_clo_Positiv=trace_clo_Positiv[19], trace_clo_Negativ=trace_clo_Negativ[19])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("24.04.2020")
                data <- data.frame(x, trace_cre_Anzahl=trace_cre_Anzahl[19], trace_cre_Positiv=trace_cre_Positiv[19], trace_cre_Negativ=trace_cre_Negativ[19])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("24.04.2020")
                data <- data.frame(x, trace_chei_Anzahl=trace_chei_Anzahl[19], trace_chei_Positiv=trace_chei_Positiv[19], trace_chei_Negativ=trace_chei_Negativ[19])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            
        }
        else if (input$filterD== "25.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("25.04.2020")
                data <- data.frame(x, trace_c_Anzahl = trace_c_Anzahl[20], trace_c_Positiv = trace_c_Positiv[20], trace_c_Negativ = trace_c_Negativ[20])
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
                
                x<-c("25.04.2020")
                data <- data.frame(x, trace_cim_Anzahl=trace_cim_Anzahl[20], trace_cim_Positiv=trace_cim_Positiv[20], trace_cim_Negativ=trace_cim_Negativ[20])
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
                
                x<-c("25.04.2020")
                data <- data.frame(x, trace_cem_Anzahl=trace_cem_Anzahl[20], trace_cem_Positiv=trace_cem_Positiv[20], trace_cem_Negativ=trace_cem_Negativ[20])
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
                
                x<-c("25.04.2020")
                data <- data.frame(x, trace_cra_Anzahl=trace_cra_Anzahl[20], trace_cra_Positiv=trace_cra_Positiv[20], trace_cra_Negativ=trace_cra_Negativ[20])
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
                
                x<-c("25.04.2020")
                data <- data.frame(x, trace_cca_Anzahl=trace_cca_Anzahl[20], trace_cca_Positiv=trace_cca_Positiv[20], trace_cca_Negativ=trace_cca_Negativ[20])
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
                
                x<-c("25.04.2020")
                data <- data.frame(x, trace_cess_Anzahl=trace_cess_Anzahl[20], trace_cess_Positiv=trace_cess_Positiv[20], trace_cess_Negativ=trace_cess_Negativ[20])
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
                
                x<-c("25.04.2020")
                data <- data.frame(x, trace_cmtk_Anzahl=trace_cmtk_Anzahl[20], trace_cmtk_Positiv=trace_cmtk_Positiv[20], trace_cmtk_Negativ=trace_cmtk_Negativ[20])
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("25.04.2020")
                data <- data.frame(x, trace_che_Anzahl=trace_che_Anzahl[20], trace_che_Positiv=trace_che_Positiv[20], trace_che_Negativ=trace_che_Negativ[20])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("25.04.2020")
                data <- data.frame(x, trace_clo_Anzahl=trace_clo_Anzahl[20], trace_clo_Positiv=trace_clo_Positiv[20], trace_clo_Negativ=trace_clo_Negativ[20])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("25.04.2020")
                data <- data.frame(x, trace_cre_Anzahl=trace_cre_Anzahl[20], trace_cre_Positiv=trace_cre_Positiv[20], trace_cre_Negativ=trace_cre_Negativ[20])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("25.04.2020")
                data <- data.frame(x, trace_chei_Anzahl=trace_chei_Anzahl[20], trace_chei_Positiv=trace_chei_Positiv[20], trace_chei_Negativ=trace_chei_Negativ[20])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            
        }
        else if (input$filterD== "26.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("26.04.2020")
                data <- data.frame(x, trace_c_Anzahl = trace_c_Anzahl[21], trace_c_Positiv = trace_c_Positiv[21], trace_c_Negativ = trace_c_Negativ[21])
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
                
                x<-c("26.04.2020")
                data <- data.frame(x, trace_cim_Anzahl=trace_cim_Anzahl[21], trace_cim_Positiv=trace_cim_Positiv[21], trace_cim_Negativ=trace_cim_Negativ[21])
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
                
                x<-c("26.04.2020")
                data <- data.frame(x, trace_cem_Anzahl=trace_cem_Anzahl[21], trace_cem_Positiv=trace_cem_Positiv[21], trace_cem_Negativ=trace_cem_Negativ[21])
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
                
                x<-c("26.04.2020")
                data <- data.frame(x, trace_cra_Anzahl=trace_cra_Anzahl[21], trace_cra_Positiv=trace_cra_Positiv[21], trace_cra_Negativ=trace_cra_Negativ[21])
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
                
                x<-c("26.04.2020")
                data <- data.frame(x, trace_cca_Anzahl=trace_cca_Anzahl[21], trace_cca_Positiv=trace_cca_Positiv[21], trace_cca_Negativ=trace_cca_Negativ[21])
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
                
                x<-c("26.04.2020")
                data <- data.frame(x, trace_cess_Anzahl=trace_cess_Anzahl[21], trace_cess_Positiv=trace_cess_Positiv[21], trace_cess_Negativ=trace_cess_Negativ[21])
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
                
                x<-c("26.04.2020")
                data <- data.frame(x, trace_cmtk_Anzahl=trace_cmtk_Anzahl[21], trace_cmtk_Positiv=trace_cmtk_Positiv[21], trace_cmtk_Negativ=trace_cmtk_Negativ[21])
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("26.04.2020")
                data <- data.frame(x, trace_che_Anzahl=trace_che_Anzahl[21], trace_che_Positiv=trace_che_Positiv[21], trace_che_Negativ=trace_che_Negativ[21])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("26.04.2020")
                data <- data.frame(x, trace_clo_Anzahl=trace_clo_Anzahl[21], trace_clo_Positiv=trace_clo_Positiv[21], trace_clo_Negativ=trace_clo_Negativ[21])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("26.04.2020")
                data <- data.frame(x, trace_cre_Anzahl=trace_cre_Anzahl[21], trace_cre_Positiv=trace_cre_Positiv[21], trace_cre_Negativ=trace_cre_Negativ[21])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("26.04.2020")
                data <- data.frame(x, trace_chei_Anzahl=trace_chei_Anzahl[21], trace_chei_Positiv=trace_chei_Positiv[21], trace_chei_Negativ=trace_chei_Negativ[21])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            
        }
        else if (input$filterD== "27.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("27.04.2020")
                data <- data.frame(x, trace_c_Anzahl = trace_c_Anzahl[22], trace_c_Positiv = trace_c_Positiv[22], trace_c_Negativ = trace_c_Negativ[22])
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
                
                x<-c("27.04.2020")
                data <- data.frame(x, trace_cim_Anzahl=trace_cim_Anzahl[22], trace_cim_Positiv=trace_cim_Positiv[22], trace_cim_Negativ=trace_cim_Negativ[22])
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
                
                x<-c("27.04.2020")
                data <- data.frame(x, trace_cem_Anzahl=trace_cem_Anzahl[22], trace_cem_Positiv=trace_cem_Positiv[22], trace_cem_Negativ=trace_cem_Negativ[22])
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
                
                x<-c("27.04.2020")
                data <- data.frame(x, trace_cra_Anzahl=trace_cra_Anzahl[22], trace_cra_Positiv=trace_cra_Positiv[22], trace_cra_Negativ=trace_cra_Negativ[22])
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
                
                x<-c("27.04.2020")
                data <- data.frame(x, trace_cca_Anzahl=trace_cca_Anzahl[22], trace_cca_Positiv=trace_cca_Positiv[22], trace_cca_Negativ=trace_cca_Negativ[22])
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
                
                x<-c("27.04.2020")
                data <- data.frame(x, trace_cess_Anzahl=trace_cess_Anzahl[22], trace_cess_Positiv=trace_cess_Positiv[22], trace_cess_Negativ=trace_cess_Negativ[22])
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
                
                x<-c("27.04.2020")
                data <- data.frame(x, trace_cmtk_Anzahl=trace_cmtk_Anzahl[22], trace_cmtk_Positiv=trace_cmtk_Positiv[22], trace_cmtk_Negativ=trace_cmtk_Negativ[22])
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("27.04.2020")
                data <- data.frame(x, trace_che_Anzahl=trace_che_Anzahl[22], trace_che_Positiv=trace_che_Positiv[22], trace_che_Negativ=trace_che_Negativ[22])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("27.04.2020")
                data <- data.frame(x, trace_clo_Anzahl=trace_clo_Anzahl[22], trace_clo_Positiv=trace_clo_Positiv[22], trace_clo_Negativ=trace_clo_Negativ[22])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("27.04.2020")
                data <- data.frame(x, trace_cre_Anzahl=trace_cre_Anzahl[22], trace_cre_Positiv=trace_cre_Positiv[22], trace_cre_Negativ=trace_cre_Negativ[22])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("27.04.2020")
                data <- data.frame(x, trace_chei_Anzahl=trace_chei_Anzahl[22], trace_chei_Positiv=trace_chei_Positiv[22], trace_chei_Negativ=trace_chei_Negativ[22])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            
        }
        else if (input$filterD== "28.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("28.04.2020")
                data <- data.frame(x, trace_c_Anzahl = trace_c_Anzahl[23], trace_c_Positiv = trace_c_Positiv[23], trace_c_Negativ = trace_c_Negativ[23])
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
                
                x<-c("28.04.2020")
                data <- data.frame(x, trace_cim_Anzahl=trace_cim_Anzahl[23], trace_cim_Positiv=trace_cim_Positiv[23], trace_cim_Negativ=trace_cim_Negativ[23])
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
                
                x<-c("28.04.2020")
                data <- data.frame(x, trace_cem_Anzahl=trace_cem_Anzahl[23], trace_cem_Positiv=trace_cem_Positiv[23], trace_cem_Negativ=trace_cem_Negativ[23])
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
                
                x<-c("28.04.2020")
                data <- data.frame(x, trace_cra_Anzahl=trace_cra_Anzahl[23], trace_cra_Positiv=trace_cra_Positiv[23], trace_cra_Negativ=trace_cra_Negativ[23])
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
                
                x<-c("28.04.2020")
                data <- data.frame(x, trace_cca_Anzahl=trace_cca_Anzahl[23], trace_cca_Positiv=trace_cca_Positiv[23], trace_cca_Negativ=trace_cca_Negativ[23])
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
                
                x<-c("28.04.2020")
                data <- data.frame(x, trace_cess_Anzahl=trace_cess_Anzahl[23], trace_cess_Positiv=trace_cess_Positiv[23], trace_cess_Negativ=trace_cess_Negativ[23])
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
                
                x<-c("28.04.2020")
                data <- data.frame(x, trace_cmtk_Anzahl=trace_cmtk_Anzahl[23], trace_cmtk_Positiv=trace_cmtk_Positiv[23], trace_cmtk_Negativ=trace_cmtk_Negativ[23])
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("28.04.2020")
                data <- data.frame(x, trace_che_Anzahl=trace_che_Anzahl[23], trace_che_Positiv=trace_che_Positiv[23], trace_che_Negativ=trace_che_Negativ[23])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("28.04.2020")
                data <- data.frame(x, trace_clo_Anzahl=trace_clo_Anzahl[23], trace_clo_Positiv=trace_clo_Positiv[23], trace_clo_Negativ=trace_clo_Negativ[23])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("28.04.2020")
                data <- data.frame(x, trace_cre_Anzahl=trace_cre_Anzahl[23], trace_cre_Positiv=trace_cre_Positiv[23], trace_cre_Negativ=trace_cre_Negativ[23])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("28.04.2020")
                data <- data.frame(x, trace_chei_Anzahl=trace_chei_Anzahl[23], trace_chei_Positiv=trace_chei_Positiv[23], trace_chei_Negativ=trace_chei_Negativ[23])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            
        }
        else if (input$filterD== "29.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("29.04.2020")
                data <- data.frame(x, trace_c_Anzahl = trace_c_Anzahl[24], trace_c_Positiv = trace_c_Positiv[24], trace_c_Negativ = trace_c_Negativ[24])
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
                
                x<-c("29.04.2020")
                data <- data.frame(x, trace_cim_Anzahl=trace_cim_Anzahl[24], trace_cim_Positiv=trace_cim_Positiv[24], trace_cim_Negativ=trace_cim_Negativ[24])
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
                
                x<-c("29.04.2020")
                data <- data.frame(x, trace_cem_Anzahl=trace_cem_Anzahl[24], trace_cem_Positiv=trace_cem_Positiv[24], trace_cem_Negativ=trace_cem_Negativ[24])
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
                
                x<-c("29.04.2020")
                data <- data.frame(x, trace_cra_Anzahl=trace_cra_Anzahl[24], trace_cra_Positiv=trace_cra_Positiv[24], trace_cra_Negativ=trace_cra_Negativ[24])
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
                
                x<-c("29.04.2020")
                data <- data.frame(x, trace_cca_Anzahl=trace_cca_Anzahl[24], trace_cca_Positiv=trace_cca_Positiv[24], trace_cca_Negativ=trace_cca_Negativ[24])
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
                
                x<-c("29.04.2020")
                data <- data.frame(x, trace_cess_Anzahl=trace_cess_Anzahl[24], trace_cess_Positiv=trace_cess_Positiv[24], trace_cess_Negativ=trace_cess_Negativ[24])
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
                
                x<-c("29.04.2020")
                data <- data.frame(x, trace_cmtk_Anzahl=trace_cmtk_Anzahl[24], trace_cmtk_Positiv=trace_cmtk_Positiv[24], trace_cmtk_Negativ=trace_cmtk_Negativ[24])
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("29.04.2020")
                data <- data.frame(x, trace_che_Anzahl=trace_che_Anzahl[24], trace_che_Positiv=trace_che_Positiv[24], trace_che_Negativ=trace_che_Negativ[24])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("29.04.2020")
                data <- data.frame(x, trace_clo_Anzahl=trace_clo_Anzahl[24], trace_clo_Positiv=trace_clo_Positiv[24], trace_clo_Negativ=trace_clo_Negativ[24])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("29.04.2020")
                data <- data.frame(x, trace_cre_Anzahl=trace_cre_Anzahl[24], trace_cre_Positiv=trace_cre_Positiv[24], trace_cre_Negativ=trace_cre_Negativ[24])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("29.04.2020")
                data <- data.frame(x, trace_chei_Anzahl=trace_chei_Anzahl[24], trace_chei_Positiv=trace_chei_Positiv[24], trace_chei_Negativ=trace_chei_Negativ[24])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            
        }
        else if (input$filterD== "30.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("30.04.2020")
                data <- data.frame(x, trace_c_Anzahl = trace_c_Anzahl[25], trace_c_Positiv = trace_c_Positiv[25], trace_c_Negativ = trace_c_Negativ[25])
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
                
                x<-c("30.04.2020")
                data <- data.frame(x, trace_cim_Anzahl=trace_cim_Anzahl[25], trace_cim_Positiv=trace_cim_Positiv[25], trace_cim_Negativ=trace_cim_Negativ[25])
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
                
                x<-c("30.04.2020")
                data <- data.frame(x, trace_cem_Anzahl=trace_cem_Anzahl[25], trace_cem_Positiv=trace_cem_Positiv[25], trace_cem_Negativ=trace_cem_Negativ[25])
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
                
                x<-c("30.04.2020")
                data <- data.frame(x, trace_cra_Anzahl=trace_cra_Anzahl[25], trace_cra_Positiv=trace_cra_Positiv[25], trace_cra_Negativ=trace_cra_Negativ[25])
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
                
                x<-c("30.04.2020")
                data <- data.frame(x, trace_cca_Anzahl=trace_cca_Anzahl[25], trace_cca_Positiv=trace_cca_Positiv[25], trace_cca_Negativ=trace_cca_Negativ[25])
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
                
                x<-c("30.04.2020")
                data <- data.frame(x, trace_cess_Anzahl=trace_cess_Anzahl[25], trace_cess_Positiv=trace_cess_Positiv[25], trace_cess_Negativ=trace_cess_Negativ[25])
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
                
                x<-c("30.04.2020")
                data <- data.frame(x, trace_cmtk_Anzahl=trace_cmtk_Anzahl[25], trace_cmtk_Positiv=trace_cmtk_Positiv[25], trace_cmtk_Negativ=trace_cmtk_Negativ[25])
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("30.04.2020")
                data <- data.frame(x, trace_che_Anzahl=trace_che_Anzahl[25], trace_che_Positiv=trace_che_Positiv[25], trace_che_Negativ=trace_che_Negativ[25])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("30.04.2020")
                data <- data.frame(x, trace_clo_Anzahl=trace_clo_Anzahl[25], trace_clo_Positiv=trace_clo_Positiv[25], trace_clo_Negativ=trace_clo_Negativ[25])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("30.04.2020")
                data <- data.frame(x, trace_cre_Anzahl=trace_cre_Anzahl[25], trace_cre_Positiv=trace_cre_Positiv[25], trace_cre_Negativ=trace_cre_Negativ[25])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("30.04.2020")
                data <- data.frame(x, trace_chei_Anzahl=trace_chei_Anzahl[25], trace_chei_Positiv=trace_chei_Positiv[25], trace_chei_Negativ=trace_chei_Negativ[25])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            
        }
        else if (input$filterD== "01.05.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("01.05.2020")
                data <- data.frame(x, trace_c_Anzahl = trace_c_Anzahl[26], trace_c_Positiv = trace_c_Positiv[26], trace_c_Negativ = trace_c_Negativ[26])
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
                
                x<-c("01.05.2020")
                data <- data.frame(x, trace_cim_Anzahl=trace_cim_Anzahl[26], trace_cim_Positiv=trace_cim_Positiv[26], trace_cim_Negativ=trace_cim_Negativ[26])
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
                
                x<-c("01.05.2020")
                data <- data.frame(x, trace_cem_Anzahl=trace_cem_Anzahl[26], trace_cem_Positiv=trace_cem_Positiv[26], trace_cem_Negativ=trace_cem_Negativ[26])
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
                
                x<-c("01.05.2020")
                data <- data.frame(x, trace_cra_Anzahl=trace_cra_Anzahl[26], trace_cra_Positiv=trace_cra_Positiv[26], trace_cra_Negativ=trace_cra_Negativ[26])
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
                
                x<-c("01.05.2020")
                data <- data.frame(x, trace_cca_Anzahl=trace_cca_Anzahl[26], trace_cca_Positiv=trace_cca_Positiv[26], trace_cca_Negativ=trace_cca_Negativ[26])
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
                
                x<-c("01.05.2020")
                data <- data.frame(x, trace_cess_Anzahl=trace_cess_Anzahl[26], trace_cess_Positiv=trace_cess_Positiv[26], trace_cess_Negativ=trace_cess_Negativ[26])
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
                
                x<-c("01.05.2020")
                data <- data.frame(x, trace_cmtk_Anzahl=trace_cmtk_Anzahl[26], trace_cmtk_Positiv=trace_cmtk_Positiv[26], trace_cmtk_Negativ=trace_cmtk_Negativ[26])
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("01.05.2020")
                data <- data.frame(x, trace_che_Anzahl=trace_che_Anzahl[26], trace_che_Positiv=trace_che_Positiv[26], trace_che_Negativ=trace_che_Negativ[26])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("01.05.2020")
                data <- data.frame(x, trace_clo_Anzahl=trace_clo_Anzahl[26], trace_clo_Positiv=trace_clo_Positiv[26], trace_clo_Negativ=trace_clo_Negativ[26])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("01.05.2020")
                data <- data.frame(x, trace_cre_Anzahl=trace_cre_Anzahl[26], trace_cre_Positiv=trace_cre_Positiv[26], trace_cre_Negativ=trace_cre_Negativ[26])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("01.05.2020")
                data <- data.frame(x, trace_chei_Anzahl=trace_chei_Anzahl[26], trace_chei_Positiv=trace_chei_Positiv[26], trace_chei_Negativ=trace_chei_Negativ[26])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            
        }
        else if (input$filterD== "02.05.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("02.05.2020")
                data <- data.frame(x, trace_c_Anzahl = trace_c_Anzahl[27], trace_c_Positiv = trace_c_Positiv[27], trace_c_Negativ = trace_c_Negativ[27])
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
                
                x<-c("02.05.2020")
                data <- data.frame(x, trace_cim_Anzahl=trace_cim_Anzahl[27], trace_cim_Positiv=trace_cim_Positiv[27], trace_cim_Negativ=trace_cim_Negativ[27])
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
                
                x<-c("02.05.2020")
                data <- data.frame(x, trace_cem_Anzahl=trace_cem_Anzahl[27], trace_cem_Positiv=trace_cem_Positiv[27], trace_cem_Negativ=trace_cem_Negativ[27])
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
                
                x<-c("02.05.2020")
                data <- data.frame(x, trace_cra_Anzahl=trace_cra_Anzahl[27], trace_cra_Positiv=trace_cra_Positiv[27], trace_cra_Negativ=trace_cra_Negativ[27])
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
                
                x<-c("02.05.2020")
                data <- data.frame(x, trace_cca_Anzahl=trace_cca_Anzahl[27], trace_cca_Positiv=trace_cca_Positiv[27], trace_cca_Negativ=trace_cca_Negativ[27])
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
                
                x<-c("02.05.2020")
                data <- data.frame(x, trace_cess_Anzahl=trace_cess_Anzahl[27], trace_cess_Positiv=trace_cess_Positiv[27], trace_cess_Negativ=trace_cess_Negativ[27])
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
                
                x<-c("02.05.2020")
                data <- data.frame(x, trace_cmtk_Anzahl=trace_cmtk_Anzahl[27], trace_cmtk_Positiv=trace_cmtk_Positiv[27], trace_cmtk_Negativ=trace_cmtk_Negativ[27])
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("02.05.2020")
                data <- data.frame(x, trace_che_Anzahl=trace_che_Anzahl[27], trace_che_Positiv=trace_che_Positiv[27], trace_che_Negativ=trace_che_Negativ[27])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("02.05.2020")
                data <- data.frame(x, trace_clo_Anzahl=trace_clo_Anzahl[27], trace_clo_Positiv=trace_clo_Positiv[27], trace_clo_Negativ=trace_clo_Negativ[27])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("02.05.2020")
                data <- data.frame(x, trace_cre_Anzahl=trace_cre_Anzahl[27], trace_cre_Positiv=trace_cre_Positiv[27], trace_cre_Negativ=trace_cre_Negativ[27])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("02.05.2020")
                data <- data.frame(x, trace_chei_Anzahl=trace_chei_Anzahl[27], trace_chei_Positiv=trace_chei_Positiv[27], trace_chei_Negativ=trace_chei_Negativ[27])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            
        }
        else if (input$filterD== "03.05.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("03.05.2020")
                data <- data.frame(x, trace_c_Anzahl = trace_c_Anzahl[28], trace_c_Positiv = trace_c_Positiv[28], trace_c_Negativ = trace_c_Negativ[28])
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
                
                x<-c("03.05.2020")
                data <- data.frame(x, trace_cim_Anzahl=trace_cim_Anzahl[28], trace_cim_Positiv=trace_cim_Positiv[28], trace_cim_Negativ=trace_cim_Negativ[28])
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
                
                x<-c("03.05.2020")
                data <- data.frame(x, trace_cem_Anzahl=trace_cem_Anzahl[28], trace_cem_Positiv=trace_cem_Positiv[28], trace_cem_Negativ=trace_cem_Negativ[28])
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
                
                x<-c("03.05.2020")
                data <- data.frame(x, trace_cra_Anzahl=trace_cra_Anzahl[28], trace_cra_Positiv=trace_cra_Positiv[28], trace_cra_Negativ=trace_cra_Negativ[28])
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
                
                x<-c("03.05.2020")
                data <- data.frame(x, trace_cca_Anzahl=trace_cca_Anzahl[28], trace_cca_Positiv=trace_cca_Positiv[28], trace_cca_Negativ=trace_cca_Negativ[28])
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
                
                x<-c("03.05.2020")
                data <- data.frame(x, trace_cess_Anzahl=trace_cess_Anzahl[28], trace_cess_Positiv=trace_cess_Positiv[28], trace_cess_Negativ=trace_cess_Negativ[28])
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
                
                x<-c("03.05.2020")
                data <- data.frame(x, trace_cmtk_Anzahl=trace_cmtk_Anzahl[28], trace_cmtk_Positiv=trace_cmtk_Positiv[28], trace_cmtk_Negativ=trace_cmtk_Negativ[28])
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("03.05.2020")
                data <- data.frame(x, trace_che_Anzahl=trace_che_Anzahl[28], trace_che_Positiv=trace_che_Positiv[28], trace_che_Negativ=trace_che_Negativ[28])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("03.05.2020")
                data <- data.frame(x, trace_clo_Anzahl=trace_clo_Anzahl[28], trace_clo_Positiv=trace_clo_Positiv[28], trace_clo_Negativ=trace_clo_Negativ[28])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("03.05.2020")
                data <- data.frame(x, trace_cre_Anzahl=trace_cre_Anzahl[28], trace_cre_Positiv=trace_cre_Positiv[28], trace_cre_Negativ=trace_cre_Negativ[28])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("03.05.2020")
                data <- data.frame(x, trace_chei_Anzahl=trace_chei_Anzahl[28], trace_chei_Positiv=trace_chei_Positiv[28], trace_chei_Negativ=trace_chei_Negativ[28])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            
        }
        else if (input$filterD== "04.05.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("04.05.2020")
                data <- data.frame(x, trace_c_Anzahl = trace_c_Anzahl[29], trace_c_Positiv = trace_c_Positiv[29], trace_c_Negativ = trace_c_Negativ[29])
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
                
                x<-c("04.05.2020")
                data <- data.frame(x, trace_cim_Anzahl=trace_cim_Anzahl[29], trace_cim_Positiv=trace_cim_Positiv[29], trace_cim_Negativ=trace_cim_Negativ[29])
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
                
                x<-c("04.05.2020")
                data <- data.frame(x, trace_cem_Anzahl=trace_cem_Anzahl[29], trace_cem_Positiv=trace_cem_Positiv[29], trace_cem_Negativ=trace_cem_Negativ[29])
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
                
                x<-c("04.05.2020")
                data <- data.frame(x, trace_cra_Anzahl=trace_cra_Anzahl[29], trace_cra_Positiv=trace_cra_Positiv[29], trace_cra_Negativ=trace_cra_Negativ[29])
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
                
                x<-c("04.05.2020")
                data <- data.frame(x, trace_cca_Anzahl=trace_cca_Anzahl[29], trace_cca_Positiv=trace_cca_Positiv[29], trace_cca_Negativ=trace_cca_Negativ[29])
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
                
                x<-c("04.05.2020")
                data <- data.frame(x, trace_cess_Anzahl=trace_cess_Anzahl[29], trace_cess_Positiv=trace_cess_Positiv[29], trace_cess_Negativ=trace_cess_Negativ[29])
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
                
                x<-c("04.05.2020")
                data <- data.frame(x, trace_cmtk_Anzahl=trace_cmtk_Anzahl[29], trace_cmtk_Positiv=trace_cmtk_Positiv[29], trace_cmtk_Negativ=trace_cmtk_Negativ[29])
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("04.05.2020")
                data <- data.frame(x, trace_che_Anzahl=trace_che_Anzahl[29], trace_che_Positiv=trace_che_Positiv[29], trace_che_Negativ=trace_che_Negativ[29])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("04.05.2020")
                data <- data.frame(x, trace_clo_Anzahl=trace_clo_Anzahl[29], trace_clo_Positiv=trace_clo_Positiv[29], trace_clo_Negativ=trace_clo_Negativ[29])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("04.05.2020")
                data <- data.frame(x, trace_cre_Anzahl=trace_cre_Anzahl[29], trace_cre_Positiv=trace_cre_Positiv[29], trace_cre_Negativ=trace_cre_Negativ[29])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("04.05.2020")
                data <- data.frame(x, trace_chei_Anzahl=trace_chei_Anzahl[29], trace_chei_Positiv=trace_chei_Positiv[29], trace_chei_Negativ=trace_chei_Negativ[29])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            
        }
        else if (input$filterD== "05.05.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("05.05.2020")
                data <- data.frame(x, trace_c_Anzahl = trace_c_Anzahl[30], trace_c_Positiv = trace_c_Positiv[30], trace_c_Negativ = trace_c_Negativ[30])
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
                
                x<-c("05.05.2020")
                data <- data.frame(x, trace_cim_Anzahl=trace_cim_Anzahl[30], trace_cim_Positiv=trace_cim_Positiv[30], trace_cim_Negativ=trace_cim_Negativ[30])
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
                
                x<-c("05.05.2020")
                data <- data.frame(x, trace_cem_Anzahl=trace_cem_Anzahl[30], trace_cem_Positiv=trace_cem_Positiv[30], trace_cem_Negativ=trace_cem_Negativ[30])
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
                
                x<-c("05.05.2020")
                data <- data.frame(x, trace_cra_Anzahl=trace_cra_Anzahl[30], trace_cra_Positiv=trace_cra_Positiv[30], trace_cra_Negativ=trace_cra_Negativ[30])
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
                
                x<-c("05.05.2020")
                data <- data.frame(x, trace_cca_Anzahl=trace_cca_Anzahl[30], trace_cca_Positiv=trace_cca_Positiv[30], trace_cca_Negativ=trace_cca_Negativ[30])
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
                
                x<-c("05.05.2020")
                data <- data.frame(x, trace_cess_Anzahl=trace_cess_Anzahl[30], trace_cess_Positiv=trace_cess_Positiv[30], trace_cess_Negativ=trace_cess_Negativ[30])
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
                
                x<-c("05.05.2020")
                data <- data.frame(x, trace_cmtk_Anzahl=trace_cmtk_Anzahl[30], trace_cmtk_Positiv=trace_cmtk_Positiv[30], trace_cmtk_Negativ=trace_cmtk_Negativ[30])
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("05.05.2020")
                data <- data.frame(x, trace_che_Anzahl=trace_che_Anzahl[30], trace_che_Positiv=trace_che_Positiv[30], trace_che_Negativ=trace_che_Negativ[30])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("05.05.2020")
                data <- data.frame(x, trace_clo_Anzahl=trace_clo_Anzahl[30], trace_clo_Positiv=trace_clo_Positiv[30], trace_clo_Negativ=trace_clo_Negativ[30])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("05.05.2020")
                data <- data.frame(x, trace_cre_Anzahl=trace_cre_Anzahl[30], trace_cre_Positiv=trace_cre_Positiv[30], trace_cre_Negativ=trace_cre_Negativ[30])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("05.05.2020")
                data <- data.frame(x, trace_chei_Anzahl=trace_chei_Anzahl[30], trace_chei_Positiv=trace_chei_Positiv[30], trace_chei_Negativ=trace_chei_Negativ[30])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            
        }
        else if (input$filterD== "06.05.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("06.05.2020")
                data <- data.frame(x, trace_c_Anzahl = trace_c_Anzahl[31], trace_c_Positiv = trace_c_Positiv[31], trace_c_Negativ = trace_c_Negativ[31])
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
                
                x<-c("06.05.2020")
                data <- data.frame(x, trace_cim_Anzahl=trace_cim_Anzahl[31], trace_cim_Positiv=trace_cim_Positiv[31], trace_cim_Negativ=trace_cim_Negativ[31])
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
                
                x<-c("06.05.2020")
                data <- data.frame(x, trace_cem_Anzahl=trace_cem_Anzahl[31], trace_cem_Positiv=trace_cem_Positiv[31], trace_cem_Negativ=trace_cem_Negativ[31])
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
                
                x<-c("06.05.2020")
                data <- data.frame(x, trace_cra_Anzahl=trace_cra_Anzahl[31], trace_cra_Positiv=trace_cra_Positiv[31], trace_cra_Negativ=trace_cra_Negativ[31])
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
                
                x<-c("06.05.2020")
                data <- data.frame(x, trace_cca_Anzahl=trace_cca_Anzahl[31], trace_cca_Positiv=trace_cca_Positiv[31], trace_cca_Negativ=trace_cca_Negativ[31])
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
                
                x<-c("06.05.2020")
                data <- data.frame(x, trace_cess_Anzahl=trace_cess_Anzahl[31], trace_cess_Positiv=trace_cess_Positiv[31], trace_cess_Negativ=trace_cess_Negativ[31])
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
                
                x<-c("06.05.2020")
                data <- data.frame(x, trace_cmtk_Anzahl=trace_cmtk_Anzahl[31], trace_cmtk_Positiv=trace_cmtk_Positiv[31], trace_cmtk_Negativ=trace_cmtk_Negativ[31])
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("06.05.2020")
                data <- data.frame(x, trace_che_Anzahl=trace_che_Anzahl[31], trace_che_Positiv=trace_che_Positiv[31], trace_che_Negativ=trace_che_Negativ[31])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("06.05.2020")
                data <- data.frame(x, trace_clo_Anzahl=trace_clo_Anzahl[31], trace_clo_Positiv=trace_clo_Positiv[31], trace_clo_Negativ=trace_clo_Negativ[31])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("06.05.2020")
                data <- data.frame(x, trace_cre_Anzahl=trace_cre_Anzahl[31], trace_cre_Positiv=trace_cre_Positiv[31], trace_cre_Negativ=trace_cre_Negativ[31])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("06.05.2020")
                data <- data.frame(x, trace_chei_Anzahl=trace_chei_Anzahl[31], trace_chei_Positiv=trace_chei_Positiv[31], trace_chei_Negativ=trace_chei_Negativ[31])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            
        }
        else if (input$filterD== "07.05.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("07.05.2020")
                data <- data.frame(x, trace_c_Anzahl = trace_c_Anzahl[32], trace_c_Positiv = trace_c_Positiv[32], trace_c_Negativ = trace_c_Negativ[32])
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
                
                x<-c("07.05.2020")
                data <- data.frame(x, trace_cim_Anzahl=trace_cim_Anzahl[32], trace_cim_Positiv=trace_cim_Positiv[32], trace_cim_Negativ=trace_cim_Negativ[32])
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
                
                x<-c("07.05.2020")
                data <- data.frame(x, trace_cem_Anzahl=trace_cem_Anzahl[32], trace_cem_Positiv=trace_cem_Positiv[32], trace_cem_Negativ=trace_cem_Negativ[32])
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
                
                x<-c("07.05.2020")
                data <- data.frame(x, trace_cra_Anzahl=trace_cra_Anzahl[32], trace_cra_Positiv=trace_cra_Positiv[32], trace_cra_Negativ=trace_cra_Negativ[32])
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
                
                x<-c("07.05.2020")
                data <- data.frame(x, trace_cca_Anzahl=trace_cca_Anzahl[32], trace_cca_Positiv=trace_cca_Positiv[32], trace_cca_Negativ=trace_cca_Negativ[32])
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
                
                x<-c("07.05.2020")
                data <- data.frame(x, trace_cess_Anzahl=trace_cess_Anzahl[32], trace_cess_Positiv=trace_cess_Positiv[32], trace_cess_Negativ=trace_cess_Negativ[32])
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
                
                x<-c("07.05.2020")
                data <- data.frame(x, trace_cmtk_Anzahl=trace_cmtk_Anzahl[32], trace_cmtk_Positiv=trace_cmtk_Positiv[32], trace_cmtk_Negativ=trace_cmtk_Negativ[32])
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("07.05.2020")
                data <- data.frame(x, trace_che_Anzahl=trace_che_Anzahl[32], trace_che_Positiv=trace_che_Positiv[32], trace_che_Negativ=trace_che_Negativ[32])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_che_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("07.05.2020")
                data <- data.frame(x, trace_clo_Anzahl=trace_clo_Anzahl[32], trace_clo_Positiv=trace_clo_Positiv[32], trace_clo_Negativ=trace_clo_Negativ[32])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_clo_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("07.05.2020")
                data <- data.frame(x, trace_cre_Anzahl=trace_cre_Anzahl[32], trace_cre_Positiv=trace_cre_Positiv[32], trace_cre_Negativ=trace_cre_Negativ[32])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_cre_Negativ, name = 'falsch beantwortet',type = 'bar') 
                Plotallgbar <- Plotallgbar %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',
                                                     yaxis = a, xaxis = b)
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("07.05.2020")
                data <- data.frame(x, trace_chei_Anzahl=trace_chei_Anzahl[32], trace_chei_Positiv=trace_chei_Positiv[32], trace_chei_Negativ=trace_chei_Negativ[32])
                a <- list(title="Besucheranzahl")
                b<- list(title="Datum")
                
                Plotallgbar <- plot_ly(data, x = ~x) 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Anzahl, name = 'Besucheranzahl',type = 'bar') 
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Positiv, name = 'korrekt beantwortet',type = 'bar')
                Plotallgbar <- Plotallgbar %>% add_trace(y = ~trace_chei_Negativ, name = 'falsch beantwortet',type = 'bar') 
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_che_Prozent_Positiv = trace_che_Prozent_Positiv[1], trace_che_Prozent_Negativ = trace_che_Prozent_Negativ[1])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_clo_Prozent_Positiv = trace_clo_Prozent_Positiv[1], trace_clo_Prozent_Negativ = trace_clo_Prozent_Negativ[1])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cre_Prozent_Positiv = trace_cre_Prozent_Positiv[1], trace_cre_Prozent_Negativ = trace_cre_Prozent_Negativ[1])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_chei_Prozent_Positiv = trace_chei_Prozent_Positiv[1], trace_chei_Prozent_Negativ = trace_chei_Prozent_Negativ[1])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
        }
        else if (input$filterD== "11.05.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_c_Prozent_Positiv = trace_c_Prozent_Positiv[36], trace_c_Prozent_Negativ = trace_c_Prozent_Negativ[36])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
            else if (input$filterC== "Corey-IM") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cim_Prozent_Positiv = trace_cim_Prozent_Positiv[36], trace_cim_Prozent_Negativ = trace_cim_Prozent_Negativ[36])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cem_Prozent_Positiv = trace_cem_Prozent_Positiv[36], trace_cem_Prozent_Negativ = trace_cem_Prozent_Negativ[36])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cra_Prozent_Positiv = trace_cra_Prozent_Positiv[36], trace_cra_Prozent_Negativ = trace_cra_Prozent_Negativ[36])
                data <- data.frame(x, y)
                
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cca_Prozent_Positiv = trace_cca_Prozent_Positiv[36], trace_cca_Prozent_Negativ = trace_cca_Prozent_Negativ[36])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cess_Prozent_Positiv = trace_cess_Prozent_Positiv[36], trace_cess_Prozent_Negativ = trace_cess_Prozent_Negativ[36])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cmtk_Prozent_Positiv = trace_cmtk_Prozent_Positiv[36], trace_cmtk_Prozent_Negativ = trace_cmtk_Prozent_Negativ[36])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_che_Prozent_Positiv = trace_che_Prozent_Positiv[36], trace_che_Prozent_Negativ = trace_che_Prozent_Negativ[36])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_clo_Prozent_Positiv = trace_clo_Prozent_Positiv[36], trace_clo_Prozent_Negativ = trace_clo_Prozent_Negativ[36])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cre_Prozent_Positiv = trace_cre_Prozent_Positiv[36], trace_cre_Prozent_Negativ = trace_cre_Prozent_Negativ[36])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_chei_Prozent_Positiv = trace_chei_Prozent_Positiv[36], trace_chei_Prozent_Negativ = trace_chei_Prozent_Negativ[36])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
        }
        else if (input$filterD== "10.05.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_c_Prozent_Positiv = trace_c_Prozent_Positiv[35], trace_c_Prozent_Negativ = trace_c_Prozent_Negativ[35])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
            else if (input$filterC== "Corey-IM") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cim_Prozent_Positiv = trace_cim_Prozent_Positiv[35], trace_cim_Prozent_Negativ = trace_cim_Prozent_Negativ[35])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cem_Prozent_Positiv = trace_cem_Prozent_Positiv[35], trace_cem_Prozent_Negativ = trace_cem_Prozent_Negativ[35])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cra_Prozent_Positiv = trace_cra_Prozent_Positiv[35], trace_cra_Prozent_Negativ = trace_cra_Prozent_Negativ[35])
                data <- data.frame(x, y)
                
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cca_Prozent_Positiv = trace_cca_Prozent_Positiv[35], trace_cca_Prozent_Negativ = trace_cca_Prozent_Negativ[35])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cess_Prozent_Positiv = trace_cess_Prozent_Positiv[35], trace_cess_Prozent_Negativ = trace_cess_Prozent_Negativ[35])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cmtk_Prozent_Positiv = trace_cmtk_Prozent_Positiv[35], trace_cmtk_Prozent_Negativ = trace_cmtk_Prozent_Negativ[35])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_che_Prozent_Positiv = trace_che_Prozent_Positiv[35], trace_che_Prozent_Negativ = trace_che_Prozent_Negativ[35])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_clo_Prozent_Positiv = trace_clo_Prozent_Positiv[35], trace_clo_Prozent_Negativ = trace_clo_Prozent_Negativ[35])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cre_Prozent_Positiv = trace_cre_Prozent_Positiv[35], trace_cre_Prozent_Negativ = trace_cre_Prozent_Negativ[35])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_chei_Prozent_Positiv = trace_chei_Prozent_Positiv[35], trace_chei_Prozent_Negativ = trace_chei_Prozent_Negativ[35])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
        }
        else if (input$filterD== "09.05.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_c_Prozent_Positiv = trace_c_Prozent_Positiv[34], trace_c_Prozent_Negativ = trace_c_Prozent_Negativ[34])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
            else if (input$filterC== "Corey-IM") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cim_Prozent_Positiv = trace_cim_Prozent_Positiv[34], trace_cim_Prozent_Negativ = trace_cim_Prozent_Negativ[34])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cem_Prozent_Positiv = trace_cem_Prozent_Positiv[34], trace_cem_Prozent_Negativ = trace_cem_Prozent_Negativ[34])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cra_Prozent_Positiv = trace_cra_Prozent_Positiv[34], trace_cra_Prozent_Negativ = trace_cra_Prozent_Negativ[34])
                data <- data.frame(x, y)
                
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cca_Prozent_Positiv = trace_cca_Prozent_Positiv[34], trace_cca_Prozent_Negativ = trace_cca_Prozent_Negativ[34])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cess_Prozent_Positiv = trace_cess_Prozent_Positiv[34], trace_cess_Prozent_Negativ = trace_cess_Prozent_Negativ[34])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cmtk_Prozent_Positiv = trace_cmtk_Prozent_Positiv[34], trace_cmtk_Prozent_Negativ = trace_cmtk_Prozent_Negativ[34])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_che_Prozent_Positiv = trace_che_Prozent_Positiv[34], trace_che_Prozent_Negativ = trace_che_Prozent_Negativ[34])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_clo_Prozent_Positiv = trace_clo_Prozent_Positiv[34], trace_clo_Prozent_Negativ = trace_clo_Prozent_Negativ[34])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cre_Prozent_Positiv = trace_cre_Prozent_Positiv[34], trace_cre_Prozent_Negativ = trace_cre_Prozent_Negativ[34])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_chei_Prozent_Positiv = trace_chei_Prozent_Positiv[34], trace_chei_Prozent_Negativ = trace_chei_Prozent_Negativ[34])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
        }
        else if (input$filterD== "08.05.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_c_Prozent_Positiv = trace_c_Prozent_Positiv[33], trace_c_Prozent_Negativ = trace_c_Prozent_Negativ[33])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
            else if (input$filterC== "Corey-IM") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cim_Prozent_Positiv = trace_cim_Prozent_Positiv[33], trace_cim_Prozent_Negativ = trace_cim_Prozent_Negativ[33])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cem_Prozent_Positiv = trace_cem_Prozent_Positiv[33], trace_cem_Prozent_Negativ = trace_cem_Prozent_Negativ[33])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cra_Prozent_Positiv = trace_cra_Prozent_Positiv[33], trace_cra_Prozent_Negativ = trace_cra_Prozent_Negativ[33])
                data <- data.frame(x, y)
                
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cca_Prozent_Positiv = trace_cca_Prozent_Positiv[33], trace_cca_Prozent_Negativ = trace_cca_Prozent_Negativ[33])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cess_Prozent_Positiv = trace_cess_Prozent_Positiv[33], trace_cess_Prozent_Negativ = trace_cess_Prozent_Negativ[33])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cmtk_Prozent_Positiv = trace_cmtk_Prozent_Positiv[33], trace_cmtk_Prozent_Negativ = trace_cmtk_Prozent_Negativ[33])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_che_Prozent_Positiv = trace_che_Prozent_Positiv[33], trace_che_Prozent_Negativ = trace_che_Prozent_Negativ[33])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_clo_Prozent_Positiv = trace_clo_Prozent_Positiv[33], trace_clo_Prozent_Negativ = trace_clo_Prozent_Negativ[33])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cre_Prozent_Positiv = trace_cre_Prozent_Positiv[33], trace_cre_Prozent_Negativ = trace_cre_Prozent_Negativ[33])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_chei_Prozent_Positiv = trace_chei_Prozent_Positiv[33], trace_chei_Prozent_Negativ = trace_chei_Prozent_Negativ[33])
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_che_Prozent_Positiv = trace_che_Prozent_Positiv[2], trace_che_Prozent_Negativ = trace_che_Prozent_Negativ[2])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_clo_Prozent_Positiv = trace_clo_Prozent_Positiv[2], trace_clo_Prozent_Negativ = trace_clo_Prozent_Negativ[2])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cre_Prozent_Positiv = trace_cre_Prozent_Positiv[2], trace_cre_Prozent_Negativ = trace_cre_Prozent_Negativ[2])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_chei_Prozent_Positiv = trace_chei_Prozent_Positiv[2], trace_chei_Prozent_Negativ = trace_chei_Prozent_Negativ[2])
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_che_Prozent_Positiv = trace_che_Prozent_Positiv[3], trace_che_Prozent_Negativ = trace_che_Prozent_Negativ[3])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_clo_Prozent_Positiv = trace_clo_Prozent_Positiv[3], trace_clo_Prozent_Negativ = trace_clo_Prozent_Negativ[3])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cre_Prozent_Positiv = trace_cre_Prozent_Positiv[3], trace_cre_Prozent_Negativ = trace_cre_Prozent_Negativ[3])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_chei_Prozent_Positiv = trace_chei_Prozent_Positiv[3], trace_chei_Prozent_Negativ = trace_chei_Prozent_Negativ[3])
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_che_Prozent_Positiv = trace_che_Prozent_Positiv[4], trace_che_Prozent_Negativ = trace_che_Prozent_Negativ[4])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_clo_Prozent_Positiv = trace_clo_Prozent_Positiv[4], trace_clo_Prozent_Negativ = trace_clo_Prozent_Negativ[4])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cre_Prozent_Positiv = trace_cre_Prozent_Positiv[4], trace_cre_Prozent_Negativ = trace_cre_Prozent_Negativ[4])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_chei_Prozent_Positiv = trace_chei_Prozent_Positiv[4], trace_chei_Prozent_Negativ = trace_chei_Prozent_Negativ[4])
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_che_Prozent_Positiv = trace_che_Prozent_Positiv[5], trace_che_Prozent_Negativ = trace_che_Prozent_Negativ[5])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_clo_Prozent_Positiv = trace_clo_Prozent_Positiv[5], trace_clo_Prozent_Negativ = trace_clo_Prozent_Negativ[5])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cre_Prozent_Positiv = trace_cre_Prozent_Positiv[5], trace_cre_Prozent_Negativ = trace_cre_Prozent_Negativ[5])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_chei_Prozent_Positiv = trace_chei_Prozent_Positiv[5], trace_chei_Prozent_Negativ = trace_chei_Prozent_Negativ[5])
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_che_Prozent_Positiv = trace_che_Prozent_Positiv[6], trace_che_Prozent_Negativ = trace_che_Prozent_Negativ[6])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_clo_Prozent_Positiv = trace_clo_Prozent_Positiv[6], trace_clo_Prozent_Negativ = trace_clo_Prozent_Negativ[6])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cre_Prozent_Positiv = trace_cre_Prozent_Positiv[6], trace_cre_Prozent_Negativ = trace_cre_Prozent_Negativ[6])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_chei_Prozent_Positiv = trace_chei_Prozent_Positiv[6], trace_chei_Prozent_Negativ = trace_chei_Prozent_Negativ[6])
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_che_Prozent_Positiv = trace_che_Prozent_Positiv[7], trace_che_Prozent_Negativ = trace_che_Prozent_Negativ[7])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_clo_Prozent_Positiv = trace_clo_Prozent_Positiv[7], trace_clo_Prozent_Negativ = trace_clo_Prozent_Negativ[7])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cre_Prozent_Positiv = trace_cre_Prozent_Positiv[7], trace_cre_Prozent_Negativ = trace_cre_Prozent_Negativ[7])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_chei_Prozent_Positiv = trace_chei_Prozent_Positiv[7], trace_chei_Prozent_Negativ = trace_chei_Prozent_Negativ[7])
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_che_Prozent_Positiv = trace_che_Prozent_Positiv[8], trace_che_Prozent_Negativ = trace_che_Prozent_Negativ[8])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_clo_Prozent_Positiv = trace_clo_Prozent_Positiv[8], trace_clo_Prozent_Negativ = trace_clo_Prozent_Negativ[8])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cre_Prozent_Positiv = trace_cre_Prozent_Positiv[8], trace_cre_Prozent_Negativ = trace_cre_Prozent_Negativ[8])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_chei_Prozent_Positiv = trace_chei_Prozent_Positiv[8], trace_chei_Prozent_Negativ = trace_chei_Prozent_Negativ[8])
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_che_Prozent_Positiv = trace_che_Prozent_Positiv[9], trace_che_Prozent_Negativ = trace_che_Prozent_Negativ[9])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_clo_Prozent_Positiv = trace_clo_Prozent_Positiv[9], trace_clo_Prozent_Negativ = trace_clo_Prozent_Negativ[9])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cre_Prozent_Positiv = trace_cre_Prozent_Positiv[9], trace_cre_Prozent_Negativ = trace_cre_Prozent_Negativ[9])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_chei_Prozent_Positiv = trace_chei_Prozent_Positiv[9], trace_chei_Prozent_Negativ = trace_chei_Prozent_Negativ[9])
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
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_che_Prozent_Positiv = trace_che_Prozent_Positiv[10], trace_che_Prozent_Negativ = trace_che_Prozent_Negativ[10])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_clo_Prozent_Positiv = trace_clo_Prozent_Positiv[10], trace_clo_Prozent_Negativ = trace_clo_Prozent_Negativ[10])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cre_Prozent_Positiv = trace_cre_Prozent_Positiv[10], trace_cre_Prozent_Negativ = trace_cre_Prozent_Negativ[10])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_chei_Prozent_Positiv = trace_chei_Prozent_Positiv[10], trace_chei_Prozent_Negativ = trace_chei_Prozent_Negativ[10])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
        }
        else if (input$filterD== "16.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_c_Prozent_Positiv = trace_c_Prozent_Positiv[11], trace_c_Prozent_Negativ = trace_c_Prozent_Negativ[11])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
            else if (input$filterC== "Corey-IM") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cim_Prozent_Positiv = trace_cim_Prozent_Positiv[11], trace_cim_Prozent_Negativ = trace_cim_Prozent_Negativ[11])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cem_Prozent_Positiv = trace_cem_Prozent_Positiv[11], trace_cem_Prozent_Negativ = trace_cem_Prozent_Negativ[11])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cra_Prozent_Positiv = trace_cra_Prozent_Positiv[11], trace_cra_Prozent_Negativ = trace_cra_Prozent_Negativ[11])
                data <- data.frame(x, y)
                
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cca_Prozent_Positiv = trace_cca_Prozent_Positiv[11], trace_cca_Prozent_Negativ = trace_cca_Prozent_Negativ[11])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cess_Prozent_Positiv = trace_cess_Prozent_Positiv[11], trace_cess_Prozent_Negativ = trace_cess_Prozent_Negativ[11])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cmtk_Prozent_Positiv = trace_cmtk_Prozent_Positiv[11], trace_cmtk_Prozent_Negativ = trace_cmtk_Prozent_Negativ[11])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_che_Prozent_Positiv = trace_che_Prozent_Positiv[11], trace_che_Prozent_Negativ = trace_che_Prozent_Negativ[11])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_clo_Prozent_Positiv = trace_clo_Prozent_Positiv[11], trace_clo_Prozent_Negativ = trace_clo_Prozent_Negativ[11])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cre_Prozent_Positiv = trace_cre_Prozent_Positiv[11], trace_cre_Prozent_Negativ = trace_cre_Prozent_Negativ[11])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_chei_Prozent_Positiv = trace_chei_Prozent_Positiv[11], trace_chei_Prozent_Negativ = trace_chei_Prozent_Negativ[11])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
        }
        else if (input$filterD== "17.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_c_Prozent_Positiv = trace_c_Prozent_Positiv[12], trace_c_Prozent_Negativ = trace_c_Prozent_Negativ[12])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
            else if (input$filterC== "Corey-IM") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cim_Prozent_Positiv = trace_cim_Prozent_Positiv[12], trace_cim_Prozent_Negativ = trace_cim_Prozent_Negativ[12])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cem_Prozent_Positiv = trace_cem_Prozent_Positiv[12], trace_cem_Prozent_Negativ = trace_cem_Prozent_Negativ[12])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cra_Prozent_Positiv = trace_cra_Prozent_Positiv[12], trace_cra_Prozent_Negativ = trace_cra_Prozent_Negativ[12])
                data <- data.frame(x, y)
                
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cca_Prozent_Positiv = trace_cca_Prozent_Positiv[12], trace_cca_Prozent_Negativ = trace_cca_Prozent_Negativ[12])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cess_Prozent_Positiv = trace_cess_Prozent_Positiv[12], trace_cess_Prozent_Negativ = trace_cess_Prozent_Negativ[12])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cmtk_Prozent_Positiv = trace_cmtk_Prozent_Positiv[12], trace_cmtk_Prozent_Negativ = trace_cmtk_Prozent_Negativ[12])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_che_Prozent_Positiv = trace_che_Prozent_Positiv[12], trace_che_Prozent_Negativ = trace_che_Prozent_Negativ[12])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_clo_Prozent_Positiv = trace_clo_Prozent_Positiv[12], trace_clo_Prozent_Negativ = trace_clo_Prozent_Negativ[12])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cre_Prozent_Positiv = trace_cre_Prozent_Positiv[12], trace_cre_Prozent_Negativ = trace_cre_Prozent_Negativ[12])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_chei_Prozent_Positiv = trace_chei_Prozent_Positiv[12], trace_chei_Prozent_Negativ = trace_chei_Prozent_Negativ[12])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
        }
        else if (input$filterD== "18.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_c_Prozent_Positiv = trace_c_Prozent_Positiv[13], trace_c_Prozent_Negativ = trace_c_Prozent_Negativ[13])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
            else if (input$filterC== "Corey-IM") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cim_Prozent_Positiv = trace_cim_Prozent_Positiv[13], trace_cim_Prozent_Negativ = trace_cim_Prozent_Negativ[13])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cem_Prozent_Positiv = trace_cem_Prozent_Positiv[13], trace_cem_Prozent_Negativ = trace_cem_Prozent_Negativ[13])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cra_Prozent_Positiv = trace_cra_Prozent_Positiv[13], trace_cra_Prozent_Negativ = trace_cra_Prozent_Negativ[13])
                data <- data.frame(x, y)
                
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cca_Prozent_Positiv = trace_cca_Prozent_Positiv[13], trace_cca_Prozent_Negativ = trace_cca_Prozent_Negativ[13])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cess_Prozent_Positiv = trace_cess_Prozent_Positiv[13], trace_cess_Prozent_Negativ = trace_cess_Prozent_Negativ[13])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cmtk_Prozent_Positiv = trace_cmtk_Prozent_Positiv[13], trace_cmtk_Prozent_Negativ = trace_cmtk_Prozent_Negativ[13])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_che_Prozent_Positiv = trace_che_Prozent_Positiv[13], trace_che_Prozent_Negativ = trace_che_Prozent_Negativ[13])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_clo_Prozent_Positiv = trace_clo_Prozent_Positiv[13], trace_clo_Prozent_Negativ = trace_clo_Prozent_Negativ[13])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cre_Prozent_Positiv = trace_cre_Prozent_Positiv[13], trace_cre_Prozent_Negativ = trace_cre_Prozent_Negativ[13])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_chei_Prozent_Positiv = trace_chei_Prozent_Positiv[13], trace_chei_Prozent_Negativ = trace_chei_Prozent_Negativ[13])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
        }
        else if (input$filterD== "19.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_c_Prozent_Positiv = trace_c_Prozent_Positiv[14], trace_c_Prozent_Negativ = trace_c_Prozent_Negativ[14])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
            else if (input$filterC== "Corey-IM") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cim_Prozent_Positiv = trace_cim_Prozent_Positiv[14], trace_cim_Prozent_Negativ = trace_cim_Prozent_Negativ[14])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cem_Prozent_Positiv = trace_cem_Prozent_Positiv[14], trace_cem_Prozent_Negativ = trace_cem_Prozent_Negativ[14])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cra_Prozent_Positiv = trace_cra_Prozent_Positiv[14], trace_cra_Prozent_Negativ = trace_cra_Prozent_Negativ[14])
                data <- data.frame(x, y)
                
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cca_Prozent_Positiv = trace_cca_Prozent_Positiv[14], trace_cca_Prozent_Negativ = trace_cca_Prozent_Negativ[14])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cess_Prozent_Positiv = trace_cess_Prozent_Positiv[14], trace_cess_Prozent_Negativ = trace_cess_Prozent_Negativ[14])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cmtk_Prozent_Positiv = trace_cmtk_Prozent_Positiv[14], trace_cmtk_Prozent_Negativ = trace_cmtk_Prozent_Negativ[14])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_che_Prozent_Positiv = trace_che_Prozent_Positiv[14], trace_che_Prozent_Negativ = trace_che_Prozent_Negativ[14])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_clo_Prozent_Positiv = trace_clo_Prozent_Positiv[14], trace_clo_Prozent_Negativ = trace_clo_Prozent_Negativ[14])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cre_Prozent_Positiv = trace_cre_Prozent_Positiv[14], trace_cre_Prozent_Negativ = trace_cre_Prozent_Negativ[14])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_chei_Prozent_Positiv = trace_chei_Prozent_Positiv[14], trace_chei_Prozent_Negativ = trace_chei_Prozent_Negativ[14])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
        }
        else if (input$filterD== "20.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_c_Prozent_Positiv = trace_c_Prozent_Positiv[15], trace_c_Prozent_Negativ = trace_c_Prozent_Negativ[15])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
            else if (input$filterC== "Corey-IM") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cim_Prozent_Positiv = trace_cim_Prozent_Positiv[15], trace_cim_Prozent_Negativ = trace_cim_Prozent_Negativ[15])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cem_Prozent_Positiv = trace_cem_Prozent_Positiv[15], trace_cem_Prozent_Negativ = trace_cem_Prozent_Negativ[15])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cra_Prozent_Positiv = trace_cra_Prozent_Positiv[15], trace_cra_Prozent_Negativ = trace_cra_Prozent_Negativ[15])
                data <- data.frame(x, y)
                
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cca_Prozent_Positiv = trace_cca_Prozent_Positiv[15], trace_cca_Prozent_Negativ = trace_cca_Prozent_Negativ[15])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cess_Prozent_Positiv = trace_cess_Prozent_Positiv[15], trace_cess_Prozent_Negativ = trace_cess_Prozent_Negativ[15])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cmtk_Prozent_Positiv = trace_cmtk_Prozent_Positiv[15], trace_cmtk_Prozent_Negativ = trace_cmtk_Prozent_Negativ[15])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_che_Prozent_Positiv = trace_che_Prozent_Positiv[15], trace_che_Prozent_Negativ = trace_che_Prozent_Negativ[15])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_clo_Prozent_Positiv = trace_clo_Prozent_Positiv[15], trace_clo_Prozent_Negativ = trace_clo_Prozent_Negativ[15])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cre_Prozent_Positiv = trace_cre_Prozent_Positiv[15], trace_cre_Prozent_Negativ = trace_cre_Prozent_Negativ[15])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_chei_Prozent_Positiv = trace_chei_Prozent_Positiv[15], trace_chei_Prozent_Negativ = trace_chei_Prozent_Negativ[15])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
        }
        else if (input$filterD== "21.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_c_Prozent_Positiv = trace_c_Prozent_Positiv[16], trace_c_Prozent_Negativ = trace_c_Prozent_Negativ[16])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
            else if (input$filterC== "Corey-IM") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cim_Prozent_Positiv = trace_cim_Prozent_Positiv[16], trace_cim_Prozent_Negativ = trace_cim_Prozent_Negativ[16])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cem_Prozent_Positiv = trace_cem_Prozent_Positiv[16], trace_cem_Prozent_Negativ = trace_cem_Prozent_Negativ[16])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cra_Prozent_Positiv = trace_cra_Prozent_Positiv[16], trace_cra_Prozent_Negativ = trace_cra_Prozent_Negativ[16])
                data <- data.frame(x, y)
                
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cca_Prozent_Positiv = trace_cca_Prozent_Positiv[16], trace_cca_Prozent_Negativ = trace_cca_Prozent_Negativ[16])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cess_Prozent_Positiv = trace_cess_Prozent_Positiv[16], trace_cess_Prozent_Negativ = trace_cess_Prozent_Negativ[16])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cmtk_Prozent_Positiv = trace_cmtk_Prozent_Positiv[16], trace_cmtk_Prozent_Negativ = trace_cmtk_Prozent_Negativ[16])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_che_Prozent_Positiv = trace_che_Prozent_Positiv[16], trace_che_Prozent_Negativ = trace_che_Prozent_Negativ[16])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_clo_Prozent_Positiv = trace_clo_Prozent_Positiv[16], trace_clo_Prozent_Negativ = trace_clo_Prozent_Negativ[16])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cre_Prozent_Positiv = trace_cre_Prozent_Positiv[16], trace_cre_Prozent_Negativ = trace_cre_Prozent_Negativ[16])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_chei_Prozent_Positiv = trace_chei_Prozent_Positiv[16], trace_chei_Prozent_Negativ = trace_chei_Prozent_Negativ[16])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
        }
        else if (input$filterD== "22.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_c_Prozent_Positiv = trace_c_Prozent_Positiv[17], trace_c_Prozent_Negativ = trace_c_Prozent_Negativ[17])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
            else if (input$filterC== "Corey-IM") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cim_Prozent_Positiv = trace_cim_Prozent_Positiv[17], trace_cim_Prozent_Negativ = trace_cim_Prozent_Negativ[17])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cem_Prozent_Positiv = trace_cem_Prozent_Positiv[17], trace_cem_Prozent_Negativ = trace_cem_Prozent_Negativ[17])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cra_Prozent_Positiv = trace_cra_Prozent_Positiv[17], trace_cra_Prozent_Negativ = trace_cra_Prozent_Negativ[17])
                data <- data.frame(x, y)
                
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cca_Prozent_Positiv = trace_cca_Prozent_Positiv[17], trace_cca_Prozent_Negativ = trace_cca_Prozent_Negativ[17])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cess_Prozent_Positiv = trace_cess_Prozent_Positiv[17], trace_cess_Prozent_Negativ = trace_cess_Prozent_Negativ[17])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cmtk_Prozent_Positiv = trace_cmtk_Prozent_Positiv[17], trace_cmtk_Prozent_Negativ = trace_cmtk_Prozent_Negativ[17])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_che_Prozent_Positiv = trace_che_Prozent_Positiv[17], trace_che_Prozent_Negativ = trace_che_Prozent_Negativ[17])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_clo_Prozent_Positiv = trace_clo_Prozent_Positiv[17], trace_clo_Prozent_Negativ = trace_clo_Prozent_Negativ[17])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cre_Prozent_Positiv = trace_cre_Prozent_Positiv[17], trace_cre_Prozent_Negativ = trace_cre_Prozent_Negativ[17])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_chei_Prozent_Positiv = trace_chei_Prozent_Positiv[17], trace_chei_Prozent_Negativ = trace_chei_Prozent_Negativ[17])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
        }
        else if (input$filterD== "23.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_c_Prozent_Positiv = trace_c_Prozent_Positiv[18], trace_c_Prozent_Negativ = trace_c_Prozent_Negativ[18])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
            else if (input$filterC== "Corey-IM") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cim_Prozent_Positiv = trace_cim_Prozent_Positiv[18], trace_cim_Prozent_Negativ = trace_cim_Prozent_Negativ[18])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cem_Prozent_Positiv = trace_cem_Prozent_Positiv[18], trace_cem_Prozent_Negativ = trace_cem_Prozent_Negativ[18])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cra_Prozent_Positiv = trace_cra_Prozent_Positiv[18], trace_cra_Prozent_Negativ = trace_cra_Prozent_Negativ[18])
                data <- data.frame(x, y)
                
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cca_Prozent_Positiv = trace_cca_Prozent_Positiv[18], trace_cca_Prozent_Negativ = trace_cca_Prozent_Negativ[18])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cess_Prozent_Positiv = trace_cess_Prozent_Positiv[18], trace_cess_Prozent_Negativ = trace_cess_Prozent_Negativ[18])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cmtk_Prozent_Positiv = trace_cmtk_Prozent_Positiv[18], trace_cmtk_Prozent_Negativ = trace_cmtk_Prozent_Negativ[18])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_che_Prozent_Positiv = trace_che_Prozent_Positiv[18], trace_che_Prozent_Negativ = trace_che_Prozent_Negativ[18])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_clo_Prozent_Positiv = trace_clo_Prozent_Positiv[18], trace_clo_Prozent_Negativ = trace_clo_Prozent_Negativ[18])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cre_Prozent_Positiv = trace_cre_Prozent_Positiv[18], trace_cre_Prozent_Negativ = trace_cre_Prozent_Negativ[18])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_chei_Prozent_Positiv = trace_chei_Prozent_Positiv[18], trace_chei_Prozent_Negativ = trace_chei_Prozent_Negativ[18])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
        }
        else if (input$filterD== "24.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_c_Prozent_Positiv = trace_c_Prozent_Positiv[19], trace_c_Prozent_Negativ = trace_c_Prozent_Negativ[19])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
            else if (input$filterC== "Corey-IM") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cim_Prozent_Positiv = trace_cim_Prozent_Positiv[19], trace_cim_Prozent_Negativ = trace_cim_Prozent_Negativ[19])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cem_Prozent_Positiv = trace_cem_Prozent_Positiv[19], trace_cem_Prozent_Negativ = trace_cem_Prozent_Negativ[19])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cra_Prozent_Positiv = trace_cra_Prozent_Positiv[19], trace_cra_Prozent_Negativ = trace_cra_Prozent_Negativ[19])
                data <- data.frame(x, y)
                
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cca_Prozent_Positiv = trace_cca_Prozent_Positiv[19], trace_cca_Prozent_Negativ = trace_cca_Prozent_Negativ[19])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cess_Prozent_Positiv = trace_cess_Prozent_Positiv[19], trace_cess_Prozent_Negativ = trace_cess_Prozent_Negativ[19])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cmtk_Prozent_Positiv = trace_cmtk_Prozent_Positiv[19], trace_cmtk_Prozent_Negativ = trace_cmtk_Prozent_Negativ[19])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_che_Prozent_Positiv = trace_che_Prozent_Positiv[19], trace_che_Prozent_Negativ = trace_che_Prozent_Negativ[19])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_clo_Prozent_Positiv = trace_clo_Prozent_Positiv[19], trace_clo_Prozent_Negativ = trace_clo_Prozent_Negativ[19])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cre_Prozent_Positiv = trace_cre_Prozent_Positiv[19], trace_cre_Prozent_Negativ = trace_cre_Prozent_Negativ[19])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_chei_Prozent_Positiv = trace_chei_Prozent_Positiv[19], trace_chei_Prozent_Negativ = trace_chei_Prozent_Negativ[19])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
        }
        else if (input$filterD== "25.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_c_Prozent_Positiv = trace_c_Prozent_Positiv[20], trace_c_Prozent_Negativ = trace_c_Prozent_Negativ[20])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
            else if (input$filterC== "Corey-IM") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cim_Prozent_Positiv = trace_cim_Prozent_Positiv[20], trace_cim_Prozent_Negativ = trace_cim_Prozent_Negativ[20])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cem_Prozent_Positiv = trace_cem_Prozent_Positiv[20], trace_cem_Prozent_Negativ = trace_cem_Prozent_Negativ[20])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cra_Prozent_Positiv = trace_cra_Prozent_Positiv[20], trace_cra_Prozent_Negativ = trace_cra_Prozent_Negativ[20])
                data <- data.frame(x, y)
                
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cca_Prozent_Positiv = trace_cca_Prozent_Positiv[20], trace_cca_Prozent_Negativ = trace_cca_Prozent_Negativ[20])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cess_Prozent_Positiv = trace_cess_Prozent_Positiv[20], trace_cess_Prozent_Negativ = trace_cess_Prozent_Negativ[20])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cmtk_Prozent_Positiv = trace_cmtk_Prozent_Positiv[20], trace_cmtk_Prozent_Negativ = trace_cmtk_Prozent_Negativ[20])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_che_Prozent_Positiv = trace_che_Prozent_Positiv[20], trace_che_Prozent_Negativ = trace_che_Prozent_Negativ[20])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_clo_Prozent_Positiv = trace_clo_Prozent_Positiv[20], trace_clo_Prozent_Negativ = trace_clo_Prozent_Negativ[20])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cre_Prozent_Positiv = trace_cre_Prozent_Positiv[20], trace_cre_Prozent_Negativ = trace_cre_Prozent_Negativ[20])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_chei_Prozent_Positiv = trace_chei_Prozent_Positiv[20], trace_chei_Prozent_Negativ = trace_chei_Prozent_Negativ[20])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
        }
        else if (input$filterD== "26.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_c_Prozent_Positiv = trace_c_Prozent_Positiv[21], trace_c_Prozent_Negativ = trace_c_Prozent_Negativ[21])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
            else if (input$filterC== "Corey-IM") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cim_Prozent_Positiv = trace_cim_Prozent_Positiv[21], trace_cim_Prozent_Negativ = trace_cim_Prozent_Negativ[21])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cem_Prozent_Positiv = trace_cem_Prozent_Positiv[21], trace_cem_Prozent_Negativ = trace_cem_Prozent_Negativ[21])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cra_Prozent_Positiv = trace_cra_Prozent_Positiv[21], trace_cra_Prozent_Negativ = trace_cra_Prozent_Negativ[21])
                data <- data.frame(x, y)
                
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cca_Prozent_Positiv = trace_cca_Prozent_Positiv[21], trace_cca_Prozent_Negativ = trace_cca_Prozent_Negativ[21])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cess_Prozent_Positiv = trace_cess_Prozent_Positiv[21], trace_cess_Prozent_Negativ = trace_cess_Prozent_Negativ[21])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cmtk_Prozent_Positiv = trace_cmtk_Prozent_Positiv[21], trace_cmtk_Prozent_Negativ = trace_cmtk_Prozent_Negativ[21])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_che_Prozent_Positiv = trace_che_Prozent_Positiv[21], trace_che_Prozent_Negativ = trace_che_Prozent_Negativ[21])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_clo_Prozent_Positiv = trace_clo_Prozent_Positiv[21], trace_clo_Prozent_Negativ = trace_clo_Prozent_Negativ[21])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cre_Prozent_Positiv = trace_cre_Prozent_Positiv[21], trace_cre_Prozent_Negativ = trace_cre_Prozent_Negativ[21])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_chei_Prozent_Positiv = trace_chei_Prozent_Positiv[21], trace_chei_Prozent_Negativ = trace_chei_Prozent_Negativ[21])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
        }
        else if (input$filterD== "27.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_c_Prozent_Positiv = trace_c_Prozent_Positiv[22], trace_c_Prozent_Negativ = trace_c_Prozent_Negativ[22])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
            else if (input$filterC== "Corey-IM") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cim_Prozent_Positiv = trace_cim_Prozent_Positiv[22], trace_cim_Prozent_Negativ = trace_cim_Prozent_Negativ[22])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cem_Prozent_Positiv = trace_cem_Prozent_Positiv[22], trace_cem_Prozent_Negativ = trace_cem_Prozent_Negativ[22])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cra_Prozent_Positiv = trace_cra_Prozent_Positiv[22], trace_cra_Prozent_Negativ = trace_cra_Prozent_Negativ[22])
                data <- data.frame(x, y)
                
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cca_Prozent_Positiv = trace_cca_Prozent_Positiv[22], trace_cca_Prozent_Negativ = trace_cca_Prozent_Negativ[22])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cess_Prozent_Positiv = trace_cess_Prozent_Positiv[22], trace_cess_Prozent_Negativ = trace_cess_Prozent_Negativ[22])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cmtk_Prozent_Positiv = trace_cmtk_Prozent_Positiv[22], trace_cmtk_Prozent_Negativ = trace_cmtk_Prozent_Negativ[22])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_che_Prozent_Positiv = trace_che_Prozent_Positiv[22], trace_che_Prozent_Negativ = trace_che_Prozent_Negativ[22])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_clo_Prozent_Positiv = trace_clo_Prozent_Positiv[22], trace_clo_Prozent_Negativ = trace_clo_Prozent_Negativ[22])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cre_Prozent_Positiv = trace_cre_Prozent_Positiv[22], trace_cre_Prozent_Negativ = trace_cre_Prozent_Negativ[22])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_chei_Prozent_Positiv = trace_chei_Prozent_Positiv[22], trace_chei_Prozent_Negativ = trace_chei_Prozent_Negativ[22])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
        }
        else if (input$filterD== "28.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_c_Prozent_Positiv = trace_c_Prozent_Positiv[23], trace_c_Prozent_Negativ = trace_c_Prozent_Negativ[23])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
            else if (input$filterC== "Corey-IM") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cim_Prozent_Positiv = trace_cim_Prozent_Positiv[23], trace_cim_Prozent_Negativ = trace_cim_Prozent_Negativ[23])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cem_Prozent_Positiv = trace_cem_Prozent_Positiv[23], trace_cem_Prozent_Negativ = trace_cem_Prozent_Negativ[23])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cra_Prozent_Positiv = trace_cra_Prozent_Positiv[23], trace_cra_Prozent_Negativ = trace_cra_Prozent_Negativ[23])
                data <- data.frame(x, y)
                
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cca_Prozent_Positiv = trace_cca_Prozent_Positiv[23], trace_cca_Prozent_Negativ = trace_cca_Prozent_Negativ[23])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cess_Prozent_Positiv = trace_cess_Prozent_Positiv[23], trace_cess_Prozent_Negativ = trace_cess_Prozent_Negativ[23])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cmtk_Prozent_Positiv = trace_cmtk_Prozent_Positiv[23], trace_cmtk_Prozent_Negativ = trace_cmtk_Prozent_Negativ[23])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_che_Prozent_Positiv = trace_che_Prozent_Positiv[23], trace_che_Prozent_Negativ = trace_che_Prozent_Negativ[23])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_clo_Prozent_Positiv = trace_clo_Prozent_Positiv[23], trace_clo_Prozent_Negativ = trace_clo_Prozent_Negativ[23])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cre_Prozent_Positiv = trace_cre_Prozent_Positiv[23], trace_cre_Prozent_Negativ = trace_cre_Prozent_Negativ[23])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_chei_Prozent_Positiv = trace_chei_Prozent_Positiv[23], trace_chei_Prozent_Negativ = trace_chei_Prozent_Negativ[23])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
        }
        else if (input$filterD== "29.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_c_Prozent_Positiv = trace_c_Prozent_Positiv[24], trace_c_Prozent_Negativ = trace_c_Prozent_Negativ[24])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
            else if (input$filterC== "Corey-IM") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cim_Prozent_Positiv = trace_cim_Prozent_Positiv[24], trace_cim_Prozent_Negativ = trace_cim_Prozent_Negativ[24])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cem_Prozent_Positiv = trace_cem_Prozent_Positiv[24], trace_cem_Prozent_Negativ = trace_cem_Prozent_Negativ[24])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cra_Prozent_Positiv = trace_cra_Prozent_Positiv[24], trace_cra_Prozent_Negativ = trace_cra_Prozent_Negativ[24])
                data <- data.frame(x, y)
                
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cca_Prozent_Positiv = trace_cca_Prozent_Positiv[24], trace_cca_Prozent_Negativ = trace_cca_Prozent_Negativ[24])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cess_Prozent_Positiv = trace_cess_Prozent_Positiv[24], trace_cess_Prozent_Negativ = trace_cess_Prozent_Negativ[24])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cmtk_Prozent_Positiv = trace_cmtk_Prozent_Positiv[24], trace_cmtk_Prozent_Negativ = trace_cmtk_Prozent_Negativ[24])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_che_Prozent_Positiv = trace_che_Prozent_Positiv[24], trace_che_Prozent_Negativ = trace_che_Prozent_Negativ[24])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_clo_Prozent_Positiv = trace_clo_Prozent_Positiv[24], trace_clo_Prozent_Negativ = trace_clo_Prozent_Negativ[24])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cre_Prozent_Positiv = trace_cre_Prozent_Positiv[24], trace_cre_Prozent_Negativ = trace_cre_Prozent_Negativ[24])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_chei_Prozent_Positiv = trace_chei_Prozent_Positiv[24], trace_chei_Prozent_Negativ = trace_chei_Prozent_Negativ[24])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
        }
        else if (input$filterD== "30.04.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_c_Prozent_Positiv = trace_c_Prozent_Positiv[25], trace_c_Prozent_Negativ = trace_c_Prozent_Negativ[25])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
            else if (input$filterC== "Corey-IM") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cim_Prozent_Positiv = trace_cim_Prozent_Positiv[25], trace_cim_Prozent_Negativ = trace_cim_Prozent_Negativ[25])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cem_Prozent_Positiv = trace_cem_Prozent_Positiv[25], trace_cem_Prozent_Negativ = trace_cem_Prozent_Negativ[25])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cra_Prozent_Positiv = trace_cra_Prozent_Positiv[25], trace_cra_Prozent_Negativ = trace_cra_Prozent_Negativ[25])
                data <- data.frame(x, y)
                
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cca_Prozent_Positiv = trace_cca_Prozent_Positiv[25], trace_cca_Prozent_Negativ = trace_cca_Prozent_Negativ[25])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cess_Prozent_Positiv = trace_cess_Prozent_Positiv[25], trace_cess_Prozent_Negativ = trace_cess_Prozent_Negativ[25])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cmtk_Prozent_Positiv = trace_cmtk_Prozent_Positiv[25], trace_cmtk_Prozent_Negativ = trace_cmtk_Prozent_Negativ[25])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_che_Prozent_Positiv = trace_che_Prozent_Positiv[25], trace_che_Prozent_Negativ = trace_che_Prozent_Negativ[25])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_clo_Prozent_Positiv = trace_clo_Prozent_Positiv[25], trace_clo_Prozent_Negativ = trace_clo_Prozent_Negativ[25])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cre_Prozent_Positiv = trace_cre_Prozent_Positiv[25], trace_cre_Prozent_Negativ = trace_cre_Prozent_Negativ[25])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_chei_Prozent_Positiv = trace_chei_Prozent_Positiv[25], trace_chei_Prozent_Negativ = trace_chei_Prozent_Negativ[25])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
        }
        else if (input$filterD== "01.05.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_c_Prozent_Positiv = trace_c_Prozent_Positiv[26], trace_c_Prozent_Negativ = trace_c_Prozent_Negativ[26])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
            else if (input$filterC== "Corey-IM") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cim_Prozent_Positiv = trace_cim_Prozent_Positiv[26], trace_cim_Prozent_Negativ = trace_cim_Prozent_Negativ[26])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cem_Prozent_Positiv = trace_cem_Prozent_Positiv[26], trace_cem_Prozent_Negativ = trace_cem_Prozent_Negativ[26])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cra_Prozent_Positiv = trace_cra_Prozent_Positiv[26], trace_cra_Prozent_Negativ = trace_cra_Prozent_Negativ[26])
                data <- data.frame(x, y)
                
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cca_Prozent_Positiv = trace_cca_Prozent_Positiv[26], trace_cca_Prozent_Negativ = trace_cca_Prozent_Negativ[26])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cess_Prozent_Positiv = trace_cess_Prozent_Positiv[26], trace_cess_Prozent_Negativ = trace_cess_Prozent_Negativ[26])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cmtk_Prozent_Positiv = trace_cmtk_Prozent_Positiv[26], trace_cmtk_Prozent_Negativ = trace_cmtk_Prozent_Negativ[26])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_che_Prozent_Positiv = trace_che_Prozent_Positiv[26], trace_che_Prozent_Negativ = trace_che_Prozent_Negativ[26])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_clo_Prozent_Positiv = trace_clo_Prozent_Positiv[26], trace_clo_Prozent_Negativ = trace_clo_Prozent_Negativ[26])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cre_Prozent_Positiv = trace_cre_Prozent_Positiv[26], trace_cre_Prozent_Negativ = trace_cre_Prozent_Negativ[26])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_chei_Prozent_Positiv = trace_chei_Prozent_Positiv[26], trace_chei_Prozent_Negativ = trace_chei_Prozent_Negativ[26])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
        }
        else if (input$filterD== "02.05.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_c_Prozent_Positiv = trace_c_Prozent_Positiv[27], trace_c_Prozent_Negativ = trace_c_Prozent_Negativ[27])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
            else if (input$filterC== "Corey-IM") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cim_Prozent_Positiv = trace_cim_Prozent_Positiv[27], trace_cim_Prozent_Negativ = trace_cim_Prozent_Negativ[27])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cem_Prozent_Positiv = trace_cem_Prozent_Positiv[27], trace_cem_Prozent_Negativ = trace_cem_Prozent_Negativ[27])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cra_Prozent_Positiv = trace_cra_Prozent_Positiv[27], trace_cra_Prozent_Negativ = trace_cra_Prozent_Negativ[27])
                data <- data.frame(x, y)
                
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cca_Prozent_Positiv = trace_cca_Prozent_Positiv[27], trace_cca_Prozent_Negativ = trace_cca_Prozent_Negativ[27])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cess_Prozent_Positiv = trace_cess_Prozent_Positiv[27], trace_cess_Prozent_Negativ = trace_cess_Prozent_Negativ[27])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cmtk_Prozent_Positiv = trace_cmtk_Prozent_Positiv[27], trace_cmtk_Prozent_Negativ = trace_cmtk_Prozent_Negativ[27])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_che_Prozent_Positiv = trace_che_Prozent_Positiv[27], trace_che_Prozent_Negativ = trace_che_Prozent_Negativ[27])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_clo_Prozent_Positiv = trace_clo_Prozent_Positiv[27], trace_clo_Prozent_Negativ = trace_clo_Prozent_Negativ[27])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cre_Prozent_Positiv = trace_cre_Prozent_Positiv[27], trace_cre_Prozent_Negativ = trace_cre_Prozent_Negativ[27])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_chei_Prozent_Positiv = trace_chei_Prozent_Positiv[27], trace_chei_Prozent_Negativ = trace_chei_Prozent_Negativ[27])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
        }
        else if (input$filterD== "03.05.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_c_Prozent_Positiv = trace_c_Prozent_Positiv[28], trace_c_Prozent_Negativ = trace_c_Prozent_Negativ[28])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
            else if (input$filterC== "Corey-IM") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cim_Prozent_Positiv = trace_cim_Prozent_Positiv[28], trace_cim_Prozent_Negativ = trace_cim_Prozent_Negativ[28])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cem_Prozent_Positiv = trace_cem_Prozent_Positiv[28], trace_cem_Prozent_Negativ = trace_cem_Prozent_Negativ[28])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cra_Prozent_Positiv = trace_cra_Prozent_Positiv[28], trace_cra_Prozent_Negativ = trace_cra_Prozent_Negativ[28])
                data <- data.frame(x, y)
                
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cca_Prozent_Positiv = trace_cca_Prozent_Positiv[28], trace_cca_Prozent_Negativ = trace_cca_Prozent_Negativ[28])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cess_Prozent_Positiv = trace_cess_Prozent_Positiv[28], trace_cess_Prozent_Negativ = trace_cess_Prozent_Negativ[28])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cmtk_Prozent_Positiv = trace_cmtk_Prozent_Positiv[28], trace_cmtk_Prozent_Negativ = trace_cmtk_Prozent_Negativ[28])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_che_Prozent_Positiv = trace_che_Prozent_Positiv[28], trace_che_Prozent_Negativ = trace_che_Prozent_Negativ[28])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_clo_Prozent_Positiv = trace_clo_Prozent_Positiv[28], trace_clo_Prozent_Negativ = trace_clo_Prozent_Negativ[28])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cre_Prozent_Positiv = trace_cre_Prozent_Positiv[28], trace_cre_Prozent_Negativ = trace_cre_Prozent_Negativ[28])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_chei_Prozent_Positiv = trace_chei_Prozent_Positiv[28], trace_chei_Prozent_Negativ = trace_chei_Prozent_Negativ[28])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
        }
        else if (input$filterD== "04.05.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_c_Prozent_Positiv = trace_c_Prozent_Positiv[29], trace_c_Prozent_Negativ = trace_c_Prozent_Negativ[29])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
            else if (input$filterC== "Corey-IM") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cim_Prozent_Positiv = trace_cim_Prozent_Positiv[29], trace_cim_Prozent_Negativ = trace_cim_Prozent_Negativ[29])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cem_Prozent_Positiv = trace_cem_Prozent_Positiv[29], trace_cem_Prozent_Negativ = trace_cem_Prozent_Negativ[29])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cra_Prozent_Positiv = trace_cra_Prozent_Positiv[29], trace_cra_Prozent_Negativ = trace_cra_Prozent_Negativ[29])
                data <- data.frame(x, y)
                
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cca_Prozent_Positiv = trace_cca_Prozent_Positiv[29], trace_cca_Prozent_Negativ = trace_cca_Prozent_Negativ[29])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cess_Prozent_Positiv = trace_cess_Prozent_Positiv[29], trace_cess_Prozent_Negativ = trace_cess_Prozent_Negativ[29])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cmtk_Prozent_Positiv = trace_cmtk_Prozent_Positiv[29], trace_cmtk_Prozent_Negativ = trace_cmtk_Prozent_Negativ[29])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_che_Prozent_Positiv = trace_che_Prozent_Positiv[29], trace_che_Prozent_Negativ = trace_che_Prozent_Negativ[29])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_clo_Prozent_Positiv = trace_clo_Prozent_Positiv[29], trace_clo_Prozent_Negativ = trace_clo_Prozent_Negativ[29])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cre_Prozent_Positiv = trace_cre_Prozent_Positiv[29], trace_cre_Prozent_Negativ = trace_cre_Prozent_Negativ[29])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_chei_Prozent_Positiv = trace_chei_Prozent_Positiv[29], trace_chei_Prozent_Negativ = trace_chei_Prozent_Negativ[29])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
        }
        else if (input$filterD== "05.05.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_c_Prozent_Positiv = trace_c_Prozent_Positiv[30], trace_c_Prozent_Negativ = trace_c_Prozent_Negativ[30])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
            else if (input$filterC== "Corey-IM") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cim_Prozent_Positiv = trace_cim_Prozent_Positiv[30], trace_cim_Prozent_Negativ = trace_cim_Prozent_Negativ[30])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cem_Prozent_Positiv = trace_cem_Prozent_Positiv[30], trace_cem_Prozent_Negativ = trace_cem_Prozent_Negativ[30])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cra_Prozent_Positiv = trace_cra_Prozent_Positiv[30], trace_cra_Prozent_Negativ = trace_cra_Prozent_Negativ[30])
                data <- data.frame(x, y)
                
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cca_Prozent_Positiv = trace_cca_Prozent_Positiv[30], trace_cca_Prozent_Negativ = trace_cca_Prozent_Negativ[30])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cess_Prozent_Positiv = trace_cess_Prozent_Positiv[30], trace_cess_Prozent_Negativ = trace_cess_Prozent_Negativ[30])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cmtk_Prozent_Positiv = trace_cmtk_Prozent_Positiv[30], trace_cmtk_Prozent_Negativ = trace_cmtk_Prozent_Negativ[30])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_che_Prozent_Positiv = trace_che_Prozent_Positiv[30], trace_che_Prozent_Negativ = trace_che_Prozent_Negativ[30])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_clo_Prozent_Positiv = trace_clo_Prozent_Positiv[30], trace_clo_Prozent_Negativ = trace_clo_Prozent_Negativ[30])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cre_Prozent_Positiv = trace_cre_Prozent_Positiv[30], trace_cre_Prozent_Negativ = trace_cre_Prozent_Negativ[30])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_chei_Prozent_Positiv = trace_chei_Prozent_Positiv[30], trace_chei_Prozent_Negativ = trace_chei_Prozent_Negativ[30])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
        }
        else if (input$filterD== "06.05.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_c_Prozent_Positiv = trace_c_Prozent_Positiv[31], trace_c_Prozent_Negativ = trace_c_Prozent_Negativ[31])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
            else if (input$filterC== "Corey-IM") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cim_Prozent_Positiv = trace_cim_Prozent_Positiv[31], trace_cim_Prozent_Negativ = trace_cim_Prozent_Negativ[31])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cem_Prozent_Positiv = trace_cem_Prozent_Positiv[31], trace_cem_Prozent_Negativ = trace_cem_Prozent_Negativ[31])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cra_Prozent_Positiv = trace_cra_Prozent_Positiv[31], trace_cra_Prozent_Negativ = trace_cra_Prozent_Negativ[31])
                data <- data.frame(x, y)
                
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cca_Prozent_Positiv = trace_cca_Prozent_Positiv[31], trace_cca_Prozent_Negativ = trace_cca_Prozent_Negativ[31])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cess_Prozent_Positiv = trace_cess_Prozent_Positiv[31], trace_cess_Prozent_Negativ = trace_cess_Prozent_Negativ[31])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cmtk_Prozent_Positiv = trace_cmtk_Prozent_Positiv[31], trace_cmtk_Prozent_Negativ = trace_cmtk_Prozent_Negativ[31])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_che_Prozent_Positiv = trace_che_Prozent_Positiv[31], trace_che_Prozent_Negativ = trace_che_Prozent_Negativ[31])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_clo_Prozent_Positiv = trace_clo_Prozent_Positiv[31], trace_clo_Prozent_Negativ = trace_clo_Prozent_Negativ[31])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cre_Prozent_Positiv = trace_cre_Prozent_Positiv[31], trace_cre_Prozent_Negativ = trace_cre_Prozent_Negativ[31])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_chei_Prozent_Positiv = trace_chei_Prozent_Positiv[31], trace_chei_Prozent_Negativ = trace_chei_Prozent_Negativ[31])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
        }
        else if (input$filterD== "07.05.2020") {
            
            if (input$filterC== "Corey") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_c_Prozent_Positiv = trace_c_Prozent_Positiv[32], trace_c_Prozent_Negativ = trace_c_Prozent_Negativ[32])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            
            else if (input$filterC== "Corey-IM") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cim_Prozent_Positiv = trace_cim_Prozent_Positiv[32], trace_cim_Prozent_Negativ = trace_cim_Prozent_Negativ[32])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Emmendingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cem_Prozent_Positiv = trace_cem_Prozent_Positiv[32], trace_cem_Prozent_Negativ = trace_cem_Prozent_Negativ[32])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Ravensburg") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cra_Prozent_Positiv = trace_cra_Prozent_Positiv[32], trace_cra_Prozent_Negativ = trace_cra_Prozent_Negativ[32])
                data <- data.frame(x, y)
                
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Calw") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cca_Prozent_Positiv = trace_cca_Prozent_Positiv[32], trace_cca_Prozent_Negativ = trace_cca_Prozent_Negativ[32])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Esslingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cess_Prozent_Positiv = trace_cess_Prozent_Positiv[32], trace_cess_Prozent_Negativ = trace_cess_Prozent_Negativ[32])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Main-Tauber-Kreis") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cmtk_Prozent_Positiv = trace_cmtk_Prozent_Positiv[32], trace_cmtk_Prozent_Negativ = trace_cmtk_Prozent_Negativ[32])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heilbronn") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_che_Prozent_Positiv = trace_che_Prozent_Positiv[32], trace_che_Prozent_Negativ = trace_che_Prozent_Negativ[32])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Loerrach") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_clo_Prozent_Positiv = trace_clo_Prozent_Positiv[32], trace_clo_Prozent_Negativ = trace_clo_Prozent_Negativ[32])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Reutlingen") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_cre_Prozent_Positiv = trace_cre_Prozent_Positiv[32], trace_cre_Prozent_Negativ = trace_cre_Prozent_Negativ[32])
                data <- data.frame(x, y)
                
                
                Plotallgpie <- plot_ly(data, labels = ~x, values = ~y, type = 'pie') 
                Plotallgpie <- Plotallgpie %>%layout(paper_bgcolor='transparent',
                                                     plot_bgcolor='transparent',title = 'korrekt vs falsch beabtwortete Fragen',
                                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
            }
            else if (input$filterC== "Corey-Heidenheim") {
                
                x<-c("Korrekt beantwortet", "Falsch beantwortet")
                y<-rbind(trace_chei_Prozent_Positiv = trace_chei_Prozent_Positiv[32], trace_chei_Prozent_Negativ = trace_chei_Prozent_Negativ[32])
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
        
        x=c("06-04-2020", "07-04-2020", "08-04-2020", "09-04-2020", "10-04-2020", "11-04-2020", "12-04-2020", "13-04-2020", "14-04-2020", "15-04-2020", "16-04-2020", "17-04-2020", "18-04-2020", "19-04-2020", "20-04-2020", "21-04-2020", "22-04-2020", "23-04-2020", "24-04-2020", "25-04-2020", "26-04-2020", "27-04-2020", "28-04-2020", "29-04-2020", "30-04-2020", "01-05-2020", "02-05-2020", "03-05-2020", "04-05-2020", "05-05-2020", "06-05-2020", "07-05-2020", "08-05-2020", "09-05-2020", "10-05-2020", "11-05-2020")
        x <-as.Date(x, format = "%d-%m-%y")
        y=c(119, 132, 120, 111, 94, 61, 45, 57, 170, 236, 306, 239, 176, 135, 191, 276, 437, 419, 434, 316, 384, 467, 433, 422, 439, 413, 422, 483, 595, 505, 745, 722, 469, 364, 343, 449)
        data <- data.frame(trace_c_Datum, trace_c_Prozent_Positiv, trace_c_Prozent_Negativ)
        a <- list(title="Gesamtanzahl der Feedbacks")
        b<- list(title="Datum")
        
        Plotfeed <- plot_ly(x = x) 
        Plotfeed <- Plotfeed %>% add_trace(y = y, name = 'Anzahl',type = 'bar') 
        Plotfeed <- Plotfeed %>%layout(paper_bgcolor='transparent',
                                             plot_bgcolor='transparent',
                                             yaxis = a, xaxis = b)
        
    })
    
    output$Plottop <- renderPlotly({
        
        x=c("Kontaktverbot-Privat Bereich", "Kontaktverbot", "Veranstaltungsverbot", "Aus- und Einreise", "Hotels", "Gastronomie",  "Sport Verordnung", "Neue Verordnung", "Friseure/Kosmetik",  "Regeln Autofahrten")
        y=c(8, 6, 6, 6, 3, 5, 3, 3, 2, 2)
        a <- list(title="Prozent (%)")
        b<- list(title="Themen")
        
        Plotfeed <- plot_ly(x = x) 
        Plotfeed <- Plotfeed %>% add_trace(y = y, name = 'Anzahl',type = 'bar') 
        Plotfeed <- Plotfeed %>%layout(paper_bgcolor='Prozent (%)',
                                       plot_bgcolor='transparent',
                                       yaxis = a, xaxis = b)
        
    })

})
