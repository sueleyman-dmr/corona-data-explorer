#library(reshape2)
#library(shiny)
library(ggplot2)
library(tidyr)
library(DT)

bundesland_list <- c("Baden-Württemberg" = "Baden-Württemberg", "Bayern" = "Bayern", "Berlin" = "Berlin", "Brandenburg" = "Brandenburg", "Bremen" = "Bremen",
                     "Hamburg" = "Hamburg", "Hessen" = "Hessen", "Mecklenburg-Vorpommern" = "Mecklenburg-Vorpommern",
                     "Niedersachsen" = "Niedersachsen", "Nordrhein_Westfalen" = "Nordrhein-Westfalen", "Rheinland-Pfalz" = "Rheinland-Pfalz",
                     "Saarland" = "Saarland", "Sachsen" = "Sachsen", "Sachsen-Anhalt" = "Sachsen-Anhalt", "Schleswig-Holstein" = "Schleswig-Holstein",
                     "Thüringen" = "Thüringen")

covid19_data <- read.csv("covid19.csv", header = TRUE, fileEncoding = "UTF-8")
bundesland <- read.csv("bevoelkerung.csv", fileEncoding = "UTF-8", header=TRUE)

corona_bl_zeit <- aggregate(list(AnzahlFall = covid19_data$AnzahlFall), by=list(Meldedatum = covid19_data$Meldedatum, Bundesland = covid19_data$Bundesland), sum)

corona_bl_zeit <- separate(corona_bl_zeit, Meldedatum, c("Jahr", "Monat", "Tag"), sep = "/", remove = TRUE)

corona_bl_zeit <- aggregate(list(AnzahlFall = corona_bl_zeit$AnzahlFall), by=list(Monat = corona_bl_zeit$Monat, Bundesland = corona_bl_zeit$Bundesland), sum)

corona_bl_zeit_rev <- corona_bl_zeit



corona_bl_zeit_rev[1:10, 3] <- corona_bl_zeit_rev[1:10, 3]/bundesland[1, 2]
corona_bl_zeit_rev[11:21, 3] <- corona_bl_zeit_rev[11:21, 3]/bundesland[2, 2]
corona_bl_zeit_rev[22:30, 3] <- corona_bl_zeit_rev[22:30, 3]/bundesland[3, 2]
corona_bl_zeit_rev[31:40, 3] <- corona_bl_zeit_rev[31:40, 3]/bundesland[4, 2]
corona_bl_zeit_rev[41:49, 3] <- corona_bl_zeit_rev[41:49, 3]/bundesland[5, 2]
corona_bl_zeit_rev[50:58, 3] <- corona_bl_zeit_rev[50:58, 3]/bundesland[6, 2]
corona_bl_zeit_rev[59:68, 3] <- corona_bl_zeit_rev[59:68, 3]/bundesland[7, 2]
corona_bl_zeit_rev[69:77, 3] <- corona_bl_zeit_rev[69:77, 3]/bundesland[8, 2]
corona_bl_zeit_rev[78:87, 3] <- corona_bl_zeit_rev[78:87, 3]/bundesland[9, 2]
corona_bl_zeit_rev[88:97, 3] <- corona_bl_zeit_rev[88:97, 3]/bundesland[10, 2]
corona_bl_zeit_rev[98:107, 3] <- corona_bl_zeit_rev[98:107, 3]/bundesland[11, 2]
corona_bl_zeit_rev[108:117, 3] <- corona_bl_zeit_rev[108:117, 3]/bundesland[12, 2]
corona_bl_zeit_rev[118:126, 3] <- corona_bl_zeit_rev[118:126, 3]/bundesland[13, 2]
corona_bl_zeit_rev[127:135, 3] <- corona_bl_zeit_rev[127:135, 3]/bundesland[14, 2]
corona_bl_zeit_rev[136:145, 3] <- corona_bl_zeit_rev[136:145, 3]/bundesland[15, 2]
corona_bl_zeit_rev[146:154, 3] <- corona_bl_zeit_rev[146:154, 3]/bundesland[16, 2]





ui <- fluidPage(
  
                 title = 'Gruppe G | Shiny - App',
                 
                 
                 
                  #Titel
                 h2("Corona - Rates Explorer", align = "left"),
                 
                
                  #Inputs
                 sidebarPanel(
                   
                   titlePanel(title = h3('Zeitliche Entiwcklung von Corona bundesweit', align = "left")),
                   tags$hr(),
                   
                   radioButtons("haeufigkeit", "Häufigkeitstyp:", c("Absolut" = "absolut", "Relativ" = "relative")),
                   checkboxGroupInput("bl", "Bundeslandauswahl für Graphen: ", bundesland_list, selected = bundesland_list),
                   actionButton("selectall", "Alle auswählen")
                 ),
                 
                 #Output-Platzhalter
                 mainPanel(
                   h1("Corona - Entwicklung (2020)"),
                   tabsetPanel(
                     id = 'dataset',
                     tabPanel("Graph", plotOutput(outputId = "haeufigkeitPlot")),
                     tabPanel("Datensatz", dataTableOutput("myTable")),
                     tabPanel("Quellen", verbatimTextOutput("txt"))
                   )
                 )

)

server = function(input, output, session) {
  
  #3. Tabpanel für Quellen
  output$txt <- renderText({
    quelle1 <- paste("Tabellenoutput: Interaktive Visualisierung eines Histogramms (Shiny-App)",
           "Link: https://moodle.htw-berlin.de/mod/url/view.php?id=787030 (Zugriff nur mit moodle)",
           sep="\n") 
   
    quelle2 <- paste("Spalten separieren: dplyr: separate and unite",
          "Link: https://www.youtube.com/watch?v=ztcsf7G4S78&ab_channel=DataDaft", sep = "\n")
    
    quelle3 <- paste("Alle Checkboxen wählen: R Shiny checkboxGroupInput - select all checkboxes by click",
                     "Link: https://stackoverflow.com/questions/28829682/r-shiny-checkboxgroupinput-select-all-checkboxes-by-click", sep  = "\n")
    
    quelle4 <- paste("How to Start Shiny (Complete)", 
                     "Link: https://shiny.rstudio.com/tutorial/", sep = "\n")
    
    quelle5 <- paste("Textoutput: how to insert new line in R shiny string",
                     "Link: https://stackoverflow.com/questions/26368192/how-to-insert-new-line-in-r-shiny-string", sep = "\n")
    
    quelle6 <- paste("Reactive ggplot: R/Shiny/ggplot2: checkboxGroup to plot specifi data",
                     "Link: https://stackoverflow.com/questions/37343387/r-shiny-ggplot2-checkboxgroup-to-plot-specific-data/37344182#37344182", sep = "\n")
    
    quelle7 <- paste("Plot nach Wahl ändern: How to reactively plot using radioButton and actionButton values in Shiny R",
                     "Link: https://stackoverflow.com/questions/60869265/how-to-reactively-plot-using-radiobutton-and-actionbutton-values-in-shiny-r", sep = "\n")
    
    quelle8 <- paste("Einwohnerzahl der Bundesländer: Fläche und Bevölkerung nach Ländern",
                     "Link: http://www.statistikportal.de/de/bevoelkerung/flaeche-und-bevoelkerung", sep = "\n")
    paste(quelle1, quelle2, quelle3, quelle4, quelle5, quelle6, quelle7, quelle8, sep="\n \n")
  })
  
  
  
  
  ##returnt ausgewaehlten wert
  data1=reactive({
    return(corona_bl_zeit[corona_bl_zeit$Bundesland%in%input$bl,])
  })
  
  data2=reactive({
    return(corona_bl_zeit_rev[corona_bl_zeit_rev$Bundesland%in%input$bl,])
  })
  
  
  
  ##Haeufigkeitsart für den Graphen
  observe({
    observeEvent(input$haeufigkeit, {
      if(input$haeufigkeit == 'absolut') {
        output$haeufigkeitPlot <- renderPlot({
          ggplot(data=data1(), aes(x=Monat, y = AnzahlFall, group=Bundesland, colour=Bundesland)) + 
            geom_line(alpha=9, size = 0.715) + 
            ggtitle("Bundesweiter absoluter Corona-Graph") +
            scale_x_discrete(breaks = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), 
                             labels = c("Januar", "Februar", "März", "April", "Mai", "Juni", "Juli", "August", "September", "Oktober", "November", "Dezember")) +
            theme(legend.position = 'bottom', legend.title = element_text(size=18), legend.text = element_text(size=16), axis.text = element_text(size=14), axis.title = element_text(size = 16)) + 
            guides(color = guide_legend(override.aes = list(size=5))) + 
            ylab("Anzahl der Infizierte")
        }, height = 800)
        output$myTable  <- renderDataTable({
          corona_bl_zeit
        }, options = list(orderClasses = TRUE, lengthMenu = c(5, 10, 20, 50), pageLength = 30,
                          initComplete = JS(
                            "function(settings, json) {",
                            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                            "}")))
      }
      
     
      
      else if(input$haeufigkeit == 'relative') {
        output$haeufigkeitPlot <- renderPlot({
          ggplot(data=data2(), aes(x=Monat, y = AnzahlFall, group=Bundesland, colour=Bundesland)) + 
            geom_line(alpha=0.9, size = 0.715) + 
            ggtitle("Bundesweiter relativer Corona-Graph") +
            scale_x_discrete(breaks = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), 
                             labels = c("Januar", "Februar", "März", "April", "Mai", "Juni", "Juli", "August", "September", "Oktober", "November", "Dezember")) +
            theme(legend.position = 'bottom', legend.title = element_text(size=18), legend.text = element_text(size=16), axis.text = element_text(size=14), axis.title = element_text(size = 16)) + 
            guides(color = guide_legend(override.aes = list(size=5))) +
            ylab("Anzahl der Infizierte")
        }, height = 800)
        
        output$myTable = renderDataTable({
          corona_bl_zeit_rev
        }, options = list(orderClasses = TRUE, lengthMenu = c(5, 10, 20, 50), pageLength = 30,
                          initComplete = JS(
                            "function(settings, json) {",
                            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                            "}")))
      }
    })
  })
  
  #Button für Auswahl der Checkboxen
  #SelectAll muss manchmal doppelt geklickt werden
  observe({
    if(input$selectall == 0) return(NULL)
    else if (input$selectall%%2 == 0) 
    {
      updateCheckboxGroupInput(session,"bl", "Bundeslandauswahl für Graph: ", choices=bundesland_list,selected=bundesland_list)
    }
  })


  
  
}
shinyApp(ui=ui, server = server)
