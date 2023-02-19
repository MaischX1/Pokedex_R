#
# Der R Code umfasst das Pokedex für das Modul Internet-Technologien an der
# Hochschule Wismar (Wings)


 
  # Die Session soll interaktiv bzw. reaktiv funktionieren
  if (interactive()) {options(device.ask.default = FALSE)
  
  
  # Aktivieren der benötigten Packages
  library(shiny)                  
  library(shinydashboard)          
  library(readxl)            
  library(DT)                       
  library(ggplot2)                   
  library(dplyr)                 
  library(plotly)             
  library(fmsb)                      
  library(shinyWidgets)              
  library(openxlsx)                  

    
  ########## Ab hier werden Dateien und Variablen eingelesen #############  
  
  
  # Definieren eines Pfades, wo alle relevanten Dateien abliegen
  path <- "./"
  
  
  # Universale XLSX Basisdatei als Variable "df" für den Data Table und jegliche Grafiken  
  df <- read_xlsx(paste0(path, "pmlist_all.xlsx"))
  
  
  # Die Spalten "caught" und "shiny_released" sollen immer logische Spalten sein (True/False)
  df$caught <- as.logical(df$caught)
  df$shiny_released <- as.logical(df$shiny_released)
  
  # Auf Basis der Spalte "de" sollen links zu den Pokemonbildern für den Data Table generiert werden 
  img_uri <- function(x) { sprintf('<img src="%s" height="52"/>', knitr::image_uri(paste0(path, "df_img/", x, ".png"))) }
  
  # Hinterlegen dieser Links im Data Table, damit diese angezeigt werden können
  df <- cbind(data.frame(image = unlist(lapply(df$de, img_uri)), stringsAsFactors = F), df)
  
  
  # Definieren eine Farbpalette mit den Hex Farben der verschiedenen Pokemontypen für die Grafiken
  color_palette_types <- c("#A6B91A", "#705746", "#6F35FC", "#F7D02C", "#D685AD", "#C22E28",
                                    "#EE8130", "#735797", "#7AC74C", "#E2BF65","#96D9D6", "#A8A77A",
                                    "#A33EA1","#F95587", "#B6A136", "#B7B7CE", "#6390F0")
                                    
  
  # XLSX Datei "df_specs" mit den Kampfattributen als Basis für den Radar Chart
  df_specs <- read_xlsx(paste0(path, "pmlist_specs.xlsx"))
  
  # Aus df_specs" erzeugter und optimierter Datensatz für den Radar Chart
  df_specs_radar <- data.frame(df_specs[,-1, drop = FALSE], row.names = df_specs$de)
  
  # Erstellen eines Vektors namens "types" für die Filterung der Pokemontypen innerhalb des Datatables (df)
  types <- c("all",unique(df$types))
  
  # Deutsche Namen für den Typenfilter hinterlegen
  names(types) <- c("Alle","Pflanze","Feuer","Wasser","Käfer","Normal","Gift","Elektro","Boden",
                    "Fee","Kampf","Psycho","Gestein","Geist","Eis","Drache","Unlicht","Stahl")
  
  # Vektor für die Selektion der Sprache der Pokemonnamen, entspricht Spalte 3-8 in der Excel Datei
  langs <- colnames(df)[3:8]
  
  # Namen für die Sprache definieren, da in der Excel (df) nur "de", "en", usw. steht
  names(langs) <- c("Deutsch","Englisch","Französisch","Japanisch","Koreanisch","Chinesisch")
  
  # Für die beiden Pokemon Selektionen auf Seite 3 habe ich mich aus der Spalte "de" der Excelliste bedient 
  pkm_selection <- df %>% select(de)
  
  
  ########## Bis hier werden Dateien und Variablen eingelesen #############  
  
  
  ## Beginn der UI ##
  
  ui <- dashboardPage(skin = "red",
                
       # Im Dashboardheader wird das Pokemon Logo angezeigt, oben rechts noch ein Message Item             
       dashboardHeader(title = img(src = "pokemon.png", height = 45, width = 120),
            dropdownMenu(type = "message", 
                         messageItem(from = "Einführung", message="Willkommen im Pokedex", icon("star")))
                       ),
                
                
       # Definieren der Sidebars innerhalb der UI
       dashboardSidebar(
             sidebarMenu(
                    
                 menuItem("Übersicht", tabName = "01", icon = icon("clipboard")),
                    
                 menuItem("Pokedex", tabName = "02", icon = icon("list")),
                    
                 menuItem("Pokemonvergleich", tabName = "03", icon = icon("check"), badgeLabel = "Check", badgeColor = "green" )
                        )
                       ),
                
       # Aufbau der Seiten innerhalb der Sidebar Items 01-03
       dashboardBody(
             tabItems(
               tabItem(tabName = "01",     # Referenz auf den TabName
               fluidRow(                   # Anordnung des Layout
                   column(12,              # Definieren der Bereiche innerhalb des Layouts
                                     
                   titlePanel(h2(strong("Übersicht"))),
                                     
                   # Definieren der Infoboxen 1-3, Seite 1
                   # infoBox steht für sich, infoBoxOutput hat einen Serverbezug
                   infoBox("Pokemon", sum(df$active), icon = icon("list"), color = "red"),
                                     
                   infoBoxOutput("caught_count"),
                                     
                   infoBoxOutput("caught_percent"),
                                     
                   # Ab hier die zweite Reihe Seite 1       
                   # Einfügen einer Box für die Grafik 1
                    box(width = 8, status = "danger",
                      mainPanel(
                                           
                         # UI Darstellung Grafik 1 (Punktdiagramm) mit Referenz zum Server   
                         plotlyOutput("gg_type_score", width = "150%", height = "420px"))),
                                     
                                     # Einfügen zweiten Box für das Pikatchu   
                    box(width = 4, status = "danger",
                                         
                         # Pikatchu Bild aus dem www Ordner       
                         img(src = "pikaintro.png", width = "110%", height = "420px")),
                                     
                    # Ab hier die dritte Reihe Seite 1       
                    # Einfügen einer Box für die Grafik 2        
                    box(width = 4,
                      mainPanel(
                                           
                         # UI Darstellung Grafik 2 (Balkendiagramm) mit Referenz zum Server
                         plotlyOutput("gg_generation", width = "150%", height = "450px"))),
                                     
                    # Einfügen einer Box für die Grafik 3      
                    box(width = 8,
                      mainPanel(
                                           
                         # UI Darstellung Grafik 3 (Punkt-Strich-Diagramm) mit Referenz zum Server 
                         plotlyOutput("gg_type_score_mean", width = "150%", height = "450px"))),
                                     
                                     
                    # Output der Bilder der drei Generationen innerhalb von Boxen, Seite 1
                    box(width = 12,
                                         
                    box(title = "Generation 1: Kanto", width = 4, status = "danger", solidHeader = TRUE,
                                             
                         img(src = "kanto.png", width = "100%")),
                                         
                    box(title = "Generation 2: Johto", width = 4, status = "danger", solidHeader = TRUE,
                                             
                         img(src = "johto.png", height = "100%", width = "100%")),   
                                         
                    box(title = "Generation 3: Hoenn", width = 4, status = "danger", solidHeader = TRUE,
                                             
                         img(src = "hoenn.png", width = "100%")))
                                     
                        ) # Abschluss column
                       ) # Abschluss fluidRow
                      ), # Abschluss tabIteam 1
                    
                    
               # Beginn Seite 2, Output der Filter und des Data Tables     
               tabItem(tabName = "02",
                fluidRow(
                    column(12,
                                     
                         titlePanel(h2(strong("Pokedex"))), # Überschrift
                                     
                       box(width = 6, status = "danger",
                                         
                         # UIOutput für die Sprachenfilterung mit Referenz zum Server  
                         selectInput("langSelector","Sprache wählen",langs)),
                                     
                       box(width = 6, status = "danger",
                                         
                         # UIOutput für die Typfilterung mit Referenz zum Server  
                         selectInput("typeOneSelector","Typ 1 wählen",types)),
                                     
                                     
                       box(width=12,
                                         
                         # UIOutput des Datatables df auf Basis der eingelesenen Daten 
                         DT::dataTableOutput("df"))
                                     
                                     
                         ) # Abschluss column
                        ) # Abschluss fluidRow
                       ), # Abschluss tabIteam 2
                    
                    
                    
               # Beginn der Seite 3, auswählen und Vergleichen der Pokemon 
               # Darstellung Links
               tabItem(tabName = "03",
               fluidRow(
                  column(12,
                                     
                         titlePanel(h2(strong("Pokemonvergleich"))), # Überschrift
                                     
                  fluidRow(
                     column(4,
                    fluidRow(
                       column(6, 
                                                       
                            # Dropdown mit den Pokemon aus Spalte "de" als Picker für die weiteren Aktionen   
                            selectInput(inputId = "pkm", label = "Pokemon wählen", choices = c("Name",pkm_selection))),
                                                
                       column(6,
                                                       
                            # Wenn das Dropdown nicht genutzt werden soll, auch per Freitext möglich         
                            textInput("pkm_text", label = "Pokemon Freitext"))),
                                              
                       fluidRow(
                          column(12,
                                                       
                              box(width = 12, title = "Pokemon #1", status = "danger", solidHeader = TRUE, 
                                                           
                                 # Linker UI Output des Pokemon mit Referenz zum Server                       
                                 imageOutput("pkm_pic"),
                                                           
                            column(10,    
                                                                  
                                 # Erstellen eines Switches innerhalb der Box und dem Bild zwecks "gefangen"     
                                 strong(materialSwitch("check", label = "Gefangen", status = "danger", 
                                                                                        right = TRUE))),
                                                           
                             column(2,
                                                                  
                                 # Ausgeben der Pokemonnummer im Bild rechts unter aus der Spalte "dex" mit Bezug zum Server                                          
                                 strong(textOutput("text")))
                                                           
                                 )
                                )
                               ),
                                              
                       fluidRow(
                          column(12,
                                                       
                              box(width = 14, status = "danger",
                                                           
                                 # Ausgeben der Specs des ausgewählten Pokemons mit Bezug zum Server                                 
                                 tableOutput("pkm_specs_selection"))))
                                              
                           ), # Ende Darstellung links
                                       
                                       
              # Darstellungen Mitte
               column(4,   
              fluidRow(
                 column(6,
                                                       
                       box(width = 12, title = strong("Typen"), status = "danger",
                                                           
                         # Ausgeben der Typen des ausgewählten Pokemons links mit Bezug zum Server                            
                         tableOutput("pkm_types_selection"))),
                                                
                 column(6,
                                                       
                       box(width = 12, title = strong("Typen"), status = "primary",
                                                           
                                                           # Ausgeben der Typen des ausgewählten Pokemons rechts mit Bezug zum Server                                 
                         tableOutput("pkm_types_selection_2")))
                      ),
                                              
              fluidRow(
                 column(12,
                                                       
                       box(width = 12,
                                                           
                         # Ausgeben des Radarcharts beider Pokemon mit Bezug zum Server                              
                         plotOutput("radar", width = "100%", height = "355px")))
                      ),
                                              
              fluidRow(
                                                
                         titlePanel(h2(strong("Schillernde Pokemon"), align = "center")),
                                                
                 column(6,
                                                       
                       box(width = 12, height = "200px", status = "danger", 
                                                           
                         # Ausgeben des Shiny Bilds des Pokemon links mit Bezug zum Server                               
                         plotOutput("pkm_pic_shiny", width = "100%", height = "250px"))),
                                                
                 column(6,
                                                       
                       box(width = 12, height = "200px", status = "primary",
                                                           
                         # Ausgeben des Shiny Bilds des Pokemon rechts mit Bezug zum Server                                 
                         plotOutput("pkm_pic_shiny_2", width = "100%", height = "250px")))
                      )
                     ), # Ende Darstellung Mitte
                                       
                                       
               # Darstellung Rechts
               column(4,  
              fluidRow(
                 column(6, 
                                                       
                         # Dropdown mit den Pokemon aus Spalte "de" als Picker für die weiteren Aktionen   
                         selectInput(inputId = "pkm_2", label = "Pokemon wählen", choices = c("Name", pkm_selection))),                        
                                                
                 column(6,
                                                       
                         # Wenn das Dropdown nicht genutzt werden soll, auch per Freitext möglich                                   
                         textInput("pkm_text_2", label = "Pokemon Freitext"))
                       ),
                                              
              fluidRow(
                 column(12,
                                                       
                       box(width = 12, title = "Pokemon #2", status = "primary", solidHeader = TRUE, 
                                                           
                         # Rechter UI Output des Pokemon mit Referenz zum Server 
                         imageOutput("pkm_pic_2"),
                                                           
                  column(10,
                                                                  
                         # Erstellen eines Switches innerhalb der Box und dem Bild zwecks "gefangen"  
                         strong(materialSwitch("check_2", label = "Gefangen", status = "primary",
                                                                                        right = TRUE))),
                                                           
                  column(2,
                                                                  
                         # Ausgeben der Pokemonnummer im Bild rechts unter aus der Spalte "dex" mit Bezug zum Server                                         
                         strong(textOutput("text_2")))
                                                           
                          )
                         )
                        ),
                                              
              fluidRow(
                 column(12,
                                                       
                       box(width = 14, status = "primary",
                                                           
                         # Ausgeben der Specs des ausgewählten Pokemons mit Bezug zum Server           
                         tableOutput("pkm_specs_selection_2"))))
                                              
                     )
                    )
                   )
                  ) 
                 )
                )
               )
              )
             }
  
  
  
  # Beginn des Backend (Serverfunktionen)
  server <- function(input, output, session) {

  # Es wird die Variable "data" erstellt. Diese zählt die in der Spalte "caught" hinterlegten TRUE Werte
  # Da sich die Werte innerhalb der Session durch den User ändern können (Seite 3), ist die Variable reaktiv.
  data <- reactiveValues(dff = df, anzahl_gefangen = sum(df$caught))  
  
  
  # Es wird der aktuelle "gefangen" Wert genommen und in der InfoBox 2, Seite 1 dargestellt.
  output$caught_count <- renderInfoBox({
    
    infoBox("Gefangen", data$anzahl_gefangen, icon = icon("list"), color = "red")
    
  })
  
  # Der "gefangen" Wert wird verwendet, um für Infobox 3, Seite 1 den prozentualen Fortschritt auszurechnen
  # (Gefangen/Gesamt)*100 = Fortschritt in %
  output$caught_percent <- renderInfoBox({
    
    infoBox("Fortschritt", paste0(round(data$anzahl_gefangen/nrow(df)*100, digits = 1), "%"),
            icon = icon("list"), color = "red")
    
  })
  
  # Mittels GGPlot und Plotly wird die Grafik 1, Seite 1 erstellt. 
  output$gg_type_score <- renderPlotly({ 
    
    df %>% plot_ly(x=~types, y=~Score, split=~types,                       # Bestimmen X und Y
                   type="scatter",color=~types, mode="markers",            # Typ und Farbe
                   colors = color_palette_types,                           # Referenz auf die Typenfarbenvariable
                   
                   hovertext = ~ paste0("Name: ", de, "<br>",              # Die Textbox, wenn wir auf den Wert clicken
                                        "Score: ", Score, "<br>",
                                        "Type: ", types, "<br>"),
                   hoverinfo = "text") %>%
    layout(title = "Verteilung der Stärkewerte nach Typen",          # Titel
             yaxis = list(title = 'Stärkewert'),                     # Beschriftung X und Y Achse
             xaxis = list(title = 'Typen'))            
    
  })
  
  # Mittels GGPlot und Plotly wird die Grafik 2 (Balkendiagramm), Seite 1 erstellt. 
  output$gg_generation <- renderPlotly({
    
    # Die Variable p wird erzeugt. Diese reagiert auf die Eingaben "gefangen" des Users
    p <- data$dff %>%
      select(Generation, caught) %>%                            # Auswählen der Achsenvariablen aus den Spalten des df
      ggplot(aes(x = Generation, fill = caught)) +              # Füllung soll die "gefangenen Pkm abbilden        
      geom_bar (alpha = 0.7) +                                  # Transparenz           
      ggtitle("Anzahl Pokemon nach Generation") +               # Titel und Beschritungen   
      xlab ("Generation") + ylab ("Anzahl") +
      theme_classic(base_size = 12) +                           # Auswahl des Templates
      scale_fill_manual(values=c("#C0C0C0", "#CE2211"))         # Eigene Farben
    
    ggplotly(p)                                                  # Ausgabe des Balkendiagramms
    
  })
  
  # Mittels GGPlot und Plotly wird die Grafik 3 (Punkt-Strich-Diagramm), Seite 1 erstellt.
  # Aufbau ist ähnlich die Grafik 2
  output$gg_type_score_mean <- renderPlotly({
    
    p <- df %>% select(types, Score) %>%
      ggplot(aes(x = types, y = Score)) +
      geom_point(stat = "summary", fun = "mean", size = 5, color = color_palette_types) + 
      geom_segment( aes(x = types, xend = types, y = 0, yend = Score )) +
      coord_flip() + ggtitle("Durchschnittlicher Stärkewert nach Typen") +
      ylab ("Stärkewert") + xlab ("Typen") +
      theme_classic(base_size = 12)
    
    ggplotly(p)
    
  })
  
  
  # Beginn der Outputs von Seite 3 
  # Output Links
  
  # Das Bild links wird mit dem Namen des ausgewählten Pkms im "Image" Ordner gesucht und bereitgestellt
  output$pkm_pic <- renderImage({
    
    pkm <- if(input$pkm_text!=""){input$pkm_text}else{input$pkm}      # Input, entweder durch Dropdown oder Freitext
    print(pkm)
    
    if (!(pkm %in% df$de)) {                                          # Vergleiche den Input mit der Spalte "de"
      pkm <- "Name"}
    
    filename <- normalizePath(file.path(paste0(path, "images"),       # Erzeuge den Bildnamen und suche im "images" Ordner
                                        paste("",pkm, ".png",sep = "")))
    
    list (src = filename, width = "100%", height = "100%")            # Auswahl des Bildes
    
  }, deleteFile = FALSE)                                              # Das Bild soll anschließend nicht gelöscht werden
  
  
  # Die Specs des ausgewählten Pkms werden aus dem Datensatz entnommen und als kleiner Data Table dargestellt
  output$pkm_specs_selection <- renderTable(
    
    df %>% filter(if(input$pkm_text!=""){input$pkm_text == df$de}else{input$pkm == df$de}) %>% 
           select(Score, HP, Attack, Defense, Sp_Atk, Sp_Def, Speed)
    
  )
  
  # Erzeugt den Text in Form der Pkm Nummer unter dem Bild
  output$text <- renderText({
    ifelse(input$pkm == "Name" & input$pkm_text == "", paste (""),  # Wenn Name ausgewählt ist, soll keine Nummer angezeigt werden
           paste("# ", df %>% filter(if(input$pkm_text!=""){input$pkm_text == df$de}else{input$pkm == df$de}) %>% 
                   select(dex)))
    
  })
  
  # Output Mitte
  # links
  
  # Ähnlich wie bei den Specs wird ein kleiner Data Table mit den Typen des ausgewählten Pkms erzeugt
  output$pkm_types_selection <- renderTable(
    
    df %>% filter(if(input$pkm_text!=""){input$pkm_text == df$de}else{input$pkm == df$de}) %>% 
      select(types, types_2), width = "100%", na = "", colnames = F
    
  )
  
  
  # Mitte,rechts
  output$pkm_types_selection_2 <- renderTable(
    df %>% filter(if(input$pkm_text_2!=""){input$pkm_text_2 == df$de}else{input$pkm_2 == df$de}) %>% 
      select(types, types_2), width = "100%", na = "",
    colnames = F
    
  )
  
  # Mitte
  
  # Radar Chart
  output$radar <- renderPlot({
    
    # Referenz auf den für den Radar Chart angelegten Data Frame "df_specs_radar" sowie Auswahl des anzuzeigenden Pkms
    radardf <- df_specs_radar[c("Max","Min", c(if(input$pkm_text!=""){input$pkm_text}else{input$pkm}), 
                                if(input$pkm_text_2!=""){input$pkm_text_2}else{input$pkm_2}), ]
    
    # Erstellen der Farben für beide Radars mithilfe von rgb Farben und Vektoren
    colors_border = c(rgb(1, 0.2, 0.1, 1),
                      rgb(0.1, 0.2, 0.8, 0.7))
    
    colors_in =     c(rgb(1, 0.2, 0.1, 0.7),
                      rgb(0.0, 0.2, 1, 0.2))
    
    # Radar Chart wird erstellt und konfiguriert
    radarchart(radardf,
               
               axistype = 1 ,
               # Anpassung des Polygons
               pcol = colors_border,
               pfcol = colors_in,
               plwd = 4 ,
               plty = 1,
               # Anpassung des Gitters
               cglcol = "grey",
               cglty = 1,
               axislabcol = "grey",
               caxislabels = seq(0, 20, 5),
               cglwd = 0.8,
               # Anpassung der Schriften und des Titels
               vlcex = 0.8,
               title = "Radar Chart")
    
  })
  
  # Shiny Bild (links) erzeugen. Das identische Vorgehen wie bei den Pkm Bildern
  output$pkm_pic_shiny <- renderImage({
    
    pkm <- if(input$pkm_text!=""){input$pkm_text}else{input$pkm}
    
    if (!(pkm %in% df$de)) {
      pkm <- "Name"}
    
    filename <- normalizePath(file.path(paste0(path, "shiny"), # Statt "images" jetzt "shiny"
                                        paste("",pkm, ".png",sep = "")))
    
    list (src = filename, width = "100%", height = "150px")
    
  }, deleteFile = FALSE)
  
  
  # Shiny Bild (rechts) erzeugen. Das identische Vorgehen wie bei den Pkm Bildern
  output$pkm_pic_shiny_2 <- renderImage({
    
    pkm <- if(input$pkm_text_2!=""){input$pkm_text_2}else{input$pkm_2}
    
    if (!(pkm %in% df$de)) {
      pkm <- "Name"}
    
    filename <- normalizePath(file.path(paste0(path, "shiny"), # Statt "images" jetzt "shiny"
                                        paste("",pkm, ".png",sep = "")))
    
    list (src = filename, width = "100%", height = "150px")
    
  }, deleteFile = FALSE)
  
  
  # Output Rechts
  # Das weitere Vorgehen ist identisch zum Output links, nur mit der "_2" im Namen der Variablen 
  
  output$pkm_pic_2 <- renderImage({
    
    pkm <- if(input$pkm_text_2!=""){input$pkm_text_2}else{input$pkm_2}
    
    if (!(pkm %in% df$de)) {
      pkm <- "Name"}
    
    filename <- normalizePath(file.path(paste0(path, "images"),
                                        paste("",pkm, ".png",sep = "")))
    
    list (src = filename, width = "100%", height = "100%")
    
  }, deleteFile = FALSE)
  
  
  output$pkm_specs_selection_2 <- renderTable(
    
    df %>% filter(if(input$pkm_text_2!=""){input$pkm_text_2 == df$de}else{input$pkm_2 == df$de}) %>% 
      select(Score, HP, Attack, Defense, Sp_Atk, Sp_Def, Speed)
    
  )
  
  output$text_2 <- renderText({
    
    ifelse(input$pkm_2 == "Name" & input$pkm_text_2 == "", paste (""),
           
    paste("# ", df %>% filter(if(input$pkm_text_2!=""){input$pkm_text_2 == df$de}else{input$pkm_2 == df$de}) %>% 
          select(dex)))
    
  })
  
  # Abschnitt für das Hinterlegen von Pokemon als gefangen sowie Aktualisierung der Excel Datei  
  
  observeEvent(input$check, {
    
    # Jede Änderung, welche sich auf das ausgewählte Pkm bezieht, soll überwacht werden
    df <- data$dff
    if (input$pkm_text!="") {
      pkm <- input$pkm_text
      df[which(df$de == pkm), "caught"] <- input$check  # Wenn der Check true ist, wird caught auch auf true gesetzt
    } else if (input$pkm != "Name") {                # Wenn Name ausgewählt ist, soll nichts passieren
      pkm <- input$pkm
      df[which(df$de == pkm), "caught"] <- input$check # Wenn der Check true ist, wird caught auch auf true gesetzt
    }
    
    data$anzahl_gefangen <- sum(df$caught) # Update den Wert für die Infoboxen 2 & 3
    
    # Speichere den df in der Excel (aber ohne die Spalte mit den Bildpfaden)
    openxlsx::write.xlsx(x=df[,-1], file=paste0(path, "pmlist_all.xlsx"), rowNames=FALSE, overwrite=T)
    
    data$dff <-  df
    
  })
  # Das gleiche Vorgehen muss nun auch für das rechte Pkm umgesetzt werden. Mit if und else wegen dem Pokeball.
  observeEvent(input$check_2, {
    
    df <- data$dff
    if (input$pkm_text_2!="") {
      pkm <- input$pkm_text_2
      df[which(df$de == pkm), "caught"] <- input$check_2
    } else if (input$pkm_2 != "Name") {
      pkm <- input$pkm_2
      df[which(df$de == pkm), "caught"] <- input$check_2
    } 
    
    data$anzahl_gefangen <- sum(df$caught) # Update den Wert für die beiden Infoboxen
    
    # Speichere den df in der Excel (aber ohne die Spalte mit den Bildpfaden)
    openxlsx::write.xlsx(x=df[,-1], file=paste0(path, "pmlist_all.xlsx"), rowNames=FALSE, overwrite=T)
    print("Excel aktualisiert")
    
    data$dff <- df
    
  })
  
  # Der Data Table von Seite 2 wird zusammengesetzt. Zudem reagiert dieser auf die true und false Werte der "caught" Spalte  
  output$df <- DT::renderDataTable({
    
    df <- data$dff 
    df[if(input$typeOneSelector!="all"){df$types %in% input$typeOneSelector}else{df$types %in% types}, # Filterkonfiguration für Sprache und Typ
       c("image", "dex",input$langSelector,"types","types_2","caught","family","Score","HP","Attack", 
         "Defense","Sp_Atk","Sp_Def","Speed","Generation")]                  # Alle Spalten, die angezeigt werden sollen
    
    # Konfiguration, Scrollen, keine Reihennamen usw.  
  }, options = list(scrollX = T), rownames = FALSE, filter = "top", selection = "multiple" , escape=FALSE) 
  
  
  
  # Konfiguration des Material Switches (Slider ob gefangen oder nicht)
  # Erster Slider
  
  # Wenn ein input für den Dropdown oder Freitext existiert, update den Material Switch mit dem Wert vom Data Frame
  toListen <- reactive({
    list(input$pkm, input$pkm_text) # Es soll auf mehr als einen Input reagiert werden
  })
  
  observeEvent(toListen(), {
    df <- data$dff
    if (input$pkm_text == "") { 
      if (input$pkm != "Name") { # Da auf der linken Seite "Name" hinterlegt ist, muss eine Ausnahme berücksichtigt werden
        if (input$pkm %in% df$de) {
          updateMaterialSwitch(session = session, "check", value = df[[which(df$de == input$pkm), "caught"]])
        }
      }} 
    else {
      if(input$pkm_text %in% df$de) {
        updateMaterialSwitch(session = session, "check", value = df[[which(df$de == input$pkm_text), "caught"]])
      }
    }
  })
  
  # Slider 2
  # Gleiches Vorgehen wie beim ersten Slider, nur für das rechte Pkm
  toListen_2 <- reactive({
    list(input$pkm_2, input$pkm_text_2) # das kann man benutzen für observeevent, wenn man auf mehr als einen input reagieren will
  })
  
  observeEvent(toListen_2(), {
    df <- data$dff
    if (input$pkm_text_2 == "") {
      if (input$pkm_2 != "Name") { # Da auf der rechten Seite ebenfalls "Name" hinterlegt ist, muss eine Ausnahme berücksichtigt werden
        if (input$pkm_2 %in% df$de) {
          updateMaterialSwitch(session = session, "check_2", value = df[[which(df$de == input$pkm_2), "caught"]])
        }
      }}
    else {
      if(input$pkm_text_2 %in% df$de) {
        updateMaterialSwitch(session = session, "check_2", value = df[[which(df$de == input$pkm_text_2), "caught"]])
      }
    }
  })
}

  shinyApp(ui, server)