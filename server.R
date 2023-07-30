source("global.R")

server <- function(input,output, session) {
  
  dataInput <- eventReactive(input$update, {
    get_raw_lichess(input$usuario) %>%
      mutate(Color = case_when(White == "Guillebarracas"~"Blanco",
                               TRUE ~ "Negro" ),
             Elo = case_when(Color == "Blanco" ~ WhiteElo,
                             TRUE ~ BlackElo),
             Elo_Rival = case_when(Color == "Blanco" ~ BlackElo,
                                   TRUE ~ WhiteElo),
             Dif_Elo = as.numeric(Elo)-as.numeric(Elo_Rival),
             Resultado = case_when(Color == "Blanco" & Result == "1-0" ~ "Victoria",
                                   Color == "Blanco" & Result == "0-1" ~ "Derrota",
                                   Color == "Negro" & Result == "0-1" ~ "Victoria",
                                   Color == "Negro" & Result == "1-0" ~ "Derrota",
                                   TRUE ~ "Tablas"),
            WhiteRatingDiff = as.numeric(as.character(WhiteRatingDiff)),
            BlackRatingDiff = as.numeric(as.character(BlackRatingDiff)),
            Elo_Actual=ifelse(Color == "Blanco",
                              as.numeric(Elo)+WhiteRatingDiff,
                              as.numeric(Elo)+BlackRatingDiff),
            Fecha = as.Date(ymd(paste0(substr(Date,1,4),substr(Date,6,7),substr(Date,9,10)))),
            Hora = as.numeric(substr(UTCTime,1,2)),
            Hora_arg = case_when(Hora == 3 ~ 0,
                                 Hora == 2 ~ 23,
                                 Hora == 1 ~ 22,
                                 Hora == 0 ~ 21,
                                 TRUE ~ (Hora - 3)),
            Hora_intervalo = case_when(Hora_arg >= 8 & Hora_arg <= 13 ~ "Manana",
                                       Hora_arg >= 14 & Hora_arg <= 19 ~ "Tarde",
                                       TRUE ~ "Noche"),
            weekday = wday(Fecha,1),
            Mes = month(Fecha))%>%  
      select(!Moves)
  })

  output$text <- renderText({
    paste("El usuario ", input$usuario, "registra un total de ", nrow(dataInput()), "partidas")
  })
    
  output$eloactual <- renderValueBox({
    valueBox(dataInput() %>% 
               select(Fecha, Elo_Actual) %>% 
               arrange(desc(Fecha)) %>%
               select(Elo_Actual) %>% 
               head(1),"Elo actual", color = "aqua")
  })

  output$fechaini <- renderValueBox({
    valueBox(dataInput() %>% 
               select(Fecha) %>% 
               arrange(Fecha) %>% 
               head(1), "Fecha 1era partida", color = "light-blue")
  })
  
  output$winrate <- renderValueBox({
      valueBox(dataInput() %>%
                 filter(TimeControl == "300+3") %>%
                 head(30) %>% 
                 group_by(Resultado) %>% 
                 summarise(Total = n()) %>% 
                 mutate(Porcentaje = round(Total/sum(Total)*100,1)) %>%
                 mutate(Etiqueta = paste0(Total, " (% ", Porcentaje, ")")) %>% 
                 filter(Resultado == "Victoria") %>%
                 select(Etiqueta) %>% 
                 pull() ,"Winrate últimas 30 partidas", color = "green")
  })
  
 
  output$winraten <- renderValueBox({
    valueBox(dataInput() %>%
               #filter(TimeControl == "300+3") %>%
               filter(Color == "Negro") %>% 
               group_by(Resultado) %>% 
               summarise(Total = n()) %>% 
               mutate(Porcentaje = round(Total/sum(Total)*100,1)) %>%
               mutate(Etiqueta = paste0(Total, " (% ", Porcentaje, ")")) %>% 
               filter(Resultado == "Victoria") %>%
               select(Etiqueta) %>% 
               pull() ,"Winrate Negras", color = "black")
  })  
  
  output$winrateb <- renderValueBox({
    valueBox(dataInput() %>%
               filter(TimeControl == "300+3") %>%
               filter(Color == "Blanco") %>% 
               group_by(Resultado) %>% 
               summarise(Total = n()) %>% 
               mutate(Porcentaje = round(Total/sum(Total)*100,1)) %>%
               mutate(Etiqueta = paste0(Total, " (% ", Porcentaje, ")")) %>% 
               filter(Resultado == "Victoria") %>%
               select(Etiqueta) %>% 
               pull() ,"Winrate Blancas", color = "lime")
  }) 
  
  output$elopromedio <- renderValueBox({
    valueBox(mean(dataInput()$Elo),"Elo_Medio", color = "lime")
  }) 
  
  
  
   
  output$plot1 <- renderPlotly({
    ggplotly(
    ggplot(dataInput() %>%
             filter(TimeControl == "300+3") %>% 
             select(Resultado) %>% 
             group_by(Resultado) %>% 
             summarise(Total = n()) %>% 
             mutate(Porcentaje = round(Total/sum(Total)*100,1)) %>% 
             mutate(Etiqueta = paste0(Total, " (% ", Porcentaje, ")")), aes(x = "" , y = Total, fill = Resultado ))+
      geom_bar(stat = "identity", width = 0.5)+
      geom_text(aes(label = Etiqueta), color = "#ada9a9",position = position_stack(vjust = 0.5))+
      scale_fill_manual(values = c("#272643", "#e3f6f5", "#2c698d"))+
      theme_minimal()+
      #guides(fill = guide_legend(reverse = TRUE))+
      coord_flip()) %>% 
      layout(title = list(text = paste0('<br>','Resumen Partidas totales',
                                        '<br>')))
  })
  
  output$plot2 <- renderPlotly({
  fig1 <- ggplot(dataInput()%>%
                   filter(TimeControl == "300+3"), aes(x = Fecha, y = Elo_Actual))+ #seleccionamos variables a graficar y a colorear
    #geom_line(color = "#2c698d")+
    geom_point(color = "#2c698d")+
    geom_hline(yintercept = mean(dataInput()$Elo_Actual),
               color =  "#000066")+
    labs(title = paste0("                   Evolutivo Elo "),
         subtitle = "Evolución Ranking Elo",
         caption = "API Lichess",
         y = "Elo",
         x = "Fecha")+
    scale_color_manual(values = c("#2c698d"))+ #coloreamos las lineas
    scale_x_date(date_breaks = "1 year",date_labels = "%b %y")+
    scale_y_discrete(breaks = seq(1500, 2000, by = 50))+
    theme_minimal()
  
  
  fig1 <- ggplotly(fig1) 

  
  fig1
    
  })
  
  
  output$table <- renderDT(dataInput() %>%
                             select(!c(Event,Date,UTCDate)) %>% 
                             mutate(Enlace = paste0("<a href='", Site, "' target= '_blank'>", "Ver Partida", "</a>")), 
                           escape = FALSE,
                           options = list(scrollX = TRUE))
                           # extensions = 'Buttons', 
                           # options = list(dom = 'Bfrtip',
                           #                 buttons =list('copy', 'print', list(extend = 'collection',
                           #                                                     buttons = c('csv', 'excel', 'pdf'),
                           #                                                    text = 'Download'))))

  }
