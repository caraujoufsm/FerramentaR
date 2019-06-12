# Funcao para o servidor de mapas
# Recebe como provedor o mapa de estradas
# Seta como padrao a longitude e latitude do centro geografico do RS, juntamente com um zoom apropriado

shinyServer(function (input, output, sesion) {
  output$map <- renderLeaflet({
     map <- leaflet(data = TotalAcidentesMaio) %>%
      addProviderTiles("Esri.WorldStreetMap")%>%
      setView(lng = -50.35, lat = -30.15, zoom = 7)
    
    for (c in gruposMaioAmarelo){
      filhoMaio = TotalAcidentesMaio[TotalAcidentesMaio$causa_acidente==c,]
      map = map %>%
        addCircleMarkers(
          data = filhoMaio,
          lng = ~ longitude, 
          lat =  ~ latitude,
          radius = 4,
          popup=paste(
            "<b>Data:</b>", filhoMaio$data_inversa, "<br>",
            "<b>Cidade:</b>", filhoMaio$municipio, "<br>",
            "<b>Sexo:</b>", filhoMaio$sexo, "<br>",
            "<b>Idade:</b>", filhoMaio$idade, "<br>",
            "<b>Ocorrido:</b>", filhoMaio$tipo_acidente, "<br>",
            "<b>Dia da Semana:</b>", filhoMaio$dia_semana, "<br>",
            "<b>Horário:</b>", filhoMaio$horario, "<br>",
            "<b>Situação Climática:</b>", filhoMaio$condicao_metereologica, "<br>",
            "<b>Tipo do Veículo:</b>", filhoMaio$tipo_veiculo, "<br>",
            "<b>Causa do Acidente:</b>", filhoMaio$causa_acidente, "<br>",
            "<b>Total de Óbitos:</b>", filhoMaio$total_obitos, "<br>"
          ),
          fillOpacity = 1,
          color = ~coresMaioAmarelo(causa_acidente),
          group = c
        )
    }
    map = map %>% addLayersControl(overlayGroups = gruposMaioAmarelo, position = "topright")
    map = map %>% hideGroup(gruposMaioAmarelo)
  })
  output$mapas <- renderLeaflet({
    mapas <- leaflet(data = TotalAcidentesLeiSeca) %>%
      addProviderTiles("Esri.WorldStreetMap")%>%
      setView(lng = -53.35, lat = -30.15, zoom = 7)
    
    for (g in gruposLeiSeca){
      filhoLS = TotalAcidentesLeiSeca[TotalAcidentesLeiSeca$total_obitos==g,]
      mapas = mapas %>%
        addCircleMarkers(
          data = filhoLS,
          lng = ~ longitude, 
          lat =  ~ latitude,
          radius = 4,
          popup=paste(
            "<b>Data:</b>", filhoLS$data_inversa, "<br>",
            "<b>Cidade:</b>", filhoLS$municipio, "<br>",
            "<b>Sexo:</b>", filhoLS$sexo, "<br>",
            "<b>Idade:</b>", filhoLS$idade, "<br>",
            "<b>Ocorrido:</b>", filhoLS$tipo_acidente, "<br>",
            "<b>Dia da Semana:</b>", filhoLS$dia_semana, "<br>",
            "<b>Horário:</b>", filhoLS$horario, "<br>",
            "<b>Situação Climática:</b>", filhoLS$condicao_metereologica, "<br>",
            "<b>Tipo do Veículo:</b>", filhoLS$tipo_veiculo, "<br>",
            "<b>Causa do Acidente:</b>", filhoLS$causa_acidente, "<br>",
            "<b>", filhoLS$total_obitos, "</b><br>"
          ),
          fillOpacity = 0,
          color = ~coresLeiSeca(total_obitos),
          group = g
          )
    }
    for (g1 in gruposLeiSeca1){
      filhoLS1 = TotalAcidentesLeiSeca[TotalAcidentesLeiSeca$sexo==g1,]
      mapas = mapas %>%
        addCircleMarkers(
          data = filhoLS1,
          lng = ~ longitude, 
          lat =  ~ latitude,
          radius = 4,
          popup=paste(
            "<b>Data:</b>", filhoLS1$data_inversa, "<br>",
            "<b>Cidade:</b>", filhoLS1$municipio, "<br>",
            "<b>Sexo:</b>", filhoLS1$sexo, "<br>",
            "<b>Idade:</b>", filhoLS1$idade, "<br>",
            "<b>Ocorrido:</b>", filhoLS1$tipo_acidente, "<br>",
            "<b>Dia da Semana:</b>", filhoLS1$dia_semana, "<br>",
            "<b>Horário:</b>", filhoLS1$horario, "<br>",
            "<b>Situação Climática:</b>", filhoLS1$condicao_metereologica, "<br>",
            "<b>Tipo do Veículo:</b>", filhoLS1$tipo_veiculo, "<br>",
            "<b>Causa do Acidente:</b>", filhoLS1$causa_acidente, "<br>",
            "<b>", filhoLS1$total_obitos, "</b><br>"
          ),
          fillOpacity = 1,
          color = ~coresLeiSecaSexo(sexo),
          group = g1
        )
    }
    mapas = mapas %>% addLayersControl(overlayGroups = c(gruposLeiSeca,gruposLeiSeca1))
    mapas = mapas %>% hideGroup(gruposLeiSeca)
    mapas = mapas %>% hideGroup(gruposLeiSeca1)
    mapas = mapas %>% addLegend("bottomright", pal = coresLeiSeca, values = TotalAcidentesLeiSeca$total_obitos, 
                                title = "Acidentes Por Total de Óbitos", opacity = 0.3)
    mapas = mapas %>% addLegend("bottomright", pal = coresLeiSecaSexo, values = TotalAcidentesLeiSeca$sexo, 
                                title = "Acidentes Por Sexo", opacity = 0.3)
    })
  
  output$mapaVerao <- renderLeaflet({
    mapaVerao <- leaflet(TotalAcidentesVeraneioSemMortes) %>%
      addProviderTiles(group = "Estradas2", "Esri.WorldStreetMap")%>%
      setView(lng = -50.35, lat = -30.15, zoom = 7)
      
      for (c in gruposVerao){
        filhoVerao = TotalAcidentesVeraneio[TotalAcidentesVeraneio$causa_acidente==c,]
        mapaVerao = mapaVerao %>%
          addCircleMarkers(
            data = filhoVerao,
            lng = ~ longitude, 
            lat =  ~ latitude,
            radius = 4,
            popup=paste(
              "<b>Data:</b>", filhoVerao$data_inversa, "<br>",
              "<b>Cidade:</b>", filhoVerao$municipio, "<br>",
              "<b>Sexo:</b>", filhoVerao$sexo, "<br>",
              "<b>Idade:</b>", filhoVerao$idade, "<br>",
              "<b>Ocorrido:</b>", filhoVerao$tipo_acidente, "<br>",
              "<b>Dia da Semana:</b>", filhoVerao$dia_semana, "<br>",
              "<b>Horário:</b>", filhoVerao$horario, "<br>",
              "<b>Situação Climática:</b>", filhoVerao$condicao_metereologica, "<br>",
              "<b>Tipo do Veículo:</b>", filhoVerao$tipo_veiculo, "<br>",
              "<b>Causa do Acidente:</b>", filhoVerao$causa_acidente, "<br>",
              "<b>Total de Óbitos:</b>", filhoVerao$total_obitos, "<br>"
            ),
            fillOpacity = 1,
            color = ~coresVerao(causa_acidente),
            group = c
          )
      }
    mapaVerao = mapaVerao %>% addLayersControl(overlayGroups = gruposVerao, position = "topright")
    mapaVerao = mapaVerao %>% hideGroup(gruposVerao)
  })
  output$PizzaCausaMaio <- renderHighchart({
    PizzaCausaMaio  
    })
  output$BarraBRMaio <- renderHighchart({
    BarraBRMaio
  })
  output$BarraDiaMaio <- renderHighchart({
    BarraDiaMaio  
  })
  output$ArvoreMaio1 <- renderPlot({
    rpart.plot(ArvoreTesteMaioAmarelo, main = "Probabilidade de sair ileso em acidentes com base na idade, dia de semana e tipo de acidente", type = 2, clip.right.labs = FALSE, branch = .3, under = TRUE)
  })
  output$PizzaSexoLeiSeca <- renderHighchart({
    PizzaSexoLeiSeca  
  })
  output$BarraAcidentesAnoLeiSeca <- renderUI({
    hw_grid(lst, rowheight = 400, ncol = 2)
  })
  output$ArvoreLeiSeca1 <- renderPlot({
    rpart.plot(ArvoreTesteLeiSeca, main = "Probabilidade de sair ileso em acidentes com base em idade, dia de semana e mês", type = 2, clip.right.labs = FALSE, branch = .3, under = TRUE)
  })
  output$PizzaCausaVeraneio <- renderHighchart({
    PizzaCausaVeraneio  
  })
  output$BarraDiaLeiSeca <- renderHighchart({
    BarraDiaLeiSeca
  })
  output$BarraBRVeraneio <- renderHighchart({
    BarraBRVeraneio
  })
  output$BarraDiaVeraneio <- renderHighchart({
    BarraDiaVeraneio  
  })
  output$BarraMesTotal <- renderHighchart({
    BarraMesTotal
  })
  output$PizzaCausaTotal <- renderHighchart({
    PizzaCausaTotal
  })
  output$PizzaCausas <- renderUI({
    hw_grid(ListaCausas, rowheight = 400, ncol=2)
  })
  output$ArvoreVeraneio <- renderPlot({
    rpart.plot(ArvoreTesteVeraneio, main = "Probabilidade de sair ileso em acidentes com base em idade, dia de semana e tipo de acidente", type = 2,  clip.right.labs = FALSE, branch = .3, under = TRUE)
  })
  output$BarraDiaTotal <- renderHighchart({
    BarraDiaTotal
  })
  output$ListaDias <- renderUI({
    hw_grid(ListaDias, rowheight = 400, ncol = 3)
  })
  output$Comparativo <- renderPlot({
    rpart.plot(ArvoreTesteVeraoSabado, main = "Probabilidade de sair ileso em acidentes no sábado, durante época de veraneio e com base na idade do condutor", type = 2, clip.right.labs = FALSE, branch = .3, under = TRUE)
    
  })
})