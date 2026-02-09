library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)
library(shinythemes)

# --- CARREGAMENTO E PREPARAÇÃO DOS DADOS ---
dados_sf <- readRDS("shiny_data.rds") %>% st_transform(4326)

medias_estado <- dados_sf %>% 
  st_drop_geometry() %>% 
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))

# 1. TEXTOS PARA O PANORAMA ESTADUAL (Resumo por Dimensão)
textos_panorama <- list(
  "sensib" = "A Sensibilidade em Pernambuco reflete o grau em que os sistemas biofísicos são afetados por perturbações climáticas, considerando o uso do solo, a infraestrutura e a capacidade adaptativa. No estado, observamos uma heterogeneidade marcante: enquanto a Zona da Mata apresenta alta sensibilidade devido à fragmentação da Mata Atlântica e ao uso intensivo pela cana-de-açúcar, o Semiárido lida com a fragilidade intrínseca da Caatinga e solos rasos. A média estadual é pressionada por lacunas em infraestrutura rural e assistência técnica, fatores que, se não endereçados, potencializam os danos de secas e cheias.",
  "expos" = "A Exposição Ambiental quantifica os ativos naturais (remanescentes vegetais e áreas protegidas) que estão diretamente na linha de frente das mudanças climáticas. Em Pernambuco, a exposição é crítica nas franjas de desertificação do Sertão e nas áreas costeiras da RMR. O panorama revela que a perda de cobertura nativa reduz a proteção natural contra o aumento da temperatura. A conservação de unidades de conservação e matas ciliares surge como a principal estratégia para diminuir este índice, que hoje mostra patamares preocupantes em regiões de forte expansão agrícola e urbana.",
  "ameaca_atual" = "A Ameaça Climática Atual sintetiza as anomalias de precipitação e temperatura que o estado já vivencia. Pernambuco enfrenta um aumento sistemático na amplitude térmica e uma maior irregularidade nos ciclos de chuva. O Sertão Central e do Araripe registram as maiores anomalias térmicas, enquanto as RDs do Agreste e Litoral sofrem com a sazonalidade extrema. Este cenário impõe um estresse hídrico permanente sobre o bioma Caatinga e sobre o sistema de abastecimento humano, configurando um estado de alerta para a segurança hídrica e alimentar.",
  "risco_atual" = "O Risco Climático Atual é a convergência da Ameaça, Sensibilidade e Exposição. O mapa estadual revela que o risco não é uniforme; ele é exacerbado onde a fragilidade social encontra eventos climáticos extremos. RDs como o Sertão do São Francisco e a Região Metropolitana apresentam núcleos de alto risco por razões distintas (clima severo vs. densidade urbana exposta). O panorama estadual indica que a redução do risco depende menos de controlar o clima e mais de aumentar a capacidade adaptativa e restaurar serviços ecossistêmicos nas áreas mais sensíveis identificadas."
)

# 2. TEXTOS DIAGNÓSTICOS REGIONAIS (Acrônimas)
textos_rd <- list(
  "MET" = "A Região Metropolitana de Recife (MET) apresenta um diagnóstico de vulnerabilidade intrinsecamente ligado à sua alta densidade urbana e ocupação sobre o bioma de Mata Atlântica e ecossistemas costeiros. Quantitativamente, observa-se que a maioria dos municípios detém índices de Exposição Ambiental e Sensibilidade de Infraestrutura acima da média estadual de Pernambuco. Este cenário é reflexo de um processo histórico de impermeabilização do solo e supressão de manguezais, o que torna a região extremamente sensível a eventos climáticos extremos, como chuvas intensas e a elevação do nível do mar. Qualitativamente, o Risco Climático é impulsionado pela fragilidade da infraestrutura urbana em áreas de morro e várzea, onde a falta de saneamento e drenagem eficiente superam os desafios encontrados em outras RDs. O diagnóstico aponta que, sem um incremento substancial na Capacidade Adaptativa — atualmente pressionada pela demanda populacional —, o risco futuro projeta impactos severos na segurança habitacional, mantendo a RMR em um patamar de risco significativamente superior ao benchmark estadual.",
  
  "MNS" = "A RD Mata Norte (MNS) é caracterizada por uma paisagem dominada pela monocultura canavieira, o que impacta diretamente os indicadores de Sensibilidade de Uso do Solo. A análise quantitativa revela que o passivo ambiental nesta região supera a média de Pernambuco em diversos municípios, fruto de décadas de manejo intensivo que fragmentou o bioma de Mata Atlântica. Qualitativamente, a região apresenta uma vulnerabilidade hídrica latente; a degradação das matas ciliares nos rios que abastecem o norte do estado aumenta a exposição a secas sazonais e processos erosivos. O risco climático atual é agravado pela baixa diversidade de cobertura vegetal nativa, o que reduz a resiliência térmica. O diagnóstico sugere que a restauração florestal estratégica é a principal via para reduzir os scores de risco, que atualmente posicionam a RD em um patamar de atenção quando comparada às médias estaduais.",
  
  "MTS" = "A RD Mata Sul (MLS) possui um perfil de risco climático fortemente marcado pela sua topografia acidentada e pelo histórico de desastres naturais hidrológicos. Quantitativamente, a Exposição e o Risco Atual nesta RD frequentemente excedem a média estadual de Pernambuco. Qualitativamente, o bioma de Mata Atlântica, embora mais preservado em fragmentos do que no norte, enfrenta pressão pela ocupação de encostas e pelo uso do solo em vales estreitos. A ameaça climática manifesta-se em regimes de chuva torrenciais que, combinados com a perda de solo e assoreamento de rios, elevam a vulnerabilidade das populações locais. A capacidade adaptativa regional ainda carece de sistemas de alerta precoce e infraestrutura de contenção mais robustos. Para mitigar o risco futuro, o diagnóstico aponta a urgência em recuperar APPs e fortalecer a resiliência das cidades que margeiam os principais eixos hídricos da região.",
  
  "AGS" = "O Agreste Setentrional (AGS), zona de transição entre o litoral e o semiárido, sofre com a pressão do Polo Têxtil sobre os recursos naturais limitados do bioma Caatinga e Mata Atlântica. A análise quantitativa mostra que a Sensibilidade de Condição do Solo e de Infraestrutura nesta RD supera a média de Pernambuco devido ao descarte de efluentes e ao uso intensivo da terra. Qualitativamente, a fragmentação florestal nesta faixa de transição eleva a Exposição Ambiental, tornando a região sensível a anomalias de temperatura. O risco climático é potencializado pela escassez hídrica, que já desafia a produção local. O diagnóstico indica que a região opera com um score de ameaça futuro elevado, exigindo uma transição para modelos produtivos que priorizem o reuso de água e a conservação dos solos para conter o avanço do risco acima do patamar estadual.",
  
  "AGC" = "O Agreste Central (AGC) apresenta indicadores de Sensibilidade de Uso do Solo que desafiam a média estadual. Quantitativamente, cerca de 60% dos municípios da RD registram Ameaça Climática Atual acima do benchmark de Pernambuco. Qualitativamente, a degradação da Caatinga e dos brejos de altitude — que funcionam como ilhas de umidade — aumentou a vulnerabilidade térmica regional. O risco climático é alimentado por uma urbanização acelerada e por práticas agropecuárias que não raro ignoram a capacidade de suporte do solo. O diagnóstico revela que o equilíbrio regional depende da recuperação da cobertura vegetal para manter o microclima. Sem um incremento na Capacidade Adaptativa, os cenários de aquecimento futuro projetam um impacto severo na viabilidade econômica e no bem-estar das populações desta RD, que já opera acima da média estadual de vulnerabilidade.",
  
  "AGM" = "A RD Agreste Meridional (AGM) possui uma economia fortemente atrelada à pecuária leiteira, o que se reflete em indicadores de Condição do Solo que superam a média de Pernambuco. Quantitativamente, a Exposição Ambiental é alta em áreas de pastagem degradada onde a Caatinga foi suprimida. Qualitativamente, a região enfrenta um processo de perda de resiliência hídrica, essencial para a manutenção da atividade produtiva. O risco climático futuro aponta para cenários de maior aridez, o que pode inviabilizar o modelo atual se não houver investimento em tecnologias de convivência com o semiárido. O diagnóstico aponta que o score de vulnerabilidade da RD é impulsionado pela baixa proteção territorial e pela falta de práticas sustentáveis de manejo de solo, colocando a região em um patamar de risco superior à média do estado.",
  
  "MOX" = "O Sertão do Moxotó (MOX) está inserido em uma das áreas mais secas do estado, com uma Caatinga sob forte estresse climático. Quantitativamente, a maioria dos municípios registra Ameaça Climática e Sensibilidade Social significativamente acima da média de Pernambuco. Qualitativamente, a região sofre com um déficit de infraestrutura básica e baixa capacidade adaptativa, o que torna qualquer variação climática um fator de alto risco para a segurança hídrica. O diagnóstico revela que a vegetação nativa é o principal escudo contra a desertificação, porém sua degradação constante tem elevado o Risco Atual para níveis alarmantes. A estratégia regional deve focar na preservação biológica e no suporte técnico ao produtor rural para tentar reverter os indicadores que hoje posicionam o Moxotó como uma das RDs mais vulneráveis do estado.",
  
  "PAJ" = "O Sertão do Pajeú (PAJ) enfrenta sérios riscos de desertificação. Quantitativamente, os indicadores de Sensibilidade de Condição do Solo nesta RD superam a média estadual. Qualitativamente, o Rio Pajeú e suas matas ciliares são as artérias de resiliência da região, contudo, a degradação dessas áreas elevou a Exposição Ambiental. O risco climático manifesta-se na irregularidade extrema das chuvas, que impacta a agricultura de subsistência e o abastecimento. O diagnóstico aponta que o Pajeú possui um potencial de recuperação através do manejo de bacias hidrográficas, mas atualmente o score de risco futuro permanece elevado, exigindo políticas públicas que fortaleçam a conservação da Caatinga para mitigar o avanço da aridez acima dos níveis médios de Pernambuco.",
  
  "ARP" = "A RD Sertão do Araripe (ARP) vive o conflito entre a mineração do gesso e a conservação da Chapada. Quantitativamente, diversos municípios superam a média estadual em Sensibilidade de Uso do Solo e Exposição Ambiental. Qualitativamente, a Chapada atua como um refúgio de biodiversidade; sua degradação para fins energéticos impacta diretamente o risco climático regional. O diagnóstico indica que o desmatamento da Caatinga potencializa a amplitude térmica e reduz a recarga de aquíferos. O risco futuro é crítico, pois a região já opera nos limites térmicos. A preservação da cobertura vegetal e a modernização da matriz energética industrial são fundamentais para reduzir a vulnerabilidade que hoje coloca o Araripe em destaque negativo em relação à média de Pernambuco.",
  
  "SCP" = "O Sertão Central (SCP) detém alguns dos maiores índices de Ameaça Climática de Pernambuco. Quantitativamente, quase a totalidade de seus municípios apresenta scores de temperatura acima da média estadual. Qualitativamente, a Caatinga nesta RD está sob pressão extrema, e a falta de vegetação nativa potencializa o aquecimento do solo. O diagnóstico revela que o risco climático futuro é severo, com projeções que indicam um aumento na frequência de secas extremas. A baixa infraestrutura e a vulnerabilidade social elevam o Risco Atual. A mitigação exige um esforço coordenado de conservação para evitar que a região ultrapasse o ponto de não-retorno em termos de degradação ambiental e viabilidade socioeconômica.",
  
  "ITA" = "A RD Sertão de Itaparica (ITA) tem sua dinâmica de risco atrelada ao Rio São Francisco. Quantitativamente, a região apresenta Sensibilidade de Infraestrutura acima da média estadual. Qualitativamente, a exposição ambiental é crítica nas margens dos reservatórios, onde o microclima foi alterado. O risco climático manifesta-se na vulnerabilidade à redução da vazão do rio e no aumento da temperatura local. O diagnóstico aponta que, embora a presença do rio ofereça segurança, a degradação da Caatinga nas áreas de sequeiro vizinhas eleva o score de risco regional. A proteção das áreas de entorno e a diversificação da economia rural são chaves para mitigar o risco climático que ameaça a estabilidade desta RD.",
  
  "SFR" = "A RD Sertão do São Francisco (SFR) representa um cenário complexo. Quantitativamente, mais de 55% dos municípios apresentam Risco Climático superior à média estadual. Qualitativamente, a agricultura irrigada convive com a fragilidade extrema da Caatinga de sequeiro. O aumento das temperaturas médias e a intensificação da amplitude térmica colocam a região em estado de estresse hídrico permanente. A agricultura eleva a Sensibilidade de Uso do Solo devido ao uso intensivo de insumos, superando os patamares médios estaduais. O diagnóstico aponta para a necessidade urgente de políticas de reflorestamento e manejo sustentável, garantindo que o desenvolvimento econômico não exacerbe a vulnerabilidade climática latente."
)

# 3. DICIONÁRIOS E HIERARQUIA (Mantidos conforme base original)
nomes_amigaveis <- c(
  "sensib" = "Sensibilidade", "expos" = "Exposição", 
  "ameaca_atual" = "Ameaça Climática Atual", "risco_atual" = "Risco Climático Atual",
  "ameaca_SWL1.5" = "Ameaça Futura (1.5°C)", "ameaca_SWL2.0" = "Ameaça Futura (2.0°C)",
  "risco_SWL1.5" = "Risco Futuro (1.5°C)", "risco_SWL2.0" = "Risco Futuro (2.0°C)",
  "s_luc_2017" = "Mudança de Uso do Solo", "s_csol_2017" = "Cobertura do Solo",
  "s_infra_2017" = "Infraestrutura e Serviços", "s_cap" = "Capacidade Adaptativa",
  "e_cobexpo" = "Áreas Expostas", "e_priocons" = "Prioridade de Conservação",
  "sl_agrotox" = "Uso de Agrotóxicos", "sl_fogo" = "Ocorrência de Fogo", 
  "sl_agr_fam" = "Ausência de Agricultura Familiar", "sl_prat_sust" = "Ausência de Práticas Sustentáveis",
  "sc_desmat" = "Perda de Vegetação", "sc_passivo" = "Passivo Ambiental",
  "sc_pastag" = "Pastagem Degradada", "sc_miner" = "Atividade Mineradora",
  "si_rodov" = "Densidade Rodoviária", "si_hidro" = "Proximidade à Planta Higroelétrica",
  "sc_orient" = "Orientação Técnica para Criação de UCs", "sc_ucons" = "Unidades de Conservação",
  "am_prec_acum" = "Anomalia de Precipitação", "am_temp_med" = "Aumento de Temperatura",
  "e_cobnat" = "Remanescentes Vegetais", "e_areaprot" = "Proteção Territorial"
)

hierarquia <- list(
  "Sensibilidade" = c("Mudança Uso Solo" = "s_luc_2017", "Condição do Solo" = "s_csol_2017", "Infraestrutura" = "s_infra_2017", "Capac. Adaptativa" = "s_cap"),
  "Exposição"     = c("Cobertura Exposta" = "e_cobexpo", "Prioridade Cons." = "e_priocons"),
  "Ameaça"        = c("Ameaça Atual" = "ameaca_atual", "Ameaça Futura (1.5°C)" = "ameaca_SWL1.5", "Ameaça Futura (2.0°C)" = "ameaca_SWL2.0"),
  "Risco"         = c("Risco Atual" = "risco_atual", "Risco Futuro (1.5°C)" = "risco_SWL1.5", "Risco Futuro (2.0°C)" = "risco_SWL2.0")
)

variaveis_brutas_map <- list(
  "s_luc_2017"   = c("sl_agrotox", "sl_agr_fam", "sl_prat_sust", "sl_fogo"),
  "s_csol_2017"  = c("sc_passivo", "sc_desmat", "sc_pastag", "sc_miner"),
  "s_infra_2017" = c("si_rodov", "si_hidro"),
  "s_cap"        = c("sc_orient", "sc_ucons"),
  "e_cobexpo"    = c("e_cobexpo", "e_cobnat"),
  "e_priocons"   = c("e_areaprot", "e_priocons"),
  "ameaca_atual" = c("am_prec_acum", "am_temp_med"),
  "risco_atual"  = c("ameaca_atual", "sensib", "expos"),
  "risco_SWL1.5" = c("ameaca_SWL1.5", "sensib", "expos"),
  "risco_SWL2.0" = c("ameaca_SWL2.0", "sensib", "expos")
)

# --- UI ---
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Dashboard PEAR: Vulnerabilidade e Riscos Climáticos (PE)"),
  sidebarLayout(
    sidebarPanel(
      # O menu lateral agora tem uma condicional para a aba de Panorama
      conditionalPanel(
        condition = "input.abas_painel != 'Panorama Estadual'",
        selectInput("selecao_rd", "Região de Desenvolvimento (RD):", choices = sort(unique(dados_sf$rds))),
        selectInput("dimensao", "Dimensão de Análise:", choices = names(hierarquia)),
        uiOutput("menu_subindice"),
        hr()
      ),
      conditionalPanel(
        condition = "input.abas_painel == 'Panorama Estadual'",
        selectInput("dim_panorama", "Selecione a Dimensão Geral:", 
                    choices = c("Sensibilidade" = "sensib", "Exposição" = "expos", 
                                "Ameaça Atual" = "ameaca_atual", "Risco Atual" = "risco_atual")),
        helpText("Este mapa apresenta a situação de todo o estado para a dimensão selecionada.")
      ),
      helpText("Linha/Ponto vermelha indica a média de Pernambuco.")
    ),
    mainPanel(
      tabsetPanel(
        id = "abas_painel",
        tabPanel("Panorama Estadual", br(),
                 wellPanel(style = "background: white;", uiOutput("texto_panorama")),
                 leafletOutput("mapa_panorama", height = "500px")),
        
        tabPanel("Diagnóstico Regional", br(), 
                 wellPanel(style = "background: white; border-left: 8px solid #2c3e50; padding: 25px;", 
                           uiOutput("texto_diagnostico"))),
        
        tabPanel("Mapas e dados detalhados", br(), 
                 leafletOutput("mapa_dinamico", height = "400px"), 
                 br(), 
                 plotOutput("plot_detalhado", height = "450px")),
        
        tabPanel("Perfil do Município", br(), 
                 uiOutput("menu_municipios"), 
                 plotOutput("plot_perfil_muni", height = "550px"))
      )
    )
  )
)

# --- SERVER ---
server <- function(input, output, session) {
  
  # --- LÓGICA PANORAMA ESTADUAL ---
  output$mapa_panorama <- renderLeaflet({
    req(input$dim_panorama)
    col <- input$dim_panorama
    pal <- colorNumeric("YlOrRd", domain = dados_sf[[col]], na.color = "transparent")
    
    leaflet(dados_sf) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(fillColor = ~pal(get(col)), weight = 1, color = "white", fillOpacity = 0.7,
                  label = ~paste(nome, ":", round(get(col), 2))) %>%
      addLegend(pal = pal, values = dados_sf[[col]], title = "Score", position = "bottomright")
  })
  
  output$texto_panorama <- renderUI({
    req(input$dim_panorama)
    tagList(
      h3(paste("Panorama de", nomes_amigaveis[input$dim_panorama]), style = "font-weight: bold;"),
      p(textos_panorama[[input$dim_panorama]], style = "font-size: 15px; text-align: justify;")
    )
  })
  
  # --- LÓGICA ORIGINAL (RD) ---
  dados_filtrados <- reactive({ dados_sf %>% filter(rds == input$selecao_rd) })
  
  output$texto_diagnostico <- renderUI({
    req(input$selecao_rd)
    resumo <- textos_rd[[input$selecao_rd]]
    tagList(
      h3(paste("Diagnóstico Regional:", input$selecao_rd), style = "font-weight: bold; color: #2c3e50;"),
      p(resumo, style = "font-size: 15px; text-align: justify; line-height: 1.7;")
    )
  })
  
  output$menu_subindice <- renderUI({
    req(input$dimensao)
    selectInput("subindice", "Indicador Específico:", choices = hierarquia[[input$dimensao]])
  })
  
  output$mapa_dinamico <- renderLeaflet({
    req(input$subindice)
    col_mapa <- input$subindice
    pal_cores <- if(grepl("ameaca|risco", col_mapa)) "Reds" else "YlOrBr"
    pal <- colorNumeric(pal_cores, domain = dados_sf[[col_mapa]], na.color = "transparent")
    
    leaflet(dados_filtrados()) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(fillColor = ~pal(get(col_mapa)), weight = 1.5, color = "white", fillOpacity = 0.8,
                  label = ~paste(nome, ":", round(get(col_mapa), 2))) %>%
      addLegend(pal = pal, values = dados_sf[[col_mapa]], title = "Score", position = "bottomright")
  })
  
  output$plot_detalhado <- renderPlot({
    req(input$subindice)
    cols_alvo <- variaveis_brutas_map[[input$subindice]]
    if(is.null(cols_alvo)) cols_alvo <- input$subindice
    cols_existentes <- intersect(cols_alvo, names(dados_filtrados()))
    
    df_plot <- dados_filtrados() %>%
      st_drop_geometry() %>%
      select(nome, all_of(cols_existentes)) %>%
      pivot_longer(cols = -nome, names_to = "Var", values_to = "Val") %>%
      mutate(Var_Nome = ifelse(Var %in% names(nomes_amigaveis), nomes_amigaveis[Var], Var),
             media_pe = as.numeric(medias_estado[Var]))
    
    ggplot(df_plot, aes(x = reorder(nome, Val), y = Val)) +
      geom_col(aes(fill = Var_Nome)) + 
      geom_hline(aes(yintercept = media_pe), color = "red", linetype = "dashed", size = 1) +
      facet_wrap(~Var_Nome, scales = "free_x") + 
      coord_flip() +
      labs(title = "Comparação Regional vs. Média Estadual", x = "Municípios", y = "Score") +
      theme_minimal() + 
      theme(legend.position = "none", axis.title = element_text(size = 14, face = "bold"),
            strip.text = element_text(size = 13, face = "bold"))
  })
  
  output$menu_municipios <- renderUI({
    municipios <- sort(unique(dados_filtrados()$nome))
    selectInput("selecao_muni", "Municípios para Comparação:", choices = municipios, multiple = TRUE, selected = municipios[1])
  })
  
  output$plot_perfil_muni <- renderPlot({
    req(input$selecao_muni)
    colunas_perfil <- c("sensib", "expos", "ameaca_atual", "risco_atual")
    
    df_perfil <- dados_filtrados() %>%
      st_drop_geometry() %>%
      filter(nome %in% input$selecao_muni) %>%
      select(nome, any_of(colunas_perfil)) %>%
      pivot_longer(cols = -nome, names_to = "Dim", values_to = "Val") %>%
      mutate(Dim_Nome = ifelse(Dim %in% names(nomes_amigaveis), nomes_amigaveis[Dim], Dim),
             media_pe = as.numeric(medias_estado[Dim]))
    
    ggplot(df_perfil, aes(x = Dim_Nome, y = Val)) +
      geom_col(aes(fill = Dim_Nome), alpha = 0.6, width = 0.7) + 
      geom_point(aes(y = media_pe, color = "Média PE"), size = 5) +
      scale_color_manual(name = "Referência", values = c("Média PE" = "red")) +
      facet_wrap(~nome) + 
      ylim(0, 1) +
      scale_fill_brewer(palette = "Greys", guide = "none") +
      labs(title = "Perfil Individual vs. Patamar de Pernambuco", x = "", y = "Score") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
            strip.text = element_text(size = 14, face = "bold"),
            legend.position = "bottom")
  })
}

shinyApp(ui, server)