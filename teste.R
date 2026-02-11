library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)
library(shinythemes)

# --- CARREGAMENTO E PREPARAÇÃO DOS DADOS ---
dados_sf <- readRDS("shiny_data.rds") %>% st_transform(4326)


# CORREÇÃO CRÍTICA: Forçar colunas de Ameaça e Risco a serem numéricas
colunas_analise <- c("sensib", "expos", "ameaca_atual", "risco_atual",
                     "am_prec_acum", "am_temp_med", "am..sazon..prep", "am.ampl_temp")

medias_estado <- dados_sf %>%
  st_drop_geometry() %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))

### PADRÃO DE CORES E CATEGORIZAÇÃO ###
cores_fixas <- c(
  "Muito Baixo" = "#02c650", 
  "Baixo"       = "#a9de00", 
  "Médio"       = "#ffcd00", 
  "Alto"        = "#ff8300", 
  "Muito Alto"  = "#f40000"
)

# Função auxiliar para categorizar os dados dinamicamente
categorizar_valor <- function(valor) {
  case_when(
    valor < 0.2 ~ "Muito Baixo",
    valor >= 0.2 & valor < 0.4 ~ "Baixo",
    valor >= 0.4 & valor < 0.6 ~ "Médio",
    valor >= 0.6 & valor < 0.8 ~ "Alto",
    valor >= 0.8 ~ "Muito Alto",
    TRUE ~ "Sem dados"
  )
}

# 1. TEXTOS PARA O PANORAMA ESTADUAL (Definições Adapta Brasil)
textos_panorama <- list(
  "sensib" = "
  <ul>
    <li><b>Fatores que aumentam a sensibilidade:</b> A sensibilidade biômica em Pernambuco é elevada pela pressão histórica de uso do solo, que fragiliza as características ecossistêmicas. Práticas agrícolas inadequadas, uso de agrotóxicos e queimadas degradam a microbiota e reduzem a autorregulação da vegetação. No Agreste e Sertão, pastagens degradadas aumentam a suscetibilidade da Caatinga.</li>
    <li><b>Medidas de contenção da sensibilidade:</b> Exige a recuperação da saúde do solo e o fortalecimento da governança. A promoção da agricultura sustentável e o suporte técnico para manejo sem uso do fogo são fundamentais, assim como a criação de novas UCs para elevar a Capacidade Adaptativa.</li>
  </ul>",
  
  "expos" = "
  <ul>
    <li><b>Fatores que aumentam a exposição:</b> Refere-se à presença e proximidade de elementos do bioma a impactos climáticos. A fragmentação dos ecossistemas e a ocupação antrópica em bordas de matas nativas criam zonas vulneráveis. A presença de infraestruturas em áreas de alta integridade biológica facilita o contato direto com ameaças externas.</li>
    <li><b>Medidas de contenção da exposição:</b> A estratégia central deve ser o fortalecimento da conectividade ecológica. A criação de Corredores Ecológicos e a restauração de matas ciliares funcionam como escudos físicos, protegendo a integridade do bioma contra a exposição direta.</li>
  </ul>",
  
  "ameaca_atual" = "
  <ul>
    <li><b>Fatores que aumentam a ameaça:</b> Fatores climáticos externos como anomalias de temperatura e precipitação que alteram a resiliência climática. No AdaptaBrasil, quanto menor a resiliência do nicho biômico projetada, maior o índice de ameaça. O aquecimento global (RCP 8.5) força a vegetação a operar fora de seus limites fisiológicos.</li>
    <li><b>Medidas de contenção da ameaça:</b> Foca na preservação de refúgios climáticos e brejos de altitude para amortecer anomalias térmicas. O monitoramento de projeções futuras é essencial para antecipar a perda de resiliência e planejar a gestão territorial climática.</li>
  </ul>",
  
  "risco_atual" = "
  <ul>
    <li><b>Fatores que aumentam o risco:</b> Resultado da convergência entre ameaça, sensibilidade e exposição. O risco aumenta onde a fragilidade social e ambiental encontra eventos climáticos extremos. Áreas com baixa capacidade adaptativa e alta dependência de recursos naturais explorados exaustivamente são as mais críticas.</li>
    <li><b>Medidas de contenção do risco:</b> Implementação de Planos de Adaptação que priorizem a restauração de ecossistemas. A contenção exige reduzir a sensibilidade e a exposição simultaneamente, focando em investimentos nos municípios identificados como hotspots de risco.</li>
  </ul>"
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

# 3. DICIONÁRIOS E HIERARQUIA
nomes_amigaveis <- c(
  "sensib" = "Sensibilidade", "expos" = "Exposição", 
  "ameaca_atual" = "Ameaça Climática Atual", "risco_atual" = "Risco Climático Atual",
  "ameaca_SWL1.5" = "Ameaça Futura (1.5°C)", "ameaca_SWL2.0" = "Ameaça Futura (2.0°C)",
  "risco_SWL1.5" = "Risco Futuro (1.5°C)", "risco_SWL2.0" = "Risco Futuro (2.0°C)",
  "s_luc_2017" = "Mudança de Uso do Solo", "s_csol_2017" = "MNudança da Cobertura do Solo",
  "s_infra_2017" = "Infraestrutura e Serviços", 
  "s_dens_pop_faz" = "Densidade populacional e de estabelecimentos",
  "sd_popul" = "Densidade populacional", "sd_fazend" = "Densidade de estabelecimentos agropecuários",
  "s_cap" = "Capacidade Adaptativa",
  "e_cobexpo" = "Áreas Expostas", "e_priocons" = "Áreas Prioritárias de Conservação",
  "e_cobnat" = "Remanescentes Vegetais", "e_areaprot" = "Proteção Territorial",
  "sl_agrotox" = "Uso de Agrotóxicos", "sl_fogo" = "Ocorrência de Fogo", 
  "sl_agr_fam" = "Ausência de Agricultura Familiar", "sl_prat_sust" = "Ausência de Práticas Sustentáveis",
  "sc_desmat" = "Perda de Vegetação", "sc_passivo" = "Passivo Ambiental",
  "sc_pastag" = "Pastagem Degradada", "sc_miner" = "Atividade Mineradora",
  "si_rodov" = "Densidade Rodoviária", "si_hidro" = "Proximidade à Planta Higroelétrica",
  "sc_orient" = "Propriedades com Orientação Técnica", "sc_ucons" = "UCs com comitê gestor e plano de manejo",
  "am_prec_acum" = "Anomalia de Precipitação", "am_temp_med" = "Aumento de Temperatura",
  "am_sazon_prep" = "Aumento de Sazonalidade da Precipitação", "am_ampl_temp"= "Aumento da Amplitude da Temperatura"
)

hierarquia <- list(
  "Sensibilidade" = c("Mudança Uso Solo" = "s_luc_2017", "Cobertura do Solo" = "s_csol_2017", "Infraestrutura" = "s_infra_2017", "Capac. Adaptativa" = "s_cap"),
  "Exposição"      = c("Cobertura Exposta" = "e_cobexpo", "Prioridade Cons." = "e_priocons", "Proteção Territorial" = "e_areaprot" ),
  "Ameaça"         = c("Ameaça Atual" = "ameaca_atual",  "Ameaça Futura (1.5°C)" = "ameaca_SWL1.5", "Ameaça Futura (2.0°C)" = "ameaca_SWL2.0" ),
  "Risco"          = c("Risco Atual" = "risco_atual", "Risco Futuro (1.5°C)" = "risco_SWL1.5", "Risco Futuro (2.0°C)" = "risco_SWL2.0")
)

variaveis_brutas_map <- list(
  "s_luc_2017"    = c("sl_agrotox", "sl_agr_fam", "sl_prat_sust", "sl_fogo"),
  "s_csol_2017"   = c("sc_passivo", "sc_desmat", "sc_pastag", "sc_miner"),
  "s_dens_pop_faz" = c("sd_popul","sd_fazend"),
  "s_infra_2017" = c("si_rodov", "si_hidro"),
  "s_cap"         = c("sc_orient", "sc_ucons"),
  "e_cobexpo"     = c("e_areaprot", "e_priocons", "e_cobnat"),
  "ameaca_atual" = c("am_prec_acum", "am_temp_med", "am_ampl_temp","am_sazon_prep"),
  "risco_atual"   = c("ameaca_atual", "sensib", "expos"),
  "risco_SWL1.5" = c("ameaca_SWL1.5", "sensib", "expos"),
  "risco_SWL2.0" = c("ameaca_SWL2.0", "sensib", "expos")
)

# --- UI ---
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Dashboard PEAR: Vulnerabilidade e Riscos Climáticos (PE)"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.abas_painel != 'Panorama Estadual'",
        selectInput("selecao_rd", "Região de Desenvolvimento (RD):", choices = sort(unique(dados_sf$rds))),
        selectInput("dimensao", "Dimensão de Análise:", choices = names(hierarquia)),
        uiOutput("menu_subindice")
      ),
      conditionalPanel(
        condition = "input.abas_painel == 'Panorama Estadual'",
        selectInput("dim_panorama", "Selecione a Dimensão Geral:", 
                    choices = c("Sensibilidade" = "sensib", "Exposição" = "expos", 
                                "Ameaça Atual" = "ameaca_atual", "Risco Atual" = "risco_atual"))
      ),
      hr(),
      h4("Documentação"),
      tags$a(href = "guia_pear_biodiv.pdf", 
             icon("file-pdf"), " Baixar PDF Metodologia", 
             target = "_blank", 
             class = "btn btn-danger btn-block"),
      br(),
      helpText("As cores seguem a escala Adapta Brasil: Verde (Baixo) a Vermelho (Alto)."),
      hr(),
      helpText("Mapa mostra panorama geral da RD em categorias. Gráficos abaixo mostram performance de cada município nos indicadores específicos que compõem a dimensão de análise")
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
                 plotOutput("plot_detalhado", height = "800px")),
        
        tabPanel("Perfil do Município", br(), 
                 uiOutput("menu_municipios"), 
                 plotOutput("plot_perfil_muni", height = "550px")),
        tabPanel("Metodologia", br(),
                 tags$iframe(style = "height: 800px; width: 100%; border: none;", 
                             src = "guia_pear_biodiv.pdf"))
      )
    )
  )
)


# --- SERVER ---
server <- function(input, output, session) {
  
  # 1. MAPA PANORAMA ESTADUAL (Mantido)
  output$mapa_panorama <- renderLeaflet({
    req(input$dim_panorama)
    col <- input$dim_panorama
    df_mapa <- dados_sf %>%
      mutate(categ = factor(categorizar_valor(get(col)), levels = names(cores_fixas)))
    pal <- colorFactor(palette = as.character(cores_fixas), domain = df_mapa$categ)
    leaflet(df_mapa) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(fillColor = ~pal(categ), weight = 1, color = "white", fillOpacity = 0.8,
                  label = ~paste(nome, "-", categ, ":", round(get(col), 2))) %>%
      addLegend(pal = pal, values = ~categ, title = "Categoria", position = "bottomright")
  })
  
  output$texto_panorama <- renderUI({
    req(input$dim_panorama)
    tagList(
      h3(paste("Panorama Estadual:", nomes_amigaveis[input$dim_panorama]), style = "font-weight: bold;"),
      HTML(textos_panorama[[input$dim_panorama]])
    )
  })
  
  # 2. MAPA REGIONAL DETALHADO (Mantido)
  dados_filtrados <- reactive({ dados_sf %>% filter(rds == input$selecao_rd) })
  
  output$mapa_dinamico <- renderLeaflet({
    req(input$subindice)
    col_mapa <- input$subindice
    df_regiao <- dados_filtrados() %>%
      mutate(categ = factor(categorizar_valor(get(col_mapa)), levels = names(cores_fixas)))
    pal <- colorFactor(palette = as.character(cores_fixas), domain = df_regiao$categ)
    leaflet(df_regiao) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(fillColor = ~pal(categ), weight = 1.5, color = "white", fillOpacity = 0.8,
                  label = ~paste(nome, ":", round(get(col_mapa), 2))) %>%
      addLegend(pal = pal, values = factor(names(cores_fixas), levels = names(cores_fixas)), 
                title = "Categoria", position = "bottomright")
  })
  
  # 3. TEXTO DIAGNÓSTICO (Mantido)
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
  
  # --- GRÁFICO DE BARRAS DETALHADO ---
  output$plot_detalhado <- renderPlot({
    req(input$subindice)
    
    cols_alvo <- variaveis_brutas_map[[input$subindice]]
    if(is.null(cols_alvo)) cols_alvo <- input$subindice
    
    cols_existentes <- intersect(cols_alvo, names(dados_filtrados()))
    if(length(cols_existentes) == 0) return(NULL)
    
    df_plot <- dados_filtrados() %>%
      st_drop_geometry() %>%
      select(nome, all_of(cols_existentes)) %>%
      pivot_longer(cols = -nome, names_to = "Var", values_to = "Val") %>%
      mutate(
        Val = as.numeric(Val),
        Var_Nome = ifelse(Var %in% names(nomes_amigaveis), nomes_amigaveis[Var], Var),
        media_pe = as.numeric(medias_estado[Var]),
        categ = factor(categorizar_valor(Val), levels = names(cores_fixas))
      )
    
    ggplot(df_plot, aes(x = reorder(nome, Val), y = Val)) +
      geom_col(aes(fill = categ), width = 0.6) + 
      scale_fill_manual(values = cores_fixas, name = "Categoria", drop = FALSE) +
      geom_hline(aes(yintercept = media_pe), color = "black", linetype = "dashed", size = 1) +
      facet_wrap(~Var_Nome, scales = "free_x") + 
      coord_flip() +
      labs(title = paste("Detalhamento Regional:", nomes_amigaveis[input$subindice]),
           subtitle = "Barras coloridas por categoria de risco/ameaça",
           x = "Municípios", y = "Score (0 a 1)") +
      theme_minimal() + 
      theme(
        plot.title = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        strip.text = element_text(size = 14, face = "bold", color = "#2c3e50"),
        legend.position = "bottom"
      )
  })
  
  # --- 4. PERFIL DO MUNICÍPIO ---
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
      geom_col(aes(fill = Dim_Nome), alpha = 0.8, width = 0.7) + 
      scale_fill_brewer(palette = "Spectral") +
      geom_point(aes(y = media_pe, color = "Média PE"), size = 5) +
      scale_color_manual(name = "Referência", values = c("Média PE" = "black")) +
      facet_wrap(~nome) + 
      ylim(0, 1) +
      labs(title = "Perfil Municipal vs. Patamar de Pernambuco", x = "Dimensão", y = "Score (0 a 1)") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold"),
        strip.text = element_text(size = 16, face = "bold"),
        legend.position = "bottom"
      )
  })
}

shinyApp(ui, server)