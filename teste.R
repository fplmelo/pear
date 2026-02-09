library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)
library(shinythemes)

# --- CARREGAMENTO E PREPARAÇÃO DOS DADOS ---
dados_sf <- readRDS("shiny_data.rds") %>% st_transform(4326)

dados_sf <- dados_sf %>%
  mutate_all(function(x) as.numeric(as.character(x)))

# GARANTIR QUE COLUNAS SEJAM NUMÉRICAS (Resolve o erro "não numérico")
colunas_para_corrigir <- c("sensib", "expos", "ameaca_atual", "risco_atual", 
                           "ameaca_SWL1.5", "ameaca_SWL2.0", "risco_SWL1.5", "risco_SWL2.0",
                           "am_prec_acum", "am_temp_med", "am..sazon..prep", "am.ampl_temp")

dados_sf <- dados_sf %>%
  mutate(across(any_of(colunas_para_corrigir), \(x) as.numeric(as.character(x))))

medias_estado <- dados_sf %>% 
  st_drop_geometry() %>% 
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))

### PADRÃO DE CORES E CATEGORIZAÇÃO ###
cores_fixas <- c(
  "Muito Baixo" = "#02c650", 
  "Baixo"       = "#a9de00", 
  "Médio"       = "#ffcd00", 
  "Alto"        = "#ff8300", 
  "Muito Alto"  = "#f40000",
  "Sem dados"   = "#cccccc"
)

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

# --- DICIONÁRIOS ---
nomes_amigaveis <- c(
  "sensib" = "Sensibilidade", "expos" = "Exposição", 
  "ameaca_atual" = "Ameaça Climática Atual", "risco_atual" = "Risco Climático Atual",
  "ameaca_SWL1.5" = "Ameaça Futura (1.5°C)", "ameaca_SWL2.0" = "Ameaça Futura (2.0°C)",
  "risco_SWL1.5" = "Risco Futuro (1.5°C)", "risco_SWL2.0" = "Risco Futuro (2.0°C)",
  "am_prec_acum" = "Anomalia de Precipitação", "am_temp_med" = "Aumento de Temperatura",
  "am..sazon..prep" = "Aumento de Sazonalidade da Precipitação", "am.ampl_temp"= "Aumento da Amplitude da Temperatura"
)

hierarquia <- list(
  "Sensibilidade" = c("Mudança Uso Solo" = "s_luc_2017", "Cobertura do Solo" = "s_csol_2017", "Infraestrutura" = "s_infra_2017", "Capac. Adaptativa" = "s_cap"),
  "Exposição"     = c("Cobertura Exposta" = "e_cobexpo", "Prioridade Cons." = "e_priocons", "Proteção Territorial" = "e_areaprot" ),
  "Ameaça"        = c("Ameaça Geral" = "ameaca_atual", "Anomalia de Precipitação" = "am_prec_acum", "Aumento de Temperatura" = "am_temp_med", "Sazonalidade" = "am..sazon..prep", "Amplitude" = "am.ampl_temp"),
  "Risco"         = c("Risco Atual" = "risco_atual", "Risco Futuro (1.5°C)" = "risco_SWL1.5", "Risco Futuro (2.0°C)" = "risco_SWL2.0")
)

variaveis_brutas_map <- list(
  "ameaca_atual" = c("am_prec_acum", "am_temp_med", "am.ampl_temp","am..sazon..prep"),
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
      helpText("Padrão de Cores: Verde (Muito Baixo) a Vermelho (Muito Alto).")
    ),
    mainPanel(
      tabsetPanel(
        id = "abas_painel",
        tabPanel("Panorama Estadual", br(), leafletOutput("mapa_panorama", height = "500px")),
        tabPanel("Mapas e dados detalhados", br(), 
                 leafletOutput("mapa_dinamico", height = "400px"), 
                 br(), 
                 plotOutput("plot_detalhado", height = "800px")),
        tabPanel("Perfil do Município", br(), 
                 uiOutput("menu_municipios"), 
                 plotOutput("plot_perfil_muni", height = "550px"))
      )
    )
  )
)

# --- SERVER ---
server <- function(input, output, session) {
  
  dados_filtrados <- reactive({ dados_sf %>% filter(rds == input$selecao_rd) })
  
  output$menu_subindice <- renderUI({
    req(input$dimensao)
    selectInput("subindice", "Indicador Específico:", choices = hierarquia[[input$dimensao]])
  })
  
  # 1. MAPA PANORAMA
  output$mapa_panorama <- renderLeaflet({
    req(input$dim_panorama)
    col <- input$dim_panorama
    df_mapa <- dados_sf %>% mutate(categ = factor(categorizar_valor(get(col)), levels = names(cores_fixas)))
    pal <- colorFactor(palette = as.character(cores_fixas), domain = df_mapa$categ)
    leaflet(df_mapa) %>% addProviderTiles("CartoDB.Positron") %>%
      addPolygons(fillColor = ~pal(categ), weight = 1, color = "white", fillOpacity = 0.8,
                  label = ~paste(nome, ":", round(get(col), 2)))
  })
  
  # 2. MAPA REGIONAL
  output$mapa_dinamico <- renderLeaflet({
    req(input$subindice)
    col_mapa <- input$subindice
    df_regiao <- dados_filtrados() %>% mutate(categ = factor(categorizar_valor(get(col_mapa)), levels = names(cores_fixas)))
    pal <- colorFactor(palette = as.character(cores_fixas), domain = df_regiao$categ)
    leaflet(df_regiao) %>% addProviderTiles("CartoDB.Positron") %>%
      addPolygons(fillColor = ~pal(categ), weight = 1.5, color = "white", fillOpacity = 0.8) %>%
      addLegend(pal = pal, values = factor(names(cores_fixas), levels = names(cores_fixas)), title = "Categoria", position = "bottomright")
  })
  
  # 3. GRÁFICO DE BARRAS (CORRIGIDO: Cores por categoria e conversão numérica)
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
        # Lógica de cores categóricas (igual ao mapa)
        categ = factor(categorizar_valor(Val), levels = names(cores_fixas))
      )
    
    ggplot(df_plot, aes(x = reorder(nome, Val), y = Val)) +
      geom_col(aes(fill = categ), width = 0.6) + 
      scale_fill_manual(values = cores_fixas, name = "Categoria", drop = FALSE) +
      geom_hline(aes(yintercept = media_pe), color = "black", linetype = "dashed", size = 1) +
      facet_wrap(~Var_Nome, scales = "free_x") + 
      coord_flip() +
      labs(title = paste("Detalhamento Regional:", nomes_amigaveis[input$subindice]), x = "Municípios", y = "Score") +
      theme_minimal() + 
      theme(axis.text.y = element_text(size = 12, face = "bold"), strip.text = element_text(size = 14, face = "bold"))
  })
  
  # 4. PERFIL DO MUNICÍPIO (Cores categóricas também aqui)
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
      mutate(
        Val = as.numeric(Val),
        Dim_Nome = ifelse(Dim %in% names(nomes_amigaveis), nomes_amigaveis[Dim], Dim),
        media_pe = as.numeric(medias_estado[Dim]),
        categ = factor(categorizar_valor(Val), levels = names(cores_fixas))
      )
    
    ggplot(df_perfil, aes(x = Dim_Nome, y = Val)) +
      geom_col(aes(fill = categ), width = 0.7) + 
      scale_fill_manual(values = cores_fixas, name = "Categoria") +
      geom_point(aes(y = media_pe), color = "black", size = 4) +
      facet_wrap(~nome) + ylim(0, 1) +
      theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

shinyApp(ui, server)