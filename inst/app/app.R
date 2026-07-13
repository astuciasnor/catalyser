# IDE_R - Aplicativo Principal Shiny
library(shiny)
library(bslib)
library(ggplot2)
library(DT)
library(readxl)

# Aumenta limite de upload para 50 MB
options(shiny.maxRequestSize = 50 * 1024^2)

# Carrega os módulos de análise
source("modules/utils_export.R", encoding = "UTF-8")
source("modules/mod_regression.R", encoding = "UTF-8")
source("modules/mod_model_discovery.R", encoding = "UTF-8")
source("modules/mod_nonlinear.R", encoding = "UTF-8")
source("modules/mod_description.R", encoding = "UTF-8")
source("modules/mod_parametric.R", encoding = "UTF-8")
source("modules/mod_sampling.R", encoding = "UTF-8")
source("modules/mod_anova.R", encoding = "UTF-8")
source("modules/mod_pca.R", encoding = "UTF-8")
source("modules/mod_hca.R", encoding = "UTF-8")
source("modules/mod_contingency.R", encoding = "UTF-8")
source("modules/mod_experimental_design.R", encoding = "UTF-8")
source("modules/mod_nonparametric.R", encoding = "UTF-8")
source("modules/mod_pizza.R", encoding = "UTF-8")
source("modules/mod_viz_extra.R", encoding = "UTF-8")
source("modules/mod_mapa.R", encoding = "UTF-8")
source("modules/mod_mapa_pontos.R", encoding = "UTF-8")
source("modules/mod_series_temporais.R", encoding = "UTF-8")
source("modules/mod_arrumar.R", encoding = "UTF-8")
source("modules/mod_calcular.R", encoding = "UTF-8")
source("modules/mod_agrupar_sumarizar.R", encoding = "UTF-8")
source("modules/registro_tratamentos.R", encoding = "UTF-8")
source("modules/registro_bases.R", encoding = "UTF-8")
source("modules/mod_tratar.R", encoding = "UTF-8")
source("modules/mod_bases_derivadas.R", encoding = "UTF-8")
source("modules/mod_comunicacao.R", encoding = "UTF-8")
source("modules/mod_correlacao.R", encoding = "UTF-8")
source("modules/mod_laboratorio.R", encoding = "UTF-8")
source("modules/mod_ancova.R", encoding = "UTF-8")
source("modules/mod_frequencia.R", encoding = "UTF-8")
# Parqueados para a v2 (fora do escopo v1 do menu Mapas — ver mapas.md / BACKLOG):
# source("modules/mod_mapa_densidade.R") # densidade/heatmap de ocorrências
# source("modules/mod_mapa_raster.R")    # raster ambiental isolado
# source("modules/mod_leaflet.R")        # mapa interativo (leaflet, só HTML)

# Funções auxiliares para tipagem de colunas
detect_col_type <- function(col) {
  if (is.factor(col)) return("factor")
  if (is.logical(col)) return("logical")
  if (inherits(col, "Date") || inherits(col, "POSIXt")) return("Date")
  if (is.integer(col)) return("integer")
  if (is.numeric(col)) return("numeric")
  return("character")
}

sanitize_id <- function(x) {
  gsub("[^a-zA-Z0-9_]", "_", x)
}

# Lista dinâmica dos conjuntos do EAPADados: puxa TODOS os datasets do pacote
# (novos conjuntos passam a aparecer sozinhos), mantendo só os que são data.frame
# e tirando tabelas auxiliares (dicionários/referências). Se o pacote não estiver
# disponível, cai numa lista mínima de segurança.
eapa_datasets <- local({
  aux <- c("dicionario_variaveis_amostragem", "referencias_dados_amostragem")
  itens <- tryCatch(
    sub("\\s.*$", "", data(package = "EAPADados")$results[, "Item"]),
    error = function(e) character(0)
  )
  itens <- setdiff(itens, aux)
  ok <- itens[vapply(itens, function(nm) {
    obj <- tryCatch(get(nm, envir = asNamespace("EAPADados")), error = function(e) NULL)
    is.data.frame(obj)
  }, logical(1))]
  ok <- sort(ok)
  if (length(ok) == 0) {
    ok <- c("artemia", "biometria_caranguejos", "camaroes_sexo",
            "cangulo_crescimento", "captura_petrechos", "isoproteica_bagre",
            "tilapia_crescimento", "walleye_erie")
  }
  ok
})
eapa_dataset_default <- if ("artemia" %in% eapa_datasets) "artemia" else eapa_datasets[1]

# Interface do Usuário (UI)
ui <- page_navbar(
  id = "main_navbar",
  window_title = "CatalyseR",
  title = div(
    style = "display: flex !important; flex-direction: row !important; align-items: center !important; justify-content: center !important; gap: 10px !important; padding: 2px 10px !important; width: 100% !important; height: 100% !important;",
    tags$a(
      href = "https://www.r-project.org/",
      target = "_blank",
      style = "display: flex; align-items: center; justify-content: center; transition: all 0.2s ease;",
      tags$img(src = "r_logo.png", height = "52px", style = "opacity: 0.95;")
    ),
    tags$a(
      href = "https://shiny.posit.co/",
      target = "_blank",
      style = "display: flex; align-items: center; justify-content: center; transition: all 0.2s ease;",
      tags$img(src = "shiny_logo.png", height = "59px", style = "opacity: 0.95;")
    ),
    tags$a(
      href = "https://portal.ufpa.br/",
      target = "_blank",
      style = "display: flex; align-items: center; justify-content: center; transition: all 0.2s ease;",
      tags$img(src = "ufpa_logo.png", height = "76px", style = "opacity: 0.95; object-fit: contain;")
    )
  ),
  theme = bs_theme(
    version = 5,
    bootswatch = "cerulean", # Tema científico limpo
    primary = "#0d6efd",
    secondary = "#6c757d"
  ),
  header = tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Outfit:wght@400;600;700;800&family=Inter:wght@400;500;600&display=swap"),
    tags$style(HTML("
      body {
        font-family: 'Inter', sans-serif !important;
        background-color: #f8f9fa;
      }
      .navbar {
        box-shadow: 0 2px 8px rgba(0,0,0,0.05);
        padding-top: 0.2rem;
        padding-bottom: 0.2rem;
        position: relative !important;
      }
      .navbar > .container-fluid {
        display: grid !important;
        grid-template-columns: 2.5fr 7fr 2.5fr !important;
        gap: 1.5rem !important; /* Adicionado gap de 1.5rem para se alinhar perfeitamente com o corpo (layout_columns) */
        padding-left: 1.5rem !important;
        padding-right: 1.5rem !important;
        width: 100% !important;
        align-items: center !important; /* Centraliza verticalmente as colunas no container pai */
      }
      .navbar-header {
        grid-column: 3 !important; /* Posicionado na coluna da direita */
        grid-row: 1 !important; /* Força na primeira linha do grid */
        width: 100% !important; /* Estica para ocupar toda a terceira coluna do grid */
        max-width: none !important;
        margin-left: 0px !important; /* Sem margem para alinhamento horizontal perfeito */
        height: auto !important;
        display: flex !important; /* Altera para flexbox para controle de alinhamento robusto */
        align-items: center !important; /* Centraliza verticalmente a marca/logos */
        justify-content: center !important; /* Centraliza horizontalmente */
        position: relative !important;
        float: none !important;
        margin-top: 0 !important;
        margin-bottom: 0 !important;
      }
      .navbar-header::before,
      .navbar-header::after {
        display: none !important;
        content: none !important;
      }
      .navbar-brand {
        display: flex !important;
        align-items: center !important;
        justify-content: center !important;
        width: 100% !important;
        max-width: none !important;
        padding: 0 !important;
        margin: 0 !important;
        height: 90px !important; /* Ajustado para 90px de altura */
        position: relative !important;
        background-color: transparent !important;
        border: none !important;
        box-shadow: none !important;
        z-index: 1 !important;
        float: none !important;
      }
      .navbar-brand::before {
        content: '' !important;
        position: absolute !important;
        top: 0 !important; /* Alinha com o limite superior do pai */
        bottom: 0 !important; /* Alinha com o limite inferior do pai */
        left: 0 !important;
        right: 0 !important;
        background-color: rgba(224, 242, 254, 0.75) !important; /* Fundo azul celeste premium */
        border: 2px solid rgba(13, 110, 253, 0.45) !important;
        border-radius: 12px !important;
        box-shadow: inset 0 1px 2px rgba(0,0,0,0.03), 0 2px 4px rgba(0,0,0,0.05) !important;
        z-index: -1 !important; /* Fica atrás do conteúdo */
      }
      .navbar-nav > li:has(.navbar-slogan-container) {
        position: absolute !important;
        top: -2px !important; /* Alinha com a borda externa superior do pai */
        bottom: -2px !important; /* Alinha com a borda externa inferior do pai (altura dinâmica!) */
        display: flex !important;
        align-items: center !important;
        justify-content: center !important;
        background-color: rgba(224, 242, 254, 0.75) !important; /* Fundo azul celeste premium */
        border: 2px solid rgba(13, 110, 253, 0.45) !important;
        box-shadow: inset 0 1px 2px rgba(0,0,0,0.03), 0 2px 4px rgba(0,0,0,0.05);
        z-index: 1000 !important;
        padding: 0 !important;
        transition: all 0.2s ease;
        width: 35.71% !important; /* Corresponde a 2.5fr/7fr da largura do pai */
        left: calc(-35.71% - 1.5rem) !important; /* Posicionado à esquerda no lugar do slogan (Coluna 1) */
        right: auto !important;
        border-radius: 12px !important;
      }
      .navbar-collapse {
        grid-column: 2 !important;
        grid-row: 1 !important; /* Força na primeira linha do grid */
        display: flex !important;
        justify-content: center !important;
        align-items: center !important; /* MUDADO de stretch para center */
        width: 100% !important;
        position: relative !important; /* Contexto de posicionamento para o slogan */
      }
      
      /* Estilo dos Menus Empilhados (Ícone no Topo, Texto Embaixo) */
      .navbar-nav {
        display: flex !important;
        flex-direction: row !important;
        flex-wrap: nowrap !important; /* Evita quebra dos menus em múltiplas linhas */
        align-items: center !important;
        width: 100% !important;
        height: 90px !important; /* Ajustado para 90px de altura */
        justify-content: space-between !important;
        gap: 2px !important;
        border: 2px solid rgba(13, 110, 253, 0.45) !important;
        padding: 4px 8px !important; /* Ajustado padding vertical */
        border-radius: 12px !important;
        background-color: rgba(228, 244, 230, 0.75) !important;
        box-shadow: inset 0 1px 2px rgba(0,0,0,0.03);
      }
      .navbar-nav .nav-item {
        flex: 1 1 0px !important;
        text-align: center !important;
      }
      
      .navbar-nav .nav-item .nav-link, 
      .navbar-nav .nav-item .dropdown-toggle {
        display: flex !important;
        flex-direction: column !important;
        align-items: center !important;
        justify-content: center !important;
        text-align: center !important;
        font-size: 0.76rem !important; /* Aumentado para 0.76rem */
        font-family: 'Outfit', sans-serif !important;
        font-weight: 700 !important;
        padding: 2px 2px !important; /* Padding vertical mínimo */
        gap: 2px !important; /* Ajustado espaçamento entre ícone e texto */
        line-height: 1.05 !important;
        width: 100% !important;
        min-width: 0 !important;
        max-width: none !important;
        color: #495057 !important;
        border-radius: 8px;
        transition: all 0.2s ease;
      }
      .navbar-nav .nav-item .nav-link:hover,
      .navbar-nav .nav-item .dropdown-toggle:hover {
        background-color: rgba(13, 110, 253, 0.05) !important;
        color: #0d6efd !important;
      }
      .navbar-nav .nav-item.active .nav-link,
      .navbar-nav .nav-item.show .dropdown-toggle,
      .navbar-nav .nav-link.active {
        background-color: rgba(13, 110, 253, 0.08) !important;
        color: #0d6efd !important;
      }
      .navbar-nav .nav-item .nav-link i,
      .navbar-nav .nav-item .dropdown-toggle i {
        font-size: 1.45rem !important; /* Aumentado para 1.45rem */
        margin-bottom: 1px !important; /* Margem mínima */
        margin-right: 0px !important;
      }
      
      /* Cores individuais e modernas para cada ícone de menu */
      .navbar-nav > li:nth-child(1) .nav-link i, .navbar-nav > li:nth-child(1) .dropdown-toggle i { color: #0d6efd !important; } /* Preparando Dados -> azul */
      .navbar-nav > li:nth-child(2) .nav-link i, .navbar-nav > li:nth-child(2) .dropdown-toggle i { color: #8b5cf6 !important; } /* Planejando sua Pesquisa -> violeta */
      .navbar-nav > li:nth-child(3) .nav-link i, .navbar-nav > li:nth-child(3) .dropdown-toggle i { color: #00b894 !important; } /* Descrevendo Dados -> verde-água */
      .navbar-nav > li:nth-child(4) .nav-link i, .navbar-nav > li:nth-child(4) .dropdown-toggle i { color: #7c3aed !important; } /* Modelos de Regressão -> roxo */
      .navbar-nav > li:nth-child(5) .nav-link i, .navbar-nav > li:nth-child(5) .dropdown-toggle i { color: #ec4899 !important; } /* Regressão Não Linear -> rosa/magenta */
      .navbar-nav > li:nth-child(6) .nav-link i, .navbar-nav > li:nth-child(6) .dropdown-toggle i { color: #f97316 !important; } /* Testes Paramétricos -> laranja */
      .navbar-nav > li:nth-child(7) .nav-link i, .navbar-nav > li:nth-child(7) .dropdown-toggle i { color: #84cc16 !important; } /* Testes Não Paramétricos -> verde-limão */
      .navbar-nav > li:nth-child(8) .nav-link i, .navbar-nav > li:nth-child(8) .dropdown-toggle i { color: #d946ef !important; } /* Estatística Multivariada -> rosa/magenta */
      .navbar-nav > li:nth-child(9) .nav-link i, .navbar-nav > li:nth-child(9) .dropdown-toggle i { color: #0ea5e9 !important; } /* Estatísticas Avançadas -> azul-ciano */
      .navbar-nav > li:nth-child(10) .nav-link i, .navbar-nav > li:nth-child(10) .dropdown-toggle i { color: #00c2cb !important; } /* Visualizando Dados -> azul claro/turquesa */
      .navbar-nav > li:nth-child(11) .nav-link i, .navbar-nav > li:nth-child(11) .dropdown-toggle i { color: #6366f1 !important; } /* Mapas -> roxo-violeta */
      .navbar-nav > li:nth-child(12) .nav-link i, .navbar-nav > li:nth-child(12) .dropdown-toggle i { color: #22c55e !important; } /* Calculando Probabilidades -> verde */
      .navbar-nav > li:nth-child(14) .nav-link i, .navbar-nav > li:nth-child(14) .dropdown-toggle i { color: #3b82f6 !important; } /* Ajuda -> azul */
      .navbar-nav > li:nth-child(15) .nav-link i, .navbar-nav > li:nth-child(15) .dropdown-toggle i { color: #1d4ed8 !important; } /* Sobre -> azul institucional */
      
      /* Estilos para o botão 'Sobre' personalizado na barrinha da direita */
      #sobre-custom-btn {
        transition: all 0.2s ease;
      }
      #sobre-custom-btn:hover {
        background-color: rgba(13, 110, 253, 0.08) !important;
        border-radius: 8px;
        transform: translateY(-1px);
      }
      .navbar-nav > li:nth-child(15) {
        display: none !important; /* Esconde a aba original 'Sobre' no menu central */
      }
      
      /* Dropdowns da bslib mantêm o formato horizontal interno clássico */
      .dropdown-menu .dropdown-item {
        display: flex !important;
        flex-direction: row !important;
        align-items: center !important;
        text-align: left !important;
        font-size: 0.85rem !important;
        font-family: 'Inter', sans-serif !important;
        padding: 8px 16px !important;
        gap: 8px !important;
      }
      .dropdown-menu .dropdown-item i {
        font-size: 1rem !important;
        color: #6c757d !important;
      }
      .dropdown-menu .dropdown-item:hover i {
        color: #0d6efd !important;
      }
      
      .card {
        border-radius: 12px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.03);
        border: 1px solid rgba(0,0,0,0.05);
      }
      .card-header {
        font-family: 'Outfit', sans-serif !important;
        font-weight: 700;
        letter-spacing: -0.2px;
      }
      .btn {
        border-radius: 8px;
        font-weight: 500;
        transition: all 0.2s ease;
      }
      .btn:hover {
        transform: translateY(-1px);
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      }
      /* Botões utilitários/secundários no tema Ocean:
         troca o cinza de baixo contraste por um verde-água suave e legível.
         Fundo seafoam claro + texto NAVY; hover em TEAL sólido com texto branco. */
      .btn-outline-secondary {
        background-color: #E6F2F1 !important;
        color: #0F3B5F !important;
        border-color: #A9D2D4 !important;
        font-weight: 600 !important;
      }
      .btn-outline-secondary:hover,
      .btn-outline-secondary:focus,
      .btn-outline-secondary:active {
        background-color: #2E7D8F !important;
        color: #ffffff !important;
        border-color: #2E7D8F !important;
      }
      .btn-outline-secondary:hover .fa,
      .btn-outline-secondary:hover svg { color: #ffffff !important; }

      /* REFINAMENTO DE DENSIDADE E COMPACTAÇÃO VISUAL */
      /* 1. Compactação de Cards (Painel Lateral e Central) */
      .card-body {
        padding: 10px 12px !important;
      }
      .card-header {
        padding: 8px 12px !important;
      }
      
      /* 2. Compactação de Form Groups, Inputs e Controles */
      .form-group, .shiny-input-container {
        margin-bottom: 8px !important;
      }
      .control-label {
        margin-bottom: 3px !important;
        font-size: 0.83rem !important;
        font-weight: 600;
        color: #495057;
      }
      .form-check {
        margin-bottom: 4px !important;
        font-size: 0.83rem !important;
      }
      .form-control, .form-select, .selectize-input {
        padding: 4px 8px !important;
        font-size: 0.83rem !important;
        height: auto !important;
        min-height: 0 !important;
      }
      .selectize-control {
        margin-bottom: 6px !important;
      }
      /* Garante área de digitação utilizável na busca do selectize */
      .selectize-input {
        min-height: 1.7rem !important;
      }
      .selectize-input input {
        font-size: 0.83rem !important;
      }
      
      /* 3. Compactação das Abas Superiores do Painel Central */
      .nav-tabs .nav-link {
        padding: 6px 12px !important;
        font-size: 0.85rem !important;
      }
      
      /* 4. Compactação do Espaço Vertical antes dos Gráficos e Tabelas */
      .card-body > .shiny-plot-output, 
      .card-body > .plotly, 
      .card-body > .shiny-html-output {
        margin-top: 2px !important;
        margin-bottom: 2px !important;
      }
      .card-body hr {
        margin: 8px 0 !important;
      }
      
      /* 5. Padronização e Ajuste Fino de Tabelas de Dados (DT) */
      .dataTables_wrapper .dataTables_filter, 
      .dataTables_wrapper .dataTables_length {
        margin-bottom: 6px !important;
        font-size: 0.83rem !important;
      }
      .dataTables_wrapper .dataTables_info, 
      .dataTables_wrapper .dataTables_paginate {
        margin-top: 6px !important;
        font-size: 0.8rem !important;
      }
      table.dataTable thead th, table.dataTable thead td {
        padding: 6px 8px !important;
        font-size: 0.83rem !important;
        font-family: 'Outfit', sans-serif !important;
        font-weight: 700;
      }
      table.dataTable tbody th, table.dataTable tbody td {
        padding: 5px 8px !important;
        font-size: 0.83rem !important;
      }
    "))
  ),
  
  # 1. Preparando Dados
  nav_menu(
    title = HTML("Preparando<br>Dados"),
    icon = icon("database"),
    nav_panel(
      title = "Importação e Visualização",
      icon = icon("file-import"),
      layout_columns(
        col_widths = c(1, 1, 1),
        style = "grid-template-columns: 2.5fr 7fr 2.5fr !important;",
        
        # COLUNA 1: CARREGAMENTO DE DADOS (CONFIGURAÇÃO)
        div(
          card(
            card_header("Carregamento de Dados"),
            card_body(
              style = "padding: 12px 15px;",
              radioButtons("data_source", "Origem dos Dados:",
                           choices = c("Arquivo Local (CSV/Excel)" = "local",
                                       "Pacote EAPADados" = "package")),
              conditionalPanel(
                condition = "input.data_source == 'local'",
                fileInput("file_upload", "Escolha o arquivo (.csv, .xlsx, .xls):",
                          accept = c(".csv", ".xlsx", ".xls")),
                conditionalPanel(
                  condition = "input.file_upload != null && input.file_upload.name.toLowerCase().endsWith('.csv')",
                  checkboxInput("csv_header", "Cabeçalho na primeira linha", TRUE),
                  radioButtons("csv_sep", "Separador de Coluna:",
                               choices = c("Vírgula (,)" = ",",
                                           "Ponto e Vírgula (;)" = ";",
                                           "Tabulação (Tab)" = "\t"),
                               selected = ","),
                  radioButtons("csv_dec", "Separador de Decimal:",
                               choices = c("Ponto (.)" = ".",
                                           "Vírgula (,)" = ","),
                               selected = ".")
                ),
                uiOutput("excel_sheet_selector")
              ),
              conditionalPanel(
                condition = "input.data_source == 'package'",
                selectizeInput("package_dataset", "Selecione o Dataset do EAPADados:",
                  choices = eapa_datasets,
                  selected = eapa_dataset_default,
                  options = list(
                    placeholder = "Digite ou escolha um conjunto de dados...",
                    openOnFocus = TRUE,
                    maxOptions = 100
                  ))
              ),
              uiOutput("dataset_vars_selector", style = "margin-top: -10px; margin-bottom: 0px; padding: 0;"),
              hr(style = "margin: 6px 0;")
            )
          ),
          uiOutput("variable_type_converter_ui")
        ),
        
        # COLUNA 2: ABAS DE EXIBIÇÃO (PRINCIPAL)
        navset_card_tab(
          nav_panel(
            title = "Visualização dos Dados",
            icon = icon("table"),
            card_body(
              style = "padding: 10px 15px;",
              DTOutput("data_preview_table")
            )
          ),
          nav_panel(
            title = "Resumo dos Dados",
            icon = icon("chart-bar"),
            card_body(
              style = "padding: 10px 15px;",
              verbatimTextOutput("data_summary_text")
            )
          )
        ),
        
        # COLUNA 3: STATUS DO DATASET & EXPORTAÇÃO
        div(
          card(
            card_header("Status do Dataset"),
            card_body(
              style = "padding: 12px 15px;",
              uiOutput("dataset_status_indicator"),
              hr(style = "margin: 10px 0;"),
              h6("Formatos Suportados", style = "color: #0d6efd; font-weight: 700; margin-bottom: 8px;"),
              tags$ul(style = "padding-left: 15px; margin-bottom: 0; font-size: 0.85rem; line-height: 1.4;",
                tags$li("CSV (.csv)"),
                tags$li("Excel (.xlsx, .xls)"),
                tags$li("Pacote R 'EAPADados'")
              )
            )
          ),
          card(
            card_header("Exportar Projeto Consolidado"),
            card_body(
              style = "padding: 12px 15px;",
              uiOutput("export_project_options_ui")
            )
          )
        )
      )
    ),
    nav_panel(
      title = "Empilhar Colunas (Largo → Longo)",
      icon = icon("layer-group"),
      mod_arrumar_ui("arrumar_emp", modo_fixo = "empilhar")
    ),
    nav_panel(
      title = "Separar Coluna em Colunas",
      icon = icon("table-columns"),
      mod_arrumar_ui("arrumar_sep", modo_fixo = "separar")
    ),
    nav_panel(
      title = "Calcular / Reescalar Variável",
      icon = icon("calculator"),
      mod_calcular_ui("calcular")
    ),
    nav_panel(
      title = "Agrupar / Sumarizar",
      icon = icon("layer-group"),
      mod_agrupar_sumarizar_ui("agrupar_sumarizar")
    ),
    nav_panel(
      title = "Trilha de Preparo",
      icon = icon("timeline"),
      mod_tratar_ui("tratar")
    ),
    nav_panel(
      title = "Bases Derivadas",
      icon = icon("diagram-project"),
      mod_bases_derivadas_ui("bases_derivadas")
    ),
    nav_panel(
      title = "Criando Tabela de Contingência",
      icon = icon("border-all"),
      mod_contingency_ui("contingency")
    )
  ),
  
  # 2. Planejando sua Pesquisa
  nav_menu(
    title = HTML("Planejando<br>sua Pesquisa"),
    icon = icon("compass-drafting"),
    nav_panel(
      title = "Planejamentos Observacionais",
      icon = icon("filter"),
      navset_card_tab(
        nav_panel(
          title = "Amostrando uma AAS",
          icon = icon("filter"),
          mod_aas_ui("aas")
        ),
        nav_panel(
          title = "Amostrando um AE Proporcional",
          icon = icon("layer-group"),
          mod_aep_ui("aep")
        ),
        nav_panel(
          title = "Amostrando uma AS (Sistemática)",
          icon = icon("network-wired"),
          mod_as_ui("as")
        )
      )
    ),
    nav_panel(
      title = "Planejamentos Experimentais",
      icon = icon("flask"),
      mod_experimental_design_ui("experimental_design")
    )
  ),
  
  # 2. Descrevendo Dados
  nav_menu(
    title = HTML("Descrevendo<br>Dados"),
    icon = icon("chart-bar"),
    nav_panel(
      title = "Estatística Descritiva",
      icon = icon("table-list"),
      mod_descr_stats_ui("descr_stats")
    ),
    nav_panel(
      title = "Tabela de Frequência",
      icon = icon("layer-group"),
      mod_frequencia_ui("frequencia")
    ),
    nav_panel(
      title = "Histogramas",
      icon = icon("chart-simple"),
      mod_histogram_ui("histogram")
    ),
    nav_panel(
      title = "Boxplot",
      icon = icon("square-poll-vertical"),
      mod_boxplot_ui("boxplot")
    )
  ),
  
  # 3. Modelos de Regressão
  nav_menu(
    title = HTML("Modelos de<br>Regressão"),
    icon = icon("chart-line"),
    nav_panel(
      title = "Correlação",
      icon = icon("square-root-variable"),
      mod_correlacao_ui("correlacao")
    ),
    nav_panel(
      title = "Descobrindo o Modelo",
      icon = icon("magnifying-glass-chart"),
      mod_model_discovery_ui("discovery")
    ),
    nav_panel(
      title = "Regressão Linear Simples",
      icon = icon("chart-line"),
      mod_regression_ui("regression")
    ),
    nav_panel(
      title = "Regressão Linear Múltipla",
      icon = icon("table-cells"),
      card(
        card_header("Regressão Linear Múltipla"),
        card_body(
          h5("Módulo em Desenvolvimento", class = "text-primary"),
          p("Esta análise estará disponível em breve no CatalyseR!"),
          helpText(
            "Permitirá ajustar modelos com múltiplas variáveis preditoras. Exemplos de aplicação:",
            tags$ul(
              tags$li("Modelagem de crescimento em cultivos fechados (fases lag, log, desaceleração e estacionária)."),
              tags$li("Curvas de seletividade de petrechos de pesca (redes de emalhe ou arrasto), estimando a probabilidade de retenção em função do comprimento do peixe.")
            )
          )
        )
      )
    ),
    nav_panel(
      title = "Séries Temporais",
      icon = icon("chart-area"),
      mod_series_temporais_ui("series")
    )
  ),

  # 3.1. Regressão Não Linear
  nav_menu(
    title = HTML("Regressão<br>Não Linear"),
    icon = icon("bezier-curve"),
    nav_panel(
      title = "Curva Exponencial",
      icon = icon("arrow-trend-up"),
      mod_nonlinear_ui("exponencial", "exponencial")
    ),
    nav_panel(
      title = "Curva de Modelo de Potência",
      icon = icon("circle-nodes"),
      mod_nonlinear_ui("potencia", "potencia")
    ),
    nav_panel(
      title = "Curva de Crescimento de Peixes",
      icon = icon("fish"),
      mod_nonlinear_ui("von_bertalanffy", "von_bertalanffy")
    ),
    nav_panel(
      title = "Modelo Polinomial",
      icon = icon("chart-area"),
      mod_nonlinear_ui("polinomial", "polinomial")
    ),
    nav_panel(
      title = "Curva Logarítmica",
      icon = icon("chart-simple"),
      mod_nonlinear_ui("logaritmica", "logaritmica")
    ),
    nav_panel(
      title = "Curva Logística",
      icon = icon("chart-line"),
      mod_regression_ui("logistic_regression", is_logistic = TRUE)
    )
  ),
  
  # 4. Testes Paramétricos
  nav_menu(
    title = HTML("Testes<br>Paramétricos"),
    icon = icon("calculator"),
    nav_panel(
      title = "Teste t de Student",
      icon = icon("arrows-left-right"),
      mod_parametric_ui("parametric")
    ),
    nav_panel(
      title = "ANOVA (Análise de Variância)",
      icon = icon("sliders"),
      mod_anova_ui("anova")
    ),
    nav_panel(
      title = "ANCOVA (Análise de Covariância)",
      icon = icon("chart-line"),
      mod_ancova_ui("ancova")
    )
  ),

  # 5. Testes Não Paramétricos
  nav_menu(
    title = HTML("Testes Não<br>Paramétricos"),
    icon = icon("percent"),
    nav_panel(
      title = "Qui-quadrado (independência)",
      icon = icon("table-cells"),
      mod_nonparametric_ui("np_qui", "quiquadrado")
    ),
    nav_panel(
      title = "Mann-Whitney (2 grupos)",
      icon = icon("arrows-left-right"),
      mod_nonparametric_ui("np_mw", "mannwhitney")
    ),
    nav_panel(
      title = "Wilcoxon (pareado)",
      icon = icon("shuffle"),
      mod_nonparametric_ui("np_wil", "wilcoxon")
    ),
    nav_panel(
      title = "Kruskal-Wallis (k grupos)",
      icon = icon("arrows-up-down"),
      mod_nonparametric_ui("np_kw", "kruskal")
    )
  ),
  
  # 6. Estatística Multivariada
  nav_menu(
    title = HTML("Estatística<br>Multivariada"),
    icon = icon("diagram-project"),
    nav_panel(
      title = "PCA (Componentes Principais)",
      icon = icon("diagram-project"),
      mod_pca_ui("pca")
    ),
    nav_panel(
      title = "Análise de Agrupamentos (Clustering)",
      icon = icon("bezier-curve"),
      mod_hca_ui("hca")
    )
  ),
  
  # 7. Comunicação de Resultados (antiga "Estatísticas Avançadas")
  nav_menu(
    title = HTML("Comunicação<br>de Resultados"),
    icon = icon("file-export"),
    nav_panel(
      title = "Projeto de Comunicação",
      icon = icon("file-export"),
      mod_comunicacao_ui("comunicacao")
    )
  ),
  
  # 9. Visualizando Dados
  nav_menu(
    title = HTML("Visualizando<br>Dados"),
    icon = icon("eye"),
    nav_panel(
      title = "Gráfico de Dispersão",
      icon = icon("ellipsis"),
      mod_scatter_ui("scatter")
    ),
    nav_panel(
      title = "Gráfico de Linhas",
      icon = icon("chart-line"),
      mod_lines_ui("lines")
    ),
    nav_panel(
      title = "Gráfico de Barras",
      icon = icon("chart-column"),
      mod_bar_ui("bar")
    ),
    nav_panel(
      title = "Gráfico de Pizza",
      icon = icon("chart-pie"),
      mod_pizza_ui("pizza")
    )
  ),
  
  # 10. Mapas
  nav_menu(
    title = "Mapas",
    icon = icon("map"),
    nav_panel(
      title = "Mapa Coroplético (Brasil por Estado)",
      icon = icon("map-location-dot"),
      mod_mapa_ui("mapa")
    ),
    nav_panel(
      title = "Pontos / Estações",
      icon = icon("location-crosshairs"),
      mod_mapa_pontos_ui("mapa_pontos", "pontos")
    ),
    nav_panel(
      title = "Bolhas Proporcionais",
      icon = icon("circle-dot"),
      mod_mapa_pontos_ui("mapa_bolhas", "bolhas")
    )
  ),
  
  # 8. Laboratório de Conceitos (repurpose de "Calculando Probabilidades")
  nav_menu(
    title = HTML("Laboratório<br>de Conceitos"),
    icon = icon("flask"),
    nav_panel(
      title = "Visão geral",
      icon = icon("compass"),
      mod_laboratorio_ui("laboratorio")
    ),
    nav_panel(
      title = "Teorema do Limite Central",
      icon = icon("bell"),
      lab_placeholder("Teorema do Limite Central (TLC)", "Médias de qualquer distribuição viram um sino conforme n cresce; sliders de n e nº de amostras.", "bell")
    ),
    nav_panel(
      title = "Lei dos Grandes Números",
      icon = icon("arrow-trend-up"),
      lab_placeholder("Lei dos Grandes Números", "A média amostral converge para o valor esperado à medida que n aumenta.", "arrow-trend-up")
    ),
    nav_panel(
      title = "Cobertura do IC",
      icon = icon("bullseye"),
      lab_placeholder("Cobertura do Intervalo de Confiança", "100 amostras, 100 intervalos: cerca de 95% contêm a média verdadeira.", "bullseye")
    ),
    nav_panel(
      title = "Distribuição sob H0 / p-valor",
      icon = icon("chart-area"),
      lab_placeholder("Distribuição sob H0 / p-valor", "A distribuição nula, a estatística observada e a área do p-valor / região crítica.", "chart-area")
    ),
    nav_panel(
      title = "Curvas z / t / F / qui-quadrado",
      icon = icon("wave-square"),
      lab_placeholder("Curvas de distribuição (z, t, F, qui-quadrado)", "Forma das curvas conforme os graus de liberdade; áreas e quantis.", "wave-square")
    ),
    nav_panel(
      title = "Distribuição Normal",
      icon = icon("circle-nodes"),
      lab_placeholder("Distribuição Normal", "Áreas sob a curva e visualização da Normal (parâmetros mu e sigma).", "circle-nodes")
    ),
    nav_panel(
      title = "Distribuição Binomial",
      icon = icon("cubes"),
      lab_placeholder("Distribuição Binomial", "Cálculo e simulação interativa de probabilidades Binomiais (n, p).", "cubes")
    )
  ),
  
  # Spacer to push Ajuda and Sobre to the right
  nav_spacer(),
  
  # 11. Ajuda de Uso
  nav_panel(
    title = "Ajuda",
    icon = icon("circle-info"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Navegação da Ajuda",
        width = 320,
        textInput("help_search", label = NULL, placeholder = "🔍 Buscar na ajuda...", width = "100%"),
        uiOutput("help_topics_list_ui")
      ),
      uiOutput("help_content_display")
    )
  ),
  
  # 12. Sobre a IDE
  nav_panel(
    title = "Sobre",
    icon = icon("university"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Navegação Sobre a IDE",
        width = 320,
        textInput("about_search", label = NULL, placeholder = "🔍 Buscar no Sobre...", width = "100%"),
        uiOutput("about_topics_list_ui")
      ),
      uiOutput("about_content_display")
    )
  ),
  
  # 13. Slogan na Barra da Direita (alinhado na Coluna 3)
  nav_item(
    div(
      class = "navbar-slogan-container",
      style = "display: flex; flex-direction: row; align-items: center; justify-content: center; gap: 15px; width: 100%;",
      # Botão personalizado "Sobre"
      tags$a(
        id = "sobre-custom-btn",
        href = "#",
        onclick = "var el = document.querySelector(\"a[data-value='Sobre']\"); if(el) el.click(); return false;",
        style = "display: flex; flex-direction: column; align-items: center; justify-content: center; text-decoration: none; color: #1d4ed8 !important; padding: 4px 8px; transition: all 0.2s ease; cursor: pointer;",
        tags$i(class = "fas fa-university", style = "font-size: 1.6rem; color: #1d4ed8; margin-bottom: 2px;"),
        span("Sobre", style = "font-family: 'Outfit', sans-serif; font-weight: 700; font-size: 0.76rem; line-height: 1.05;")
      ),
      # Linha divisória vertical
      div(
        style = "width: 1px; height: 42px; background-color: rgba(13, 110, 253, 0.25); margin-left: 2px; margin-right: 2px;"
      ),
      # Logo do CatalyseR (nova identidade "CatalyseR Studio")
      tags$img(
        src = "catalyser_logo2.png",
        height = "76px", # Ajustado para a nova altura de 90px
        style = "border-radius: 6px; box-shadow: 0 1px 3px rgba(0,0,0,0.15); object-fit: contain;"
      )
    )
  )
)
# Servidor (Server)
server <- function(input, output, session) {

  # Reinicializa o selectize do dataset ao mostrar o painel do pacote — sem isto,
  # o selectize nasce dentro de um conditionalPanel oculto e a BUSCA (digitar
  # parte do nome) não funciona. Ao reabrir visível, a busca passa a valer.
  observeEvent(input$data_source, {
    if (identical(input$data_source, "package")) {
      sel <- if (!is.null(input$package_dataset) && nzchar(input$package_dataset)) input$package_dataset else eapa_dataset_default
      updateSelectizeInput(session, "package_dataset",
        choices = eapa_datasets, selected = sel,
        options = list(placeholder = "Digite ou escolha um conjunto de dados...",
                       openOnFocus = TRUE, maxOptions = 100),
        server = FALSE)
    }
  }, ignoreInit = TRUE)


  # ==========================================
  # 1. SERVIÇOS DO MÓDULO DE AJUDA DE USO
  # ==========================================
  
  help_topics <- list(
    flow = list(
      title = "1. Fluxo de Trabalho",
      keywords = "ide_r catalyst andaimes visuais paradigma ensino aprendizagem fluxo trabalho",
      content = HTML("
        <h4 class='text-primary' style='font-family: \"Outfit\", sans-serif; font-weight: 700;'>Fluxo de Trabalho Recomendado</h4>
        <p>O CatalyseR guia você através de uma jornada visual para a realização de análises estatísticas reprodutíveis. Para tirar o máximo proveito da IDE, siga este fluxo:</p>
        <ol>
          <li><b>Importação de Dados:</b> Carregue e prepare seus dados no menu <b>Preparando Dados</b>. Certifique-se de ajustar a tipagem das colunas se necessário.</li>
          <li><b>Análise Exploratória e Modelagem:</b> Acesse os menus de análise (como <i>Estatística Descritiva</i>, <i>Histogramas</i>, <i>Boxplot</i> ou <i>Regressão Linear</i>) e defina suas variáveis e opções estéticas.</li>
          <li><b>Exportação Consolidada:</b> Volte ao painel de importação de dados e use a seção <i>Exportar Projeto Consolidado</i> para gerar o pacote ZIP com todos os seus scripts e relatórios.</li>
        </ol>
      ")
    ),
    import = list(
      title = "2. Importação e Tipagem",
      keywords = "carregar ler csv excel xlsx eapadados converter tipo coluna numeric factor character date tipagem",
      content = HTML("
        <h4 class='text-primary' style='font-family: \"Outfit\", sans-serif; font-weight: 700;'>Importação e Preparação de Dados</h4>
        <p>Para iniciar suas análises, o primeiro passo é carregar um conjunto de dados. A IDE suporta:</p>
        <ul>
          <li><b>Arquivos Locais:</b> Planilhas Excel (.xlsx, .xls) ou arquivos de texto delimitados (.csv). Você pode escolher a aba específica no Excel ou configurar o cabeçalho e separadores decimais no CSV.</li>
          <li><b>Pacotes de Exemplo:</b> Datasets didáticos inclusos no pacote científico R 'EAPADados'.</li>
        </ul>
        <p><b>Tipagem de Variáveis:</b> Na barra lateral esquerda (após carregar o arquivo), você verá uma tabela listando cada coluna e seu tipo de dado atual. Você pode alterar a tipagem de forma interativa (ex: converter para <i>numeric</i>, <i>factor</i>, <i>character</i> ou <i>Date</i>) para garantir a consistência das suas análises.</p>
        <p><b>Filtrar Dados (coluna <i>Filtro</i>):</b> Cada coluna ganha um botão <b>Filtrar</b> adequado ao seu tipo. Para <i>factor</i>, abre uma janela com os níveis da variável (ex.: <i>Macho</i>/<i>Fêmea</i>); ao desmarcar um nível, as linhas correspondentes são removidas. Para <i>numeric</i>/<i>integer</i>, abre um controle deslizante de faixa (mínimo–máximo); apenas os valores dentro do intervalo escolhido são mantidos. Em ambos os casos o efeito é em tempo real e vale para <b>todas</b> as análises. O botão sinaliza quando há filtro ativo (ex.: <i>1/2</i> para níveis, <i>Faixa</i> para intervalos), e todos os filtros são reiniciados ao carregar um novo conjunto de dados.</p>
      ")
    ),
    descr = list(
      title = "3. Estatística Descritiva",
      keywords = "média mediana desvio padrão variância resumo tabela descrever dados estatistica descritiva",
      content = HTML("
        <h4 class='text-primary' style='font-family: \"Outfit\", sans-serif; font-weight: 700;'>Estatística Descritiva</h4>
        <p>Disponível no menu <b>Descrevendo Dados > Estatística Descritiva</b>, este módulo calcula de forma automática medidas de tendência central e dispersão.</p>
        <p><b>Principais Recursos:</b></p>
        <ul>
          <li><b>Escolha de Variáveis:</b> Selecione múltiplas variáveis numéricas simultaneamente.</li>
          <li><b>Agrupamento por Fator:</b> Escolha uma variável categórica para calcular as estatísticas separadas por cada nível ou subgrupo.</li>
          <li><b>Métricas Customizadas:</b> Ative ou desative medidas como tamanho amostral (N), valores faltantes (NAs), média, mediana, desvio padrão, variância e quartis diretamente no painel de exibição.</li>
        </ul>
      ")
    ),
    hist = list(
      title = "4. Histogramas",
      keywords = "histograma frequência densidade distribuição bins classes cor tema grafico",
      content = HTML("
        <h4 class='text-primary' style='font-family: \"Outfit\", sans-serif; font-weight: 700;'>Gráficos de Histograma</h4>
        <p>Disponível no menu <b>Descrevendo Dados > Histogramas</b>, o histograma é uma ferramenta essencial para analisar a forma da distribuição de variáveis quantitativas (simetria, assimetria, curtose).</p>
        <p><b>Customizações Disponíveis:</b></p>
        <ul>
          <li><b>Classes (Bins):</b> Ajuste o número de barras para melhorar o detalhamento do gráfico.</li>
          <li><b>Densidade:</b> Adicione a estimativa de densidade Kernel sobre o gráfico.</li>
          <li><b>Estética e Temas:</b> Customize eixos, títulos e aplique temas visuais profissionais (ex: Mínimo, Clássico, Preto e Branco).</li>
          <li><b>Legendas por Grupo:</b> Se selecionar uma variável de agrupamento, o histograma mapeia automaticamente cores separadas por categoria.</li>
        </ul>
      ")
    ),
    boxplot = list(
      title = "5. Boxplot (Diagrama de Caixa)",
      keywords = "boxplot caixa outliers dispersão jitter pontos agrupar cor preenchimento",
      content = HTML("
        <h4 class='text-primary' style='font-family: \"Outfit\", sans-serif; font-weight: 700;'>Diagrama de Caixa (Boxplot)</h4>
        <p>Disponível no menu <b>Descrevendo Dados > Boxplot</b>, o boxplot permite visualizar a distribuição de dados e detectar outliers (valores atípicos) através de quartis.</p>
        <p><b>Destaques do CatalyseR:</b></p>
        <ul>
          <li><b>Pontos Individuais (Jitter):</b> Superponha os dados reais ao boxplot de forma dispersa para melhor visualização da densidade de pontos.</li>
          <li><b>Agrupamento Avançado:</b> Além de separar os dados no eixo X por uma variável categórica, você pode aplicar uma segunda variável de agrupamento mapeando separadamente cores de contorno ou de preenchimento.</li>
        </ul>
      ")
    ),
    reg = list(
      title = "6. Regressão Linear",
      keywords = "regressão linear reta ajuste lm resíduos diagnóstico normalidade q-q modelos regressao",
      content = HTML("
        <h4 class='text-primary' style='font-family: \"Outfit\", sans-serif; font-weight: 700;'>Modelos de Regressão Linear Simples</h4>
        <p>Disponível no menu <b>Modelos de Regressão > Regressão Linear Simples</b>, este módulo permite ajustar um modelo para avaliar a influência de uma variável preditora (X) sobre uma variável resposta (Y).</p>
        <p><b>O que é gerado na tela e nos scripts:</b></p>
        <ul>
          <li><b>Equação do Modelo:</b> Reta calculada exibida dinamicamente no título.</li>
          <li><b>Coeficientes e Ajuste Global:</b> Tabelas interativas de coeficientes (estimativa, erro padrão, valor t, p-valor) e métricas globais (R-quadrado, erro padrão dos resíduos).</li>
          <li><b>Diagnóstico Gráfico:</b> Gráficos de Resíduos vs Ajustados e Normal Q-Q Plot integrados, fundamentais para a validação dos pressupostos do modelo estatístico.</li>
        </ul>
      ")
    ),
    export = list(
      title = "7. Exportação de Projetos",
      keywords = "exportar zip consolidado nome dataset projeto scripts rproj quarto qmd download individual unica",
      content = HTML("
        <h4 class='text-primary' style='font-family: \"Outfit\", sans-serif; font-weight: 700;'>Tipos de Exportação de Projetos</h4>
        <p>Na IDE CatalyseR, você encontrará dois tipos de exportadores de código, com objetivos didáticos distintos:</p>
        
        <h5 class='text-primary' style='font-family: \"Outfit\", sans-serif; font-weight: 600; margin-top: 15px;'>1. Exportação Consolidada (No menu \"Preparando Dados\")</h5>
        <p>Ideal para quando você conclui sua sessão de estudos ou para a entrega de relatórios e trabalhos práticos completos. Ela une todas as análises realizadas:</p>
        <ul>
          <li><b>Nome do Projeto Baseado no Dataset:</b> O arquivo compactado (.zip) gerado recebe automaticamente o nome do conjunto de dados que está ativo na sua sessão (ex: <i>projeto_artemia_2026-06-09.zip</i>).</li>
          <li><b>Rastreamento de Abas:</b> Apenas as análises que você visitou e trabalhou na sessão atual estarão habilitadas para seleção na lista. As demais ficam bloqueadas.</li>
          <li><b>Multidisciplinar:</b> Gera scripts R numerados sequencialmente na pasta <i>scripts/</i> (ex: <i>1_estatistica_descritiva.R</i>, <i>2_regressao_linear.R</i>) e cria um único <b>relatório Quarto (.qmd) consolidado</b> contendo todas as análises combinadas no mesmo documento.</li>
        </ul>

        <h5 class='text-primary' style='font-family: \"Outfit\", sans-serif; font-weight: 600; margin-top: 15px;'>2. Exportação de Análise Única (Dentro de cada aba/análise específica)</h5>
        <p>Perfeita para quando você deseja focar exclusivamente no estudo isolado de uma única técnica estatística (ex: apenas o boxplot ou apenas a regressão linear), sem carregar códigos de outras análises:</p>
        <ul>
          <li><b>Foco Total:</b> Gera um pacote contendo apenas o script específico daquela análise (ex: <i>boxplot.R</i> ou <i>regressao.R</i>) e um relatório Quarto exclusivo daquele gráfico ou modelo.</li>
          <li><b>Facilidade de Estudo:</b> Excelente para entender a lógica e depurar os andaimes visuais de código passo a passo, sem misturar múltiplos tópicos de estudo.</li>
        </ul>
        <p>Ambos os formatos geram a pasta <i>dados/</i> com o seu dataset limpo/tipado e o arquivo <i>projeto_analise.Rproj</i> para abrir todo o ambiente de forma automática localmente no RStudio.</p>
      ")
    ),
    packages = list(
      title = "8. Pacotes R & Tidyverse",
      keywords = "pacotes tidyverse read_csv instalar ggplot2 readr library install.packages programacao r writexl readxl",
      content = HTML("
        <h4 class='text-primary' style='font-family: \"Outfit\", sans-serif; font-weight: 700;'>Pacotes R e o Ecossistema Tidyverse</h4>
        <p>A programação moderna em R baseia-se amplamente no <b>Tidyverse</b>, uma coleção de pacotes projetados para ciência de dados. Nossos scripts utilizam ferramentas desse ecossistema para facilitar e padronizar seu aprendizado.</p>
        
        <h5 class='text-primary' style='font-family: \"Outfit\", sans-serif; font-weight: 600; margin-top: 15px;'>Como Instalar os Pacotes Necessários</h5>
        <p>Para executar os scripts e compilar os relatórios localmente no RStudio, certifique-se de que os pacotes necessários estejam instalados. Execute os comandos abaixo no Console do RStudio:</p>
        <pre><code class='language-r'># Instalação do Tidyverse completo (inclui readxl, ggplot2, etc.)
install.packages(\"tidyverse\")

# Pacotes adicionais necessários para manipulação de planilhas e relatórios:
install.packages(\"readxl\")
install.packages(\"writexl\")
install.packages(\"flextable\")
install.packages(\"officer\")
install.packages(\"knitr\")</code></pre>

        <h5 class='text-primary' style='font-family: \"Outfit\", sans-serif; font-weight: 600; margin-top: 15px;'>Principais Pacotes Utilizados no Projeto</h5>
        <ul>
          <li><b>readxl:</b> Utilizado para carregar planilhas eletrônicas do Excel (.xlsx) para o ambiente de análise.</li>
          <li><b>writexl:</b> Utilizado para exportar planilhas Excel organizadas de forma limpa, permitindo criar metadados (como o Dicionário de Variáveis).</li>
          <li><b>ggplot2:</b> O padrão para criação de gráficos estatísticos e científicos de alta qualidade.</li>
          <li><b>knitr, flextable e officer:</b> Pacotes utilizados na formatação de tabelas e geração automatizada de relatórios em Word (.docx).</li>
        </ul>
      ")
    ),
    formats = list(
      title = "9. Formatos de Dados (CSV, XLSX, RDA)",
      keywords = "csv xlsx rda formato dados extensao diferença importar exportar dicionario",
      content = HTML("
        <h4 class='text-primary' style='font-family: \"Outfit\", sans-serif; font-weight: 700;'>Formatos de Dados Exportados</h4>
        <p>Para que você compreenda o fluxo de trabalho real de um pesquisador, a IDE exporta seus dados limpos e tipados em três formatos diferentes dentro da pasta <code>dados/</code>. Cada um possui vantagens e propósitos específicos:</p>
        
        <h5 class='text-primary' style='font-family: \"Outfit\", sans-serif; font-weight: 600; margin-top: 15px;'>1. Planilha Excel (.xlsx)</h5>
        <ul>
          <li><b>O que é:</b> É o formato mais popular no meio científico para visualização e edição manual de dados.</li>
          <li><b>Como é gerado:</b> A IDE cria duas abas (sheets) neste arquivo:
            <ul>
              <li><b>Dados:</b> Os dados propriamente ditos no formato <i>tidy</i> (limpos, onde cada linha é uma observação e cada coluna é uma variável).</li>
              <li><b>Dicionario_Variaveis:</b> Um dicionário de metadados listando todas as variáveis, seus tipos de dados e explicações sobre o significado ecológico ou experimental de cada coluna.</li>
            </ul>
          </li>
          <li><b>Uso no R:</b> Carregado usando a biblioteca <code>readxl</code>: <code>readxl::read_excel('dados/dados_limpos.xlsx', sheet = 'Dados')</code>.</li>
        </ul>

        <h5 class='text-primary' style='font-family: \"Outfit\", sans-serif; font-weight: 600; margin-top: 15px;'>2. Arquivo CSV (.csv)</h5>
        <ul>
          <li><b>O que é:</b> Um formato universal, aberto e em texto simples (Comma-Separated Values). É lido por praticamente qualquer ferramenta de banco de dados ou linguagem de programação no mundo.</li>
          <li><b>Uso no R:</b> Carregado nativamente com <code>read.csv()</code> ou via Tidyverse com <code>readr::read_csv()</code>.</li>
        </ul>

        <h5 class='text-primary' style='font-family: \"Outfit\", sans-serif; font-weight: 600; margin-top: 15px;'>3. Arquivo RData (.rda)</h5>
        <ul>
          <li><b>O que é:</b> Formato nativo e binário do R. Salva o objeto exatamente como ele estava na memória da IDE. Preserva todas as tipagens finas que outros formatos perdem, como a ordenação interna de fatores categóricos (sexo, tratamentos) e formatos de data.</li>
          <li><b>Uso no R:</b> Carregado instantaneamente com o comando <code>load('dados/dados_limpos.rda')</code>.</li>
        </ul>
      ")
    )
  )
  
  selected_help_topic <- reactiveVal("flow")
  
  filtered_help_topics <- reactive({
    query <- trimws(tolower(input$help_search))
    if (is.null(query) || !nzchar(query)) {
      return(help_topics)
    }
    
    matches <- sapply(help_topics, function(topic) {
      grepl(query, tolower(topic$title)) || grepl(query, tolower(topic$keywords))
    })
    
    help_topics[matches]
  })
  
  observe({
    topics <- filtered_help_topics()
    req(length(topics) > 0)
    current <- selected_help_topic()
    if (!(current %in% names(topics))) {
      selected_help_topic(names(topics)[1])
    }
  })
  
  observe({
    lapply(names(help_topics), function(id) {
      observeEvent(input[[paste0("help_btn_", id)]], {
        selected_help_topic(id)
      })
    })
  })
  
  output$help_topics_list_ui <- renderUI({
    topics <- filtered_help_topics()
    if (length(topics) == 0) {
      return(p("Nenhum tópico encontrado.", style = "color: #dc3545; font-style: italic; margin-top: 10px; font-size: 0.85rem;"))
    }
    
    current <- selected_help_topic()
    
    tags$div(
      class = "list-group help-list-group",
      style = "margin-top: 10px; border-radius: 8px; overflow: hidden;",
      lapply(names(topics), function(id) {
        is_active <- (id == current)
        actionButton(
          inputId = paste0("help_btn_", id),
          label = topics[[id]]$title,
          class = paste0("list-group-item list-group-item-action", if (is_active) " active" else ""),
          style = paste0(
            "text-align: left; border: 1px solid rgba(0,0,0,0.08); font-size: 0.85rem; font-weight: 500; padding: 10px 12px;",
            if (is_active) " background-color: #0d6efd; color: white;" else " background-color: white; color: #495057;"
          )
        )
      })
    )
  })
  
  output$help_content_display <- renderUI({
    topic_id <- selected_help_topic()
    req(topic_id %in% names(help_topics))
    
    topic <- help_topics[[topic_id]]
    
    card(
      card_header(
        topic$title, 
        style = "background-color: rgba(13, 110, 253, 0.05); color: #0d6efd; font-weight: 700; font-family: 'Outfit', sans-serif; font-size: 1.1rem; padding: 12px 15px;"
      ),
      card_body(
        style = "padding: 22px; min-height: 400px; line-height: 1.6;",
        topic$content
      )
    )
  })
  
  # ==========================================
  # 2. SERVIÇOS DO MÓDULO "SOBRE A IDE"
  # ==========================================
  
  about_topics <- list(
    intro = list(
      title = "1. Introdução e Filosofia",
      keywords = "ide_r catalyst barreira sintaxe frustracao graducao posgraduacao ufpa catalyser estresse origem nome trocadilho",
      content = HTML("
        <h4 class='text-primary' style='font-family: \"Outfit\", sans-serif; font-weight: 700;'>A Barreira da Sintaxe e o Estresse no Aprendizado</h4>
        <p>O ensino de Estatística Aplicada para cursos de graduação e pós-graduação que não pertencem à área da computação (como Ciências Biológicas, Agronomia, Ciências da Saúde) enfrenta um dilema clássico. O aprendizado da linguagem de programação frequentemente cria uma barreira de entrada muito alta devido a erros de digitação de sintaxe (como parênteses esquecidos, aspas erradas ou caminhos locais incorretos), fazendo com que alunos se frustrem e desistam antes mesmo de conseguirem interpretar os resultados estatísticos.</p>
        <p>A <b>IDE CatalyseR</b>, desenvolvida na <b>UFPA (Universidade Federal do Pará)</b>, rompe essa barreira ao colocar o estudante no controle conceitual. Criada especificamente para catalisar o aprendizado e as análises com R, ela visa reduzir drasticamente o estresse e a frustração dos estudantes ao utilizar a metodologia pedagógica de <i>Andaimes Visuais</i> e <i>Engenharia Reversa</i>.</p>
        
        <h5 class='text-primary' style='font-family: \"Outfit\", sans-serif; font-weight: 600; margin-top: 20px;'>A Origem do Nome \"CatalyseR\"</h5>
        <p>O nome é um neologismo científico e um trocadilho linguístico rico:</p>
        <ul>
          <li><b>Origem Francesa:</b> Deriva do verbo francês <i>catalyser</i> (catalisar) e de <i>analyser</i> (analisar). Reflete a fusão perfeita de acelerar reações (aprendizado) com o exame minucioso de dados (análise).</li>
          <li><b>O \"R\" Capitalizado:</b> O sufixo <b>R</b> capitalizado ao final denota o foco na linguagem de programação R. Portanto, <b>CatalyseR</b> significa literalmente: <i>\"catalisar e analisar por meio do R\"</i>.</li>
          <li><b>Redução da Energia de Ativação (Estresse):</b> Em termos químicos, um catalisador diminui a energia de ativação necessária para iniciar uma reação. Da mesma forma, a IDE CatalyseR diminui a \"energia de ativação cognitiva\" (o estresse inicial de lidar com códigos e erros de sintaxe), permitindo que os estudantes saltem direto para a análise científica produtiva.</li>
        </ul>

        <div style='text-align: center; margin-top: 25px; border-top: 1px solid #dee2e6; padding-top: 15px;'>
          <span style='font-size: 0.9rem; color: #6c757d; font-weight: 600; margin-right: 12px; vertical-align: middle;'>Desenvolvido com:</span>
          <a href='https://shiny.posit.co/' target='_blank' style='text-decoration: none;'>
            <img src='https://shiny.posit.co/images/shiny-logo.png' height='42px' style='vertical-align: middle; opacity: 0.95;'
                 onerror='this.onerror=null; this.src=\"https://raw.githubusercontent.com/rstudio/shiny/main/man/figures/logo.png\";'>
          </a>
        </div>
      ")
    ),
    scaffolding = list(
      title = "2. Andaimes Visuais",
      keywords = "paradigma visual scaffolding passos exploracao visual engenharia reversa",
      content = HTML("
        <h4 class='text-primary' style='font-family: \"Outfit\", sans-serif; font-weight: 700;'>O Paradigma dos Andaimes Visuais</h4>
        <p>O modelo pedagógico da IDE CatalyseR baseia-se em 3 passos essenciais:</p>
        <ol>
          <li><b>Passo 1: Exploração Visual.</b> O estudante carrega os dados e ajusta modelos estatísticos e gráficos de forma imediata na tela, validando suas hipóteses visualmente e sem erros de código.</li>
          <li><b>Passo 2: Geração de Código Limpo.</b> A IDE gera de forma automática e transparente os scripts correspondentes a cada decisão tomada em tela.</li>
          <li><b>Passo 3: Engenharia Reversa e Consolidação.</b> O estudante baixa o projeto consolidado (.zip) e abre no RStudio local, executando linha a linha os scripts estruturados e modificando os códigos já prontos para observar os resultados correspondentes.</li>
        </ol>
      ")
    ),
    revolutionary = list(
      title = "3. Por que a IDE CatalyseR?",
      keywords = "revolucionaria tradicional comparacao paradigma tabela ufpa estresse",
      content = HTML("
        <h4 class='text-primary' style='font-family: \"Outfit\", sans-serif; font-weight: 700;'>Por que essa abordagem é Revolucionária?</h4>
        <p>Veja como o paradigma dos Andaimes Visuais da IDE CatalyseR inverte a frustração e diminui o estresse do aprendizado de R:</p>
        <table class='table table-striped table-bordered table-sm' style='margin-top: 15px; font-size: 0.9rem;'>
          <thead>
            <tr>
              <th>Aspecto</th>
              <th>Abordagem Tradicional</th>
              <th>O Paradigma CatalyseR (UFPA)</th>
            </tr>
          </thead>
          <tbody>
            <tr>
              <td><b>Ponto de Partida</b></td>
              <td>Tela preta/prompt vazio.</td>
              <td>Exploração visual com geração de código correspondente.</td>
            </tr>
            <tr>
              <td><b>Lidando com Erros</b></td>
              <td>Foco em depurar sintaxe e erros de digitação.</td>
              <td>Código garantido livre de erros, pronto para rodar.</td>
            </tr>
            <tr>
              <td><b>Nível de Estresse</b></td>
              <td>Alto (sensação de impotência frente a erros de código).</td>
              <td>Baixo (foco na lógica científica e interpretação de dados).</td>
            </tr>
            <tr>
              <td><b>Estrutura de Trabalho</b></td>
              <td>Arquivos desorganizados e soltos.</td>
              <td>Estrutura profissional de pastas (dados/, scripts/, relatorios/).</td>
            </tr>
          </tbody>
        </table>
      ")
    ),
    structure = list(
      title = "4. Estrutura do Pacote",
      keywords = "estrutura arquivos rproj rda csv script qmd quarto rstudio",
      content = HTML("
        <h4 class='text-primary' style='font-family: \"Outfit\", sans-serif; font-weight: 700;'>Estrutura de Cada Componente Gerado</h4>
        <p>Os projetos baixados na IDE CatalyseR possuem uma estrutura padronizada e profissional:</p>
        <ul>
          <li><b>projeto_analise.Rproj:</b> Dando duplo clique neste arquivo, o RStudio abre automaticamente configurado com a pasta de trabalho correta, dispensando comandos complicados como <i>setwd()</i>.</li>
          <li><b>dados/dados_limpos.rda e csv:</b> Arquivos de dados limpos gerados nativamente pela IDE, garantindo formatação correta de decimais e evitando problemas de importação.</li>
          <li><b>scripts/descrever.R / regressao.R:</b> Scripts R com sintaxe limpa e amplamente comentada para reproduzir localmente os resultados da tela.</li>
          <li><b>relatorios/relatorio.qmd:</b> Relatório estruturado em Quarto Markdown (.qmd) pronto para gerar apresentações em HTML, Word ou PDF com um clique.</li>
        </ul>
      ")
    ),
    deployment = list(
      title = "5. Como Rodar e Implantar",
      keywords = "rodar executar implantar rstudio pacote instalar shiny server nuvem deploy local",
      content = HTML("
        <h4 class='text-primary' style='font-family: \"Outfit\", sans-serif; font-weight: 700;'>Como Rodar e Implantar o CatalyseR</h4>
        <p>Como uma ferramenta pedagógica, existem três formas principais de disponibilizar o CatalyseR para estudantes e pesquisadores, cada uma atendendo a um nível de maturidade técnica:</p>
        
        <h5 class='text-primary' style='font-family: \"Outfit\", sans-serif; font-weight: 600; margin-top: 15px;'>1. Nuvem/Servidor Web (A mais elegante e sem barreiras)</h5>
        <p>A melhor abordagem de todas é hospedar o aplicativo em um servidor web (como <b>Shiny Server</b>, <b>Shinyapps.io</b> ou <b>Posit Connect</b>).</p>
        <ul>
          <li><b>Como funciona:</b> A instituição de ensino hospeda a IDE em seu servidor. Os alunos acessam diretamente pelo navegador através de um link (ex: <i>https://r-catalyst.suauniversidade.edu</i>).</li>
          <li><b>Vantagem pedagógica:</b> <b>Zero instalação inicial.</b> O estudante não precisa ter R ou RStudio instalado no primeiro dia de aula. Ele carrega seus dados e explora conceitos estatísticos de imediato. Após validar suas análises visualmente, baixa o arquivo ZIP e inicia o estudo local no RStudio.</li>
        </ul>

        <h5 class='text-primary' style='font-family: \"Outfit\", sans-serif; font-weight: 600; margin-top: 15px;'>2. Empacotamento em Pacote R (A mais portátil e offline)</h5>
        <p>O aplicativo pode ser empacotado como uma biblioteca oficial de R (por exemplo, criando um pacote chamado <code>RCatalyst</code>) e disponibilizado no GitHub.</p>
        <ul>
          <li><b>Como funciona:</b> Os arquivos da IDE são estruturados como uma biblioteca de R. O usuário instala o pacote com o comando:
            <pre style='background: #f1f3f5; padding: 8px; border-radius: 6px; font-size: 0.85rem;'>remotes::install_github(\"usuario/RCatalyst\")
RCatalyst::run_ide()</pre>
          </li>
          <li><b>Vantagem pedagógica:</b> Serve como um excelente passo intermediário de transição, onde o aluno executa um comando simples para abrir a interface em sua própria máquina, offline, familiarizando-se com o terminal do RStudio.</li>
        </ul>

        <h5 class='text-primary' style='font-family: \"Outfit\", sans-serif; font-weight: 600; margin-top: 15px;'>3. Execução Direta via RStudio (A melhor para desenvolvimento)</h5>
        <p>Baixar a pasta do projeto (ou clonar via Git) e abrir diretamente no RStudio local.</p>
        <ul>
          <li><b>Como funciona:</b> O usuário abre a pasta no RStudio, abre o arquivo <code>app.R</code> e clica no botão <b>\"Run App\"</b> no canto superior direito do editor, ou executa no console:
            <pre style='background: #f1f3f5; padding: 8px; border-radius: 6px; font-size: 0.85rem;'>shiny::runApp()</pre>
          </li>
          <li><b>Vantagem pedagógica:</b> Ideal para professores ou alunos interessados em entender a arquitetura do próprio Shiny, permitindo que eles editem o código e customizem novos módulos ou estilos para o CatalyseR.</li>
        </ul>

        <hr>
        <p><b>Recomendação Pedagógica:</b> A abordagem híbrida é a mais elegante. Disponibilize o <b>CatalyseR na nuvem (Opção 1)</b> para as aulas teóricas e práticas iniciais. À medida que os alunos ganham autonomia executando localmente os códigos do ZIP exportado, incentive-os a instalar a ferramenta em suas próprias máquinas usando as <b>Opções 2 ou 3</b>.</p>
      ")
    ),
    pedagogical_cycle = list(
      title = "6. O Ciclo Científico e os Três Tripés",
      keywords = "ciclo cientifico tripe pedagogico tripes planejamento experimental amostral script comunicacao artigo relatorio quarto qmd",
      content = HTML("
        <h4 class='text-primary' style='font-family: \"Outfit\", sans-serif; font-weight: 700;'>O Ciclo Científico e os Três Tripés Pedagógicos</h4>
        <p>O CatalyseR foi concebido não apenas como uma ferramenta para gerar códigos de forma automática, mas como um elemento de integração para um ciclo completo de aprendizado científico, estruturado sobre <b>três pilares essenciais (os três tripés)</b>:</p>
        
        <ol style='line-height: 1.6; margin-top: 15px;'>
          <li><b>Pilar 1: Obtenção Rigorosa de Dados (Planejamento)</b>
            <br>Toda pesquisa de qualidade começa antes da análise de dados propriamente dita. O estudante deve desenhar e executar um rigoroso <b>Planejamento de Coleta Amostral</b> ou um sólido <b>Planejamento Experimental</b>. Esta etapa de coleta estruturada constitui a base metodológica de seu artigo ou relatório de pesquisa.
          </li>
          <li style='margin-top: 10px;'><b>Pilar 2: Análise Estatística Rigorosa com Scripts R (Execução)</b>
            <br>Utilizando a IDE, o estudante realiza explorações visuais interativas, obtendo como resultado um projeto R completo contendo scripts R limpos e comentados. O aluno abre esse projeto localmente no RStudio e executa os scripts linha a linha, testando os pressupostos do modelo estatístico (ex: normalidade, resíduos, homocedasticidade) de forma rigorosa e reprodutível.
          </li>
          <li style='margin-top: 10px;'><b>Pilar 3: Comunicação Científica de Resultados (Divulgação)</b>
            <br>A ciência só está completa quando comunicada de forma clara. Utilizando o arquivo <b>Quarto Markdown (.qmd) consolidado</b> gerado pela IDE, o estudante produz um <b>Relatório Científico Final</b> elegante no formato de um artigo, contendo a contextualização, a metodologia de amostragem/experimento, o código R com a análise de dados e a interpretação científica dos principais resultados.
          </li>
        </ol>
        
        <h5 class='text-primary' style='font-family: \"Outfit\", sans-serif; font-weight: 600; margin-top: 20px;'>Fechando o Ciclo Pedagógico e Científico</h5>
        <p>A produção do <b>Projeto R</b> exportado integra-se de forma direta com a <b>Comunicação de Resultados</b>. Ao abrir o arquivo <code>relatorio_consolidado.qmd</code> no RStudio local, o estudante é encorajado a preencher as seções de Metodologia (detalhando o Planejamento Amostral/Experimental do Pilar 1) e Introdução ao lado das saídas automáticas do Pilar 2. Isso simula com fidelidade o fluxo real de redação de um artigo científico em revistas de alto impacto, consolidando a ponte entre o planejamento rigoroso, a análise de dados e a comunicação acadêmica.</p>
      ")
    ),
    t_dist_visual = list(
      title = "7. Visualização de Regiões Críticas",
      keywords = "distribuicao t student regiao critica rejeicao t calculado critico p-valor hipóteses didatico plus curva caudas",
      content = HTML("
        <h4 class='text-primary' style='font-family: \"Outfit\", sans-serif; font-weight: 700;'>O Plus Didático: Curva de Distribuição Teórica Interativa</h4>
        <p>No ensino clássico de bioestatística e testes de hipóteses, um dos maiores desafios pedagógicos é a abstraction da tomada de decisão. Alunos frequentemente decoram regras como <i>\"p-valor menor que alfa rejeita H0\"</i> ou <i>\"t calculado maior que t tabelado rejeita H0\"</i>, mas sem compreender o significado geométrico e probabilístico dessas relações.</p>
        <p>Para preencher essa lacuna, o CatalyseR introduz uma ferramenta visual avançada: a <b>Visualização Gráfica da Distribuição t de Student Teórica</b>, permitindo o confronto imediato entre a teoria probabilística e os dados observados.</p>

        <h5 class='text-primary' style='font-family: \"Outfit\", sans-serif; font-weight: 600; margin-top: 15px;'>1. Objetivos Pedagógicos</h5>
        <ul>
          <li><b>Geometrizar a Decisão Estatística:</b> O aluno observa fisicamente onde a estatística amostral está localizada na curva de distribuição de probabilidade sob a hipótese nula.</li>
          <li><b>Compreensão Visual de Hipóteses Unilaterais e Bilaterais:</b> A área sombreada se ajusta instantaneamente caso o teste seja bilateral (duas caudas vermelhas de tamanho &alpha;/2) ou unilateral (uma cauda vermelha à esquerda ou à direita de tamanho &alpha;).</li>
          <li><b>Entendimento Intuitivo do p-valor:</b> Se a linha vertical azul (t experimental) cair na área vermelha (região de rejeição), fica evidente que a probabilidade de obter um valor tão extremo ou mais extremo sob a hipótese nula é menor que a significância adotada (&alpha;).</li>
        </ul>

        <h5 class='text-primary' style='font-family: \"Outfit\", sans-serif; font-weight: 600; margin-top: 15px;'>2. Como Foi Desenvolvido (Lógica Técnica no R)</h5>
        <p>Em vez de depender de pacotes externos complexos (como o <i>vistributions</i>, que pode exigir compilação ou falhar em ambientes restritos), a visualização foi programada utilizando a biblioteca principal <code>ggplot2</code> combinada com funções internas de probabilidade do R:</p>
        <ul>
          <li><b>Cálculo da Densidade:</b> A curva de densidade t é calculada dinamicamente com base nos graus de liberdade (df) do teste através da função de densidade <code>dt(x, df)</code>.</li>
          <li><b>Determinação dos Limites Críticos:</b> Os pontos de corte exatos que definem a fronteira da região de rejeição (t<sub>crit</sub>) são encontrados de forma dinâmica por meio da função de quantil <code>qt(p, df)</code>, onde p depende do nível de significância &alpha; e do sentido da hipótese alternativa.</li>
          <li><b>Sombreamento da Região de Rejeição:</b> A área crítica sob a curva é preenchida em vermelho translúcido utilizando a função <code>geom_area()</code> aplicada a subconjuntos filtrados de dados (ex: x &le; t<sub>crit</sub> ou x &ge; t<sub>crit</sub>).</li>
          <li><b>Plote do t Experimental (Amostral):</b> Uma linha azul sólida espessa (<code>geom_vline()</code>) marca o valor exato do t calculado, identificada com uma etiqueta branca (<code>annotate(\"label\")</code>) contendo o valor exato da estatística.</li>
          <li><b>Escalonamento Automático dos Eixos:</b> Para evitar que valores extremamente elevados de t calculado fiquem de fora do gráfico, a escala do eixo X é ajustada dinamicamente com base no valor de t<sub>calc</sub>: <code>max(4.5, abs(t_calc) + 1.5)</code>.</li>
        </ul>

        <h5 class='text-primary' style='font-family: \"Outfit\", sans-serif; font-weight: 600; margin-top: 15px;'>3. Integração com a Filosofia do CatalyseR</h5>
        <p>Mantendo o paradigma dos <i>Andaimes Visuais</i>, toda essa lógica de plotagem não é apenas exibida na tela. O código completo do ggplot2 que desenha a curva, calcula as caudas e adiciona as marcações críticas é <b>gerado automaticamente e exportado</b> como parte do script R do teste t. Isso permite que o aluno execute o código em seu próprio RStudio local, estude como manipular funções de densidade em gráficos e personalize a curva para seus próprios relatórios.</p>
      ")
    )
  )
  
  selected_about_topic <- reactiveVal("intro")
  
  filtered_about_topics <- reactive({
    query <- trimws(tolower(input$about_search))
    if (is.null(query) || !nzchar(query)) {
      return(about_topics)
    }
    
    matches <- sapply(about_topics, function(topic) {
      grepl(query, tolower(topic$title)) || grepl(query, tolower(topic$keywords))
    })
    
    about_topics[matches]
  })
  
  observe({
    topics <- filtered_about_topics()
    req(length(topics) > 0)
    current <- selected_about_topic()
    if (!(current %in% names(topics))) {
      selected_about_topic(names(topics)[1])
    }
  })
  
  observe({
    lapply(names(about_topics), function(id) {
      observeEvent(input[[paste0("about_btn_", id)]], {
        selected_about_topic(id)
      })
    })
  })
  
  output$about_topics_list_ui <- renderUI({
    topics <- filtered_about_topics()
    if (length(topics) == 0) {
      return(p("Nenhum tópico encontrado.", style = "color: #dc3545; font-style: italic; margin-top: 10px; font-size: 0.85rem;"))
    }
    
    current <- selected_about_topic()
    
    tags$div(
      class = "list-group about-list-group",
      style = "margin-top: 10px; border-radius: 8px; overflow: hidden;",
      lapply(names(topics), function(id) {
        is_active <- (id == current)
        actionButton(
          inputId = paste0("about_btn_", id),
          label = topics[[id]]$title,
          class = paste0("list-group-item list-group-item-action", if (is_active) " active" else ""),
          style = paste0(
            "text-align: left; border: 1px solid rgba(0,0,0,0.08); font-size: 0.85rem; font-weight: 500; padding: 10px 12px;",
            if (is_active) " background-color: #0d6efd; color: white;" else " background-color: white; color: #495057;"
          )
        )
      })
    )
  })
  
  output$about_content_display <- renderUI({
    topic_id <- selected_about_topic()
    req(topic_id %in% names(about_topics))
    
    topic <- about_topics[[topic_id]]
    
    card(
      card_header(
        topic$title, 
        style = "background-color: rgba(13, 110, 253, 0.05); color: #0d6efd; font-weight: 700; font-family: 'Outfit', sans-serif; font-size: 1.1rem; padding: 12px 15px;"
      ),
      card_body(
        style = "padding: 22px; min-height: 400px; line-height: 1.6;",
        topic$content
      )
    )
  })

  # Nome limpo do conjunto de dados para personalizar o download do arquivo ZIP
  clean_dataset_name <- reactive({
    if (input$data_source == "local") {
      if (!is.null(input$file_upload)) {
        base_name <- tools::file_path_sans_ext(input$file_upload$name)
        ext <- tolower(tools::file_ext(input$file_upload$name))
        if (ext %in% c("xlsx", "xls") && !is.null(input$excel_sheet)) {
          base_name <- paste0(base_name, "_", input$excel_sheet)
        }
        gsub("[^a-zA-Z0-9_]", "_", base_name)
      } else {
        "datasets_projetos_regressao"
      }
    } else {
      if (!is.null(input$package_dataset)) {
        input$package_dataset
      } else {
        "dados"
      }
    }
  })

  # Valores Reativos para carregar e converter o dataset
  raw_data <- reactiveVal(NULL)
  col_types_rv <- reactiveVal(list())
  selected_cols_rv <- reactiveVal(character(0))
  # Filtros de niveis para variaveis fator: lista nomeada col -> niveis mantidos.
  # Ausencia de entrada (ou todos os niveis) = sem filtro.
  level_filters_rv <- reactiveVal(list())
  # Filtros de faixa para variaveis numericas/inteiras: lista nomeada col -> c(min, max).
  # Ausencia de entrada (ou faixa completa) = sem filtro.
  range_filters_rv <- reactiveVal(list())
  # Mapeamento para recodificação/agrupamento de fatores
  col_recodes_rv <- reactiveVal(list())
  # Renomeação opcional de colunas: vetor nomeado nome_antigo -> nome_novo.
  # Vazio por padrão => sem efeito (comportamento idêntico ao anterior).
  col_renames_rv <- reactiveVal(character(0))
  # Ordem das colunas exibidas no modal de renomeação (para ler os inputs).
  rn_import_cols <- reactiveVal(character(0))
  # Registro de observadores criados dinamicamente (evita duplicatas no re-render).
  lvl_obs_registry <- new.env()

  # Quando raw_data muda, inicializamos os tipos de coluna
  observeEvent(raw_data(), {
    df <- raw_data()
    # Reinicia os filtros (niveis e faixas) e agrupamentos a cada novo dataset.
    level_filters_rv(list())
    range_filters_rv(list())
    col_recodes_rv(list())
    if (!is.null(df)) {
      initial_types <- lapply(df, detect_col_type)
      col_types_rv(initial_types)
      selected_cols_rv(names(df))
    } else {
      col_types_rv(list())
      selected_cols_rv(character(0))
    }
  })
  
  # Observador para abrir o modal de seleção de variáveis
  observeEvent(input$select_vars_btn, {
    df <- raw_data()
    req(df)
    all_cols <- names(df)
    selected_cols <- selected_cols_rv()
    if (length(selected_cols) == 0) {
      selected_cols <- all_cols
    }
    
    showModal(modalDialog(
      title = "Selecionar Variáveis do Conjunto",
      tags$p(
        style = "font-size: 0.85rem; color: #6c757d;",
        "Marque as variáveis que deseja manter no conjunto de dados. As desmarcadas serão ocultadas de todas as análises e da tabela de tipagem."
      ),
      checkboxGroupInput(
        inputId = "modal_selected_cols",
        label = NULL,
        choices = all_cols,
        selected = selected_cols
      ),
      footer = tagList(
        actionButton("modal_vars_all", "Selecionar todas",
                     class = "btn btn-sm btn-outline-secondary"),
        modalButton("Cancelar"),
        actionButton("modal_vars_apply", "Aplicar",
                     class = "btn btn-sm btn-primary")
      ),
      easyClose = TRUE,
      size = "s"
    ))
  })
  
  # Observador para marcar todas as variáveis no modal
  observeEvent(input$modal_vars_all, {
    df <- raw_data()
    req(df)
    all_cols <- names(df)
    updateCheckboxGroupInput(session, "modal_selected_cols", selected = all_cols)
  })
  
  # Observador para aplicar a seleção de variáveis do modal
  observeEvent(input$modal_vars_apply, {
    req(input$modal_selected_cols)
    selected_cols_rv(input$modal_selected_cols)
    removeModal()
  })

  # --- Renomear colunas (import) : modal com "nome atual -> nome novo" ---
  observeEvent(input$rename_cols_btn, {
    df <- raw_data(); req(df)
    all_cols <- names(df)
    sel <- intersect(selected_cols_rv(), all_cols)
    cols <- if (length(sel) > 0) sel else all_cols
    rn_import_cols(cols)
    ren <- col_renames_rv()
    atual <- cols
    if (length(ren)) for (o in names(ren)) atual[cols == o] <- unname(ren[o])
    showModal(modalDialog(
      title = "Renomear Colunas", size = "l", easyClose = TRUE,
      tags$p(style = "font-size: 0.85rem; color: #6c757d;",
             "Ajuste os nomes à direita. A tipagem e os filtros continuam usando os dados originais; a renomeação vale para a prévia, as análises e as exportações."),
      div(style = "max-height: 430px; overflow-y: auto; padding-right: 6px;",
        lapply(seq_along(cols), function(i)
          div(style = "display:flex; gap:10px; align-items:center; margin-bottom:6px;",
            div(style = "flex:1; font-size:0.82rem; color:#666; word-break:break-word;", cols[i]),
            div(style = "flex:0 0 20px; text-align:center; color:#aaa;", "→"),
            div(style = "flex:1;", textInput(paste0("rename_in_", i), NULL, value = atual[i], width = "100%"))))),
      footer = tagList(
        actionButton("rename_reset", "Restaurar originais", class = "btn btn-sm btn-outline-secondary"),
        modalButton("Cancelar"),
        actionButton("rename_apply", "Aplicar", class = "btn btn-sm btn-primary"))
    ))
  })

  observeEvent(input$rename_reset, {
    cols <- rn_import_cols(); req(length(cols) > 0)
    for (i in seq_along(cols)) updateTextInput(session, paste0("rename_in_", i), value = cols[i])
  })

  observeEvent(input$rename_apply, {
    cols <- rn_import_cols(); req(length(cols) > 0)
    novos <- vapply(seq_along(cols), function(i) {
      v <- input[[paste0("rename_in_", i)]]; if (is.null(v)) cols[i] else trimws(v)
    }, character(1))
    if (any(!nzchar(novos))) { showNotification("Os nomes não podem ficar vazios.", type = "error"); return() }
    if (anyDuplicated(novos)) { showNotification("Há nomes de coluna duplicados.", type = "error"); return() }
    mudou <- novos != cols
    col_renames_rv(stats::setNames(novos[mudou], cols[mudou]))
    removeModal()
    showNotification(if (any(mudou)) sprintf("%d coluna(s) renomeada(s).", sum(mudou)) else "Nenhuma alteração de nome.",
                     type = "message", duration = 3)
  })

  # Observador para capturar mudanças nos inputs de tipagem dinâmica gerados
  observe({
    df <- raw_data()
    req(df)
    types <- col_types_rv()
    req(length(types) > 0)
    
    updated <- FALSE
    for (col_name in names(df)) {
      input_id <- paste0("col_type_", sanitize_id(col_name))
      val <- input[[input_id]]
      if (!is.null(val) && val != types[[col_name]]) {
        types[[col_name]] <- val
        updated <- TRUE
      }
    }
    if (updated) {
      col_types_rv(types)
    }
  })
  
  # Dataset atualizado reativamente com os tipos selecionados
  current_data <- reactive({
    df <- raw_data()
    req(df)
    types <- col_types_rv()
    req(length(types) > 0)
    
    # Filtrar colunas selecionadas pelo usuário (só as que existem no dataset atual —
    # evita erro/dado velho ao trocar de aba/dataset com colunas diferentes)
    cols_to_keep <- intersect(selected_cols_rv(), names(df))
    if (length(cols_to_keep) > 0) {
      df <- df[, cols_to_keep, drop = FALSE]
    }
    
    # Recodificar colunas se houver mapeamento/agrupamento registrado
    recodes <- col_recodes_rv()
    for (col_name in names(df)) {
      if (!is.null(recodes[[col_name]])) {
        col_map <- recodes[[col_name]]
        map_vec <- unlist(col_map)
        val_vec <- as.character(df[[col_name]])
        mapped_vec <- map_vec[val_vec]
        # Onde a busca falhar (e o original não era NA), mantém o original
        fallback_mask <- is.na(mapped_vec) & !is.na(val_vec)
        mapped_vec[fallback_mask] <- val_vec[fallback_mask]
        df[[col_name]] <- unname(mapped_vec)
      }
    }
    
    for (col_name in names(df)) {
      target_type <- types[[col_name]]
      if (!is.null(target_type)) {
        df[[col_name]] <- tryCatch({
          if (target_type == "numeric") {
            cleaned <- gsub(",", ".", as.character(df[[col_name]]))
            as.numeric(cleaned)
          } else if (target_type == "character") {
            as.character(df[[col_name]])
          } else if (target_type == "factor") {
            as.factor(df[[col_name]])
          } else if (target_type == "integer") {
            cleaned <- gsub(",", ".", as.character(df[[col_name]]))
            as.integer(cleaned)
          } else if (target_type == "logical") {
            as.logical(df[[col_name]])
          } else if (target_type == "Date") {
            as.Date(as.character(df[[col_name]]))
          } else {
            df[[col_name]]
          }
        }, error = function(e) {
          df[[col_name]]
        })
      }
    }

    # Aplica os filtros de niveis (fatores): mantem apenas as linhas cujos
    # valores estejam entre os niveis selecionados e remove niveis vazios.
    filtros <- level_filters_rv()
    if (length(filtros) > 0) {
      for (col_name in names(filtros)) {
        manter <- filtros[[col_name]]
        if (!is.null(manter) && length(manter) > 0 && col_name %in% names(df) &&
            identical(types[[col_name]], "factor")) {
          valores <- as.character(df[[col_name]])
          df <- df[!is.na(valores) & valores %in% manter, , drop = FALSE]
          if (is.factor(df[[col_name]])) {
            df[[col_name]] <- droplevels(df[[col_name]])
          }
        }
      }
    }

    # Aplica os filtros de faixa (numericas/inteiras): mantem apenas as linhas
    # cujos valores estejam dentro do intervalo [min, max] selecionado.
    faixas <- range_filters_rv()
    if (length(faixas) > 0) {
      for (col_name in names(faixas)) {
        faixa <- faixas[[col_name]]
        if (!is.null(faixa) && length(faixa) == 2 && col_name %in% names(df) &&
            identical(types[[col_name]], "factor") == FALSE &&
            (identical(types[[col_name]], "numeric") || identical(types[[col_name]], "integer"))) {
          valores <- suppressWarnings(as.numeric(df[[col_name]]))
          df <- df[!is.na(valores) & valores >= faixa[1] & valores <= faixa[2], , drop = FALSE]
        }
      }
    }

    # Renomeação opcional de colunas — SEMPRE por último, para não interferir na
    # seleção/tipagem/filtros (que operam sobre os nomes originais). Mapa vazio =
    # sem efeito, então o comportamento padrão fica idêntico ao anterior.
    ren <- col_renames_rv()
    if (length(ren) > 0) {
      nm <- names(df)
      for (o in names(ren)) nm[nm == o] <- unname(ren[o])
      names(df) <- nm
    }

    df
  })
  
  # Renderiza a interface de alteração de tipos
  output$variable_type_converter_ui <- renderUI({
    df <- raw_data()
    req(df)
    # Só mantém colunas que existem no dataset atual (evita "colunas indefinidas
    # selecionadas" ao trocar/abrir um conjunto com colunas diferentes das do estado
    # anterior). Espelha a proteção usada em current_data().
    cols_to_keep <- intersect(selected_cols_rv(), names(df))
    if (length(cols_to_keep) > 0) {
      df <- df[, cols_to_keep, drop = FALSE]
    }
    types <- col_types_rv()
    req(length(types) > 0)
    lvlf <- level_filters_rv()
    rngf <- range_filters_rv()

    card(
      card_header("Tipagem de Variáveis", style = "font-size: 0.95rem; font-weight: 700; color: #0d6efd;"),
      card_body(
        style = "padding: 10px 15px; max-height: 250px; overflow-y: auto;",
        tags$table(class = "table table-sm table-borderless align-middle", style = "margin-bottom: 0; font-size: 0.85rem;",
          tags$thead(
            tags$tr(
              tags$th("Coluna", style = "width: 38%; color: #495057; font-weight: 600;"),
              tags$th("Tipo de Dado", style = "width: 37%; color: #495057; font-weight: 600;"),
              tags$th("Filtro", style = "width: 25%; color: #495057; font-weight: 600;")
            )
          ),
          tags$tbody(
            lapply(names(df), function(col_name) {
              current_type <- types[[col_name]]
              if (is.null(current_type)) current_type <- "character"

              tags$tr(
                tags$td(
                  style = "padding: 4px 0; max-width: 110px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;",
                  tags$b(col_name)
                ),
                tags$td(
                  style = "padding: 4px 4px;",
                  selectInput(
                    inputId = paste0("col_type_", sanitize_id(col_name)),
                    label = NULL,
                    choices = c("character" = "character",
                                "numeric" = "numeric",
                                "factor" = "factor",
                                "integer" = "integer",
                                "logical" = "logical",
                                "Date" = "Date"),
                    selected = current_type,
                    width = "100%"
                  )
                ),
                tags$td(
                  style = "padding: 4px 0; text-align: right; white-space: nowrap;",
                  if (identical(current_type, "factor") || identical(current_type, "character")) {
                    fobj <- lvlf[[col_name]]
                    ativo <- !is.null(fobj) && length(fobj) > 0
                    n_total <- length(levels(as.factor(df[[col_name]])))
                    rotulo <- if (ativo) sprintf("%d/%d", length(fobj), n_total) else "Filtrar"
                    
                    col_recode <- col_recodes_rv()[[col_name]]
                    ativo_r <- !is.null(col_recode) && any(names(col_recode) != unlist(col_recode))
                    n_recoded <- if (!is.null(col_recode)) length(unique(unlist(col_recode))) else n_total
                    rotulo_r <- if (ativo_r) sprintf("Agrupado (%d)", n_recoded) else "Agrupar"
                    
                    tags$div(
                      style = "display: flex; gap: 4px; justify-content: flex-end;",
                      actionButton(
                        inputId = paste0("col_filter_btn_", sanitize_id(col_name)),
                        label = tagList(icon("filter"), rotulo),
                        class = if (ativo) "btn btn-sm btn-primary" else "btn btn-sm btn-outline-secondary",
                        style = "font-size: 0.72rem; padding: 2px 6px;"
                      ),
                      actionButton(
                        inputId = paste0("col_recode_btn_", sanitize_id(col_name)),
                        label = tagList(icon("object-group"), rotulo_r),
                        class = if (ativo_r) "btn btn-sm btn-primary" else "btn btn-sm btn-outline-secondary",
                        style = "font-size: 0.72rem; padding: 2px 6px;"
                      )
                    )
                  } else if (current_type %in% c("numeric", "integer")) {
                    robj <- rngf[[col_name]]
                    ativo <- !is.null(robj) && length(robj) == 2
                    rotulo <- if (ativo) "Faixa" else "Filtrar"
                    actionButton(
                      inputId = paste0("col_filter_btn_", sanitize_id(col_name)),
                      label = tagList(icon(if (ativo) "sliders" else "filter"), rotulo),
                      class = if (ativo) "btn btn-sm btn-primary" else "btn btn-sm btn-outline-secondary",
                      style = "font-size: 0.72rem; padding: 2px 6px;"
                    )
                  } else {
                    tags$span("—", style = "color: #adb5bd;")
                  }
                )
              )
            })
          )
        )
      )
    )
  })

  # ---- Filtro de niveis de fatores: modal + observadores dinamicos -----------
  abrir_modal_niveis <- function(col_name) {
    df <- raw_data()
    if (is.null(df) || !(col_name %in% names(df))) return(invisible(NULL))
    id <- sanitize_id(col_name)
    todos <- levels(as.factor(df[[col_name]]))
    atual <- level_filters_rv()[[col_name]]
    selecionados <- if (is.null(atual)) todos else atual
    showModal(modalDialog(
      title = paste0("Filtrar níveis — ", col_name),
      tags$p(
        style = "font-size: 0.85rem; color: #6c757d;",
        "Mantenha apenas os níveis desejados. As linhas dos níveis desmarcados ",
        "serão removidas de todas as análises."
      ),
      checkboxGroupInput(
        inputId = paste0("col_levels_", id),
        label = NULL,
        choices = todos,
        selected = selecionados
      ),
      footer = tagList(
        actionButton(paste0("col_levels_all_", id), "Selecionar todos",
                     class = "btn btn-sm btn-outline-secondary"),
        modalButton("Cancelar"),
        actionButton(paste0("col_levels_apply_", id), "Aplicar",
                     class = "btn btn-sm btn-primary")
      ),
      easyClose = TRUE,
      size = "s"
    ))
  }

  # ---- Agrupamento/Recodificação de categorias/fatores ------------------------
  abrir_modal_agrupar <- function(col_name) {
    df <- raw_data()
    if (is.null(df) || !(col_name %in% names(df))) return(invisible(NULL))
    id <- sanitize_id(col_name)
    niveis <- sort(unique(as.character(df[[col_name]])))
    recode_atual <- col_recodes_rv()[[col_name]]
    
    inputs_ui <- lapply(niveis, function(niv) {
      val_def <- if (!is.null(recode_atual[[niv]])) recode_atual[[niv]] else niv
      tags$tr(
        tags$td(style = "padding: 5px; font-weight: bold; width: 45%; vertical-align: middle;", niv),
        tags$td(style = "padding: 5px; width: 10%; text-align: center; vertical-align: middle;", icon("arrow-right")),
        tags$td(style = "padding: 5px; width: 45%; vertical-align: middle;",
                textInput(
                  inputId = paste0("recode_val_", id, "_", sanitize_id(niv)),
                  label = NULL,
                  value = val_def,
                  width = "100%"
                )
        )
      )
    })
    
    showModal(modalDialog(
      title = paste0("Agrupar / Recodificar categorias — ", col_name),
      tags$p(
        style = "font-size: 0.85rem; color: #6c757d; margin-bottom: 15px;",
        "Redefina os nomes das categorias para agrupar níveis. ",
        "Por exemplo, mapeie múltiplos níveis originais para '0' e '1' (para regressão logística) ou para nomes comuns de sua escolha."
      ),
      tags$div(
        style = "max-height: 250px; overflow-y: auto; padding-right: 5px;",
        tags$table(
          style = "width: 100%; border-collapse: collapse;",
          tags$tbody(inputs_ui)
        )
      ),
      footer = tagList(
        actionButton(paste0("col_recode_reset_", id), "Restaurar Padrão",
                     class = "btn btn-sm btn-outline-secondary"),
        modalButton("Cancelar"),
        actionButton(paste0("col_recode_apply_", id), "Aplicar",
                     class = "btn btn-sm btn-primary")
      ),
      easyClose = TRUE,
      size = "m"
    ))
  }

  abrir_modal_faixa <- function(col_name) {
    df <- raw_data()
    if (is.null(df) || !(col_name %in% names(df))) return(invisible(NULL))
    id <- sanitize_id(col_name)
    valores <- suppressWarnings(as.numeric(gsub(",", ".", as.character(df[[col_name]]))))
    if (all(is.na(valores))) {
      showNotification("A coluna não possui valores numéricos para filtrar.", type = "warning")
      return(invisible(NULL))
    }
    lim_min <- min(valores, na.rm = TRUE)
    lim_max <- max(valores, na.rm = TRUE)
    atual <- range_filters_rv()[[col_name]]
    valor <- if (is.null(atual)) c(lim_min, lim_max) else atual
    passo <- signif((lim_max - lim_min) / 100, 2)
    if (!is.finite(passo) || passo <= 0) passo <- NULL
    showModal(modalDialog(
      title = paste0("Filtrar faixa — ", col_name),
      tags$p(
        style = "font-size: 0.85rem; color: #6c757d;",
        "Mantenha apenas os valores dentro do intervalo. As linhas fora dele ",
        "serão removidas de todas as análises."
      ),
      sliderInput(
        inputId = paste0("col_range_", id),
        label = NULL,
        min = lim_min, max = lim_max, value = valor,
        step = passo, width = "100%"
      ),
      footer = tagList(
        actionButton(paste0("col_range_full_", id), "Faixa completa",
                     class = "btn btn-sm btn-outline-secondary"),
        modalButton("Cancelar"),
        actionButton(paste0("col_range_apply_", id), "Aplicar",
                     class = "btn btn-sm btn-primary")
      ),
      easyClose = TRUE,
      size = "s"
    ))
  }

  # Registra (uma unica vez por coluna) os observadores de filtro. O botao de
  # cada coluna decide, no clique, qual modal abrir conforme o tipo atual.
  observe({
    df <- raw_data()
    req(df)
    for (col_name in names(df)) {
      id <- sanitize_id(col_name)
      if (is.null(lvl_obs_registry[[paste0("reg_", id)]])) {
        local({
          cn <- col_name
          i <- id
          # Abrir o modal apropriado conforme o tipo atual da coluna
          lvl_obs_registry[[paste0("open_", i)]] <- observeEvent(
            input[[paste0("col_filter_btn_", i)]], {
              tp <- col_types_rv()[[cn]]
              if (identical(tp, "factor")) {
                abrir_modal_niveis(cn)
              } else if (identical(tp, "numeric") || identical(tp, "integer")) {
                abrir_modal_faixa(cn)
              }
            }, ignoreInit = TRUE)
          # --- Fator: selecionar todos / aplicar niveis ---
          lvl_obs_registry[[paste0("all_", i)]] <- observeEvent(
            input[[paste0("col_levels_all_", i)]], {
              todos <- levels(as.factor(raw_data()[[cn]]))
              updateCheckboxGroupInput(session, paste0("col_levels_", i), selected = todos)
            }, ignoreInit = TRUE)
          lvl_obs_registry[[paste0("apply_", i)]] <- observeEvent(
            input[[paste0("col_levels_apply_", i)]], {
              sel <- input[[paste0("col_levels_", i)]]
              todos <- levels(as.factor(raw_data()[[cn]]))
              f <- level_filters_rv()
              if (is.null(sel) || length(sel) == 0 || setequal(sel, todos)) {
                f[[cn]] <- NULL
              } else {
                f[[cn]] <- sel
              }
              level_filters_rv(f)
              removeModal()
            }, ignoreInit = TRUE)
          # --- Numerica: faixa completa / aplicar faixa ---
          lvl_obs_registry[[paste0("rfull_", i)]] <- observeEvent(
            input[[paste0("col_range_full_", i)]], {
              valores <- suppressWarnings(as.numeric(gsub(",", ".", as.character(raw_data()[[cn]]))))
              if (!all(is.na(valores))) {
                updateSliderInput(session, paste0("col_range_", i),
                                  value = c(min(valores, na.rm = TRUE), max(valores, na.rm = TRUE)))
              }
            }, ignoreInit = TRUE)
          lvl_obs_registry[[paste0("rapply_", i)]] <- observeEvent(
            input[[paste0("col_range_apply_", i)]], {
              sel <- input[[paste0("col_range_", i)]]
              valores <- suppressWarnings(as.numeric(gsub(",", ".", as.character(raw_data()[[cn]]))))
              rf <- range_filters_rv()
              if (is.null(sel) || length(sel) != 2 || all(is.na(valores))) {
                rf[[cn]] <- NULL
              } else {
                lim_min <- min(valores, na.rm = TRUE)
                lim_max <- max(valores, na.rm = TRUE)
                if (isTRUE(all.equal(sel[1], lim_min)) && isTRUE(all.equal(sel[2], lim_max))) {
                  rf[[cn]] <- NULL
                } else {
                  rf[[cn]] <- c(sel[1], sel[2])
                }
              }
              range_filters_rv(rf)
              removeModal()
            }, ignoreInit = TRUE)
          
          # --- Recodificação/Agrupamento de Fator ---
          lvl_obs_registry[[paste0("recode_open_", i)]] <- observeEvent(
            input[[paste0("col_recode_btn_", i)]], {
              abrir_modal_agrupar(cn)
            }, ignoreInit = TRUE)
            
          lvl_obs_registry[[paste0("recode_reset_", i)]] <- observeEvent(
            input[[paste0("col_recode_reset_", i)]], {
              niveis <- sort(unique(as.character(raw_data()[[cn]])))
              for (niv in niveis) {
                updateTextInput(session, paste0("recode_val_", i, "_", sanitize_id(niv)), value = niv)
              }
            }, ignoreInit = TRUE)
            
          lvl_obs_registry[[paste0("recode_apply_", i)]] <- observeEvent(
            input[[paste0("col_recode_apply_", i)]], {
              niveis <- sort(unique(as.character(raw_data()[[cn]])))
              mapa <- list()
              for (niv in niveis) {
                novo_val <- input[[paste0("recode_val_", i, "_", sanitize_id(niv))]]
                if (!is.null(novo_val) && nzchar(novo_val)) {
                  mapa[[niv]] <- novo_val
                } else {
                  mapa[[niv]] <- niv
                }
              }
              
              # Grava o mapeamento
              recodes <- col_recodes_rv()
              recodes[[cn]] <- mapa
              col_recodes_rv(recodes)
              
              # Reseta filtros de niveis se houver, pois os niveis mudaram
              f <- level_filters_rv()
              if (!is.null(f[[cn]])) {
                f[[cn]] <- NULL
                level_filters_rv(f)
              }
              
              removeModal()
            }, ignoreInit = TRUE)
            
          lvl_obs_registry[[paste0("reg_", i)]] <- TRUE
        })
      }
    }
  })
  
  # Indicador do status do dataset
  output$dataset_status_indicator <- renderUI({
    df <- current_data()
    if (is.null(df)) {
      div(
        class = "alert alert-warning",
        style = "padding: 10px; border-radius: 8px; font-size: 0.9rem; margin-bottom: 0;",
        icon("triangle-exclamation"), " Nenhum dataset carregado no momento."
      )
    } else {
      div(
        div(
          class = "alert alert-success",
          style = "padding: 10px; border-radius: 8px; font-size: 0.9rem; margin-bottom: 12px; font-weight: 500;",
          icon("circle-check"), " Dataset pronto para análise!"
        ),
        tags$table(class = "table table-sm table-borderless", style = "margin-bottom: 0; font-size: 0.85rem;",
          tags$tbody(
            tags$tr(
              tags$td(tags$b("Linhas:")),
              tags$td(nrow(df))
            ),
            tags$tr(
              tags$td(tags$b("Colunas:")),
              tags$td(ncol(df))
            ),
            tags$tr(
              tags$td(tags$b("Origem:")),
              tags$td(if (input$data_source == "local") {
                if (is.null(input$file_upload)) "Arquivo Padrão" else input$file_upload$name
              } else {
                paste("EAPADados:", input$package_dataset)
              })
            )
          )
        ),
        # Linha do DATASET ATIVO das análises (reage à promoção do Arrumar)
        uiOutput("dataset_ativo_linha")
      )
    }
  })

  # Mostra qual tabela as análises estão usando (importados x resultado do Arrumar)
  output$dataset_ativo_linha <- renderUI({
    da <- dataset_ativo_rv()
    if (is.null(da)) {
      div(style = "margin-top: 8px; font-size: 0.82rem; color: #2E7D8F;",
          icon("circle-info"), " Análises usando: dados importados.")
    } else {
      div(style = "margin-top: 8px;",
        div(class = "alert alert-info",
            style = "padding: 8px 10px; border-radius: 8px; font-size: 0.82rem; margin-bottom: 6px;",
            icon("wand-magic-sparkles"),
            sprintf(" Análises usando: %s (%d x %d).", da$fonte, nrow(da$df), ncol(da$df))),
        actionButton("voltar_importados", "Voltar aos dados importados",
                     icon = icon("rotate-left"),
                     class = "btn btn-sm btn-outline-secondary w-100"))
    }
  })
  
  # --- IMPORTAÇÃO DE DADOS LOCAL E DE PACOTE ---
  
  # Gera seletor dinâmico de variáveis do dataset (Botão de Abertura de Modal)
  output$dataset_vars_selector <- renderUI({
    df <- raw_data()
    req(df)
    
    all_cols <- names(df)
    selected_cols <- selected_cols_rv()
    if (length(selected_cols) == 0) {
      selected_cols <- all_cols
    }
    
    ativo <- length(selected_cols) < length(all_cols)
    rotulo <- sprintf("Selecionar Variáveis (%d/%d)", length(selected_cols), length(all_cols))

    n_ren <- length(col_renames_rv())
    rotulo_ren <- if (n_ren > 0) sprintf("Renomear Colunas (%d)", n_ren) else "Renomear Colunas"

    tagList(
      actionButton(
        "select_vars_btn",
        label = tagList(icon("list-check"), rotulo),
        class = if (ativo) "btn btn-sm btn-primary w-100" else "btn btn-sm btn-outline-secondary w-100",
        style = "font-size: 0.85rem; padding: 4px 10px; margin-top: 0px; margin-bottom: 0px;"
      ),
      actionButton(
        "rename_cols_btn",
        label = tagList(icon("i-cursor"), rotulo_ren),
        class = if (n_ren > 0) "btn btn-sm btn-primary w-100" else "btn btn-sm btn-outline-secondary w-100",
        style = "font-size: 0.85rem; padding: 4px 10px; margin-top: 6px; margin-bottom: 0px;"
      )
    )
  })
  
  # Gera seletor de sheets dinâmico se for planilha Excel
  output$excel_sheet_selector <- renderUI({
    req(input$file_upload)
    ext <- tolower(tools::file_ext(input$file_upload$name))
    if (ext %in% c("xlsx", "xls")) {
      sheets <- excel_sheets(input$file_upload$datapath)
      # Cria opções formatadas mostrando o índice (Ex: "3 - regressao")
      sheet_choices <- setNames(sheets, paste0(1:length(sheets), " - ", sheets))
      # Tenta selecionar a Sheet 3 por padrão (índice 3), se disponível
      selected_sheet <- if (length(sheets) >= 3) sheets[3] else sheets[1]
      selectizeInput("excel_sheet", "Selecione a Aba (Sheet):", choices = sheet_choices, selected = selected_sheet,
        options = list(placeholder = "Digite ou escolha a aba...", openOnFocus = TRUE))
    } else {
      NULL
    }
  })
  
  # Observador reativo para atualizar o dataset com base nas entradas
  observe({
    if (input$data_source == "local") {
      req(input$file_upload)
      path <- input$file_upload$datapath
      ext <- tolower(tools::file_ext(input$file_upload$name))
      
      tryCatch({
        if (ext == "csv") {
          df <- read.csv(path, 
                         header = input$csv_header, 
                         sep = input$csv_sep, 
                         dec = input$csv_dec,
                         stringsAsFactors = FALSE,
                         check.names = FALSE)
          raw_data(df)
        } else if (ext %in% c("xlsx", "xls")) {
          req(input$excel_sheet, nzchar(input$excel_sheet))
          req(input$excel_sheet %in% excel_sheets(path))   # só lê uma aba válida
          df <- as.data.frame(read_excel(path, sheet = input$excel_sheet))
          raw_data(df)
        }
      }, error = function(e) {
        showNotification(paste("Erro ao ler o arquivo:", e$message), type = "error")
        # NÃO zera raw_data: mantém o último dataset válido para não travar a tela
      })
      
    } else if (input$data_source == "package") {
      req(input$package_dataset, nzchar(input$package_dataset))
      req(input$package_dataset %in% eapa_datasets)   # só carrega um conjunto válido
      if (requireNamespace("EAPADados", quietly = TRUE)) {
        tryCatch({
          # Carrega o dataset de forma ultra-rápida direto do namespace
          df <- tryCatch({
            get(input$package_dataset, envir = asNamespace("EAPADados"))
          }, error = function(err) {
            # Fallback seguro usando data()
            data(list = input$package_dataset, package = "EAPADados", envir = environment())
            get(input$package_dataset)
          })
          raw_data(as.data.frame(df))
        }, error = function(e) {
          showNotification(paste("Erro ao carregar do pacote:", e$message), type = "error")
          # NÃO zera raw_data: mantém o último dataset válido
        })
      }
    }
  })
  
  # Caso o usuário tenha colocado o arquivo 'datasets-projetos.xlsx' diretamente na pasta dados
  # Vamos pré-carregar ele por padrão se nenhum arquivo for carregado
  observe({
    default_excel_path <- "dados/datasets-projetos.xlsx"
    if (is.null(raw_data()) && file.exists(default_excel_path)) {
      tryCatch({
        sheets <- excel_sheets(default_excel_path)
        # Prefere a sheet 3 como padrão
        selected_sheet <- if (length(sheets) >= 3) sheets[3] else sheets[1]
        df <- as.data.frame(read_excel(default_excel_path, sheet = selected_sheet))
        raw_data(df)
      }, error = function(e) {
        # Ignora erro silenciosamente no setup inicial
      })
    }
  })
  
  # Exibe a tabela de dados
  output$data_preview_table <- renderDT({
    df <- current_data()
    req(df)
    datatable(df, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Exibe o sumário dos dados
  output$data_summary_text <- renderPrint({
    df <- current_data()
    req(df)
    summary(df)
  })
  # Informações de importação reativas para exportação de código
  import_info <- reactive({
    list(
      source = input$data_source,
      file_name = if (!is.null(input$file_upload)) input$file_upload$name else "datasets-projetos.xlsx",
      datapath = if (!is.null(input$file_upload)) input$file_upload$datapath else "dados/datasets-projetos.xlsx",
      excel_sheet = if (!is.null(input$excel_sheet)) input$excel_sheet else "regressao",
      csv_sep = if (!is.null(input$csv_sep)) input$csv_sep else ",",
      csv_dec = if (!is.null(input$csv_dec)) input$csv_dec else ".",
      csv_header = if (!is.null(input$csv_header)) input$csv_header else TRUE,
      package_dataset = input$package_dataset
    )
  })

  # ============================================================================
  # DATASET ATIVO PARA AS ANÁLISES  (Fase 2)
  # A Trilha de Preparo é a camada MAIS EXTERNA do dataset ativo. Resolução:
  #   importados (current_data)
  #     -> Arrumar/Calcular promovem via dataset_ativo_rv -> base_resolvida
  #     -> replay(base_resolvida, pipeline_rv)            -> dados_analise
  # REGRA anti-dupla-aplicação: Arrumar/Calcular/Trilha leem a base_resolvida
  # (pré-trilha), NUNCA o dados_analise. A trilha é sempre a última camada.
  # ============================================================================
  dataset_ativo_rv <- reactiveVal(NULL)   # NULL = usar dados importados
  pipeline_rv      <- reactiveVal(list()) # trilha de preparo (lista de etapas)
  base_externa_rv  <- reactiveVal(NULL)   # provenance da base promovida (Arrumar): list(fonte, codigo)
  registro_bases_rv <- reactiveVal(bases_vazio()) # Fase 3A: ramos diretos de dados_analise
  cache_bases_rv <- reactiveVal(bases_cache_vazio()) # Fase 3A.1: resultados somente em memória
  revisao_dados_analise_rv <- reactiveVal(1L)

  # Base sobre a qual a trilha atua (importados ou resultado promovido).
  base_resolvida <- reactive({
    da <- dataset_ativo_rv()
    if (is.null(da)) current_data() else da$df
  })

  # Replay da trilha (uma vez só): devolve df + erros por etapa.
  replay_res <- reactive({ replay_pipeline(base_resolvida(), pipeline_rv()) })

  # Dataset que TODAS as análises leem (base_resolvida + trilha).
  dados_analise <- reactive({ replay_res()$df })

  # A revisão acompanha as FONTES de dados_analise, não sua mera leitura. Assim,
  # abrir uma prévia ou recalcular um ramo nunca o torna obsoleto imediatamente.
  # Na Fase 3C, análises registradas usarão o mesmo número para pedir atualização.
  observeEvent(list(current_data(), dataset_ativo_rv(), pipeline_rv()), {
    revisao_dados_analise_rv(revisao_dados_analise_rv() + 1L)
  }, ignoreInit = TRUE)

  # Callback que os módulos Arrumar chamam ao clicar "Usar nas análises".
  promover_dataset <- function(df, fonte, codigo = NULL) {
    dataset_ativo_rv(list(df = df, fonte = fonte))
    base_externa_rv(if (!is.null(codigo) && nzchar(codigo)) list(fonte = fonte, codigo = codigo) else NULL)
    showNotification(sprintf("Análises agora usam: %s (%d linhas x %d colunas).",
                             fonte, nrow(df), ncol(df)), type = "message", duration = 6)
  }

  # Voltar aos dados importados
  observeEvent(input$voltar_importados, {
    dataset_ativo_rv(NULL); base_externa_rv(NULL)
    showNotification("Análises voltaram aos dados importados.", type = "message", duration = 4)
  })

  # Trocar de arquivo/fonte descarta a promoção E a trilha (evita dado velho)
  observeEvent(raw_data(), {
    dataset_ativo_rv(NULL)
    pipeline_rv(list())
    base_externa_rv(NULL)
    registro_bases_rv(bases_vazio())
    cache_bases_rv(bases_cache_vazio())
    revisao_dados_analise_rv(1L)
  }, ignoreInit = TRUE)

  # --- CHAMADA DO MÓDULO DE REGRESSÃO ---
  mod_regression_server("regression", dados_analise, import_info)
  
  # --- CHAMADA DOS MÓDULOS DE REGRESSÃO NÃO LINEAR ---
  mod_nonlinear_server("exponencial", dados_analise, import_info, "exponencial")
  mod_nonlinear_server("potencia", dados_analise, import_info, "potencia")
  mod_nonlinear_server("von_bertalanffy", dados_analise, import_info, "von_bertalanffy")
  mod_nonlinear_server("polinomial", dados_analise, import_info, "polinomial")
  mod_nonlinear_server("logaritmica", dados_analise, import_info, "logaritmica")
  regressao_logistica <- mod_regression_server(
    "logistic_regression", dados_analise, import_info, is_logistic = TRUE,
    registro_bases_rv = registro_bases_rv,
    cache_bases_rv = cache_bases_rv,
    revisao_origem_rv = revisao_dados_analise_rv
  )
  mod_model_discovery_server("discovery", dados_analise, import_info)

  # --- CHAMADAS DOS MÓDULOS DE DESCRIÇÃO DE DADOS ---
  mod_descr_stats_server("descr_stats", dados_analise, import_info)
  mod_frequencia_server("frequencia", dados_analise, import_info)
  mod_histogram_server("histogram", dados_analise, import_info)
  mod_boxplot_server("boxplot", dados_analise, import_info)
  mod_pizza_server("pizza", dados_analise, import_info)
  mod_scatter_server("scatter", dados_analise, import_info)
  mod_lines_server("lines", dados_analise, import_info)
  mod_bar_server("bar", dados_analise, import_info)
  mod_mapa_server("mapa", dados_analise, import_info)
  mod_mapa_pontos_server("mapa_pontos", dados_analise, import_info, "pontos")
  mod_mapa_pontos_server("mapa_bolhas", dados_analise, import_info, "bolhas")
  mod_series_temporais_server("series", dados_analise, import_info)
  mod_comunicacao_server("comunicacao", dados_analise, import_info)
  mod_correlacao_server("correlacao", dados_analise, import_info)
  mod_laboratorio_server("laboratorio")

  # --- CHAMADA DO MÓDULO PARAMÉTRICO ---
  mod_parametric_server("parametric", dados_analise, import_info)

  # --- CHAMADAS DOS NOVOS MÓDULOS ---
  mod_aas_server("aas", dados_analise, import_info)
  mod_aep_server("aep", dados_analise, import_info)
  mod_as_server("as", dados_analise, import_info)
  contingency_shared <- mod_contingency_server("contingency", dados_analise, import_info)
  # Arrumar continua lendo os dados IMPORTADOS (current_data) e promove seu
  # resultado às análises via callback on_usar.
  mod_arrumar_server("arrumar_emp", current_data, import_info, modo_fixo = "empilhar", on_usar = promover_dataset)
  mod_arrumar_server("arrumar_sep", current_data, import_info, modo_fixo = "separar", on_usar = promover_dataset)
  # Calcular/Reescalar lê a BASE RESOLVIDA (pré-trilha, para não aplicar a trilha
  # duas vezes) e promove seu resultado pelo mesmo callback.
  mod_calcular_server("calcular", base_resolvida, import_info, on_usar = promover_dataset)
  # Agrupar/Sumarizar também atua antes da Trilha: cria uma base com uma linha
  # por grupo e só a promove quando o usuário confirma o resultado.
  mod_agrupar_sumarizar_server("agrupar_sumarizar", base_resolvida, import_info,
                               on_usar = promover_dataset)
  # Trilha de Preparo (Fase 2): edita o pipeline GLOBAL; é a camada mais externa
  # do dados_analise, automática (sem "Usar nas análises").
  mod_tratar_server("tratar", base_resolvida, replay_res, pipeline_rv, import_info, base_externa_rv)
  # Fases 3A/3B: cadastro, receita e replay lazy de ramos em estrela. A escolha
  # da base pelos módulos analíticos entra na próxima integração incremental.
  bases_derivadas <- mod_bases_derivadas_server(
    "bases_derivadas", dados_analise, registro_bases_rv, cache_bases_rv,
    revisao_dados_analise_rv
  )
  mod_anova_server("anova", dados_analise, import_info)
  mod_ancova_server("ancova", dados_analise, import_info)

  # --- MÓDULOS DE TESTES NÃO PARAMÉTRICOS (um por item de menu; qui-quadrado usa a tabela preparada) ---
  mod_nonparametric_server("np_qui", dados_analise, import_info, contingency_shared, "quiquadrado")
  mod_nonparametric_server("np_mw",  dados_analise, import_info, contingency_shared, "mannwhitney")
  mod_nonparametric_server("np_wil", dados_analise, import_info, contingency_shared, "wilcoxon")
  mod_nonparametric_server("np_kw",  dados_analise, import_info, contingency_shared, "kruskal")
  mod_pca_server("pca", dados_analise, import_info)
  mod_hca_server("hca", dados_analise, import_info)
  mod_experimental_design_server("experimental_design")
  
  # --- CONTROLE E RASTREAMENTO PARA EXPORTAÇÃO CONSOLIDADA ---
  used_analyses <- reactiveValues(
    descr_stats = FALSE,
    histogram = FALSE,
    boxplot = FALSE,
    regression = FALSE,
    parametric = FALSE,
    aas = FALSE,
    aep = FALSE,
    as = FALSE,
    contingency = FALSE,
    anova = FALSE,
    pca = FALSE,
    hca = FALSE,
    exponencial = FALSE,
    potencia = FALSE,
    von_bertalanffy = FALSE,
    polinomial = FALSE,
    logaritmica = FALSE,
    logistico = FALSE
  )
  
  # Observa a aba ativa para registrar visitas/trabalho em cada análise
  observe({
    req(input$main_navbar)
    tab <- input$main_navbar
    if (tab == "Estatística Descritiva") {
      used_analyses$descr_stats <- TRUE
    } else if (tab == "Histogramas") {
      used_analyses$histogram <- TRUE
    } else if (tab == "Boxplot") {
      used_analyses$boxplot <- TRUE
    } else if (tab == "Regressão Linear Simples") {
      used_analyses$regression <- TRUE
    } else if (tab == "Curva Exponencial") {
      used_analyses$exponencial <- TRUE
    } else if (tab == "Curva de Modelo de Potência") {
      used_analyses$potencia <- TRUE
    } else if (tab == "Curva de Crescimento de Peixes") {
      used_analyses$von_bertalanffy <- TRUE
    } else if (tab == "Modelo Polinomial") {
      used_analyses$polinomial <- TRUE
    } else if (tab == "Curva Logarítmica") {
      used_analyses$logaritmica <- TRUE
    } else if (tab == "Curva Logística") {
      used_analyses$logistico <- TRUE
    } else if (tab == "Teste t de Student") {
      used_analyses$parametric <- TRUE
    } else if (tab == "Amostrando uma AAS") {
      used_analyses$aas <- TRUE
    } else if (tab == "Amostrando um AE Proporcional") {
      used_analyses$aep <- TRUE
    } else if (tab == "Amostrando uma AS (Sistemática)") {
      used_analyses$as <- TRUE
    } else if (tab == "Criando Tabela de Contingência") {
      used_analyses$contingency <- TRUE
    } else if (tab == "ANOVA (Análise de Variância)") {
      used_analyses$anova <- TRUE
    } else if (tab == "PCA (Componentes Principais)") {
      used_analyses$pca <- TRUE
    } else if (tab == "Análise de Agrupamentos (Clustering)") {
      used_analyses$hca <- TRUE
    }
  })
  
  # Limpa o rastreamento ao carregar ou alterar o conjunto de dados
  observeEvent(raw_data(), {
    col_renames_rv(character(0))   # zera renomeações ao trocar de dataset
    used_analyses$descr_stats <- FALSE
    used_analyses$histogram <- FALSE
    used_analyses$boxplot <- FALSE
    used_analyses$regression <- FALSE
    used_analyses$exponencial <- FALSE
    used_analyses$potencia <- FALSE
    used_analyses$von_bertalanffy <- FALSE
    used_analyses$polinomial <- FALSE
    used_analyses$logaritmica <- FALSE
    used_analyses$logistico <- FALSE
    used_analyses$parametric <- FALSE
    used_analyses$aas <- FALSE
    used_analyses$aep <- FALSE
    used_analyses$as <- FALSE
    used_analyses$contingency <- FALSE
    used_analyses$anova <- FALSE
    used_analyses$pca <- FALSE
    used_analyses$hca <- FALSE
    
    # Se já estiver em uma aba de análise durante a troca, re-habilita para o novo dataset
    if (!is.null(input$main_navbar)) {
      tab <- input$main_navbar
      if (tab == "Estatística Descritiva") {
        used_analyses$descr_stats <- TRUE
      } else if (tab == "Histogramas") {
        used_analyses$histogram <- TRUE
      } else if (tab == "Boxplot") {
        used_analyses$boxplot <- TRUE
      } else if (tab == "Regressão Linear Simples") {
        used_analyses$regression <- TRUE
      } else if (tab == "Curva Exponencial") {
        used_analyses$exponencial <- TRUE
      } else if (tab == "Curva de Modelo de Potência") {
        used_analyses$potencia <- TRUE
      } else if (tab == "Curva de Crescimento de Peixes") {
        used_analyses$von_bertalanffy <- TRUE
      } else if (tab == "Modelo Polinomial") {
        used_analyses$polinomial <- TRUE
      } else if (tab == "Curva Logarítmica") {
        used_analyses$logaritmica <- TRUE
      } else if (tab == "Curva Logística") {
        used_analyses$logistico <- TRUE
      } else if (tab == "Teste t de Student") {
        used_analyses$parametric <- TRUE
      } else if (tab == "Amostrando uma AAS") {
        used_analyses$aas <- TRUE
      } else if (tab == "Amostrando um AE Proporcional") {
        used_analyses$aep <- TRUE
      } else if (tab == "Amostrando uma AS (Sistemática)") {
        used_analyses$as <- TRUE
      } else if (tab == "Criando Tabela de Contingência") {
        used_analyses$contingency <- TRUE
      } else if (tab == "ANOVA (Análise de Variância)") {
        used_analyses$anova <- TRUE
      } else if (tab == "PCA (Componentes Principais)") {
        used_analyses$pca <- TRUE
      } else if (tab == "Análise de Agrupamentos (Clustering)") {
        used_analyses$hca <- TRUE
      }
    }
  }, ignoreInit = FALSE)
  
  output$export_project_options_ui <- renderUI({
    df <- current_data()
    if (is.null(df)) {
      helpText("Carregue um conjunto de dados para habilitar a exportação do projeto.")
    } else {
      any_analysis <- used_analyses$descr_stats || used_analyses$histogram || 
                      used_analyses$boxplot || used_analyses$regression ||
                      used_analyses$exponencial || used_analyses$potencia ||
                      used_analyses$von_bertalanffy || used_analyses$polinomial ||
                      used_analyses$logaritmica || used_analyses$logistico ||
                      used_analyses$parametric || used_analyses$aas ||
                      used_analyses$aep || used_analyses$as ||
                      used_analyses$contingency || used_analyses$anova ||
                      used_analyses$pca || used_analyses$hca
      
      tagList(
        helpText("Selecione quais análises incluir no pacote de estudo (.zip):", style = "margin-bottom: 12px; font-size: 0.88rem; color: #495057;"),
        
        # Menu: Preparando Dados
        h6("Preparando Dados (Amostragem & Contingência)", style = "font-weight: 700; color: #0d6efd; margin-top: 8px; margin-bottom: 6px; font-size: 0.85rem; border-bottom: 1px solid rgba(0,0,0,0.06); padding-bottom: 2px;"),
        div(
          style = if (!used_analyses$aas) "opacity: 0.5; pointer-events: none;" else "",
          checkboxInput("exp_aas", 
                        label = if (used_analyses$aas) "Amostragem Aleatória Simples (AAS)" else "Amostragem AAS (Não realizada)", 
                        value = used_analyses$aas)
        ),
        div(
          style = if (!used_analyses$aep) "opacity: 0.5; pointer-events: none;" else "",
          checkboxInput("exp_aep", 
                        label = if (used_analyses$aep) "Amostragem Estratificada Proporcional (AEP)" else "Amostragem AEP (Não realizada)", 
                        value = used_analyses$aep)
        ),
        div(
          style = if (!used_analyses$as) "opacity: 0.5; pointer-events: none;" else "",
          checkboxInput("exp_as", 
                        label = if (used_analyses$as) "Amostragem Sistemática (AS)" else "Amostragem AS (Não realizada)", 
                        value = used_analyses$as)
        ),
        div(
          style = if (!used_analyses$contingency) "opacity: 0.5; pointer-events: none;" else "",
          checkboxInput("exp_contingency", 
                        label = if (used_analyses$contingency) "Tabela de Contingência e Qui-Quadrado" else "Tabela de Contingência (Não realizada)", 
                        value = used_analyses$contingency)
        ),
        
        # Menu: Descrevendo Dados
        h6("Descrevendo Dados", style = "font-weight: 700; color: #0d6efd; margin-top: 14px; margin-bottom: 6px; font-size: 0.85rem; border-bottom: 1px solid rgba(0,0,0,0.06); padding-bottom: 2px;"),
        div(
          style = if (!used_analyses$descr_stats) "opacity: 0.5; pointer-events: none;" else "",
          checkboxInput("exp_descr_stats", 
                        label = if (used_analyses$descr_stats) "Estatística Descritiva" else "Estatística Descritiva (Não realizada)", 
                        value = used_analyses$descr_stats)
        ),
        div(
          style = if (!used_analyses$histogram) "opacity: 0.5; pointer-events: none;" else "",
          checkboxInput("exp_histogram", 
                        label = if (used_analyses$histogram) "Histogramas" else "Histogramas (Não realizada)", 
                        value = used_analyses$histogram)
        ),
        div(
          style = if (!used_analyses$boxplot) "opacity: 0.5; pointer-events: none;" else "",
          checkboxInput("exp_boxplot", 
                        label = if (used_analyses$boxplot) "Boxplots" else "Boxplots (Não realizada)", 
                        value = used_analyses$boxplot)
        ),
        
        # Menu: Modelos de Regressão
        h6("Modelos de Regressão", style = "font-weight: 700; color: #0d6efd; margin-top: 14px; margin-bottom: 6px; font-size: 0.85rem; border-bottom: 1px solid rgba(0,0,0,0.06); padding-bottom: 2px;"),
        div(
          style = if (!used_analyses$regression) "opacity: 0.5; pointer-events: none;" else "",
          checkboxInput("exp_regression", 
                        label = if (used_analyses$regression) "Regressão Linear Simples" else "Regressão Linear (Não realizada)", 
                        value = used_analyses$regression)
        ),
        
        # Menu: Regressão Não Linear
        h6("Regressão Não Linear (Modelos de Crescimento)", style = "font-weight: 700; color: #0d6efd; margin-top: 14px; margin-bottom: 6px; font-size: 0.85rem; border-bottom: 1px solid rgba(0,0,0,0.06); padding-bottom: 2px;"),
        div(
          style = if (!used_analyses$exponencial) "opacity: 0.5; pointer-events: none;" else "",
          checkboxInput("exp_exponencial", 
                        label = if (used_analyses$exponencial) "Curva Exponencial" else "Curva Exponencial (Não realizada)", 
                        value = used_analyses$exponencial)
        ),
        div(
          style = if (!used_analyses$potencia) "opacity: 0.5; pointer-events: none;" else "",
          checkboxInput("exp_potencia", 
                        label = if (used_analyses$potencia) "Curva de Modelo de Potência" else "Modelo de Potência (Não realizada)", 
                        value = used_analyses$potencia)
        ),
        div(
          style = if (!used_analyses$von_bertalanffy) "opacity: 0.5; pointer-events: none;" else "",
          checkboxInput("exp_von_bertalanffy", 
                        label = if (used_analyses$von_bertalanffy) "Curva de Crescimento de Peixes" else "Crescimento de Peixes (Não realizada)", 
                        value = used_analyses$von_bertalanffy)
        ),
        div(
          style = if (!used_analyses$polinomial) "opacity: 0.5; pointer-events: none;" else "",
          checkboxInput("exp_polinomial", 
                        label = if (used_analyses$polinomial) "Modelo Polinomial" else "Modelo Polinomial (Não realizada)", 
                        value = used_analyses$polinomial)
        ),
        div(
          style = if (!used_analyses$logaritmica) "opacity: 0.5; pointer-events: none;" else "",
          checkboxInput("exp_logaritmica", 
                        label = if (used_analyses$logaritmica) "Curva Logarítmica" else "Curva Logarítmica (Não realizada)", 
                        value = used_analyses$logaritmica)
        ),
        div(
          style = if (!used_analyses$logistico) "opacity: 0.5; pointer-events: none;" else "",
          checkboxInput("exp_logistico", 
                        label = if (used_analyses$logistico) "Curva Logística" else "Curva Logística (Não realizada)", 
                        value = used_analyses$logistico)
        ),
        
        # Menu: Testes Paramétricos
        h6("Testes Paramétricos", style = "font-weight: 700; color: #0d6efd; margin-top: 14px; margin-bottom: 6px; font-size: 0.85rem; border-bottom: 1px solid rgba(0,0,0,0.06); padding-bottom: 2px;"),
        div(
          style = if (!used_analyses$parametric) "opacity: 0.5; pointer-events: none;" else "",
          checkboxInput("exp_parametric", 
                        label = if (used_analyses$parametric) "Teste t de Student" else "Teste t de Student (Não realizada)", 
                        value = used_analyses$parametric)
        ),
        div(
          style = if (!used_analyses$anova) "opacity: 0.5; pointer-events: none;" else "",
          checkboxInput("exp_anova", 
                        label = if (used_analyses$anova) "ANOVA (Análise de Variância)" else "ANOVA (Não realizada)", 
                        value = used_analyses$anova)
        ),
        
        # Menu: Estatística Multivariada
        h6("Estatística Multivariada", style = "font-weight: 700; color: #0d6efd; margin-top: 14px; margin-bottom: 6px; font-size: 0.85rem; border-bottom: 1px solid rgba(0,0,0,0.06); padding-bottom: 2px;"),
        div(
          style = if (!used_analyses$pca) "opacity: 0.5; pointer-events: none;" else "",
          checkboxInput("exp_pca", 
                        label = if (used_analyses$pca) "PCA (Componentes Principais)" else "PCA (Não realizada)", 
                        value = used_analyses$pca)
        ),
        div(
          style = if (!used_analyses$hca) "opacity: 0.5; pointer-events: none;" else "",
          checkboxInput("exp_hca", 
                        label = if (used_analyses$hca) "Análise de Agrupamentos (Clustering)" else "Clustering (Não realizada)", 
                        value = used_analyses$hca)
        ),
        
        hr(style = "margin: 12px 0;"),
        
        if (any_analysis) {
          downloadButton("download_consolidated_zip", "Exportar Projeto R (.zip)", class = "btn-success w-100")
        } else {
          div(
            class = "alert alert-info",
            style = "padding: 8px 10px; font-size: 0.8rem; margin-bottom: 0;",
            icon("info-circle"), " Visite e configure alguma análise nos menus para liberar a exportação."
          )
        },
        helpText("Gera scripts R numerados, dados limpos e um relatório Quarto (.qmd) consolidado.", style = "margin-top: 10px; font-size: 0.8rem; margin-bottom: 0;")
      )
    }
  })
  
  output$download_consolidated_zip <- downloadHandler(
    filename = function() {
      dataset_name <- clean_dataset_name()
      paste0("projeto_", dataset_name, "_", format(Sys.Date(), "%Y-%m-%d"), ".zip")
    },
    content = function(file) {
      df_clean <- current_data()
      req(df_clean)
      
      proj_dir_name <- paste0("projeto_", clean_dataset_name(), "_", format(Sys.Date(), "%Y-%m-%d"))
      temp_dir <- tempdir()
      proj_dir <- file.path(temp_dir, proj_dir_name)
      dir.create(proj_dir, showWarnings = FALSE)
      
      dir_dados <- file.path(proj_dir, "dados")
      dir_scripts <- file.path(proj_dir, "scripts")
      dir_relatorios <- file.path(proj_dir, "relatorios")
      
      dir.create(dir_dados, showWarnings = FALSE)
      dir.create(dir_scripts, showWarnings = FALSE)
      dir.create(dir_relatorios, showWarnings = FALSE)
      
      # 1. Salvar os dados
      save(df_clean, file = file.path(dir_dados, "dados_limpos.rda"))
      write.csv(df_clean, file = file.path(dir_dados, "dados_limpos.csv"), row.names = FALSE)
      
      # 2. Gerar scripts e seções do relatório
      scripts_incluidos <- c()
      qmd_sections <- list()
      
      # --- SEÇÃO 1: ESTATÍSTICA DESCRITIVA ---
      if (isTRUE(input$exp_descr_stats)) {
        vars_selected <- input[["descr_stats-vars_selected"]]
        if (is.null(vars_selected) || length(vars_selected) == 0) {
          vars_selected <- names(df_clean)[sapply(df_clean, is.numeric)]
          if (length(vars_selected) == 0) vars_selected <- names(df_clean)[1]
        }
        vars_str <- paste(paste0("'", vars_selected, "'"), collapse = ", ")
        
        var_group <- input[["descr_stats-var_group"]]
        if (is.null(var_group)) var_group <- "none"
        
        show_group_comp <- input[["descr_stats-show_group_comp"]]
        if (is.null(show_group_comp)) show_group_comp <- TRUE
        
        descr_script_content <- c(
          "# --- SCRIPT DE ESTATÍSTICA DESCRITIVA (Consolidado) ---",
          "# 1. CARREGAR OS DADOS LIMPOS",
          "# (Carrega o arquivo RDA que preserva fatores e formatação)",
          "load('dados/dados_limpos.rda')",
          "dados <- df_clean",
          "",
          "# Alternativa em formato aberto CSV (se preferir):",
          "# library(readr)",
          "# dados <- read_csv('dados/dados_limpos.csv')",
          "",
          "# 2. SELEÇÃO DE VARIÁVEIS",
          sprintf("vars_selecionadas <- c(%s)", vars_str),
          "",
          "# 3. CÁLCULO DE ESTATÍSTICAS DESCRITIVAS",
          "obter_resumo <- function(x) {",
          "  c(N = sum(!is.na(x)),",
          "    NAs = sum(is.na(x)),",
          "    Media = mean(x, na.rm = TRUE),",
          "    Mediana = median(x, na.rm = TRUE),",
          "    Desvio_Padrao = sd(x, na.rm = TRUE),",
          "    Variancia = var(x, na.rm = TRUE),",
          "    Minimo = min(x, na.rm = TRUE),",
          "    Maximo = max(x, na.rm = TRUE),",
          "    Q25 = quantile(x, 0.25, na.rm = TRUE),",
          "    Q75 = quantile(x, 0.75, na.rm = TRUE))",
          "}",
          "",
          "cat('--- ESTATÍSTICAS GLOBAIS ---\\n')",
          "for (v in vars_selecionadas) {",
          "  cat('\\nVariável:', v, '\\n')",
          "  print(obter_resumo(dados[[v]]))",
          "}"
        )
        if (var_group != "none") {
          descr_script_content <- c(descr_script_content,
            "",
            "# 4. CÁLCULO DE ESTATÍSTICAS AGRUPADAS",
            sprintf("cat('\\n--- ESTATÍSTICAS AGRUPADAS POR %s ---\\n')", var_group),
            "for (v in vars_selecionadas) {",
            "  cat('\\nVariável:', v, '\\n')",
            sprintf("  print(tapply(dados[[v]], dados[['%s']], obter_resumo))", var_group),
            "}"
          )
        }
        writeLines(descr_script_content, file.path(dir_scripts, "1_estatistica_descritiva.R"))
        scripts_incluidos <- c(scripts_incluidos, "scripts/1_estatistica_descritiva.R")
        
        # Seção QMD
        qmd_descr <- c(
          "## Estatística Descritiva",
          "Abaixo estão as medidas de tendência central e dispersão calculadas para as variáveis selecionadas.",
          "",
          "```{r}",
          "#| label: descr-stats-consolidado",
          "obter_resumo_df <- function(df, vars, grupo = NULL) {",
          "  res_list <- list()",
          "  for (v in vars) {",
          "    if (is.null(grupo) || grupo == 'none') {",
          "      res_list[[v]] <- data.frame(",
          "        Variavel = v,",
          "        N = sum(!is.na(df[[v]])),",
          "        NAs = sum(is.na(df[[v]])),",
          "        Media = mean(df[[v]], na.rm = TRUE),",
          "        Mediana = median(df[[v]], na.rm = TRUE),",
          "        SD = sd(df[[v]], na.rm = TRUE),",
          "        Min = min(df[[v]], na.rm = TRUE),",
          "        Max = max(df[[v]], na.rm = TRUE)",
          "      )",
          "    } else {",
          "      gps <- as.factor(df[[grupo]])",
          "      for (l in levels(gps)) {",
          "        sub_x <- df[[v]][gps == l]",
          "        res_list[[paste0(v, '_', l)]] <- data.frame(",
          "          Variavel = v,",
          "          Grupo = l,",
          "          N = sum(!is.na(sub_x)),",
          "          NAs = sum(is.na(sub_x)),",
          "          Media = mean(sub_x, na.rm = TRUE),",
          "          Mediana = median(sub_x, na.rm = TRUE),",
          "          SD = sd(sub_x, na.rm = TRUE),",
          "          Min = min(sub_x, na.rm = TRUE),",
          "          Max = max(sub_x, na.rm = TRUE)",
          "        )",
          "      }",
          "    }",
          "  }",
          "  do.call(rbind, res_list)",
          "}",
          sprintf("res_tab <- obter_resumo_df(dados, c(%s), '%s')", vars_str, var_group),
          "knitr::kable(res_tab, digits = 3, caption = 'Estatísticas de Resumo dos Dados')",
          "```",
          ""
        )
        if (show_group_comp && var_group != "none" && length(vars_selected) > 0) {
          qmd_descr <- c(qmd_descr,
            "### Comparação Gráfica entre Grupos",
            "```{r}",
            "#| label: boxplot-grupo-consolidado",
            "#| echo: false",
            sprintf("for (v in c(%s)) {", vars_str),
            sprintf("  p <- ggplot(dados, aes(x = `%s`, y = .data[[v]], fill = `%s`)) +", var_group, var_group),
            "    geom_boxplot(alpha = 0.7) +",
            "    theme_minimal() +",
            sprintf("    labs(title = paste('Distribuição de', v, 'por %s'), y = v)", var_group),
            "  print(p)",
            "}",
            "```",
            ""
          )
        }
        qmd_sections[["descr_stats"]] <- qmd_descr
      }
      
      # --- SEÇÃO 2: HISTOGRAMA ---
      if (isTRUE(input$exp_histogram)) {
        hist_var_x <- input[["histogram-var_x"]]
        if (is.null(hist_var_x)) hist_var_x <- names(df_clean)[sapply(df_clean, is.numeric)][1]
        
        hist_var_group <- input[["histogram-var_group"]]
        if (is.null(hist_var_group)) hist_var_group <- "none"
        
        hist_bins <- input[["histogram-bins"]]
        if (is.null(hist_bins)) hist_bins <- 15
        
        hist_density <- input[["histogram-show_density"]]
        if (is.null(hist_density)) hist_density <- FALSE
        
        hist_theme <- input[["histogram-graph_theme"]]
        if (is.null(hist_theme)) hist_theme <- "minimal"
        
        hist_title <- input[["histogram-custom_title"]]
        if (is.null(hist_title) || !nzchar(hist_title)) hist_title <- paste("Distribuição de", hist_var_x)
        
        hist_label_x <- input[["histogram-custom_label_x"]]
        if (is.null(hist_label_x) || !nzchar(hist_label_x)) hist_label_x <- hist_var_x
        
        hist_label_y <- input[["histogram-custom_label_y"]]
        if (is.null(hist_label_y) || !nzchar(hist_label_y)) hist_label_y <- if (hist_density) "Densidade" else "Frequência"
        
        hist_theme_code <- switch(hist_theme,
                                  "minimal" = "theme_minimal(base_size = 14)",
                                  "classic" = "theme_classic(base_size = 14)",
                                  "bw"      = "theme_bw(base_size = 14)",
                                  "gray"    = "theme_gray(base_size = 14)",
                                  "light"   = "theme_light(base_size = 14)",
                                  "theme_minimal(base_size = 14)")
        
        hist_script_content <- c(
          "# --- SCRIPT DE HISTOGRAMA (Consolidado) ---",
          "library(ggplot2)",
          "",
          "# 1. CARREGAR OS DADOS LIMPOS",
          "# (Carrega o arquivo RDA que preserva fatores e formatação)",
          "load('dados/dados_limpos.rda')",
          "dados <- df_clean",
          "",
          "# Alternativa em formato aberto CSV (se preferir):",
          "# library(readr)",
          "# dados <- read_csv('dados/dados_limpos.csv')",
          "",
          "# 2. GERAR O PLOT DO HISTOGRAMA"
        )
        if (hist_var_group != "none") {
          hist_script_content <- c(hist_script_content,
            sprintf("dados$`%s` <- as.factor(dados$`%s`)", hist_var_group, hist_var_group),
            if (hist_density) {
              c(
                sprintf("ggplot(dados, aes(x = `%s`, y = after_stat(density), fill = `%s`, color = `%s`)) +", hist_var_x, hist_var_group, hist_var_group),
                sprintf("  geom_histogram(bins = %d, alpha = 0.5, position = 'identity') +", hist_bins),
                "  geom_density(linewidth = 1, fill = NA) +"
              )
            } else {
              c(
                sprintf("ggplot(dados, aes(x = `%s`, fill = `%s`)) +", hist_var_x, hist_var_group),
                sprintf("  geom_histogram(bins = %d, alpha = 0.7, position = 'dodge') +", hist_bins)
              )
            }
          )
        } else {
          if (hist_density) {
            hist_script_content <- c(hist_script_content,
              sprintf("ggplot(dados, aes(x = `%s`, y = after_stat(density))) +", hist_var_x),
              sprintf("  geom_histogram(bins = %d, fill = '#cfe2ff', color = '#0d6efd', alpha = 0.7) +", hist_bins),
              "  geom_density(color = '#dc3545', linewidth = 1, fill = NA) +"
            )
          } else {
            hist_script_content <- c(hist_script_content,
              sprintf("ggplot(dados, aes(x = `%s`)) +", hist_var_x),
              sprintf("  geom_histogram(bins = %d, fill = '#cfe2ff', color = '#0d6efd', alpha = 0.8) +", hist_bins)
            )
          }
        }
        hist_script_content <- c(hist_script_content,
          sprintf("  %s +", hist_theme_code),
          sprintf("  labs(title = '%s', x = '%s', y = '%s') +", hist_title, hist_label_x, hist_label_y),
          "  theme(plot.title = element_text(face = 'bold'))"
        )
        writeLines(hist_script_content, file.path(dir_scripts, "2_histograma.R"))
        scripts_incluidos <- c(scripts_incluidos, "scripts/2_histograma.R")
        
        # Seção QMD
        start_idx_hist <- grep("^# 2\\. GERAR", hist_script_content)
        if (length(start_idx_hist) == 0) start_idx_hist <- 14
        qmd_sections[["histogram"]] <- c(
          "## Histograma de Distribuição",
          sprintf("Visualização gráfica da distribuição de frequências para a variável **%s**.", hist_var_x),
          "",
          "```{r}",
          "#| label: histograma-consolidado",
          "#| echo: false",
          paste(hist_script_content[start_idx_hist:length(hist_script_content)], collapse = "\n"),
          "```",
          ""
        )
      }
      
      # --- SEÇÃO 3: BOXPLOT ---
      if (isTRUE(input$exp_boxplot)) {
        box_var_y <- input[["boxplot-var_y"]]
        if (is.null(box_var_y)) box_var_y <- names(df_clean)[sapply(df_clean, is.numeric)][1]
        
        box_var_x <- input[["boxplot-var_x"]]
        if (is.null(box_var_x)) box_var_x <- "none"
        
        box_var_group <- input[["boxplot-var_group"]]
        if (is.null(box_var_group)) box_var_group <- "none"
        
        box_grp_color <- input[["boxplot-grp_color"]]
        if (is.null(box_grp_color)) box_grp_color <- TRUE
        
        box_grp_fill <- input[["boxplot-grp_fill"]]
        if (is.null(box_grp_fill)) box_grp_fill <- TRUE
        
        box_points <- input[["boxplot-show_points"]]
        if (is.null(box_points)) box_points <- FALSE
        
        box_theme <- input[["boxplot-graph_theme"]]
        if (is.null(box_theme)) box_theme <- "minimal"
        
        box_title <- input[["boxplot-custom_title"]]
        if (is.null(box_title) || !nzchar(box_title)) {
          box_suffix <- if (box_var_x == "none") "" else paste(" por", box_var_x)
          if (box_var_group != "none") {
            box_suffix <- paste0(box_suffix, if (box_var_x == "none") " agrupado por " else " e ", box_var_group)
          }
          box_title <- paste("Diagrama de Caixa (Boxplot) de", box_var_y, box_suffix)
        }
        
        box_label_x <- input[["boxplot-custom_label_x"]]
        if (is.null(box_label_x) || !nzchar(box_label_x)) box_label_x <- if (box_var_x == "none") "" else box_var_x
        
        box_label_y <- input[["boxplot-custom_label_y"]]
        if (is.null(box_label_y) || !nzchar(box_label_y)) box_label_y <- box_var_y
        
        box_theme_code <- switch(box_theme,
                                 "minimal" = "theme_minimal(base_size = 14)",
                                 "classic" = "theme_classic(base_size = 14)",
                                 "bw"      = "theme_bw(base_size = 14)",
                                 "gray"    = "theme_gray(base_size = 14)",
                                 "light"   = "theme_light(base_size = 14)",
                                 "theme_minimal(base_size = 14)")
        
        box_script_content <- c(
          "# --- SCRIPT DE BOXPLOT (Consolidado) ---",
          "library(ggplot2)",
          "",
          "# 1. CARREGAR OS DADOS LIMPOS",
          "# (Carrega o arquivo RDA que preserva fatores e formatação)",
          "load('dados/dados_limpos.rda')",
          "dados <- df_clean",
          "",
          "# Alternativa em formato aberto CSV (se preferir):",
          "# library(readr)",
          "# dados <- read_csv('dados/dados_limpos.csv')",
          "",
          "# 2. GERAR O PLOT DO BOXPLOT"
        )
        
        box_var_x_active <- box_var_x != "none"
        box_var_group_active <- box_var_group != "none"
        
        if (box_var_x_active) {
          box_script_content <- c(box_script_content, sprintf("dados$`%s` <- as.factor(dados$`%s`)", box_var_x, box_var_x))
        }
        if (box_var_group_active) {
          box_script_content <- c(box_script_content, sprintf("dados$`%s` <- as.factor(dados$`%s`)", box_var_group, box_var_group))
        }
        
        if (box_var_x_active && box_var_group_active) {
          if (box_grp_fill && box_grp_color) {
            box_script_content <- c(box_script_content,
              sprintf("ggplot(dados, aes(x = `%s`, y = `%s`, fill = `%s`, color = `%s`, group = interaction(`%s`, `%s`))) +", box_var_x, box_var_y, box_var_group, box_var_group, box_var_x, box_var_group),
              "  geom_boxplot(alpha = 0.7, outlier.size = 2, position = position_dodge(0.8)) +"
            )
          } else if (box_grp_fill) {
            box_script_content <- c(box_script_content,
              sprintf("ggplot(dados, aes(x = `%s`, y = `%s`, fill = `%s`, group = interaction(`%s`, `%s`))) +", box_var_x, box_var_y, box_var_group, box_var_x, box_var_group),
              "  geom_boxplot(alpha = 0.7, outlier.size = 2, position = position_dodge(0.8)) +"
            )
          } else if (box_grp_color) {
            box_script_content <- c(box_script_content,
              sprintf("ggplot(dados, aes(x = `%s`, y = `%s`, color = `%s`, group = interaction(`%s`, `%s`))) +", box_var_x, box_var_y, box_var_group, box_var_x, box_var_group),
              "  geom_boxplot(alpha = 0.7, outlier.size = 2, position = position_dodge(0.8)) +"
            )
          } else {
            box_script_content <- c(box_script_content,
              sprintf("ggplot(dados, aes(x = `%s`, y = `%s`, group = interaction(`%s`, `%s`))) +", box_var_x, box_var_y, box_var_x, box_var_group),
              "  geom_boxplot(alpha = 0.7, outlier.size = 2, position = position_dodge(0.8)) +"
            )
          }
          if (box_points) {
            box_script_content <- c(box_script_content,
              "  geom_jitter(color = '#495057', alpha = 0.5, size = 1.8, position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8)) +"
            )
          }
        } else if (box_var_x_active && !box_var_group_active) {
          box_script_content <- c(box_script_content,
            sprintf("ggplot(dados, aes(x = `%s`, y = `%s`, fill = `%s`)) +", box_var_x, box_var_y, box_var_x),
            "  geom_boxplot(alpha = 0.7, outlier.color = '#dc3545', outlier.size = 2) +"
          )
          if (box_points) {
            box_script_content <- c(box_script_content,
              "  geom_jitter(color = '#495057', width = 0.15, alpha = 0.5, size = 1.8) +"
            )
          }
        } else if (!box_var_x_active && box_var_group_active) {
          if (box_grp_fill && box_grp_color) {
            box_script_content <- c(box_script_content,
              sprintf("ggplot(dados, aes(x = `%s`, y = `%s`, fill = `%s`, color = `%s`)) +", box_var_group, box_var_y, box_var_group, box_var_group)
            )
          } else if (box_grp_fill) {
            box_script_content <- c(box_script_content,
              sprintf("ggplot(dados, aes(x = `%s`, y = `%s`, fill = `%s`)) +", box_var_group, box_var_y, box_var_group)
            )
          } else if (box_grp_color) {
            box_script_content <- c(box_script_content,
              sprintf("ggplot(dados, aes(x = `%s`, y = `%s`, color = `%s`)) +", box_var_group, box_var_y, box_var_group)
            )
          } else {
            box_script_content <- c(box_script_content,
              sprintf("ggplot(dados, aes(x = `%s`, y = `%s`)) +", box_var_group, box_var_y)
            )
          }
          box_script_content <- c(box_script_content,
            "  geom_boxplot(alpha = 0.7, outlier.size = 2) +"
          )
          if (box_points) {
            box_script_content <- c(box_script_content,
              "  geom_jitter(color = '#495057', width = 0.15, alpha = 0.5, size = 1.8) +"
            )
          }
        } else {
          box_script_content <- c(box_script_content,
            sprintf("ggplot(dados, aes(x = '', y = `%s`)) +", box_var_y),
            "  geom_boxplot(fill = '#cfe2ff', color = '#0d6efd', alpha = 0.7, outlier.color = '#dc3545', outlier.size = 2) +"
          )
          if (box_points) {
            box_script_content <- c(box_script_content,
              "  geom_jitter(color = '#495057', width = 0.1, alpha = 0.5, size = 1.8) +"
            )
          }
        }
        box_script_content <- c(box_script_content,
          sprintf("  %s +", box_theme_code),
          sprintf("  labs(title = '%s', x = '%s', y = '%s') +", box_title, box_label_x, box_label_y),
          "  theme(plot.title = element_text(face = 'bold'))"
        )
        writeLines(box_script_content, file.path(dir_scripts, "3_boxplot.R"))
        scripts_incluidos <- c(scripts_incluidos, "scripts/3_boxplot.R")
        
        # Seção QMD
        start_idx_box <- grep("^# 2\\. GERAR", box_script_content)
        if (length(start_idx_box) == 0) start_idx_box <- 14
        qmd_sections[["boxplot"]] <- c(
          "## Diagrama de Caixa (Boxplot)",
          sprintf("Comparação gráfica de dispersão e medianas para a variável **%s**.", box_var_y),
          "",
          "```{r}",
          "#| label: boxplot-consolidado",
          "#| echo: false",
          paste(box_script_content[start_idx_box:length(box_script_content)], collapse = "\n"),
          "```",
          ""
        )
      }
      
      # --- SEÇÃO 4: REGRESSÃO LINEAR SIMPLES ---
      if (isTRUE(input$exp_regression)) {
        reg_var_y <- input[["regression-var_y"]]
        if (is.null(reg_var_y)) {
          num_cols <- names(df_clean)[sapply(df_clean, is.numeric)]
          reg_var_y <- if (length(num_cols) > 0) num_cols[1] else names(df_clean)[1]
        }
        
        reg_var_x <- input[["regression-var_x"]]
        if (is.null(reg_var_x)) {
          num_cols <- names(df_clean)[sapply(df_clean, is.numeric)]
          reg_var_x <- if (length(num_cols) > 1) num_cols[2] else (if (length(num_cols) > 0) num_cols[1] else names(df_clean)[1])
        }
        
        reg_var_group <- input[["regression-var_group"]]
        if (is.null(reg_var_group)) reg_var_group <- "none"
        
        show_eq <- input[["regression-show_eq"]]
        if (is.null(show_eq)) show_eq <- TRUE
        
        show_out_coef <- input[["regression-show_out_coef"]]
        if (is.null(show_out_coef)) show_out_coef <- TRUE
        
        show_out_metrics <- input[["regression-show_out_metrics"]]
        if (is.null(show_out_metrics)) show_out_metrics <- TRUE
        
        show_out_fit_plot <- input[["regression-show_out_fit_plot"]]
        if (is.null(show_out_fit_plot)) show_out_fit_plot <- TRUE
        
        show_out_resid_plot <- input[["regression-show_out_resid_plot"]]
        if (is.null(show_out_resid_plot)) show_out_resid_plot <- TRUE
        
        show_out_qq_plot <- input[["regression-show_out_qq_plot"]]
        if (is.null(show_out_qq_plot)) show_out_qq_plot <- TRUE
        
        reg_theme <- input[["regression-graph_theme"]]
        if (is.null(reg_theme)) reg_theme <- "minimal"
        
        reg_title <- input[["regression-custom_title"]]
        if (is.null(reg_title) || !nzchar(reg_title)) {
          reg_title <- paste("Ajuste Linear:", reg_var_y, "vs", reg_var_x)
        }
        
        reg_label_x <- input[["regression-custom_label_x"]]
        if (is.null(reg_label_x) || !nzchar(reg_label_x)) reg_label_x <- reg_var_x
        
        reg_label_y <- input[["regression-custom_label_y"]]
        if (is.null(reg_label_y) || !nzchar(reg_label_y)) reg_label_y <- reg_var_y
        
        reg_theme_code <- switch(reg_theme,
                                 "minimal" = "theme_minimal(base_size = 14)",
                                 "classic" = "theme_classic(base_size = 14)",
                                 "bw"      = "theme_bw(base_size = 14)",
                                 "gray"    = "theme_gray(base_size = 14)",
                                 "light"   = "theme_light(base_size = 14)",
                                 "theme_minimal(base_size = 14)")
        
        reg_script_content <- c(
          "# --- SCRIPT DE ANÁLISE DE REGRESSÃO (Consolidado) ---",
          "library(ggplot2)",
          "",
          "# 1. CARREGAR OS DADOS LIMPOS",
          "# (Carrega o arquivo RDA que preserva fatores e formatação)",
          "load('dados/dados_limpos.rda')",
          "dados <- df_clean",
          "",
          "# Alternativa em formato aberto CSV (se preferir):",
          "# library(readr)",
          "# dados <- read_csv('dados/dados_limpos.csv')",
          "",
          "# 2. AJUSTAR O MODELO LINEAR",
          sprintf("modelo <- lm(`%s` ~ `%s`, data = dados)", reg_var_y, reg_var_x),
          "print(summary(modelo))",
          "",
          "# 3. GERAR O GRÁFICO DA RETA AJUSTADA",
          if (reg_var_group != "none") {
            c(
              sprintf("dados$`%s` <- as.factor(dados$`%s`)", reg_var_group, reg_var_group),
              sprintf("ggplot(dados, aes(x = `%s`, y = `%s`, color = `%s`, fill = `%s`)) +", reg_var_x, reg_var_y, reg_var_group, reg_var_group),
              "  geom_point(alpha = 0.8, size = 2.5) +",
              "  geom_smooth(method = 'lm', formula = y ~ x, size = 1.2) +"
            )
          } else {
            c(
              sprintf("ggplot(dados, aes(x = `%s`, y = `%s`)) +", reg_var_x, reg_var_y),
              "  geom_point(color = '#495057', alpha = 0.7, size = 2.5) +",
              "  geom_smooth(method = 'lm', formula = y ~ x, color = '#0d6efd', fill = '#cfe2ff', size = 1.2) +"
            )
          },
          sprintf("  %s +", reg_theme_code),
          "  labs(",
          sprintf("    title = '%s',", reg_title),
          if (show_eq) {
            fit_tmp <- tryCatch(lm(as.formula(paste0("`", reg_var_y, "` ~ `", reg_var_x, "`")), data = df_clean), error = function(e) NULL)
            if (!is.null(fit_tmp)) {
              coefs <- coef(fit_tmp)
              sprintf("    subtitle = 'Y = %.4f + (%.4f) * X',", coefs[1], coefs[2])
            } else {
              "    subtitle = 'Equação ajustada',"
            }
          } else {
            NULL
          },
          sprintf("    x = '%s',", reg_label_x),
          sprintf("    y = '%s'", reg_label_y),
          "  ) +",
          "  theme(",
          "    plot.title = element_text(face = 'bold', size = 16, color = '#212529'),",
          "    plot.subtitle = element_text(color = '#0d6efd', face = 'italic', size = 13)",
          "  )",
          "",
          "# 4. GRÁFICO DE RESÍDUOS VS VALORES AJUSTADOS",
          "diag_data <- data.frame(Ajustados = fitted(modelo), Residuos = residuals(modelo))",
          "ggplot(diag_data, aes(x = Ajustados, y = Residuos)) +",
          "  geom_point(color = '#495057', alpha = 0.7, size = 2.5) +",
          "  geom_hline(yintercept = 0, linetype = 'dashed', color = '#dc3545', size = 1) +",
          "  geom_smooth(method = 'loess', formula = y ~ x, color = '#198754', fill = '#d1e7dd', se = FALSE, size = 1) +",
          sprintf("  %s +", reg_theme_code),
          "  labs(title = 'Resíduos vs Valores Ajustados', x = 'Valores Ajustados (Fitted)', y = 'Resíduos (Residuals)') +",
          "  theme(plot.title = element_text(face = 'bold', size = 16, color = '#212529'))",
          "",
          "# 5. GRÁFICO DE NORMALIDADE (Q-Q PLOT)",
          "diag_data_qq <- data.frame(ResiduosStd = rstandard(modelo))",
          "ggplot(diag_data_qq, aes(sample = ResiduosStd)) +",
          "  stat_qq(color = '#495057', alpha = 0.7, size = 2.5) +",
          "  stat_qq_line(color = '#0d6efd', size = 1) +",
          sprintf("  %s +", reg_theme_code),
          "  labs(title = 'Normal Q-Q Plot', x = 'Quantis Teóricos', y = 'Resíduos Padronizados') +",
          "  theme(plot.title = element_text(face = 'bold', size = 16, color = '#212529'))"
        )
        writeLines(reg_script_content, file.path(dir_scripts, "4_regressao_linear.R"))
        scripts_incluidos <- c(scripts_incluidos, "scripts/4_regressao_linear.R")
        
        # Seção QMD
        qmd_reg <- c(
          "## Regressão Linear Simples",
          sprintf("Análise da relação de causalidade ou correlação linear entre a variável explicada **%s** e a explicativa **%s**.", reg_var_y, reg_var_x),
          "",
          "```{r}",
          "#| label: regression-setup-consolidado",
          "#| include: false",
          sprintf("modelo <- lm(`%s` ~ `%s`, data = dados)", reg_var_y, reg_var_x),
          "```",
          ""
        )
        if (show_out_coef) {
          qmd_reg <- c(qmd_reg,
            "### Tabela de Coeficientes do Modelo",
            "```{r}",
            "#| label: regression-coef-consolidado",
            "#| echo: false",
            "coef_df <- as.data.frame(summary(modelo)$coefficients)",
            "names(coef_df) <- c('Estimativa', 'Erro Padrão', 'Valor t', 'p-valor')",
            "knitr::kable(coef_df, digits = 4, caption = 'Coeficientes do Modelo Ajustado')",
            "```",
            ""
          )
        }
        if (show_out_metrics) {
          qmd_reg <- c(qmd_reg,
            "### Métricas de Ajuste Global",
            "```{r}",
            "#| label: regression-metrics-consolidado",
            "#| echo: false",
            "#| comment: NA",
            "summary(modelo)",
            "```",
            ""
          )
        }
        if (show_out_fit_plot) {
          fit_tmp <- tryCatch(lm(as.formula(paste0("`", reg_var_y, "` ~ `", reg_var_x, "`")), data = df_clean), error = function(e) NULL)
          subtitle_expr <- if (show_eq && !is.null(fit_tmp)) {
            coefs <- coef(fit_tmp)
            sprintf("Y = %.4f + (%.4f) * X", coefs[1], coefs[2])
          } else {
            ""
          }
          qmd_reg <- c(qmd_reg,
            "### Reta Ajustada",
            if (reg_var_group != "none") {
              c(
                "```{r}",
                "#| label: regression-fit-plot-consolidado",
                "#| echo: false",
                sprintf("dados$`%s` <- as.factor(dados$`%s`)", reg_var_group, reg_var_group),
                sprintf("ggplot(dados, aes(x = `%s`, y = `%s`, color = `%s`, fill = `%s`)) +", reg_var_x, reg_var_y, reg_var_group, reg_var_group),
                "  geom_point(alpha = 0.8, size = 2.5) +",
                "  geom_smooth(method = 'lm', formula = y ~ x, size = 1.2) +"
              )
            } else {
              c(
                "```{r}",
                "#| label: regression-fit-plot-consolidado",
                "#| echo: false",
                sprintf("ggplot(dados, aes(x = `%s`, y = `%s`)) +", reg_var_x, reg_var_y),
                "  geom_point(color = '#495057', alpha = 0.7, size = 2.5) +",
                "  geom_smooth(method = 'lm', formula = y ~ x, color = '#0d6efd', fill = '#cfe2ff', size = 1.2) +"
              )
            },
            sprintf("  %s +", reg_theme_code),
            "  labs(",
            sprintf("    title = '%s',", reg_title),
            if (nzchar(subtitle_expr)) sprintf("    subtitle = '%s',", subtitle_expr) else NULL,
            sprintf("    x = '%s',", reg_label_x),
            sprintf("    y = '%s'", reg_label_y),
            "  ) +",
            "  theme(plot.title = element_text(face = 'bold'))",
            "```",
            ""
          )
        }
        if (show_out_resid_plot) {
          qmd_reg <- c(qmd_reg,
            "### Análise de Diagnóstico: Resíduos vs Valores Ajustados",
            "```{r}",
            "#| label: regression-resid-plot-consolidado",
            "#| echo: false",
            "diag_data <- data.frame(Ajustados = fitted(modelo), Residuos = residuals(modelo))",
            "ggplot(diag_data, aes(x = Ajustados, y = Residuos)) +",
            "  geom_point(color = '#495057', alpha = 0.7, size = 2.5) +",
            "  geom_hline(yintercept = 0, linetype = 'dashed', color = '#dc3545', size = 1) +",
            "  geom_smooth(method = 'loess', formula = y ~ x, color = '#198754', fill = '#d1e7dd', se = FALSE, size = 1) +",
            sprintf("  %s +", reg_theme_code),
            "  labs(title = 'Resíduos vs Valores Ajustados', x = 'Valores Ajustados', y = 'Resíduos') +",
            "  theme(plot.title = element_text(face = 'bold'))",
            "```",
            ""
          )
        }
        if (show_out_qq_plot) {
          qmd_reg <- c(qmd_reg,
            "### Normalidade dos Resíduos: Normal Q-Q Plot",
            "```{r}",
            "#| label: regression-qq-plot-consolidado",
            "#| echo: false",
            "diag_data_qq <- data.frame(ResiduosStd = rstandard(modelo))",
            "ggplot(diag_data_qq, aes(sample = ResiduosStd)) +",
            "  stat_qq(color = '#495057', alpha = 0.7, size = 2.5) +",
            "  stat_qq_line(color = '#0d6efd', size = 1) +",
            sprintf("  %s +", reg_theme_code),
            "  labs(title = 'Normal Q-Q Plot', x = 'Quantis Teóricos', y = 'Resíduos Padronizados') +",
            "  theme(plot.title = element_text(face = 'bold'))",
            "```",
            ""
          )
        }
        qmd_sections[["regression"]] <- qmd_reg
      }
      
      # --- SEÇÃO: REGRESSÃO NÃO LINEAR (CURVAS DE CRESCIMENTO) ---
      gerar_secao_curva <- function(tipo_curva, label_curva, exp_input, var_y_id, var_x_id) {
        if (isTRUE(input[[exp_input]])) {
          file.copy("templates/funcoes_crescimento.R", file.path(dir_scripts, "funcoes_crescimento.R"), overwrite = TRUE)
          var_y <- input[[var_y_id]]
          var_x <- input[[var_x_id]]
          if (is.null(var_y)) var_y <- names(df_clean)[sapply(df_clean, is.numeric)][1]
          if (is.null(var_x)) var_x <- names(df_clean)[sapply(df_clean, is.numeric)][2]
          
          script_name <- paste0("4_regressao_nao_linear_", tipo_curva, ".R")
          
          c_script <- c(
            sprintf("# --- SCRIPT DE REGRESSÃO NÃO LINEAR (%s) ---", label_curva),
            "library(ggplot2)",
            "library(tibble)",
            "source('scripts/funcoes_crescimento.R')",
            "load('dados/dados_limpos.rda')",
            "dados <- df_clean",
            "",
            sprintf("fit_res <- ajustar_curva(dados, var_y = '%s', var_x = '%s', tipo = '%s')", var_y, var_x, tipo_curva),
            "print(mostrar_coefs_curva(fit_res))",
            "print(mostrar_metricas_curva(fit_res))",
            "print(mostrar_normalidade_curva(fit_res))",
            "cat(relatar_curva(fit_res))",
            "",
            "# Gráfico com a curva predita",
            "grid <- curva_predita(fit_res)",
            sprintf("ggplot(dados, aes(x = `%s`, y = `%s`)) +", var_x, var_y),
            "  geom_point(color = '#495057', alpha = 0.7, size = 2.5) +",
            sprintf("  geom_line(data = grid, aes(x = `%s`, y = `%s`), color = '#0d6efd', size = 1.2) +", var_x, var_y),
            "  theme_minimal() +",
            sprintf("  labs(title = 'Ajuste: %s', x = '%s', y = '%s')", label_curva, var_x, var_y)
          )
          
          writeLines(c_script, file.path(dir_scripts, script_name))
          scripts_incluidos <<- c(scripts_incluidos, paste0("scripts/", script_name))
          
          qmd_sections[[tipo_curva]] <<- c(
            sprintf("## Regressão Não Linear: %s", label_curva),
            sprintf("Ajuste do modelo não linear para descrever **%s** em função de **%s**.", var_y, var_x),
            "",
            "```{r}",
            sprintf("#| label: curve-%s-setup", tipo_curva),
            "#| include: false",
            "source('../scripts/funcoes_crescimento.R')",
            sprintf("fit_res <- ajustar_curva(dados, '%s', '%s', '%s')", var_y, var_x, tipo_curva),
            "```",
            "",
            "### Coeficientes Estimados",
            "```{r}",
            sprintf("#| label: curve-%s-coefs", tipo_curva),
            "#| output: asis",
            "knitr::kable(mostrar_coefs_curva(fit_res), digits = 5, caption = 'Estimativas dos Parâmetros')",
            "```",
            "",
            "### Métricas de Ajuste",
            "```{r}",
            sprintf("#| label: curve-%s-metrics", tipo_curva),
            "#| output: asis",
            "knitr::kable(mostrar_metricas_curva(fit_res), digits = 4, caption = 'Métricas de Ajuste do Modelo')",
            "```",
            "",
            "### Relato Descritivo",
            "> `r relatar_curva(fit_res)`",
            "",
            "### Pressupostos (Normalidade dos Resíduos)",
            "```{r}",
            sprintf("#| label: curve-%s-pressupostos", tipo_curva),
            "#| output: asis",
            "knitr::kable(mostrar_normalidade_curva(fit_res), digits = 4, caption = 'Teste de Normalidade dos Resíduos')",
            "```",
            "",
            "### Representação Visual",
            "```{r}",
            sprintf("#| label: curve-%s-plot", tipo_curva),
            "#| echo: false",
            "grid <- curva_predita(fit_res)",
            sprintf("ggplot(dados, aes(x = `%s`, y = `%s`)) +", var_x, var_y),
            "  geom_point(color = '#495057', alpha = 0.7, size = 2.5) +",
            sprintf("  geom_line(data = grid, aes(x = `%s`, y = `%s`), color = '#0d6efd', size = 1.2) +", var_x, var_y),
            "  theme_minimal() +",
            sprintf("  labs(x = '%s', y = '%s')", var_x, var_y),
            "```",
            ""
          )
        }
      }
      
      gerar_secao_curva("exponencial", "Curva Exponencial", "exp_exponencial", "exponencial-var_y", "exponencial-var_x")
      gerar_secao_curva("potencia", "Curva de Modelo de Potência", "exp_potencia", "potencia-var_y", "potencia-var_x")
      gerar_secao_curva("von_bertalanffy", "Curva de Crescimento de Peixes (Von Bertalanffy)", "exp_von_bertalanffy", "von_bertalanffy-var_y", "von_bertalanffy-var_x")
      gerar_secao_curva("polinomial", "Modelo Polinomial (Regressão Quadrática)", "exp_polinomial", "polinomial-var_y", "polinomial-var_x")
      gerar_secao_curva("logaritmica", "Curva Logarítmica", "exp_logaritmica", "logaritmica-var_y", "logaritmica-var_x")
      gerar_secao_curva("logistico", "Curva Logística", "exp_logistico", "logistico-var_y", "logistico-var_x")
      
      # --- SEÇÃO 5: TESTE T DE STUDENT ---
      if (isTRUE(input$exp_parametric)) {
        test_type <- input[["parametric-test_type"]]
        if (is.null(test_type)) test_type <- "one_val"
        
        conf_level <- input[["parametric-conf_level"]]
        if (is.null(conf_level)) conf_level <- 95
        conf_level_decimal <- conf_level / 100
        
        alt_val <- input[["parametric-alternative"]]
        if (is.null(alt_val)) alt_val <- "two.sided"
        
        graph_theme <- input[["parametric-graph_theme"]]
        if (is.null(graph_theme)) graph_theme <- "minimal"
        
        theme_code <- switch(graph_theme,
                             "minimal" = "theme_minimal(base_size = 14)",
                             "classic" = "theme_classic(base_size = 14)",
                             "bw"      = "theme_bw(base_size = 14)",
                             "gray"    = "theme_gray(base_size = 14)",
                             "light"   = "theme_light(base_size = 14)",
                             "theme_minimal(base_size = 14)")
        
        t_script_content <- c(
          "# --- SCRIPT DE TESTE T DE STUDENT (Consolidado) ---",
          "library(ggplot2)",
          "",
          "# 1. CARREGAR OS DADOS LIMPOS",
          "# (Carrega o arquivo RDA que preserva fatores e formatação)",
          "load('dados/dados_limpos.rda')",
          "dados <- df_clean",
          "",
          "# Alternativa em formato aberto CSV (se preferir):",
          "# library(readr)",
          "# dados <- read_csv('dados/dados_limpos.csv')",
          ""
        )
        
        qmd_t <- c(
          "## Teste t de Student",
          "Abaixo estão as hipóteses e resultados do Teste t de Student.",
          ""
        )
        
        if (test_type == "one_val") {
          one_var_y <- input[["parametric-one_var_y"]]
          if (is.null(one_var_y)) one_var_y <- names(df_clean)[sapply(df_clean, is.numeric)][1]
          one_mu <- input[["parametric-one_mu"]]
          if (is.null(one_mu)) one_mu <- 0
          
          t_script_content <- c(t_script_content,
            "# 2. EXECUÇÃO DO TESTE T DE UMA AMOSTRA",
            sprintf("resultado <- t.test(dados$`%s`, mu = %s, alternative = '%s', conf.level = %s)", 
                    one_var_y, one_mu, alt_val, conf_level_decimal),
            "print(resultado)",
            "",
            "# 3. GRÁFICO DO TESTE",
            sprintf("ggplot(dados, aes(x = '', y = `%s`)) +", one_var_y),
            "  geom_boxplot(fill = '#cfe2ff', color = '#0d6efd', alpha = 0.7, outlier.color = NA) +",
            "  geom_jitter(color = '#495057', width = 0.15, alpha = 0.5) +",
            sprintf("  geom_hline(aes(yintercept = mean(`%s`, na.rm=TRUE), color = 'Média Amostral'), linewidth = 1.2) +", one_var_y),
            sprintf("  geom_hline(aes(yintercept = %s, color = 'Média Hipotética'), linetype = 'dashed', linewidth = 1.2) +", one_mu),
            "  scale_color_manual(name = 'Referências', values = c('Média Amostral'='#0d6efd', 'Média Hipotética'='#dc3545')) +",
            sprintf("  %s +", theme_code),
            sprintf("  labs(title = 'Teste t de Uma Amostra: %s', y = '%s', x = '')", one_var_y, one_var_y)
          )
          
          qmd_t <- c(qmd_t,
            "### Teste t de Uma Amostra",
            sprintf("Verificação de hipótese para a média da variável **%s** contra o valor de referência **%s**.", one_var_y, one_mu),
            "",
            "```{r}",
            "#| label: test-t-one-sample",
            "#| echo: false",
            sprintf("resultado <- t.test(dados$`%s`, mu = %s, alternative = '%s', conf.level = %s)", 
                    one_var_y, one_mu, alt_val, conf_level_decimal),
            "print(resultado)",
            "```",
            "",
            "### Gráfico de Distribuição e Média Hipotética",
            "```{r}",
            "#| label: test-t-one-sample-plot",
            "#| echo: false",
            sprintf("ggplot(dados, aes(x = '', y = `%s`)) +", one_var_y),
            "  geom_boxplot(fill = '#cfe2ff', color = '#0d6efd', alpha = 0.7, outlier.color = NA) +",
            "  geom_jitter(color = '#495057', width = 0.15, alpha = 0.5) +",
            sprintf("  geom_hline(aes(yintercept = mean(`%s`, na.rm=TRUE), color = 'Média Amostral'), linewidth = 1.2) +", one_var_y),
            sprintf("  geom_hline(aes(yintercept = %s, color = 'Média Hipotética'), linetype = 'dashed', linewidth = 1.2) +", one_mu),
            "  scale_color_manual(name = 'Referências', values = c('Média Amostral'='#0d6efd', 'Média Hipotética'='#dc3545')) +",
            sprintf("  %s +", theme_code),
            sprintf("  labs(title = 'Teste t de Uma Amostra: %s', y = '%s', x = '')", one_var_y, one_var_y),
            "```",
            ""
          )
        } else if (test_type == "two_ind") {
          two_var_y <- input[["parametric-two_var_y"]]
          if (is.null(two_var_y)) two_var_y <- names(df_clean)[sapply(df_clean, is.numeric)][1]
          two_var_x <- input[["parametric-two_var_x"]]
          if (is.null(two_var_x)) two_var_x <- names(df_clean)[!sapply(df_clean, is.numeric)][1]
          two_var_equal <- input[["parametric-two_var_equal"]]
          if (is.null(two_var_equal)) two_var_equal <- FALSE
          
          t_script_content <- c(t_script_content,
            "# 2. EXECUÇÃO DO TESTE T DE DUAS AMOSTRAS INDEPENDENTES",
            sprintf("dados$`%s` <- as.factor(dados$`%s`)", two_var_x, two_var_x),
            sprintf("resultado <- t.test(`%s` ~ `%s`, data = dados, alternative = '%s', conf.level = %s, var.equal = %s)",
                    two_var_y, two_var_x, alt_val, conf_level_decimal, as.character(two_var_equal)),
            "print(resultado)",
            "",
            "# 3. GRÁFICO DO TESTE",
            sprintf("ggplot(dados, aes(x = `%s`, y = `%s`, fill = `%s`)) +", two_var_x, two_var_y, two_var_x),
            "  geom_boxplot(alpha = 0.7, outlier.color = NA) +",
            "  geom_jitter(color = '#495057', width = 0.15, alpha = 0.5) +",
            "  stat_summary(fun = mean, geom = 'point', shape = 23, size = 4, fill = 'white') +",
            sprintf("  %s +", theme_code),
            sprintf("  labs(title = 'Comparação de Médias: %s por %s', x = '%s', y = '%s')", 
                    two_var_y, two_var_x, two_var_x, two_var_y),
            "theme(legend.position='none')"
          )
          
          qmd_t <- c(qmd_t,
            "### Teste t de Duas Amostras Independentes",
            sprintf("Comparação de médias da variável dependente **%s** entre os grupos definidos por **%s**.", two_var_y, two_var_x),
            "",
            "```{r}",
            "#| label: test-t-two-samples",
            "#| echo: false",
            sprintf("dados$`%s` <- as.factor(dados$`%s`)", two_var_x, two_var_x),
            sprintf("resultado <- t.test(`%s` ~ `%s`, data = dados, alternative = '%s', conf.level = %s, var.equal = %s)",
                    two_var_y, two_var_x, alt_val, conf_level_decimal, as.character(two_var_equal)),
            "print(resultado)",
            "```",
            "",
            "### Gráfico de Comparação",
            "```{r}",
            "#| label: test-t-two-samples-plot",
            "#| echo: false",
            sprintf("ggplot(dados, aes(x = `%s`, y = `%s`, fill = `%s`)) +", two_var_x, two_var_y, two_var_x),
            "  geom_boxplot(alpha = 0.7, outlier.color = NA) +",
            "  geom_jitter(color = '#495057', width = 0.15, alpha = 0.5) +",
            "  stat_summary(fun = mean, geom = 'point', shape = 23, size = 4, fill = 'white') +",
            sprintf("  %s +", theme_code),
            sprintf("  labs(title = 'Comparação de Médias: %s por %s', x = '%s', y = '%s') +", 
                    two_var_y, two_var_x, two_var_x, two_var_y),
            "  theme(legend.position='none')",
            "```",
            ""
          )
        } else if (test_type == "paired") {
          pair_var_y1 <- input[["parametric-pair_var_y1"]]
          if (is.null(pair_var_y1)) pair_var_y1 <- names(df_clean)[sapply(df_clean, is.numeric)][1]
          pair_var_y2 <- input[["parametric-pair_var_y2"]]
          if (is.null(pair_var_y2)) pair_var_y2 <- names(df_clean)[sapply(df_clean, is.numeric)][2]
          
          t_script_content <- c(t_script_content,
            "# 2. EXECUÇÃO DO TESTE T PAREADO",
            sprintf("resultado <- t.test(dados$`%s`, dados$`%s`, paired = TRUE, alternative = '%s', conf.level = %s)",
                    pair_var_y1, pair_var_y2, alt_val, conf_level_decimal),
            "print(resultado)",
            "",
            "# 3. GRÁFICO DO TESTE",
            "df_long <- data.frame(",
            "  ID = rep(1:nrow(dados), 2),",
            sprintf("  Condicao = factor(rep(c('%s', '%s'), each = nrow(dados)), levels = c('%s', '%s')),", 
                    pair_var_y1, pair_var_y2, pair_var_y1, pair_var_y2),
            sprintf("  Valores = c(dados$`%s`, dados$`%s`)", pair_var_y1, pair_var_y2),
            ")",
            "ggplot(df_long, aes(x = Condicao, y = Valores, group = ID)) +",
            "  geom_line(color = 'gray70', alpha = 0.6) +",
            "  geom_point(aes(color = Condicao), size = 2.5) +",
            "  geom_boxplot(aes(group = Condicao), fill = NA, outlier.color = NA, width = 0.3) +",
            sprintf("  %s +", theme_code),
            sprintf("  labs(title = 'Comparação Pareada: %s vs %s', x = 'Condição', y = 'Valores') +", 
                    pair_var_y1, pair_var_y2),
            "  theme(legend.position='none')"
          )
          
          qmd_t <- c(qmd_t,
            "### Teste t Pareado",
            sprintf("Comparação de médias das observações pareadas entre **%s** e **%s**.", pair_var_y1, pair_var_y2),
            "",
            "```{r}",
            "#| label: test-t-paired",
            "#| echo: false",
            sprintf("resultado <- t.test(dados$`%s`, dados$`%s`, paired = TRUE, alternative = '%s', conf.level = %s)",
                    pair_var_y1, pair_var_y2, alt_val, conf_level_decimal),
            "print(resultado)",
            "```",
            "",
            "### Gráfico de Evolução Pareada",
            "```{r}",
            "#| label: test-t-paired-plot",
            "#| echo: false",
            "df_long <- data.frame(",
            "  ID = rep(1:nrow(dados), 2),",
            sprintf("  Condicao = factor(rep(c('%s', '%s'), each = nrow(dados)), levels = c('%s', '%s')),", 
                    pair_var_y1, pair_var_y2, pair_var_y1, pair_var_y2),
            sprintf("  Valores = c(dados$`%s`, dados$`%s`)", pair_var_y1, pair_var_y2),
            ")",
            "ggplot(df_long, aes(x = Condicao, y = Valores, group = ID)) +",
            "  geom_line(color = 'gray70', alpha = 0.6) +",
            "  geom_point(aes(color = Condicao), size = 2.5) +",
            "  geom_boxplot(aes(group = Condicao), fill = NA, outlier.color = NA, width = 0.3) +",
            sprintf("  %s +", theme_code),
            sprintf("  labs(title = 'Comparação Pareada: %s vs %s', x = 'Condição', y = 'Valores') +", 
                    pair_var_y1, pair_var_y2),
            "  theme(legend.position='none')",
            "```",
            ""
          )
        }
        
        writeLines(t_script_content, file.path(dir_scripts, "5_teste_t.R"))
        scripts_incluidos <- c(scripts_incluidos, "scripts/5_teste_t.R")
        qmd_sections[["parametric"]] <- qmd_t
      }
      
      # --- SEÇÃO: AMOSTRAGEM AAS ---
      if (isTRUE(input$exp_aas)) {
        file.copy("templates/funcoes_sampling.R", file.path(dir_scripts, "funcoes_sampling.R"), overwrite = TRUE)
        var_y <- input[["aas-var_y"]]
        if (is.null(var_y)) var_y <- names(df_clean)[sapply(df_clean, is.numeric)][1]
        sample_n <- input[["aas-sample_n"]]
        if (is.null(sample_n)) sample_n <- 10
        seed <- input[["aas-seed"]]
        if (is.null(seed)) seed <- 42
        
        aas_script_content <- c(
          "# --- SCRIPT DE AMOSTRAGEM ALEATÓRIA SIMPLES (AAS) ---",
          "source('scripts/funcoes_sampling.R')",
          "load('dados/dados_limpos.rda')",
          "dados <- df_clean",
          "",
          sprintf("r_aas <- calcular_aas(dados, var_y = '%s', n = %d, seed = %d)", var_y, sample_n, seed),
          "print(mostrar_aas_estimativas(r_aas))",
          "print(head(r_aas$amostra, 15))"
        )
        writeLines(aas_script_content, file.path(dir_scripts, "6_amostragem_aas.R"))
        scripts_incluidos <- c(scripts_incluidos, "scripts/6_amostragem_aas.R")
        
        qmd_sections[["aas"]] <- c(
          "## Amostragem Aleatória Simples (AAS)",
          "Resultados do sorteio de amostra por AAS e estimativas populacionais associadas.",
          "",
          "```{r}",
          "#| label: aas-consolidado",
          "source('../scripts/funcoes_sampling.R')",
          sprintf("r_aas <- calcular_aas(dados, '%s', %d, %d)", var_y, sample_n, seed),
          "knitr::kable(mostrar_aas_estimativas(r_aas), digits = 3, caption = 'Estimativas da AAS')",
          "```",
          "",
          "### Valores Sorteados (Primeiras Linhas)",
          "```{r}",
          "#| label: aas-dados-sorteados",
          "knitr::kable(head(r_aas$amostra, 15), digits = 3, caption = 'Dados Amostrados (AAS)')",
          "```",
          ""
        )
      }
      
      # --- SEÇÃO: AMOSTRAGEM AEP ---
      if (isTRUE(input$exp_aep)) {
        file.copy("templates/funcoes_sampling.R", file.path(dir_scripts, "funcoes_sampling.R"), overwrite = TRUE)
        var_y <- input[["aep-var_y"]]
        if (is.null(var_y)) var_y <- names(df_clean)[sapply(df_clean, is.numeric)][1]
        var_strata <- input[["aep-var_strata"]]
        if (is.null(var_strata)) var_strata <- names(df_clean)[sapply(df_clean, function(x) is.factor(x) || is.character(x))][1]
        sample_n <- input[["aep-sample_n"]]
        if (is.null(sample_n)) sample_n <- 10
        seed <- input[["aep-seed"]]
        if (is.null(seed)) seed <- 42
        
        aep_script_content <- c(
          "# --- SCRIPT DE AMOSTRAGEM ESTRATIFICADA PROPORCIONAL (AEP) ---",
          "source('scripts/funcoes_sampling.R')",
          "load('dados/dados_limpos.rda')",
          "dados <- df_clean",
          "",
          sprintf("r_aep <- calcular_aep(dados, var_y = '%s', var_strata = '%s', n = %d, seed = %d)", var_y, var_strata, sample_n, seed),
          "print(mostrar_aep_estimativas(r_aep))",
          "print(mostrar_aep_alocacao(r_aep))"
        )
        writeLines(aep_script_content, file.path(dir_scripts, "7_amostragem_aep.R"))
        scripts_incluidos <- c(scripts_incluidos, "scripts/7_amostragem_aep.R")
        
        qmd_sections[["aep"]] <- c(
          "## Amostragem Estratificada Proporcional (AEP)",
          "Resultados do sorteio de amostra por AEP e estimativas populacionais associadas.",
          "",
          "```{r}",
          "#| label: aep-consolidado",
          "source('../scripts/funcoes_sampling.R')",
          sprintf("r_aep <- calcular_aep(dados, '%s', '%s', %d, %d)", var_y, var_strata, sample_n, seed),
          "knitr::kable(mostrar_aep_estimativas(r_aep), digits = 3, caption = 'Estimativas da AEP')",
          "```",
          "",
          "### Alocação de Amostras por Estrato",
          "```{r}",
          "#| label: aep-alocacao",
          "knitr::kable(mostrar_aep_alocacao(r_aep), digits = 3, caption = 'Alocação de Amostras por Estrato')",
          "```",
          ""
        )
      }
      
      # --- SEÇÃO: AMOSTRAGEM AS ---
      if (isTRUE(input$exp_as)) {
        file.copy("templates/funcoes_sampling.R", file.path(dir_scripts, "funcoes_sampling.R"), overwrite = TRUE)
        var_y <- input[["as-var_y"]]
        if (is.null(var_y)) var_y <- names(df_clean)[sapply(df_clean, is.numeric)][1]
        sample_n <- input[["as-sample_n"]]
        if (is.null(sample_n)) sample_n <- 10
        seed <- input[["as-seed"]]
        if (is.null(seed)) seed <- 42
        
        as_script_content <- c(
          "# --- SCRIPT DE AMOSTRAGEM SISTEMÁTICA (AS) ---",
          "source('scripts/funcoes_sampling.R')",
          "load('dados/dados_limpos.rda')",
          "dados <- df_clean",
          "",
          sprintf("r_as <- calcular_as(dados, var_y = '%s', n = %d, seed = %d)", var_y, sample_n, seed),
          "print(mostrar_as_estimativas(r_as))",
          "print(head(r_as$amostra, 15))"
        )
        writeLines(as_script_content, file.path(dir_scripts, "8_amostragem_as.R"))
        scripts_incluidos <- c(scripts_incluidos, "scripts/8_amostragem_as.R")
        
        qmd_sections[["as"]] <- c(
          "## Amostragem Sistemática (AS)",
          "Resultados do sorteio de amostra por AS e estimativas populacionais associadas.",
          "",
          "```{r}",
          "#| label: as-consolidado",
          "source('../scripts/funcoes_sampling.R')",
          sprintf("r_as <- calcular_as(dados, '%s', %d, %d)", var_y, sample_n, seed),
          "knitr::kable(mostrar_as_estimativas(r_as), digits = 3, caption = 'Estimativas da AS')",
          "```",
          "",
          "### Valores Sorteados (AS)",
          "```{r}",
          "#| label: as-dados-sorteados",
          "knitr::kable(head(r_as$amostra, 15), digits = 3, caption = 'Dados Amostrados (AS)')",
          "```",
          ""
        )
      }
      
      # --- SEÇÃO: TABELA DE CONTINGÊNCIA ---
      if (isTRUE(input$exp_contingency)) {
        file.copy("templates/funcoes_contingency.R", file.path(dir_scripts, "funcoes_contingency.R"), overwrite = TRUE)
        var_row <- input[["contingency-var_row"]]
        var_col <- input[["contingency-var_col"]]
        pct_type <- input[["contingency-pct_type"]]
        if (is.null(pct_type)) pct_type <- "none"
        
        conting_script_content <- c(
          "# --- SCRIPT DE TABELA DE CONTINGÊNCIA E ASSOCIAÇÃO ---",
          "source('scripts/funcoes_contingency.R')",
          "load('dados/dados_limpos.rda')",
          "dados <- df_clean",
          "",
          sprintf("r_conting <- calcular_contingencia(dados, var_row = '%s', var_col = '%s', pct_type = '%s')", var_row, var_col, pct_type),
          "print(mostrar_contingencia(r_conting))",
          "cat(relatar_contingencia(r_conting))"
        )
        writeLines(conting_script_content, file.path(dir_scripts, "9_tabela_contingencia.R"))
        scripts_incluidos <- c(scripts_incluidos, "scripts/9_tabela_contingencia.R")
        
        qmd_sections[["contingency"]] <- c(
          "## Tabela de Contingência e Teste Qui-Quadrado",
          "Cruzamento de frequências e teste de associação de Pearson.",
          "",
          "```{r}",
          "#| label: conting-consolidado",
          "source('../scripts/funcoes_contingency.R')",
          sprintf("r_conting <- calcular_contingencia(dados, '%s', '%s', '%s')", var_row, var_col, pct_type),
          "knitr::kable(mostrar_contingencia(r_conting), caption = 'Tabela de Contingência Cruzada')",
          "```",
          "",
          "### Relato do Teste Qui-Quadrado",
          "> `r relatar_contingencia(r_conting)`",
          ""
        )
      }
      
      # --- SEÇÃO: ANOVA ---
      if (isTRUE(input$exp_anova)) {
        file.copy("templates/funcoes_anova.R", file.path(dir_scripts, "funcoes_anova.R"), overwrite = TRUE)
        var_x <- input[["anova-var_x"]]
        var_y <- input[["anova-var_y"]]
        
        anova_script_content <- c(
          "# --- SCRIPT DE ANÁLISE DE VARIÂNCIA (ANOVA) ---",
          "source('scripts/funcoes_anova.R')",
          "load('dados/dados_limpos.rda')",
          "dados <- df_clean",
          "",
          sprintf("r_anova <- calcular_anova(dados, var_x = '%s', var_y = '%s')", var_x, var_y),
          "print(mostrar_anova_tab(r_anova))",
          "print(mostrar_anova_tukey(r_anova))",
          "print(mostrar_anova_pressupostos(r_anova))"
        )
        writeLines(anova_script_content, file.path(dir_scripts, "10_analise_anova.R"))
        scripts_incluidos <- c(scripts_incluidos, "scripts/10_analise_anova.R")
        
        qmd_sections[["anova"]] <- c(
          "## Análise de Variância (ANOVA)",
          "Comparação de médias entre três ou mais grupos.",
          "",
          "```{r}",
          "#| label: anova-consolidado",
          "source('../scripts/funcoes_anova.R')",
          sprintf("r_anova <- calcular_anova(dados, '%s', '%s')", var_x, var_y),
          "knitr::kable(mostrar_anova_tab(r_anova), digits = 4, caption = 'Tabela da ANOVA')",
          "```",
          "",
          "### Comparações Múltiplas (Tukey HSD)",
          "```{r}",
          "#| label: anova-tukey",
          "knitr::kable(mostrar_anova_tukey(r_anova), digits = 4, caption = 'Comparações de Tukey HSD')",
          "```",
          "",
          "### Verificação dos Pressupostos",
          "```{r}",
          "#| label: anova-pressupostos",
          "knitr::kable(mostrar_anova_pressupostos(r_anova), digits = 4, caption = 'Testes de Normalidade e Homocedasticidade')",
          "```",
          ""
        )
      }
      
      # --- SEÇÃO: PCA ---
      if (isTRUE(input$exp_pca)) {
        file.copy("templates/funcoes_pca.R", file.path(dir_scripts, "funcoes_pca.R"), overwrite = TRUE)
        vars_sel <- input[["pca-vars_selected"]]
        scale_val <- input[["pca-scale"]]
        if (is.null(scale_val)) scale_val <- TRUE
        vars_str <- paste(paste0("'", vars_sel, "'"), collapse = ", ")
        
        pca_script_content <- c(
          "# --- SCRIPT DE COMPONENTES PRINCIPAIS (PCA) ---",
          "source('scripts/funcoes_pca.R')",
          "load('dados/dados_limpos.rda')",
          "dados <- df_clean",
          "",
          sprintf("vars_sel <- c(%s)", vars_str),
          sprintf("r_pca <- calcular_pca(dados, vars_sel, scale = %s)", as.character(scale_val)),
          "print(mostrar_pca_var(r_pca))",
          "print(mostrar_pca_loadings(r_pca))"
        )
        writeLines(pca_script_content, file.path(dir_scripts, "11_analise_pca.R"))
        scripts_incluidos <- c(scripts_incluidos, "scripts/11_analise_pca.R")
        
        vars_str_qmd <- paste(paste0("'", vars_sel, "'"), collapse = ", ")
        qmd_sections[["pca"]] <- c(
          "## Análise de Componentes Principais (PCA)",
          "Redução de dimensionalidade linear.",
          "",
          "```{r}",
          "#| label: pca-consolidado",
          "source('../scripts/funcoes_pca.R')",
          sprintf("r_pca <- calcular_pca(dados, c(%s), scale = %s)", vars_str_qmd, as.character(scale_val)),
          "knitr::kable(mostrar_pca_var(r_pca), digits = 3, caption = 'Variância Explicada por Componente')",
          "```",
          "",
          "### Cargas dos Componentes (Loadings)",
          "```{r}",
          "#| label: pca-loadings-consolidado",
          "knitr::kable(mostrar_pca_loadings(r_pca), digits = 4, caption = 'Cargas dos Componentes Principais')",
          "```",
          ""
        )
      }
      
      # --- SEÇÃO: HCA ---
      if (isTRUE(input$exp_hca)) {
        file.copy("templates/funcoes_hca.R", file.path(dir_scripts, "funcoes_hca.R"), overwrite = TRUE)
        vars_sel <- input[["hca-vars_selected"]]
        dist_method <- input[["hca-distance_method"]]
        link_method <- input[["hca-linkage_method"]]
        k_groups <- input[["hca-k_groups"]]
        scale_val <- input[["hca-scale"]]
        if (is.null(scale_val)) scale_val <- TRUE
        vars_str <- paste(paste0("'", vars_sel, "'"), collapse = ", ")
        
        hca_script_content <- c(
          "# --- SCRIPT DE CLUSTERING HIERÁRQUICO (AAH / HCA) ---",
          "source('scripts/funcoes_hca.R')",
          "load('dados/dados_limpos.rda')",
          "dados <- df_clean",
          "",
          sprintf("vars_sel <- c(%s)", vars_str),
          sprintf("r_hca <- calcular_hca(dados, vars_sel, distance_method = '%s', linkage_method = '%s', k_groups = %d, scale = %s)", 
                  dist_method, link_method, k_groups, as.character(scale_val)),
          "print(mostrar_hca_perfil(r_hca))",
          "print(head(mostrar_hca_pertinencia(r_hca), 20))"
        )
        writeLines(hca_script_content, file.path(dir_scripts, "12_analise_aah.R"))
        scripts_incluidos <- c(scripts_incluidos, "scripts/12_analise_aah.R")
        
        vars_str_qmd <- paste(paste0("'", vars_sel, "'"), collapse = ", ")
        qmd_sections[["hca"]] <- c(
          "## Análise de Agrupamento Hierárquico (AAH)",
          "Classificação multivariada por agrupamento aglomerativo.",
          "",
          "```{r}",
          "#| label: hca-consolidado",
          "source('../scripts/funcoes_hca.R')",
          sprintf("r_hca <- calcular_hca(dados, c(%s), '%s', '%s', %d, %s)", 
                  vars_str_qmd, dist_method, link_method, k_groups, as.character(scale_val)),
          "knitr::kable(mostrar_hca_perfil(r_hca), digits = 3, caption = 'Perfil de Médias por Cluster')",
          "```",
          "",
          "### Dendrograma de Agrupamento",
          "```{r}",
          "#| label: hca-dendrograma-consolidado",
          "#| fig-width: 6.5",
          "#| fig-height: 4.5",
          "ocean_cols <- c('#0F3B5F', '#2E7D8F', '#62B6B7', '#E89B3C', '#E76F51')",
          sprintf("k_groups_val <- %d", k_groups),
          "border_cols <- ocean_cols[1:min(k_groups_val, length(ocean_cols))]",
          "if(k_groups_val > length(ocean_cols)) { border_cols <- c(border_cols, rainbow(k_groups_val - length(ocean_cols))) }",
          "plot(r_hca$fit, labels = FALSE, hang = -1, main = 'Dendrograma Hierárquico',",
          "     xlab = 'Observações', ylab = 'Altura')",
          "rect.hclust(r_hca$fit, k = k_groups_val, border = border_cols)",
          "```",
          ""
        )
      }
      
      # 3. Gerar arquivo Quarto Consolidado
      qmd_lines <- c(
        "---",
        "title: \"Relatório de Análises Consolidadas\"",
        "author: \"IDE CatalyseR - CatalyseR\"",
        sprintf("date: \"%s\"", format(Sys.Date(), "%d/%m/%Y")),
        "format:",
        "  html:",
        "    theme: cosmo",
        "    toc: true",
        "  docx:",
        "    toc: true",
        "  typst:",
        "    toc: true",
        "---",
        "",
        "```{r}",
        "#| label: setup",
        "#| include: false",
        "knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE, fig.width = 6, fig.height = 4, fig.align = 'center')",
        "library(ggplot2)",
        "",
        "# Carrega os dados limpos (preservando fatores e tipagem)",
        "load('../dados/dados_limpos.rda')",
        "dados <- df_clean",
        "",
        "# Alternativa em CSV:",
        "# library(readr)",
        "# dados <- read_csv('../dados/dados_limpos.csv')",
        "```",
        "",
        "## Introdução",
        "Este relatório apresenta a compilação das análises estatísticas executadas na IDE visual CatalyseR.",
        ""
      )
      
      # Adicionar as seções selecionadas sequencialmente na ordem em que aparecem no menu
      if (!is.null(qmd_sections[["aas"]])) {
        qmd_lines <- c(qmd_lines, "---", qmd_sections[["aas"]])
      }
      if (!is.null(qmd_sections[["aep"]])) {
        qmd_lines <- c(qmd_lines, "---", qmd_sections[["aep"]])
      }
      if (!is.null(qmd_sections[["as"]])) {
        qmd_lines <- c(qmd_lines, "---", qmd_sections[["as"]])
      }
      if (!is.null(qmd_sections[["contingency"]])) {
        qmd_lines <- c(qmd_lines, "---", qmd_sections[["contingency"]])
      }
      if (!is.null(qmd_sections[["descr_stats"]])) {
        qmd_lines <- c(qmd_lines, "---", qmd_sections[["descr_stats"]])
      }
      if (!is.null(qmd_sections[["histogram"]])) {
        qmd_lines <- c(qmd_lines, "---", qmd_sections[["histogram"]])
      }
      if (!is.null(qmd_sections[["boxplot"]])) {
        qmd_lines <- c(qmd_lines, "---", qmd_sections[["boxplot"]])
      }
      if (!is.null(qmd_sections[["regression"]])) {
        qmd_lines <- c(qmd_lines, "---", qmd_sections[["regression"]])
      }
      if (!is.null(qmd_sections[["exponencial"]])) {
        qmd_lines <- c(qmd_lines, "---", qmd_sections[["exponencial"]])
      }
      if (!is.null(qmd_sections[["potencia"]])) {
        qmd_lines <- c(qmd_lines, "---", qmd_sections[["potencia"]])
      }
      if (!is.null(qmd_sections[["von_bertalanffy"]])) {
        qmd_lines <- c(qmd_lines, "---", qmd_sections[["von_bertalanffy"]])
      }
      if (!is.null(qmd_sections[["polinomial"]])) {
        qmd_lines <- c(qmd_lines, "---", qmd_sections[["polinomial"]])
      }
      if (!is.null(qmd_sections[["logaritmica"]])) {
        qmd_lines <- c(qmd_lines, "---", qmd_sections[["logaritmica"]])
      }
      if (!is.null(qmd_sections[["logistico"]])) {
        qmd_lines <- c(qmd_lines, "---", qmd_sections[["logistico"]])
      }
      if (!is.null(qmd_sections[["parametric"]])) {
        qmd_lines <- c(qmd_lines, "---", qmd_sections[["parametric"]])
      }
      if (!is.null(qmd_sections[["anova"]])) {
        qmd_lines <- c(qmd_lines, "---", qmd_sections[["anova"]])
      }
      if (!is.null(qmd_sections[["pca"]])) {
        qmd_lines <- c(qmd_lines, "---", qmd_sections[["pca"]])
      }
      if (!is.null(qmd_sections[["hca"]])) {
        qmd_lines <- c(qmd_lines, "---", qmd_sections[["hca"]])
      }
      
      writeLines(paste(qmd_lines, collapse = "\n"), file.path(dir_relatorios, "relatorio_consolidado.qmd"))
      
      # 4. Criar Rproj
      rproj_content <- c(
        "Version: 1.0",
        "RestoreWorkspace: Default",
        "SaveWorkspace: Default",
        "AlwaysSaveHistory: Default",
        "EnableCodeIndexing: Yes",
        "UseSpacesForTab: Yes",
        "NumSpacesForTab: 2",
        "Encoding: UTF-8"
      )

      writeLines(rproj_content, file.path(proj_dir, "projeto_analise.Rproj"))
      
      # 5. Criar README.txt
      readme_content <- c(
        "===========================================================",
        " PACOTE DE ESTUDOS MULTI-ANÁLISE (CatalyseR)",
        "===========================================================",
        "",
        "Este pacote contém o ambiente de estudo consolidado da CatalyseR.",
        "",
        "ESTRUTURA DE PASTAS:",
        "- projeto_analise.Rproj: Dê duplo clique para abrir no RStudio.",
        "- dados/               : Dados limpos (.rda e .csv).",
        "- scripts/             : Scripts R executáveis de cada análise.",
        if (length(scripts_incluidos) > 0) paste(paste0("  * ", scripts_incluidos), collapse = "\n") else "  * (Nenhum script selecionado)",
        "- relatorios/relatorio_consolidado.qmd: Relatório Quarto unificado.",
        "",
        "INSTRUÇÕES DE USO:",
        "1. Abra 'projeto_analise.Rproj' no RStudio.",
        "2. Acesse os scripts na pasta 'scripts/' para rodar as análises linha a linha.",
        "3. Abra 'relatorios/relatorio_consolidado.qmd' e clique em 'Render' para compilar o relatório."
      )
      writeLines(readme_content, file.path(proj_dir, "README.txt"))

      # 6. Compactar
      old_wd <- getwd()
      setwd(temp_dir)
      utils::zip(file, files = proj_dir_name)
      setwd(old_wd)
    }
  )
}

# Inicializa o app
shinyApp(ui, server)
