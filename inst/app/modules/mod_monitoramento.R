# Delineamento observacional Monitoramento (série temporal): poucos locais
# medidos muitas vezes ao longo do tempo (desembarque, chuva, temperatura em
# estação fixa, maré). A tela entrega um protótipo de tabela tidy em Excel
# (abas dados e metadados, com fórmulas prontas nos índices) para o pesquisador
# preencher em campo e ajustar depois; a sub-aba Tabela mostra a tabela
# completa, paginada pelo DT. Formato próprio de três abas, fora do padrão de
# quatro sub-abas dos demais observacionais (costura do ARQUITETURA.md), e
# módulo autossuficiente: não lê nem grava na ficha de planejamento.
# Único módulo do app que usa openxlsx (fórmulas e cor de célula) e
# shinyWidgets (seletor de mês/ano); o restante segue com writexl e shiny.

# ---- Funções puras (testáveis fora do Shiny) --------------------------------

# Limpa um nome para virar coluna da tabela: minúsculas, sem acentos nem espaços.
limpar_nome <- function(texto) {
  # Troca letras acentuadas pela base ASCII (ç vira c, ã vira a).
  sem_acento <- iconv(texto, from = "UTF-8", to = "ASCII//TRANSLIT")
  # Se a conversão falhar num caractere estranho, segue com o texto original.
  sem_acento[is.na(sem_acento)] <- texto[is.na(sem_acento)]
  # Tudo que não for letra ou número vira um sublinhado (espaços e sinais inclusos).
  limpo <- gsub("[^a-z0-9]+", "_", tolower(trimws(sem_acento)))
  # Junta sublinhados repetidos e tira os que sobrarem nas pontas.
  limpo <- gsub("_+", "_", limpo)
  gsub("^_|_$", "", limpo)
}

# Primeiro dia do mês de uma data, base das sequências mensais e quinzenais.
primeiro_do_mes <- function(d) {
  # Recompõe a data só com ano e mês, fixando o dia 1.
  as.Date(format(d, "%Y-%m-01"))
}

# Todas as datas da série entre início e fim, conforme a frequência escolhida.
datas_da_serie <- function(inicio, fim, frequencia) {
  # Se as pontas vierem trocadas, fecha o período na data de início (orientar não é bloquear).
  if (fim < inicio) fim <- inicio
  # Mensal e quinzenal andam por mês fechado, então as pontas caem no dia 1.
  ini <- if (frequencia %in% c("mensal", "quinzenal")) primeiro_do_mes(inicio) else inicio
  fim_ref <- if (frequencia %in% c("mensal", "quinzenal")) primeiro_do_mes(fim) else fim
  # O switch despacha a frequência; não há if/else aninhado.
  switch(frequencia,
    # Diária: uma data por dia, do início ao fim.
    diaria = seq(ini, fim_ref, by = "1 day"),
    # Semanal: o seq() de semanas preserva o dia da semana da data de início.
    semanal = seq(ini, fim_ref, by = "1 week"),
    # Mensal: sempre o primeiro dia de cada mês do período.
    mensal = seq(ini, fim_ref, by = "1 month"),
    quinzenal = {
      # Um seq() mensal gera os dias 1; somando 15 dias temos os dias 16.
      meses <- seq(ini, fim_ref, by = "1 month")
      # Junta as duas quinzenas e ordena o calendário.
      candidatas <- sort(c(meses, meses + 15))
      # Mantém só as datas dentro do período, comparando mês com mês.
      candidatas[candidatas >= ini & primeiro_do_mes(candidatas) <= primeiro_do_mes(fim)]
    },
    stop("Frequência desconhecida.", call. = FALSE)
  )
}

# Reúne as definições da aba 1 numa configuração única, já com nomes de coluna.
montar_config <- function(inicio, fim, frequencia, horario, hora_real,
                          locais_raw, tem_esforco, unidade_esforco,
                          medidas_raw, indices_marcados) {
  # O calendário vem da função pura de datas.
  datas <- datas_da_serie(inicio, fim, frequencia)
  # Locais sem nome digitado ganham um rótulo numerado, para a tabela nunca ficar sem coluna.
  locais <- trimws(locais_raw)
  locais <- ifelse(nzchar(locais), locais, sprintf("local_%d", seq_along(locais)))
  # Só entram as medidas com nome preenchido; linha vazia é ignorada, sem erro.
  medidas <- medidas_raw[nzchar(trimws(medidas_raw$nome)), , drop = FALSE]
  # A coluna da medida junta nome e unidade já limpos (captura + kg vira captura_kg).
  medidas$coluna <- vapply(seq_len(nrow(medidas)), function(i) {
    limpar_nome(paste(trimws(medidas$nome[i]), trimws(medidas$unidade[i])))
  }, character(1))
  # A unidade limpa do esforço serve ao nome da coluna e ao sufixo dos índices.
  uni <- limpar_nome(unidade_esforco)
  # Sem unidade digitada, a coluna fica só "esforco".
  esforco_col <- if (tem_esforco) paste0("esforco_", if (nzchar(uni)) uni else "esforco") else NULL
  # O sufixo do índice repete a unidade do esforço (captura_kg_por_viagens).
  sufixo <- if (nzchar(uni)) uni else "esforco"
  # Marcados e com nome: só essas linhas de medida viram índice.
  marcar <- indices_marcados & nzchar(trimws(medidas_raw$nome))
  # As colunas das medidas marcadas, na ordem em que aparecem na tabela.
  colunas_marcadas <- medidas$coluna[match(which(marcar), which(nzchar(trimws(medidas_raw$nome))))]
  # Uma linha por índice: coluna nova, colunas de origem e a marca de CPUE.
  # Sem esforço ou sem índice marcado, a tabela nasce vazia, já com as quatro
  # colunas (montar um data.frame com colunas de tamanho zero e uma de tamanho
  # um quebra com "arguments imply differing number of rows").
  if (tem_esforco && length(colunas_marcadas) > 0) {
    indices <- data.frame(
      coluna = paste0(colunas_marcadas, "_por_", sufixo),
      coluna_medida = colunas_marcadas,
      coluna_esforco = esforco_col,
      cpue = grepl("^captura", colunas_marcadas),
      stringsAsFactors = FALSE
    )
  } else {
    indices <- data.frame(
      coluna = character(0), coluna_medida = character(0),
      coluna_esforco = character(0), cpue = logical(0),
      stringsAsFactors = FALSE
    )
  }
  # Rótulo por extenso da frequência, usado nos metadados e na tela.
  rotulo_freq <- c(diaria = "Diária", semanal = "Semanal", quinzenal = "Quinzenal", mensal = "Mensal")[[frequencia]]
  # A configuração sai numa lista única, pronta para a tabela, o Excel e os metadados.
  list(
    datas = datas, locais = locais, hora = hora_real, horario = horario,
    frequencia = frequencia, frequencia_rotulo = rotulo_freq,
    medidas = medidas, tem_esforco = tem_esforco, unidade_esforco = unidade_esforco,
    esforco_col = esforco_col, indices = indices
  )
}

# Monta a tabela tidy da série: uma linha por data e local, na ordem data → local.
montar_tabela <- function(datas, locais, hora, colunas_medidas, coluna_esforco, colunas_indices) {
  # A coluna de datas repete cada data uma vez por local (a data anda devagar).
  # A coluna de locais percorre todos os locais dentro de cada data.
  tab <- data.frame(
    data = format(rep(datas, each = length(locais)), "%Y-%m-%d"),
    local = rep(locais, times = length(datas)),
    stringsAsFactors = FALSE
  )
  # A coluna de hora só existe quando o pesquisador pede registrar a hora real.
  if (hora) tab$hora <- ""
  # Cada medida vira uma coluna numérica vazia, pronta para o campo.
  for (cm in colunas_medidas) tab[[cm]] <- NA_real_
  # A coluna de esforço só existe quando o registro de esforço está ligado.
  if (!is.null(coluna_esforco)) tab[[coluna_esforco]] <- NA_real_
  # As colunas de índice nascem vazias; no Excel recebem a fórmula pronta.
  for (ci in colunas_indices) tab[[ci]] <- NA_real_
  # A coluna de observação sempre existe: é o caderno de ocorrências da série.
  tab$observacao <- ""
  tab
}

# Monta a aba metadados do Excel: duas colunas, campo e valor, com tudo da aba 1.
montar_metadados <- function(cfg) {
  # Cada linha dos metadados é um par campo e valor.
  linha <- function(campo, valor) data.frame(campo = campo, valor = valor, stringsAsFactors = FALSE)
  # O período vai com as datas reais do calendário gerado.
  met <- rbind(
    linha("delineamento", "Monitoramento (série temporal)"),
    linha("período", paste(format(min(cfg$datas)), "a", format(max(cfg$datas)))),
    linha("frequência", cfg$frequencia_rotulo),
    linha("horário da coleta", if (nzchar(trimws(cfg$horario))) cfg$horario else "não definido"),
    linha("registra hora real de cada coleta", if (cfg$hora) "sim" else "não"),
    linha("locais", paste(cfg$locais, collapse = ", ")),
    linha("coordenadas", ""),
    linha("registra esforço", if (cfg$tem_esforco) "sim" else "não")
  )
  # A unidade do esforço só aparece quando o esforço está ligado.
  if (cfg$tem_esforco) met <- rbind(met, linha("unidade do esforço", cfg$unidade_esforco))
  # Uma linha por medida, com sua unidade.
  for (i in seq_len(nrow(cfg$medidas))) {
    met <- rbind(met, linha(paste0("medida: ", cfg$medidas$nome[i]),
                            if (nzchar(trimws(cfg$medidas$unidade[i]))) cfg$medidas$unidade[i] else "sem unidade"))
  }
  # Uma linha por índice, com a fórmula descrita em palavras.
  for (i in seq_len(nrow(cfg$indices))) {
    descricao <- paste0(cfg$indices$coluna_medida[i], " dividido por ", cfg$indices$coluna_esforco[i],
                        if (cfg$indices$cpue[i]) " (CPUE)" else "")
    met <- rbind(met, linha(paste0("índice: ", cfg$indices$coluna[i]), descricao))
  }
  # Protocolo e responsável ficam vazios, para o pesquisador completar.
  met <- rbind(met, linha("protocolo", ""), linha("responsável", ""))
  met
}

# Escreve o Excel com as abas dados e metadados; os índices levam fórmula pronta.
escrever_excel <- function(caminho, tab, metadados, indices) {
  # O arquivo nasce de uma pasta de trabalho do openxlsx.
  wb <- openxlsx::createWorkbook()
  # A aba dados recebe a tabela tidy como está.
  openxlsx::addWorksheet(wb, "dados")
  openxlsx::writeData(wb, "dados", tab)
  # A cor suave marca as colunas que não se digita (têm fórmula).
  estilo_indice <- openxlsx::createStyle(fgFill = "#E4F0EF")
  # Cada índice vira uma coluna de fórmulas medida ÷ esforço, linha a linha.
  for (i in seq_len(nrow(indices))) {
    # As posições das três colunas na tabela viram letras de planilha.
    col_indice <- which(names(tab) == indices$coluna[i])
    col_medida <- which(names(tab) == indices$coluna_medida[i])
    col_esforco <- which(names(tab) == indices$coluna_esforco[i])
    lm <- openxlsx::int2col(col_medida)
    le <- openxlsx::int2col(col_esforco)
    # As linhas da fórmula começam na 2, porque a linha 1 é o cabeçalho.
    linhas <- seq_len(nrow(tab)) + 1
    # A fórmula retorna vazio quando falta a medida, falta o esforço ou o esforço é zero.
    formulas <- sprintf('=IF(OR(%s%d="",%s%d="",%s%d=0),"",%s%d/%s%d)',
                        lm, linhas, le, linhas, le, linhas, lm, linhas, le, linhas)
    # Escreve a coluna inteira de uma vez, a partir da segunda linha.
    openxlsx::writeFormula(wb, "dados", x = formulas, startCol = col_indice, startRow = 2)
    # Pinta a coluna do índice, cabeçalho incluso, com a cor de não digitar.
    openxlsx::addStyle(wb, "dados", style = estilo_indice,
                       rows = seq_len(nrow(tab) + 1), cols = col_indice, gridExpand = TRUE)
  }
  # A aba metadados recebe o quadro campo e valor.
  openxlsx::addWorksheet(wb, "metadados")
  openxlsx::writeData(wb, "metadados", metadados)
  # Grava o arquivo no caminho pedido pelo download.
  openxlsx::saveWorkbook(wb, caminho, overwrite = TRUE)
}

# Rótulos em português da paginação do DT, no mesmo texto do preparo; ficam
# aqui para o módulo continuar testável sem puxar o exportador inteiro.
idioma_tabela_monitoramento <- function() {
  list(
    search = "Buscar:", lengthMenu = "Mostrar _MENU_ linhas",
    zeroRecords = "Nenhuma ocorrência encontrada", emptyTable = "Nenhum dado disponível",
    infoEmpty = "0 linhas", info = "Mostrando _START_ a _END_ de _TOTAL_ linhas",
    infoFiltered = "(consulta sobre _MAX_ linhas)",
    paginate = list(first = "Primeira", previous = "Anterior",
                    "next" = "Próxima", last = "Última")
  )
}

# ---- Interface ---------------------------------------------------------------

mod_monitoramento_ui <- function(id) {
  ns <- shiny::NS(id)

  # Estilo local: faixas compactas, para a aba de definições caber na tela do notebook.
  estilo <- shiny::tags$style(shiny::HTML("
    .mon-estudio .form-group { margin-bottom: 4px; }
    .mon-estudio .control-label { font-size: 0.85rem; margin-bottom: 2px; }
    .mon-estudio .card, .mon-estudio .card-body, .mon-estudio .tab-content, .mon-estudio .tab-pane {
      overflow: visible !important; height: auto !important; min-height: 0; }
    .mon-estudio .card-body { display: block !important; padding: 10px 14px; }
    .mon-card { background: #ffffff; border: 1px solid #dbe5e8; border-radius: 10px;
      padding: 10px 14px; box-shadow: 0 1px 3px rgba(15, 59, 95, 0.03); margin-bottom: 10px; }
    .mon-card h5 { color: #0F3B5F; font-size: 0.92rem; font-weight: 700; margin-bottom: 6px; }
    .mon-linha { display: flex; gap: 8px; align-items: flex-end; }
    .mon-linha .shiny-input-container { flex: 1; }
    .mon-rem { margin-bottom: 6px; padding: 2px 9px; line-height: 1.4; }
  "))

  shiny::div(class = "mon-estudio",
    estilo,
    shiny::div(class = "alert alert-light border mb-2 py-2 px-3",
      style = "border-left: 4px solid #2E7D8F !important;",
      shiny::tags$b("Monitoramento (série temporal)"), shiny::tags$br(),
      "Poucos locais medidos muitas vezes ao longo do tempo: desembarque pesqueiro, chuva, temperatura e qualidade da água em estação fixa, altura de maré. Defina a série, baixe o protótipo da tabela e ajuste o que precisar no Excel."
    ),
    bslib::navset_card_tab(
      id = ns("abas"),

      # Aba 1: o que o pesquisador define aqui alimenta a tabela e os metadados.
      bslib::nav_panel("Definições da série", icon = shiny::icon("sliders"),
        bslib::card_body(fillable = FALSE,

          # Duas colunas de cartões para aproveitar a largura da tela: à
          # esquerda o tempo e o esforço; à direita o resumo e as respostas.
          bslib::layout_columns(col_widths = c(6, 6),
            shiny::div(
              shiny::div(class = "mon-card",
                shiny::h5("1. Tempo"),
                bslib::layout_columns(col_widths = c(6, 6),
                  shiny::uiOutput(ns("ui_inicio")),
                  shiny::uiOutput(ns("ui_fim"))
                ),
                shiny::radioButtons(ns("frequencia"), "Frequência:",
                  choices = c("Diária" = "diaria", "Semanal" = "semanal", "Quinzenal" = "quinzenal", "Mensal" = "mensal"),
                  selected = "mensal", inline = TRUE),
                bslib::layout_columns(col_widths = c(6, 6),
                  shiny::textInput(ns("horario"), "Horário da coleta (opcional):", placeholder = "Ex.: 8h, na preamar"),
                  shiny::radioButtons(ns("hora_real"), "Registrar a hora real de cada coleta?",
                    choices = c("Não" = "nao", "Sim" = "sim"), inline = TRUE)
                )
              ),

              shiny::div(class = "mon-card",
                shiny::h5("2. Onde e com que esforço"),
                bslib::layout_columns(col_widths = c(7, 5),
                  shiny::div(
                    shiny::uiOutput(ns("ui_locais")),
                    shiny::actionButton(ns("add_local"), "+ local", class = "btn-sm btn-outline-primary")
                  ),
                  shiny::div(
                    shiny::radioButtons(ns("tem_esforco"), "Registra esforço?",
                      choices = c("Não" = "nao", "Sim" = "sim"), inline = TRUE),
                    shiny::conditionalPanel(sprintf("input['%s'] === 'sim'", ns("tem_esforco")),
                      shiny::textInput(ns("unidade_esforco"), "Unidade do esforço:", value = "viagens")
                    )
                  )
                )
              )
            ),

            shiny::div(
              # O resumo da série abre a coluna da direita: contagem de linhas
              # e avisos gentis, sempre visíveis enquanto se define o plano.
              shiny::div(class = "mon-card",
                shiny::uiOutput(ns("resumo"))
              ),
              shiny::div(class = "mon-card",
                shiny::h5("3. Respostas"),
                shiny::p(class = "small text-muted mb-1",
                  "Medidas são o que se registra em campo; o nome da coluna junta nome e unidade, em minúsculas e sem acentos (captura + kg vira captura_kg)."),
                shiny::uiOutput(ns("ui_medidas")),
                shiny::actionButton(ns("add_medida"), "+ medida", class = "btn-sm btn-outline-primary")
              )
            )
          )
        )
      ),

      # Aba 2: a tabela completa, paginada, e o botão de download do Excel.
      bslib::nav_panel("Tabela", icon = shiny::icon("table"),
        bslib::card_body(fillable = FALSE,
          shiny::p(class = "small text-muted", "Tabela completa que o Excel vai trazer, com paginação. Uma linha por data e local, na ordem data, depois local."),
          DT::DTOutput(ns("tabela")),
          shiny::downloadButton(ns("baixar_excel"), "Baixar Excel", class = "btn-primary"),
          shiny::p(class = "small text-muted mt-2 mb-0",
            "Este é um protótipo: ajuste o que precisar no Excel, mantendo as colunas data e local. Colunas coloridas trazem fórmula pronta e não devem ser digitadas.")
        )
      ),

      # Aba 3: orientação curta, em duas colunas de texto para aproveitar a tela.
      bslib::nav_panel("Metodologia", icon = shiny::icon("book-open"),
        bslib::card_body(fillable = FALSE,
          bslib::layout_columns(col_widths = c(6, 6),
            shiny::div(class = "mon-card",
              shiny::h5("Quando usar"),
              shiny::p(class = "mb-1", "Quando poucos locais são medidos muitas vezes ao longo do tempo: desembarque pesqueiro, chuva, temperatura e qualidade da água em estação fixa, altura de maré."),
              shiny::p(class = "mb-0", "Se forem muitas unidades medidas poucas vezes, o delineamento é o Longitudinal comparativo.")
            ),
            shiny::div(class = "mon-card",
              shiny::h5("Antes de começar"),
              shiny::tags$ul(class = "mb-0",
                shiny::tags$li("Escreva a pergunta numa frase. Exemplo: a captura de pescada-amarela em Bragança está caindo?"),
                shiny::tags$li("Fixe o local e registre as coordenadas."),
                shiny::tags$li("Escolha a frequência pela velocidade do fenômeno. Na dúvida, colete com mais frequência: agrupar depois é fácil, desagrupar é impossível."),
                shiny::tags$li("Garanta a duração: pelo menos dois ciclos completos para enxergar a sazonalidade (dois anos com dados mensais). Para tendência ou previsão, o ideal são 50 observações ou mais."),
                shiny::tags$li("Escreva o protocolo: horário, equipamento, unidade e responsável. No desembarque, defina como medir o esforço, porque sem esforço não há CPUE.")
              )
            ),
            shiny::div(class = "mon-card",
              shiny::h5("Durante a coleta"),
              shiny::tags$ul(class = "mb-0",
                shiny::tags$li("Mesmo dia, mesmo horário, mesmo método."),
                shiny::tags$li("Dado não medido é célula vazia, nunca zero."),
                shiny::tags$li("Anote as ocorrências na coluna de observação: greve, defeso, troca de equipamento, enchente."),
                shiny::tags$li("Se precisar trocar um equipamento, meça com o antigo e o novo ao mesmo tempo por algumas semanas.")
              )
            ),
            shiny::div(class = "mon-card",
              shiny::h5("Revisão mensal"),
              shiny::tags$ul(class = "mb-0",
                shiny::tags$li("Conte as lacunas. Se passarem de cerca de 10%, investigue a causa."),
                shiny::tags$li("Faça um gráfico da série para achar valores estranhos."),
                shiny::tags$li("Confira se o protocolo continua o mesmo.")
              )
            )
          ),
          shiny::p(class = "small text-muted mb-0",
            "Quando a série estiver andando, a análise é a de Séries Temporais, no menu Modelos de Regressão.")
        )
      )
    )
  )
}

# ---- Servidor ----------------------------------------------------------------

# O módulo é autossuficiente: as definições da série moram no Excel que ele
# gera, não na ficha de planejamento.
mod_monitoramento_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    # ---- Faixa 1: seletores de data -----------------------------------------
    # Quando a frequência é mensal, o seletor mostra mês e ano; nas demais, a data completa.
    # O valor digitado se preserva quando a frequência muda de tipo de seletor.
    padrao_inicio <- as.Date("2026-01-01")
    padrao_fim <- as.Date("2027-12-01")
    seletor_data <- function(campo, rotulo, padrao) {
      valor <- shiny::isolate(input[[campo]]) %||% padrao
      if (identical(input$frequencia, "mensal")) {
        shinyWidgets::airDatepickerInput(session$ns(campo), rotulo,
          value = valor, view = "months", minView = "months",
          dateFormat = "yyyy-MM", autoClose = TRUE, language = "pt-BR")
      } else {
        shiny::dateInput(session$ns(campo), rotulo, value = valor,
          format = "yyyy-mm-dd", language = "pt-BR")
      }
    }
    output$ui_inicio <- shiny::renderUI(seletor_data("inicio", "Início:", padrao_inicio))
    output$ui_fim <- shiny::renderUI(seletor_data("fim", "Fim:", padrao_fim))

    # ---- Faixa 2: locais dinâmicos ------------------------------------------
    # Os valores dos campos moram num vetor reativo; o número de linhas é o tamanho dele.
    locais_vals <- shiny::reactiveVal(c("", ""))
    # Lê o que está digitado nos campos de local neste momento.
    ler_locais <- function() {
      vapply(seq_along(shiny::isolate(locais_vals())), function(j) {
        shiny::isolate(input[[paste0("local_", j)]]) %||% ""
      }, character(1))
    }
    shiny::observeEvent(input$add_local, {
      locais_vals(c(ler_locais(), ""))
    })
    # Um observador por linha possível (limite de 12 locais).
    lapply(seq_len(12), function(i) {
      shiny::observeEvent(input[[paste0("rem_local_", i)]], {
        vals <- ler_locais()
        if (length(vals) <= 1L) return()
        locais_vals(vals[-i])
      }, ignoreInit = TRUE)
    })
    output$ui_locais <- shiny::renderUI({
      vals <- locais_vals()
      shiny::tagList(lapply(seq_along(vals), function(i) {
        shiny::div(class = "mon-linha",
          shiny::textInput(session$ns(paste0("local_", i)),
            label = if (i == 1) "Locais:" else NULL,
            value = vals[i], placeholder = sprintf("Local %d", i)),
          if (length(vals) > 1L) {
            shiny::actionButton(session$ns(paste0("rem_local_", i)), "\u00d7",
              class = "btn-sm btn-outline-danger mon-rem")
          }
        )
      }))
    })

    # ---- Faixa 3: medidas dinâmicas -----------------------------------------
    # As medidas moram num quadro reativo de nome e unidade; começa com captura em kg.
    medidas_vals <- shiny::reactiveVal(data.frame(nome = "captura", unidade = "kg", stringsAsFactors = FALSE))
    # Lê o que está digitado nos campos de medida neste momento.
    ler_medidas <- function() {
      n <- nrow(shiny::isolate(medidas_vals()))
      data.frame(
        nome = vapply(seq_len(n), function(j) shiny::isolate(input[[paste0("medida_nome_", j)]]) %||% "", character(1)),
        unidade = vapply(seq_len(n), function(j) shiny::isolate(input[[paste0("medida_unidade_", j)]]) %||% "", character(1)),
        stringsAsFactors = FALSE
      )
    }
    shiny::observeEvent(input$add_medida, {
      medidas_vals(rbind(ler_medidas(), data.frame(nome = "", unidade = "", stringsAsFactors = FALSE)))
    })
    # Um observador por linha possível (limite de 10 medidas).
    lapply(seq_len(10), function(i) {
      shiny::observeEvent(input[[paste0("rem_medida_", i)]], {
        vals <- ler_medidas()
        if (nrow(vals) <= 1L) return()
        medidas_vals(vals[-i, , drop = FALSE])
      }, ignoreInit = TRUE)
    })
    # Ligar ou desligar o esforço redesenha as linhas, então os valores se sincronizam antes.
    shiny::observeEvent(input$tem_esforco, {
      medidas_vals(ler_medidas())
      locais_vals(ler_locais())
    }, ignoreInit = TRUE)
    output$ui_medidas <- shiny::renderUI({
      medidas_vals()
      input$tem_esforco
      vals <- shiny::isolate(medidas_vals())
      shiny::tagList(lapply(seq_len(nrow(vals)), function(i) {
        # O nome limpo da medida neste momento rotula a caixa de índice da linha.
        coluna <- limpar_nome(paste(trimws(vals$nome[i]), trimws(vals$unidade[i])))
        shiny::div(
          shiny::div(class = "mon-linha",
            shiny::textInput(session$ns(paste0("medida_nome_", i)),
              label = if (i == 1) "Medida:" else NULL,
              value = vals$nome[i], placeholder = "Ex.: captura"),
            shiny::textInput(session$ns(paste0("medida_unidade_", i)),
              label = if (i == 1) "Unidade:" else NULL,
              value = vals$unidade[i], placeholder = "Ex.: kg"),
            if (nrow(vals) > 1L) {
              shiny::actionButton(session$ns(paste0("rem_medida_", i)), "\u00d7",
                class = "btn-sm btn-outline-danger mon-rem")
            }
          ),
          if (identical(input$tem_esforco, "sim") && nzchar(coluna)) {
            shiny::div(class = "small mb-1", style = "margin-left: 2px;",
              shiny::checkboxInput(session$ns(paste0("indice_", i)),
                label = shiny::tags$span(
                  sprintf("calcular %s por unidade de esforço", coluna),
                  if (grepl("^captura", coluna)) shiny::tags$span(class = "text-muted", " (CPUE)")
                ),
                value = shiny::isolate(input[[paste0("indice_", i)]]) %||% FALSE)
            )
          }
        )
      }))
    })

    # ---- Configuração reativa -------------------------------------------------
    # Tudo o que a aba 1 define, reunido e com os nomes de coluna prontos.
    cfg <- shiny::reactive({
      shiny::req(input$inicio, input$fim, input$frequencia)
      n_med <- nrow(medidas_vals())
      montar_config(
        inicio = input$inicio, fim = input$fim, frequencia = input$frequencia,
        horario = input$horario %||% "", hora_real = identical(input$hora_real, "sim"),
        locais_raw = vapply(seq_along(locais_vals()), function(j) input[[paste0("local_", j)]] %||% "", character(1)),
        tem_esforco = identical(input$tem_esforco, "sim"),
        unidade_esforco = input$unidade_esforco %||% "viagens",
        medidas_raw = data.frame(
          nome = vapply(seq_len(n_med), function(j) input[[paste0("medida_nome_", j)]] %||% "", character(1)),
          unidade = vapply(seq_len(n_med), function(j) input[[paste0("medida_unidade_", j)]] %||% "", character(1)),
          stringsAsFactors = FALSE
        ),
        indices_marcados = vapply(seq_len(n_med), function(j) isTRUE(input[[paste0("indice_", j)]]), logical(1))
      )
    })

    # ---- Resumo ao vivo --------------------------------------------------------
    output$resumo <- shiny::renderUI({
      c0 <- cfg()
      n_datas <- length(c0$datas)
      duracao_dias <- as.numeric(max(c0$datas) - min(c0$datas))
      shiny::tagList(
        shiny::p(class = "mb-1", shiny::tags$b(
          sprintf("%d datas \u00d7 %d locais = %d linhas", n_datas, length(c0$locais), n_datas * length(c0$locais)))),
        if (duracao_dias < 730) {
          shiny::p(class = "small mb-1", style = "color:#B26A00;",
            "A série cobre menos de dois anos; ainda não será possível separar sazonalidade de variação casual.")
        },
        if (n_datas < 50) {
          shiny::p(class = "small mb-0", style = "color:#B26A00;",
            "Séries com menos de 50 observações limitam a análise de tendência e previsão.")
        }
      )
    })

    # ---- Aba 2: tabela completa e download --------------------------------------
    # A tabela aparece inteira, paginada pelo DT, com as células vazias como no
    # Excel. Como é uma tabela de planejamento (poucas milhares de linhas no
    # máximo), o DT roda no cliente, sem idas ao servidor a cada página.
    output$tabela <- DT::renderDT({
      c0 <- cfg()
      tab <- montar_tabela(c0$datas, c0$locais, c0$hora,
        c0$medidas$coluna, c0$esforco_col, c0$indices$coluna)
      DT::datatable(tab, rownames = FALSE, options = list(
        pageLength = 12, lengthMenu = c(12, 24, 48, 96), scrollX = TRUE,
        language = idioma_tabela_monitoramento()
      ))
    }, server = FALSE)

    # O download entrega as abas dados e metadados, sem nenhuma validação que impeça.
    output$baixar_excel <- shiny::downloadHandler(
      filename = function() paste0("monitoramento_serie_", format(Sys.Date(), "%Y-%m-%d"), ".xlsx"),
      content = function(file) {
        c0 <- cfg()
        tab <- montar_tabela(c0$datas, c0$locais, c0$hora,
          c0$medidas$coluna, c0$esforco_col, c0$indices$coluna)
        escrever_excel(file, tab, montar_metadados(c0), c0$indices)
      }
    )
  })
}
