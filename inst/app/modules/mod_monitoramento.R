# Delineamento observacional Monitoramento (série temporal): poucos locais
# medidos muitas vezes ao longo do tempo (desembarque, chuva, temperatura em
# estação fixa, maré). A tela entrega um protótipo de tabela tidy em Excel
# (abas dados e metadados, com fórmulas prontas nos índices) para o pesquisador
# preencher em campo e ajustar depois; a sub-aba Tabela mostra a tabela
# completa, paginada pelo DT. As seis abas seguem o transversal comparativo,
# com conteúdo próprio para séries temporais; o
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
  met <- rbind(met, linha("protocolo", ""), linha("responsável", ""),
    linha("zero e ausência", "Zero observado é válido; medida não obtida fica vazia. Preserve local e data e registre o motivo em observacao."),
    linha("alcance", "Mudanças estão associadas aos locais e ao período acompanhados; a série, por si só, não identifica suas causas."))
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

# Infográfico do plano, sem inventar respostas ou trajetórias observadas.
# Mostramos apenas cinco datas no esquema; a quadrícula contém todo o calendário.
desenhar_plano_monitoramento <- function(cfg) {
  n <- length(cfg$datas)
  pos <- unique(as.integer(round(seq(1, n, length.out = min(n, 5L)))))
  xs <- seq(4.8, 14.6, length.out = length(pos))
  ys <- 4.8 + rev(seq_along(cfg$locais) - 1) * 1.25
  topo <- max(ys) + 1.15
  quebra <- function(x, largura = 27) paste(strwrap(x, width = largura), collapse = "\n")
  p <- ggplot2::ggplot()
  texto <- function(x, y, label, size = 3.3, bold = FALSE, colour = "#0F3B5F") {
    p <<- p + ggplot2::annotate("text", x = x, y = y, label = label,
      size = size, fontface = if (bold) "bold" else "plain", colour = colour, lineheight = 1.1)
  }
  caixa <- function(xmin, xmax, ymin, ymax, fill) {
    p <<- p + ggplot2::annotate("rect", xmin = xmin, xmax = xmax,
      ymin = ymin, ymax = ymax, fill = fill, colour = NA)
  }
  texto(1.6, topo, "LOCAIS FIXOS", bold = TRUE)
  for (j in seq_along(pos)) {
    caixa(xs[j] - 0.9, xs[j] + 0.9, topo - 0.4, topo + 0.4, "#CBDCE9")
    texto(xs[j], topo + 0.12, format(cfg$datas[pos[j]], "%d/%m/%Y"), size = 3.0, bold = TRUE)
    texto(xs[j], topo - 0.2, paste("data", pos[j]), size = 2.8)
  }
  for (i in seq_along(cfg$locais)) {
    y <- ys[i]
    caixa(0, 16, y - 0.5, y + 0.5, if (i %% 2) "#F1F7F8" else "#FAFCFC")
    texto(1.6, y + 0.12, quebra(cfg$locais[i], 20), size = 3.4, bold = TRUE)
    texto(1.6, y - 0.32, paste("local", i), size = 2.8)
    if (length(xs) > 1) p <- p + ggplot2::annotate("segment", x = min(xs), xend = max(xs),
      y = y, yend = y, colour = "#62B6B7", linewidth = 0.8)
    for (j in seq_along(pos)) {
      p <- p + ggplot2::annotate("point", x = xs[j], y = y, size = 5,
        shape = 21, fill = "#2E7D8F", colour = "#0F3B5F")
      texto(xs[j], y - 0.32, "mesmo local", size = 2.7)
    }
  }
  texto(8, 3.8, "EM CADA LOCAL, EM CADA DATA", bold = TRUE)
  fundos <- c("#CBDCE9", "#E6F3F1", "#FBEAD1")
  for (i in 1:3) caixa((i - 1) * 5.4, (i - 1) * 5.4 + 5.1, 1.6, 3.4, fundos[i])
  texto(2.55, 3.03, "1. Medir", bold = TRUE, size = 3.8)
  medidas <- paste(cfg$medidas$nome, ifelse(nzchar(cfg$medidas$unidade),
    paste0("(", cfg$medidas$unidade, ")"), ""))
  rotulo <- if (!length(medidas)) "Defina as respostas na aba Definições" else
    paste(utils::head(medidas, 3), collapse = "; ")
  if (length(medidas) > 3) rotulo <- paste0(rotulo, "; + ", length(medidas) - 3, " na ficha")
  texto(2.55, 2.25, quebra(rotulo), size = 3.0)
  texto(7.95, 3.03, "2. Registrar o esforço", bold = TRUE, size = 3.8)
  texto(7.95, 2.25, quebra(if (cfg$tem_esforco) paste("Esforço em", cfg$unidade_esforco,
    if (nrow(cfg$indices)) "· índice = medida / esforço" else "· mesma definição em toda a série") else
    "Esforço não informado no plano\nManter o protocolo de medição"), size = 3.0)
  texto(13.35, 3.03, "3. Preencher a ficha", bold = TRUE, size = 3.8)
  texto(13.35, 2.25, "1 linha por data × local\nZero observado ≠ medida ausente\nAusência: guardar data, local e motivo", size = 3.0)
  texto(8, 1.03, sprintf("%d locais fixos × %d datas = %d registros previstos",
    length(cfg$locais), n, length(cfg$locais) * n), bold = TRUE, size = 3.9)
  texto(8, 0.46, sprintf("Frequência %s · %s a %s", tolower(cfg$frequencia_rotulo),
    format(min(cfg$datas), "%d/%m/%Y"), format(max(cfg$datas), "%d/%m/%Y")))
  p + ggplot2::coord_cartesian(xlim = c(-0.1, 16.2), ylim = c(0, topo + 0.55), expand = FALSE) +
    ggplot2::labs(title = "Monitoramento: acompanhar os mesmos locais ao longo do tempo",
      subtitle = if (n > length(pos)) sprintf("Esquema com %d de %d datas. O calendário completo está nas sub-abas seguintes.", length(pos), n) else
        "Cada ponto é uma visita prevista; as linhas ligam revisitas ao mesmo local.",
      caption = "Revisitas não criam novos locais amostrais. A dependência temporal deverá ser considerada na análise.\nO esquema não comprova independência espacial e a série, sozinha, não identifica causas das mudanças.") +
    tema_desenho_monitoramento()
}

tema_desenho_monitoramento <- function() {
  ggplot2::theme_void(base_size = 12) + ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", colour = "#0F3B5F", size = 15),
    plot.subtitle = ggplot2::element_text(size = 10, margin = ggplot2::margin(b = 12)),
    plot.caption = ggplot2::element_text(hjust = 0, size = 10, margin = ggplot2::margin(t = 12)),
    plot.background = ggplot2::element_rect(fill = "white", colour = NA),
    plot.margin = ggplot2::margin(12, 16, 12, 16))
}

# Uma célula por mês: a contagem usa todas as datas, inclusive meses parciais.
# Na vista anual mostramos os dias; por local mostramos o esforço em visitas.
desenhar_calendario_monitoramento <- function(cfg, ano, por_local = FALSE) {
  datas <- cfg$datas[format(cfg$datas, "%Y") == as.character(ano)]
  meses <- c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez")
  linhas <- if (por_local) cfg$locais else as.character(ano)
  grade <- expand.grid(mes = 1:12, linha = seq_along(linhas))
  contagens <- tabulate(as.integer(format(datas, "%m")), nbins = 12)
  grade$n <- contagens[grade$mes]
  grade$rotulo <- ifelse(grade$n == 0, "—", as.character(grade$n))
  p <- ggplot2::ggplot(grade, ggplot2::aes(x = mes, y = linha)) +
    ggplot2::geom_tile(ggplot2::aes(fill = n > 0), width = 0.94, height = 0.86) +
    ggplot2::scale_fill_manual(values = c("FALSE" = "#EEF2F3", "TRUE" = "#E6F3F1"), guide = "none")
  if (por_local) {
    p <- p + ggplot2::geom_text(ggplot2::aes(label = rotulo), colour = "#0F3B5F", size = 4)
  } else {
    # Os dias ocupam cinco fileiras dentro do mês, sem simular datas adicionais.
    pontos <- data.frame(mes = as.integer(format(datas, "%m")), dia = as.integer(format(datas, "%d")))
    pontos$x <- pontos$mes - 0.35 + ((pontos$dia - 1) %% 7) * 0.115
    pontos$y <- 0.72 + ((pontos$dia - 1) %/% 7) * 0.12
    p <- p + ggplot2::geom_point(data = pontos, ggplot2::aes(x = x, y = y),
      inherit.aes = FALSE, colour = "#2E7D8F", size = 2) +
      ggplot2::geom_text(ggplot2::aes(y = linha + 0.33,
        label = ifelse(n == 0, "—", paste0(n, " visitas"))), colour = "#0F3B5F", size = 3)
  }
  p + ggplot2::scale_x_continuous(breaks = 1:12, labels = meses, position = "top") +
    ggplot2::scale_y_reverse(breaks = seq_along(linhas),
      labels = vapply(linhas, function(x) paste(strwrap(x, 22), collapse = "\n"), character(1))) +
    ggplot2::coord_cartesian(xlim = c(0.5, 12.5), ylim = c(0.5, length(linhas) + 0.5), expand = FALSE) +
    ggplot2::labs(title = if (por_local) paste("Visitas previstas por local ·", ano) else paste("Quadrícula de coletas ·", ano),
      subtitle = if (por_local) "Número na célula = visitas no mês. Todos os locais seguem o mesmo calendário." else
        "Cada ponto é um dia previsto de coleta em todos os locais; a contagem é por local.",
      caption = sprintf("%d datas por local × %d locais = %d registros neste ano. Traço = nenhuma visita prevista.\n%s",
        length(datas), length(cfg$locais), length(datas) * length(cfg$locais),
        if (por_local) "Repetir a coleta no tempo não aumenta o número de locais amostrais." else
          "Posições dentro da célula: dias 1–7, 8–14, 15–21, 22–28 e 29–31, de cima para baixo.")) +
    tema_desenho_monitoramento() + ggplot2::theme(
      axis.text.x = ggplot2::element_text(colour = "#0F3B5F", size = 11),
      axis.text.y = ggplot2::element_text(colour = "#0F3B5F", size = 11))
}

# A minuta completa vai para o Word; a tela resume as mesmas definições.
conteudo_metodologia_monitoramento <- function(cfg) {
  respostas <- if (nrow(cfg$medidas)) paste(paste0(cfg$medidas$nome,
    ifelse(nzchar(cfg$medidas$unidade), paste0(" (", cfg$medidas$unidade, ")"), "")), collapse = ", ") else
      "[definir as respostas e suas unidades]"
  list(
    paragrafos = c(
      sprintf("Será realizado um monitoramento observacional entre %s e %s, com frequência %s, em %d locais fixos (%s). O estudo acompanhará %s. A pergunta, a região, os critérios de escolha dos locais e o alcance da inferência serão explicitados no protocolo antes da primeira coleta.",
        format(min(cfg$datas), "%d/%m/%Y"), format(max(cfg$datas), "%d/%m/%Y"),
        tolower(cfg$frequencia_rotulo), length(cfg$locais), paste(cfg$locais, collapse = ", "), respostas),
      sprintf("Os mesmos locais serão revisitados nas %d datas previstas, totalizando %d registros de local e data. %s O protocolo definirá equipamento, procedimento, responsável e condições de coleta comparáveis. As datas do calendário são previstas; alterações e a data efetiva da visita deverão ser registradas na ficha ou no diário de campo.",
        length(cfg$datas), length(cfg$datas) * length(cfg$locais),
        if (nzchar(trimws(cfg$horario))) paste0("O horário previsto será ", cfg$horario, ".") else
          "O horário e sua relação com o fenômeno estudado serão definidos antes do campo."),
      if (cfg$tem_esforco) sprintf("O esforço será registrado em %s em cada visita. %s A definição do esforço e das condições de medição será mantida ao longo da série; mudanças de equipamento, apetrecho ou operação serão documentadas para avaliar a comparabilidade.",
        cfg$unidade_esforco, if (nrow(cfg$indices)) paste0("Os índices previstos (",
          paste(cfg$indices$coluna, collapse = ", "), ") serão calculados pela divisão da medida pelo esforço, somente quando houver valores válidos e esforço maior que zero.") else
          "Não foram selecionados índices de medida por esforço.") else
        "O plano atual não prevê uma coluna de esforço. Caso a resposta dependa de esforço de pesca ou de observação, sua unidade e forma de registro serão definidas antes da coleta. A captura total e a captura por unidade de esforço respondem a perguntas diferentes.",
      "Uma resposta igual a zero será mantida quando resultar de uma coleta efetivamente realizada. Medidas não obtidas permanecerão ausentes, preservando-se o local e a data previstos e anotando-se o motivo em observacao. Fechamento, ausência de desembarque, mau tempo e falha de coleta serão distinguidos no diário de campo.",
      "As revisitas de cada local formarão uma série temporal e não serão tratadas como novos locais amostrais. A análise considerará dependência temporal, sazonalidade e possíveis condições compartilhadas entre locais. As mudanças serão interpretadas no âmbito dos locais e do período acompanhados; o monitoramento, por si só, não identifica suas causas. A escolha da análise dependerá da pergunta, da resposta e da estrutura dos dados."
    ),
    protocolo = c(
      "Pergunta e abrangência: explicitar o fenômeno, a área e quais locais ou populações o estudo pretende representar.",
      "Locais e subamostras: registrar códigos e coordenadas; identificar currais, catadores ou outras medidas dentro do local. O formulário atual gera uma linha por local e data; detalhar subamostras em uma ficha vinculada quando necessário.",
      "Tempo e maré: justificar frequência, duração e horário. Em armadilhas movidas pela maré, planejar condições comparáveis e equilibrar os tipos de maré entre meses; conferir a tábua oficial.",
      "Resposta e esforço: definir unidades, equipamento, método e denominador do índice. Para currais, registrar dimensões e reformas; para catadores, definir como registrar duração e número de participantes.",
      "Séries de apoio: definir chuva, maré ou outras variáveis relevantes e como vinculá-las ao mesmo calendário."
    ),
    revisao = c(
      "Períodos biológicos e legais: conferir literatura local e regras vigentes antes de cada campanha; não transferir automaticamente meses de outra região ou outro ano.",
      "Acesso e autorizações: registrar acordos com os participantes e autorizações aplicáveis à área e ao procedimento.",
      "Lacunas e ocorrências: revisar os motivos de visitas não realizadas, mudanças de método e valores inesperados; não substituir ausências por zero.",
      "Duração e análise: verificar se há ciclos suficientes para a pergunta e dados para avaliar previsão. A régua de 2, 3 e 4 anos do guia é uma orientação didática para séries mensais, não uma garantia de análise.",
      "Versão do plano: registrar referências, responsável, data e decisões antes da primeira coleta."
    )
  )
}

escrever_word_monitoramento <- function(caminho, cfg) {
  conteudo <- conteudo_metodologia_monitoramento(cfg)
  doc <- officer::read_docx()
  doc <- officer::body_set_default_section(doc, officer::prop_section(
    page_size = officer::page_size(width = 8.27, height = 11.69),
    page_margins = officer::page_mar(top = 0.8, bottom = 0.8, left = 0.9, right = 0.9)))
  paragrafo <- function(texto, bullet = FALSE) {
    doc <<- officer::body_add_fpar(doc, officer::fpar(
      officer::ftext(paste0(if (bullet) "• " else "", texto),
        officer::fp_text(font.family = "Calibri", font.size = 11)),
      fp_p = officer::fp_par(text.align = if (bullet) "left" else "justify", padding.bottom = 7)))
  }
  titulo <- function(texto) doc <<- officer::body_add_par(doc, texto, style = "heading 1")
  doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext("Planejamento do monitoramento",
    officer::fp_text(font.family = "Cambria", font.size = 20, bold = TRUE, color = "#0F3B5F"))))
  paragrafo("CatalyseR · Minuta para revisão antes da coleta. Este documento reúne as definições da série, a metodologia prevista e os pontos do protocolo que ainda precisam ser completados.")
  titulo("Metodologia prevista")
  for (texto in conteudo$paragrafos) paragrafo(texto)
  titulo("Resumo do plano")
  resumo <- data.frame(Item = c("Locais fixos", "Datas por local", "Registros previstos", "Medidas", "Índices"),
    Quantidade = c(length(cfg$locais), length(cfg$datas), length(cfg$locais) * length(cfg$datas),
      nrow(cfg$medidas), nrow(cfg$indices)))
  ft <- flextable::flextable(resumo)
  ft <- flextable::theme_booktabs(ft)
  ft <- flextable::font(ft, fontname = "Calibri", part = "all")
  ft <- flextable::bg(ft, bg = "#0F3B5F", part = "header")
  ft <- flextable::color(ft, color = "white", part = "header")
  doc <- flextable::body_add_flextable(doc, flextable::autofit(ft))
  # O infográfico ganha uma página horizontal, como nos outros delineamentos.
  secao <- function(horizontal = FALSE) officer::block_section(officer::prop_section(
    page_size = officer::page_size(width = 8.27, height = 11.69,
      orient = if (horizontal) "landscape" else "portrait"),
    page_margins = officer::page_mar(top = 0.7, bottom = 0.7, left = 0.9, right = 0.9),
    type = "nextPage"))
  doc <- officer::body_end_block_section(doc, secao())
  titulo("Esquema do delineamento")
  imagem <- tempfile(fileext = ".png")
  on.exit(unlink(imagem), add = TRUE)
  altura <- max(6.2, 4.7 + 0.65 * length(cfg$locais))
  ggplot2::ggsave(imagem, desenhar_plano_monitoramento(cfg), width = 12, height = altura,
    dpi = 180, bg = "white")
  fator <- min(9.5 / 12, 5.5 / altura)
  doc <- officer::body_add_img(doc, imagem, width = 12 * fator, height = altura * fator, style = "centered")
  doc <- officer::body_end_block_section(doc, secao(TRUE))
  titulo("Protocolo a completar")
  for (texto in conteudo$protocolo) paragrafo(texto, TRUE)
  titulo("Cuidados e revisão")
  for (texto in conteudo$revisao) paragrafo(texto, TRUE)
  titulo("Modelo de leitura da série")
  paragrafo("Para interpretar uma série, distinguem-se o nível de referência do local, a tendência ao longo do tempo, um possível padrão sazonal e a variação residual. O componente residual pode conservar dependência temporal. A decomposição aditiva é uma representação didática, e não um modelo já ajustado pelo painel; sua adequação e a forma de análise precisam ser avaliadas após a coleta.")
  titulo("Base de curadoria")
  paragrafo("Adaptado do Guia Painel Monitoramento do Ecossistema EAPA (setembro de 2026), especialmente as seções de exemplos, cuidados essenciais e lista de verificação. Referências de apoio do guia: Hurlbert (1984), Legg e Nagy (2006), Krumme et al. (2015), Diele et al. (2005, 2010) e Hyndman e Athanasopoulos (2021).")
  print(doc, target = caminho)
  invisible(caminho)
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
    .mon-coluna { border: 2px solid #168BFF; border-radius: 14px;
      padding: 12px; height: 100%; min-width: 0; }
    .mon-coluna .mon-card:last-child { margin-bottom: 0; }
    .mon-definicoes { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr));
      gap: 16px; align-items: stretch; }
    .mon-campos { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr));
      gap: 12px 18px; align-items: start; }
    .mon-definicoes .shiny-input-container { width: 100%; min-width: 0; }
    .mon-definicoes .form-group { margin-bottom: 0; }
    .mon-definicoes .mon-card { padding: 14px; }
    .mon-definicoes .mon-card h5 { margin-bottom: 12px; }
    .mon-definicoes .control-label { display: block; margin-bottom: 5px; }
    .mon-definicoes .mon-data .input-group { width: 100% !important; }
    .mon-frequencia { margin: 14px 0; }
    .mon-esforco { border-top: 1px solid #dbe5e8; margin-top: 14px; padding-top: 12px; }
    .mon-definicoes .mon-linha { gap: 8px; margin-bottom: 8px; }
    .mon-definicoes .mon-linha .shiny-input-container { flex: 1 1 0; }
    .mon-definicoes .mon-rem { flex: 0 0 30px; margin-bottom: 0;
      min-height: 31px; padding: 3px 8px; }
    .mon-planejar { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr));
      gap: 12px 20px; position: relative; padding: 14px; min-height: 490px;
      align-content: start; }
    .mon-planejar::before, .mon-planejar::after { content: ''; position: absolute;
      top: 0; bottom: 0; width: calc(50% - 4px); border: 2px solid #4b5155;
      border-radius: 12px; pointer-events: none; }
    .mon-planejar::before { left: 0; } .mon-planejar::after { right: 0; }
    .mon-planejar h5 { color: #0F3B5F; font-size: 0.92rem; font-weight: 700; margin: 0; }
    .mon-planejar > div { min-width: 0; }
    .mon-planejar .mon-frequencia { margin: 0; }
    .mon-planejar .mon-frequencia .shiny-options-group { display: grid;
      grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 4px; }
    .mon-planejar .radio-inline { margin-left: 0; }
    .mon-planejar .mon-horario .control-label { min-height: 34px; }
    .mon-planejar .mon-secao { border-top: 1px solid #dbe5e8; padding-top: 16px; }
    .mon-planejar .mon-esforco { border: 0; margin: 0; padding: 0; }
    .mon-planejar .mon-esforco .conditional-panel { margin-top: 12px; }
    @media (max-width: 991px) { .mon-definicoes { grid-template-columns: minmax(0, 1fr); } }
    @media (max-width: 575px) { .mon-campos { grid-template-columns: minmax(0, 1fr); } }
    .mon-estudio .mon-card li { margin-bottom: 7px; }
    .mon-estudio .mon-card li:last-child { margin-bottom: 0; }
    .mon-modelo { background: #E6F3F1; padding: 14px; border-radius: 8px;
      color: #0F3B5F; font-size: 1.2rem; text-align: center; margin: 12px 0; }
    .mon-compacto .mon-coluna { padding: 8px; }
    .mon-compacto .mon-card { padding: 8px 12px; margin-bottom: 8px; }
    .mon-compacto .mon-card p, .mon-compacto .mon-card ul { margin-bottom: 6px; }
    .mon-compacto .mon-card li { margin-bottom: 4px; }
    .mon-compacto .mon-modelo { padding: 8px; margin: 6px 0; }
    .mon-download-word, .mon-download-word:hover, .mon-download-word:focus {
      background-color: #0d6efd !important; border-color: #0d6efd !important; color: white !important; }
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
      bslib::nav_panel("Definições", icon = shiny::icon("sliders"),
        bslib::card_body(fillable = FALSE,

          # Duas colunas de cartões para aproveitar a largura da tela: à
          # esquerda o tempo e o esforço; à direita as respostas.
          shiny::div(class = "mon-definicoes",
            shiny::div(class = "mon-coluna",
              shiny::div(class = "mon-planejar",
                shiny::div(shiny::h5("1. Tempo")), shiny::div(),
                shiny::div(class = "mon-data", shiny::uiOutput(ns("ui_inicio"))),
                shiny::div(class = "mon-data", shiny::uiOutput(ns("ui_fim"))),
                shiny::div(class = "mon-frequencia", shiny::radioButtons(ns("frequencia"), "Frequência:",
                  choices = c("Diária" = "diaria", "Semanal" = "semanal", "Quinzenal" = "quinzenal", "Mensal" = "mensal"),
                  selected = "mensal", inline = TRUE)), shiny::div(),
                shiny::div(class = "mon-horario",
                  shiny::textInput(ns("horario"), "Horário da coleta (opcional):", placeholder = "Ex.: 8h, na preamar")
                ),
                shiny::div(class = "mon-horario",
                  shiny::radioButtons(ns("hora_real"), "Registrar a hora real de cada coleta?",
                    choices = c("Não" = "nao", "Sim" = "sim"), inline = TRUE)
                ),
                shiny::div(class = "mon-secao", shiny::h5("2. Onde e com que esforço")),
                shiny::div(class = "mon-secao"),
                  shiny::div(
                    shiny::uiOutput(ns("ui_locais")),
                    shiny::actionButton(ns("add_local"), "+ local", class = "btn-sm btn-outline-primary")
                  ),
                  shiny::div(class = "mon-esforco",
                    shiny::radioButtons(ns("tem_esforco"), "Registrar esforço em cada coleta?",
                      choices = c("Não" = "nao", "Sim" = "sim"), inline = TRUE),
                    shiny::conditionalPanel(sprintf("input['%s'] === 'sim'", ns("tem_esforco")),
                      shiny::textInput(ns("unidade_esforco"), "Unidade do esforço:", value = "viagens")
                    )
                  )
              )
            ),

            shiny::div(class = "mon-coluna",

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

      bslib::nav_panel("Desenho", icon = shiny::icon("diagram-project"),
        bslib::card_body(fillable = FALSE,
          bslib::navset_tab(id = ns("vistas_desenho"),
            bslib::nav_panel("Estrutura da série",
              shiny::uiOutput(ns("ui_desenho"))),
            bslib::nav_panel("Calendário anual",
              shiny::uiOutput(ns("ui_ano_calendario")),
              shiny::plotOutput(ns("calendario"), height = "360px")),
            bslib::nav_panel("Visitas por local",
              shiny::uiOutput(ns("ui_ano_locais")),
              shiny::uiOutput(ns("ui_desenho_locais")))
          ))),
      # Ficha: tabela completa, paginada; download disponível no Resumo.
      bslib::nav_panel("Ficha do delineamento", icon = shiny::icon("table"),
        bslib::card_body(fillable = FALSE,
          shiny::p(class = "small text-muted", "Tabela completa que o Excel vai trazer, com paginação. Uma linha por data e local, na ordem data, depois local."),
          DT::DTOutput(ns("tabela")),

          shiny::p(class = "small text-muted mt-2 mb-0",
            "Este é um protótipo: ajuste o que precisar no Excel, mantendo as colunas data e local. Colunas coloridas trazem fórmula pronta e não devem ser digitadas.")
        )
      ),

      bslib::nav_panel("Metodologia para artigo", icon = shiny::icon("paragraph"),
        bslib::card_body(fillable = FALSE,
          shiny::h5("Metodologia do monitoramento"),
          shiny::p(class = "small text-muted", "Texto baseado nas definições da série; complete o protocolo de coleta antes de usar no artigo."),
          shiny::uiOutput(ns("metodologia")))),
      # Aba 3: orientação curta, em duas colunas de texto para aproveitar a tela.
      bslib::nav_panel("Modelo e cuidados", icon = shiny::icon("book-open"),
        bslib::card_body(fillable = FALSE,
          shiny::div(class = "mon-compacto",
          bslib::layout_columns(col_widths = c(6, 6),
            shiny::div(class = "mon-coluna",
            shiny::div(class = "mon-card",
              shiny::h5("Quando usar"),
              shiny::p(class = "mb-0", "Poucos locais, muitas revisitas: captura, chuva ou qualidade da água. Muitas unidades em poucas visitas: use Longitudinal comparativo.")
            ),
            shiny::div(class = "mon-card",
              shiny::h5("Modelo de leitura da série"),
              shiny::div(class = "mon-modelo", "Y", shiny::tags$sub("i,t"),
                " = μ", shiny::tags$sub("i"), " + T", shiny::tags$sub("i"), "(t) + S",
                shiny::tags$sub("i"), "(t) + ε", shiny::tags$sub("i,t")),
              shiny::tags$ul(class = "mb-1",
                shiny::tags$li(shiny::strong("Y: "), "resposta no local i e na data t; μ: nível do local."),
                shiny::tags$li(shiny::strong("T: "), "tendência; ", shiny::strong("S: "), "padrão sazonal."),
                shiny::tags$li(shiny::strong("ε: "), "resíduo, que pode ter dependência temporal.")),
              shiny::p(class = "small mb-0", "Esquema aditivo didático; o painel não ajusta o modelo. A adequação depende dos dados.")),
            shiny::div(class = "mon-card",
              shiny::h5("O exemplo dos currais"),
              shiny::p(class = "mb-0", "Três locais, dois currais por local: currais são subamostras. Equilibre as marés e registre as subamostras em ficha vinculada. Revisitas não criam novos locais."))),
            shiny::div(class = "mon-coluna",
            shiny::div(class = "mon-card",
              shiny::h5("Antes de começar"),
              shiny::tags$ul(class = "mb-0",
                shiny::tags$li("Defina pergunta, locais fixos e coordenadas."),
                shiny::tags$li("Justifique frequência e duração pelos ciclos do fenômeno."),
                shiny::tags$li("Registre método, horário, esforço e responsável.")
              )
            ),
            shiny::div(class = "mon-card",
              shiny::h5("Durante a coleta"),
              shiny::tags$ul(class = "mb-0",
                shiny::tags$li("Mantenha condições comparáveis; confira marés e defesos."),
                shiny::tags$li("Zero é observado; ausência fica vazia, com motivo."),
                shiny::tags$li("Anote ocorrências e mudanças de método ou equipamento.")
              )
            ),
            shiny::div(class = "mon-card",
              shiny::h5("Revisão mensal"),
              shiny::tags$ul(class = "mb-0",
                shiny::tags$li("Revise lacunas, valores inesperados e protocolo."),
                shiny::tags$li("Considere dependência temporal e entre locais.")
              )
            ))
          )),
          shiny::p(class = "small text-muted mb-0",
            "Quando a série estiver andando, a análise é a de Séries Temporais, no menu Modelos de Regressão.")
        )
      ),
      bslib::nav_panel("Resumo", icon = shiny::icon("clipboard-check"),
        bslib::card_body(fillable = FALSE,
          shiny::h5("Resumo do planejamento"),
          shiny::uiOutput(ns("resumo")),
          bslib::layout_columns(col_widths = c(6, 6),
            shiny::tableOutput(ns("resumo_tabela")),
            shiny::div(
              shiny::div(class = "d-flex flex-wrap gap-2",
                shiny::downloadButton(ns("baixar_excel"), "Planilha Excel (.xlsx)", class = "btn-primary"),
                shiny::downloadButton(ns("baixar_word"), "Planejamento Word (.docx)", class = "btn-primary mon-download-word")),
              shiny::p(class = "small text-muted mt-2", "O Excel reúne os dados e os metadados, incluindo as fórmulas dos índices escolhidos. O Word reúne a metodologia prevista, o resumo e os pontos a completar no protocolo.")))))
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

    output$ui_desenho <- shiny::renderUI({
      shiny::plotOutput(session$ns("desenho"), height = paste0(430 + 65 * length(cfg()$locais), "px"))
    })
    output$desenho <- shiny::renderPlot(desenhar_plano_monitoramento(cfg()), res = 110)
    # Os seletores conservam o ano enquanto ele pertencer ao novo período.
    seletor_ano <- function(campo) {
      anos <- unique(format(cfg()$datas, "%Y"))
      atual <- shiny::isolate(input[[campo]])
      shiny::selectInput(session$ns(campo), "Ano do calendário:", choices = anos,
        selected = if (!is.null(atual) && atual %in% anos) atual else anos[1], width = "180px")
    }
    output$ui_ano_calendario <- shiny::renderUI(seletor_ano("ano_calendario"))
    output$ui_ano_locais <- shiny::renderUI(seletor_ano("ano_locais"))
    ano_do_plano <- function(campo) {
      anos <- unique(format(cfg()$datas, "%Y"))
      atual <- input[[campo]]
      if (!is.null(atual) && atual %in% anos) atual else anos[1]
    }
    output$calendario <- shiny::renderPlot({
      desenhar_calendario_monitoramento(cfg(), ano_do_plano("ano_calendario"))
    }, res = 110)
    output$ui_desenho_locais <- shiny::renderUI({
      shiny::plotOutput(session$ns("desenho_locais"), height = paste0(230 + 60 * length(cfg()$locais), "px"))
    })
    output$desenho_locais <- shiny::renderPlot({
      desenhar_calendario_monitoramento(cfg(), ano_do_plano("ano_locais"), por_local = TRUE)
    }, res = 110)
    output$metodologia <- shiny::renderUI({
      c0 <- cfg()
      respostas <- if (nrow(c0$medidas)) paste(c0$medidas$coluna, collapse = ", ") else "respostas a definir"
      shiny::div(class = "mon-compacto",
      bslib::layout_columns(col_widths = c(6, 6),
        shiny::div(class = "mon-coluna",
          shiny::div(class = "mon-card", shiny::h5("Texto de metodologia previsto"),
            shiny::p(sprintf("Serão acompanhados %d locais fixos (%s), de %s a %s, com frequência %s. As %d datas por local produzirão %d registros previstos. Respostas: %s.",
              length(c0$locais), paste(c0$locais, collapse = ", "),
              format(min(c0$datas), "%d/%m/%Y"), format(max(c0$datas), "%d/%m/%Y"),
              tolower(c0$frequencia_rotulo), length(c0$datas), length(c0$datas) * length(c0$locais), respostas)),
            shiny::p(if (c0$tem_esforco) paste0("O esforço será registrado em ", c0$unidade_esforco,
              if (nrow(c0$indices)) "; os índices serão calculados como medida ÷ esforço válido." else "; não foram selecionados índices.") else
              "O plano ainda não prevê registro de esforço; complete-o se a resposta depender desse denominador."),
            shiny::p(class = "mb-0", "Revisitas compõem a mesma série. A análise considerará dependência temporal e sazonalidade; o monitoramento, sozinho, não identifica causas."))),
        shiny::div(class = "mon-coluna",
          shiny::div(class = "mon-card", shiny::h5("Como completar o protocolo"),
            shiny::tags$ul(class = "mb-0",
              shiny::tags$li("Pergunta, região, seleção dos locais e coordenadas."),
              shiny::tags$li("Método, horário, unidades, esforço e responsável."),
              shiny::tags$li("Subamostras e séries de apoio, como chuva e maré."))),
          shiny::div(class = "mon-card", shiny::h5("O que conferir antes e durante a coleta"),
            shiny::tags$ul(class = "mb-0",
              shiny::tags$li("Marés, períodos biológicos, defesos e autorizações."),
              shiny::tags$li("Zero observado ≠ ausência: preserve data, local e motivo."),
              shiny::tags$li("Duração justificada; plano datado antes do campo."))),
          shiny::p(class = "small text-muted mb-0", "Base: Guia Painel Monitoramento. Texto completo e orientações no Word da aba Resumo."))))
    })
    output$resumo_tabela <- shiny::renderTable({
      c0 <- cfg()
      data.frame(Item = c("Locais fixos", "Datas por local", "Linhas da coleta", "Medidas", "Índices"),
        Quantidade = c(length(c0$locais), length(c0$datas), length(c0$locais) * length(c0$datas),
          nrow(c0$medidas), nrow(c0$indices)))
    }, striped = TRUE, bordered = TRUE, spacing = "s", rownames = FALSE)

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
            "A série cobre menos de dois anos; a avaliação da sazonalidade anual fica limitada e exige cautela.")
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
    output$baixar_word <- shiny::downloadHandler(
      filename = function() paste0("planejamento_monitoramento_", format(Sys.Date(), "%Y-%m-%d"), ".docx"),
      content = function(file) escrever_word_monitoramento(file, cfg())
    )
  })
}
