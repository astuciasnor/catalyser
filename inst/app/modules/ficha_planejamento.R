# Ficha de planejamento: o contrato 1 do ARQUITETURA.md, num objeto só.
# Cada módulo de planejamento preenche a sua parte; a planilha de coleta, o
# Projeto R e as telas de análise leem a mesma lista. O que não se sabe fica
# NULL: a ficha nunca inventa um valor.

# Os dez campos do contrato, nesta ordem.
ficha_campos <- c(
  "origem", "tipo", "pergunta", "resposta_coluna", "eixos", "unidade_coluna",
  "hierarquia", "n_planejado", "sorteio", "analise_sugerida"
)

# Ficha vazia: os dez campos existem e todos começam como NULL.
ficha_nova <- function() {
  ficha <- vector("list", length(ficha_campos))
  names(ficha) <- ficha_campos
  ficha
}

# Junta uma parte à ficha. Só os campos trazidos pela parte são trocados;
# a hierarquia é mesclada por dentro, porque dois módulos a descrevem.
ficha_mesclar <- function(ficha, parte) {
  if (is.null(ficha)) ficha <- ficha_nova()
  for (campo in intersect(names(parte), ficha_campos)) {
    novo <- parte[[campo]]
    antigo <- ficha[[campo]]
    if (identical(campo, "hierarquia") && is.list(novo) && is.list(antigo)) {
      novo <- utils::modifyList(antigo, novo, keep.null = TRUE)
    }
    # A forma ficha[campo] <- list(x) preserva o campo mesmo quando x é NULL.
    ficha[campo] <- list(novo)
  }
  ficha[ficha_campos]
}

# Atalhos de leitura usados pelas análises.
ficha_subamostra_coluna <- function(ficha) {
  as.character(ficha$hierarquia$subamostra_coluna %||% "")
}
ficha_fator_coluna <- function(ficha) {
  as.character(ficha$eixos$grupos$coluna %||% "")
}
ficha_tem_subamostras <- function(ficha) nzchar(ficha_subamostra_coluna(ficha))

# Parte da ficha preenchida pelo Quantos coletar (n por poder).
ficha_parte_n <- function(res) {
  list(n_planejado = list(
    valor = res$n_por_grupo,
    total = res$n_total,
    unidade = "unidades independentes por grupo",
    metodo = if (identical(res$metodo, "t")) {
      "n por poder: teste t bilateral (stats::power.t.test)"
    } else {
      "n por poder: ANOVA de um fator (stats::power.anova.test)"
    },
    premissas = list(grupos = res$grupos, medias = res$medias,
                     desvio = res$desvio, poder = res$poder, alfa = res$alfa)
  ))
}

# Parte da ficha preenchida pelas abas de estimação (uma média, uma proporção)
# e de comparação de proporções ou de relação entre variáveis do Quanto
# amostrar: cada cálculo traz o valor de destaque (total ou por grupo), o
# total, a unidade, o método e as premissas. O n_planejado é um só: calcular
# de novo, por qualquer método, sobrescreve o anterior.
ficha_parte_n_calculo <- function(res) {
  list(n_planejado = list(
    valor = res$n,
    total = res$n_total,
    unidade = res$unidade,
    metodo = res$metodo_descricao,
    premissas = res$premissas
  ))
}

# Parte da ficha preenchida pelo Como sortear (registro + unidades escolhidas).
ficha_parte_sorteio <- function(s) {
  if (is.null(s) || is.null(s$registro)) return(list(sorteio = NULL))
  r <- s$registro
  id <- r$id
  list(sorteio = list(
    marco = s$marco %||% NULL,
    identificador = id,
    metodo = r$metodo,
    semente = r$semente,
    # Só os parâmetros que o método usou; os que não se aplicam saem.
    parametros = Filter(Negate(is.null), r[setdiff(names(r), c("metodo", "semente", "id"))]),
    unidades_selecionadas = if (!is.null(id) && id %in% names(s$sorteados)) {
      as.character(s$sorteados[[id]])
    } else NULL
  ))
}

# Nome por extenso das análises que o planejamento sabe sugerir.
ficha_rotulo_analise <- function(analise) {
  rotulos <- c(
    anova_um_fator = "ANOVA de um fator",
    anova_mista_subamostras = "ANOVA com subamostras (modelo misto)",
    regressao_linear_simples = "regressão linear simples",
    series_temporais = "análise de séries temporais (menu Modelos de Regressão)"
  )
  if (is.null(analise) || !nzchar(analise)) return("não definida")
  if (analise %in% names(rotulos)) unname(rotulos[analise]) else analise
}

# Tabela campo | valor, para a aba da planilha e para leitura humana no projeto.
ficha_tabela <- function(ficha) {
  ficha <- ficha_mesclar(ficha_nova(), ficha %||% list())
  texto <- function(x) {
    if (is.null(x)) return("NULL (não informado)")
    if (is.data.frame(x)) return(sprintf("tabela com %d linha(s): %s", nrow(x), paste(names(x), collapse = ", ")))
    if (is.list(x)) {
      partes <- vapply(names(x), function(nm) paste0(nm, " = ", texto(x[[nm]])), character(1))
      return(paste(partes, collapse = "; "))
    }
    paste(as.character(x), collapse = ", ")
  }
  data.frame(
    campo = ficha_campos,
    valor = vapply(ficha_campos, function(campo) texto(ficha[[campo]]), character(1)),
    stringsAsFactors = FALSE, row.names = NULL
  )
}

# Grava a ficha dentro de um projeto, na pasta dados/ que o molde já tem
# (sem criar pasta nova): o objeto (.rds) e a versão legível (.csv).
# Devolve as linhas do README que explicam como recuperá-la no R.
ficha_salvar_projeto <- function(ficha, pasta_projeto) {
  if (is.null(ficha)) return(character())
  dir.create(file.path(pasta_projeto, "dados"), showWarnings = FALSE, recursive = TRUE)
  ficha <- ficha_mesclar(ficha_nova(), ficha)
  saveRDS(ficha, file.path(pasta_projeto, "dados", "ficha_planejamento.rds"))
  utils::write.csv(ficha_tabela(ficha), file.path(pasta_projeto, "dados", "ficha_planejamento.csv"),
                   row.names = FALSE, fileEncoding = "UTF-8")
  c("", "## Ficha de planejamento", "",
    "`dados/ficha_planejamento.rds` guarda as decisões do planejamento (eixos, unidade independente,",
    "hierarquia, n planejado, sorteio e análise sugerida). Para consultá-la no R:", "",
    "```r", "ficha <- readRDS(here::here(\"dados\", \"ficha_planejamento.rds\"))", "str(ficha, max.level = 1)", "```", "",
    "Campos em `NULL` não foram informados no planejamento. A versão em tabela está em `dados/ficha_planejamento.csv`.")
}

# Aviso das telas de análise: mostra a sugestão que veio do planejamento.
ficha_aviso_analise_ui <- function(ficha, analise_id) {
  sugerida <- ficha$analise_sugerida
  if (is.null(ficha) || is.null(sugerida)) {
    return(shiny::div(class = "alert alert-light border py-2 small",
      "Nenhuma ficha de planejamento com análise sugerida. Escolha as colunas abaixo."))
  }
  coincide <- identical(sugerida, analise_id)
  shiny::div(
    class = if (coincide) "alert alert-success py-2 small" else "alert alert-warning py-2 small",
    shiny::icon("link"), " ", shiny::tags$b("Sugestão do planejamento: "), ficha_rotulo_analise(sugerida),
    " (", ficha$origem %||% "planejamento", "). ",
    if (coincide) "As colunas previstas na ficha foram pré-selecionadas; confira se ainda correspondem à planilha importada."
    else "Esta tela faz outra análise; confira se ela responde à pergunta planejada."
  )
}

# ---- Fio visível entre os módulos do planejamento ----------------------------
# O cabeçalho "Planejamento atual" e a dica de análise futura só LEEM a ficha.
# Nada aqui decide pelo pesquisador: é um lembrete do estudo que ele esboçou.

# Nome legível do delineamento guardado em ficha$tipo.
ficha_nome_delineamento <- function(tipo) {
  if (is.null(tipo) || !nzchar(tipo)) return(NULL)
  observacionais <- if (exists("catalogo_delineamentos_observacionais")) catalogo_delineamentos_observacionais() else list()
  experimentais <- c(dic = "DIC", dbc = "DBC", dql = "Quadrado latino", fatorial = "Fatorial",
                     split_plot = "Parcelas subdivididas")
  outros <- c(monitoramento = "Monitoramento (série temporal)")
  if (tipo %in% names(observacionais)) return(observacionais[[tipo]]$titulo)
  if (tipo %in% names(experimentais)) return(unname(experimentais[tipo]))
  if (tipo %in% names(outros)) return(unname(outros[tipo]))
  tipo
}

# O eixo categórico do estudo (grupos, impacto ou gradiente), com coluna e níveis.
ficha_eixo_principal <- function(ficha) {
  for (eixo in c("grupos", "impacto", "gradiente")) {
    dados <- ficha$eixos[[eixo]]
    if (!is.null(dados)) return(c(list(eixo = eixo), dados))
  }
  NULL
}

# Número de níveis do eixo principal, ou NULL se o planejamento não o declarou.
ficha_niveis <- function(ficha) {
  eixo <- ficha_eixo_principal(ficha)
  # Eixo contínuo (estudo de gradiente) não tem níveis de grupos: o número de
  # estações fica no eixo, mas não deve preencher as abas do Quanto amostrar.
  if (isTRUE(eixo$continua)) return(NULL)
  niveis <- eixo$niveis %||% ficha$hierarquia$niveis
  if (is.null(niveis) || !is.finite(niveis) || niveis < 1) NULL else as.integer(niveis)
}

# Cabeçalho curto que diz em que estudo o usuário está trabalhando.
ficha_cabecalho_ui <- function(ficha) {
  nome <- ficha_nome_delineamento(ficha$tipo)
  if (is.null(nome)) {
    return(shiny::div(class = "alert alert-light border py-2 mb-3 small",
      shiny::icon("circle-info"), " ",
      "Ainda não há um planejamento atual. Monte o delineamento primeiro em ",
      shiny::tags$b("Delineamentos observacionais"),
      " e clique em Usar esta ficha nas análises. Você pode seguir aqui mesmo assim, digitando os valores."))
  }
  eixo <- ficha_eixo_principal(ficha)
  # Eixo contínuo (estudo de gradiente): a contagem é de estações, não níveis.
  continuo <- !is.null(eixo) && isTRUE(eixo$continua)
  palavra_eixo <- c(grupos = "fator", impacto = "local", gradiente = "gradiente")
  palavra_niveis <- c(grupos = "níveis", impacto = "condições", gradiente = "faixas")
  partes <- c(paste("delineamento", nome))
  if (!is.null(eixo)) {
    partes <- c(partes,
      if (!is.null(eixo$coluna) && nzchar(eixo$coluna)) paste(palavra_eixo[[eixo$eixo]], eixo$coluna),
      if (continuo) {
        paste(eixo$niveis, "estações")
      } else if (!is.null(ficha_niveis(ficha))) {
        paste(ficha_niveis(ficha), palavra_niveis[[eixo$eixo]])
      })
  }
  if (!is.null(ficha$unidade_coluna)) partes <- c(partes, paste("unidade", ficha$unidade_coluna))
  if (!is.null(ficha$hierarquia$momentos)) {
    partes <- c(partes, paste(length(ficha$hierarquia$momentos), "momentos"))
  }
  # O que os outros módulos já acrescentaram aparece como etapa cumprida.
  etapas <- c(
    if (!is.null(ficha$n_planejado)) {
      if (continuo) {
        sprintf("n planejado: %s estações", ficha$n_planejado$valor)
      } else {
        # A unidade do valor vem do escritor de cada aba ("por grupo" ou "no
        # total"); fichas antigas sem unidade caem no texto de sempre.
        unidade_n <- as.character(ficha$n_planejado$unidade %||% "por grupo")
        dplyr::case_when(
          grepl("no total", unidade_n, fixed = TRUE) ~ sprintf("n planejado: %s no total", ficha$n_planejado$valor),
          grepl("por grupo", unidade_n, fixed = TRUE) ~ sprintf("n planejado: %s por grupo", ficha$n_planejado$valor),
          TRUE ~ sprintf("n planejado: %s", ficha$n_planejado$valor)
        )
      }
    },
    if (!is.null(ficha$sorteio)) sprintf("sorteio feito (semente %s)", ficha$sorteio$semente)
  )
  shiny::div(class = "alert alert-light border py-2 mb-3",
    style = "border-left:4px solid #0F3B5F !important;",
    shiny::icon("link"), " ", shiny::tags$b("Planejamento atual: "), paste0(paste(partes, collapse = ", "), "."),
    if (length(etapas)) shiny::tags$span(class = "text-muted", paste0(" ", paste(etapas, collapse = "; "), ".")),
    ficha_dica_analise_ui(ficha)
  )
}

# Dica leve das análises que os dados provavelmente vão pedir. Sem sugestão, nada aparece.
ficha_dica_analise_ui <- function(ficha) {
  sugerida <- ficha$analise_sugerida
  if (is.null(sugerida) || !nzchar(sugerida)) return(NULL)
  texto <- switch(sugerida,
    anova_um_fator = "com este desenho, os dados provavelmente pedirão uma ANOVA de um fator; se houver subamostras dentro de cada unidade (sub-pooling), uma ANOVA com subamostras.",
    anova_mista_subamostras = "com este desenho, os dados provavelmente pedirão uma ANOVA com subamostras (modelo misto), com a unidade como efeito aleatório; a ANOVA sobre a média de cada unidade é o caminho simples equivalente.",
    regressao_linear_simples = "com este desenho, os dados provavelmente pedirão uma regressão da resposta contra a variável do gradiente, começando pela reta; se a relação não for reta, considere transformação ou termo quadrático.",
    paste0("com este desenho, os dados provavelmente pedirão: ", ficha_rotulo_analise(sugerida), ".")
  )
  shiny::div(class = "small mt-1", style = "color:#2E7D8F;",
    shiny::icon("lightbulb"), " ", shiny::tags$b("Análise futura: "), texto,
    shiny::tags$span(class = "text-muted", " É uma orientação; a escolha fica com você."))
}
