# =============================================================================
#  FUNÇÕES PRÓPRIAS DO PROJETO
# =============================================================================
#
#  Este arquivo só DEFINE funções; ele não executa nada sozinho. O script
#  (R/analise.R) carrega tudo daqui, na seção 1, com:
#
#      source(here::here("R", "funcoes.R"), encoding = "UTF-8")
#
#  Seções deste arquivo (Ctrl+Shift+O no RStudio mostra o sumário):
#    1. Conferência e preparo ..... conferir_base(), moda(), converter_datas()
#    2. Apresentação .............. fmt(), formatar_p(), tema_projeto(),
#                                  flextable_ocean()
#
#  Convenção: dentro das funções usamos pacote::funcao() (ex.: dplyr::mutate)
#  em vez de library(). Assim a função funciona em qualquer lugar, mesmo que
#  o pacote não tenha sido carregado.
# =============================================================================



# 1. Conferência e preparo ----------------------------------------------------

# conferir_base() ---------------------------------------------------------
#
# O que faz: compara a base reconstruída pelo script com a fotografia que
#            acompanha o projeto (dados/processados/base_compartilhada.rds).
#            Se algo divergir, avisa no console com a mensagem de ATENÇÃO,
#            mas NÃO interrompe a análise: quem decide o que fazer é você.
#
# Argumentos:
#   reconstruida        - a base que o script acabou de montar (data.frame)
#   caminho_fotografia  - caminho do arquivo .rds exportado
#   rotulo              - nome que aparece nas mensagens
#
# Retorna: TRUE quando as bases são equivalentes, FALSE caso contrário
#          (invisível nos dois casos). Nunca interrompe o script.
#
# A comparação é tolerante ao que a ida e volta pelo Excel muda sem alterar o
# significado (um inteiro que volta como decimal, por exemplo) e intolerante
# ao que importa: número de linhas, nomes de colunas e valores.
#
conferir_base <- function(reconstruida, caminho_fotografia,
                          rotulo = "base compartilhada") {

  # Sem fotografia não há o que comparar: avisa e segue adiante.
  if (!file.exists(caminho_fotografia)) {
    cat(sprintf("[%s] Fotografia ausente em '%s'; conferência não realizada.\n",
                rotulo, caminho_fotografia))
    return(invisible(FALSE))
  }

  # A fotografia viaja como .rds; as duas entram como data.frame para a
  # comparação valer para qualquer tabela vinda do R.
  fotografia <- as.data.frame(readRDS(caminho_fotografia))
  reconstruida <- as.data.frame(reconstruida)

  # Cada diferença encontrada entra nesta lista; no fim, a lista decide a mensagem.
  divergencias <- character()

  # Dimensões: o número de linhas precisa bater exatamente.
  if (!identical(nrow(reconstruida), nrow(fotografia))) {
    divergencias <- c(divergencias, sprintf(
      "número de linhas (reconstruída: %d; fotografia: %d)",
      nrow(reconstruida), nrow(fotografia)
    ))
  }

  # Colunas: nenhuma pode faltar nem sobrar.
  faltando <- setdiff(names(fotografia), names(reconstruida))
  sobrando <- setdiff(names(reconstruida), names(fotografia))
  if (length(faltando))
    divergencias <- c(divergencias, paste("colunas ausentes:", paste(faltando, collapse = ", ")))
  if (length(sobrando))
    divergencias <- c(divergencias, paste("colunas a mais:", paste(sobrando, collapse = ", ")))

  # Valores: compara coluna a coluna, respeitando o tipo de cada uma.
  # Números são comparados com uma pequena tolerância (1e-8), porque um
  # inteiro pode voltar como decimal sem mudar de significado.
  comuns <- intersect(names(fotografia), names(reconstruida))
  if (identical(nrow(reconstruida), nrow(fotografia))) {
    for (coluna in comuns) {
      a <- reconstruida[[coluna]]
      b <- fotografia[[coluna]]
      igual <- if (is.numeric(a) && is.numeric(b)) {
        isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = 1e-8))
      } else {
        isTRUE(all.equal(as.character(a), as.character(b)))
      }
      if (!igual) divergencias <- c(divergencias, sprintf("valores da coluna '%s'", coluna))
    }
  }

  # Nenhuma divergência: o preparo reproduziu a fotografia.
  if (!length(divergencias)) {
    cat(sprintf(
      "[%s] Reconstruída a partir da planilha e idêntica à fotografia: %d linhas e %d colunas.\n",
      rotulo, nrow(reconstruida), ncol(reconstruida)
    ))
    return(invisible(TRUE))
  }

  # Com divergência, o aviso explica o que mudou e aponta a referência.
  cat(sprintf("[%s] ATENÇÃO — a reconstrução divergiu da fotografia em: %s.\n",
              rotulo, paste(divergencias, collapse = "; ")))
  cat(sprintf("[%s] Use a fotografia ('%s') como referência e reveja o preparo.\n",
              rotulo, caminho_fotografia))
  invisible(FALSE)
}

# moda() ------------------------------------------------------------------
#
# O que faz: devolve o valor mais frequente de um vetor, ignorando os
#            ausentes. O R tem mean() e median(), mas não tem moda; esta
#            função preenche a lacuna quando a Trilha de Preparo imputa um
#            dado faltante pela moda.
#
# Argumentos:
#   x - um vetor de qualquer tipo (números, textos, fatores...)
#
# Exemplo:  moda(c("norte", "sul", "norte", NA))  ->  "norte"
#
moda <- function(x) {
  valores <- x[!is.na(x)]                                  # ignora os ausentes
  if (!length(valores)) return(NA)                         # tudo ausente: não há moda
  nomes <- names(sort(table(valores), decreasing = TRUE))  # contagem, da maior para a menor
  utils::type.convert(nomes[[1]], as.is = TRUE)            # devolve o valor no tipo original
}

# converter_datas() -------------------------------------------------------
#
# O que faz: converte uma coluna de texto em datas, aceitando dia-mês-ano
#            ou ano-mês-dia, com barra, traço ou ponto (12/09/2026,
#            2026-09-12, 12.09.2026). Datas já reconhecidas pelo R ou
#            importadas do Excel também passam. Valores que não forem datas
#            interrompem a conversão e são listados para correção nos dados
#            originais.
#
# Argumentos:
#   x - coluna a converter: Date, POSIXt, texto ou fator
#
# Retorna: um vetor da classe Date, com o mesmo comprimento de x.
#
converter_datas <- function(x) {

  # Preserva datas já reconhecidas; nas datas do Excel, retira o horário.
  if (inherits(x, "Date"))   return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x, tz = "UTC"))

  # Remove espaços das extremidades e trata células vazias como dados ausentes.
  texto <- trimws(as.character(x))
  texto[texto == ""] <- NA_character_

  # dmy = dia, mês, ano; ymd = ano, mês, dia. O pacote reconhece os separadores.
  resultado <- lubridate::parse_date_time(
    texto,
    orders = c("dmy", "ymd"),
    quiet = TRUE,  # A mensagem sobre datas inválidas será apresentada abaixo.
    tz = "UTC"    # Mantém uma referência fixa de horário durante a conversão.
  )

  # Aceita algarismos, barras, traços e pontos; não ignora texto extra na célula.
  tem_texto_extra <- grepl("[^0-9/.-]", texto)
  # Detecta valores preenchidos que precisam ser corrigidos na origem.
  problema <- !is.na(texto) & (is.na(resultado) | tem_texto_extra)
  if (any(problema)) {
    # Lista os valores distintos para você localizar na planilha original.
    exemplos <- paste(unique(texto[problema]), collapse = ", ")
    stop(
      sprintf(
        paste0(
          "%d valor(es) não reconhecido(s) como data: %s. ",
          "Corrija esses valores nos dados originais. Use dia-mês-ano ou ano-mês-dia. ",
          "Nenhum valor desta coluna foi convertido."
        ),
        sum(problema), exemplos
      ),
      call. = FALSE
    )
  }

  # Devolve somente as datas, sem horário.
  as.Date(resultado, tz = "UTC")
}



# 2. Apresentação -------------------------------------------------------------

fmt <- function(x, dig = 2) {
  # Mantém a precisão dos objetos; arredonda apenas a exibição.
  saida <- formatC(x, format = "f", digits = dig, decimal.mark = ",")
  saida[is.na(x)] <- "não calculado"
  saida
}

formatar_p <- function(p, no_texto = FALSE) {
  # Um p muito pequeno nunca é mostrado como zero.
  saida <- ifelse(p < 0.001, "< 0,001", fmt(p, 3))
  if (no_texto) saida <- ifelse(p < 0.001, paste("p", saida), paste("p =", saida))
  saida[is.na(p)] <- "não calculado"
  saida
}

tema_projeto <- function() {
  ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", colour = "#0F3B5F"),
      plot.title.position = "plot",
      legend.position = "bottom",
      plot.background = ggplot2::element_rect(fill = "white", colour = NA)
    )
}

flextable_ocean <- function(tab) {
  # Largura limitada à área útil do modelo Word; linhas não se dividem.
  flextable::flextable(tab) |>
    flextable::theme_booktabs() |>
    flextable::bg(bg = "#0F3B5F", part = "header") |>
    flextable::color(color = "white", part = "header") |>
    flextable::bold(part = "header") |>
    flextable::font(fontname = "Times New Roman", part = "all") |>
    flextable::fontsize(size = 10, part = "all") |>
    flextable::align(align = "center", part = "all") |>
    flextable::align(j = 1, align = "left", part = "all") |>
    flextable::padding(padding = 4, part = "all") |>
    flextable::autofit() |>
    flextable::fit_to_width(max_width = 6.1) |>
    flextable::set_table_properties(layout = "autofit", width = 1,
      opts_word = list(split = FALSE, repeat_headers = TRUE))
}
