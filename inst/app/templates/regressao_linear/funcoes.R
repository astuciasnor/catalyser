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
#    1. Preparo .................... moda(), converter_datas()
#    2. Apresentação .............. fmt(), formatar_p(), tema_projeto(),
#                                  flextable_ocean()
#
#  Convenção: dentro das funções usamos pacote::funcao() (ex.: dplyr::mutate)
#  em vez de library(). Assim a função funciona em qualquer lugar, mesmo que
#  o pacote não tenha sido carregado.
# =============================================================================



# 1. Preparo ---------------------------------------------------------------

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
