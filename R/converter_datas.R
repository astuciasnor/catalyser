#' Converter uma coluna para data
#'
#' Use dia-mês-ano ou ano-mês-dia, com barra ou traço:
#' 12/09/2026, 12-09-2026, 2026/09/12 ou 2026-09-12.
#' Todos representam 12 de setembro de 2026. Também aceita 12.09.2026
#' e dia/mês sem zero à esquerda, como 1/9/2026. Prefira ano com quatro
#' dígitos. No R, o resultado aparece como 2026-09-12.
#' A ordem mês-dia-ano não é utilizada.
#'
#' Datas já reconhecidas pelo R ou importadas do Excel também são aceitas.
#' Espaços nas extremidades são ignorados; vazios e NA permanecem ausentes.
#' A conversão usa lubridate. Valores não reconhecidos interrompem a conversão
#' e são informados para correção nos dados originais. Não interpreta números
#' avulsos como datas do Excel; importe a planilha com readxl primeiro.
#'
#' @param x Coluna a converter: Date, POSIXt, texto ou fator.
#' @return Vetor da classe Date, com o mesmo comprimento de x.
#' @export
#' @examples
#' converter_datas(c("12/09/2026", "12-09-2026", "2026/09/12", "2026-09-12"))
#' converter_datas(c("1/9/2026", "12.09.2026", "", NA))
#'
#' # Para converter uma coluna da sua planilha:
#' dados <- data.frame(data_coleta = c("12/09/2026", "13/09/2026"))
#' dados$data_coleta <- converter_datas(dados$data_coleta)
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
    # Lista os valores distintos para o pesquisador localizar na planilha original.
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
