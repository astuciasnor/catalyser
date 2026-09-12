\name{converter_datas}
\alias{converter_datas}
\title{Converter uma coluna para data}
\usage{converter_datas(x)}
\arguments{\item{x}{Coluna a converter: Date, POSIXt, texto ou fator.}}
\value{Vetor da classe Date, com o mesmo comprimento de x.}
\description{Use dia-mês-ano ou ano-mês-dia, com barra ou traço:
12/09/2026, 12-09-2026, 2026/09/12 ou 2026-09-12.
Todos representam 12 de setembro de 2026. Também aceita 12.09.2026
e dia/mês sem zero à esquerda, como 1/9/2026. Prefira ano com quatro
dígitos. No R, o resultado aparece como 2026-09-12.
A ordem mês-dia-ano não é utilizada.}
\details{Datas já reconhecidas pelo R ou importadas do Excel também são aceitas.
Espaços nas extremidades são ignorados; vazios e NA permanecem ausentes.
A conversão usa lubridate. Valores não reconhecidos interrompem a conversão
e são informados para correção nos dados originais. Não interpreta números avulsos como datas do
Excel; importe a planilha com readxl primeiro. Data-hora é convertida em UTC.}
\examples{
converter_datas(c("12/09/2026", "12-09-2026", "2026/09/12", "2026-09-12"))
converter_datas(c("1/9/2026", "12.09.2026", "", NA))

# Para converter uma coluna da sua planilha:
dados <- data.frame(data_coleta = c("12/09/2026", "13/09/2026"))
dados$data_coleta <- converter_datas(dados$data_coleta)
}
