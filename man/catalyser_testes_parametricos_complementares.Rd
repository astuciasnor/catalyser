\name{catalyser_testes_parametricos_complementares}
\encoding{UTF-8}
\alias{catalyser_anova_medidas_repetidas}
\alias{catalyser_variancia_uma}
\alias{catalyser_variancias_duas}
\title{Testes paramétricos complementares}
\usage{
catalyser_anova_medidas_repetidas(dados, p)

catalyser_variancia_uma(dados, p)

catalyser_variancias_duas(dados, p)
}
\arguments{
\item{dados}{Um data.frame com os dados da análise.}

\item{p}{Lista de parâmetros. Para medidas repetidas, informe \code{resposta},
\code{sujeito} e \code{momento}. Para uma variância, informe \code{variavel} e
\code{desvio_hipotetico}. Para duas variâncias, informe \code{resposta} e
\code{grupo}.}
}
\value{
Uma lista com narrativa, tabela, gráfico, pressupostos, diagnósticos, console e
o objeto do teste ou modelo.
}
\description{
Executa a ANOVA de medidas repetidas em formato longo, o teste qui-quadrado para
uma variância ou o teste F para a razão de duas variâncias. As funções são usadas
pela interface e pelo Projeto R exportado pela CatalyseR.
}
