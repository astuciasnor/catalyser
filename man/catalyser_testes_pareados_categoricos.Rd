\name{catalyser_testes_pareados_categoricos}
\encoding{UTF-8}
\alias{catalyser_friedman}
\alias{catalyser_mcnemar}
\title{Testes para dados pareados}
\usage{
catalyser_friedman(dados, p)

catalyser_mcnemar(dados, p)
}
\arguments{
\item{dados}{Um data.frame com os dados da análise.}

\item{p}{Lista de parâmetros. Para Friedman, informe \code{resposta},
\code{condicao}, \code{bloco}, \code{posteste} e \code{nivel_confianca}. Para
McNemar, informe \code{variavel_1}, \code{variavel_2} e \code{correcao}.}
}
\value{
Uma lista com narrativa, tabela, gráfico, pressupostos, diagnósticos, console e
o objeto do teste. Friedman também devolve as comparações pós-teste.
}
\description{
Friedman compara três ou mais condições medidas nos mesmos blocos ou indivíduos.
McNemar compara duas respostas binárias obtidas nos mesmos pares.
}
