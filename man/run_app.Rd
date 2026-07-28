\name{run_app}
\alias{run_app}
\title{Abrir a aplicação Shiny da CatalyseR}
\usage{
run_app(
  launch.browser = getOption("shiny.launch.browser", interactive()),
  ...
)
}
\arguments{
\item{launch.browser}{\code{TRUE} para abrir a CatalyseR automaticamente
no navegador padrão; \code{FALSE} para apenas iniciar o servidor e mostrar
o endereço local no console. Também aceita uma função compatível com
\code{shiny::runApp()}.}

\item{...}{Outros argumentos repassados para \code{shiny::runApp()}, como
\code{port} e \code{host}.}
}
\description{
Inicia a aplicação Shiny incluída no pacote CatalyseR instalado.
}
\examples{
\dontrun{
# Abrir no navegador padrão
run_app(launch.browser = TRUE)

# Iniciar sem abrir o navegador automaticamente
run_app(launch.browser = FALSE)
}
}
