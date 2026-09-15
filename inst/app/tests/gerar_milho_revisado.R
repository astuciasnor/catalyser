# Conferência local das duas execuções enviadas pelo autor, sem alterar o original.
# Uso a partir de inst/app: source(...), com origem/destino nos argumentos do Rscript.
for (arquivo in c("mod_arrumar.R", "mod_organizar_variaveis.R", "registro_tratamentos.R",
                  "registro_bases.R", "registro_execucoes.R", "registro_comunicacao.R",
                  "exportacao_comunicacao.R")) source(file.path("modules", arquivo), encoding = "UTF-8")
args <- commandArgs(trailingOnly = TRUE)
origem <- args[1]
destino <- args[2]
stopifnot(!is.na(origem), !is.na(destino), !dir.exists(destino))
info <- readRDS(file.path(origem, "metadados/origem_dados.rds"))
manifesto <- readRDS(file.path(origem, "metadados/manifesto_editorial.rds"))
registro <- readRDS(file.path(origem, "metadados/registro_execucoes.rds"))
bases <- readRDS(file.path(origem, "metadados/registro_bases.rds"))
stopifnot(length(bases) == 0L)
brutos <- as.data.frame(readxl::read_excel(file.path(origem, "dados/brutos/aulas_bioestatistica.xlsx"), sheet = info$excel_sheet))
compartilhada <- readRDS(file.path(origem, "dados/processados/base_compartilhada.rds"))
pipeline <- list(
  list(tipo = "organizar", ativa = TRUE, params = list(selecionar = c("Var", "Prod"))),
  list(tipo = "organizar", ativa = TRUE, params = list(renomear = c(Var = "Variedade", Prod = "Producao"))),
  list(tipo = "organizar", ativa = TRUE, params = list(tipos = list(Variedade = "fator"))))
stopifnot(identical(as.data.frame(replay_pipeline(brutos, pipeline)$df), as.data.frame(compartilhada)))
dir.create(destino, recursive = TRUE)
projeto <- exportacao_criar_projeto(destino, "milho", brutos, brutos, compartilhada,
  pipeline, NULL, bases, list(), registro, manifesto, 1L, info)
zip::zipr(file.path(destino, "milho_revisado.zip"), basename(projeto), root = destino)
env <- new.env(parent = globalenv())
sys.source(file.path(projeto, "R/funcoes.R"), env)
env$here <- function(...) file.path(projeto, ...)
qmd <- readLines(file.path(projeto, "relatorios/relatorio.qmd"), encoding = "UTF-8")
grDevices::pdf(tempfile(fileext = ".pdf"))
modelos <- 0L
for (ch in env$chunks_do_relatorio(qmd)) {
  if (ch$rotulo %in% c("codigo-do-script", "atualizar", "instalar")) next
  codigo <- qmd[ch$ini:ch$fim]
  if (any(grepl("#| eval: false", codigo, fixed = TRUE))) next
  codigo <- codigo[!grepl("^library\\(here\\)", codigo)]
  valor <- eval(parse(text = codigo), env)
  if (grepl("analise-modelo$", ch$rotulo)) {
    modelos <- modelos + 1L
    stopifnot(isTRUE(all.equal(coef(env$modelo), coef(stats::aov(Producao ~ Variedade, data = compartilhada)))))
  }
  if (grepl("^fig-.*-grupos$", ch$rotulo)) {
    ggplot2::ggsave(file.path(destino, paste0(ch$rotulo, ".png")), valor, width = 7, height = 4, dpi = 150)
  }
}
grDevices::dev.off()
stopifnot(modelos == 2L,
  env$conferir_codigo(file.path(projeto, "relatorios/relatorio.qmd"), file.path(projeto, "R/analise.R")))
cat("MILHO CONFERIDO:", projeto, "— duas execuções preservadas e reproduzidas.\n")
