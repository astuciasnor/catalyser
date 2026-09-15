# Gera dois ZIPs para a conferência final no RStudio, sem abrir a IDE.
# Uso a partir de inst/app: Rscript tests/gerar_projetos_validacao.R <pasta-nova>
for (arquivo in c("mod_arrumar.R", "registro_tratamentos.R", "registro_bases.R",
                  "registro_execucoes.R", "registro_comunicacao.R",
                  "exportacao_comunicacao.R")) {
  source(file.path("modules", arquivo), encoding = "UTF-8")
}
destino <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(destino)) stop("Informe uma pasta nova para a conferência.")
dir.create(destino, recursive = TRUE, showWarnings = FALSE)
destino <- normalizePath(destino, winslash = "/")
brutos <- data.frame(id = 1:15,
  peso_g = c(102, 107, 98, 110, 105, 125, 130, 118, 128, 123, 144, 151, 139, 148, 145),
  especie = rep(c(" Bagre ", " Corvina ", " Pescada "), each = 5),
  data_coleta = rep(c("10/09/2026", "11/09/2026", "12/09/2026"), each = 5))
original <- file.path(destino, "campo_original.xlsx")
writexl::write_xlsx(list(biometria = brutos, notas = data.frame(nota = "Esta aba não entra no projeto.")), original)
info <- list(source = "local", file_name = "campo_original.xlsx", datapath = original,
  excel_sheet = "biometria", preparo_importacao = list(
    colunas = names(brutos), colunas_originais = names(brutos),
    tipos = list(data_coleta = "Date")))
importada <- brutos
importada$data_coleta <- catalyser::converter_datas(brutos$data_coleta)
pipeline <- list(
  list(tipo = "padronizar_texto", params = list(coluna = "especie", metodo = "squish"), ativa = TRUE),
  list(tipo = "calcular", params = list(nome = "peso_kg", expr = "peso_g / 1000"), ativa = TRUE))
compartilhada <- replay_pipeline(importada, pipeline)$df
base <- bases_novo_registro("base_0001", "Biometria para ANOVA", "base_biometria", finalidade = "anova", revisao_origem = 1L)
bases <- bases_adicionar_etapa(list(base), base$id, "filtrar",
  list(coluna = "peso_g", origem = "numerica", operador = ">=", valor = 100), compartilhada)
cache <- list(base_0001 = bases_recalcular_cache(compartilhada, bases[[1]], 1L))
bases <- bases_finalizar(bases, base$id, cache, 1L)
e <- list(id = "execucao_0001", analise_id = "anova_um_fator", tipo = "anova_um_fator",
  titulo = "Peso dos peixes entre espécies",
  parametros = list(resposta = "peso_g", fator = "especie", nivel_confianca = .95,
    rotulo_x = "Espécie", rotulo_y = "Peso (g)", tema = "minimal", ajuste_comparacoes = "tukey"),
  saidas_disponiveis = c("narrativa", "descritivos", "tabela", "comparacoes", "grafico", "pressupostos", "diagnosticos"),
  base_id = base$id, base_objeto = base$nome_r, base_tipo = "derivada", codigo_r = NULL)
for (multiplas in c(FALSE, TRUE)) {
  nome <- if (multiplas) "pesca_varias_analises" else "pesca_anova"
  registro <- list(execucao_0001 = e)
  if (multiplas) {
    grafico <- e
    grafico$id <- "execucao_0002"
    grafico$tipo <- grafico$analise_id <- "grafico_linhas"
    grafico$titulo <- "Peso por observação"
    grafico$parametros <- list(x = "id", y = "peso_g", grupo = "none", mostrar_pontos = TRUE,
      espessura_linha = 1, tema = "minimal", posicao_legenda = "right", rotulo_x = "Observação", rotulo_y = "Peso (g)")
    grafico$saidas_disponiveis <- "grafico"
    registro$execucao_0002 <- grafico
  }
  estado <- comunicacao_sincronizar(comunicacao_estado_vazio(), registro)
  estado <- comunicacao_definir_item(estado, e$id, saidas_selecionadas = e$saidas_disponiveis,
    saidas_disponiveis = e$saidas_disponiveis)
  manifesto <- comunicacao_manifesto(estado, registro,
    stats::setNames(as.list(rep("Atualizada", length(registro))), names(registro)),
    list(introducao = "Exemplo fictício para conferir o Projeto R exportado pela CatalyseR. Não representa uma conclusão científica sobre as espécies.",
      conclusao = "Documento de validação da geração do projeto."))
  zip <- file.path(destino, paste0(nome, ".zip"))
  exportacao_empacotar_projeto(zip, nome_projeto = nome, dados_brutos = brutos,
    base_resolvida = importada, dados_analise = compartilhada, pipeline = pipeline,
    base_externa = NULL, registro_bases = bases, cache_bases = cache,
    registro_execucoes = registro, manifesto = manifesto, revisao_origem = 1L,
    import_info = info, templates_dir = "templates")
  utils::unzip(zip, exdir = destino)
  projeto <- file.path(destino, exportacao_nome_curto(nome))
  stopifnot(identical(readxl::excel_sheets(file.path(projeto, "dados/brutos/biometria.xlsx")), "biometria"),
    !file.exists(file.path(projeto, "_quarto.yml")), !file.exists(file.path(projeto, "R/gerar_word.R")),
    file.exists(file.path(projeto, "dados/processados/base_biometria.xlsx")),
    identical(readRDS(file.path(projeto, "dados/processados/base_compartilhada.rds")), as.data.frame(compartilhada)))
  cat("ZIP CONFERIDO:", projeto, "\n")
}
