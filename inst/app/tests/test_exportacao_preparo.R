invisible(Sys.setlocale("LC_ALL", "English_United States.utf8"))
source("tests/test_refinamentos_preparo.R", encoding="UTF-8")
library(dplyr)
app_dir <- normalizePath(".", winslash="/")
saida <- tempfile("conferencia_preparo_");dir.create(saida)
ps <- isolate(pipeline()); preparada <- isolate(final())
# Cada derivada recebe a compartilhada inteira, e mantém somente seu recorte.
bs <- bases_adicionar(bases_vazio(), bases_novo_registro("base_0001", "Peso acima de 100 g", "base_peso_alto", finalidade="anova"))
bs <- bases_adicionar_etapa(bs, "base_0001", "filtrar", list(coluna="peso_g", origem="numerica", operador=">", valor=100), preparada)
caches <- list(base_0001=bases_recalcular_cache(preparada, bases_obter(bs,"base_0001"), 1L))
bs <- bases_finalizar(bs, "base_0001", caches, 1L)
stopifnot(all(caches$base_0001$df$peso_g>100),nrow(caches$base_0001$df)<nrow(preparada))
info <- list(source="local",file_name="Treino-Transformacoes.xlsx",datapath=file.path(app_dir,"dados/Treino-Transformacoes.xlsx"),excel_sheet="biometria")
e <- list(id="execucao_0001",analise_id="anova_um_fator",tipo="anova_um_fator",titulo="Massa por espécie",
 parametros=list(resposta="massa_kg",fator="especie",nivel_confianca=.95,rotulo_x="Espécie",rotulo_y="Massa (kg)"),
 saidas_disponiveis=c("narrativa","tabela","grafico","pressupostos","diagnosticos"),
 base_id="dados_analise",base_objeto="dados_analise",base_tipo="compartilhada",codigo_r=NULL)
extrair <- function(linhas, nome, script=FALSE) {
 if (script) {
  inicio <- which(linhas==paste0("## ---- ",nome," ----"))+1L
  fins <- which(grepl("^## ---- ",linhas));fim <- min(fins[fins>inicio])-1L
 } else {
  inicio <- which(linhas==paste0("#| label: ",nome))+1L
  fins <- which(linhas=="```");fim <- min(fins[fins>inicio])-1L
 }
 linhas[inicio:fim]
}
for (derivada in c(FALSE,TRUE)) {
 nome <- if(derivada) "biometria_filtrada" else "biometria_treino"
 if(derivada) {e$base_id <- "base_0001";e$base_objeto <- "base_peso_alto";e$base_tipo <- "derivada"}
 execucoes <- list(execucao_0001=e)
 manifesto <- comunicacao_manifesto(comunicacao_estado_vazio(),execucoes,list(execucao_0001="Atualizada"))
 destino <- tempfile("exportado_",tmpdir=saida);dir.create(destino)
 projeto <- exportacao_criar_projeto(destino,nome,df,df,preparada,ps,NULL,bs,caches,execucoes,manifesto,1L,info,file.path(app_dir,"templates"))
 stopifnot(file.exists(file.path(projeto,paste0(nome,".Rproj"))),
           identical(readxl::excel_sheets(file.path(projeto,"dados/brutos",exportacao_nome_planilha(info))), "biometria"),
           isTRUE(all.equal(
             as.data.frame(readxl::read_excel(file.path(projeto,"dados/brutos",exportacao_nome_planilha(info)))),
             as.data.frame(readxl::read_excel(info$datapath, sheet="biometria")))) )
 esperada <- if(derivada)caches$base_0001$df else preparada
 esperada$especie <- factor(esperada$especie)
 esperada <- droplevels(tidyr::drop_na(esperada,massa_kg,especie))
 for (script in c(TRUE,FALSE)) {
  arq <- if(script)"R/analise.R" else "relatorios/relatorio.qmd"
  linhas <- readLines(file.path(projeto,arq),encoding="UTF-8")
  env <- new.env(parent=globalenv());env$dados_brutos <- df;sys.source(file.path(projeto,"R/funcoes.R"),env)
  # Somente a gravação vai para a pasta temporária do projeto desta conferência.
  env$here <- function(...) file.path(projeto,...)
  if (script) {
    eval(parse(text=extrair(linhas,"tratar",TRUE)),env)
    eval(parse(text=extrair(linhas,"preparar-analise",TRUE)),env)
  } else {
    eval(parse(text=extrair(linhas,"carregar-bases",FALSE)),env)
    eval(parse(text=extrair(linhas,"preparo",FALSE)),env)
  }
  stopifnot(isTRUE(all.equal(as.list(env$dados),as.list(esperada))))
  stopifnot(length(env$cores_grupos) >= nlevels(env$dados$especie))
  modelo <- stats::aov(massa_kg~especie,data=env$dados)
  referencia <- stats::aov(massa_kg~especie,data=esperada)
  stopifnot(isTRUE(all.equal(stats::coef(modelo),stats::coef(referencia))))
 }

 cat("PASSOU: projeto",nome,"reproduz",nrow(esperada),"linhas no script e no relatório; aba utilizada e nome Rproj conferidos.\n")
}
