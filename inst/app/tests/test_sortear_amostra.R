# Teste do tratamento sortear_amostra (bases derivadas)
# Rodar a partir de inst/app:  Rscript tests/test_sortear_amostra.R
source("app.R", local = TRUE)
library(dplyr)

data(abalone_adultos, package = "EAPADados")
base <- abalone_adultos

codigo_bases_ui <- paste(
  readLines("modules/mod_bases_derivadas.R", encoding = "UTF-8"),
  collapse = "\n"
)
stopifnot(
  # A ação aparece no grupo "Recortes e resumos" e tem painel de parâmetros
  grepl('"Sortear subamostra" = "sortear_amostra"', codigo_bases_ui, fixed = TRUE),
  grepl("ramo_sam_n", codigo_bases_ui, fixed = TRUE),
  grepl("ramo_sam_semente", codigo_bases_ui, fixed = TRUE),
  grepl("sortear_amostra = list(coluna = input$ramo_sam_col", codigo_bases_ui, fixed = TRUE)
)

# --- 1. Replay com grupo: 200 por sexo --------------------------------------
etapa <- list(tipo = "sortear_amostra",
              params = list(coluna = "sexo", n = 200, semente = 42),
              ativa = TRUE)
r1 <- replay_pipeline(base, list(etapa))
tab <- table(r1$df$sexo)
stopifnot(
  !length(r1$erros),
  nrow(r1$df) == 400L,
  length(tab) == 2L, all(tab == 200L),
  # determinismo: dois replays devolvem o mesmo resultado
  identical(r1$df, replay_pipeline(base, list(etapa))$df)
)

# --- 2. O código gerado reproduz exatamente o sorteio da IDE -----------------
codigo <- tratamentos$sortear_amostra$codigo(etapa$params)
stopifnot(
  grepl("set.seed(42)", codigo, fixed = TRUE),
  grepl("dplyr::slice_sample(n = 200)", codigo, fixed = TRUE)
)
amb <- new.env(parent = globalenv())
amb$dados <- base
eval(parse(text = codigo), envir = amb)
stopifnot(identical(as.data.frame(amb$dados), as.data.frame(r1$df)))

# --- 3. Semente diferente muda o sorteio -------------------------------------
r_outra <- replay_pipeline(base, list(list(
  tipo = "sortear_amostra",
  params = list(coluna = "sexo", n = 200, semente = 7), ativa = TRUE)))
stopifnot(!identical(r_outra$df, r1$df))

# --- 4. Sorteio simples (sem grupo) ------------------------------------------
etapa_simples <- list(tipo = "sortear_amostra",
                      params = list(coluna = "", n = 100, semente = 42),
                      ativa = TRUE)
r4 <- replay_pipeline(base, list(etapa_simples))
stopifnot(nrow(r4$df) == 100L)
amb2 <- new.env(parent = globalenv())
amb2$dados <- base
eval(parse(text = tratamentos$sortear_amostra$codigo(etapa_simples$params)), envir = amb2)
stopifnot(identical(as.data.frame(amb2$dados), as.data.frame(r4$df)))

# --- 5. Validação com mensagens amigáveis ------------------------------------
tt <- tratamentos$sortear_amostra
stopifnot(
  grepl("excede", tt$validar(base, list(coluna = "sexo", n = 5000, semente = 42))),
  grepl("nao existe", tt$validar(base, list(coluna = "sexo2", n = 10, semente = 42))),
  grepl("excede", tt$validar(base, list(coluna = "", n = 99999, semente = 42))),
  !is.null(tt$validar(base, list(coluna = "", n = NULL, semente = 42))),
  is.null(tt$validar(base, list(coluna = "sexo", n = 200, semente = 42)))
)

# --- 6. Rótulo e SVG da trilha ------------------------------------------------
rot <- tt$rotulo(etapa$params)
stopifnot(
  grepl("200", rot) && grepl("sexo", rot) && grepl("42", rot),
  grepl("<svg", desenhar_trilha_svg(list(etapa)))
)

# --- 7. Integração com bases derivadas (não é redutor: aceita etapa depois) --
regs <- bases_adicionar(bases_vazio(), bases_novo_registro(
  "derivada_01", "Subamostra 200 por sexo", "base_subamostra", "geral"))
regs <- bases_adicionar_etapa(regs, "derivada_01", "sortear_amostra",
                              list(coluna = "sexo", n = 200, semente = 42), base)
regs <- bases_adicionar_etapa(regs, "derivada_01", "calcular",
                              list(nome = "razao_altura", expr = "altura_mm / diametro_mm"),
                              r1$df)
b <- bases_obter(regs, "derivada_01")
cache <- bases_recalcular_cache(base, b, 1L)
stopifnot(length(b$etapas) == 2L, !length(cache$erros), cache$linhas == 400L,
          "razao_altura" %in% names(cache$df))
codigo_base <- bases_codigo(b, incluir_print = FALSE)
amb3 <- new.env(parent = globalenv())
amb3$dados_analise <- base
eval(parse(text = codigo_base), envir = amb3)
stopifnot(identical(as.data.frame(amb3$base_subamostra), as.data.frame(cache$df)))

# --- 8. O script de preparo também gera o trecho do sorteio -------------------
script <- gerar_script_preparo(list(etapa),
                               list(source = "package", package_dataset = "abalone_adultos"))
stopifnot(grepl("slice_sample", script, fixed = TRUE))

# --- 9. Faltantes nos grupos bloqueiam o sorteio, inclusive no exportado ----
base_com_na <- data.frame(
  id = seq_len(605),
  sexo = c(rep("Femea", 300), rep("Macho", 300), rep(NA_character_, 5))
)
base_so_na <- data.frame(id = 1:5, sexo = rep(NA_character_, 5))
base_vazia <- data.frame(id = integer(), sexo = factor(character(), levels = c("Femea", "Macho")))
base_pequena <- data.frame(id = 1:305, sexo = c(rep("Femea", 300), rep("Macho", 5)))
for (entrada in list(base_com_na, base_so_na, base_vazia, base_pequena)) {
  mensagem <- tt$validar(entrada, etapa$params)
  replay <- replay_pipeline(entrada, list(etapa))
  amb_invalido <- new.env(parent = globalenv())
  amb_invalido$dados <- entrada
  erro_exportado <- tryCatch({
    eval(parse(text = codigo), envir = amb_invalido)
    NULL
  }, error = conditionMessage)
  stopifnot(
    !is.null(mensagem), length(replay$erros) == 1L,
    identical(replay$df, entrada),
    identical(erro_exportado, mensagem),
    identical(amb_invalido$dados, entrada)
  )
}
stopifnot(grepl("Trate os dados faltantes", tt$validar(base_com_na, etapa$params)))
# O sorteio simples nao usa sexo: NA nessa coluna nao impede sortear linhas.
simples_com_na <- replay_pipeline(base_com_na, list(etapa_simples))
stopifnot(!length(simples_com_na$erros), nrow(simples_com_na$df) == 100L)

# --- 10. Filtrar um fator nao obriga a sortear categorias que ficaram vazias --
so_femeas <- base[base$sexo == "Femea", , drop = FALSE]
stopifnot("Macho" %in% levels(so_femeas$sexo))
amostra_femeas <- replay_pipeline(so_femeas, list(etapa))
amb_femeas <- new.env(parent = globalenv())
amb_femeas$dados <- so_femeas
eval(parse(text = codigo), envir = amb_femeas)
stopifnot(
  is.null(tt$validar(so_femeas, etapa$params)),
  !length(amostra_femeas$erros), nrow(amostra_femeas$df) == 200L,
  all(amostra_femeas$df$sexo == "Femea"),
  identical(as.data.frame(amb_femeas$dados), amostra_femeas$df)
)
# Sem reposicao: o mesmo individuo nao deve aparecer duas vezes.
entrada_id <- base_com_na[!is.na(base_com_na$sexo), ]
amostra_id <- replay_pipeline(entrada_id, list(etapa))$df
stopifnot(nrow(amostra_id) == 400L, !anyDuplicated(amostra_id$id))

# --- 11. Erro em novo recálculo preserva a prévia e impede finalizar ---------
base_alterada <- base
base_alterada$sexo[1] <- NA
cache_com_erro <- bases_recalcular_cache(
  base_alterada, b, 2L, entrada_anterior = cache
)
stopifnot(
  length(cache_com_erro$erros) > 0L, isTRUE(cache_com_erro$preview_anterior),
  identical(cache_com_erro$df, cache$df),
  identical(bases_estado_cache(b, cache_com_erro, 2L), "Com erro"),
  length(bases_disponiveis_analise(regs, list(derivada_01 = cache_com_erro), 2L)) == 0L
)

# --- 12. O trecho final da exportacao preserva as conferencias e o sorteio ---
item <- list(base_id = "derivada_01", base_tipo = "derivada")
trecho <- exportacao_trecho_base(item, "teste", regs)
amb_trecho <- new.env(parent = globalenv())
amb_trecho$dados_analise <- base
eval(parse(text = trecho), envir = amb_trecho)
stopifnot(identical(as.data.frame(amb_trecho$dados_da_analise), as.data.frame(cache$df)))
amb_trecho$dados_analise <- base_alterada
erro_trecho <- tryCatch({
  eval(parse(text = trecho), envir = amb_trecho)
  NULL
}, error = conditionMessage)
stopifnot(grepl("Trate os dados faltantes", erro_trecho))

cat("OK: sortear_amostra (subamostra reprodutível) conferido de ponta a ponta\n")
