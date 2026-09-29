# Execute de inst/app. Unidade de exportacao_sanitizar_molde(): as chamadas
# que a rota legada escreve com o pacote local são reescritas para as funções
# equivalentes de R/funcoes.R (moda, converter_datas) e os comentários que
# citam a IDE ganham texto neutro — sem tocar na conferência
# (catalyser_conferir_base) nem em outras chamadas com pacote.
for (arquivo in c("registro_tratamentos.R", "registro_bases.R", "registro_execucoes.R",
                  "registro_comunicacao.R", "exportacao_comunicacao.R")) {
  source(file.path("modules", arquivo), encoding = "UTF-8")
}

entrada <- c(
  "# Datas: use catalyser::converter_datas(); ajuda em ?catalyser::converter_datas.",
  "dados <- dados |> dplyr::mutate(data = catalyser::converter_datas(data))",
  "dados <- dados |> dplyr::mutate(peso_log = catalyser::catalyser_moda(peso))",
  "dados <- dados |> dplyr::mutate(grupo = trat_moda(grupo))",
  "dados <- dados |> dplyr::mutate(quando = converter_data(quando))",
  "catalyser_conferir_base(dados, here(\"dados\", \"processados\", \"base_compartilhada.rds\"), rotulo = \"Base\")",
  "# A CatalyseR também exportou uma fotografia de `dados_analise`. A função",
  "# Houve mudança estrutural promovida na CatalyseR (Pivotar/Separar ou",
  "# viu na tela, o script carrega a fotografia materializada na exportação.",
  "# Escolhas da importação e reestruturações, na ordem registrada na IDE.",
  "# a receita registrada na CatalyseR. Sai dados_da_analise, lido adiante.",
  "dados <- EAPADados::isoproteica_bagre",
  "dados <- catalyser::funcao_que_nao_existe(dados)"
)

saida <- exportacao_sanitizar_molde(entrada)

stopifnot(
  identical(saida[1], "# Datas: use converter_datas(), definida em R/funcoes.R."),
  identical(saida[2], "dados <- dados |> dplyr::mutate(data = converter_datas(data))"),
  identical(saida[3], "dados <- dados |> dplyr::mutate(peso_log = moda(peso))"),
  identical(saida[4], "dados <- dados |> dplyr::mutate(grupo = moda(grupo))"),
  identical(saida[5], "dados <- dados |> dplyr::mutate(quando = converter_datas(quando))"),
  # A conferência do molde é catalyser_conferir_base() e NÃO é reescrita.
  identical(saida[6], entrada[6]),
  # Comentários dos trechos compartilhados, com texto neutro no molde.
  identical(saida[7], "# O projeto traz uma fotografia de `dados_analise` em dados/processados/. A função"),
  identical(saida[8], "# Houve mudança estrutural (Pivotar/Separar ou"),
  identical(saida[9], "# viu na tela, o script carrega a fotografia que acompanha o projeto."),
  identical(saida[10], "# Escolhas da importação e reestruturações, nesta ordem:"),
  identical(saida[11], "# a receita registrada. Sai dados_da_analise, lido adiante."),
  # Chamadas com pacote fora da lista ficam intactas (EAPADados, outras).
  identical(saida[12], entrada[12]),
  identical(saida[13], entrada[13])
)

cat("OK: exportacao_sanitizar_molde reescreve moda/converter_datas e os comentários da IDE, preservando catalyser_conferir_base e as demais chamadas com pacote.\n")
