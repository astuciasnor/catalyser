# =============================================================================
#  ClaRa: o R escrito com clareza
#  Versão 0.7.3 (outubro de 2026)
# =============================================================================
#
#  Cada função da ClaRa responde a uma pergunta da pesquisa, em português.
#  Por trás de cada uma há uma "receita": código R comum, no estilo do
#  tidyverse. Com mostrar_codigo = TRUE, a função imprime a receita com os
#  nomes das suas colunas e depois a executa. O código mostrado é exatamente
#  o código que rodou.
#
#  Como carregar no analise.R:
#
#      source("R/clara.R", encoding = "UTF-8")
#
#  Este arquivo é só a porta de entrada: ele carrega os outros arquivos da
#  pasta R/, um por pergunta da pesquisa. Quem quiser estudar uma pergunta
#  abre um arquivo só e encontra tudo dela (Ctrl+Shift+O mostra o sumário):
#    clara_medias.R ............. comparar_medias(), grafico_medias(),
#                                 grafico_boxplot(), grafico_pares()
#    clara_medianas.R ........... comparar_medianas(), grafico_medianas()
#    clara_qualquer_analise.R ... medir_efeito(), escrever_resultados(),
#                                 grafico_residuos(), grafico_qq(),
#                                 salvar_tabelas(), salvar_figuras(),
#                                 registrar_ambiente()
#    clara_motor.R .............. ajuda() e as peças internas; o aluno não
#                                 precisa abrir
#
#  As colunas podem ser escritas sem aspas (peso_g) ou com aspas ("peso_g").
#  Para ver a lista de funções: ajuda(). Para uma delas: ajuda(comparar_medias).
# =============================================================================


# Descobre a pasta da ClaRa durante o source(). O source() mais recente é o
# que está lendo a ClaRa, mesmo quando um roteiro chama outro.
pasta_da_clara <- function() {
  for (quadro in rev(sys.frames())) {
    if (is.character(quadro$ofile)) return(dirname(normalizePath(quadro$ofile)))
  }
  "R"
}

# Os arquivos da ClaRa, na ordem em que a ajuda() lista as funções: primeiro
# as perguntas, depois o que serve a qualquer análise, por fim o motor.
arquivos_clara <- file.path(pasta_da_clara(), c(
  "clara_medias.R",
  "clara_medianas.R",
  "clara_qualquer_analise.R",
  "clara_motor.R"
))

# Carregamos na ordem inversa: o motor primeiro, porque os outros usam as
# peças dele; cada pergunta por último, porque acrescenta as suas receitas
# às listas de clara_qualquer_analise.R.
for (arquivo in rev(arquivos_clara)) source(arquivo, encoding = "UTF-8")
rm(arquivo)

# A versão desta cópia da ClaRa. Cada projeto exportado leva a sua cópia;
# a versão diz qual é, quando algo não funcionar igual em dois projetos.
# Ao mudar a ClaRa, atualize aqui e no título acima.
versao_clara <- "0.8.1"
message("ClaRa ", versao_clara, " carregada.")
