# CatalyseR 0.1.17 — barras no teste t e instalação correta

Data: 02/10/2026.

A biblioteca padrão C:/R/R-4.6.1/library ainda continha catalyser 0.1.12, enquanto a biblioteca de validação continha 0.1.16. Isso permitia que a tela e o relatório fossem executados com a versão antiga. A instalação padrão foi atualizada; sua cópia anterior foi preservada em APOIO/temp/catalyser_instalacao_padrao_0.1.12_antes.zip. Uma sessão R já aberta precisa ser reiniciada para carregar a atualização.

A tela também continha dois desenhos na aba Gráfico do Teste: um boxplot e, abaixo, as médias com barras. Para o teste t independente, agora essa aba mostra somente as médias com barras. As outras variantes mantêm seus gráficos próprios. A figura de exploração por boxplot continua disponível no caderno HTML do molde isolado.

No artigo Word, o teste t independente tem apenas a figura de barras transparentes com pontos, IC da média por grupo, losango e média ± DP. Quando a regressão também está selecionada, a figura de regressão permanece na seção correspondente. O texto auxiliar da figura tem linhas ajustadas para evitar corte na dimensão do Word.

O projeto integrado novo exige catalyser >= 0.1.16 em execução, e o trecho de instalação também usa esse mínimo. Uma versão antiga não deve mais produzir silenciosamente outro gráfico. As funções didáticas experimentais ficaram para revisão posterior e não foram integradas.

## Verificação

A versão foi instalada na biblioteca padrão. Um projeto novo com teste t e regressão renderizou HTML e Word com código final 0. A figura de barras foi extraída do Word e inspecionada; o arquivo contém duas figuras, uma de cada análise. A interface foi conferida pela condição do painel, que oculta o boxplot para two_ind. O teste existente test_exportacao_comunicacao.R passou. Os ICs e o resultado t foram comparados com chamadas diretas ao R no roteiro da prova.

Logs: APOIO/temp/verificar_tela_t_0.1.17.log e APOIO/temp/validar_padrao_0.1.17_final.log. A suíte completa não foi repetida nesta mudança: o resultado anterior da 0.1.16 foi 33/37 após dois retestes, com quatro falhas antigas documentadas. A migração didática da rota integrada continua pendente.

## Conferir no RStudio

Pare a CatalyseR e reinicie a sessão R. Execute APOIO/scripts/abrir_catalyser_principal.R: ele prioriza a biblioteca padrão atualizada e informa a versão disponível. Gere um Projeto R em uma pasta vazia e renderize relatorio_artigo.qmd. Projetos antigos não mudam de estrutura automaticamente, embora usem o motor atualizado após o reinício e a seleção da biblioteca correta.
