# CatalyseR 0.1.16 — relatório integrado com duas análises

Data: 02/10/2026.

## Correção

A seleção de várias análises seguia uma rota anterior do exportador. Ela criava apenas relatorio.qmd e o motor do teste t independente reconstruía um boxplot. As descrições metodológicas da versão 0.1.15 estavam corretas, mas não resolviam essas duas diferenças.

Agora essa rota entrega relatorios/relatorio_completo.qmd (HTML), relatorios/relatorio_artigo.qmd (Word) e _quarto.yml com output-dir: saida. Renderizar o projeto gera saida/relatorios/relatorio_completo.html e saida/relatorios/relatorio_artigo.docx. Renderizar apenas um QMD gera seu respectivo documento.

O gráfico reconstruído do teste t independente apresenta barras de largura 0,30 e transparência 0,22, pontos com deslocamento somente horizontal, IC bilateral da média de cada grupo, losango da média e texto média ± DP em negrito, sem fundo. A expansão inferior é zero; observações e intervalos negativos continuam visíveis. O IC da diferença entre médias continua na tabela do teste. Os cálculos inferenciais e a escolha Student/Welch foram preservados.

## Verificação

Foi exportado um projeto novo com as configurações do teste t e da regressão por tratamento do projeto EAPACadernos/dados. O projeto inteiro renderizou com código final 0, gerando os dois documentos na pasta configurada. O HTML não apresentou referências irresolvidas; o Word foi conferido como ZIP íntegro, contendo metodologias das duas análises e duas figuras.

As hastes foram comparadas com t.test por grupo. Também foram verificadas a hipótese com IC de 99%, observações negativas e rótulos distintos para as médias dos dois grupos. O teste inferencial coincidiu com t.test executado diretamente. O gráfico foi inspecionado visualmente.

Prova: APOIO/temp/prova_duas_0.1.16_113047/teste_t_e_regressao. Logs: APOIO/temp/prova_final_duas_0.1.16.log e APOIO/temp/prova_duas_0.1.16_113047/render.log.

## Limites e uso

A exportação integrada ainda mantém funções catalyser_executar/catalyser_mostrar e sincronização dos chunks com o script. A entrega dos dois QMDs não equivale à migração didática completa para o molde de cálculos nativos. Essa migração e as funções pequenas de apresentação continuam como próximo trabalho.

Projetos já baixados não são alterados pela atualização da IDE. Reinicie R e CatalyseR na versão nova e exporte novamente em uma pasta vazia, evitando misturar documentos de exportações anteriores. O projeto EAPACadernos/dados foi preservado nesta correção.

## Conferência geral

A execução completa registrou 31/37 testes aprovados, sem verificações puladas. Dois testes foram repetidos após a correção do caminho antigo do QMD e o término da reinstalação local: test_preparo_comunicacao_completo.R e test_preparo_csv_datas.R passaram, código final 0. O resultado consolidado é 33/37.

Permanecem as mesmas quatro falhas registradas na versão 0.1.14: test_descrevendo_interface.R (expectativa antiga de layout), test_estados_execucao.R (fixture de PCA), test_logisticas_separadas.R (expectativa de interface) e test_anova_mista.R (fixture). A suíte inteira não está aprovada. Os fluxos de exportação afetados, o teste t, Welch e a regressão passaram.

Logs: APOIO/temp/suite_0.1.16.log, APOIO/temp/reteste_preparo_0.1.16.log e logs individuais de preparo. A renderização final do projeto de duas análises terminou com EXIT=0 em APOIO/temp/prova_final_duas_0.1.16.log.
