# Conferência do preparo de dados — 10/09/2026

Ponto de partida: commit `229abec`, que preserva a exportação ANOVA validada antes dos refinamentos.

## Alterações

- Menu Preparar Dados com Importar Dados, Reestruturar Planilha, Preparar Base Compartilhada e Preparar Bases Derivadas.
- Edição de variáveis, cálculo, reescala e limpeza na mesma sequência de etapas; prévia pendente identificada e download da base efetivamente usada nas análises.
- Bases derivadas com seletor sincronizado ao registro, ações reunidas, tabelas sem corte e indicação de preparo desatualizado.
- Grupos de ações habilitados após criar ou reabrir uma derivada; filtros preservam coluna, operador e valor quando o formulário atualiza.
- Nome curto editável e consistente na pasta, Rproj, ZIP e README.
- Tipagem de fatores numéricos e recodificação com aspas ou trocas simultâneas coerentes com a prévia.
- Paleta da ANOVA ampliada quando a quantidade de grupos supera as oito cores originais.
- Importação aguarda a escolha da aba Excel sem exibir erro vazio.

## Evidências

Na planilha Treino-Transformacoes.xlsx (aba biometria), a sequência cálculo de peso_dobro, reescala para peso_kg, renomeação para massa_kg e remoção de ausentes produziu 65 linhas e 12 colunas. O navegador percorreu os quatro grupos de operações relevantes e baixou a planilha preparada; os valores conferem com a base de análise. O filtro peso_g > 100 em uma derivada produziu 46 linhas. Recalcular e Finalizar Preparo concluíram sem erros na interface. As telas foram inspecionadas em 1440 e 1366 pixels de largura, sem transbordamento horizontal da página.

Os testes de organização, bases derivadas, menu e reestruturação concluíram suas asserções. Os testes novos cobrem a sequência compartilhada, download, código reprodutível, fatores numéricos, recodificação e seleção da base derivada. A exportação foi verificada para a compartilhada e para a derivada: o script e o QMD reproduzem os mesmos dados e coeficientes de ANOVA. O Excel de origem permanece idêntico, com todas as abas.

## Pendência do ambiente

Com a configuração de localidade UTF-8 nesta sessão de teste, os 27 passos do QMD, incluindo tabelas e gráficos com 13 grupos, executaram até gerar relatorio.knit.md. O processo R encerrou com erro nativo -1073741819, também observado após testes que já haviam concluído suas asserções. Por isso, a geração integral e a apresentação final de HTML e DOCX desta versão ainda devem ser conferidas no RStudio do usuário; não estão declaradas aprovadas por esta execução. A instalação do R não foi alterada.

## Como conferir a versão incorporada

Reiniciar a CatalyseR pelo run.R. Importar Treino-Transformacoes.xlsx e escolher biometria. Percorrer a preparação compartilhada e derivada, recalcular e finalizar. Executar e registrar uma ANOVA, escolher o nome curto em Comunicação e exportar um projeto novo. Abrir o Rproj exportado no RStudio e renderizar relatorios/relatorio.qmd; conferir HTML e o Word em Outros formatos.
