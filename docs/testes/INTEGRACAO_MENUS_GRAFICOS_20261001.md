# Integração para entrega, 01/10/2026

Implementação na branch molde-fase2, em D:/Claude/EAPA-Ecossistema/CATALYSER_fase2.
Versão do pacote: 0.1.13. Registro de conclusão da integração local. Não houve push ou merge da main.

Mudanças autorizadas pelo autor:
- Menus e módulos associados copiados do trabalho atual da pasta catalyser.
- R² de cada categoria no gráfico da regressão; seleção de uma reta global ou
  retas por grupo preservada na apresentação e no código reproduzível.
- Barras de médias com alpha 0,22, pontos individuais, losango e hastes de IC.
- ICs das médias na confiança escolhida, bilaterais nas figuras. O IC do teste
  unilateral permanece na tabela; o rótulo média ± DP descreve dispersão.

O Projeto R com retas por grupo também deve usar o molde de dois QMDs e
apresentar tabela de coeficientes e R² de cada grupo. Tabelas e diagnósticos
globais devem ser explicitamente identificados. Esse gráfico não testa
igualdade de inclinações, e os pressupostos de cada ajuste exigem revisão.

A pasta principal não foi modificada. Snapshot e lista dos arquivos copiados:
APOIO/temp/integracao_menus_snapshot.json e integracao_menus_arquivos.json.
Arquivos antigos retirados do menu foram preservados sem source ativo.
As sobreposições no teste t foram revistas, preservando o código da Fase 2.

Provas em execução: painéis, barras/IC por referências independentes,
sete projetos com HTML/Word, comparação de camadas ANOVA e suíte oficial.

## Resultado da verificação

- Painéis: R² das categorias e escolha global conferidos; ICs de cada média do
  t independente a 99% conferidos com t.test. Log prova_paineis_integrados_final.log.
- Camadas ANOVA: barras, médias, hastes, letras e rótulos iguais entre painel
  e script exportado. figuras_barras_final.log, EXIT=0. A primeira comparação
  encontrou diferença de linewidth herdada dos temas; foi fixado zero nas
  duas barras, sem alteração numérica.
- ANOVA integrada: anova_barras_final.log, EXIT=0. O teste passou a exigir
  barras transparentes e a preservar a conferência dos pontos individuais.
- Exportações: seis projetos aprovados em provas_integracao_20261001_232230;
  a regressão por grupo estava na rota legada. A seleção do molde foi corrigida.
  ANOVA clássica, Welch e retas por grupo foram aprovadas novamente em
  provas_integracao_20261001_234427, incluindo modelos/R² por categoria e
  os dois documentos. prova_integracao_ajustes_finais.log, EXIT=0.
- Suíte completa: suite_integracao.log, 32/37, EXIT=1. A ANOVA corrigida passou
  separadamente, totalizando 33 arquivos com aprovação; não houve nova rodada
  completa após essa correção. Quatro falhas antigas: interface descritiva,
  PCA/estados, interface logística e ficha ANOVA mista. A primeira leitura da
  interface foi afetada pela normalização concomitante do arquivo; a repetição
  estável em descrevendo_menus_final.log confirmou o antigo teste de layout
  col_widths = c(7, 5). Não atribuir o erro de leitura a uma falha da interface.
- Os scripts novos não receberam dados alterados do barbo: as categorias da
  prova da exportação foram criadas somente para verificação, não são uma
  classificação biológica. Não copiar essa coluna artificial para o livro.
- Hashes da pasta principal coincidem com o snapshot anterior à integração.

## Ver a versão integrada

Pare a execução anterior e reinicie R. No RStudio:

```r
setwd("D:/Claude/EAPA-Ecossistema/CATALYSER_fase2")
shiny::runApp("inst/app", launch.browser = TRUE)
```

O pacote da revisão foi instalado somente na biblioteca experimental
APOIO/temp/Rlib_validacao para as provas do agente. A instalação global do
professor não foi atualizada automaticamente. Para testar em outro Windows,
use o novo kit 0.1.13; o kit 67eaa4f anterior não contém esta integração.

A versão continua para avaliação, com revisão científica/didática do autor
pendente. Não foi validada no macOS. Não houve push/merge ou exclusão de
worktree. Os dados EAPADados/EAPACadernos e o trabalho original foram preservados.
