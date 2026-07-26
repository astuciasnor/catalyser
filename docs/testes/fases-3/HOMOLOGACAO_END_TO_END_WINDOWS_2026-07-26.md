# Homologação ponta a ponta no Windows — 26/07/2026

## Escopo

Foi homologado o percurso integrado da CatalyseR desde o preparo dos dados até
a reprodução fora do aplicativo:

1. composição da Base Compartilhada;
2. criação e finalização de Bases Derivadas;
3. seleção explícita da base em módulos analíticos;
4. execução e registro independente das análises;
5. organização editorial em Comunicação de Resultados;
6. exportação do Projeto R;
7. abertura do projeto no RStudio em Windows 11;
8. execução dos scripts numerados;
9. renderização do `relatorio.qmd`;
10. geração do relatório Word.

## Ambientes

- Homologação da interface: VM Windows 10.
- Reprodução do Projeto R: Windows 11, RStudio e Quarto.
- Artefato da interface: V14, com sub-abas de execução/registro e de
  Comunicação de Resultados.

## Resultado

O percurso completo foi aprovado. As Bases Derivadas, os parâmetros congelados
das análises, a seleção editorial, os scripts R e o QMD permaneceram
reproduzíveis fora da CatalyseR.

O motor integrado passa, portanto, a ser considerado um **checkpoint
funcional**. Os ciclos seguintes devem priorizar refinamentos de usabilidade,
clareza pedagógica do QMD, qualidade das saídas por análise e integração
vertical de novos módulos, começando pela ANOVA.

## Pontos de refinamento observados

- O menu suspenso de Base utilizada precisa exibir melhor nomes longos.
- Alguns módulos podem exigir um segundo clique em Executar análise porque a
  assinatura reativa ainda está sendo sincronizada no primeiro clique.
- O QMD executa corretamente os scripts, mas deve tornar o código canônico de
  cada análise mais explícito para estudo no RStudio, sem poluir o Word.

## Evidência automatizada

Antes do checkpoint, `inst/app/tests/run_tests.R` aprovou:

- sintaxe dos 64 arquivos R;
- contrato e replay das Bases Derivadas;
- reorganização do menu Preparando Dados;
- registro explícito e independente de execuções;
- estados de execução dos módulos prioritários;
- separação entre regressão logística binária e curva logística;
- Comunicação de Resultados;
- replay analítico no Projeto R;
- exportação seletiva do Word e preservação integral do Projeto R.
