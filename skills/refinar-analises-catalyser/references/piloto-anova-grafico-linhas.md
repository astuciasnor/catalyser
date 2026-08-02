# Piloto atual: ANOVA e gráfico de linhas

## Estado inspecionado no último ciclo

- **Branch:** `main`
- **Commit:** `6aa407a` (`feat: humaniza projetos R dos pilotos na v0.1.5`)
- **Versão do `DESCRIPTION`:** 0.1.5
- **Inspecionado em:** 27/07/2026, direto nos módulos e testes — não a partir de
  planos históricos.

Ao abrir o próximo ciclo, refazer este registro. Se ele estiver desatualizado,
o resto deste arquivo também está sob suspeita.

## Estado do projeto

O pipeline foi homologado em mais de um Windows: instalação pela `main`,
preparação, bases derivadas, análises, Comunicação de Resultados, Projeto R,
execução no RStudio e Word via Quarto. Tratar esse percurso como estável.

A primeira humanização do Projeto R foi entregue na versão **0.1.5**. Os scripts
numerados e o QMD usam o gerador compartilhado
`exportacao_codigo_estudo()`, leem a configuração técnica de
`metadados/registro_execucoes.rds` e exibem o método científico em código R
legível. Não refazer essa fundação; refiná-la com mudanças pequenas e testadas.

## Análise 1 — ANOVA de um fator

### Código canônico mínimo

```r
formula_anova <- stats::reformulate(
  variavel_fator,
  response = variavel_resposta
)
modelo_anova <- stats::aov(formula_anova, data = dados_anova)
resumo_anova <- summary(modelo_anova)
comparacoes_tukey <- stats::TukeyHSD(modelo_anova)
```

Complementar com:

- resumo por grupo;
- η² e ω²;
- Levene e Shapiro-Wilk dos resíduos;
- tabela ANOVA;
- Tukey;
- gráfico sem linha entre fatores nominais;
- narrativa sem “aceitar H0”.

### Arquivos centrais

- `inst/app/modules/mod_anova.R`
- `inst/app/templates/funcoes_anova.R`
- `inst/app/templates/funcoes_projeto_integrado.R`
- `inst/app/modules/exportacao_comunicacao.R`
- `inst/app/tests/test_anova_integrada.R`
- `inst/app/tests/test_anova_exportacao.R`

## Análise 2 — gráfico de linhas com troca de Y

### Código canônico mínimo

```r
variavel_x <- "id"
variavel_y <- "comprimento_cm"

grafico_linhas <- ggplot2::ggplot(
  dados,
  ggplot2::aes(
    x = .data[[variavel_x]],
    y = .data[[variavel_y]],
    group = 1
  )
) +
  ggplot2::geom_line() +
  ggplot2::geom_point()
```

Preservar duas execuções independentes quando Y muda, por exemplo:

- comprimento por observação;
- peso por observação.

Não sobrescrever o primeiro resultado ao escolher “Adicionar Novo Resultado”.

### Arquivos centrais

- `inst/app/modules/mod_viz_extra.R`
- `inst/app/modules/mod_registrar_execucao.R`
- `inst/app/templates/funcoes_projeto_integrado.R`
- `inst/app/modules/exportacao_comunicacao.R`
- `inst/app/tests/test_grafico_linhas_troca_y.R`
- `inst/app/tests/test_anova_exportacao.R`

## Linha de base já entregue

- `dput()` extenso removido da superfície pedagógica principal;
- metadados técnicos lidos de `metadados/registro_execucoes.rds`;
- código humano compartilhado entre script numerado e QMD;
- ANOVA explícita com fórmula, modelo, Tukey, diagnósticos e tamanhos de efeito;
- gráfico de linhas explícito com mapeamento, camadas, rótulos e tema;
- duas execuções independentes preservadas quando o eixo Y muda;
- testes dos scripts, replay, QMD e renderização Word aprovados.

## Segundo ciclo de refinamento — entregue

Ainda na 0.1.5, sobre o commit `6aa407a`:

- **Narrativa sem duplicação.** `relatar_anova()` deixou de repetir as médias de
  cada grupo e os p de Shapiro-Wilk e Levene, que já estão nas tabelas ao lado.
  Ficou com pergunta, amostra e exclusões, decisão sobre H0 com F e p, tamanho de
  efeito com leitura, síntese dos pares de Tukey e remissão explícita às tabelas.
- **A narrativa do replay acompanha a da interface.** `catalyser_anova()` foi
  alinhada à mesma regra, senão o Word contaria uma história diferente da tela.
  As duas são textos irmãos: mudar uma obriga a mudar a outra, e um teste compara
  os marcadores das duas.
- **Números em português nas tabelas.** `anova_num_col()` e `anova_p_col()`
  aplicam vírgula decimal e `< 0,001` em todas as saídas da ANOVA, com casas
  decimais escolhidas pelo significado da coluna. Antes, a narrativa usava vírgula
  e as tabelas, ponto.
- **Tukey ordenado por p ajustado.** Com cinco grupos são dez linhas; quem lê
  procura primeiro os pares com evidência.
- **Leitura convencional do tamanho de efeito.** Coluna com a referência de Cohen
  e nota de que é convenção estatística, não interpretação biológica.
- **Labels semânticos no QMD.** `anova-profundidade-m-modelo`,
  `anova-profundidade-m-tukey`, `linhas-comprimento-cm-grafico`. A raiz vem do
  tipo mais a variável que distingue a execução, com desempate por ID quando duas
  coincidem — o Quarto falha com labels repetidos. Comentários separam método de
  mecanismo editorial.
- **Exclusões visíveis no gráfico de linhas.** O `ggplot2` descartava linhas
  incompletas com um aviso discreto. Agora o módulo, o replay e o código
  exportado usam `dados_grafico` com `complete.cases()`, e a interface informa
  quantas observações entraram e quantas saíram — o mesmo padrão do `dados_anova`.
- **Mini-refatoração:** `catalyser_completos()` centraliza a exclusão contada e
  serve à ANOVA e ao gráfico.

### Novo caminho dos dados no Projeto R

Mudança estrutural do exportador feita no mesmo ciclo:

- a **planilha bruta** vai no `.xlsx` dentro de `dados/` e é o ponto de entrada;
- `R/01_base_compartilhada.R` é o **único** arquivo que produz `dados_analise`,
  com conferência contra a fotografia por `catalyser_conferir_base()`;
- os antigos `00_importar.R`, `01_operacoes_estruturais.R` e
  `02_preparo_compartilhado.R` foram fundidos nele;
- os scripts `03_*` das bases derivadas **deixaram de existir**: cada análise
  constrói a sua base no chunk `<raiz>-base` do QMD (`echo: false`), e os
  scripts `04_*` reconstroem o que precisam com o mesmo gerador `bases_codigo()`;
- `dados/` ficou com três arquivos no caso comum — planilha bruta (entrada),
  `dados_analise.rds` (conferência) e `base_compartilhada.xlsx` (entrega). Saíram
  `dados_brutos.rds`, `dados_analise.csv` e as fotografias das derivadas;
- o QMD não usa mais `sys.source()` em ambiente oculto: chama
  `catalyser_executar()` direto sobre a base construída no chunk anterior.

Ao mexer no exportador, conferir `exportacao_codigo_base_compartilhada()`,
`exportacao_bloco_base_analise()` e `exportacao_leiame_projeto()`.

## Terceiro ciclo — saída da ANOVA (aprovada)

Conferido no Word pelo autor em 28/07/2026. **Não desfazer sem pedido explícito.**

- boxplot substituído por **barras + IC 95% + letras de diferença**, eixo Y em zero;
- tabela virou `Grupo | n | Média ± DP | IC da média | Diferença`;
- letras por `anova_letras_tukey()` / `catalyser_letras_tukey()`, algoritmo
  "inserir e absorver" escrito no projeto, sem `multcompView`;
- `console` saiu de `saidas_disponiveis` (segue na interface, só para estudo).

## Próximo refinamento da dupla

1. **Clareza dos scripts exportados para humanos** — pedido explícito do autor em
   28/07/2026, e a prioridade do próximo ciclo. Reler `R/01_base_compartilhada.R`
   e os `R/04_*` como um aluno leria: nomes de objeto, ordem dos passos, o que
   cada comentário promete e entrega.
2. Continuar refinando a apresentação dos resultados (o gráfico e a tabela da
   ANOVA já foram aprovados; o gráfico de linhas ainda não passou por essa lupa).
3. Observar, com o Word na mão, se o resumo por grupo e a tabela da ANOVA ainda
   dizem coisas distintas o bastante para justificar duas tabelas.
4. Decidir se `grafico_anova()` e o gráfico do replay devem compartilhar um
   gerador único, hoje o maior trecho duplicado da dupla — e o mesmo vale para as
   duas cópias do algoritmo das letras.
4. Manter automáticas as escalas dos eixos. Limites ou `coord_cartesian()` só
   entram depois de uma necessidade observada e com aviso sobre pontos fora da
   faixa visível.
5. Reexecutar ANOVA e as duas versões do gráfico dentro da CatalyseR, nos scripts
   externos e no Word, comparando os resultados essenciais.

## Não fazer neste ciclo

- ANOVA de dois fatores;
- novos tipos de gráficos;
- troca do motor de registro;
- alteração da topologia base/ramos;
- redesign geral da Comunicação;
- refatoração ampla de todas as análises.
