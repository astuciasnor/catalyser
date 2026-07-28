# Piloto atual: ANOVA e gráfico de linhas

## Estado do projeto

O pipeline foi homologado em mais de um Windows: instalação pela `main`,
preparação, bases derivadas, análises, Comunicação de Resultados, Projeto R,
execução no RStudio e Word via Quarto. Tratar esse percurso como estável.

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

## Melhoria inicial prioritária

1. Remover o `dput()` extenso da parte principal dos scripts numerados.
2. Ler a execução de `metadados/registro_execucoes.rds`.
3. Inserir uma seção de código essencial humano antes da integração editorial.
4. Fazer o QMD espelhar esse mesmo código, sem manter dois geradores divergentes.
5. Testar que os scripts continuam criando o objeto
   `resultado_<id>` esperado pelo relatório.
6. Renderizar Word com ANOVA e duas versões do gráfico.

## Não fazer neste ciclo

- ANOVA de dois fatores;
- novos tipos de gráficos;
- troca do motor de registro;
- alteração da topologia base/ramos;
- redesign geral da Comunicação;
- refatoração ampla de todas as análises.
