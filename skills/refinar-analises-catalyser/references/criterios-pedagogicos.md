# Critérios pedagógicos para saídas e código

## Objetivo

Fazer a transição “mouse → código” ser reconhecível por um estudante iniciante.
O código exportado deve parecer uma análise humana, não um arquivo interno de
serialização.

## Script R numerado

Usar esta ordem:

1. cabeçalho com pergunta, base e análise;
2. carregamento da base;
3. escolha explícita das variáveis;
4. preparação mínima específica;
5. função estatística ou gráfico canônico;
6. objetos intermediários com nomes legíveis;
7. impressão dos resultados;
8. seção final “Integração com o relatório”.

Preferir nomes como:

```r
variavel_resposta <- "profundidade_m"
variavel_fator <- "especie"
formula_anova <- reformulate(variavel_fator, response = variavel_resposta)
modelo_anova <- aov(formula_anova, data = dados)
resumo_anova <- summary(modelo_anova)
```

Evitar como superfície principal:

```r
execucao <- structure(list(... centenas de caracteres ...))
resultado <- catalyser_executar(execucao, dados)
```

O registro técnico pode ser lido de `metadados/registro_execucoes.rds` no fim do
script, sem ocultar o método científico usado.

## QMD

- Usar labels descritivos: `anova-modelo`, `anova-tukey`,
  `linhas-comprimento`.
- Separar preparação, cálculo e apresentação.
- Fazer o código de estudo funcionar quando o aluno o copiar.
- Ocultar no Word apenas o mecanismo editorial, não a explicação científica.
- Manter o Word limpo por padrão; código pode permanecer visível no fonte QMD.
- Explicar em comentário por que uma função é usada, não cada detalhe da sintaxe.

## Narrativa

- Informar amostra analisada e exclusões.
- Nomear variáveis e grupos.
- Apresentar estimativa, incerteza, estatística e p-valor quando aplicável.
- Usar “há evidência” ou “não houve evidência suficiente”; não escrever “H0
  aceita”.
- Separar resultado estatístico de interpretação biológica.

## Tabelas e gráficos

- Rótulos em português e unidades quando disponíveis.
- Casas decimais coerentes com o significado.
- Tabelas de consulta simples podem usar estilo sage; tabelas referenciadas no
  relatório usam Ocean.
- Gráfico precisa de título informativo, eixos legíveis e legenda apenas quando
  acrescentar informação.
- Não conectar por linha categorias nominais sem ordem científica.

## Critério de aceite

Um estudante deve conseguir:

1. apontar onde a base é carregada;
2. identificar as variáveis;
3. localizar a função R principal;
4. executar o script fora da CatalyseR;
5. relacionar objetos do script às tabelas e figuras do relatório.
