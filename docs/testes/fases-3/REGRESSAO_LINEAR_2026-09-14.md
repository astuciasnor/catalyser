# Regressão linear simples — conferência de 14/09/2026

O roteiro do autor foi aplicado à regressão com uma reta global, preservando
a ANOVA já aprovada e o vínculo entre script e relatório. As funções de cálculo
são `lm`, `broom` e `performance`; a equação do gráfico é extraída do modelo
que fornece os coeficientes, sem depender de `ggpubr` para a anotação.

## Alterações

- Tela: IC de 95%, EP, t e p na tabela; N/AIC nas métricas; testes dos resíduos;
  R² junto à equação. A opção de autocorrelação integra a assinatura analítica
  e exige nova execução quando alterada.
- Código de consulta: `tidy`, `glance`, `augment`, Shapiro e Breusch-Pagan,
  com autocorrelação somente quando solicitada.
- Projeto integrado: roteiro comentado em `templates/regressao_linear/analise.R`,
  aplicado às variáveis e à base registradas. Coeficientes, métricas, narrativa
  e figura no artigo; testes detalhados e gráficos de diagnóstico no HTML.
- Leitura científica: p acima de alfa não comprova pressuposto. Independência
  é discutida pelo delineamento; Durbin-Watson exige ordem real das linhas.
  Cook é uma conferência complementar, sem exclusão automática de observações.
- `broom` e `performance` declarados nas dependências e no instalador exportado.

## Verificações executadas

`test_regressao_roteiro.R` compara coeficientes e IC diretamente com `lm` e
`confint`; reproduz Shapiro e as métricas de `cars`; confere declive negativo,
NA, cabeçalhos com espaços/unidades, colunas constantes, infinitos, poucos pares,
amostra acima do limite do Shapiro e autocorrelação opcional. Gera um projeto
real e executa os chunks sincronizados, incluindo tabelas e gráfico. Duas
regressões recebem rótulos de chunks distintos.

`test_regressao_interface.R` verifica tabela com IC, p-valores, registro da opção
de autocorrelação, invalidação após mudança e sintaxe do código de consulta.
`test_exportacao_comunicacao.R` concluiu as asserções do exportador geral.

Os marcadores de sucesso foram observados, mas os processos R 4.6.1 encerraram
com falha nativa `-1073741819`; não se declara saída zero dos testes. O mesmo
problema ocorreu numa verificação curta com R 4.6.0.

Com `LC_ALL=English_United States.utf8`, `quarto render relatorio.qmd --to docx`
executou os 33 passos, gerou as figuras e chegou a `relatorio.knit.md`.
O Quarto encerrou com código 1 por falha nativa do R; não foi produzido um Word
final. A figura da reta foi inspecionada visualmente. HTML/Word completos e
paginação continuam pendentes de Render no RStudio.

## Correção do exemplo fornecido

Para `cars`, β = 3,93240876 e R² = 0,65107938. Shapiro-Wilk produziu
p = 0,02152458, e Breusch-Pagan via `performance` produziu p = 0,03104933.
Ambos indicam evidência de violação a 5%, incorporada ao texto automaticamente.

Referências dos métodos: [Breusch-Pagan no performance](https://easystats.github.io/performance/reference/check_heteroscedasticity.html),
[Durbin-Watson no performance](https://easystats.github.io/performance/reference/check_autocorrelation.html)
e [coeficientes e IC no broom](https://broom.tidymodels.org/reference/tidy.lm.html).
