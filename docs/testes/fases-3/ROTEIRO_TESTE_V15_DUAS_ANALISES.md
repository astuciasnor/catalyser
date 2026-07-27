# Roteiro V15 — duas bases derivadas e duas análises

## Objetivo

Validar, sem repetir todo o percurso de homologação, quatro refinamentos:

1. nomes longos ficam legíveis no seletor **Base utilizada**;
2. **Executar análise** funciona no primeiro clique após escolher a base;
3. duas análises registradas permanecem independentes;
4. o Projeto R expõe o código essencial no QMD, mas esse código não aparece
   impresso no Word.

## Dados

Use `inst/app/dados/Treino-Transformacoes.xlsx`, aba `biometria`.

Na Base Compartilhada:

- padronize o texto de `especie`, `local` e `sexo`;
- remova linhas duplicadas;
- não remova globalmente os dados faltantes, pois cada ramo tratará somente as
  colunas necessárias à sua análise.

## Base derivada 1 — teste t

- Nome amigável: `CPUE de corvina por sexo`
- Objeto R: `base_t_cpue_corvina_sexo`
- Receita:
  1. filtrar `especie` igual a `corvina`;
  2. remover linhas com NA em `cpue`;
  3. remover linhas com NA em `sexo`.
- Recalcule e finalize o preparo.

Em **Teste t de Student**:

- escolha `CPUE de corvina por sexo — base_t_cpue_corvina_sexo`;
- selecione teste de duas amostras independentes;
- resposta: `cpue`;
- grupo: `sexo`;
- clique uma única vez em **Executar análise**;
- confirme a atualização da saída e clique em **Adicionar aos resultados**.

## Base derivada 2 — regressão linear

- Nome amigável: `Peso e comprimento de corvina`
- Objeto R: `base_reg_peso_comprimento_corvina`
- Receita:
  1. filtrar `especie` igual a `corvina`;
  2. remover linhas com NA em `comprimento_cm`;
  3. remover linhas com NA em `peso_g`.
- Recalcule e finalize o preparo.

Em **Regressão linear simples**:

- escolha
  `Peso e comprimento de corvina — base_reg_peso_comprimento_corvina`;
- resposta: `peso_g`;
- preditor: `comprimento_cm`;
- clique uma única vez em **Executar análise**;
- confirme a atualização da saída e clique em **Adicionar aos resultados**.

## Comunicação e Projeto R

Em **Comunicação de Resultados**:

1. confirme as duas execuções registradas;
2. mantenha ambas incluídas no Word;
3. baixe o Projeto R;
4. abra o `.Rproj` no Windows 11;
5. execute os scripts numerados;
6. abra `relatorio.qmd`.

Antes de cada bloco oculto de renderização, o QMD deve mostrar no arquivo-fonte
um chunk comentado como `Código R essencial desta execução`. Esses chunks usam
`eval: false` e `include: false`: podem ser estudados e executados manualmente no
RStudio, mas não são impressos no relatório Word.

## Critérios de aprovação

- Os dois nomes completos aparecem no menu de Base utilizada.
- Cada análise é executada no primeiro clique.
- As duas execuções aparecem separadamente na Comunicação.
- O Projeto R preserva as duas bases e os dois scripts.
- O QMD contém `stats::t.test(...)` e `stats::lm(...)`.
- O Word renderiza sem mostrar os chunks pedagógicos.
