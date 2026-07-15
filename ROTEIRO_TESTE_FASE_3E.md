# Roteiro manual — Fase 3E (exportação integrada)

## Objetivo

Confirmar que o **Relatório Word** obedece à seleção editorial e que o
**Projeto R** preserva todas as execuções registradas, suas bases e seus
scripts, inclusive quando uma execução não entra no Word.

## Conjunto de dados recomendado

Use o arquivo:

```text
inst/app/dados/Treino-Transformacoes.xlsx
```

Importe a aba **biometria**. Ela contém variáveis numéricas, categóricas,
valores ausentes e texto inconsistente, permitindo testar preparo, ramo e
exportação no mesmo projeto.

## Pré-requisitos

- testar em uma instalação ou pasta separada da versão usada pelos alunos;
- ter o Quarto instalado para testar o download direto do Word;
- não misturar esta branch com a versão estável;
- iniciar uma sessão nova da CatalyseR.

## Etapa 1 — preparar a base compartilhada

1. Importe `Treino-Transformacoes.xlsx`, aba `biometria`.
2. Na Trilha, trate os valores ausentes de `peso_g` pela mediana.
3. Padronize o texto de `especie` para iniciais maiúsculas.
4. Confirme que `dados_analise` continua disponível e que a Trilha mostra as
   duas etapas na ordem correta.

## Etapa 2 — criar um ramo de um único salto

1. Crie uma base derivada com o nome amigável **Regressão peso-comprimento**.
2. Use como nome R `base_regressao_peso`.
3. Adicione um filtro que mantenha `comprimento_cm >= 15`.
4. Clique em **Recalcular esta base** e depois em **Finalizar preparo**.
5. Confirme que o ramo nasce de `dados_analise`; não deve existir opção para
   criar uma base derivada a partir desse ramo.

## Etapa 3 — registrar quatro execuções

Execute e clique em **Adicionar aos resultados** após cada caso:

1. **Estatística Descritiva** em `dados_analise`:
   `comprimento_cm`, `peso_g` e `cpue`.
2. **Regressão Linear** em `base_regressao_peso`:
   resposta `peso_g`, preditor `comprimento_cm`.
3. **Gráfico de Linhas 1** em `dados_analise`:
   X = `id`, Y = `comprimento_cm`; título **Comprimento por indivíduo**.
4. **Gráfico de Linhas 2** em `dados_analise`:
   X = `id`, Y = `peso_g`; título **Peso por indivíduo**.

Resultado esperado: a Comunicação deve listar quatro IDs independentes. Os
dois gráficos usam a mesma base e o mesmo X, mas não podem se sobrescrever.

## Etapa 4 — montar o Word

No menu **Comunicação de Resultados**:

1. coloque **Comprimento por indivíduo** antes da regressão;
2. mantenha a Estatística Descritiva no Word apenas com **Tabela**;
3. mantenha **Comprimento por indivíduo** apenas com **Gráfico**;
4. retire **Peso por indivíduo** do Word;
5. retire a Regressão Linear do Word;
6. preencha uma frase curta em Introdução, Métodos, Discussão e Conclusão.

O contador esperado é:

```text
2 no Word / 4 no Projeto R
```

## Etapa 5 — baixar e conferir o Word

1. Clique em **Baixar Relatório Word (.docx)**.
2. Abra o documento.
3. Confirme a presença da tabela descritiva e do gráfico
   **Comprimento por indivíduo**.
4. Confirme que a regressão e o gráfico **Peso por indivíduo** não aparecem.
5. Confirme que as seções globais foram incluídas.

Se o Quarto não estiver disponível, o botão Word deve permanecer desabilitado
com um aviso; o Projeto R deve continuar disponível.

## Etapa 6 — baixar e conferir o Projeto R

1. Clique em **Baixar Projeto R (.zip)** e extraia o arquivo.
2. Confirme a existência de:

```text
relatorio.qmd
projeto_analise.Rproj
custom-reference.docx
README.md
dados/
R/
metadados/
resultados/
```

3. Na pasta `R/`, confirme:
   - `00_importar.R`;
   - `01_operacoes_estruturais.R`;
   - `02_preparo_compartilhado.R`;
   - um script `03_*` para `base_regressao_peso`;
   - **quatro scripts `04_*`**, um para cada execução registrada.
4. Abra `metadados/MANIFESTO.md`: ele deve indicar duas execuções no Word e
   quatro preservadas no Projeto R.
5. Abra `relatorio.qmd`: a regressão e o segundo gráfico não devem possuir seção
   de resultado, embora seus scripts `04_*` existam.
6. Abra o `.Rproj` no RStudio e execute os scripts na ordem numérica.
7. Renderize `relatorio.qmd` e confirme que o Word reproduz a seleção feita na
   CatalyseR.

## Etapa 7 — testar a trava de dependência

1. Volte à CatalyseR e altere a Trilha ou reabra `base_regressao_peso`.
2. Retorne à Comunicação.
3. A execução dependente deve aparecer como **Precisa atualizar** e os downloads
   integrados devem ser bloqueados.
4. Recalcule/finalize a base, execute novamente a análise e use
   **Atualizar resultado**.
5. Confirme que os downloads voltam a ficar disponíveis.

## Critério de aprovação

A Fase 3E está aprovada quando:

- o Word contém somente o que foi escolhido;
- o Projeto R contém todas as execuções registradas;
- cada execução possui base e script próprios;
- os dois gráficos de mesmo X e Y diferentes permanecem independentes;
- a ordem editorial é respeitada;
- uma dependência desatualizada bloqueia a exportação;
- os scripts e o `relatorio.qmd` executam fora da CatalyseR.
