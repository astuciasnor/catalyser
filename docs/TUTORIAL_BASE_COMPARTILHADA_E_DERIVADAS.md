# Tutorial — Base Compartilhada e Bases Derivadas

**Verificado em:** CatalyseR 0.1.5, commit `6aa407a`, 27/07/2026

## A ideia em uma frase

A **Base Compartilhada** é o ingrediente básico preparado uma vez para o projeto;
as **Bases Derivadas** são preparações específicas feitas a partir dela para uma
finalidade determinada.

## Da massa compartilhada à pizza servida

Pense nos dados importados como os ingredientes que chegam à cozinha. O preparo
comum — arrumar, tipar, corrigir e registrar tratamentos na Trilha — produz uma
**massa-base** confiável, a Base Compartilhada. Quando uma análise pede algo
específico, separamos uma porção dessa mesma massa e criamos uma Base Derivada.
Nessa porção entram apenas os ingredientes e transformações necessários àquela
finalidade: um pouco de sal, óleo ou ovo; na CatalyseR, por exemplo, um filtro,
uma dicotomização ou uma padronização.

Cada porção tem sua própria receita. Alterá-la não muda a massa compartilhada
nem as outras porções. Quando a receita está conferida, a análise leva essa
preparação ao forno. A Comunicação de Resultados serve a pizza para compartilhar
com outras pessoas — acompanhada da receita, do modo de preparo e do resultado,
para que a ciência continue reprodutível.

Na CatalyseR:

- a massa-base é a **Base Compartilhada** (`dados_analise`);
- cada porção com preparo próprio é uma **Base Derivada** (`base_<finalidade>`);
- adicionar à receita inclui transformações apenas naquela porção;
- recalcular executa a receita e permite conferir a massa preparada;
- finalizar declara que a preparação está pronta para ir ao forno;
- executar a análise é assar a pizza;
- comunicar os resultados é servi-la, mostrando também como foi feita.

```text
                         ┌─ base_reg_logistica
Dados → Base Compartilhada ├─ base_pca
                         └─ base_cpue_ano_especie
```

Todos os ramos nascem diretamente da Base Compartilhada. Uma Base Derivada não
pode servir de origem para outra: a topologia permanece em estrela, com apenas
um salto. Em termos da cozinha, todas as porções são separadas da mesma
massa-base; uma pizza já temperada não vira a massa de outra receita.

## Como a Base Compartilhada é construída

O fluxo de preparo comum é:

```text
Dados importados
    ↓
Pivotar e Separar Dados
    ↓
Organizar Variáveis
    ↓
base_resolvida (mudanças estruturais confirmadas)
    ↓
Adicionar Tratamentos à Base → Tratamentos e trilha
    ↓
Base Compartilhada (dados_analise)
```

A Base Compartilhada existe desde a importação. Ela não nasce quando o usuário
abre o módulo Bases Derivadas. Abrir ou fechar menus também não define a fronteira
entre preparo comum e específico.

Ao clicar em **Adicionar Mudança à Trilha da Base Compartilhada** nos módulos
estruturais, o resultado confirmado passa a compor `base_resolvida`. Apesar do
rótulo pedagógico do botão, essas mudanças estruturais ficam em
`base_externa_rv`; a lista de tratamentos de `pipeline_rv` é outra camada,
reaplicada por último para gerar `dados_analise`.

## A pergunta que decide

Antes de adicionar um tratamento, pergunte:

> Todas as análises do projeto deveriam receber esta mudança?

- **Sim:** o tratamento pertence à Base Compartilhada.
- **Não, apenas uma finalidade precisa dele:** pertence a uma Base Derivada.

Exemplos de preparo compartilhado:

- corrigir nomes de espécies;
- tipar variáveis;
- remover duplicatas inválidas;
- organizar a planilha em estrutura tidy;
- tratar um erro de unidade que afeta todo o estudo.

No painel **Tratamentos e trilha**, reduzir linhas é permitido somente para
remover duplicatas ou durante o tratamento explícito de NA. Pivotar pode mudar
o número de linhas porque é uma arrumação estrutural, anterior a essa regra.

Exemplos de preparo derivado:

- dicotomizar `cpue` para regressão logística;
- padronizar variáveis apenas para PCA;
- filtrar um subgrupo para uma análise específica;
- resumir observações por ano e espécie para um gráfico ou série temporal;
- produzir uma tabela agregada que muda a unidade de análise.

## Criando o primeiro ramo

No módulo **Preparando Dados → Bases Derivadas**:

1. clique em **Criar base derivada**;
2. informe um nome amigável, a finalidade e o nome R;
3. abra **Receita da base** e adicione os tratamentos específicos na ordem
   lógica;
4. clique em **Recalcular a Base** na própria sub-aba ou em
   **Recalcular esta base** na gestão;
5. confira a prévia e o código R;
6. clique em **Finalizar preparo** quando a receita estiver pronta.

Exemplo:

```text
Nome amigável: Logística da CPUE
Finalidade: Regressão logística
Nome R: base_reg_logistica
Etapa: cpue_alta = 1 quando cpue >= 3; caso contrário, 0
```

Esse ramo recebe `cpue_alta`, mas a Base Compartilhada e os outros ramos não
ganham essa coluna.

## Ordem lógica e recálculo manual

A ordem mostrada é a ordem executada. Se um filtro usa `cpue_alta`, ele deve vir
depois da etapa que cria `cpue_alta`. Inverter as etapas produz um erro isolado no
ramo, sem derrubar a Base Compartilhada nem as demais bases.

Editar uma receita não recalcula silenciosamente a base. O cache fica
`Desatualizada` até o clique em **Recalcular esta base**. Isso torna explícito o
momento em que a nova receita passa a produzir uma prévia oficial.

## O que o usuário deve enxergar

Ao trabalhar com uma Base Derivada, a interface deve sempre informar:

- origem: **Base Compartilhada (`dados_analise`)**;
- nome amigável e nome R;
- finalidade;
- número e ordem das etapas;
- estado da receita;
- estado do cache;
- dimensões da última prévia válida;
- código R reproduzível.

## Do ingrediente ao relatório

O ciclo completo é:

```text
Importar → preparar a Base Compartilhada → criar Bases Derivadas
         → escolher a base → executar a análise → registrar o resultado
         → selecionar o que comunicar → gerar Word e Projeto R
```

Cada clique importante deve deixar uma trilha e produzir código R canônico. O
objetivo é que o aluno entenda tanto a receita visual quanto a linguagem que a
executa por trás da interface.
