# Contrato de entrada e divisão de responsabilidades no preparo de dados

> Spec de design da CatalyseR (jul/2026). Define **o que o Excel deve entregar** e
> **o que a CatalyseR faz** de forma assistida e reproduzível. Complementa
> `EVOLUCAO_TRATAMENTO_DADOS.md` (a Trilha) e `PIPELINE_DADOS_E_RELATORIOS.md`.

## Princípio central

A CatalyseR **não** deve exigir uma planilha completamente *tidy* nem já preparada
para uma análise específica. A arrumação correta depende da **unidade observacional**,
do **desenho do estudo** e da **análise pretendida** — compreensão que muitas vezes só
vem depois de bastante experiência com os próprios dados. Divisão de responsabilidades:

- **o Excel** fornece dados **estruturalmente organizados e compreensíveis**;
- **a CatalyseR** realiza, de forma assistida e reproduzível, a **tipagem, limpeza,
  transformação e preparação das bases específicas** para cada análise.

## Contrato mínimo de entrada (o que o Excel deve fornecer)

- uma tabela por aba;
- uma única linha de cabeçalho;
- nomes de variáveis não vazios e não repetidos;
- uma observação por linha; uma variável por coluna; um único valor por célula;
- uma coluna identificadora (`id_peixe`, `id_amostra`, `id_local`);
- uma unidade de medida constante em cada coluna;
- categorias escritas de forma consistente;
- datas como datas reais do Excel ou em formato inequívoco (`2026-07-13`);
- números como números (sem unidades/textos na célula);
- ausentes em branco ou por um código previamente informado.

### A evitar

- células mescladas; títulos/subtítulos acima do cabeçalho; duas tabelas na mesma aba;
- linhas de totais/médias/subtotais misturadas aos dados;
- linhas/colunas vazias como separadores;
- informação só por cor/negrito/formatação;
- várias informações na mesma célula (`12,5 ± 1,3`, `10–15`, `12 cm`, `macho/adulto`);
- mistura de números, textos e datas na mesma coluna;
- zero para representar ausente;
- fórmulas do Excel como única documentação de variáveis calculadas.

> **Vírgula decimal não é problema** quando a célula é realmente numérica. O problema é
> o número guardado como **texto**: `12,5 g`, `<0,01`, `aproximadamente 12`.

Colunas calculadas devem, de preferência, nascer **dentro da CatalyseR**, preservando as
variáveis originais e registrando a fórmula.

## Nomes das variáveis

Não é preciso obrigar o usuário a tirar espaços, acentos ou nomes em português. A
CatalyseR **preserva o nome original para exibição** e cria um **nome técnico interno**:

- nome apresentado: `Comprimento total (cm)`;
- nome interno: `comprimento_total_cm`.

Os resultados continuam acessíveis aos estudantes; o código interno usa nomes seguros.

## Formatos longo e largo

Não impor um único formato. **Longo** é melhor quando a mesma variável é medida
repetidamente (momentos, tratamentos): `id_peixe | momento | peso_g`. Já **PCA,
agrupamento, heatmap e matrizes de abundância** costumam precisar de **largo**. Solução:
aceitar planilhas **estruturalmente coerentes** e deixar as trilhas produzirem as bases
derivadas (`dados_analise`, `base_pca`, `base_agrupamento`, `base_regressao`,
`base_qui_quadrado`). A base original **nunca** é modificada diretamente.

## O que a CatalyseR faz

**Automático (operações seguras):** retirar espaços acidentais; remover linhas/colunas
totalmente vazias; criar nomes técnicos internos; detectar nomes repetidos; sugerir
tipos; mostrar categorias e ausentes encontrados.

**Só com confirmação:** converter texto em número/data; unir categorias de grafia
parecida; transformar em fator; ordenar níveis; converter unidades; pivotar; separar/unir
colunas; tratar um código como ausente; remover duplicatas.

**Nunca (silenciosamente):** excluir *outliers*; imputar ausências; transformar zero em
ausente; calcular médias de réplicas; alterar valores originais.

## Categoria de referência (para modelos com fatores)

Definida na CatalyseR **antes** das análises com fatores em modelos. Ao tipar uma
variável como fator, a interface pode mostrar: níveis encontrados; frequência de cada um;
possíveis erros de grafia; nominal × ordinal; ordem dos níveis; e a categoria de
referência.

- Ex.: `tratamento` com `controle`, `racao_A`, `racao_B` → escolher `controle` como referência.
- Afeta a **interpretação** (intercepto, coeficientes, comparações; ANOVA/ANCOVA,
  regressão linear/logística, MLG). Nem sempre muda o teste global, mas muda a leitura —
  por isso **não** deve ser escolhida silenciosamente pela ordem alfabética.
- A CatalyseR pode **sugerir** (grupo controle, momento inicial, condição padrão,
  categoria mais frequente), mas o usuário **confirma**.

Distinguir: **ordem dos níveis** (para ordinais e apresentação) × **categoria de
referência** (base de comparação nos modelos). A referência pode ser padrão na
`dados_analise`, mas cada trilha de análise pode alterá-la **sem** mexer na base
original. Só aparece quando estatisticamente relevante. O dicionário no Excel pode ter uma
coluna opcional `referencia_sugerida`, mas a decisão final é na CatalyseR.

## Fluxo recomendado

```text
arquivo original → diagnóstico de importação → dados_brutos → dados_analise → base específica da análise
```

Toda alteração fica numa **receita reproduzível**. Assim a CatalyseR não só arruma: ela
**ensina** o que foi feito, por que foi necessário e como se relaciona com a análise
escolhida. A planilha precisa chegar organizada o suficiente para se entender o
significado de linhas, colunas e células; o preparo estatístico mais avançado é papel das
trilhas.
