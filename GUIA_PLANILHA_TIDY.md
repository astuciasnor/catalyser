# Prepare seu Excel para a CatalyseR — o "contrato mínimo"

> Você **não precisa** entregar uma planilha 100% pronta para a análise. A CatalyseR faz
> a tipagem, a limpeza e o preparo específico de cada análise — de forma assistida e
> reproduzível. Sua parte é entregar dados **estruturalmente organizados e compreensíveis**:
> que dê para entender o significado de cada linha, coluna e célula. Uma coluna a mais na
> coleta economiza horas de faxina — e evita erros silenciosos — depois.

## A ideia central (*tidy* no essencial)

- **cada coluna = uma variável**
- **cada linha = uma observação**
- **cada célula = um único valor**

## O contrato mínimo (o que sua planilha deve ter)

- **uma tabela por aba**, começando em A1, com **uma única linha de cabeçalho**;
- **nomes de coluna não vazios e não repetidos**;
- **uma coluna identificadora** (`id_peixe`, `id_amostra`, `id_local`);
- **uma unidade constante por coluna** (toda a coluna em cm, ou toda em g);
- um único valor por célula; números como números; datas como datas;
- categorias com **grafia consistente**;
- ausentes **em branco** (ou um código que você informe).

## Uma coisa por célula

| Evite (❌) | Faça (✅) |
|---|---|
| `12/M` (tamanho e sexo juntos) | `comprimento_cm` = `12` · `sexo` = `M` |
| `12,5 ± 1,3`, `10–15` | valor numa coluna; desvio/faixa em outra |
| `5 kg`, `~5`, `< 0,1`, `5 (estimado)` | só o número; a observação vai numa coluna `obs` |

Uma única célula com texto numa coluna de números **força a coluna inteira a virar texto**.
Obs.: **vírgula decimal não é problema** se a célula for numérica de verdade — o problema é o número guardado como **texto** (`12,5 g`).

## Faltantes

- **Deixe em branco** (a CatalyseR lê como ausente).
- Evite `-`, `n/d`, `"sem dado"`. **Nunca use `0`** para faltante — zero é um valor.

## Categorias (níveis de fator)

- **Grafia única**: escolha `macho`/`femea`, sem misturar `M`, `Macho`, `masculino`.
- **Sem espaços invisíveis** no fim (`"controle "` ≠ `"controle"`).
- Não misture **código e rótulo** na mesma coluna.

*(A ordem dos níveis e a categoria de referência você define depois, dentro da CatalyseR — dependem da análise.)*

## Datas

- Melhor: **data de verdade do Excel**. Se for texto, use dia-mês-ano ou ano-mês-dia e escolha o tipo **Data / Date** na CatalyseR. Barras, traços e pontos são aceitos: `13/07/2026`, `13-07-2026`, `2026-07-13`, `2026/07/13` ou `13.07.2026`. Dia/mês sem zero à esquerda também funciona (`1/7/2026`); prefira ano com quatro dígitos. Valores não reconhecidos são informados para correção na planilha original.
- A barra é interpretada como dia/mês/ano. Mantenha um formato consistente na planilha. Datas inválidas impedem a conversão da coluna e são informadas para correção; números seriais do Excel devem ser formatados como datas no próprio Excel.

## CSV

- Escolha separadamente o delimitador das colunas e o separador decimal.
- Exemplo: colunas separadas por `;` e números como `12,5` → **Ponto e Vírgula** nas colunas e **Vírgula** no decimal.
- Exemplo: colunas separadas por `,` e números como `12.5` → **Vírgula** nas colunas e **Ponto** no decimal.
- Confira a prévia antes de preparar. Se a tabela inteira aparecer em uma coluna, revise o delimitador. Se os números aparecerem como texto, confira o decimal e possíveis letras misturadas aos valores.

## A aba inteira

- **Sem** células mescladas, cabeçalho de duas linhas, duas tabelas na mesma aba.
- **Sem** linhas de total/média/subtotal no meio dos dados.
- **Sem** linhas/colunas em branco como separador; **cor não é dado**.
- Notas e legendas ficam **em outra aba**.

## O que a CatalyseR faz por você (não precisa fazer na mão)

- **Nomes:** os nomes podem ser ajustados em Renomear variáveis. Não há padronização automática com `janitor`; prepare cabeçalhos únicos e não vazios. O leitor de Excel pode reparar cabeçalhos inválidos ao abrir o arquivo, por isso confira os nomes na prévia.
- Reconhece os tipos de entrada e permite conferir dados, categorias e ausentes. Linhas/colunas vazias e códigos como `-` ou `n/d` devem ser organizados previamente na planilha.
- Com sua escolha: padroniza espaços e caixa do texto, recodifica categorias, define tipos, reescala unidades, **pivota (long ↔ wide)**, separa colunas e trata duplicatas.
- Exclusão de linhas e preenchimento de ausentes são ações escolhidas pelo pesquisador. A IDE não decide quais valores extremos são erros e não transforma zero em ausente automaticamente. O arquivo original permanece preservado.
- Colunas **calculadas**: prefira criá-las **na CatalyseR** (ela preserva o original e registra a fórmula).

> Sobre **formato longo × largo**: não se preocupe em escolher. Entregue estruturalmente coerente — a CatalyseR deriva as bases certas (`base_pca`, `base_agrupamento`, …) sem tocar na sua planilha original.

## ✅ Checklist antes de enviar

- [ ] Uma tabela por aba, começando em A1, com **um** cabeçalho?
- [ ] Nomes de coluna **não vazios e não repetidos**?
- [ ] Uma **coluna identificadora** e **uma unidade por coluna**?
- [ ] Um valor por célula; números sem texto; datas como data/`AAAA-MM-DD`?
- [ ] Faltantes **em branco** (nunca `0`)?
- [ ] Categorias com **grafia única**, sem espaço no fim?
- [ ] Sem mescla, total no meio, ou cor-como-dado?

## Por que vale a pena

Não é burocracia — é o que **liberta**: os dados "simplesmente funcionam", ficam
**reprodutíveis** e legíveis para colegas, revisores e para você mesmo meses depois. E o
resto do preparo — o que depende da análise — a CatalyseR faz com você, mostrando **o que
fez e por quê**. Adotar isso desde a graduação é um hábito que acompanha o pesquisador a
vida inteira.
