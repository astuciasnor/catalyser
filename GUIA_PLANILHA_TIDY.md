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

- Melhor: **data de verdade do Excel**. Se for texto, use **`AAAA-MM-DD`** (`2026-07-13`).
- Evite formatos misturados (`15/10/2020`, `Out 15`) e erros como `15/10//2020`.

## A aba inteira

- **Sem** células mescladas, cabeçalho de duas linhas, duas tabelas na mesma aba.
- **Sem** linhas de total/média/subtotal no meio dos dados.
- **Sem** linhas/colunas em branco como separador; **cor não é dado**.
- Notas e legendas ficam **em outra aba**.

## O que a CatalyseR faz por você (não precisa fazer na mão)

- **Nomes:** você pode manter `Comprimento total (cm)`; ela cria o nome técnico interno (`comprimento_total_cm`) e mostra o original nos resultados.
- Retira espaços acidentais; remove linhas/colunas vazias; detecta nomes repetidos; sugere tipos; mostra categorias e ausentes.
- Com sua confirmação: converte texto→número/data, une categorias parecidas, tipa fatores, ordena níveis, converte unidades, **pivota (long ↔ wide)**, separa/une colunas, trata duplicatas.
- **Nunca** faz nada silencioso perigoso: não exclui *outliers*, não imputa, não vira zero em ausente, não altera valores originais.
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
