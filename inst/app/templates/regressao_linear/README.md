# {{TITULO}}

Este Projeto R foi gerado pela CatalyseR para estudar e comunicar uma regressão
linear simples. Abra **{{PROJETO_RPROJ}}** no RStudio.

## Um convite a aprender programação

O arquivo `R/analise.R` mostra como a base preparada se transforma em modelo,
diagnósticos, tabelas, gráficos e textos estatísticos. Os comentários explicam
as decisões e as operações menos familiares. Execute as seções em ordem e
examine os objetos indicados no começo do script.

## Estrutura

```text
projeto/
├── _quarto.yml
├── {{PROJETO_RPROJ}}
├── dados/
│   ├── brutos/                    entrada preservada
│   └── processados/               bases adotadas e base da regressão
├── R/
│   ├── analise.R                  fonte da verdade da análise
│   └── funcoes.R                  apresentação de números, tabelas e figuras
├── imagens/                       fotos e esquemas fornecidos pelo pesquisador
├── relatorios/
│   ├── relatorio_completo.qmd     caderno HTML
│   ├── relatorio_artigo.qmd       documento Word
│   ├── referencias.bib
│   ├── apa.csl
│   ├── custom-reference.docx
│   └── ocean.scss
└── saida/
    ├── tabelas/
    ├── figuras/
    ├── relatorios/
    └── sessionInfo.txt
```

## Como executar

1. Abra o arquivo `.Rproj`.
2. Reinicie o R.
3. Abra `relatorios/relatorio_completo.qmd` e clique em **Render** para gerar
   o HTML.
4. Abra `relatorios/relatorio_artigo.qmd` e clique em **Render** para gerar
   o Word.

Também é possível gerar os dois documentos, na raiz do projeto, com:

```sh
quarto render
```

Os dois QMDs executam o mesmo `R/analise.R` numa sessão nova. Eles usam os
objetos recém-calculados; os CSVs e PNGs de `saida/` são cópias para consulta.

## Dados e preparo

A entrada preservada é `dados/brutos/{{ARQUIVO_BRUTO}}`. A CatalyseR exportou
a receita de preparo e a fotografia da base adotada. O script reconstrói o
percurso e confere essa fotografia antes da análise. Alterar a receita não
substitui silenciosamente a base adotada.

A análise usa:

- resposta: **{{RESPOSTA}}**;
- preditor: **{{PREDITOR}}**;
- grupo exploratório: **{{GRUPO}}**;
- intervalo de confiança: **{{IC}}%**.

## O papel dos textos

Depois das tabelas e dos gráficos, a seção 9 reúne os resultados em frases e os
mostra no console com `print()`. Os relatórios usam esses objetos, mas a
discussão e a conclusão científica precisam ser revistas pelo pesquisador.

O HTML documenta o percurso completo. O Word seleciona os resultados esperados
em um artigo. Edite os cálculos no script e a argumentação nos QMDs.

## Reprodutibilidade

Os dados de entrada permanecem em `dados/brutos/`. Produtos regeneráveis ficam
em `dados/processados/` e `saida/`. O arquivo `saida/sessionInfo.txt`
registra as versões do R, dos pacotes e do Quarto usadas na execução.

O estilo bibliográfico fornecido é APA. Para outra revista, coloque o arquivo
CSL correspondente em `relatorios/` e altere o caminho em `_quarto.yml`.
