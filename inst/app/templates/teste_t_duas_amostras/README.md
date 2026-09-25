# {{TITULO}}

Este projeto compara a média de **{{RESPOSTA}}** entre os dois grupos de
**{{GRUPO}}** por um teste t para amostras independentes. Abra
**{{PROJETO_RPROJ}}** no RStudio. O projeto funciona com a base local e pacotes
do CRAN.

## Um convite a aprender programação

Este projeto foi organizado para você acompanhar como a análise funciona: de
onde vêm os dados, quais pressupostos são checados e como os resultados chegam
ao relatório. Os comentários do script explicam cada decisão; os objetos com
nomes claros permitem examinar etapa por etapa. É o caminho do mouse ao código:
quem começa pela CatalyseR encontra aqui a chance de entender o que a ferramenta
faz e de modificar a análise com autonomia.

## O que você encontra

```text
{{PROJETO_RPROJ}}
├── _quarto.yml
├── dados/
│   ├── brutos/{{ARQUIVO_BRUTO}}     # planilha original, somente leitura
│   └── processados/                 # base preparada adotada pela análise
├── R/
│   ├── analise.R                    # leia e altere os cálculos aqui
│   └── funcoes.R                    # números, tema e tabelas Ocean
├── imagens/                         # para fotos e esquemas fornecidos por você
├── relatorios/
│   ├── relatorio_completo.qmd       # caderno HTML, com exploração e pressupostos
│   ├── relatorio_artigo.qmd         # Word, com os resultados principais
│   ├── referencias.bib
│   ├── apa.csl
│   ├── custom-reference.docx
│   └── ocean.scss
└── saida/
    ├── tabelas/                     # CSV
    ├── figuras/                     # PNG em 300 dpi
    ├── relatorios/                  # HTML e Word
    └── sessionInfo.txt              # R, pacotes e Quarto da execução
```

O R calcula; os QMDs executam esse script e apresentam os objetos prontos.
Cada Render recalcula a análise numa sessão limpa. Os QMDs **não leem** os
CSVs e PNGs de `saida/`: usam os objetos criados na memória da execução.

## Preparar o computador, uma vez

Instale R, RStudio e Quarto. No console do R, instale os pacotes:

```r
install.packages(c("here", "dplyr", "ggplot2", "car", "stringr",
                   "flextable", "knitr", "rmarkdown"))
```

Nenhum pacote é instalado automaticamente durante a análise.

## Gerar os documentos

1. Abra o `.Rproj` e reinicie o R para começar com uma sessão limpa.
2. Abra `relatorios/relatorio_completo.qmd` e clique em **Render** para o HTML.
3. Abra `relatorios/relatorio_artigo.qmd` e clique em **Render** para o Word.

Para gerar os dois pelo terminal, na raiz do projeto: `quarto render`.

## Como a análise decide o método

O teste t clássico pede **normalidade dentro de cada grupo** e **variâncias
parecidas entre os grupos**. O script confere a normalidade com Shapiro-Wilk em
cada grupo e a igualdade de variâncias com o **teste de Levene**. Quando Levene
não dá evidência de variâncias diferentes, usa-se o **t de Student**; caso
contrário, o **t de Welch**, guardado também para comparação. As frases dos
relatórios seguem o resultado real de cada teste: um p acima de {{IC}}% de
confiança complementar não prova o pressuposto, apenas não dá evidência para
rejeitá-lo.

O tamanho do efeito é o **d de Cohen**, calculado com o desvio padrão combinado,
com rótulos de referência (pequeno, médio, grande). A significância diz que a
diferença existe; o d diz o quanto ela importa.

## Origem dos dados

A base preparada foi adotada a partir da importação e dos tratamentos
registrados na CatalyseR. A planilha original fica em `dados/brutos/` e não é
alterada. O registro `saida/sessionInfo.txt` identifica o ambiente da execução.
