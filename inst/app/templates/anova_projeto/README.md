# {{TITULO}}

Este Projeto R foi gerado pela CatalyseR para estudar e comunicar uma ANOVA de um fator.
Abra **{{PROJETO_RPROJ}}** no RStudio. O projeto funciona com a base local e pacotes
do CRAN.

## Um convite a aprender programação

O arquivo `R/analise.R` mostra como a base preparada se transforma em ANOVA,
comparações de Tukey, pressupostos, tabelas, gráficos e textos estatísticos.
Os comentários explicam as decisões e as operações menos familiares. Execute as seções em ordem e
examine os objetos indicados no começo do script. É o caminho do mouse ao código:
quem começa pela CatalyseR encontra aqui a chance de entender o que a ferramenta
faz e de modificar a análise com autonomia.

## O que você encontra

```text
{{PROJETO_RPROJ}}
├── _quarto.yml
├── dados/
│   ├── brutos/                    entrada preservada
│   └── processados/               bases adotadas e base da ANOVA
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

O R calcula; os QMDs executam esse script e apresentam os objetos prontos.
Você não precisa copiar código entre arquivos nem sincronizar chunks.
O Render de cada documento recalcula a análise e recria as saídas. O projeto
cria automaticamente as pastas necessárias. Não depende de objetos no console.

Os QMDs **não leem** as tabelas CSV ou figuras PNG de `saida/`. Eles usam os
objetos que o script acabou de criar na memória. Se você executar o script no
RStudio, verá os objetos no Environment; se clicar em Render, o Quarto usa uma
sessão própria. Não é preciso povoar o Environment manualmente antes do Render.

## Preparar o computador, uma vez

Instale R, RStudio e Quarto. No console do R, instale os pacotes:

```r
install.packages(
{{PACOTES_INSTALAR}}
)
```

Nenhum pacote é instalado automaticamente durante a análise.

## Gerar os documentos

1. Abra o `.Rproj` e reinicie o R para começar com uma sessão limpa.
2. Abra `relatorios/relatorio_completo.qmd` e clique em **Render** para o HTML.
3. Abra `relatorios/relatorio_artigo.qmd` e clique em **Render** para o Word.

Também é possível gerar os dois documentos, na raiz do projeto, com:

```sh
quarto render
```

O Render **executa** o `R/analise.R` antes de montar cada documento. Não use
modos que pulam essa execução (como `quarto render --no-execute`): os
relatórios leem objetos calculados pelo script e, sem a execução, param com
`object '<nome>' not found` na primeira expressão.

Os caminhos usam `here::i_am()` e `here::here()` para reconhecer este projeto,
inclusive quando ele está dentro de outro projeto R. Abra o `.Rproj` antes de
executar; se mover o arquivo para outra subpasta, atualize a declaração
`here::i_am("R/analise.R")` no começo do script.

## Dados e preparo

A entrada preservada é `dados/brutos/{{ARQUIVO_BRUTO}}`. A CatalyseR exportou
a receita de preparo e a fotografia da base adotada. O script reconstrói o
percurso e confere essa fotografia antes da análise. Alterar a receita não
substitui silenciosamente a base adotada.

A análise usa:

- resposta: **{{RESPOSTA}}**;
- fator (grupos comparados): **{{FATOR}}**;
- intervalo de confiança: **{{IC}}%**.

## Como escrever e adaptar

Os dois QMDs trazem sugestões em Introdução, Material e métodos, Resultados,
Discussão e Conclusão. O HTML documenta o percurso completo, com a exploração e
os diagnósticos; o Word seleciona os resultados esperados em um artigo. Edite
os cálculos no script e a argumentação nos QMDs.

Depois das tabelas e dos gráficos, a seção 10 do script reúne os resultados em
frases e as mostra no console com `print()`. Os relatórios usam esses objetos,
mas a discussão e a conclusão científica precisam ser revistas pelo pesquisador.

## Reprodutibilidade

Os dados de entrada permanecem em `dados/brutos/`. Produtos regeneráveis ficam
em `dados/processados/` e `saida/`. O arquivo `saida/sessionInfo.txt`
registra as versões do R, dos pacotes e do Quarto usadas na execução.

O estilo bibliográfico fornecido é APA. Para outra revista, coloque o arquivo
CSL correspondente em `relatorios/` e altere o caminho em `_quarto.yml`.
