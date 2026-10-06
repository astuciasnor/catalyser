# {{TITULO}}

Este Projeto R foi gerado pela CatalyseR para estudar e comunicar uma ANOVA de
um fator, escrita em **ClaRa**, o R escrito com clareza. Abra
**{{PROJETO_RPROJ}}** no RStudio. O projeto funciona com a base local, pacotes
do CRAN e dois pacotes do ecossistema instalados do GitHub.

## Um convite a aprender programação

Na ClaRa, cada função responde a uma pergunta da pesquisa, em português:
`comparar_medias()` faz o resumo, a ANOVA, os pressupostos e o Tukey;
`grafico_medias()` desenha a figura principal; `escrever_resultados()` escreve
as frases do relatório. Por trás de cada função roda R comum, no estilo do
tidyverse. Para vê-lo, acrescente `mostrar_codigo = TRUE` a qualquer chamada:
a função imprime o código que vai rodar, com os nomes das suas colunas, e o
código mostrado é exatamente o que rodou. Para entender uma função, digite
`ajuda(comparar_medias)` no console.

## O que você encontra

```text
{{PROJETO_RPROJ}}
├── _quarto.yml
├── dados/
│   ├── brutos/                    entrada preservada
│   └── processados/               bases adotadas e base da ANOVA
├── R/
│   ├── analise.R                  o roteiro da análise, em ClaRa
│   ├── funcoes.R                  apresentação de números e tabelas
│   ├── clara.R                    a porta de entrada da ClaRa
│   └── clara_*.R                  as funções da ClaRa, um arquivo por pergunta
├── imagens/                       fotos e esquemas fornecidos pelo pesquisador
├── relatorios/
│   ├── relatorio_completo.qmd     caderno HTML
│   ├── relatorio_artigo.qmd       documento Word
│   ├── referencias.bib
│   ├── apa.csl
│   ├── custom-reference.docx
│   └── ocean.scss
└── saida/                         nasce quando o script roda
    ├── tabelas/
    ├── figuras/
    └── sessionInfo.txt
```

O `R/analise.R` é o lugar de estudo: rode as seções em ordem e examine os
objetos no console. Os dois QMDs **repetem as chamadas principais da ClaRa**,
de forma mais enxuta, e rodam sozinhos: o Render não lê o script. Por isso,
uma escolha mudada no script (um rótulo, a confiança, um detalhe da figura)
precisa ser mudada também nos relatórios.

| No script e nos relatórios | O que guarda | Cópia salva pelo script |
|---|---|---|
| `resultado <- ... comparar_medias(...)` | resumo, ANOVA, pressupostos e Tukey | `saida/tabelas/resumo_grupos.csv`, `anova.csv`, `tukey.csv` |
| `resultado \|> grafico_medias(...)` | a figura principal | `saida/figuras/barras.png` |
| `textos <- resultado \|> escrever_resultados()` | as frases do relatório | no texto: `` `r textos$teste` `` |

## Preparar o computador, uma vez

Instale R, RStudio e Quarto. No console do R, instale os pacotes do CRAN:

```r
install.packages(
{{PACOTES_INSTALAR}}
)
```

Depois, os dois pacotes do ecossistema, que não estão no CRAN e são
instalados do GitHub:

```r
remotes::install_github("astuciasnor/catalyser")
remotes::install_github("astuciasnor/EAPADados")
```

Nenhum pacote é instalado automaticamente durante a análise. Se um dos dois
pacotes do GitHub faltar, o script para na seção 1 e mostra o comando de
instalação.

## Gerar os documentos

1. Abra o `.Rproj` e reinicie o R para começar com uma sessão limpa.
2. Abra `relatorios/relatorio_completo.qmd` e clique em **Render** para o HTML.
3. Abra `relatorios/relatorio_artigo.qmd` e clique em **Render** para o Word.

Também é possível gerar os dois documentos, na raiz do projeto, com:

```sh
quarto render
```

Os caminhos usam `here::i_am()` e `here::here()` para reconhecer este projeto.
Abra o `.Rproj` antes de executar.

## Dados e preparo

A entrada preservada é `dados/brutos/{{ARQUIVO_BRUTO}}`. A CatalyseR exportou
a receita de preparo e a fotografia da base adotada. A seção 3 do script
reconstrói o percurso e confere essa fotografia; os relatórios partem da base
adotada. Alterar a receita não substitui silenciosamente a base adotada.

A análise usa:

- resposta: **{{RESPOSTA}}**;
- fator (grupos comparados): **{{FATOR}}**;
- intervalo de confiança: **{{IC}}%**.

## Como escrever e adaptar

Os dois QMDs trazem sugestões em Introdução, Material e métodos, Resultados,
Discussão e Conclusão. O HTML documenta o percurso completo, com a exploração e
os diagnósticos; o Word seleciona os resultados esperados em um artigo. As
frases de `escrever_resultados()` mudam junto com os dados, mas a discussão e
a conclusão científica precisam ser revistas pelo pesquisador.

## A versão da ClaRa

Este projeto leva a sua própria cópia da ClaRa em `R/`. A versão aparece no
console quando ela é carregada e fica em `saida/sessionInfo.txt`. Projetos
exportados em datas diferentes podem ter versões diferentes da ClaRa.

## Origem dos dados

Registre aqui a origem da planilha, a licença e o período de coleta. A
CatalyseR não conhece a proveniência dos seus dados e não a declara no lugar
do pesquisador. A planilha original fica em `dados/brutos/{{ARQUIVO_BRUTO}}` e
não é alterada.

## Como ler a figura principal

Pontos mostram as observações e o losango marca a média. O rótulo traz média
± DP amostral, com duas casas decimais. As hastes mostram o IC da média no
nível escolhido, enquanto o DP do rótulo descreve a dispersão dos indivíduos.
IC e DP respondem a perguntas diferentes; não se deve interpretar um como se
fosse o outro.

## Método da ANOVA

Este roteiro usa a ANOVA clássica com Tukey. Se o Levene indicar variâncias
diferentes, o relatório avisa; nesse caso, a ANOVA de Welch com Games-Howell,
disponível na CatalyseR, é a alternativa a considerar.

## Ambiente computacional

Ambiente registrado automaticamente na exportação:

{{AMBIENTE_COMPUTACIONAL}}

Ao executar, o script registra o ambiente efetivo em `saida/sessionInfo.txt`.
