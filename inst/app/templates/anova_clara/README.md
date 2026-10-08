# {{TITULO}}

Este Projeto R foi gerado pela CatalyseR para estudar e comunicar uma ANOVA de
um fator, escrita em **ClaRa**, o R escrito com clareza. Abra
**{{PROJETO_RPROJ}}** no RStudio. O projeto funciona com a base local e
pacotes do CRAN; não precisa da CatalyseR instalada.

## Um convite a aprender programação

Na ClaRa, cada função responde a uma pergunta da pesquisa, em português:
`comparar_medias()` faz o resumo, a ANOVA, os pressupostos e o {{POS_TESTE}};
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
│   └── {{ARQUIVO_BRUTO_ARVORE}}a planilha, entrada preservada
├── R/
│   ├── analise.R                  o roteiro da análise, em ClaRa
│   ├── funcoes.R                  apresentação de números e tabelas
│   └── clara/                     a ClaRa: clara.R, a porta de entrada, e um
│                                  arquivo clara_*.R por pergunta
├── imagens/                       fotos e esquemas fornecidos pelo pesquisador
├── relatorios/
│   ├── relatorio.qmd              o relatório, que vira Word
│   ├── referencias.bib
│   ├── apa.csl
│   └── custom-reference.docx
└── saida/                         nasce quando o script roda
    ├── tabelas/
    ├── figuras/
    ├── relatorios/                o Word gerado no Render
    └── sessionInfo.txt
```

Cada arquivo tem um papel só:

- **`R/analise.R` é o caderno de estudo.** Rode as seções em ordem e examine
  os objetos no console: a receita de preparo, a exploração, a análise, os
  diagnósticos dos pressupostos e os textos dinâmicos.
- **`relatorios/relatorio.qmd` é o relatório.** Nele você escreve o artigo ou
  a tese; o Render gera o Word com os resultados principais. Ele **repete as
  chamadas principais da ClaRa**, de forma mais enxuta, e roda sozinho: o
  Render não lê o script. Por isso, uma escolha mudada no script (um rótulo,
  a confiança, um detalhe da figura) precisa ser mudada também no relatório.

| No script e no relatório | O que guarda | Cópia salva pelo script |
|---|---|---|
| `resultado <- ... comparar_medias(...)` | resumo, ANOVA, pressupostos e {{POS_TESTE}} | `saida/tabelas/resumo_grupos.csv`, `anova.csv`, `{{ARQUIVO_PARES_CSV}}` |
| `resultado \|> grafico_medias(...)` | a figura principal | `saida/figuras/barras.png` |
| `textos <- resultado \|> escrever_resultados()` | as frases do relatório | no texto: `` `r textos$teste` `` |

## Preparar o computador, uma vez

Instale R, RStudio e Quarto. No console do R, instale os pacotes do CRAN:

```r
install.packages(
{{PACOTES_INSTALAR}}
)
```

Nenhum pacote é instalado automaticamente durante a análise.

## Gerar o relatório

1. Abra o `.Rproj` e reinicie o R para começar com uma sessão limpa.
2. Abra `relatorios/relatorio.qmd` e clique em **Render**: sai o Word, em
   `saida/relatorios/`.

Também é possível gerá-lo, na raiz do projeto, com:

```sh
quarto render
```

Os caminhos usam `here::i_am()` e `here::here()` para reconhecer este projeto.
Abra o `.Rproj` antes de executar.

## Dados e preparo

A entrada preservada é `dados/{{ARQUIVO_BRUTO}}`, a única coisa na pasta
`dados/`: com uma planilha só, não há subpastas. A CatalyseR exportou
a receita de preparo: a seção 3 do script a aplica à planilha, com pipe e
dplyr, e o script e o relatório partem da base que ela produz. No lugar de
uma cópia da base, o script traz um **carimbo**: o número de linhas, a
contagem e a média por grupo que a CatalyseR mostrou na tela. Confira a
tabela do R com o carimbo; se a receita mudar o número de linhas, o
`stopifnot()` para o script e o Render. Para abrir a base fora do R, o
script grava uma cópia em `saida/tabelas/base.csv`, refeita a cada execução:
ela sempre bate com a receita.

A análise usa:

- resposta: **{{RESPOSTA}}**;
- fator (grupos comparados): **{{FATOR}}**;
- intervalo de confiança: **{{IC}}%**.

## Origem dos dados

Registre aqui a origem da planilha, a licença e o período de coleta. A
CatalyseR não conhece a proveniência dos seus dados e não a declara no lugar
do pesquisador. A planilha original fica em `dados/{{ARQUIVO_BRUTO}}` e
não é alterada.

## Método da ANOVA

{{METODO_README}}

## Como ler a figura principal

Pontos mostram as observações e o losango marca a média. O rótulo traz média
± DP amostral, com duas casas decimais. As hastes mostram o IC da média no
nível escolhido, enquanto o DP do rótulo descreve a dispersão dos indivíduos.
IC e DP respondem a perguntas diferentes; não se deve interpretar um como se
fosse o outro.

## Como escrever e adaptar

O relatório traz sugestões em Introdução, Material e métodos, Resultados,
Discussão e Conclusão, com os resultados esperados em um artigo: a tabela da
ANOVA, o resumo por grupo e a figura principal. A exploração e os
diagnósticos ficam no roteiro, onde se conferem. As frases de
`escrever_resultados()` mudam junto com os dados, mas a discussão e a
conclusão científica precisam ser revistas pelo pesquisador.

## Ambiente computacional

Ambiente registrado automaticamente na exportação:

{{AMBIENTE_COMPUTACIONAL}}

Ao executar, o script registra o ambiente efetivo em `saida/sessionInfo.txt`,
com a versão da ClaRa. Este projeto leva a sua própria cópia da ClaRa em
`R/clara/`; projetos exportados em datas diferentes podem ter versões
diferentes dela.
