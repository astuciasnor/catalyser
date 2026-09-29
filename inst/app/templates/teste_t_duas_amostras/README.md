# {{TITULO}}

Este projeto compara a média de **{{RESPOSTA}}** entre os dois grupos de
**{{GRUPO}}** por um teste t para amostras independentes. Abra
**{{PROJETO_RPROJ}}** no RStudio. O projeto funciona com a base local,
pacotes do CRAN e dois pacotes do ecossistema instalados do GitHub.

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

| No script R | No relatório | Cópia salva para compartilhar |
|---|---|---|
| `tabela_descritiva` contém os números; `tabela_descritiva_exibir` formata | `flextable_ocean(tabela_descritiva_exibir)` | `saida/tabelas/descritiva.csv` |
| `tabela_teste` e `tabela_pressupostos` guardam os testes | `flextable_ocean(tabela_teste)` e `flextable_ocean(tabela_pressupostos)` | `saida/tabelas/teste.csv` e `pressupostos.csv` |
| `grafico_caixa` e `grafico_medias` guardam as figuras | `grafico_caixa` e `grafico_medias` | `saida/figuras/caixa.png` e `medias.png` |
| `texto_resultado`, `texto_efeito` e `texto_pressupostos` reúnem números em frases | Expressão R inline no parágrafo | As frases entram no HTML e no Word |

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

Para gerar os dois pelo terminal, na raiz do projeto: `quarto render`.

O Render **executa** o `R/analise.R` antes de montar cada documento. Não use
modos que pulam essa execução (como `quarto render --no-execute`): os
relatórios leem objetos calculados pelo script e, sem a execução, param com
`object '<nome>' not found` na primeira expressão.

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
acompanhado do seu intervalo de confiança (pacote `effectsize`) e de rótulos de
referência (pequeno, médio, grande). A significância diz que a diferença
existe; o d diz o quanto ela importa. Quando o teste não encontra evidência de
diferença e o poder estatístico é baixo (pacote `pwr`), os relatórios
acrescentam uma ressalva: ausência de evidência não é evidência de ausência.

## Como escrever e adaptar

Os dois QMDs trazem sugestões em Introdução, Material e métodos, Resultados,
Discussão e Conclusão. O HTML documenta o percurso completo, com a exploração e
os pressupostos; o Word seleciona os resultados esperados em um artigo. Edite
os cálculos no script e a argumentação nos QMDs.

Depois das tabelas e dos gráficos, a seção 9 do script reúne os resultados em
frases e as mostra no console com `print()`. Os relatórios usam esses objetos,
mas a discussão e a conclusão científica precisam ser revistas pelo pesquisador.

## Reprodutibilidade

Os dados de entrada permanecem em `dados/brutos/`. Produtos regeneráveis ficam
em `dados/processados/` e `saida/`. O arquivo `saida/sessionInfo.txt`
registra as versões do R, dos pacotes e do Quarto usadas na execução.

O estilo bibliográfico fornecido é APA. Para outra revista, coloque o arquivo
CSL correspondente em `relatorios/` e altere o caminho em `_quarto.yml`.

## Origem dos dados

Registre aqui a origem da planilha, a licença e o período de coleta — a
CatalyseR não conhece a proveniência dos seus dados e não a declara no lugar
do pesquisador. A base preparada foi adotada a partir da importação e dos
tratamentos registrados na CatalyseR. A planilha original fica em
`dados/brutos/{{ARQUIVO_BRUTO}}` e não é alterada. O registro
`saida/sessionInfo.txt` identifica o ambiente da execução.
