# {{TITULO}}

Este Projeto R foi gerado pela CatalyseR para estudar e comunicar uma regressão
linear simples. Abra **{{PROJETO_RPROJ}}** no RStudio. O projeto funciona com a
base local, pacotes do CRAN e dois pacotes do ecossistema instalados do GitHub.

## Um convite a aprender programação

O arquivo `R/analise.R` mostra como a base preparada se transforma em modelo,
diagnósticos, tabelas, gráficos e textos estatísticos. Os comentários explicam
as decisões e as operações menos familiares. Execute as seções em ordem e
examine os objetos indicados no começo do script. É o caminho do mouse ao código:
quem começa pela CatalyseR encontra aqui a chance de entender o que a ferramenta
faz e de modificar a análise com autonomia.

## O que você encontra

```text
{{PROJETO_RPROJ}}
├── _quarto.yml
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

O R calcula; os QMDs executam esse script e apresentam os objetos prontos.
Você não precisa copiar código entre arquivos nem sincronizar chunks.
O Render de cada documento recalcula a análise e recria as saídas. O projeto
cria automaticamente as pastas necessárias. Não depende de objetos no console.

Os QMDs **não leem** as tabelas CSV ou figuras PNG de `saida/`. Eles usam os
objetos que o script acabou de criar na memória. Se você executar o script no
RStudio, verá os objetos no Environment; se clicar em Render, o Quarto usa uma
sessão própria. Não é preciso povoar o Environment manualmente antes do Render.

| No script R | No relatório | Cópia salva para compartilhar |
|---|---|---|
| `tabela_coeficientes` contém os números; `tabela_coeficientes_exibir` formata | `flextable_ocean(tabela_coeficientes_exibir)` | `saida/tabelas/coeficientes.csv` |
| `tabela_ajuste` guarda as métricas do modelo | `flextable_ocean(tabela_ajuste)` | `saida/tabelas/ajuste.csv` |
| `grafico_regressao` guarda a figura | `grafico_regressao` | `saida/figuras/regressao.png` |
| `texto_ajuste` e `texto_coeficiente` reúnem números em frases | Expressão R inline no parágrafo | As frases entram no HTML e no Word |

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
- preditor: **{{PREDITOR}}**;
- grupo exploratório: **{{GRUPO}}**;
- intervalo de confiança: **{{IC}}%**.

## Como escrever e adaptar

Os dois QMDs trazem sugestões em Introdução, Material e métodos, Resultados,
Discussão e Conclusão. O HTML documenta o percurso completo, com a exploração e
os diagnósticos; o Word seleciona os resultados esperados em um artigo. Edite
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

## Ambiente computacional

Ambiente registrado automaticamente na exportação:

{{AMBIENTE_COMPUTACIONAL}}

Ao executar, o script registra o ambiente efetivo em `saida/ambiente.csv` e
`saida/sessionInfo.txt`; o relatório completo apresenta a tabela atualizada.
Um commit ausente nos metadados aparece como “não registrado”.

Para conferir a reprodução dos dois documentos, abra um terminal na raiz e rode:

```text
Rscript verificar_reprodutibilidade.R
```

A conferência recalcula cada QMD em um processo novo, registra os logs em
`saida/verificacao/` e retorna erro se algum render falhar, se o arquivo
não for novo ou se houver referência `??`. Ela não instala pacotes.

## Ler o diagnóstico antes dos resultados

O caderno apresenta os diagnósticos dentro da Exploração, antes dos Resultados,
com gráficos empilhados e menores. A saída crua fica para estudo no console:
execute `summary(modelo_lm)` em R/analise.R. Os relatórios usam as tabelas
formatadas e não exibem esse despejo de console.

## Quando foram escolhidas retas por categoria

`retas_por_grupo` conserva a escolha do painel. Cada reta mostra sua equação
e R², com um modelo calculado apenas nas observações daquela categoria.
A tabela adicional reúne N, intercepto, inclinação e R² de cada ajuste.
A tabela principal e os diagnósticos globais são identificados separadamente.
Retas separadas não testam se as inclinações diferem; confira também os
pressupostos de cada categoria antes de interpretar seus coeficientes.
