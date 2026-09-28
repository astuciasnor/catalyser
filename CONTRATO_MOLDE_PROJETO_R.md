# Contrato do Molde de Projeto R

Este documento registra o contrato do **molde novo** de Projeto R exportado pela
CatalyseR: a árvore de pastas, o contrato de `R/analise.R`, os objetos que os
dois QMDs consomem, a divisão HTML × Word, o catálogo de marcadores de template
e o passo a passo para adicionar uma análise. Foi extraído do que já existe
implementado (templates e exportador), não inventa regras novas. As decisões
de 16/09 e 18/09 estão em `MODULO_COMUNICACAO_RESULTADOS.md`; o roteiro de
implementação e o gabarito aprovado estão em `PLANO_GERACAO_PROJETO_R_V1.md`.

Análises no molde novo (set/2026): **regressão linear simples** (uma execução
global), **ANOVA de um fator** (isolada no projeto) e **teste t de duas
amostras independentes** (isolado no projeto). As demais análises seguem o
exportador geral (árvore legada, relatório sincronizado).

## 1. Árvore do projeto exportado

```
projeto/
├── _quarto.yml                    renderiza os dois QMDs; saídas em saida/
├── projeto.Rproj
├── dados/
│   ├── brutos/<ARQUIVO_BRUTO>     a planilha de entrada, preservada (somente leitura)
│   └── processados/               bases adotadas (fotografias p/ conferência + cópias Excel)
├── R/
│   ├── analise.R                  FONTE DA VERDADE da análise (seções numeradas)
│   └── funcoes.R                  apresentação: fmt, formatar_p, tema_projeto, flextable_ocean
├── imagens/
│   └── LEIA-ME.txt                para que serve a pasta (fotos e esquemas do pesquisador)
├── relatorios/
│   ├── relatorio_completo.qmd     caderno HTML (percurso completo)
│   ├── relatorio_artigo.qmd       documento Word (recorte de artigo)
│   ├── referencias.bib
│   ├── apa.csl                    estilo bibliográfico padrão (APA)
│   ├── custom-reference.docx      modelo de página do Word (tema Ocean)
│   └── ocean.scss                 tema HTML
└── saida/                         criada pelo script na 1ª execução; NÃO viaja no ZIP
    ├── tabelas/                   CSVs (cópias para consulta; não alimentam os QMDs)
    ├── figuras/                   PNGs (idem)
    ├── relatorios/                onde _quarto.yml entrega HTML e Word
    └── sessionInfo.txt            R, pacotes e versão do Quarto
```

Regras da árvore:

- O exportador gera três blocos que entram DENTRO das seções numeradas do
  `R/analise.R` (bibliotecas na seção 1, leitura da planilha na seção 2,
  preparo e adoção da base na seção 3); o restante vem do template da análise.
  O roteiro tem uma numeração contínua, sem cabeçalho ou prefixo antes dela.
- `dados/brutos/` guarda somente a aba utilizada da planilha original. Os
  dados brutos nunca são editados pelo script.
- `dados/processados/` guarda as fotografias para conferência
  (`base_compartilhada.rds` e `.xlsx`, `base_resolvida.rds` quando aplicável) e
  a base da análise em CSV no fim do roteiro.
- `saida/` só guarda produtos regeneráveis e **não viaja no ZIP**: a seção 2
  do script a cria na primeira execução (assim como `dados/processados/`). O
  ZIP também não carrega `.quarto/` nem outros artefatos de build. Os QMDs
  **não leem** nada de `saida/`: cada Render executa `R/analise.R` numa sessão
  nova e usa os objetos em memória. Os CSVs e PNGs são cópias para consulta e
  compartilhamento.
- Pastas vazias do molde viajam com um `LEIA-ME.txt` curto explicando a função
  da pasta (hoje: `imagens/`), para não serem perdidas pelo ZIP.
- `funcoes.R`, `apa.csl` e `_quarto.yml` são únicos para todo o molde e moram
  em `templates/regressao_linear/`; o exportador os copia para qualquer análise.
- **Regra do molde:** o projeto exportado usa **só pacotes do CRAN** — nada de
  `EAPADados` nem de `catalyser`. Os dados de contexto do EAPADados viajam como
  arquivos em `dados/brutos/`, e as funções de apoio (`conferir_base()`,
  `moda()`, `converter_datas()`) viajam em `R/funcoes.R`. O exportador aplica
  essa regra num ponto único (`exportacao_sanitizar_molde()`), antes de gravar
  o script.
- O estilo padrão é **APA** (`relatorios/apa.csl`). `abnt.csl` não é mais
  copiado para projetos novos.

## 2. Contrato de R/analise.R

### 2.1 Cabeçalho do roteiro (as 30 primeiras linhas)

Todo `analise_projeto.R` de template começa com o mesmo cabeçalho, nesta ordem:

1. `# {{TITULO_COMENTARIO}} — ROTEIRO DE ANÁLISE` e a régua `x====x`;
2. `# Pergunta: {{PERGUNTA_COMENTARIO}}` — a pergunta da análise em uma frase;
3. `# COMO ESTUDAR` — como abrir o `.Rproj`, executar seções com Ctrl+Enter,
   examinar objetos no console e navegar pelo sumário (Ctrl+Shift+O);
4. `# MAPA DO ROTEIRO` — o que cada bloco de seções faz, em 4–5 linhas;
5. `# OBJETOS QUE OS RELATÓRIOS VÃO USAR` — um objeto por linha, com descrição
   curta (o "mapa de objetos"; os QMDs consomem exatamente esses nomes);
6. Três linhas fixas: cada QMD executa o script numa sessão nova; CSVs/PNGs são
   cópias; cálculos se editam no script e a argumentação nos QMDs; instalar
   pacotes uma única vez conforme o README.

### 2.2 Seções numeradas, em ordem

As seções usam o formato de seção do RStudio (`# 1. Nome ------`), para o
sumário do editor (Ctrl+Shift+O) navegar pelo roteiro. A ordem é obrigatória;
o número total varia com a análise (12 na ANOVA, 11 na regressão e no teste t).
A numeração é **contínua e única**: o preparo gerado pela IDE entra DENTRO
dela, nos marcadores `{{BIBLIOTECAS_PREPARO}}`, `{{TRECHO_IMPORTAR}}` e
`{{TRECHO_PREPARO}}` — nada de cabeçalho ou prefixo antes da seção 1, e
`funcoes.R` é carregado uma única vez, na seção 1:

1. **Preparar o ambiente** — na ordem do barbo: `library(here)`, o comentário
   que o explica, `here::i_am("R/analise.R")`, o bloco gerado de bibliotecas da
   leitura e do preparo (readxl, dplyr, tidyr, lubridate, ggplot2, car,
   multcompView, effectsize — todas do CRAN), os `library()` da análise e
   `source(here::here("R", "funcoes.R"), encoding = "UTF-8")`, uma única vez.
2. **Definir as escolhas e ler os dados** — variáveis e fator escolhidos
   na CatalyseR (com marcadores `{{..._R}}`), rótulos de apresentação, nível de
   confiança, `alfa`, `ic_percentual`, paleta de cores, o laço que cria
   `dados/processados` e `saida/{tabelas,figuras,relatorios}` e, no fim, o
   bloco gerado `{{TRECHO_IMPORTAR}}` (da planilha a `dados_brutos`).
3. **Preparar a base** — o bloco gerado `{{TRECHO_PREPARO}}` (operações
   estruturais, trilha de tratamentos, conferência de `dados_analise` contra a
   fotografia, banner da análise e adoção de `dados_da_analise`) e, na
   sequência do template, validações com `stop()` (nomes presentes, resposta
   numérica, grupos suficientes, etc.), casos completos, `n_total`,
   `n_utilizado`, `n_excluido` e a base `base_<analise>`. Os títulos internos
   do preparo gerado não são numerados, para não colidirem com a numeração do
   roteiro.
4. **Explorar** — resumos por grupo/medidas (tabelas de exploração), com
   comentários sobre o que procurar.
5. **Ajustar o modelo** — a chamada canônica (mesma da CatalyseR), `resumo_console`
   para o aluno conhecer a saída bruta uma vez, e a extração dos resultados
   (tidy/glance ou equivalente).
6. **Pressupostos e diagnóstico** — resíduos, Shapiro-Wilk (com a regra dos
   3–5000 e da variação nula: registra `NA` e a leitura cai no Q-Q), Levene ou
   Breusch-Pagan, e as leituras `leitura_*` (NA = teste não calculado, não
   atendido). Na ANOVA: também as comparações (Tukey + `multcompLetters4`) e o
   tamanho do efeito (η²/ω² ou d de Cohen).
7–8. **Tabelas de apresentação** — as `*_exibir` (formatação com `fmt()` e
   `formatar_p()`; os objetos numéricos ficam intactos).
8–9. **Gráficos** — subseções numeradas (8.1, 9.1…), um gráfico por subseção,
   com `tema_projeto()`, cores Ocean e comentários "o que conferir"/"o que é
   sinal de problema" nos diagnósticos.
9–10. **Preparar os textos** — os textos ficam **no fim**, depois de tabelas e
   gráficos: primeiro a evidência e as leituras, depois os textos, cada um
   atribuído com `<-` e mostrado com `print()`. O artigo recebe frases curtas
   (`*_artigo`); o caderno recebe também a orientação de leitura.
11. **Salvar cópias** — `write.csv2()` (ponto e vírgula, vírgula decimal) para
    a base e as tabelas em `saida/tabelas/`; `ggsave()` para as figuras em
    `saida/figuras/` (7 × 4,6, 300 dpi, fundo branco).
12. **Registrar o ambiente** — `registro_ambiente` com a versão do Quarto e o
    `sessionInfo()`, gravado em `saida/sessionInfo.txt` (`useBytes = TRUE`).

### 2.3 Voz dos comentários

Comentários em português, curtos, em tom de professor conversando: explicam o
**porquê** e as operações menos familiares (o que `|>` faz, o que é `NA_real_`,
por que `droplevels()`, por que o hífen não pode aparecer no nome de grupo).
Não descrevem o óbvio. Uma vez por roteiro, o aluno é convidado a digitar
`resumo_console` no console para conhecer a saída bruta — o console cru
aparece pelo menos uma vez, de propósito.

### 2.4 README do projeto

O `README.md` do molde segue as seções do barbo: **Um convite a aprender
programação**, **O que você encontra** (a árvore comentada), **Preparar o
computador, uma vez** (com `install.packages({{PACOTES_INSTALAR}})` e a frase
"Nenhum pacote é instalado automaticamente durante a análise."), **Gerar os
documentos** (com o aviso de que o Render executa o script e de que
`--no-execute` falha com `object not found`), as seções específicas da análise
(**Dados e preparo** / **Como a análise decide o método**), **Como escrever e
adaptar**, **Reprodutibilidade** e **Origem dos dados** conforme o tipo. O
marcador `{{PACOTES_INSTALAR}}` recebe a **lista exata** de pacotes usados por
`R/analise.R` e `R/funcoes.R` do projeto (`exportacao_molde_pacotes()` sobre o
script recém-gerado e o `funcoes.R` da pasta de apoio), montada como
`c("a", "b", ...)` quebrado em linhas — nunca uma lista fixa.

## 3. Objetos que os QMDs consomem

O contrato é: **os QMDs só usam objetos criados por `R/analise.R`** — nunca
caminhos de arquivo, nunca `saida/`. Todo script entrega pelo menos:

| Objeto | Papel |
|---|---|
| `n_total`, `n_utilizado`, `n_excluido` | tamanhos da amostra |
| `texto_amostra` | frase com a amostra utilizada (M&M/Resultados) |
| `texto_sintese_estatistica` | síntese de uma frase (Conclusão) |
| `alerta_modelo` | alerta honesto sobre pressupostos/sinais de inadequação |
| `registro_ambiente` | vetor de linhas do ambiente computacional |
| `ic_percentual` | nível de confiança em texto (usado nos métodos) |

Por análise, o mapa de objetos do cabeçalho lista os demais:

- **Regressão**: `base_regressao`, `modelo_lm`, `tabela_coeficientes(_exibir)`,
  `tabela_ajuste`, `tabela_descritiva(_exibir)`, `tabela_testes`,
  `grafico_regressao`, `grafico_residuos`, `grafico_qq`, `grafico_escala`,
  `grafico_cook`, `grafico_alavancagem` (+ gráficos de grupo/ordem quando
  aplicáveis), `texto_ajuste`, `texto_coeficiente`, `texto_diagnosticos(_artigo)`,
  `texto_influencia(_artigo)`.
- **ANOVA**: `base_anova`, `modelo_anova`, `tabela_anova(_exibir)`,
  `tabela_resumo(_exibir)`, `tabela_tukey(_exibir)`, `tabela_testes`,
  `tabela_efeito`, `grafico_barras`, `grafico_boxplot`, `grafico_residuos`,
  `grafico_qq`, `texto_anova`, `texto_efeito`, `texto_tukey`,
  `texto_pressupostos(_artigo)`.
- **Teste t**: `dados`, `teste_t`, `d_cohen`, `tabela_descritiva_exibir`,
  `tabela_teste`, `tabela_pressupostos`, `grafico_caixa`, `grafico_medias`,
  `texto_resultado`, `texto_efeito`, `texto_pressupostos`, `texto_welch`.

## 4. Os dois QMDs: o que vai em cada um

Os dois documentos têm o **mesmo** primeiro chunk — `executar-analise`, com
`#| include: false`, que declara `here::i_am("relatorios/<arquivo>.qmd")` e
executa `source(here::here("R", "analise.R"), encoding = "UTF-8")`. O corpo
só apresenta objetos; não há `read.csv`, `readRDS` nem `ggsave` nos QMDs.

**O Render exige execução.** Todas as expressões inline dos QMDs leem objetos
que só existem depois do `source()` do chunk `executar-analise`. Renderizar sem
execução (`quarto render --no-execute` ou qualquer modo que pule o código)
produz `object '<nome>' not found` na primeira expressão inline — comportamento
esperado, não um defeito do molde. Os três READMEs do molde avisam o aluno com
essa mesma frase.

### 4.1 Caderno HTML (`relatorio_completo.qmd`)

YAML: tema `[cosmo, ocean.scss]`, banner `#0F3B5F`, TOC com 2 níveis, seções
numeradas, `code-fold`/`code-tools`, `embed-resources`, figuras 7 × 4,6 a 150
dpi, `execute: echo: true`. Os títulos de 1º nível seguem o **padrão
título-pergunta** do barbo (decisão D1 desta rodada, 28/09): "Como usar este
caderno", "Introdução: qual relação queremos investigar?" (adaptada à análise:
"qual comparação queremos investigar?" na ANOVA), "Material e métodos: o que
entrou na análise?", "Exploração: conhecer antes de ajustar" (adaptada),
"Resultados: o que a ANOVA responde?" (adaptada: "o que a reta descreve?" na
regressão, também no padrão do barbo), "Discussão: voltar à
pergunta biológica", "Conclusão", "Reproduzir e adaptar" (com
`## Ambiente computacional`), "Referências". A ordem das seções: **Como usar
este caderno**, **Introdução**, **Material e métodos** (com `## Dados e
preparo` citando `dados/brutos/{{ARQUIVO_BRUTO}}` e `{{ARQUIVO_BASE}}`, e
`## Modelo`), **Exploração**, **Resultados** (que abre com a frase estatística
dinâmica — `texto_anova` — seguida da tabela da ANOVA), **Discussão**,
**Conclusão**, **Reproduzir e adaptar**, **Referências**.

**Regras do molde (decididas pelo professor, 28/09):**

- os **Diagnósticos são subseção (`##`) da Exploração, antes dos Resultados** —
  o aluno vê os grupos, confere os pressupostos e só então lê o teste F. Dentro
  dela, a homogeneidade e a normalidade formam uma única subseção (`###`), cujos
  dois gráficos (`grafico_residuos` e `grafico_qq`, objetos separados no script
  e PNGs separados em `saida/figuras/`) saem **um depois do outro**, cada um no
  seu chunk (`fig-residuos`, `fig-qq`) com o próprio `fig-cap`, seguidos da
  tabela de testes. Os dois chunks usam `#| out-width: "75%"` e
  `#| fig-align: center`, sem `layout-ncol`; as referências cruzadas são
  `@fig-residuos` e `@fig-qq`.
- **os relatórios não exibem saída bruta de console**; ela fica no script
  (`resumo_console`, comentado na seção 5), e os relatórios apresentam os
  resultados em tabelas e frases formatadas.

O barbo e a regressão exportada ainda não seguem essas regras (no barbo,
Diagnósticos é seção de 1º nível com gráficos separados e o console aparece no
caderno; na regressão exportada, idem) e serão ajustados em rodada própria.

Só no HTML: a seção Exploração, os diagnósticos completos, o "Como usar este
caderno", o "Reproduzir e adaptar" e o ambiente computacional. No teste t há
também cercas `.content-visible when-format="html"` com os avisos didáticos
(H0/H1, Welch) — recurso do Quarto que o exportador usa quando a análise pede.

### 4.2 Artigo Word (`relatorio_artigo.qmd`)

YAML: `docx` com `reference-doc: custom-reference.docx`, sem TOC, seções
numeradas, figuras 6 × 4 a 300 dpi. Depois do chunk de execução vem um bloco
`{=html}` com o `GUIA DE EDIÇÃO` em comentário (não aparece no Word): altere
cálculos no script e a argumentação aqui; revise os textos antes de usar como
artigo. Os títulos são os **simples** do artigo do barbo (sem pergunta), e a
ordem das seções é a do barbo: **Introdução**, **Material e métodos**,
**Resultados** (apenas as tabelas e a figura principais, com `tbl-cap`/
`fig-cap`, e os textos `*_artigo` — o recorte de artigo), **Discussão** (com
`alerta_modelo`), **Conclusão** (`texto_sintese_estatistica`),
**Disponibilidade dos dados e do código**, **Referências**.

Só no Word: o recorte enxuto — a tabela-resumo/descritiva, a tabela do teste,
a figura principal, a síntese dos pressupostos e a disponibilidade. As
explicações de como ler cada diagnóstico pertencem ao caderno HTML.

## 5. Marcadores de template

O exportador substitui marcadores `{{CHAVE}}` com `exportacao_preencher_template()`:
uma linha inteira `{{CHAVE}}` é trocada pelo bloco (textos de seção); dentro de
uma linha, o valor entra no lugar. O catálogo por arquivo:

| Arquivo | Marcadores |
|---|---|
| `analise_projeto.R` | `{{TITULO_COMENTARIO}}`, `{{PERGUNTA_COMENTARIO}}`, `{{BIBLIOTECAS_PREPARO}}` (bibliotecas da leitura/preparo, todas do CRAN, na seção 1), `{{TRECHO_IMPORTAR}}` (da planilha a `dados_brutos`, na seção 2), `{{TRECHO_PREPARO}}` (tratamentos, conferência da fotografia e adoção de `dados_da_analise`, na seção 3), variáveis e rótulos (`{{RESPOSTA_R}}`, `{{PREDITOR_R}}`, `{{GRUPO_R}}`, `{{FATOR_R}}`, `{{ROTULO_*_R}}`), `{{CONFIANCA}}`, `{{TITULO_R}}`, `{{EQUACAO}}`, `{{AUTOCORRELACAO}}` (regressão) |
| `relatorio_completo.qmd` / `relatorio_artigo.qmd` | `{{TITULO}}`, `{{INTRODUCAO}}`, `{{METODOS}}`, `{{DISCUSSAO}}`, `{{CONCLUSAO}}`, `{{ARQUIVO_BRUTO}}`, `{{ARQUIVO_BASE}}` |
| `README.md` | `{{TITULO}}`, `{{PROJETO_RPROJ}}`, `{{ARQUIVO_BRUTO}}`, `{{RESPOSTA}}`, `{{PREDITOR}}`/`{{FATOR}}`/`{{GRUPO}}`, `{{IC}}`, `{{PACOTES_INSTALAR}}` |

Regras dos marcadores:

- Variáveis de R (`{{RESPOSTA_R}}` e parentes) recebem `encodeString(..., quote = '"')`.
- `{{CONFIANCA}}` recebe o número com ponto decimal e 15 dígitos.
- `{{TITULO_R}}`/rótulos vazios voltam ao nome da variável (ou `NULL` para
  título de gráfico) — a lógica de "rótulo vazio cai no padrão" mora no gerador.
- Um template nunca chega ao projeto com `{{...}}` sobrando; o teste de
  exportação confere isso.

## 6. Seções autorais e sugestões padrão

`{{INTRODUCAO}}`, `{{METODOS}}`, `{{DISCUSSAO}}` e `{{CONCLUSAO}}` recebem o
texto do autor quando ele preencheu a seção na Comunicação de Resultados;
senão, as **sugestões padrão** de `exportacao_textos_<tipo>()`, que começam com
uma linha em itálico (`*Sugestão de redação: ...*`) orientando o autor. As
sugestões **não inferem** local, período, unidade amostral, causalidade nem
licença dos dados; só descrevem o método e o contexto pesqueiro genérico.
Exceção registrada: a regressão com o dataset `morfometria_barbo` (pacote)
recebe parágrafos específicos do barbo, inclusive na conclusão padrão.

## 7. Como adicionar uma análise ao molde

Depois da T2 (gerador único), adicionar uma análise são **dois passos**:

1. **Pasta de templates** `inst/app/templates/<analise>/` com
   `analise_projeto.R` (cabeçalho + seções numeradas da seção 2 deste
   contrato), `relatorio_completo.qmd`, `relatorio_artigo.qmd` e `README.md`,
   usando o catálogo de marcadores da seção 5.
2. **Uma entrada no registro do gerador** `molde_projeto_registro` (em
   `exportacao_comunicacao.R`), com: o **seletor** (quando o manifesto usa a
   árvore nova — hoje: uma única execução incluída do tipo), a **pasta** de
   templates, a **pasta de apoio** (de onde vêm `funcoes.R`, `apa.csl` e
   `_quarto.yml` — hoje a da regressão), o **prefixo** do script (reaproveitar
   o exportador geral, como a regressão, ou montar peça por peça com
   `exportacao_molde_projeto_prefixo_preparo`) e as três **tabelas de
   marcadores**: do script (`exportacao_<tipo>_marcadores_script`), dos QMDs
   (`exportacao_<tipo>_marcadores_qmd`, que usa `exportacao_textos_<tipo>` para
   as sugestões padrão) e do README (`exportacao_<tipo>_marcadores_readme`).

O gerador único (`exportacao_molde_projeto_entrada/script/qmd/readme`) cuida do
resto: escolha da entrada pelo seletor, preenchimento de marcadores, gravação
do script, dos dois QMDs e do README, e a cópia dos arquivos de apoio
(`funcoes.R`, `apa.csl`, `_quarto.yml`, `custom-reference.docx`, `ocean.scss`,
`referencias.bib`). Sem tocar no resto do exportador.

## 8. O que não muda

- A CatalyseR **não gera o Word** nos módulos: o Projeto R nasce só na
  Comunicação de Resultados, e o Word nasce no Render, no RStudio do pesquisador.
- O molde novo **prevalece** sobre a árvore legada para as três análises
  migradas; análises não migradas seguem o exportador geral, sem alteração.
- Nenhuma alteração de apresentação (rótulos, títulos, tema, CSL) pode mudar
  os cálculos: os números do script exportado são os mesmos da CatalyseR.
