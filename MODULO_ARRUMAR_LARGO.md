# Proposta de arquitetura — Módulo "Arrumar (Largo → Longo)" na CatalyseR

> Módulo genérico de *reshape* wide→tidy com extração de metadados do nome das
> colunas, 100% no mouse, que **gera o script `.R`** ao final. Encaixa na *etapa de
> preparo compartilhada* (importar → arrumar → filtrar → tipar → recodificar) e no
> livro na Unidade II · "Antes da análise". É a materialização do "do mouse ao código"
> para a fase de arrumação.

Baseado no script `preparar-dados-tidy.R` e no dataset de exportação (Comex),
mas **sem nada hardcoded**: o usuário define colunas, regex e saída. O caso do
Comex (`YYYY - Valor US$ FOB` / `YYYY - Quilograma Líquido`) vira apenas uma
**pré-configuração sugerida**, não uma regra fixa.

---

## 0. Modelo mental do fluxo

Espelha o script, mas moderniza a extração (usa `names_pattern` do `pivot_longer`
em vez de `str_extract` em `mutate` separado — menos passos, mesmo resultado):

```
[dados largos]
   │  (1) usuário marca IDENTIFICADORES × MEDIDAS   (auto-detecção sugere)
   ▼
pivot_longer(cols = medidas,
             names_to   = c("ano","metrica"),   ← nomes que o usuário define
             names_pattern = "^(\\d{4}) - (.*)$", ← regex do usuário (ou preset)
             values_to  = "valor",
             values_transform = list(valor = as.numeric))
   │  (2) → formato longo já com metadados extraídos
   ▼
[opcional] pivot_wider(names_from = metrica, values_from = valor)
   │  (3) → uma coluna por métrica (valor_usd, massa_kg, …)
   ▼
[prévia + download do .R + download dos dados arrumados]
```

Decisão de design central: **uma única fonte da verdade — a configuração
(`reactiveValues`)**. Dela derivam *ao mesmo tempo* (a) os dados transformados
(prévia) e (b) o texto do script. Assim a prévia e o `.R` baixado **nunca
divergem** — é a mesma regra do ecossistema (`arrumar_` embaixo, `exibir_` em cima).

---

## 1. Widgets Shiny (UI) — assistente em 4 passos

Layout em `navset_tab` (ou cards revelados progressivamente). Cada passo tem uma
prévia parcial à direita (`DTOutput`), então o usuário **vê o efeito de cada clique**.

### Passo 1 — Identificadores × Medidas
```r
# Detecção automática sugere as medidas por heurística (4 dígitos no início)
actionButton(ns("auto_detectar"), "🔍 Detectar colunas de medida automaticamente",
             class = "btn-outline-primary"),
helpText("Sugerimos como 'medida' colunas que começam com 4 dígitos (ex.: '2025 - ...')."),

selectizeInput(ns("cols_medida"), "Colunas de MEDIDA (serão empilhadas):",
               choices = NULL, multiple = TRUE,
               options = list(placeholder = "clique para escolher…", plugins = list("remove_button"))),
# Os identificadores são o complemento automático (como o setdiff do script),
# mostrados apenas para conferência:
verbatimTextOutput(ns("cols_id_preview"))
```
Racional: pedir só as **medidas** e derivar os IDs por `setdiff` é o menor esforço de
mouse (o script faz exatamente isso). O botão de auto-detecção pré-preenche as medidas.

### Passo 2 — Extrair metadados do nome da coluna
```r
selectInput(ns("regex_preset"), "Padrão de extração:",
  choices = c(
    "Ano + métrica  →  ^(\\d{4}) - (.*)$"      = "ano_metrica",
    "Métrica + ano  →  ^(.*) - (\\d{4})$"      = "metrica_ano",
    "Separar por hífen ' - '"                  = "hifen",
    "Personalizado (escrever regex)"           = "custom")),

# aparece só quando preset == "custom"
conditionalPanel(sprintf("input['%s'] == 'custom'", ns("regex_preset")),
  textInput(ns("regex"), "Regex com grupos de captura ( ):",
            value = "^(\\d{4}) - (.*)$"),
  helpText("Cada parêntese ( ) captura um pedaço → vira uma coluna nova.")),

textInput(ns("novas_cols"), "Nomes das colunas novas (na ordem dos grupos):",
          value = "ano, metrica"),
textInput(ns("values_to"), "Nome da coluna de valores:", value = "valor"),
checkboxInput(ns("como_numero"), "Converter valores para número", value = TRUE)
```

### Passo 3 — Formato de saída
```r
radioButtons(ns("saida"), "Formato final:",
  c("Manter em formato LONGO (uma linha por observação)" = "longo",
    "ALARGAR uma métrica em colunas (pivot_wider)"        = "largo")),

conditionalPanel(sprintf("input['%s'] == 'largo'", ns("saida")),
  selectInput(ns("wider_names"), "Coluna que vira nomes de colunas:", choices = NULL), # ex.: 'metrica'
  helpText("Ex.: 'metrica' → cria colunas 'valor_usd', 'massa_kg'."))
```

### Passo 4 — Aplicar / Baixar
```r
actionButton(ns("aplicar"), "▶ Aplicar transformação", class = "btn-primary"),
actionButton(ns("desfazer"), "↩ Desfazer último passo", class = "btn-outline-secondary"),
downloadButton(ns("baixar_script"), "⬇ Baixar script .R"),
downloadButton(ns("baixar_dados"),  "⬇ Baixar dados arrumados (.xlsx)"),

# Painéis de conferência
card(card_header("Antes"),  DTOutput(ns("preview_antes"))),
card(card_header("Depois"), DTOutput(ns("preview_depois"))),
card(card_header("Script gerado"), verbatimTextOutput(ns("script_preview")))
```

Resumo dos tipos de widget pedidos: `selectizeInput`/`selectInput` (colunas e
presets), `checkboxInput` (numérico), `textInput` (regex, nomes), `radioButtons`
(saída), `actionButton` (auto-detectar, aplicar, desfazer) e `downloadButton`
(script + dados). Nenhuma digitação de código R — no máximo uma regex opcional.

---

## 2. Lógica reativa (Server) — pipeline passo a passo em `reactiveValues`

Duas peças: um **acumulador de passos** (para a prévia incremental que o usuário pediu)
e um **construtor** que, da mesma config, produz *dados* e *código*.

### 2.1 Estado
```r
mod_arrumar_server <- function(id, data_rv, import_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Pilha de passos aplicados. Cada passo guarda: rótulo, código (texto) e a
    # função que transforma o data.frame. É a "fonte da verdade" do pipeline.
    rv <- reactiveValues(steps = list())

    base_data <- reactive({ req(data_rv()); data_rv() })

    # Popular selects quando os dados chegam
    observeEvent(base_data(), {
      updateSelectizeInput(session, "cols_medida", choices = names(base_data()), server = TRUE)
    })

    # Auto-detecção (heurística: começa com 4 dígitos)
    observeEvent(input$auto_detectar, {
      med <- grep("^\\d{4}", names(base_data()), value = TRUE)
      updateSelectizeInput(session, "cols_medida", selected = med)
    })

    # IDs = complemento (como o setdiff do script)
    cols_id <- reactive(setdiff(names(base_data()), input$cols_medida))
    output$cols_id_preview <- renderText(
      paste("Identificadores (automático):", paste(cols_id(), collapse = ", ")))
```

### 2.2 Aplicar um passo = empilhar na pilha
```r
    observeEvent(input$aplicar, {
      cfg <- coletar_config(input)          # lê os widgets numa lista (ver §3)
      val <- validar_config(cfg, base_data())  # devolve NULL se ok, ou msg de erro
      if (!is.null(val)) { showNotification(val, type = "error"); return() }

      passo <- construir_passo(cfg)         # list(rotulo, codigo, fun)
      rv$steps <- c(rv$steps, list(passo))  # empilha
    })

    observeEvent(input$desfazer, {
      n <- length(rv$steps); if (n > 0) rv$steps <- rv$steps[-n]
    })
```

### 2.3 Dobrar a pilha → dados atuais (prévia reage a cada clique)
```r
    dados_atual <- reactive({
      Reduce(function(df, passo) passo$fun(df), rv$steps, init = base_data())
    })

    output$preview_antes  <- renderDT(head(base_data(), 50))
    output$preview_depois <- renderDT({
      validate(need(length(rv$steps) > 0, "Aplique uma transformação para ver o resultado."))
      head(dados_atual(), 50)
    })
```

### 2.4 Script = mesma pilha, só que texto
```r
    script_texto <- reactive({
      cabecalho <- c("# Script gerado pela CatalyseR — arrumação largo → longo",
                     "library(tidyverse)", "library(readxl)", "",
                     'dados_largo <- read_excel("SEU_ARQUIVO.xlsx", sheet = "Resultado")', "")
      corpo <- vapply(rv$steps, function(p) p$codigo, character(1))
      paste(c(cabecalho, corpo), collapse = "\n")
    })
    output$script_preview <- renderText(script_texto())
```

> Por que `Reduce`/pilha e não um único bloco fixo? Porque o usuário pediu ver a
> prévia **a cada clique** e poder **desfazer**. A pilha dá isso de graça: cada passo
> é um par (função, texto) e tanto os dados quanto o script são a *dobra* da mesma
> lista — impossível a prévia e o `.R` discordarem.

---

## 3. Geração do script final (concatenar código como string)

`construir_passo(cfg)` devolve **a função e o texto juntos**, garantindo paridade.
Exemplo do passo `pivot_longer` (o coração):

```r
construir_passo_pivot_longer <- function(cfg) {
  novas <- trimws(strsplit(cfg$novas_cols, ",")[[1]])
  names_to_txt <- paste(sprintf('"%s"', novas), collapse = ", ")

  # >>> DETALHE CRÍTICO: escapar as barras da regex ao virar texto de código <<<
  # A regex "^(\d{4}) - (.*)$" precisa aparecer no .R como "^(\\d{4}) - (.*)$"
  regex_txt <- gsub("\\", "\\\\", cfg$regex, fixed = TRUE)

  vt <- if (cfg$como_numero) ',\n    values_transform = list(%s = as.numeric)' else ''
  codigo <- sprintf(
'dados_long <- dados_largo %%>%%
  pivot_longer(
    cols = all_of(c(%s)),
    names_to = c(%s),
    names_pattern = "%s",
    values_to = "%s"%s
  )',
    paste(sprintf('"%s"', cfg$cols_medida), collapse = ", "),
    names_to_txt, regex_txt, cfg$values_to,
    if (cfg$como_numero) sprintf(',\n    values_transform = list(%s = as.numeric)', cfg$values_to) else '')

  fun <- function(df) {
    args <- list(df, cols = tidyselect::all_of(cfg$cols_medida),
                 names_to = novas, names_pattern = cfg$regex, values_to = cfg$values_to)
    if (cfg$como_numero) args$values_transform <- setNames(list(as.numeric), cfg$values_to)
    do.call(tidyr::pivot_longer, args)
  }
  list(rotulo = "Empilhar colunas (pivot_longer)", codigo = codigo, fun = fun)
}
```

Passo opcional `pivot_wider`:
```r
codigo <- sprintf(
'dados_final <- dados_long %%>%%
  pivot_wider(names_from = %s, values_from = %s)', cfg$wider_names, cfg$values_to)
fun <- function(df) tidyr::pivot_wider(df, names_from = cfg$wider_names, values_from = cfg$values_to)
```

Download:
```r
output$baixar_script <- downloadHandler(
  filename = function() paste0("arrumar_dados_", Sys.Date(), ".R"),
  content  = function(file) writeLines(script_texto(), file)
)
output$baixar_dados <- downloadHandler(
  filename = function() paste0("dados_arrumados_", Sys.Date(), ".xlsx"),
  content  = function(file) writexl::write_xlsx(dados_atual(), file)  # ou export_to_xlsx()
)
```

O `.R` resultante é **auto-suficiente e reproduzível** — abre no RStudio e roda,
fechando o ciclo "do mouse ao código". (Pode-se reaproveitar `exportar_projeto_zip()`
de `utils_export.R` para entregar um projeto completo dados/scripts, se quiser.)

### Nota sobre a barra invertida (o erro nº 1 deste tipo de gerador)
A regex vive **duas vezes** em contextos diferentes:
- na *reatividade* (aplicada com `str_*`/`pivot_longer`) ela é uma string R normal:
  `"^(\\d{4}) - (.*)$"` (o usuário digita `\d`, o R guarda `\d`);
- no *texto do script gerado* ela precisa aparecer literalmente como `\\d`, senão
  o `.R` baixado quebra. Por isso o `gsub("\\", "\\\\", regex, fixed = TRUE)` acima.
Testar sempre gerando o `.R`, salvando e rodando — não só a prévia.

---

## 4. Tratamento de erros (`validar_config` + guardas)

Validar **antes** de aplicar e dar mensagem clara (`showNotification`/`validate`):

```r
validar_config <- function(cfg, df) {
  # (a) pelo menos uma medida
  if (length(cfg$cols_medida) == 0)
    return("Selecione ao menos uma coluna de medida para empilhar.")

  # (b) colunas existem
  faltando <- setdiff(cfg$cols_medida, names(df))
  if (length(faltando))
    return(paste("Colunas não encontradas:", paste(faltando, collapse = ", ")))

  # (c) regex compila?
  ok <- tryCatch({ grepl(cfg$regex, "teste"); TRUE }, error = function(e) FALSE)
  if (!ok) return("Regex inválida — verifique os parênteses e as barras invertidas.")

  # (d) nº de grupos de captura == nº de nomes novos
  novas <- trimws(strsplit(cfg$novas_cols, ",")[[1]])
  n_grupos <- length(attr(regexpr(cfg$regex, "x", perl = TRUE), "capture.start")) # ou contar "("
  n_grupos <- length(gregexpr("(?<!\\\\)\\((?!\\?)", cfg$regex, perl = TRUE)[[1]])  # grupos de captura
  if (n_grupos != length(novas))
    return(sprintf("A regex tem %d grupo(s) de captura, mas você nomeou %d coluna(s).",
                   n_grupos, length(novas)))
  NULL
}
```

Outros guardas, no momento de aplicar (`tryCatch` em volta de `pivot_longer`/`wider`):

| Risco | Sintoma | Tratamento |
|---|---|---|
| **Valores não numéricos** | `values_transform = as.numeric` gera `NA` por coerção | Contar `NA` introduzidos e avisar: *"X valores não puderam virar número"*; deixar o usuário desligar "Converter para número" e manter texto. |
| **`pivot_wider` com chaves duplicadas** | aviso *"values are not uniquely identified"* e colunas-lista | Detectar duplicação nas chaves; oferecer `values_fn` (soma / primeiro) num `selectInput`; mensagem explicando que faltam identificadores para unicidade. |
| **Regex não casa nenhuma coluna** | colunas novas todas `NA` | Após aplicar, checar se a coluna extraída é toda `NA` → avisar *"O padrão não casou com os nomes das colunas."* |
| **Tipos mistos ao empilhar** | `pivot_longer` vira `list`/`character` | Sem `values_transform`, deixar caractere e avisar; com ele, coagir e reportar coerção. |
| **Erro inesperado** | qualquer exceção | `tryCatch(..., error = \(e) showNotification(conditionValue(e), type = "error"))`; a prévia não trava (usa `validate(need())`). |

Princípio: **nunca deixar a IDE travar** — toda transformação roda dentro de
`tryCatch`, o erro vira notificação amigável em PT-BR, e a pilha de passos só é
alterada se a validação passou (o passo inválido nunca entra em `rv$steps`).

---

## 5. Encaixe no ecossistema (para decidir depois)

- **Menu:** entra num grupo "Preparar dados" / "Arrumar" (a etapa compartilhada do
  CLAUDE.md: importar → **arrumar** → filtrar → tipar → recodificar).
- **Livro:** exemplo natural para a Unidade II · "Antes da análise" — mostra que uma
  planilha *tidy* chega pronta ao painel de seleção/tipagem sem retrabalho, e que a
  arrumação também "vira código".
- **Escopo v1:** avaliar. É um módulo novo (não estava na lista de análises dominadas).
  Se for pesado, registrar no BACKLOG como candidato a v1.x. A base já está desenhada.
- **Reuso:** `baixar_dados` pode chamar `export_to_xlsx()`; um "Baixar projeto (.zip)"
  pode reusar `exportar_projeto_zip()` de `utils_export.R`.

---

## 6. Pré-configuração sugerida para o caso Comex (exemplo, não hardcode)

Ao detectar cabeçalhos que casam `^\\d{4} - `, o módulo **sugere** (mas não impõe):

- medidas = todas as colunas `^\\d{4} - .*`;
- preset de regex = `^(\\d{4}) - (.*)$`;
- novas colunas = `ano, metrica`;
- saída = "alargar" com `names_from = metrica` → gera `Valor US$ FOB` e
  `Quilograma Líquido` como colunas (o usuário renomeia se quiser `valor_usd`,
  `massa_kg`).

O usuário aceita a sugestão com um clique ou ajusta tudo — exatamente o ponto de
"genérico com bons atalhos".

---

Próximo passo sugerido: se aprovar esta arquitetura, implemento `mod_arrumar.R` +
o wiring no `app.R`, começando pela pré-config do Comex como teste real.
