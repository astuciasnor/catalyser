# Módulo "Comunicação de Resultados" — relatório integrado (.docx)

> Spec de trabalho. Nome do módulo/menu: **Comunicação de Resultados** (o
> `CLAUDE.md` já usa este termo para o entregável v1; evita colisão com o
> "Projeto R" `.zip` exportado). Arquivo do módulo: `inst/app/modules/mod_comunicacao.R`.

> **Estado em julho de 2026:** a Fase 3C implementou o registro explícito por
> execução nos oito módulos prioritários. O estúdio conjunto, a ordenação e a
> seleção do conteúdo do Word entram na Fase 3D; a exportação integrada entra
> na Fase 3E. O consolidado antigo permanece isolado até essa substituição.

## 1. Objetivo

Reunir, num **único documento `.docx`** (tema Ocean), várias análises feitas na
IDE — na ordem que o usuário montar — cada análise (e sub-análise) como **seção
própria**, com o mesmo motor Quarto→Word que já roda por análise. Gera também o
**script R integrador** espelhando essas seções.

## 2. Fundação que já existe (reaproveitar, não reinventar)

- **Motor docx in-app**: `render_relatorio_docx()` em `inst/app/modules/utils_export.R`
  monta um tempdir com `template.qmd` + `custom-reference.docx` + `dados_limpos.rda`
  + `funcoes_*.R` e roda `system2("quarto", c("render", qmd, "--to", "docx"))`.
  Cada módulo já chama isso (mod_parametric, mod_regression, mod_anova, …).
- **Templates `.qmd` por análise** em `templates/relatorio_*.qmd` (params + tema
  Ocean via `reference-doc: custom-reference.docx`, `number-sections`, `toc`).
- **Funções canônicas** `templates/funcoes_*.R` (ex.: `relatar_teste_t()`,
  `mostrar_teste_t()`, `flextable_ocean()`).
- **Consolidado atual** (`download_consolidated_zip` no `app.R`, ~1400 linhas):
  já tem a IDEIA (scripts numerados + `qmd_sections[[chave]]` concatenados), mas
  a ordem/lista está **hardcoded em 3 lugares**, marca "usei" só por *visitar* a
  aba, e **não renderiza docx** (só entrega o `.qmd`). É o que vamos superar.

## 3. Decisões travadas

1. **Granularidade = fila por execução.** Botão "Adicionar ao relatório" em cada
   análise; aceita a MESMA análise várias vezes (ex.: 3 testes-*t*), cada uma sua
   subseção, na ordem adicionada.
2. **Estrutura = esqueleto de artigo + Preparação.** Seção 0 (Preparação dos
   dados) → Introdução/Métodos globais → Resultados (uma (sub)seção por item da
   fila) → Discussão/Conclusão globais.
3. **Geradores = MVP reaproveita, refatora depois.** 1ª versão usa os builders que
   já existem; fase 2 migra cada `gerar_script_*`/`gerar_qmd_*` para o EAPADados.
4. **Ordem = reordenável** (setas subir/descer), começando na ordem do menu.

## 4. Arquitetura

### 4.1 Registry canônico (uma fonte de ordem)

Uma lista ordenada — a **ordem do menu da IDE** — em que cada entrada descreve uma
análise disponível:

```r
# registro_analises.R (fonte única de ordem + rótulos + geradores)
registro_analises <- list(
  list(id = "descr_stats", titulo = "Estatística descritiva", menu = "Descrevendo Dados",
       nivel = 1, add_id = "add_descr", gerar_script = ..., gerar_qmd = ...),
  list(id = "boxplot", titulo = "Boxplot", menu = "Descrevendo Dados", nivel = 1, ...),
  list(id = "regressao", titulo = "Regressão linear simples", menu = "Regressão", nivel = 1, ...),
  list(id = "nl_von_bertalanffy", titulo = "Crescimento de peixes (von Bertalanffy)",
       menu = "Regressão não linear", nivel = 2, ...),
  ...
)
```

Esse registry passa a dirigir **de uma vez**: os botões "Adicionar", a numeração
dos scripts, a ordem-padrão da fila e o nível de cabeçalho no docx. Elimina as
três duplicações de hoje.

### 4.2 Contrato de cada análise

Cada módulo de análise expõe um **estado de relatório** padronizado (uma função
que captura a config atual):

```r
estado_relatorio() -> list(
  id       = "parametric",
  titulo   = "Teste t (comp_cm ~ sexo)",   # já com as variáveis
  nivel    = 1,                              # 1 = seção, 2 = subseção
  params   = list(var_y = "comp_cm", var_x = "sexo", ...),
  dataset_entrada = "base",                   # HOOK p/ ramos: padrão = base compartilhada;
                                               # futuro: "base_reg_logistica" etc.
  script   = "<código R desta análise>",     # string
  qmd      = "<corpo .qmd SEM o título>"      # string, headings só internos ≥ nível+1
)
```

Regra de ouro do contrato: **o builder devolve o CORPO sem o título de topo**; o
módulo integrador injeta o cabeçalho (`#` ou `##`) no nível certo — assim a
numeração do documento fica coerente (hoje os `##` embutidos brigam entre si).

### 4.3 A fila do relatório

`fila_rv <- reactiveVal(list())` — cada item é um `estado_relatorio()` congelado no
momento do clique "Adicionar ao relatório". Suporta:
- repetição (mesmo `id`, params diferentes);
- reordenar (subir/descer);
- remover;
- editar o título da (sub)seção.

## 5. UI do módulo (3 colunas, padrão da IDE)

- **Coluna 1 — Fila**: lista dos itens adicionados, com nível indentado, setas
  ↑/↓, remover, e campo de título editável. Botão "Limpar fila".
- **Coluna 2 — Prévia do sumário (outline)**: árvore numerada (1, 1.1, 2, …) do
  documento que será gerado + prévia do `.qmd`/script integrador em abas.
- **Coluna 3 — Saída**: formato (docx [padrão], html, typst/pdf), campos de
  Introdução/Métodos/Discussão globais (textareas), botão **Gerar .docx**,
  download do `.qmd` e do script integrador, e "Usar dados ativos".

O botão "Adicionar ao relatório" vive **em cada análise** (não só aqui), enviando
`estado_relatorio()` para a `fila_rv` via callback — igual ao `on_usar` do Arrumar.

## 6. Montagem do `.qmd` integrado

### 6.1 Cabeçalho único (Ocean)

Um só YAML: `format: docx: {reference-doc: custom-reference.docx, toc: true,
toc-depth: 3, number-sections: true}`. Nada de `params:` (o consolidado é
self-contained: valores **inline** em cada seção). Um `setup` chunk carrega
`dados_limpos.rda` e as funções (EAPADados ou `source()` de rede de segurança).

### 6.1.1 Invariante das duas fases (preparo antes das análises)

O documento tem SEMPRE duas fases nesta ordem: (1) **Preparação dos dados** — todo
o bloco de preparo (a trilha, ver `EVOLUCAO_TRATAMENTO_DADOS.md` §6.6), com os
datasets nomeados emitidos em ordem de dependência (topológica); (2) **Análises** —
cada item da fila declara qual dataset nomeado consome. A ordenação/reordenação da
FILA de análises nunca pode empurrar uma análise para antes do seu preparo — o
gerador coloca a Fase 1 inteira antes da Fase 2, por estrutura. Preparo específico
de uma análise entra como **ramo nomeado** na Fase 1, não intercalado (guarda
contra p-hacking).

### 6.2 Hierarquia e numeração

- Seção 0: `# Preparação dos dados` (unnumbered? ou numerada — decidir).
- Introdução/Métodos: `#` globais.
- Resultados: `# Resultados`, e cada item da fila vira `##` (ou `###` se sub-análise).
- Discussão/Conclusão: `#` globais.

O nível de cada item sai do `nivel` do registry + posição na fila.

### 6.3 Namespacing de labels (gotcha crítico)

Concatenar seções gera **labels de chunk/tabela/figura duplicados** → Quarto
quebra. O montador reescreve todo `#| label: tbl-x` / `fig-x` e as refs `@tbl-x`
para `...-s<i>` (índice da seção). Função pura `renomear_labels(qmd, sufixo)`
testável e espelhável no livro.

### 6.4 Isolamento de erro

Cada chunk de análise recebe `#| error: true` (ou o corpo é embrulhado em
`tryCatch`) — uma seção que falha imprime o erro mas **não derruba** o documento.

## 7. Script R integrador

Percorre a fila na ordem final e emite:

```r
# ============================================================
# Seção 1 — Estatística descritiva
# ============================================================
<script da análise>

# ============================================================
# Seção 2 — Regressão linear simples
#   2.1 — (sub-análise, se houver)
# ============================================================
<script...>
```

Cabeçalho comum (leitura de dados conforme a fonte, como no `calc_gerar_codigo`) e
as seções encadeadas. Sub-análises = subcomentário `#   N.M —`.

## 8. Render docx (generalizar `render_relatorio_docx`)

Novo `render_relatorio_integrado(file, qmd_lines, df_clean, extras)`:
1. Detectar Quarto: `quarto --version` via `system2(..., stdout=TRUE)`; se ausente,
   **degradar** — entregar o `.qmd` + `custom-reference.docx` num `.zip` com
   instruções, em vez de falhar.
2. Montar tempdir: `dados_limpos.rda` (snapshot do **dataset ativo**, `dados_analise`),
   `custom-reference.docx`, funções necessárias (união das análises da fila),
   figuras estáticas se houver.
3. `withProgress()` durante o render (pode demorar com muitas seções).
4. `setwd(tempdir)` + `system2("quarto", c("render", "relatorio.qmd", "--to", "docx"))`;
   copiar o `.docx` gerado para `file`. Tratar caminho do Quarto no Windows.

## 9. Relação com o que já existe

- **Substitui** gradualmente o `download_consolidated_zip` (o consolidado vira um
  caso particular: "adicionar tudo na ordem do menu"). Manter o `.zip` como opção
  de saída (Projeto R para o aluno) além do `.docx`.
- **Coexiste** com os relatórios individuais por análise (continuam úteis para
  foco em uma análise só).

## 10. Plano em fases

- **Fase 0 — Registry + contrato.** Criar `registro_analises` e definir
  `estado_relatorio()` em 2–3 módulos-piloto (descritiva, teste t, regressão).
- **Fase 1 — Módulo + fila + outline.** UI, `fila_rv`, reordenar, prévia do sumário.
- **Fase 2 — Montador do `.qmd`** com namespacing de labels + hierarquia + esqueleto
  de artigo + Seção 0 (Preparação).
- **Fase 3 — Render docx integrado** (generalizar helper) + degradação sem Quarto.
- **Fase 4 — Cobrir todas as análises** (migrar os builders do `app.R`).
- **Fase 5 — Entrosamento**: mover `gerar_script_*`/`gerar_qmd_*` para o EAPADados;
  o livro passa a derivar dos mesmos geradores; remover as ~1400 linhas do `app.R`.

## 11. Riscos / itens abertos

- **Labels duplicados** (§6.3) — resolver cedo, é o que mais quebra o render.
- **Pacotes por análise** ausentes na máquina do aluno → `#| error: true` + nota.
- **Dataset único**: o relatório assume um dataset ativo; se o usuário promoveu
  datasets diferentes entre análises, avisar/registrar qual foi usado por seção.
- **Seção 0 (Preparação)** depende de capturar o script do Arrumar/Calcular — ver
  a discussão de "evolução do tratamento de dados" (as etapas de preparo precisam
  ser um objeto capturável, não só efeito colateral na UI).
- **Numeração de tabelas/figuras** global vs por seção — definir.
- **Tempo de render** com muitas seções — progress + possível render assíncrono.

## 12. Entrosamento com o livro

Quando os geradores viverem no EAPADados (Fase 5), o capítulo "Comunicação de
Resultados" do livro mostra o MESMO `.qmd`/script que a IDE emite — fecha o "do
mouse ao código" também na etapa de comunicação.

## 13. Menu na IDE (implementado — casca / Fase 0)

Decisão (jul/2026): o menu **"Estatísticas Avançadas"** (que só continha Séries
Temporais) foi **renomeado para "Comunicação de Resultados"** e recebeu a **casca**
do estúdio de montagem (`mod_comunicacao.R`): 3 colunas — Fila do relatório (vazia),
Esboço do documento / Código gerado, e Saída (formato .docx/.zip + botão "Gerar",
por ora em construção). É a ponte visível **Mouse → Código → Relatório**, saindo como
projeto. **Séries Temporais** foi movida temporariamente para **Modelos de Regressão**.
Próximo: preencher a casca com a fila real (contrato `estado_relatorio()` por análise),
o outline com namespacing de labels e a geração docx (generalizar `render_relatorio_docx`).
