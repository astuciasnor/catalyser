# Módulo "Comunicação de Resultados" — relatório integrado (.docx)

> Spec de trabalho. Nome do módulo/menu: **Comunicação de Resultados** (o
> `CLAUDE.md` já usa este termo para o entregável v1; evita colisão com o
> "Projeto R" `.zip` exportado). Arquivo do módulo: `inst/app/modules/mod_comunicacao.R`.

> **Estado em julho de 2026:** as Fases 3C/3C.1 implementaram o registro e a
> execução analítica explícita; a Fase 3D conectou o registro ao estúdio; e a
> **Fase 3E implementou a exportação integrada do Word e do Projeto R**. O
> consolidado antigo permanece internamente durante a migração, mas deixou de
> ser oferecido na interface.

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

1. **Granularidade = registro por execução.** O botão **Adicionar aos resultados**
   aceita a MESMA análise várias vezes (ex.: 3 testes-*t*), cada uma com ID,
   base, parâmetros e título próprios.
2. **Estrutura = esqueleto de artigo + Preparação.** Seção 0 (Preparação dos
   dados) → Introdução/Métodos globais → Resultados (uma (sub)seção por item da
   fila) → Discussão/Conclusão globais.
3. **Geradores = MVP reaproveita, refatora depois.** 1ª versão usa os builders que
   já existem; fase 2 migra cada `gerar_script_*`/`gerar_qmd_*` para o EAPADados.
4. **Ordem = reordenável** (setas subir/descer), começando na ordem em que as
   execuções foram registradas.
5. **Projeto R ≠ Word.** O Projeto R preserva todas as execuções registradas; o
   Word mostra somente as execuções e componentes escolhidos no estúdio.

## 4. Arquitetura

### 4.1 Registro canônico de execuções (implementado)

`registro_execucoes_rv` é a fonte da verdade do que foi executado e preservado.
Ele é preenchido somente pelo clique **Adicionar aos resultados**, aceita
repetições e guarda um objeto leve por execução. O estúdio não copia esse
registro: sincroniza sua ordem editorial pelos IDs existentes.

O replay da Fase 3E é centralizado em
`templates/funcoes_projeto_integrado.R` e despachado pelo campo `tipo`. Ele cobre
os oito módulos prioritários e mantém uma camada de cálculo separada da camada
de apresentação do Word.

### 4.2 Contrato de cada análise

Cada módulo prioritário expõe um **estado de execução** padronizado:

```r
estado_execucao() -> list(
  analise_id = "parametric",
  tipo = "teste_t_independente",
  titulo = "Teste t: comp_cm por sexo",
  parametros = list(resposta = "comp_cm", grupo = "sexo", ...),
  saidas_disponiveis = c("narrativa", "tabela", "grafico", "pressupostos"),
  resultado_resumo = list(...),
  codigo_r = "<código R desta análise>"
)
```

O registrador acrescenta ID da execução, base consumida, revisão e versão. Na
3E, o builder do `.qmd` deve devolver o CORPO sem título de topo; o integrador
injeta o cabeçalho no nível certo.

### 4.3 Estado editorial do relatório

`estado_editorial_rv` não contém resultados, dados, modelos nem gráficos. Guarda
somente a ordem dos IDs, `incluir_word` e `saidas_selecionadas`. Novas execuções
entram no fim; execuções removidas no módulo analítico desaparecem; atualizações
preservam escolhas ainda válidas.

## 5. UI do módulo (3 colunas, padrão da IDE)

- **Coluna 1 — Execuções e bases**: lista ordenável com base, dependência,
  inclusão no Word e componentes disponíveis; abaixo, lista da base
  compartilhada e de todos os ramos derivados.
- **Coluna 2 — Esboço e manifesto**: estrutura do futuro documento e registro
  textual das decisões editoriais.
- **Coluna 3 — Seções globais e saída**: Introdução, Métodos, Discussão,
  Conclusão, contadores Word/Projeto R e downloads independentes `.docx`/`.zip`.

O registro é feito dentro de cada análise. A Comunicação apenas organiza e
seleciona; remover uma execução continua sendo uma ação do módulo analítico.

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

Implementado em `modules/exportacao_comunicacao.R`:

1. valida o manifesto e bloqueia execuções desatualizadas;
2. monta um diretório temporário autocontido com dados, scripts, metadados,
   `relatorio.qmd` e `custom-reference.docx`;
3. disponibiliza o `.zip` mesmo sem Quarto;
4. quando o Quarto está presente, renderiza o `.docx` e só entrega o arquivo se
   o processo terminar com sucesso.

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

## 13. Menu na IDE (Fase 3D implementada)

Decisão (jul/2026): o menu **"Estatísticas Avançadas"** (que só continha Séries
Temporais) foi **renomeado para "Comunicação de Resultados"**. O estúdio
`mod_comunicacao.R` usa três colunas: execuções registradas, esboço/manifesto e
seções globais/saída planejada. É a ponte visível **Mouse → Código → Relatório**.
**Séries Temporais** foi movida temporariamente para **Modelos de Regressão**.

Na Fase 3D, o estúdio passou a consumir o `registro_execucoes_rv` implementado na
Fase 3C. Esse registro já cumpre o papel do antigo `estado_relatorio()`: preserva
base, parâmetros, saídas disponíveis, resumo e código R sem duplicar dados ou
modelos. O estúdio sincroniza novas execuções, remove referências excluídas,
preserva escolhas existentes, permite ordenar e produz um manifesto editorial.

O Word recebe somente os itens e componentes selecionados. O Projeto R mantém
todas as execuções registradas. A Fase 3E gera labels a partir do ID de cada
execução, evitando colisões, e oculta o exportador antigo por visita à aba.
