# Evolução do tratamento de dados — o pipeline de preparação capturável

> Spec de trabalho. Companheira de `MODULO_COMUNICACAO_RESULTADOS.md` (a Seção 0
> "Preparação dos dados" do relatório é gerada por este pipeline). Módulo/painel:
> **Tratar Dados** / "Trilha de preparo". Arquivo: `inst/app/modules/mod_tratar.R`
> + `inst/app/modules/registro_tratamentos.R`.

## 1. A ideia em uma frase

Trocar as várias "ilhas" de preparo (cada uma com sua pilha e seu código) por
**uma trilha única, ordenada e capturável** de etapas — a fonte da verdade do
tratamento de dados, da importação até o dataset pronto para análise.

Disso saem três ganhos que hoje custam caro:

1. `dados_analise` = **replay** determinístico do pipeline sobre os dados brutos.
2. **Seção 0 do relatório** (Métodos/Preparação) = percorrer o pipeline e emitir o
   script de preparo — autocontido e reprodutível.
3. Uma **trilha visível** (chips na ordem) com desfazer, reordenar e desativar
   etapa, e prévia do dataset reagindo a cada passo.

## 2. Diagnóstico: as ilhas de hoje

Preparo já existe, mas espalhado e sem trilha única:

| Onde | Estado atual (reactiveVal) | Vira etapa tipo |
|------|----------------------------|-----------------|
| Importação · tipagem | `col_types_rv` | `tipar` |
| Importação · recodificar/agrupar | `col_recodes_rv` | `recodificar` |
| Importação · filtro de nível | `level_filters_rv` | `filtrar_niveis` |
| Importação · filtro de faixa | `range_filters_rv` | `filtrar_faixa` |
| Importação · renomear | `col_renames_rv` | `renomear` |
| Importação · selecionar colunas | `selected_cols_rv` | `selecionar` |
| `mod_arrumar` · empilhar/separar/alargar | `passos_rv` (cfg) | `empilhar`/`separar`/`alargar` |
| `mod_calcular` · calcular/reescalar | `passos_rv` | `calcular`/`reescalar` |
| **NOVO** | — | `tratar_na` |
| **NOVO** | — | `dicotomizar` |

Cada ilha gera seu fragmento de código isolado e promove via `dados_analise`. Não
há de onde "ler" o preparo inteiro — é isso que trava a Seção 0.

## 3. A espinha

### 3.1 Registro de tratamentos (extensível — a "evolução")

Um catálogo de **tipos** de etapa. Crescer o tratamento de dados = adicionar um
tipo aqui, sem tocar no resto.

```r
# registro_tratamentos.R — funções puras, testáveis, espelháveis no livro
tratamentos <- list(
  tratar_na = list(
    rotulo  = function(p) sprintf("Tratar NA de %s (%s)", p$coluna, p$metodo),
    validar = function(df, p) { if (!p$coluna %in% names(df)) "coluna inexistente" else NULL },
    aplicar = function(df, p) { ... },      # df -> df
    codigo  = function(p) "dados <- dados |> ..."   # -> string R
  ),
  dicotomizar = list(...),
  tipar = list(...), filtrar_faixa = list(...), ...  # ilhas migradas
)
```

### 3.2 O pipeline

```r
pipeline_rv <- reactiveVal(list())   # lista ordenada de instâncias
# instância: list(id, tipo, params, ativa = TRUE)

# dataset ativo = replay do pipeline sobre os dados brutos
dados_analise <- reactive({
  Reduce(function(df, et) {
    if (!isTRUE(et$ativa)) return(df)
    tratamentos[[et$tipo]]$aplicar(df, et$params)
  }, pipeline_rv(), raw_data())
})
```

Replay determinístico substitui a promoção manual (`dataset_ativo_rv`). Se uma
etapa falha no replay (ex.: reordenou e a coluna ainda não existia), marca-se a
etapa em vermelho na trilha e segue com as anteriores.

### 3.3 Geração de código (Seção 0)

```r
gerar_script_preparo <- function(pipeline, info) {
  # cabeçalho de leitura (como em calc_gerar_codigo) +
  # para cada etapa ativa: comentário + tratamentos[[tipo]]$codigo(params)
}
```

O mesmo texto vira a Seção 0 do `.qmd` do relatório (um chunk por etapa, ou um
bloco único de preparo antes das análises).

## 4. Como inserir as ilhas atuais (migração não-destrutiva)

Adaptador fino, sem reescrever as UIs existentes:

1. **Mantém-se** cada modal/painel atual (tipar, recodificar, filtros, arrumar,
   calcular).
2. No "confirmar", em vez de escrever no seu `reactiveVal` próprio, a ilha **anexa
   uma etapa** ao `pipeline_rv` (um `append_etapa(tipo, params)`). Shim pequeno.
3. `dados_analise` passa a ser o replay (§3.2). Os `reactiveVal` antigos podem ser
   derivados do pipeline ou aposentados um a um.
4. Arrumar/Calcular já têm `cfg`/`expr` prontos → viram `params` quase direto, e o
   código deles (`arrumar_codigo_transformacao`, `calc_gerar_codigo`) migra para as
   funções `codigo()` do registro.

Ordem de migração sugerida (menor risco primeiro): filtros → tipar → recodificar →
selecionar/renomear → calcular/reescalar → arrumar. Cada uma isolada e testável.

## 5. Os dois tratamentos que validam o contrato (agora)

Feitos em paralelo com a espinha, servem de caso real para o contrato de etapa.

### 5.1 `tratar_na` — dados faltantes

Params: `coluna` (ou "todas as numéricas"), `metodo ∈ {remover_linhas,
imputar_media, imputar_mediana, imputar_moda, constante}`, `valor` (se constante).

Código gerado (exemplos):
```r
dados <- tidyr::drop_na(dados, peso_g)                                  # remover_linhas
dados <- dados |> mutate(peso_g = ifelse(is.na(peso_g),
                                          median(peso_g, na.rm = TRUE), peso_g))  # mediana
```
UI mostra a **contagem de NA por coluna** antes de aplicar (orienta a escolha).

### 5.2 `dicotomizar` — variável 0/1

Params: `coluna`, `origem ∈ {numerica, categorica}`, e:
- numérica: `operador` (≥, >, ≤, <) + `limiar` → `as.integer(coluna >= limiar)`;
- categórica: `niveis_1` (quais níveis viram 1) → `as.integer(coluna %in% c(...))`.
`nome` da nova coluna; rótulos opcionais.

Código:
```r
dados <- dados |> mutate(maturo = as.integer(comp_cm >= 25))            # numérica
dados <- dados |> mutate(presente = as.integer(especie %in% c("Sardinha","Corvina")))  # categórica
```
Liga direto à **regressão logística** (desfecho 0/1) — ponto que o `CLAUDE.md`
pede explicitamente na etapa de preparo.

## 6. UI — a trilha de preparo

Novo painel em **Preparando Dados → "Trilha de preparo"** (e o `mod_tratar` hospeda
os tratamentos NA/dicotomização):

- **Trilha** (coluna esquerda/topo): chips numerados na ordem
  (1 Importar · 2 Tipar comp_cm · 3 Filtrar faixa · 4 Calcular fator · …), cada um
  com ↑/↓, ativar/desativar (checkbox), editar, remover. Etapa com erro de replay
  fica vermelha.
- **Prévia** (centro): dataset resultante do replay até a etapa selecionada + abas
  "Script de preparo" (o `gerar_script_preparo`) e "Antes/Depois".
- **Ações** (direita): adicionar tratamento (NA, dicotomizar, …), baixar script,
  e o pipeline já alimenta as análises (é o `dados_analise`).

Genuinamente novo: **uma única trilha** que reúne TODO o preparo, de qualquer
origem, reproduzível e que vira Métodos do relatório.

## 6.5 Ordem LÓGICA vs. ordem CRONOLÓGICA (a "viagem no tempo")

Princípio central pedido pelo Evaldo: o pipeline guarda a **ordem lógica** (a que
faz sentido para preparar os dados), **não** a ordem cronológica real dos cliques.
Como `dados_analise` é o **replay** do pipeline sobre os dados brutos, mover uma
etapa "para o passado" (reordenar para antes) faz tudo ser **re-derivado como se
aquele tratamento sempre tivesse estado ali** — mesmo que, na vida real, você tenha
feito uma análise numa estrutura antiga e só depois voltado e inserido o
tratamento. O documento e o script saem lineares e "planejados"; a bagunça
temporal fica só na sua cabeça. É a mesma ideia de um pipeline de banco de dados /
de um "recipe" reprodutível: declarativo, desacoplado do tempo.

Isso exige três coisas na Fase 1:

1. **Inserir em qualquer posição** (não só no fim) — append + Subir/Descer já
   permite; posição explícita é um plus.
2. **Validação de replay + dependências.** Ao reordenar/inserir, o replay pode
   quebrar (a etapa reordenada usa uma coluna que só nasce depois; ou uma análise
   já enfileirada perde a variável de que dependia). O replay valida etapa a etapa,
   **pula/sinaliza em vermelho** a que falhou e segue; e checa se as variáveis de
   que as análises dependem ainda existem no fim.
3. **Log cronológico opcional (auditoria/honestidade).** Guardar, à parte, a ordem
   real dos cliques — para transparência e para o ensino (mostrar "como foi de
   verdade" × "como ficou organizado"). Não entra no script; é só trilha de
   auditoria. Hook previsto, implementação depois.

**Ressalva científica (importante num software de ensino):** reorganizar etapas de
*preparo* (tipar, filtrar estrutural, arrumar) numa ordem lógica é boa prática e
totalmente reprodutível — dados brutos + script reconstroem tudo. Mas se um
*filtro/tratamento* foi escolhido **por causa** de um resultado de análise (ex.:
remover outliers que atrapalhavam o p-valor) e depois for apresentado como
"pré-planejado", isso beira o HARKing/p-hacking. A ferramenta deve deixar o caminho
honesto fácil (bruto + script sempre reprodutíveis) e o log cronológico serve
justamente para não apagar essa distinção.

## 6.6 Duas fases, nomeação de datasets e o DAG (invariantes do script)

Três regras duras que o script/relatório integrado SEMPRE respeita — por
**estrutura**, não por reordenação manual:

1. **Preparo antes das análises (duas fases).** No script integrado, todo o bloco
   de preparo vem ANTES de qualquer análise. Uma análise nunca pode aparecer antes
   de os dados dela estarem prontos — isso é estruturalmente impossível: o gerador
   emite a Fase de Preparo inteira e só então a Fase de Análises. Reordenar dentro
   da trilha nunca empurra uma análise para antes do seu preparo.

2. **Datasets nomeados em ordem de dependência (ordenação topológica).** Quando um
   conjunto deriva de outro, os objetos são emitidos na ordem em que dependem uns
   dos outros, então um dataset nunca é usado antes de ser criado. Dois níveis:
   - **Trilha linear (v1):** um único objeto corrente `dados` encadeado por pipe
     (`dados <- dados_brutos |> etapa1 |> etapa2 |> ...`), terminando em
     `dados_preparados`. Sem N nomes, sem como quebrar.
   - **Ramos nomeados (evolução) — topologia em ESTRELA, um salto só:** existe UMA
     base tratada compartilhada (`base`, = `dados_preparados`) e, dela, partem
     ramos-irmãos, cada um o sub-preparo de uma análise. Todos derivam DIRETO da
     base — nunca ramo-de-ramo. Nomeação: `base_<analise>` (ex.: `base_reg_logistica
     <- base |> dicotomizar(...)`, `base_pca <- base |> escalar(...)`). Assim não há
     `base_subtrat1_subtrat2`; há `base_subtrat1`, `base_subtrat2`, `base_subtrat3`,
     todos a um salto da base. Ordenação trivial: emite `base`, depois os `base_*`,
     depois as análises. Cada análise DECLARA qual dataset consome (padrão = `base`).

3. **Não intercalar tratamento com análise (guarda contra p-hacking).** O preparo
   específico de uma análise NÃO é "movido para junto" da análise (intercalar é
   confuso e abre porta para tunar os dados por resultado). Em vez disso, ele vira
   um **ramo nomeado explícito** na Fase de Preparo, que a análise apenas consome.
   O nome torna a proveniência visível e auditável — o oposto de reajustar os dados
   em silêncio. O log cronológico (§6.5) reforça essa honestidade.

Convenção de nomes (estrela, um salto): base compartilhada = `base`
(= `dados_preparados`); ramo por análise = `base_<analise>` (ex.:
`base_reg_logistica`, `base_pca`) — todos derivando DIRETO de `base`. Para manter a
maioria dos tratamentos não-destrutivos, a regra é **criar coluna nova** em vez de
sobrescrever (como o Calcular/Reescalar já faz); só filtros de linha e recortes são
inerentemente destrutivos e, por isso, candidatos naturais a ramo.

**Tabela de Contingência = o ramo exemplar (decisão jul/2026).** A tabela de
contingência é o poster-child do conceito de ramo: uma estrutura larga/agregada
(contagens linha×coluna), específica do qui-quadrado, derivada da base — logo um
`base_contingencia`, NUNCA a base compartilhada (se virasse `dados_analise`,
quebraria t/regressão/etc., que esperam observações tidy). Decisão: o tab **fica em
Preparando Dados**, assumido como ramo que alimenta o qui-quadrado (a tabela é o
reshape/preparo; o teste é o resultado). Hoje já está seguro — o `mod_contingency`
lê `dados_analise` e não promove nada.

**Decisão de escopo (jul/2026): "compartilhado agora, ramos previstos".** A v1 usa
UMA base compartilhada (`base`) que todas as análises leem; os ramos ficam para a
evolução, mas os GANCHOS entram já: cada análise carrega um campo `dataset_entrada`
(padrão = `base`), pronto para apontar para um `base_<analise>` quando os ramos
chegarem — sem retrabalho.

## 6.7 Como reconhecer a base (e onde começam os ramos)

**Teste conceitual:** você chegou à base quando o próximo tratamento serviria a
UMA análise só. Passo que TODAS as análises usariam (limpar NA, tipar, arrumar
tidy, variável de interesse geral) → ainda é a base compartilhada. Passo que só
faz sentido para uma análise (dicotomizar p/ logística, padronizar p/ PCA, montar
tabela p/ qui-quadrado, filtrar um subgrupo p/ um teste) → é ramo. Pergunta que
decide: "isto todas as análises usariam?" Sim → base; só uma → ramo.

**Sinais que a IDE destaca:** a base é o que as análises leem por padrão
(`dataset_entrada = base`); um passo que produz algo **não-tidy** (largo/agregado,
como a contingência) é ramo por natureza; um **filtro de linha** específico é
candidato a ramo. Na trilha, um **divisor "── base ──"** separa: acima = base
compartilhada; abaixo/pendurado = ramos `base_<analise>`.

**Marcação da base (decidido jul/2026 — AS DUAS):** a IDE **sugere por pergunta**
(ao adicionar um passo que parece específico de análise — não-tidy, filtro de
subgrupo, dicotomizar/padronizar — oferece "criar ramo `base_<analise>`?", e a base
é tudo antes) E o usuário também pode **marcar/mover a base manualmente** (um
divisor "── base ──" que ele arrasta/posiciona). A pergunta guia o iniciante; a
marcação manual dá controle ao avançado. Implementação: Fase 3+.

## 7. Plano em fases

- **Fase 1 — Espinha + 2 tratamentos.** `registro_tratamentos.R` (contrato +
  `tratar_na` + `dicotomizar`), `pipeline_rv`, replay, `gerar_script_preparo`,
  `mod_tratar` com a trilha. Opera como ilha (base = dados atuais, promove
  resultado) para não quebrar nada.
- **Fase 2 — Replay vira o `dados_analise` global.** Trocar `dataset_ativo_rv` pelo
  replay do pipeline.
- **Fase 3 — Migrar as ilhas** (§4), uma por vez, para o registro.
- **Fase 4 — Seção 0 do relatório** consome `gerar_script_preparo` (fecha o elo com
  `MODULO_COMUNICACAO_RESULTADOS.md`).
- **Fase 5 — Entrosamento**: registro de tratamentos migra para o EAPADados; o livro
  ("Antes da análise") deriva dos mesmos geradores.

## 7.1 Fase 2 em detalhe — o replay vira o `dados_analise` global

Objetivo: a Trilha deixa de ser ilha e passa a ser a **camada mais externa** que
define o dataset ativo. Some o "Usar nas análises" da Trilha — os tratamentos se
aplicam automaticamente às análises.

### Resolução do dataset ativo (ordem das camadas)

```
importados (current_data, pós-painel de importação)
   │
   ├─ Arrumar / Calcular promovem via dataset_ativo_rv  →  base_resolvida
   │
   └─ replay(base_resolvida, pipeline_rv)  →  dados_analise  →  análises
```

- `base_resolvida <- reactive({ da <- dataset_ativo_rv(); if (is.null(da)) current_data() else da$df })`
- `dados_analise  <- reactive({ replay_pipeline(base_resolvida(), pipeline_rv())$df })`
- `pipeline_rv` **sobe para o nível do app** (estado global); `mod_tratar` recebe e edita.

### Armadilha a evitar: DUPLA aplicação da trilha

Se o Calcular lesse o `dados_analise` (já com a trilha) e promovesse, a trilha
seria **re-aplicada por cima** do resultado — uma reescala dividiria duas vezes, um
filtro removeria linhas de novo. Regra de ouro da Fase 2: **a trilha é a camada
externa ÚNICA**; todos os produtores (Arrumar, Calcular, Trilha) leem a
`base_resolvida` (pré-trilha), nunca o `dados_analise`. Ordem efetiva:
`importados → Arrumar → Calcular → Trilha → análises`.

### Mudanças concretas

1. Criar `pipeline_rv` no app; passar para `mod_tratar_server` (lê/escreve o
   global). Remover o `on_usar` do tratar e o botão "Usar nas análises" (a trilha
   é automática agora).
2. Definir `base_resolvida` + `dados_analise = replay(base_resolvida, pipeline_rv)`;
   expor `erros_replay_rv` global (para sinalizar etapas com erro em qualquer lugar).
3. Trocar a fonte de `mod_calcular_server` (hoje `dados_analise`) e
   `mod_arrumar_server` (hoje `current_data`) para **`base_resolvida`**.
4. `mod_tratar_server` passa a ler `base_resolvida` (a mesma base do `dados_analise`).
5. O indicador do dataset ativo mostra também "trilha: N etapas (M ativas)";
   `voltar_importados` continua zerando a camada Arrumar/Calcular; adicionar
   "limpar trilha".
6. Testes: importar → NA/dico na trilha → ver as análises reagirem **sem** clicar
   "usar"; promover um Arrumar e ver a trilha reaplicar por cima; reordenar e
   conferir o replay + as etapas que ficam vermelhas.

### O que fica para a Fase 3

Aposentar o `dataset_ativo_rv` de vez: Arrumar/Calcular viram **etapas do registro**
(`empilhar`/`separar`/`calcular`/`reescalar`) anexadas ao mesmo `pipeline_rv` — some
a camada separada e tudo vira uma trilha só. Migrar também os tratamentos do painel
de importação (tipar/filtrar/recodificar/selecionar).

### Nota sobre as duas fases

"Preparo antes das análises" é invariante do **script/relatório gerado**, não do
reativo ao vivo. No app há **um** dataset ativo (o replay); a estrutura em duas
fases é propriedade do artefato exportado (ver `MODULO_COMUNICACAO_RESULTADOS.md`
§6.1.1). A Fase 2 não precisa impor isso na tela.

## 8. Riscos / itens abertos

- **Reordenar quebra dependências** (calcular antes de arrumar a coluna) → validar
  no replay e sinalizar; opcional: sugerir ordem válida.
- **Dois níveis de "desfazer"** (o da trilha vs. os das ilhas atuais) durante a
  migração — evitar conflito aposentando o desfazer local ao migrar cada ilha.
- **Custo de replay** em datasets grandes (recomputa tudo a cada mudança) →
  memoizar por prefixo de etapas se necessário.
- **Compatibilidade** com a promoção atual do Arrumar/Calcular durante a transição.
- **Catálogo futuro** (após validar o contrato): outliers, padronizar/z-score,
  binning/classes de tamanho, datas→estação, join de tabelas, deduplicar.

## 9. Status de implementação e dataset de treino (jul/2026)

**Feito:** Fase 1 (espinha `pipeline_rv` + registro + replay + trilha com desenho
SVG ao vivo) e Fase 2 (replay = `dados_analise` global; produtores leem a
`base_resolvida` para não aplicar a trilha duas vezes; trilha automática, sem "Usar
nas análises"). Registro com **6 tratamentos**: `tratar_na`, `dicotomizar`,
`padronizar` (z-score/centralizar/normalizar), `binning` (classes de tamanho),
`remover_duplicatas`, `padronizar_texto`. O módulo estrutural
**Agrupar/Sumarizar** também integra a v1 (uma linha por grupo; `n`, soma, média,
mediana, mínimo, máximo e desvio-padrão) e promove seu resultado antes do replay
da Trilha. A arquitetura integrada está em `PIPELINE_DADOS_E_RELATORIOS.md`.
Fora desse conjunto frequente, o usuário continua fazendo a transformação no
Excel.

**Feito (Fase 3A):** registro central e painel de bases derivadas, com topologia
em estrela obrigatória, nomes R validados, criação, replay, prévia, código,
renomeação, finalização, reabertura e exclusão confirmada. Ramos novos ainda têm
zero etapas e são idênticos a `dados_analise`.

**Feito (Fase 3A.1):** cache por ramo separado da receita, replay lazy somente
por botão, estados Não calculada/Atualizada/Desatualizada/Com erro, preservação
da última prévia válida, isolamento de falhas e bloqueio de cache inválido para
os futuros seletores analíticos. Nenhuma interação comum dispara recálculo em
cascata.

**Feito (Fase 3B.1):** editor da receita específica de cada ramo, reutilizando o
registro canônico de tratamentos. Permite adicionar, ordenar, ativar/desativar,
remover e limpar etapas somente em rascunhos. Toda edição incrementa a versão e
invalida o cache sem recálculo automático; o ramo continua nascendo diretamente
de `dados_analise`.

**Feito (Fase 3B.2 — piloto):** seletor de base na Regressão Logística. Mostra a
base compartilhada e somente ramos prontos/atualizados, prioriza a finalidade
logística, resolve o `data.frame` pelo ID estável e volta com aviso para
`dados_analise` quando o ramo deixa de estar disponível. O módulo expõe o
contexto da base para o futuro registro de execuções.

**Feito (Fase 3B.3):** o contrato de seleção foi extraído para um componente
reutilizável e conectado aos módulos prioritários: Estatística Descritiva,
Regressão Linear Simples, Teste t, Gráfico de Linhas, Qui-quadrado, PCA e
Análise de Agrupamentos. Cada seletor oferece `dados_analise` e somente ramos
prontos/atualizados, sugere a finalidade compatível, resolve pelo ID estável e
consome o cache sem replay. No Qui-quadrado, a base escolhida vale para a fonte
**Duas variáveis**; tabela preparada e entrada manual mantêm suas fontes.

**Feito (Fase 3C):** registro explícito e leve de execuções nos oito pontos
prioritários (os sete módulos da 3B.3 mais a Regressão Logística). Cada clique
captura ID independente, título, vínculo com a base, revisão, parâmetros,
saídas disponíveis e resumo compacto, sem duplicar dados ou gráficos. Alterar a
prévia não sobrescreve o item; atualizar, salvar como novo e remover são ações
explícitas. Dependências alteradas ficam marcadas como **Precisa atualizar**.

**A fazer depois da Fase 3C:** fazer as execuções alimentarem a Comunicação de
Resultados (3D) e integrar o Projeto R/relatório (3E). A eventual aposentadoria
de `dataset_ativo_rv` permanece uma migração posterior e gradual, fora dessas
duas entregas.

**Dataset de treino:** `inst/app/dados/Treino-Transformacoes.xlsx` — abas
`biometria` (bagunçada de propósito: NA, 3 duplicatas, texto inconsistente, escalas
diversas, coluna composta `amostra`), `desembarques_largo` (colunas
`2022/2023/2024 - Captura (t)` para Empilhar) e `guia` (feature → coluna). Usado
para exercitar toda a trilha ponta a ponta.
