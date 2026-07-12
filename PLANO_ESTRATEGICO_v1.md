# Plano estratégico — CatalyseR rumo à v1 (estabilizar + polir)

> **Princípio-guia (jul/2026):** *não* incluir análises novas. Só **corrigir o que já
> temos** e **arrumar a Trilha e o relatório com várias análises**, deixando-os
> **leves, rápidos e usáveis**. Tudo o que for "novo tipo de análise" vai para backlog.

Meta transversal em três palavras: **leve · rápido · usável**.

---

## Bloco 0 — Destravar a instalação (prioridade máxima, baixo esforço)

O que **já está corrigido nos arquivos** (falta o seu commit + push — mesmo playbook do EAPADados):

- `inst/app/app.R`: o crash **"colunas indefinidas selecionadas"** ao abrir dataset — resolvido com a guarda `intersect(selected_cols_rv(), names(df))` nos dois recortes de coluna.
- `DESCRIPTION`: passa a declarar as dependências de runtime que o `install_github` não puxava — **EAPADados** (via `Remotes:`) + `car`, `emmeans`, `effectsize`, `vistributions`, `scales`, `readr`.
- `README.md`: aponta para o **instalador** (`instalar_catalyser.R`, binário-primeiro) em vez do `install_github` cru.
- `mod_mapa.R`: aviso de que **Mapas** exige `sf`/`geobr` (dependem de fonte; Rtools no Windows).

**Ações:** commit + push → validar instalação **limpa** em (a) Windows com Rtools e (b) Linux VM (via Posit PPM), como fizemos no EAPADados.

**Definição de pronto:** um aluno instala pelo instalador, **abre e carrega um dataset sem o app fechar**, e ANOVA/ANCOVA/regressão não quebram por dependência faltando.

---

## Bloco 1 — Trilha de Preparo: estabilizar e enxugar (sem tratamentos novos)

Estado: Fases 1–2 implementadas (trilha linear + *replay* = `dados_analise`, lendo `base_resolvida`); 6 tratamentos. Spec: `EVOLUCAO_TRATAMENTO_DADOS.md`.

Foco (leve/rápido/usável) — **nenhum tratamento novo**:

1. **Robustez de estado.** O crash do Bloco 0 nasceu de *estado velho* (colunas do dataset anterior). Garantir que **trocar/abrir dataset re-deriva a trilha sem resíduo**, com guarda `intersect` em todo recorte de coluna. É a mesma disciplina, aplicada em toda a cadeia `base_resolvida → replay → dados_analise`.
2. **Replay eficiente (rápido).** Cachear `base_resolvida`/`replay_res` para não recomputar a base a cada interação; recalcular só o necessário ao reordenar/editar etapas.
3. **UX (usável).** Desenho SVG ao vivo claro; reordenar fluido; **erro por etapa** que mostra a mensagem sem derrubar a trilha (o `replay_pipeline` já isola — reforçar o feedback visual).
4. **Trilha como Seção 0 capturável.** O script do preparo precisa sair em **ordem de dependência (topológica)** — é exatamente o que o relatório (Bloco 2) consome como "Preparação dos dados".

**Fora de escopo agora:** base/ramos (Fase 3), Box-Cox, joins, datas — backlog.

---

## Bloco 2 — Comunicação de Resultados: do casca ao funcional (leve e rápido)

Estado: casca de 3 colunas feita (`mod_comunicacao.R`); legado `download_consolidated_zip` (~1400 linhas, ordem **hardcoded em 3 lugares**, marca "usei" só por *visitar* a aba, **não renderiza docx**). Spec: `MODULO_COMUNICACAO_RESULTADOS.md`.

Seguir as fases da spec, **reaproveitando os builders que já existem** (nenhuma análise nova):

- **Fase 0 — Registry + contrato.** `registro_analises` como **fonte única de ordem** (elimina as 3 duplicações) + `estado_relatorio()` em 2–3 módulos-piloto (descritiva, teste t, regressão).
- **Fase 1 — Módulo + fila.** `fila_rv` real: adicionar/repetir/reordenar/remover/editar título + prévia do *outline* numerado.
- **Fase 2 — Montador do `.qmd`.** **Namespacing de labels** (§6.3 — é o que mais quebra o render), hierarquia/numeração coerente, esqueleto de artigo e **Seção 0 (Preparação)** vinda da Trilha.
- **Fase 3 — Render docx integrado.** Generalizar `render_relatorio_docx()` + `withProgress` + **degradar sem Quarto** (entrega `.qmd` + `custom-reference.docx` num `.zip` em vez de falhar).
- **Fase 4 — Cobrir as demais análises** (migrar os builders do `app.R`, aposentando as ~1400 linhas duplicadas).

Leve/rápido/usável neste bloco: **isolamento de erro por seção** (`#| error: true` — uma análise que falha não derruba o documento); *progress* no render (e render assíncrono se demorar com muitas seções); o registry único deixa o módulo **enxuto**.

**Adiar para agosto (Fase 5):** mover os geradores para o EAPADados e espelhar no livro (entrosamento). Não bloqueia a v1.

---

## Sequência e valor

**0 → 1 → 2.** O Bloco 0 desbloqueia os alunos já (baixo esforço, alto impacto). O Bloco 1 é **pré-requisito** da Seção 0 do relatório. O Bloco 2 é o maior esforço, mas entrega o **diferencial** — o relatório multi-análise "do mouse ao código ao relatório".

## Metas transversais

- **Leve:** sem novas dependências pesadas; registry único elimina duplicação; degradação graciosa sem Quarto.
- **Rápido:** cache de reactives (base/replay); render com *progress*/assíncrono.
- **Usável:** estado sempre coerente (sem resíduo), erros isolados e legíveis, fluxo **Mouse → Código → Relatório** visível.

## Fora de escopo (explícito)

Nenhuma análise nova. Nenhum tratamento novo. `base/ramos`, Box-Cox, joins, datas, Laboratório de Conceitos → backlog.
