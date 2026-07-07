# CatalyseR — Refatoração leve (retomar depois)

Status: **pausado** em jul/2026. Retomar a partir daqui quando houver disposição
para propagar aos demais módulos e testar.

## Diagnóstico

O projeto não é complexo — cada análise é um módulo isolado, cada dataset também.
A única duplicação real e vale a pena atacar é o **pipeline de exportação**, hoje
copiado em ~11 módulos: cada `downloadHandler` de `.docx` e de `.zip` repetia:

- copiar `templates/custom-reference.docx` + `funcoes_*.R` + `.qmd` para `tempdir()`;
- salvar `dados_limpos.rda`;
- (docx) `setwd()` + `system2("quarto", render ... --to docx)` + copiar resultado;
- (zip) montar `dados/` + `scripts/` + `relatorios/`, escrever `.Rproj` + README,
  `zip::zip(...)`.

Raio-x da época: `app.R` ≈ 3862 linhas, 20 downloadHandlers, 20 layouts de 3 colunas,
7 paletas Ocean locais.

## Feito (passo 1 e prova de conceito) — NÃO refazer

1. **Helpers compartilhados** já criados em
   `catalyser/inst/app/modules/utils_export.R` (mudança aditiva, ninguém quebrou):
   - `render_relatorio_docx(file, qmd_name, funcoes_name, df_clean, customizar_qmd = NULL)`
   - `exportar_projeto_zip(file, prefix, qmd_name, funcoes_name, df_clean, info, r_code, customizar_qmd = NULL, readme = NULL)`
   - (`export_to_xlsx()` já existia antes; os novos usam ele por baixo.)
2. **Prova de conceito migrada:** `mod_contingency.R` — os dois handlers
   (`download_report_docx` e `download_project_zip`) agora chamam os helpers.
   O que é específico da análise ficou no módulo: `customizar_qmd_contingency()`,
   o script R de reprodutibilidade e o README. Trecho caiu de ~127 → ~90 linhas.
   - `source()` no `app.R`: `utils_export.R` (linha 12) vem antes de
     `mod_contingency.R` (linha 22) — ordem correta, helpers existem no runtime.
   - Diferença cosmética adotada: no `.zip`, o script agora se chama `contingencia.R`
     (convenção `prefix.R` do helper), antes `analise_contingencia.R`. README ajustado.

## Pendente (retomar aqui)

- [ ] **Testar a Contingência** na IDE: carregar um dataset, gerar `.docx` e `.zip`,
      conferir que saem idênticos ao comportamento anterior. (Não há R no sandbox;
      só o Evaldo consegue rodar.)
- [ ] Se ok, **propagar aos ~10 módulos restantes**, um de cada vez, preservando
      comportamento e sinalizando para teste: teste t, regressão linear, ANOVA,
      testes não-paramétricos (qui-quadrado, Mann-Whitney, Wilcoxon, Kruskal-Wallis),
      descritiva, e demais que tenham handler `.docx`/`.zip`.
- [ ] Padrão de migração por módulo:
      1. criar `customizar_qmd_<analise>(qmd_path)` no módulo (fecha sobre os `input$`);
      2. `download_report_docx` → `render_relatorio_docx(...)`;
      3. `download_project_zip` → `exportar_projeto_zip(...)` passando `r_code` (script)
         e `readme` específicos;
      4. remover o código antigo copiado.
- [ ] (Opcional, se pedirem) parâmetro `script_name` em `exportar_projeto_zip` para
      manter o nome de script original de cada módulo, em vez de `prefix.R`.

## Backlog de refatoração (fora do escopo desta rodada — só anotado)

- Extrair uma **paleta Ocean única** (hoje ~7 cópias locais no `app.R`).
- Considerar um helper de **layout de 3 colunas** (20 repetições), se valer a pena.
- Avaliar quebrar o `app.R` monolítico (~3862 linhas) — provavelmente não vale por ora.

## Regra de ouro (lição aprendida)

Tocar na **espinha de carregamento de dados** com cuidura cirúrgico. Já houve pânico
com a IDE deixando de carregar Excel após uma mudança "esperta". Refatoração de
exportação é segura (aditiva); mexer em import/reactives de dados, não.
