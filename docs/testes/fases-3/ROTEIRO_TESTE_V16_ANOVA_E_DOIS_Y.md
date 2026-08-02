# Roteiro de homologação da V16 — ANOVA integrada e dois valores de Y

- **Data:** 27/07/2026
- **Base:** branch `main`, commit `6aa407a`, `DESCRIPTION` 0.1.5 — a V16 já foi
  integrada e recebeu dois ciclos de refinamento dos pilotos
- **Escopo:** ANOVA de um fator no pipeline comum + duas execuções do Gráfico de
  Linhas alternando o eixo Y
- **Dataset:** `inst/app/dados/Treino-Transformacoes.xlsx`, planilha `biometria`

> Este roteiro é o percurso manual. Os testes automatizados que o acompanham são
> `tests/test_anova_integrada.R`, `tests/test_anova_exportacao.R` e
> `tests/test_grafico_linhas_troca_y.R`, todos incluídos em `tests/run_tests.R`.

## 0. Antes de começar

```powershell
Set-Location D:\Claude\eapa\catalyser
$R = "C:\R\R-4.6.1\bin\Rscript.exe"
& $R inst/app/tests/run_tests.R
& $R -e "fs <- list.files('inst/app', pattern='[.]R$', recursive=TRUE, full.names=TRUE); invisible(lapply(fs, parse)); cat(length(fs), 'arquivos R aprovados\n')"
```

Os três testes novos devem aparecer com `[OK]`. Se algum falhar, pare aqui: o
percurso manual não separa defeito de configuração.

## 1. Importar e montar a Base Compartilhada

1. **Importando Dados** → carregar `Treino-Transformacoes.xlsx`, planilha `biometria`.
2. **Preparando Dados → Trilha de Preparo**, nesta ordem:
   1. padronizar texto de `especie` — remover espaços extras;
   2. padronizar texto de `especie` — minúsculas;
   3. padronizar texto de `local` — remover espaços extras;
   4. padronizar texto de `local` — minúsculas;
   5. remover duplicatas exatas.
3. Conferir a prévia: devem restar **68 observações** (três duplicatas removidas).

## 2. Base Derivada A — ANOVA

Em **Bases Derivadas**, criar:

| Campo | Valor |
|---|---|
| Nome amigável | `Profundidade de captura por espécie` |
| Objeto R | `base_anova_profundidade_especie` |
| Finalidade | `ANOVA` |
| Descrição | `Compara a profundidade de captura entre as espécies.` |

Receita: remover linhas com NA em `profundidade_m` → **Recalcular** → conferir a
prévia → **Finalizar preparo**.

Não agrupar nem sumarizar: a ANOVA precisa das observações individuais.

## 3. Base Derivada B — gráficos

| Campo | Valor |
|---|---|
| Nome amigável | `Biometria completa das corvinas` |
| Objeto R | `base_graficos_corvina` |
| Finalidade | `Gráficos` |
| Descrição | `Observações de corvina com comprimento e peso completos.` |

Receita: filtrar `especie == "corvina"` → remover NA em `comprimento_cm` →
remover NA em `peso_g` → **Recalcular** → conferir → **Finalizar preparo**.

Checkpoint: **19 linhas**; `comprimento_cm` entre ~13,4 e ~33,1; `peso_g` entre
~33 e ~340.

## 4. Executar a ANOVA

1. **Testes Paramétricos → ANOVA (Análise de Variância)**.
2. Em **Base utilizada**, escolher
   `Profundidade de captura por espécie — base_anova_profundidade_especie`
   (deve aparecer com ★ por causa da finalidade `ANOVA`).
3. Resposta Y: `profundidade_m`; fator X: `especie`; nível de confiança: 95%.
4. **Confirmar que nada foi calculado automaticamente** — o painel deve mostrar
   "Configure a análise e clique em Executar análise".
5. Clicar **uma vez** em **Executar análise**.
6. Percorrer as cinco abas: Resultado principal, Comparações,
   Pressupostos e diagnósticos, Console R, Laboratório didático.
7. Conferir que o resultado usa **68 observações** e **cinco espécies**.
8. Sub-aba **2. Adicionar aos resultados** → título
   `Profundidade de captura entre espécies` → **Adicionar Novo Resultado**.

### 4.1. Benchmarks

| Componente | Valor esperado |
|---|---:|
| n | 68 |
| grupos | 5 |
| F | 2,831 |
| gl do fator | 4 |
| gl dos resíduos | 63 |
| p da ANOVA | 0,0318 |
| η² | 0,152 |
| ω² | 0,097 |
| p de Shapiro-Wilk | 0,7035 |
| p de Levene (mediana) | 0,2458 |

Médias de `profundidade_m` por espécie:

| Espécie | n | Média |
|---|---:|---:|
| bagre | 6 | 28,1 |
| corvina | 26 | 19,7 |
| pargo | 7 | 26,5 |
| pescada amarela | 14 | 29,9 |
| sardinha | 15 | 21,6 |

No Tukey, `pescada amarela-corvina` deve ter p ajustado próximo de **0,0300**.

### 4.2. Verificações de comportamento

- Trocar Y, X, o nível de confiança, o tema ou os rótulos deixa a execução
  **pendente** e o painel avisa "Execute novamente".
- Enquanto pendente, o resultado exibido continua sendo o da configuração
  anterior — nada muda silenciosamente.
- O botão **Adicionar Novo Resultado** fica desabilitado sem execução válida.
- Escolher uma configuração inválida (resposta não numérica, fator com um só
  nível, grupo com uma observação) mostra mensagem com ação corretiva e **não**
  produz resultado parcial.
- A narrativa nunca escreve "H0 aceita"; usa "rejeitou-se H0" ou "não houve
  evidência suficiente para rejeitar H0".
- O gráfico principal mostra observações, distribuição, média e IC 95%, e
  **não** conecta as médias das espécies por linha.

### 4.3. Refinamentos do segundo ciclo

- Todas as tabelas usam **vírgula decimal**; nenhum número aparece com ponto.
- O p-valor abaixo de 0,001 aparece como **`< 0,001`**, não como `0,0000`.
- A linha **Total** da tabela da ANOVA traz `-` em quadrado médio, F e p.
- O **Tukey vem ordenado por p ajustado**: com esta base,
  `pescada amarela-corvina` deve ser a primeira linha.
- O tamanho de efeito tem coluna **Leitura convencional**; com η² = 0,152 deve
  dizer **grande**, e a nota abaixo deve avisar que é convenção de Cohen, não
  interpretação biológica.
- A **narrativa não repete** as médias por grupo nem os p de Shapiro e Levene;
  ela remete ao resumo por grupo e à tabela de pressupostos.
- A narrativa do **Word deve ser a mesma da tela** (mesma estrutura, mesmos
  F/gl/p e mesma remissão). Se divergirem, é defeito.

## 5. Duas escolhas de Y no Gráfico de Linhas

1. **Visualizando Dados → Gráfico de Linhas**.
2. Base: `Biometria completa das corvinas — base_graficos_corvina`.
3. Configurar: X = `id`; Y = `comprimento_cm`; série por cor = `Nenhuma`;
   marcar pontos = sim.
4. Título: `Comprimento das corvinas por observação`.
5. Clicar **uma vez** em **Executar análise** → **Adicionar Novo Resultado**.
6. Alterar **somente Y** para `peso_g`.
7. Confirmar que a interface marca a execução como **pendente** e que o gráfico
   **não** muda antes do clique.
8. Título: `Peso das corvinas por observação`.
9. Clicar **uma vez** em **Executar análise**; confirmar que o gráfico agora usa
   `peso_g`.
10. Em **Execução selecionada**, manter `Nova execução` e clicar
    **Adicionar Novo Resultado** (segunda execução, não substituição).

O eixo `id` é apenas a ordem das observações — não é série temporal.

### 5.1. Contagem das observações

- Abaixo do gráfico deve aparecer a contagem: com a base B,
  **19 observações plotadas; 0 descartadas**.
- Para testar o aviso, escolher Y = `cpue` na Base Compartilhada (que tem
  faltantes): a faixa deve ficar **amarela** e informar quantas linhas saíram.
- Nenhuma linha pode ser descartada em silêncio.

## 6. Comunicação de Resultados

1. Confirmar **três execuções**: a ANOVA e os dois gráficos.
2. Confirmar as **duas Bases Derivadas** em *Bases do projeto*.
3. Ordem: ANOVA primeiro, gráficos depois.
4. Na ANOVA, marcar todos os componentes: Narrativa, Resumo por grupo, Tabela,
   Comparações múltiplas, Gráfico, Pressupostos, Diagnósticos, Console.
5. Marcar os dois gráficos para o Word.
6. Conferir o **Esboço do documento** e o **Manifesto editorial**.
7. Gerar o **Word**.
8. Gerar o **Projeto R**.

O Projeto R preserva todas as execuções, mesmo as desmarcadas no Word.

## 7. Projeto R no RStudio (Windows 11)

1. Descompactar e abrir `projeto_analise.Rproj`.
2. Executar os arquivos de `R/` na ordem numérica.
3. Conferir que o script `04_..._anova_um_fator.R` carrega
   `base_anova_profundidade_especie` e reproduz F = 2,831 e p = 0,0318.
4. Abrir `relatorio.qmd`: o chunk pedagógico da ANOVA deve conter
   `stats::aov`, `stats::TukeyHSD`, `car::leveneTest`, `stats::shapiro.test`,
   `effectsize::eta_squared` e `effectsize::omega_squared`, com
   `eval: false` e `include: false`.
5. Conferir os **labels dos chunks**: devem ser
   `anova-profundidade-m-codigo`, `-modelo`, `-tukey`, `-resumo-grupos`,
   `-replay` e, nos gráficos, `linhas-comprimento-cm-...` e
   `linhas-peso-g-...`. Nenhum label com sublinhado ou com o ID cru como raiz.
6. Conferir que o código do gráfico monta `dados_grafico` com
   `stats::complete.cases()` e imprime quantas observações foram descartadas.
7. Renderizar para Word e comparar os benchmarks com os da CatalyseR.

## 8. Interface

Inspecionar em **1366 × 768**: nenhum painel, botão ou tabela pode ficar
cortado; nenhuma rolagem interna com altura fixa nas abas de resultado.

## 9. Registro de divergências

Anotar, com captura de tela: passo, o que era esperado, o que aconteceu, e se o
problema é de dados, de configuração ou da V16.
