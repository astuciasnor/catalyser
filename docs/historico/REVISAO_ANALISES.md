# Revisão das Análises — CatalyseR (roteiro hoje/amanhã/sexta)

> **Documento histórico.** Este inventário pontual de julho de 2026 não
> representa sozinho o estado atual das análises. O refinamento vigente ocorre
> em ciclos de duas análises, conforme a documentação e a skill atuais.

> Passe de revisão módulo a módulo. Para cada análise: **estado atual**, **o que
> melhorar** (priorizado: 🔴 importante · 🟡 médio · 🟢 polimento) e **esforço**
> estimado. Vira o roteiro dos três dias. Datado jul/2026.

Legenda de esforço: **P** (baixo, < 1h) · **M** (médio) · **G** (grande).

---

## 1. Teste *t* (`mod_parametric` + `funcoes_teste_t.R`)

**Estado atual.** Maduro. Arquitetura canônica exemplar (`calcular_` → `mostrar_`
→ `relatar_`), cobrindo uma amostra, independentes (Welch por padrão) e pareado.
Tem *d* de Cohen com interpretação, rótulo de IC dinâmico, formato numérico BR e
narrativa automática em PT. É o padrão-ouro que os outros módulos devem seguir.

**O que melhorar.**

- 🔴 **Pressupostos (B-003).** Não há teste de **normalidade** (Shapiro-Wilk) nem,
  no independente, de **homogeneidade de variâncias** (Levene/`var.test`). Falta um
  bloco de pressupostos na tabela/relatório dizendo o que foi checado, o resultado e
  a recomendação (ex.: "variâncias desiguais → Welch"). É a maior lacuna. **Esforço M.**
- 🔴 **Limiar de significância fixo em 0,05.** `significativo <- p < 0.05` está
  *hardcoded* nos três `calcular_`, mesmo quando o usuário escolhe `conf = 0,99`.
  Deveria ser `p < (1 - conf)`. Inconsistência real entre o IC exibido e a conclusão.
  **Esforço P.**
- 🟡 **d de Cohen no independente sob Welch.** Usa desvio combinado (*pooled*) mesmo
  quando as variâncias são desiguais; considerar *Hedges' g* ou SD médio nesse caso.
  Opcional: **IC do d**. **Esforço M.**
- 🟡 **Escolha automática do teste.** O `equal_var` é manual; poderia sugerir
  Student vs Welch a partir do teste de variâncias. **Esforço P.**
- 🟢 Migração canônica ao EAPADados já iniciada (`relatos_teste_t.R`) — manter como
  fonte única e o app só chamar.

---

## 2. Regressão Linear Simples (`mod_regression` + `funcoes_regressao.R`)

**Estado atual.** Boa base canônica: `calcular_regressao()` devolve coeficientes,
R²/R² ajustado, RSE, F do modelo e **já testa pressupostos** (Shapiro-Wilk nos
resíduos + homocedasticidade por F auxiliar). Três tabelas (coeficientes, métricas,
pressupostos) e narrativa automática. Melhor que a média.

**O que melhorar / incrementar.**

- 🔴 **Gráfico de resíduos ausente (B-003).** O relatório fala de resíduos mas não
  mostra os diagnósticos visuais. Incluir o painel clássico: **resíduos × ajustados**
  e **QQ-plot** (e, idealmente, escala-locação e distância de Cook). É a maior
  lacuna e tem alto valor didático. **Esforço M.**
- 🟡 **Coluna "Valor" mistura número e texto (B-004b).** Em `mostrar_metricas_regressao`,
  a coluna usa `fmt()` (string, "-" para NA), virando texto e perdendo o alinhamento
  decimal. Separar em coluna numérica + tratar NA sem quebrar o tipo. **Esforço P.**
- 🟡 **Homocedasticidade caseira.** O teste é um F auxiliar `lm(res² ~ ajustados)`,
  uma aproximação de Breusch-Pagan. Trocar pelo **BP formal** (`lmtest::bptest`) ou
  `car::ncvTest` deixa mais rigoroso e citável. **Esforço P.**
- 🟡 **IC dos coeficientes.** A tabela de coeficientes não traz `confint()`; somar
  o IC de intercepto e inclinação. Opcional: **coeficientes padronizados**. **Esforço P.**
- 🟡 **Faixas no gráfico.** Conferir se o gráfico do módulo tem banda de confiança/
  predição da reta; se não, incluir (`geom_smooth(method="lm")` + intervalo). **Esforço P.**
- 🟡 **Só simples, apesar do nome "Simples e Múltipla".** `calcular_regressao` assume
  UM preditor (`coef[2]` = inclinação) e a narrativa idem. Se a múltipla é escopo,
  falta: seleção de vários X, **VIF** (multicolinearidade) e narrativa adaptada.
  Decidir se múltipla entra na v1 ou vira backlog. **Esforço M–G.**
- 🟢 Dedup de `fmt()`/`flextable_ocean()` ao consolidar no EAPADados (B-004d).

---

## 3. ANOVA (`mod_anova` + `funcoes_anova.R`)

**Estado atual.** Sólida para **um fator**: tabela ANOVA (entre/dentro/total),
pressupostos (Shapiro nos resíduos + Bartlett), **Tukey HSD** com IC e narrativa que
já cita pressupostos e pares significativos. Guarda `residuals`/`fitted` para gráficos.

**O que melhorar / incrementar.**

- 🔴 **Só unifatorial.** `calcular_anova(df, dep, ind)` = um fator. O módulo de
  *Delineamento* gera fatorial e parcelas subdivididas, mas a ANOVA que **analisa**
  é só de uma via. Fechar o ciclo com **ANOVA de dois fatores + interação** (e o
  gráfico de interação) seria o maior ganho. **Esforço M–G.**
- 🔴 **Tamanho de efeito ausente.** Falta **η² (eta-quadrado)** / ω² — a proporção da
  variância explicada pelo fator. Baixo esforço, alto valor didático. **Esforço P.**
- 🟡 **Bartlett é sensível à não-normalidade.** Oferecer **Levene** (`car::leveneTest`)
  como alternativa mais robusta. **Esforço P.**
- 🟡 **Sem plano B quando falha pressuposto.** Quando Shapiro/Bartlett rejeitam, o
  relato avisa mas segue na ANOVA; sugerir **Welch (`oneway.test`)** ou **Kruskal-Wallis**
  como alternativa. **Esforço P.**
- 🟡 **CLD (letras) no boxplot.** O Kruskal já traz Dunn+CLD; a ANOVA poderia mostrar
  as **letras de Tukey** sobre o boxplot (mesma linguagem visual). **Esforço M.**
- 🟢 Verificar no módulo se há **boxplot por grupo + diagnósticos de resíduo**; se não,
  incluir (os dados já estão em `r$residuals`/`r$fitted`). Migrar ao EAPADados
  (`relatos_anova.R` já existe).

---

## 4. Qui-quadrado (`mod_contingency` + `funcoes_contingency.R` e `funcoes_nonparametric.R`)

**Estado atual.** Tabela de contingência bem feita (contagens + % por linha/coluna/
total, marginais em negrito), qui-quadrado de Pearson e narrativa. A versão em
`funcoes_nonparametric.R` já é mais completa: **correção de Yates** (2×2), aviso de
**frequência esperada < 5** e **teste exato de Fisher** como alternativa.

**O que melhorar / incrementar.**

- 🔴 **Tamanho de efeito ausente.** Somar **V de Cramér** (e **Phi** no 2×2) — a força
  da associação, não só a significância. É o que fecha a interpretação (B-010). **Esforço P.**
- 🟡 **Duas implementações de qui-quadrado** (contingency e nonparametric) — consolidar
  numa fonte única para não divergirem. **Esforço P.**
- 🟡 **Resíduos padronizados/ajustados** para mostrar *quais células* puxam a associação
  (interpretação célula a célula). **Esforço P.**
- 🟡 **Visualização** — conferir/incluir **gráfico de mosaico** ou barras agrupadas. **Esforço M.**
- 🟢 Migrar ao par canônico `calcular_associacao()`/`relatar_associacao()` no EAPADados (B-010).

## 5. Kruskal-Wallis e Não-Paramétricos (`mod_nonparametric` + `funcoes_nonparametric.R`)

**Estado atual.** O melhor conjunto não-paramétrico: **Kruskal-Wallis** com pós-teste
de **Dunn + letras CLD** (rstatix/rcompanion, com degradação elegante se faltarem),
**Mann-Whitney** e **Wilcoxon** pareado, todos no padrão `calcular_`/`arrumar_`/
`relatar_` com medianas e narrativa.

**O que melhorar / incrementar.**

- 🔴 **Tamanho de efeito em todos.** É a lacuna transversal: **Mann-Whitney/Wilcoxon**
  → *r* (= Z/√N) ou correlação bisserial de postos (`rstatix::wilcox_effsize`);
  **Kruskal-Wallis** → **ε² (épsilon²)** ou η²_H (`rstatix::kruskal_effsize`). Alto
  valor, baixo esforço (rstatix já está nos Suggests). **Esforço P–M.**
- 🟡 **IC do deslocamento (Hodges-Lehmann).** `wilcox.test(conf.int = TRUE)` dá a
  estimativa do deslocamento e IC — bom complemento à mediana. **Esforço P.**
- 🟡 **Nota sobre empates/aproximação** — com empates o `wilcox.test` usa aproximação
  normal (aviso suprimido); registrar isso no relato. **Esforço P.**
- 🟢 **Precisam de dados de exemplo** (pedido do dia): ver seção *Dados* — Mann-Whitney
  (2 grupos) e Wilcoxon (pareado antes/depois).

## 6. Regressão Não-Linear / curvas (`mod_nonlinear` + `funcoes_crescimento.R`)

**Estado atual.** Muito bom. Seis curvas: **potência** (peso-comprimento),
**von Bertalanffy**, **logístico**, **exponencial**, **polinomial** (quadrática) e
**logarítmica**. Valores iniciais robustos (auto-start `SSasymp`/`SSlogis` e
linearização), pseudo-R², RSE, **AIC**, Shapiro nos resíduos e narrativa
**interpretativa de verdade** (alometria *b* vs 3; L∞/*k*/t₀; ponto de inflexão;
ponto ótimo do quadrático). `curva_predita()` para a curva suave.

**O que melhorar / incrementar.**

- 🔴 **Comparação de modelos por AIC.** Com 6 curvas, o maior ganho é **ajustar
  várias e rankear por AIC** ("qual curva descreve melhor estes dados?"). Verificar se
  o `mod_model_discovery` já faz isso e integrar/expor. **Esforço M.**
- 🟡 **IC dos parâmetros.** Faltam ICs (perfil de verossimilhança, `confint(fit)`) para
  L∞, *k*, *b* — muito úteis por serem parâmetros biológicos. Envolver em `tryCatch`
  (nls pode não convergir o perfil). **Esforço P–M.**
- 🟡 **Banda de confiança da curva** predita (além da linha) + **gráfico de resíduos**
  (hoje só Shapiro). **Esforço P.**
- 🟡 **Gompertz** como 4º modelo de crescimento (comum em pesca/aquicultura). **Esforço P.**
- 🟢 **Pseudo-R²**: manter o rótulo "pseudo" e reforçar que **AIC** é o critério de
  comparação (R² não é bem-definido em não-linear). Datasets já existem
  (`tilapia_crescimento`, `cangulo_crescimento`, `biometria_caranguejos`).

## 7. Análise de Agrupamento / HCA (`mod_hca` + `funcoes_hca.R`)

**Estado atual.** Funcional: `dist` (euclidiana/Manhattan) + `hclust` (Ward.D2,
completa, simples, média/UPGMA) + `cutree(k)`, com padronização opcional, **perfil de
médias por cluster**, tabela de pertinência e narrativa.

**O que melhorar / incrementar.**

- 🔴 **Apoio à escolha de k.** Hoje k é 100% manual. Incluir **silhueta**
  (`cluster::silhouette`) e/ou método do cotovelo para sugerir/validar o número de
  grupos — é a dúvida nº 1 do aluno. **Esforço M.**
- 🟡 **Validação do agrupamento.** **Correlação cofenética** (concordância dendrograma
  × distâncias) e largura média de silhueta como qualidade do corte. **Esforço P–M.**
- 🟡 **Dendrograma colorido.** Garantir dendrograma com os *k* grupos destacados
  (retângulos/cores, `rect.hclust`/`factoextra::fviz_dend`) — espelhar o livro. **Esforço P.**
- 🟡 **k-means** como companheiro (backlog B-011: migrar HCA/PCA canônicos ao EAPADados).
- 🟢 **Precisa de dataset multivariado de exemplo** (pedido do dia) — ver seção *Dados*.

## 8. Descrevendo Dados + **nova Tabela de Distribuição de Frequência** (`mod_description` + `funcoes_descritiva.R`)

**Estado atual.** `calcular_descr()` traz N, NAs, média, mediana, DP, variância, mín,
máx, Q25, Q75, com agrupamento opcional e narrativa. Sólido.

**O que fazer / incrementar.**

- 🔴 **NOVO: Tabela de Distribuição de Frequência** (pedido explícito). Nova seção em
  *Descrevendo Dados*, com dois modos:
  - **Discretos** (valores/categorias): `Valor | fᵢ | fr (%) | Fᵢ (acum.) | Fr (acum. %)`.
  - **Contínuos** (classes): nº de classes (padrão **Sturges**, editável) →
    `Classe [a, b) | ponto médio | fᵢ | fr (%) | Fᵢ | Fr (%)`.
  - `flextable` no tema Ocean (referenciada, com legenda); opcionalmente o histograma/
    barras ao lado. Fonte canônica `calcular_freq()`/`mostrar_freq()`/`relatar_freq()`
    no EAPADados, para o livro espelhar. **Esforço M.** *(candidata a hoje/amanhã.)*
- 🟡 **Métricas a somar** na descritiva: **CV (coef. de variação)**, **amplitude**,
  **erro-padrão da média**, **IQR**, e opcional **assimetria/curtose**. O CV é quase
  obrigatório em biologia. **Esforço P.**
- 🟢 Consolidar `calcular_descr` como fonte única (B-002) e dedup de `fmt`/`flextable_ocean`.

## 9. Preparando Dados e Delineamento de Experimentos

**Preparando Dados (`mod_arrumar` + painel de importação).** Acabou de amadurecer
muito (jul/2026): modo por delimitador com auto-detecção, tipagem, recodificação de
níveis, encadeamento de etapas + Desfazer, operação Alargar avulsa e o **dataset ativo**
para as análises. **O que falta:** (🟡) a **Exportação Consolidada** ainda usa os
importados, não o dataset ativo (**B-024**); (🟡) encadeamento *entre* menus (Empilhar
promovido virar entrada do Separar); (🟢) a "checagem *tidy*" que policia vícios (**B-017**).

**Delineamento de Experimentos (`mod_experimental_design`).** DIC, DBC, DQL, fatorial e
parcelas subdivididas, com croqui colorido, casualização por semente, planilha de
coleta e relato. Acabou de ganhar **múltiplas variáveis de resposta**. **O que melhorar:**
(🔴) **fechar o ciclo com a ANOVA** — hoje o módulo gera o delineamento mas a análise
(ANOVA de dois fatores/parcelas) não existe (ver seção 3); (🟡) exportar direto para o
menu ANOVA/dataset ativo depois de preencher os dados; (🟡) checar o `n` mínimo por
célula e avisar sobre desbalanceamento.

---

## Novos conjuntos de dados (EAPADados) — pedido do dia

Contexto amazônico, pesca/aquicultura, no padrão do pacote (`data-raw/*.R` + doc
roxygen + `.rda`). Sugestões de escopo:

- **Análise de Agrupamento** — dataset **multivariado numérico** (≥ 4 variáveis) para
  agrupar unidades. Ex.: *morfometria de várias espécies de peixes de desembarque*
  (comprimento, altura, peso, largura da boca…) ou *parâmetros de qualidade de água por
  ponto de coleta* (OD, pH, temperatura, turbidez, clorofila). ~30–60 linhas, grupos
  latentes plausíveis. Nome sugerido: `morfometria_peixes` ou `qualidade_agua_estacoes`.
- **Mann-Whitney** — **duas amostras independentes**, resposta não-normal/ordinal. Ex.:
  *CPUE (ou nota de frescor) entre dois petrechos/portos*, distribuição assimétrica.
  Nome: `cpue_dois_petrechos`.
- **Wilcoxon (pareado)** — **medições antes/depois** no mesmo indivíduo/ponto. Ex.:
  *escore de vigor de pós-larvas antes e depois de um tratamento*, ou *contagem de uma
  variável no mesmo viveiro em dois momentos*. Nome: `tratamento_pareado_poslarvas`.

**Prompt para buscar/gerar dados (ChatGPT/Gemini) — a preparar:** um *template* que
descreve o ecossistema EAPA (contexto amazônico, colunas *tidy*, tipos, faixas
plausíveis, a análise-alvo e o formato de saída `.csv`/`.xlsx`), para gerar dados
sintéticos realistas ou orientar a busca de dados reais. Padronizar num arquivo
(`PROMPT_DADOS_EAPA.md`) reutilizável por análise.

---

## ANCOVA (inclusão nos paramétricos)

A **ANCOVA** (análise de covariância) mistura um **fator categórico** com uma
**covariável contínua** (ex.: comparar peso entre sexos *ajustando* pelo comprimento).
Estatisticamente é um `lm(y ~ covariavel + fator)` com pressuposto extra de
**homogeneidade das inclinações** (testar a interação `covariavel:fator`).

**Onde colocar:** cabe na **família paramétrica/ANOVA**, como um item próprio "ANCOVA"
ao lado da ANOVA (não dentro do teste *t*). Reaproveita muito da ANOVA: tabela de
efeitos ajustados, pressupostos (normalidade + homocedasticidade + **paralelismo das
retas**), médias marginais ajustadas (*emmeans*) e um gráfico de retas por grupo.
**Esforço M–G** (nova análise; entra como candidata dos 3 dias ou backlog).

---

## Sugestão de ordenação (hoje / amanhã / sexta)

**Ganhos rápidos e transversais (alto valor, baixo esforço) — começar por aqui:**
tamanhos de efeito onde faltam (η²/ε²/V de Cramér/*r* nos não-paramétricos e qui-quadrado),
limiar de significância `1 - conf` no teste *t*, coluna "Valor" da regressão, CV na
descritiva.

**Construções de médio porte:** Tabela de Distribuição de Frequência (nova), gráfico de
resíduos na regressão e no não-linear, apoio à escolha de *k* na HCA, comparação de
curvas por AIC.

**Maiores (decidir se v1 ou backlog):** ANOVA de dois fatores (fechando o ciclo com o
Delineamento), ANCOVA, regressão múltipla, novos datasets + prompt.


