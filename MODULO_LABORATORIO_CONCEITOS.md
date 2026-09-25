# Laboratório de Conceitos — visualizadores pedagógicos (apoio ao professor)

**Verificado em:** CatalyseR 0.1.5, commit `6aa407a`, 27/07/2026

> Repurpose do menu **"Calculando Probabilidades" → "Laboratório de Conceitos"**
> (decidido jul/2026). Um QUARTO pilar pedagógico do ecossistema: **visualizar**
> os fundamentos da estatística, não só calcular. Apoio direto ao professor em aula.

## Estado implementado

O menu **Laboratório de Conceitos** apresenta uma lista de opções; cada escolha
abre uma única tela. A primeira opção, **Visão geral**, é o mapa pedagógico.
O primeiro visualizador,
**Teorema do Limite Central (TLC)**, está implementado em `mod_lab_tlc.R`: ele
simula populações uniforme, exponencial e binomial, permite variar *n* e o
número de amostras e compara o histograma das médias à normal teórica.

O **Simulador da ANOVA**, em `mod_lab_anova.R`, também está implementado. Ele
gera grupos artificiais independentes da base importada, permitindo variar
número de grupos, tamanho das amostras, médias e variação dentro dos grupos.
O simulador antes presente no painel da ANOVA foi movido para cá: aprendizado
conceitual não entra no resultado, no relatório nem no Projeto R.
Cada grupo tem média e desvio-padrão lado a lado. Como os desvios podem ser
diferentes, a rodada usa a ANOVA de Welch. Uma terceira coluna mostra a curva F
sob H0, com o F crítico teórico (α = 5%) e o F simulado em cores distintas.

O **Simulador do teste t**, em `mod_lab_teste_t.R`, completa o par de
comparações de médias com dois grupos artificiais. Permite variar tamanho dos
grupos, médias e desvio-padrão para observar o estatístico *t* e o p-valor sem
depender de dados importados. A opção **Visão geral** tem uma barra lateral:
cada item abre a explicação pedagógica do visualizador e indica a aba onde ele
é experimentado.
Cada um dos dois grupos tem média e desvio-padrão próprios. O teste é o *t* de
Welch; a terceira coluna mostra a curva *t* sob H0, os dois limites críticos
teóricos de um teste bilateral (α = 5%) e o *t* simulado.

Lei dos Grandes Números, Cobertura do IC, H0/p-valor e Curvas
z/t/F/qui-quadrado ainda mostram placeholders “em construção”.

**Distribuição Normal** e **Distribuição Binomial** permanecem reservadas como
opções próprias; as telas atuais são placeholders. Não descrever os outros
visualizadores como implementados até que haja módulo e teste correspondentes.

## Objetivo
Menu-laboratório com visualizadores INTERATIVOS (sliders + gráfico reativo) de
conceitos fundamentais, para o professor mostrar/ensinar ideias com o momento "aha".

## Conteúdo (decidido)
**Migram para cá (já existem):** explorar **Distribuição Normal** e **Binomial**
(os calculadores atuais viram a parte "explorar distribuições").

**Novos visualizadores:**
- **Simulador da ANOVA** — [IMPLEMENTADO] grupos artificiais; distância entre
  médias e variação dentro dos grupos mostram como muda F e o p-valor.
- **Simulador do teste t** — [IMPLEMENTADO] dois grupos artificiais; tamanho
  amostral, diferença entre médias e variação mostram como mudam *t* e o p-valor.
- **TLC (Teorema do Limite Central)** — [PILOTO] médias de qualquer distribuição
  (uniforme/exponencial/binomial/assimétrica) viram um sino conforme *n* cresce;
  sliders de *n* e nº de amostras; sobrepõe a normal teórica.
- **Lei dos Grandes Números** — a média amostral correndo para μ conforme *n* aumenta.
- **Cobertura do Intervalo de Confiança** — 100 amostras → 100 ICs; ~95% "pegam" μ;
  sliders de confiança e *n*. Casa com a análise de IC da v1.
- **Distribuição sob H0 / p-valor** — distribuição nula, estatística observada e a
  área do p-valor / região crítica; sliders de estatística, gl e cauda.
- **Curvas de distribuição** — **z** (normal padrão), **t**, **F**, **qui-quadrado**:
  forma × graus de liberdade, áreas/quantis. Explorar visualmente cada família.

## Padrão de módulo
Cada visualizador = `mod_lab_<conceito>.R`: UI com sliders/controles + `renderPlot`
reativo (ggplot no tema Ocean) + texto explicativo curto. **Autocontido**
(`rnorm`/`sample`/`replicate`…), NÃO depende de dados importados. Todos vivem no
menu **Laboratório de Conceitos**.

Nomes atuais/reservados para a família: `mod_lab_tlc.R`, `mod_lab_anova.R`,
`mod_lab_teste_t.R`, `mod_lab_lgn.R`,
`mod_lab_cobertura_ic.R`, `mod_lab_h0_pvalor.R`, `mod_lab_curvas.R`,
`mod_lab_normal.R` e `mod_lab_binomial.R`.

## Próxima implementação planejada

Com o TLC como piloto, o próximo visualizador deve ser escolhido pelo ponto do
percurso em que ele fará mais diferença. H0/p-valor e as curvas de distribuição
conversam diretamente com os testes; LGN e Cobertura do IC aprofundam a ponte
entre exploração e inferência. Os visualizadores entram um a um.
