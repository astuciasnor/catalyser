# Laboratório de Conceitos — visualizadores pedagógicos (apoio ao professor)

> Repurpose do menu **"Calculando Probabilidades" → "Laboratório de Conceitos"**
> (decidido jul/2026). Um QUARTO pilar pedagógico do ecossistema: **visualizar**
> os fundamentos da estatística, não só calcular. Apoio direto ao professor em aula.

## Objetivo
Menu-laboratório com visualizadores INTERATIVOS (sliders + gráfico reativo) de
conceitos fundamentais, para o professor mostrar/ensinar ideias com o momento "aha".

## Conteúdo (decidido)
**Migram para cá (já existem):** explorar **Distribuição Normal** e **Binomial**
(os calculadores atuais viram a parte "explorar distribuições").

**Novos visualizadores:**
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

## Plano (v1.1, paralelo às análises)
Pilotar o **TLC** primeiro (mais impactante), validando o padrão; depois LGN,
Cobertura do IC, H0/p-valor e as curvas z/t/F/qui-quadrado. O rename do menu é barato
(como foi o da Comunicação de Resultados); os visualizadores entram um a um.
