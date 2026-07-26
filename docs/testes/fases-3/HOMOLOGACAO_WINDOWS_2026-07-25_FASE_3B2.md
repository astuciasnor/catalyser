# Homologação da Fase 3B.2 no Windows 10 — 25/07/2026

## Identificação

- Ambiente: Windows 10 em máquina virtual, RStudio.
- Diretório de execução:
  `C:/Users/odlav/Documents/catalyser-fase-3b2-corrigida`.
- Branch histórica: `feature/fase-3b2-seletor-base-piloto`.
- Commit de origem: `07c2cb8`.
- Artefato inicial: `catalyser-fase-3b2-corrigida-NA.zip`.
- SHA-256 do artefato inicial:
  `399BD426C36594C3E6479DBBF43C89747FD48BA4E2FF03449D65500500497FC9`.
- Dados: `inst/app/dados/Treino-Transformacoes.xlsx`, aba `biometria`.
- Base Compartilhada: 71 linhas e 10 colunas.
- Correção de importação por `NA`: revalidada antes do roteiro.

## Preparação

Foram criados dois ramos:

```text
Logística da CPUE:
  base_reg_logistica · Regressão logística
  pronta · Atualizada · 1 etapa · 71 linhas · 11 colunas

Gráficos biométricos:
  base_graficos_biometria · Gráficos
  pronta · Atualizada · 0 etapas · 71 linhas · 10 colunas
```

O ramo logístico criou `cpue_alta = as.integer(cpue >= 3)`. O limiar histórico
`5` foi substituído por `3`, pois o primeiro não produz casos positivos neste
conjunto. O resultado esperado e previamente confirmado é 7 casos iguais a 1 e
64 iguais a 0.

## Resultado dos testes

| Bloco | Resultado | Evidência principal |
|---|---|---|
| 1. Opções elegíveis | Aprovado | O seletor mostrou `dados_analise`, o ramo logístico sugerido com estrela e o ramo de gráficos, nessa ordem. |
| 2. Base compartilhada como padrão | Aprovado | `dados_analise` foi o padrão; `cpue_alta` não apareceu; a Regressão Linear Simples permaneceu sem seletor. |
| 3. Executar com base derivada | Aprovado | Badge 71 × 11, variáveis, ajuste, tabela, diagnósticos e código R usaram o ramo. Os erros encontrados nas métricas e na exportação foram corrigidos e revalidados na VM. |
| 4. Alternar entre bases | Aprovado | Variáveis acompanharam a base escolhida; nenhum ramo foi recalculado ou alterado. |
| 5. Ramo deixa de estar disponível | Aprovado | Reabrir como rascunho removeu o ramo das opções, mostrou aviso e retornou a `dados_analise`; apenas finalizar o tornou elegível novamente. |
| 6. Origem compartilhada muda | Aprovado | Alterar a Trilha invalidou todos os ramos; recalcular somente o ramo logístico o devolveu ao seletor, sem atualizar o ramo de gráficos. |
| 7. Exclusão segura | Aprovado | Excluir o ramo selecionado devolveu o módulo a `dados_analise`, removeu a opção e preservou o ramo de gráficos. |

## Defeito de execução encontrado e corrigido

Ao ajustar Y = `cpue_alta` e X = `comprimento_cm`, o modelo e a tabela foram
gerados, mas a seção de métricas mostrou:

```text
Error: não foi possível encontrar a função "null.deviance"
```

A implementação tratava `null.deviance` como função:

```r
dev_null <- null.deviance(fit)
```

Em um objeto `glm`, o desvio nulo é um componente. A correção aplicada foi:

```r
dev_null <- fit$null.deviance
```

Após reiniciar e repetir o ajuste, foram exibidos sem erro o pseudo-R² de
McFadden, os desvios residual e nulo, o AIC, o N efetivo e o L50/X50. A correção
foi incorporada à fonte atual e ao artefato:

```text
catalyser-fase-3b2-corrigida-NA-null-deviance.zip
SHA-256: 1ADACC0A090A9B0D4709A706E3F2816B2380A6BB284F9A96A4F263ACF7173207
```

## Discrepâncias conceituais e de interface

1. O módulo implementa **regressão logística binária** com
   `glm(..., family = binomial)`, mas aparece como **Curva Logística** dentro de
   **Regressão Não Linear**. O destino correto é **Modelos de Regressão →
   Regressão Logística Binária**, sem confundi-lo com a curva logística não
   linear.
2. A aba do gráfico se chama **Reta Ajustada** e o título padrão mostra
   **Ajuste Linear**, embora o resultado seja uma curva de probabilidade
   logística.
3. O L50/X50 estimado ficou fora da amplitude observada de `comprimento_cm`; a
   linha foi desenhada no limite direito e seu rótulo ficou cortado.
4. O servidor contém o gerador e o modal de **Código R de Reprodutibilidade**,
   mas a interface não possui um botão `export_code` que permita abri-lo. Assim,
   o requisito de inspecionar a receita e o objeto `base_reg_logistica` não pôde
   ser homologado pela interface.
5. As abas **Resíduos vs Ajustados** e **Normalidade (Q-Q Plot)** foram herdadas
   do módulo linear. Para regressão logística, a apresentação deve usar
   diagnósticos próprios de GLM e não sugerir normalidade dos resíduos como
   pressuposto do modelo.

## Conclusão

O contrato central da Fase 3B.2 está aprovado no Windows 10: somente ramos
prontos e atualizados alimentam a análise; a troca é isolada e não executa
replay; mudanças de estado, origem e exclusão provocam retorno seguro à Base
Compartilhada.

A homologação está **aprovada**. O teste focal confirmou o acesso ao código R,
a localização correta da regressão logística binária e os diagnósticos próprios
de GLM. Os blocos de isolamento, invalidação e exclusão permaneceram válidos e
não precisaram ser repetidos.

## Correções preparadas para o teste focal — 26/07/2026

As duas análises foram preservadas separadamente:

- **Modelos de Regressão → Regressão Logística Binária** usa
  `glm(..., family = binomial)` e conserva o seletor de Base Derivada;
- **Regressão Não Linear → Curva Logística** usa o modelo sigmoidal
  `L(t) = Linf / (1 + exp(-k * (t - tm)))` pelo motor canônico
  `ajustar_curva(..., tipo = "logistico")`.

O módulo binário recebeu:

- aba **Curva de Probabilidade**;
- aba **Resíduos de Deviance**;
- aba **Influência (Cook)**, no lugar do Q-Q plot;
- título e eixo Y próprios da regressão logística binária;
- aviso de extrapolação quando L50/X50 está fora da faixa observada, sem
  distorcer o eixo X;
- botão **Ver Código R**.

O novo teste automatizado `test_logisticas_separadas.R` exercita as duas
análises. Durante esse teste foi encontrado um erro adicional no código R
histórico: `%in%` estava dentro de `sprintf()` sem o escape `%%in%%`. A correção
também entrou no artefato focal.

Artefato final preparado:

```text
catalyser-fase-3b2-logisticas-separadas-codigo-corrigido.zip
SHA-256: 3CA61CE7A2731FA4278F1BFA9D715B77B328223B4ECD5860CC83147415536E65
```

Validações concluídas:

- bateria completa da fonte atual: 63 arquivos R e todos os testes aprovados;
- artefato focal extraído novamente: 48 arquivos R com sintaxe válida;
- GLM binomial, métricas, curva, resíduos de deviance, Cook e código R sem erro;
- curva logística não linear ajustada por `nls`, com `Linf`, `k` e `tm`.

## Resultado do teste visual focal no Windows 10 — 26/07/2026

O teste manual foi concluído com Y = `cpue_alta` e X = `comprimento_cm`:

- base `base_reg_logistica` reconhecida com 71 linhas e 11 colunas;
- tabela e métricas do GLM exibidas sem erro de `null.deviance`;
- curva restrita à faixa observada de X, com aviso de extrapolação para
  L50/X50 = 153,37;
- gráficos de resíduos de deviance e distância de Cook exibidos sem erro;
- receita do ramo, `dados <- base_reg_logistica` e `glm(..., family = binomial)`
  presentes no código R.

Durante a verificação do código completo, encontrou-se uma última contaminação:
ao abrir **Influência (Cook)** antes de **Ver Código R**, o gráfico exportado
herdava o título e os eixos da aba ativa e desenhava a linha do L50 fora da
faixa. O gerador foi corrigido para sempre exportar a curva de probabilidade,
usar `Probabilidade estimada`, incluir `caption = aviso_l50` e criar a linha
vertical somente quando `l50_na_faixa` for verdadeiro. A repetição na VM,
acionando a exportação diretamente da aba Cook, confirmou a correção.

Resultado final: **Fase 3B.2 aprovada no Windows 10**.
