# Homologação das Fases 3B e 3B.1 no Windows 10 — 19/07/2026

## Identificação

- Ambiente: Windows 10 em máquina virtual, RStudio.
- Diretório de execução: `C:/Users/odlav/Documents/catalyser-fase-3b1`.
- Artefato: `catalyser-fase-3b1-99a4b1a.zip`.
- Branch: `feature/fase-3b-transformacoes-ramos`.
- Commit: `99a4b1ac0c5cd72f953617d1603827270ee5745d`.
- SHA-256 do ZIP:
  `89527DAD98D81FDD163C3671322120579B695023D2960D4333F5D67171A294FB`.
- Inicialização: `shiny::runApp("inst/app", launch.browser = TRUE)`.
- Dados: `inst/app/dados/Treino-Transformacoes.xlsx`, aba `biometria`.
- Dimensões da Base Compartilhada: 71 linhas e 10 variáveis.

## Falha de importação encontrada antes do roteiro

A versão original `99a4b1a` abriu o arquivo Excel, mas falhou ao selecionar a
aba `biometria`:

```text
Aviso: Error in if: valor ausente onde TRUE/FALSE necessário
51: observe [inst/app/app.R#1643]
```

A condição comparava o input de tipagem durante a transição reativa sem rejeitar
explicitamente `NA`:

```r
if (!is.null(val) && val != types[[col_name]]) {
```

Para continuar a homologação, a cópia da VM recebeu a correção provisória:

```r
if (length(val) == 1L && !is.na(val) &&
    !identical(val, types[[col_name]])) {
```

Após a correção, a aba abriu com 71 linhas e 10 variáveis. A mesma correção foi
incorporada à fonte de trabalho para o próximo artefato. Portanto, o ZIP
`99a4b1a` não deve ser considerado aprovável sem essa correção.

## Resultado dos testes 3B/3B.1

| Bloco | Resultado | Evidência principal |
|---|---|---|
| 1. Editor sem replay automático | Aprovado | Ramo criado com origem `dados_analise`, cache `Não calculada`, dimensões vazias e prévia aguardando recálculo. |
| 2. Dicotomizar a resposta | Aprovado | `cpue_alta = as.integer(cpue >= 3)`; após recálculo, 71 linhas, 11 colunas e valores somente 0/1. |
| 3. Composição de etapas | Aprovado | Filtro `cpue_alta == 1` aceito após materialização; resultado com 7 linhas, confirmando 7 uns e 64 zeros. |
| 4. Ordem lógica e erro isolado | Aprovado | Filtro antes da dicotomização produziu `Com erro` e a mensagem `A coluna 'cpue_alta' não existe neste ponto.`; restaurar a ordem recuperou o ramo. |
| 5. Ativar, desativar e remover | Aprovado | Desativar devolveu 71 linhas; reativar devolveu 7; remover preservou apenas a dicotomização e deixou o cache desatualizado, sem replay automático. |
| 6. Calcular e usar coluna nova | Aprovado | Segundo ramo criado diretamente de `dados_analise`; bloqueio antes do primeiro recálculo funcionou; resultado final com 2 etapas, 71 linhas e 12 colunas. |
| 7. Bloqueio de base pronta | Aprovado | Desativar e remover foram recusados no estado `pronta`; após reabrir como rascunho, a edição voltou a funcionar. |
| 8. Limpeza confirmada | Aprovado | Cancelar preservou a receita; confirmar limpou somente o ramo selecionado; recálculo retornou esse ramo a 71 linhas e 10 colunas, sem alterar o ramo irmão. |

## Estado final observado

```text
Logística da CPUE:   rascunho · Atualizada · 1 etapa  · 71 linhas · 11 colunas
Gráficos biométricos: rascunho · Atualizada · 0 etapas · 71 linhas · 10 colunas
```

## Conclusão

O comportamento funcional das Fases 3B e 3B.1 foi aprovado no Windows 10 após
a correção local da importação. O replay permaneceu manual e restrito ao ramo
selecionado; erros foram isolados; a topologia em estrela foi preservada; e a
limpeza devolveu o ramo à Base Compartilhada sem afetar o irmão.

A homologação do artefato original é **condicional**, pois o ZIP `99a4b1a`
contém o bloqueio de importação. Produzir um novo artefato com a correção e
executar ao menos o teste de abertura da aba antes de promovê-lo.

## Revalidação do artefato corrigido — 25/07/2026

Foi produzido o artefato `catalyser-fase-3b1-corrigida-NA.zip`, contendo apenas
a correção explícita de `NA` sobre a versão histórica da Fase 3B.1. A sintaxe
do `app.R` foi validada com R 4.6.1 e o artefato foi aberto na mesma VM
Windows 10.

A importação de `Treino-Transformacoes.xlsx`, aba `biometria`, foi aprovada com
71 linhas e 10 colunas, sem o erro `valor ausente onde TRUE/FALSE necessário`.
Assim, a correção de importação está homologada e os oito blocos funcionais
anteriores não precisam ser repetidos.

## Aprendizado pedagógico incorporado

A sessão confirmou a utilidade da analogia da cozinha: `dados_analise` é a
massa-base compartilhada; cada Base Derivada é uma porção com receita própria;
recalcular prepara e permite conferir a porção; finalizar a deixa pronta para o
forno; a análise assa; e a Comunicação de Resultados serve a pizza acompanhada
da receita reprodutível. A analogia foi incorporada ao tutorial e à ajuda da IDE.
