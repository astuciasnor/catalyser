# Roteiro de teste — Fase 3B.1: Receitas das Bases Derivadas

> Roteiro histórico da branch `feature/fase-3b-transformacoes-ramos`. Consulte
> também a [matriz e a ordem completa de homologação](README.md).

## Objetivo

Validar no Zorin OS a edição das transformações específicas de cada ramo, sem
conectar ainda as bases derivadas aos módulos de análise. Esta fase preserva a
topologia em estrela e o cache lazy implementados nas Fases 3A e 3A.1.

## Conjunto de dados

Use `inst/app/dados/Treino-Transformacoes.xlsx`, aba `biometria`.

Variáveis principais deste roteiro:

- `cpue`: numérica, usada para criar uma resposta 0/1;
- `peso_g` e `comprimento_cm`: numéricas, usadas para cálculo e padronização;
- `especie` e `local`: categóricas, usadas para texto e filtro;
- `id`: identificador, útil para verificar que as linhas continuam rastreáveis.

## Preparação

1. Inicie a CatalyseR no Zorin.
2. Importe a aba `biometria`.
3. Abra **Preparando Dados → Bases Derivadas**.
4. Crie o ramo:
   - nome amigável: `Logística da CPUE`;
   - finalidade: `Regressão logística`;
   - nome R: `base_reg_logistica`.
5. Selecione a linha criada no Registro de Bases.

## Teste 1 — editor sem replay automático

Sem clicar em **Recalcular esta base**, navegue pelo editor e alterne entre os
tipos de tratamento.

Confirme que:

- o cache permanece `Não calculada`;
- as dimensões continuam vazias;
- a aba de prévia continua pedindo recálculo;
- o formulário informa que usa `dados_analise` ou nomes previstos;
- não existe campo para escolher outra base como origem.

## Teste 2 — dicotomizar a resposta

Adicione à receita:

- tratamento: `Dicotomizar (0/1)`;
- coluna: `cpue`;
- definição: `Por limiar`;
- operador: `>=`;
- limiar: `5`;
- nova variável: `cpue_alta`.

Antes de recalcular, confirme que:

- a coluna `Etapas` do registro passa para `1`;
- o ramo permanece `rascunho`;
- o código do ramo já contém a criação de `cpue_alta`;
- o cache continua sem materialização oficial;
- se havia uma prévia anterior, ela fica preservada como `Desatualizada`.

Clique em **Recalcular esta base** e confirme que:

- o cache muda para `Atualizada`;
- a prévia contém `cpue_alta` somente com 0 e 1;
- `dados_analise` não ganha essa coluna;
- outros ramos não ganham essa coluna.

## Teste 3 — composição de etapas

Depois do recálculo anterior, adicione:

- tratamento: `Filtrar linhas`;
- coluna: `cpue_alta`;
- condição numérica: `==`;
- valor: `1`.

Confirme que o clique em adicionar aceita a coluna criada pela primeira etapa
porque o cache anterior estava atualizado, mas não substitui a prévia oficial.
O cache deve ficar `Desatualizada` até novo clique em **Recalcular esta base**.

Após recalcular, todas as linhas da prévia devem ter `cpue_alta == 1`.

## Teste 4 — ordem lógica e erro isolado

1. Selecione a etapa de filtro.
2. Mova-a para cima, antes da dicotomização.
3. Confira que a receita muda sem recálculo automático.
4. Clique em **Recalcular esta base**.

Resultado esperado: o ramo fica `Com erro`, pois `cpue_alta` ainda não existe
quando o filtro é executado. O app não deve travar, `dados_analise` permanece
disponível e os ramos irmãos continuam funcionando.

Mova o filtro novamente para depois da dicotomização e recalcule. O ramo deve
voltar a `Atualizada`.

## Teste 5 — ativar, desativar e remover

1. Desative o filtro.
2. Confirme que ele aparece marcado como inativo e que o código R deixa de
   executá-lo.
3. Recalcule e confira que voltam a aparecer linhas com `cpue_alta` igual a 0.
4. Ative o filtro novamente e recalcule.
5. Remova o filtro e confira que somente a dicotomização permanece.

Cada operação deve invalidar somente o cache deste ramo. Nenhuma delas deve
executar replay por conta própria.

## Teste 6 — calcular e usar uma coluna nova

Crie um segundo ramo:

- nome amigável: `Gráficos biométricos`;
- finalidade: `Gráficos`;
- nome R: `base_graficos_biometria`.

Adicione:

1. `Calcular variável`: nome `fator_condicao`, fórmula
   `100 * peso_g / comprimento_cm^3`.
2. Tente adicionar imediatamente a padronização. A IDE deve pedir que o ramo
   seja recalculado, sem executar replay silencioso.
3. Clique em **Recalcular esta base**.
4. Adicione `Padronizar / Escalar`: coluna `fator_condicao`, método `Escore z`,
   nome `fator_condicao_z`.

O segundo passo deve ser validado contra o cache já atualizado. Depois de
adicioná-lo, o cache volta a ficar desatualizado. Após novo recálculo, as duas
colunas devem aparecer somente nesse ramo.

## Teste 7 — bloqueio de base pronta

1. Com um ramo `Atualizada`, clique em **Finalizar preparo**.
2. Tente adicionar, mover, desativar ou remover uma etapa.

As alterações devem ser recusadas com a orientação para reabrir o ramo como
rascunho. Depois de **Reabrir como rascunho**, a edição deve voltar a funcionar.

## Teste 8 — limpeza confirmada

1. Em um ramo com etapas, clique em **Limpar receita**.
2. Cancele e confirme que nada mudou.
3. Repita e confirme a limpeza.

Somente a receita selecionada deve ser esvaziada. O ramo volta a ser equivalente
a `dados_analise`, mas o cache permanece desatualizado até o recálculo manual.

## Critérios de aprovação

- nenhum travamento do Shiny;
- nenhum ramo de ramo;
- edição permitida somente em `rascunho`;
- validação usa o mesmo registro canônico da Trilha;
- adicionar uma etapa não executa replay nem grava prévia oficial;
- uma etapa dependente exige recálculo manual da anterior;
- ordenar, ativar, remover e limpar não executam replay automático;
- toda mudança de receita incrementa sua versão e invalida apenas o ramo;
- erros de dependência ficam isolados no ramo;
- o código R acompanha imediatamente a receita e ignora etapas inativas;
- o recálculo manual materializa a receita na ordem mostrada;
- `dados_analise` e os ramos irmãos não são modificados;
- os módulos analíticos continuam usando `dados_analise` nesta fase.

## Fora desta fase

Ainda não espere:

- escolher a base derivada dentro de uma análise;
- registrar uma execução analítica;
- selecionar gráficos, tabelas ou narrativas para o relatório;
- exportar as receitas dos ramos no Projeto R final.

O seletor de base nos módulos analíticos será a Fase 3B.2.
