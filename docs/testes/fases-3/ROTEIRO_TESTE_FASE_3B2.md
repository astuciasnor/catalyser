# Roteiro de teste — Fase 3B.2: Seletor piloto de base

> Roteiro histórico da branch `feature/fase-3b2-seletor-base-piloto`. Consulte
> também a [matriz e a ordem completa de homologação](README.md).

## Objetivo

Validar no Zorin o primeiro vínculo entre uma base derivada e um módulo
analítico. O piloto está restrito à **Regressão Logística** para que o contrato
seja testado antes de ser replicado.

## Conjunto de dados

Use `inst/app/dados/Treino-Transformacoes.xlsx`, aba `biometria`.

## Preparar a base piloto

Em **Preparando Dados → Bases Derivadas**:

1. crie `Logística da CPUE`, finalidade `Regressão logística`, nome R
   `base_reg_logistica`;
2. adicione uma dicotomização de `cpue` com operador `>=`, limiar `5` e nova
   variável `cpue_alta`;
3. clique em **Recalcular esta base**;
4. confira `cpue_alta` com valores 0/1;
5. clique em **Finalizar preparo**.

Crie também um ramo de controle, com finalidade `Gráficos`, recalcule e finalize.
Ele permite conferir a ordem das sugestões.

## Teste 1 — opções elegíveis

Abra **Modelos de Regressão → Regressão Logística**.

O seletor **Base utilizada** deve mostrar:

1. `Base compartilhada — dados_analise`;
2. `★ Logística da CPUE — base_reg_logistica`;
3. o ramo de gráficos pronto, sem estrela, depois da sugestão logística.

Ramos em rascunho, não calculados, desatualizados ou com erro não devem aparecer.

## Teste 2 — base compartilhada como padrão

Sem alterar o seletor, confirme que:

- `dados_analise` está selecionada;
- aparece o aviso de base compartilhada;
- `cpue_alta` não aparece entre as variáveis, salvo se tiver sido criada também
  na Trilha compartilhada;
- a Regressão Linear Simples continua sem seletor e sem mudança de comportamento.

## Teste 3 — executar com a base derivada

Selecione `base_reg_logistica` e confira:

- o badge mostra o nome R e as dimensões da base;
- `cpue_alta` passa a aparecer nas escolhas de Y e X;
- escolha Y = `cpue_alta` e X = `comprimento_cm`;
- o modelo logístico, a tabela e o gráfico são atualizados usando o ramo;
- `dados_analise` continua sem a coluna específica.

Abra a visualização do código R do módulo. O código deve:

- identificar que uma base derivada foi escolhida;
- conter a receita que cria `base_reg_logistica`;
- usar esse objeto como `dados` antes do ajuste logístico;
- informar que a Trilha compartilhada completa será montada pelo Projeto R
  integrado em fase posterior.

## Teste 4 — alternar entre bases

Alterne entre `dados_analise` e `base_reg_logistica`.

Confirme que:

- as listas de variáveis acompanham o conjunto escolhido;
- escolhas que não existem na nova base são substituídas por variáveis válidas;
- nenhuma troca recalcula o ramo;
- os horários do cache não mudam.

## Teste 5 — ramo deixa de estar disponível

1. deixe `base_reg_logistica` selecionada na Regressão Logística;
2. volte a Bases Derivadas;
3. clique em **Reabrir como rascunho**;
4. retorne à Regressão Logística.

Resultado esperado:

- aparece uma notificação clara;
- o seletor retorna explicitamente a `dados_analise`;
- a última prévia do ramo não alimenta o modelo;
- o app não trava.

Recalcular sem finalizar ainda não deve devolver o ramo ao seletor. Ele só volta
depois de **Finalizar preparo**.

## Teste 6 — origem compartilhada muda

1. selecione novamente `base_reg_logistica`;
2. altere uma etapa da Trilha compartilhada;
3. volte à Regressão Logística.

O ramo agora está desatualizado. A análise deve avisar e voltar para
`dados_analise`. Depois de recalcular e finalizar o ramo, ele volta às opções.

## Teste 7 — exclusão segura

Com o ramo selecionado, exclua-o no Gerenciador de Bases. O módulo piloto deve
retornar à base compartilhada com aviso e remover o ramo das opções, sem erro.

## Critérios de aprovação

- `dados_analise` é sempre a opção padrão;
- somente ramos prontos e atualizados aparecem;
- a finalidade logística é sugerida primeiro, mas não imposta;
- o ID interno, e não o nome amigável, sustenta a seleção;
- modelo, tabela e gráficos usam o mesmo `data.frame` selecionado;
- alternar bases não executa replay;
- ramo inválido nunca alimenta a análise;
- indisponibilidade provoca aviso e retorno explícito à base compartilhada;
- a Regressão Linear Simples permanece inalterada;
- o contexto da base fica disponível para o futuro registro de execuções.

## Fora desta fase

Ainda não estão incluídos:

- seletor de base nos demais módulos;
- botão **Adicionar aos resultados**;
- registro persistente da execução logística;
- Comunicação de Resultados orientada pelas execuções;
- exportação integrada da Trilha compartilhada, de todos os ramos e das análises.
