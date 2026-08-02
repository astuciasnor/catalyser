# Decisão de arquitetura — Agrupar/Sumarizar como Base Derivada

**Status:** implementada e homologada
**Verificado em:** CatalyseR 0.1.5, commit `6aa407a`, 27/07/2026

## Situação atual

**Agrupar / Sumarizar** aparece em
**Preparando Dados → Bases Derivadas → Receita da base**, dentro da caixa
**Tratamento a adicionar**. A etapa parte obrigatoriamente da Base Compartilhada,
fica registrada na receita do ramo nomeado e produz uma linha por grupo.

O fluxo compartilhado antigo, que promovia uma prévia anônima para
`base_resolvida`, não é o comportamento canônico.

## Razão da decisão

Sumarizar muda a unidade de observação: várias linhas podem virar uma linha por
grupo. Essa transformação costuma servir a uma finalidade específica e não
deve substituir silenciosamente a base comum usada pelas outras análises.

Exemplo: uma linha por desembarque pode virar uma linha por `ano × espécie`.
Essa tabela é apropriada para determinado gráfico ou série, mas não
necessariamente para teste t, regressão individual ou PCA.

O comportamento canônico é, portanto, manter a tabela agrupada como **Base
Derivada nomeada**, diretamente da Base Compartilhada (`dados_analise`). Várias
tabelas agrupadas podem coexistir na mesma sessão.

Exemplos:

- `base_cpue_ano_especie`;
- `base_producao_mensal`;
- `base_desembarque_local`.

## Fluxo de interface atual

1. Criar ou selecionar uma Base Derivada no **Registro de Bases**.
2. Abrir a sub-aba **Receita da base**.
3. Escolher **Agrupar / Sumarizar** em **Tratamento a adicionar**.
4. Selecionar variáveis de agrupamento, variáveis numéricas e medidas-resumo.
5. Adicionar a etapa à receita do ramo.
6. Manter a agregação como última etapa da receita, pois ela muda a unidade de
   observação.
7. Clicar em **Recalcular esta base** e conferir prévia e código.
8. Clicar em **Finalizar preparo** quando a receita estiver pronta.

## Contrato da nova base

Cada tabela agrupada deve registrar:

- ID estável;
- origem obrigatória `dados_analise`;
- nome amigável;
- nome R único;
- finalidade;
- parâmetros de agrupamento e sumarização;
- código R canônico;
- versão da receita;
- revisão da Base Compartilhada usada como origem;
- cache e dimensões da última prévia;
- estados `Não calculada`, `Atualizada`, `Desatualizada` e `Com erro`.

Não será permitido agrupar a partir de outra Base Derivada. A topologia permanece
em estrela.

## Limite da Base Compartilhada

Não há ação comum para promover uma sumarização à Base Compartilhada. Na
filosofia vigente, a sub-aba **Tratamentos e trilha** preserva as observações,
salvo remoção explícita de duplicatas ou de linhas com NA. A arrumação
estrutural compartilhada pode pivotar a tabela; filtros, agrupamentos,
sumarizações e tabelas de contingência pertencem às Bases Derivadas.

## Critérios de teste

- criar duas tabelas agrupadas com nomes diferentes;
- confirmar que ambas permanecem no Registro de Bases;
- confirmar que uma não altera a outra;
- confirmar que `dados_analise` não é modificado pelo fluxo padrão;
- impedir nomes R duplicados ou reservados;
- invalidar apenas a tabela cuja receita foi editada;
- deixar todas as tabelas derivadas desatualizadas quando a Base Compartilhada
  mudar;
- gerar código R reproduzível para cada tabela;
- impedir ramo de ramo;
- permitir que os seletores analíticos consumam apenas bases prontas e atualizadas.

## Histórico

A decisão nasceu após os testes da branch histórica
`feature/fase-3b-transformacoes-ramos`, no commit `99a4b1a`, e foi incorporada
posteriormente ao fluxo homologado. A referência à branch permanece apenas para
auditoria; novos trabalhos devem seguir a implementação da `main`.
