# Decisão de arquitetura — Agrupar/Sumarizar como Base Derivada

## Situação atual

O módulo **Agrupar/Sumarizar** recebe `base_resolvida`, gera uma prévia temporária
e oferece **Usar este resultado nas análises**. Ao confirmar, substitui a camada
que alimenta a Base Compartilhada. O resultado não recebe nome e uma nova promoção
substitui a anterior.

## Problema

Sumarizar normalmente muda a unidade de observação: várias linhas viram uma linha
por grupo. Essa transformação costuma servir a uma finalidade específica e não
deve substituir silenciosamente a base comum usada pelas outras análises.

Exemplo: uma linha por desembarque pode virar uma linha por `ano × espécie`. Essa
tabela é apropriada para determinado gráfico ou série, mas não necessariamente
para teste t, regressão individual ou PCA.

## Decisão

O comportamento padrão será salvar a tabela agrupada como **Base Derivada
nomeada**, diretamente da Base Compartilhada (`dados_analise`). Deve ser possível
manter várias tabelas agrupadas na mesma sessão.

Exemplos:

- `base_cpue_ano_especie`;
- `base_producao_mensal`;
- `base_desembarque_local`.

## Fluxo de interface proposto

1. Selecionar variáveis de agrupamento, variáveis numéricas e medidas-resumo.
2. Clicar em **Gerar prévia da tabela agrupada**.
3. Conferir resultado e código R.
4. Informar nome amigável, finalidade e nome R sugerido.
5. Clicar em **Salvar como Base Derivada**.
6. Abrir ou destacar a nova entrada no Registro de Bases.
7. Finalizar o preparo quando a receita estiver pronta.

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

## Exceção compartilhada

Aplicar uma tabela agrupada à Base Compartilhada pode permanecer como ação
avançada e secundária, somente quando a nova unidade de observação for realmente
comum a todas as análises. A ação deve:

- usar rótulo explícito, como **Substituir a Base Compartilhada**;
- explicar a mudança da unidade de observação;
- exigir confirmação;
- informar que as Bases Derivadas ficarão desatualizadas.

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

## Sequenciamento

Esta mudança não deve alterar retroativamente a branch histórica
`feature/fase-3b-transformacoes-ramos` no commit `99a4b1a`, que ainda será
retestada na VM Windows 10. A implementação entra em uma revisão posterior e
recebe roteiro próprio de migração e homologação.
