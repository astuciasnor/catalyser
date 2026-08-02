# Documentação da CatalyseR

**Última revisão:** 27/07/2026

Este índice separa decisões vigentes, especificações factuais, guias de uso,
evidências de teste e memória histórica. Os documentos atuais registram o
commit em que foram verificados, mas não substituem a inspeção do código.

Em caso de divergência, prevalecem o comportamento, os testes e os módulos da
`main`. Documentos de `docs/historico/` nunca definem a interface vigente.

## Leitura essencial

1. [Decisões pedagógicas e de refinamento](DECISOES_PEDAGOGICAS_E_REFINAMENTO.md)
   — filosofia do produto, código humano e ciclos de duas análises.
2. [Pipeline de dados e relatórios](../PIPELINE_DADOS_E_RELATORIOS.md)
   — fluxo canônico da importação ao Word e ao Projeto R.
3. [Preparo de dados atual](../EVOLUCAO_TRATAMENTO_DADOS.md)
   — mapa factual da cozinha, estados reativos e topologia base/ramos.
4. [Comunicação de Resultados](../MODULO_COMUNICACAO_RESULTADOS.md)
   — registro de execuções, quatro sub-abas, QMD, Word e Projeto R.

## Preparo de dados

- [Contrato de entrada e preparo](../CONTRATO_DADOS_E_PREPARO.md)
- [Guia de planilha tidy](../GUIA_PLANILHA_TIDY.md)
- [Tutorial da Base Compartilhada e Bases Derivadas](TUTORIAL_BASE_COMPARTILHADA_E_DERIVADAS.md)
- [Decisão: Agrupar/Sumarizar como Base Derivada](DECISAO_AGRUPAR_COMO_BASE_DERIVADA.md)

Distinção vigente:

> **Arrumação estrutural compartilhada:** pivotar, separar, criar e organizar
> variáveis pode mudar a forma da tabela para torná-la adequada ao projeto.
> **Trilha de tratamentos compartilhada:** não reduz linhas, exceto remoção de
> duplicatas ou de linhas com NA.
> **Bases Derivadas:** recebem o preparo específico; filtros,
> agrupamentos/sumarizações e contingência só aparecem nelas, mas os demais
> tratamentos também podem ser repetidos quando forem específicos da análise.

## Produto e escopo futuro

- [Laboratório de Conceitos](../MODULO_LABORATORIO_CONCEITOS.md) — o menu e a
  visão geral já existem; os visualizadores indicados como “em construção”
  permanecem planejados e não fazem parte do ciclo atual de refinamento.

Planos V15/V16 e diagnósticos pontuais concluídos foram movidos para
[Histórico](historico/README.md). Eles preservam decisões e evidências, mas não
devem orientar uma implementação nova sem confronto com este índice e com a
`main`.

## Testes e homologação

- [Fases 3 — índice de roteiros e evidências](testes/fases-3/README.md)
- [Homologação ponta a ponta no Windows](testes/fases-3/HOMOLOGACAO_END_TO_END_WINDOWS_2026-07-26.md)
- [Revisão pré-main da V16](testes/fases-3/REVISAO_PRE_MAIN_V16_2026-07-27.md)
- [Roteiro ANOVA e dois valores de Y](testes/fases-3/ROTEIRO_TESTE_V16_ANOVA_E_DOIS_Y.md)

O Windows 11 do autor é o ambiente cotidiano. VM só deve ser presumida quando
for explicitamente indicada; instalações limpas periódicas continuam úteis.

## Skill versionada

A skill [refinar-analises-catalyser](../skills/refinar-analises-catalyser/SKILL.md)
orienta outra IA a aprimorar exatamente duas análises por ciclo, com
mini-refatorações e validação do app ao Word.

Ela possui duas referências:

- [critérios pedagógicos](../skills/refinar-analises-catalyser/references/criterios-pedagogicos.md);
- [piloto ANOVA + gráfico de linhas](../skills/refinar-analises-catalyser/references/piloto-anova-grafico-linhas.md).

## Regra de organização

- raiz: documentos canônicos citados por instruções do ecossistema, `README` e
  licença;
- `docs/`: decisões e guias atuais;
- `docs/testes/`: roteiros e evidências auditáveis;
- `docs/historico/`: planos concluídos, esboços e diagnósticos datados;
- `skills/`: fluxos reutilizáveis para agentes, com referências próprias.

Não mover documentos canônicos da raiz sem atualizar também `AGENTS.md`,
referências internas e testes que dependam do caminho. Ao atualizar um
documento factual, registrar versão/commit verificado e conferir os rótulos
diretamente nos módulos R.
