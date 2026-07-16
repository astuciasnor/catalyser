# Roteiro de teste — Fase 3B.3: seletores nas análises prioritárias

> Roteiro histórico da branch `feature/fase-3b3-seletores-prioritarios`.
> Consulte também a [matriz e a ordem completa de homologação](README.md).

## Objetivo

Validar no Zorin a expansão moderada do contrato da Fase 3B.2. A escolha de
base foi conectada a sete módulos sem alterar seus motores estatísticos:

- Estatística Descritiva;
- Regressão Linear Simples;
- Teste t de Student;
- Gráfico de Linhas;
- Qui-quadrado de independência;
- PCA;
- Análise de Agrupamentos (HCA).

A Regressão Logística conserva o seletor-piloto da Fase 3B.2.

## Conjunto de dados

Use `inst/app/dados/Treino-Transformacoes.xlsx`, aba `biometria`. Para o teste
de linhas, você também pode usar a aba `desembarques_largo` depois de arrumá-la
para o formato longo.

## Preparação dos ramos

Em **Preparando Dados → Bases Derivadas**, crie, recalcule e finalize pelo menos
três ramos diretos de `dados_analise`:

1. `base_geral_teste`, finalidade **Uso geral**, com uma transformação simples
   que produza uma coluna facilmente reconhecível;
2. `base_graficos`, finalidade **Gráficos**, com as variáveis necessárias ao
   gráfico de linhas;
3. `base_multivariada`, finalidade **PCA / Agrupamentos**, contendo várias
   colunas numéricas adequadas a PCA e HCA.

Para o Qui-quadrado, crie opcionalmente `base_qui_quadrado`, finalidade
**Qui-quadrado**, com duas variáveis categóricas. Todos os ramos devem nascer
diretamente de `dados_analise`; não existe base derivada de outra derivada.

## Teste 1 — padrão e elegibilidade

Abra cada um dos sete módulos. Confirme que:

- `Base compartilhada — dados_analise` é a seleção inicial;
- somente ramos **prontos** e com cache **Atualizado** aparecem;
- ramos em rascunho, não calculados, desatualizados ou com erro não aparecem;
- a finalidade compatível aparece primeiro com uma estrela, mas os demais ramos
  válidos continuam disponíveis;
- cada módulo conserva sua própria escolha.

## Teste 2 — o módulo usa realmente a base escolhida

Em cada módulo, selecione um ramo que tenha uma coluna exclusiva e confirme:

- o badge mostra o nome R e as dimensões da base;
- a coluna exclusiva aparece nas escolhas de variáveis;
- tabela, modelo ou gráfico muda de acordo com o ramo;
- voltar a `dados_analise` remove a coluna exclusiva das escolhas;
- a base compartilhada não foi modificada.

Na Regressão Linear, confira também a visualização do código: quando um ramo é
usado, a receita específica e o nome do objeto da base devem aparecer antes do
ajuste. A montagem completa da Trilha compartilhada ficará para a Fase 3E.

## Teste 3 — duas respostas no mesmo gráfico de linhas

Use a mesma base e a mesma variável de eixo X. Gere um gráfico com uma resposta
Y e depois troque para outra resposta Y.

Nesta fase, a tela deve atualizar corretamente o gráfico a cada troca. Ela
ainda não cria dois itens independentes no relatório: isso será responsabilidade
do registro de execuções da Fase 3C e da seleção de conteúdo da Fase 3D.

## Teste 4 — particularidade do Qui-quadrado

No Qui-quadrado:

1. escolha a fonte **Duas variáveis** e confirme que as variáveis vêm da base
   selecionada no novo seletor;
2. mude para **Tabela preparada** e confirme que o módulo usa a tabela produzida
   no preparo de contingência;
3. mude para **Entrada manual** e confirme que os valores digitados são usados.

O aviso acima do módulo deve explicar essa diferença. O seletor não substitui
as fontes próprias de tabela preparada ou entrada manual.

## Teste 5 — troca sem replay

Alterne várias vezes entre `dados_analise` e os ramos. Confirme que:

- a troca é imediata;
- o horário da última prévia no Gerenciador de Bases não muda;
- nenhuma seleção dispara recálculo;
- não ocorre recomputação em cascata.

## Teste 6 — invalidação segura

Com um ramo selecionado em um dos módulos:

1. reabra o ramo como rascunho, ou altere a Trilha compartilhada;
2. volte ao módulo analítico.

Resultado esperado:

- uma notificação informa que a base deixou de estar pronta/atualizada;
- o módulo volta explicitamente para `dados_analise`;
- a última prévia antiga não alimenta a análise;
- o app continua funcionando.

Repita o teste excluindo o ramo selecionado. O comportamento deve ser o mesmo.

## Teste 7 — regressão logística preservada

Repita os pontos essenciais do `ROTEIRO_TESTE_FASE_3B2.md` e confirme que o
seletor interno da Regressão Logística continua funcionando, inclusive a
sugestão de finalidade `reg_logistica` e o retorno seguro à base compartilhada.

## Módulos fora do escopo da 3B.3

Os módulos não listados neste roteiro continuam lendo `dados_analise`. Isso é
intencional: a expansão foi limitada aos pontos prioritários para reduzir o
risco antes dos testes no Zorin.

Ainda não fazem parte desta fase:

- botão **Adicionar aos resultados**;
- duas execuções independentes do mesmo gráfico/análise;
- registro de parâmetros e saídas;
- seleção do que entra no relatório Word;
- exportação integrada de todas as bases, receitas e execuções.

## Critérios de aprovação

- os sete módulos prioritários usam o mesmo componente de seleção;
- somente caches prontos e atuais alimentam análises;
- a finalidade sugere, mas não impõe uma base;
- cada módulo resolve o ramo pelo ID interno estável;
- alternar bases não executa replay;
- invalidação ou exclusão causa aviso e retorno seguro a `dados_analise`;
- o Qui-quadrado respeita suas três fontes;
- a Regressão Logística da 3B.2 não sofre regressão;
- módulos fora do escopo mantêm o comportamento anterior.
