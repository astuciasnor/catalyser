# Roteiro de teste — Fase 3C.1: execução explícita e rascunho

> Roteiro histórico da branch `feature/fase-3c1-execucao-explicita`. Consulte
> também a [matriz e a ordem completa de homologação](README.md).

## Objetivo

Confirmar que a CatalyseR não interpreta as primeiras variáveis compatíveis
como uma escolha analítica do usuário. A configuração só produz resultados
após **Executar análise**, e somente uma prévia atualizada pode ser registrada.

Módulos cobertos:

- Estatística Descritiva;
- Regressão Linear Simples;
- Regressão Logística;
- Teste t de Student;
- Gráfico de Linhas;
- Qui-quadrado de independência;
- PCA;
- Análise de Agrupamentos (HCA).

## Conjunto de dados

Use `inst/app/dados/Treino-Transformacoes.xlsx`, principalmente a aba
`biometria`. Para o fluxo de interrupção, são úteis:

- `comprimento_cm`;
- `peso_g`;
- `cpue`;
- `profundidade_m`;
- `sexo` e outras variáveis categóricas após a padronização necessária.

## Teste 1 — abrir não calcula

1. abra um dos oito módulos;
2. observe que os seletores podem sugerir variáveis compatíveis;
3. não clique em **Executar análise**.

Resultado esperado:

- o painel informa que nenhum resultado foi calculado;
- tabela, teste, modelo ou gráfico não aparece;
- o cartão de registro informa **Aguardando execução da análise**;
- **Adicionar aos resultados** está desabilitado;
- os arquivos Word/Projeto R do módulo permanecem bloqueados.

## Teste 2 — primeira execução consciente

1. escolha conscientemente a base e as variáveis;
2. ajuste as opções analíticas;
3. clique em **Executar análise**.

Resultado esperado:

- o botão passa a se chamar **Executar novamente**;
- o painel mostra os resultados;
- o status informa que a prévia está pronta para ser registrada;
- **Adicionar aos resultados** fica habilitado.

## Teste 3 — alteração não recalcula silenciosamente

Depois de executar:

1. troque X, Y, o agrupamento ou uma opção que afete a análise;
2. não execute novamente.

Resultado esperado:

- a prévia anterior fica oculta;
- aparece **A base ou a configuração mudou**;
- o status fica pendente;
- adicionar, atualizar e salvar como novo ficam bloqueados;
- a execução registrada anteriormente, se houver, não é apagada.

Clique em **Executar novamente**. A nova prévia deve aparecer e o registro deve
ser liberado outra vez.

## Teste 4 — interromper para criar uma variável

No Gráfico de Linhas ou na Regressão Linear:

1. escolha X = `comprimento_cm` e Y = `peso_g`;
2. execute a análise, mas não precisa registrá-la;
3. volte a **Preparando Dados → Calcular / Reescalar Variável**;
4. crie, por exemplo, `peso_kg = peso_g / 1000`;
5. clique no comando do módulo que promove o resultado para uso nas análises;
6. retorne à análise interrompida.

Resultado esperado:

- X e Y antigos permanecem selecionados se ainda existirem;
- a prévia anterior está pendente e oculta;
- `peso_kg` aparece entre as variáveis disponíveis;
- nenhuma nova análise foi calculada automaticamente.

Selecione Y = `peso_kg` e clique em **Executar novamente**. Só então a nova
variável deve integrar a prévia e poder ser registrada.

## Teste 5 — interrupção usando uma base derivada

1. execute uma análise usando um ramo pronto;
2. altere a Trilha compartilhada ou a fonte de `dados_analise`;
3. retorne ao módulo.

Resultado esperado:

- o ramo antigo fica desatualizado e deixa de ser oferecido;
- a análise volta explicitamente para `dados_analise` quando necessário;
- a prévia fica pendente;
- nenhuma base derivada de outra base derivada é criada.

No Registro de Bases, use **Recalcular** e **Finalizar preparo**. Depois retorne,
escolha o mesmo ramo e execute novamente.

## Teste 6 — registrar somente a última execução

1. execute uma análise válida;
2. clique em **Adicionar aos resultados**;
3. altere uma variável;
4. tente registrar antes de executar novamente;
5. execute e então use **Atualizar resultado** ou **Salvar como novo**.

Resultado esperado:

- o passo 4 fica bloqueado;
- **Atualizar resultado** conserva o ID e aumenta a versão;
- **Salvar como novo** cria outro ID;
- a execução anterior não muda apenas porque o rascunho mudou.

## Teste 7 — personalizações visuais

Depois de executar uma análise estatística, navegue entre suas abas e ajuste
somente opções visuais, como tema ou exibição de rótulos.

Resultado esperado:

- mudar de aba não torna a análise pendente;
- opções puramente visuais podem atualizar a apresentação sem reajustar o teste
  ou modelo;
- no Gráfico de Linhas, que é o próprio resultado, alterações visuais exigem
  **Executar novamente**.

## Teste 8 — repetir nos oito módulos

Em cada módulo coberto, confirme pelo menos:

1. abrir não calcula;
2. executar mostra resultado;
3. mudar variável deixa pendente;
4. executar novamente atualiza;
5. somente a prévia atual pode ser registrada.

## Critérios de aprovação

- nenhuma combinação automática de variáveis gera resultado inicial;
- o cálculo depende de clique explícito;
- a base e os parâmetros da última execução ficam congelados;
- alterações analíticas ou de dados não modificam a prévia silenciosamente;
- seleções válidas sobrevivem à ida ao preparo e ao retorno;
- resultados pendentes não podem alimentar o registro;
- o registro continua leve e não duplica dados, modelos ou gráficos;
- o comportamento anterior de bases desatualizadas e recálculo manual continua
  funcionando.

## Fora desta fase

- lista conjunta no menu Comunicação;
- escolha de narrativa, tabela, gráfico e diagnósticos para o Word;
- renderização integrada do `.docx`;
- Projeto R orientado pelo novo registro.

Essas entregas permanecem nas Fases 3D e 3E.
