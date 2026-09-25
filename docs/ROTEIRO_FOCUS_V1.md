# CatalyseR — roteiro de pequenas atividades

Registrado em 21/09/2026. O autor autorizou, no recorte da F01, a implementação
das três análises de Frequências e Proporções; outras mudanças continuam exigindo
autorização específica.

Este é o lugar para guardar a fila, não uma lista para copiar inteira no Focus
To-Do. O escopo e as referências ficam em
[Prioridades e expansões](PLANO_PRIORIDADES_E_EXPANSOES.md).

## Para quem estamos construindo

Para alunos, alunas e professores que precisam compreender a pergunta, escolher
uma análise adequada e reconhecer os cálculos no código. O critério de qualidade
é correção, clareza e reprodução do resultado — não atender a todo desejo de um
especialista. Limitações explícitas fazem parte de uma boa v1.

Nenhum projeto fica imune a críticas. Uma crítica pode indicar uma correção
necessária, uma melhoria de clareza ou uma expansão opcional: não são a mesma coisa.
Corrigir resultados errados ou interpretações enganosas tem prioridade; ampliar o
catálogo depende de escolha, não de pressão para parecer completo.

## Avaliação da proposta recebida de outra IA

- **Acolher:** as três análises de proporções já aprovadas como prioridade;
  conectar pergunta, desenho e análise; explicitar denominadores, métodos e
  incerteza; aproveitar o que já existe.
- **Não adotar como estimativa:** “muito baixo”, “quase de graça” e “pouco código”.
  Cálculo, interface, validação, Comunicação e exportação são trabalhos distintos.
  A proposta externa não comprova sozinha o estado atual do código.
- **Não transformar em condição de lançamento:** regressão múltipla e um pacote
  completo de poder/dimensionamento para t, ANOVA e proporções. Permanecem em
  lotes posteriores, com autorização própria.
- **Conferir agora, ampliar depois:** compatibilidade entre planejamento e análise.
  Identificar e sinalizar desenhos sem suporte não equivale a implementar todos.
- **Manter McNemar em Testes Não Paramétricos**, conforme decisão do autor.
  É uma etapa posterior, não parte das três primeiras análises independentes.
- **Preservar o existente:** expansões pesqueiras futuras não significam retirar
  ou refazer peso–comprimento e crescimento já presentes. Revalidar o inventário
  antes de implementar qualquer suposta ausência.
- **Sem prazo imposto:** o marco de setembro citado pela outra IA não vira uma
  obrigação diária nem uma promessa de entrega deste roteiro.

## Ritmo e tamanho da fila visível

- Escolher um bloco de **25 ou 40 minutos**, conforme a tarefa e a disponibilidade.
- Um bloco já é uma sessão válida. Um segundo é opcional; não é preciso completar
  dois, recuperar dias ou acumular dívida de produtividade.
- Mostrar **duas atividades por vez**, no máximo três pendentes no Focus. Se o
  autor preferir, mostrar apenas uma. Não repor a fila antes de ele pedir.
- Ao terminar o tempo, registrar o que avançou e o próximo ponto de retomada.
  Pausar uma tarefa incompleta não transforma o bloco em fracasso.
- Os minutos são orçamento de atenção do autor, **não promessa de duração da
  programação**. Trabalho técnico maior deve ser dividido e validado pelo agente.
- O agente prepara propostas, código e evidências quando autorizado; o autor
  escolhe o recorte e faz a revisão científica/didática. Não transferir toda a
  depuração para o tempo de foco do autor.

Um bloco de foco não é um ciclo de desenvolvimento. A convenção de refinamento
em duas análises pode atravessar várias sessões; não exige duas análises por dia.
Planejar a primeira dupla com uma e duas proporções. Aderência vem depois; não
inventar uma quarta análise só para preencher uma dupla. Definir o pareamento
do ciclo seguinte com o autor quando chegar a hora.

## Ordem de trabalho guardada

1. Delimitar Frequências e Proporções e escolher o primeiro exemplo.
2. Desenvolver uma proporção; depois duas proporções, usando a experiência da
   primeira. Integrar e validar a dupla antes de abrir outra frente.
3. Desenvolver aderência em um lote seguinte. A definição de seu ciclo não é
   necessária para iniciar a primeira dupla.
4. Conferir o percurso completo e os limites de desenho de cada entrega.
5. Só então escolher uma prioridade posterior. Nada da fila futura entra sozinho.

Não reabrir toda a interface nem toda a arquitetura de preparo. Defeito comprovado
no caminho em uso deve receber uma tarefa pequena e específica.

## Primeira retirada — somente duas atividades

### F01 — Reestruturar Menus e Módulos da CatalyseR · 25 minutos

O autor confere a localização do grupo e revisa uma execução de cada análise. O
agente mantém o Qui-quadrado de independência, com Fisher opcional, no mesmo
motor, execução e exportação; implementa **uma proporção**, **duas proporções**
e **Qui-quadrado de aderência** como módulos completos. As três exigem unidades
independentes, registram a configuração e podem ser levadas ao Projeto R.
McNemar continua em Testes Não Paramétricos quando for implementado. Preparar
Dados será testado em etapa separada.

**Pronto quando:** o autor aprovar a posição e percorrer, com uma base de exemplo,
uma das novas telas e sua exportação; o agente confirmar cálculo, limites e um
único acesso ao Qui-quadrado de independência.

### F03 — Escolher o exemplo de uma proporção · 25 minutos

O agente procura um exemplo documentado no EAPADados e apresenta a pergunta,
o que conta como sucesso, o denominador e a unidade observada. O autor aprova
ou ajusta a pergunta e a interpretação. Se nenhum exemplo servir, registrar a
lacuna e decidir a fonte em outra tarefa, sem inventar dados reais.

**Pronto quando:** houver uma pergunta e um exemplo com origem e limites claros.
Não usar a base de abalone balanceada por sexo para estimar a razão sexual natural.
Esta escolha pode ocorrer antes de qualquer autorização para alterar código.

## Pequenas tarefas reservadas — não copiar todas no Focus

Depois de F01/F02, solicitar autorização para um recorte concreto de implementação.
Não interpretar aprovação de nomes ou exemplos como autorização para programar.

Para cada análise A (uma proporção), B (duas proporções) e C (aderência), usar a
sequência abaixo. Liberar apenas as próximas tarefas necessárias. Se a inspeção
mostrar que uma etapa já está correta, conferir e registrar, sem refazê-la.

| ID | Tarefa pequena | Critério de término | Foco do autor |
|---|---|---|---|
| A1/B1/C1 | Aprovar entradas, resultado e limites | Uma ficha curta com pergunta, desenho suportado, entradas e saídas essenciais | 25 min |
| A2/B2/C2 | Conferir um resultado de referência | Agente apresenta cálculo e referência independente; autor confere interpretação | 25 min |
| A3/B3/C3 | Revisar uma tela funcional | Entradas e resultado legíveis, método explícito e mensagens de erro compreensíveis | 25 min |
| A4/B4/C4 | Conferir a troca de configuração | Trocar variável/grupo atualiza o resultado e permite registrar uma nova análise sem sobrescrever a anterior | 25 min |
| A5/B5/C5 | Conferir uma análise na Comunicação | Estimativas, rótulos e escolhas coincidem com a execução selecionada | 25 min |
| A6/B6/C6 | Ler o código exportado de um exemplo | Autor consegue reconhecer dados, cálculo e apresentação; agente demonstra reprodução | 40 min |
| A7/B7/C7 | Revisar uma página do relatório | Resultado reproduzido no projeto exportado, texto e tabela compreensíveis; figura só quando ajudar | 25 min |
| A8/B8/C8 | Aceitar a entrega ou apontar um ajuste | Evidências técnicas disponíveis e revisão didática registrada, com pendências delimitadas | 25 min |

O agente deve preparar cada revisão antes de colocá-la no Focus. Essas tarefas
não pressupõem funcionalidades prontas: programação, testes e correções são
dependências técnicas, executadas apenas no recorte autorizado. Para trabalho
conjunto, um bloco de 40 minutos pode ser dedicado a uma única dependência,
com um ponto de retomada se não terminar. Não chamar entrega incompleta de pronta.

### Cuidados específicos a incluir nas fichas

- **A:** sucessos e total válido, referência da hipótese, método do IC/teste,
  limites 0 e total, denominador inválido e dados faltantes.
- **B:** dois grupos independentes, diferença de proporções e IC; definir método
  e tratamento de pequenas contagens. Dados pareados não seguem este caminho.
- **C:** categorias observadas/esperadas alinhadas, soma das proporções esperadas,
  contagens e restrições de aproximação. Não misturar aderência com independência.

O agente verifica esses casos com testes proporcionais à mudança, antes da revisão
do autor. Uma tela bonita ou um único exemplo funcionando não bastam.

### Meta permanente, aplicada sem virar uma lista infinita

Em cada ficha: identificar unidade amostral/experimental e cenário suportado;
explicar limites para pares, blocos, estratos ou agrupamentos; preservar a origem
da base. Não oferecer inferência incompatível como se estivesse validada.

Reservar uma revisão curta de um desenho por vez para mapear planejamento →
análise, começando por DBC. Cada revisão termina com “atendido”, “limitado” ou
“não atendido” e a orientação necessária. Isso não autoriza novos modelos.

## Depois do primeiro lote — escolher apenas uma frente

Ordem sugerida, sujeita à escolha do autor:

1. Amostragem nas bases derivadas: conferir o que existe; integrar alocação
   proporcional; validar sorteio e reprodução. Neyman fica em sublote separado.
2. McNemar em Testes Não Paramétricos: cenário pareado, exemplo e percurso completo.
3. Dimensionamento por precisão para uma proporção; depois decidir média e poder
   a priori para um desenho específico, sem abrir t e ANOVA simultaneamente.

Regressão múltipla, Welch/Games–Howell e Friedman ficam reservados para a revisão
de suas famílias. Outras expansões e a comparação de modelos orientada por
hipóteses continuam no plano principal. Esta fila não é condição para fechar v1.

## Estado para a próxima conversa

**Etapa atual:** F01, **Reestruturar Menus e Módulos da CatalyseR**, está
concluída, aprovada e testada pelo autor. F02, **Reformular Menu Explorando os
Dados (antigo Descrevendo Dados)**, está em andamento. Não oferecer outra
atividade até novo pedido do autor.
**Autorização para código neste roteiro:** concedida para a F01: implementação
completa de uma proporção, duas proporções e Qui-quadrado de aderência, inclusive
limites de desenho, registro e Projeto R. Nenhum outro recorte foi autorizado.
**Atividades disponíveis:** nenhuma nova atividade está sendo oferecida. F03
permanece guardada para quando o autor pedir.

| Atividade | Estado | Evidência / decisão |
|---|---|---|
| F01 — Reestruturar Menus e Módulos da CatalyseR | Concluída — aprovada e testada pelo autor | Em 21/09/2026, o autor abriu a CatalyseR local e aprovou a nova organização de menus e módulos, inclusive Frequências e Proporções. O agente havia verificado sintaxe, cálculos de referência, registro e exportação. Não há McNemar ainda: ele continua explicitamente reservado a Testes Não Paramétricos. |
| F02 — Reformular Menu Explorando os Dados (antigo Descrevendo Dados) | Em andamento | Recorte aberto pelo autor para transformar a descrição em exploração orientada por perguntas, retratos recomendados e encaminhamentos pedagógicos, sem escolher o teste pelo aluno. Ainda não há aprovação visual ou didática desta F02. |
| F03 | Parcialmente preparada — ainda não iniciada no Focus | A tela real de Uma proporção já existe como resultado técnico da F01. Falta somente a escolha didática do exemplo, pergunta, sucesso, denominador e unidade observada; não há aprovação científica desse exemplo. Agente verificou em 21/09/2026 `EAPADados::lagostas_kelp_sexo`: 1.457 lagostas (663 fêmeas, 794 machos), origem documentada e limite de armadilhas/sítios. |

**Registro da retomada — 21/09/2026.** O agente preparou as propostas de F01 e
F02 sem alterar código. Verificação técnica não equivale à aprovação científica ou
didática do autor. Próximo passo pequeno: o autor revisa os dois cartões abaixo;
se uma decisão não for tomada, registrar a preferência já definida e retomar só o
ponto restante em outro bloco. F01 foi iniciada pelo autor como teste de Preparar
Dados; F02 fica reservada como a próxima atividade, sem criar outras pendências.

**Preparação visual — 21/09/2026.** Antes de F01/F02, o agente adicionou somente
a estrutura vazia de **Frequências e Proporções** após **Descrevendo Dados**
(ícone de gráfico de pizza) e moveu o atalho de Ajuda ao lado de Sobre. Não foram
criadas análises nem módulos. Agente verificou a sintaxe de `app.R`; o autor
aprovou a aparência, a exposição dos menus e o ícone. Esta aprovação visual não
aprova ainda a ordem pedagógica das análises dentro do grupo.

**Marco de trajetória — 21/09/2026.** O commit `6add73f` (`docs: registrar
roteiro Focus v1`) registra o plano de prioridades e este roteiro. Ele não inclui
as outras mudanças pendentes do repositório. Próximo passo técnico de F01,
somente se o autor autorizar: executar a CatalyseR local e o carregamento de
`registro_tratamentos.R` para reproduzir o percurso, sem editar arquivos.

**Verificação técnica de F01 — 21/09/2026.** O autor autorizou a execução sem
edição. O registro de tratamentos foi carregado e expôs os 12 tratamentos. Neste
terminal, o carregamento padrão falha ao interpretar o símbolo `µ` por causa da
localidade; a leitura UTF-8 explícita carregou o mesmo arquivo, com um aviso de
tradução. O acesso ao Qui-quadrado de independência/Fisher foi movido sem mudar
seus identificadores de módulo, registro ou exportação. Os menus foram ordenados
como Paramétricos → Não Paramétricos → Regressão → Regressão Não Linear. Próximo
passo pequeno do autor: conferir essa sequência e abrir o Qui-quadrado em
Frequências e Proporções; o agente confirma que não há acesso duplicado antes de
propor as três análises novas.

**Implementação autorizada de F01 — 21/09/2026.** O autor autorizou a
implementação completa de Uma proporção, Duas proporções e Qui-quadrado de
aderência. O agente criou três telas em Frequências e Proporções, um motor comum
de cálculo e replay, registro de execução e código passo a passo para o Projeto
R. Uma proporção usa `binom.test()`; duas proporções exige dois grupos
independentes e mostra Fisher como referência quando a frequência esperada é
pequena; aderência exige proporções esperadas que somem 100% e impede o p-valor
assintótico quando há frequência esperada menor que 1 ou mais de 20% menores que
5. O agente verificou a sintaxe das telas e da exportação, calculou as três
análises em dados de referência e confirmou o bloqueio de aderência inadequada.
Neste terminal, a verificação integral por `source()` do motor continua limitada
pela localidade UTF-8 já registrada; ao definir `LC_CTYPE` como `pt_BR.UTF-8`,
a CatalyseR completou o carregamento inicial. Isso não equivale a aprovação do
autor. Próximo passo pequeno: o autor abre uma tela com uma base conhecida e
avalia se as entradas, alertas e narrativa estão claros.

**Encerramento de F01 — 21/09/2026.** O autor abriu e testou a CatalyseR pelo
caminho local, após trocar a porta ocupada, e aprovou Frequências e Proporções:
Uma proporção, Duas proporções, Qui-quadrado de aderência e Qui-quadrado de
independência. A F01 está concluída por aprovação do autor e evidência técnica
anterior do agente. O autor observou corretamente que McNemar ainda não é um
módulo disponível; permanece uma entrega futura em Testes Não Paramétricos.
A atividade de exemplo aparece como F03 no Focus do autor; no roteiro ela fica
parcialmente preparada, sem ser oferecida novamente até pedido explícito.

**Retirada de Aplicações Bioestatísticas — 21/09/2026.** O autor concluiu que
o menu e suas subabas criariam redundância: seus percursos pertencem aos módulos
analíticos próprios da CatalyseR. O menu foi retirado, sem iniciar cálculos de
bioecologia, fator de condição, estrutura de tamanhos, razão sexual ou CPUE.
Comunicação de Resultados volta a suceder Mapas. O Laboratório de Conceitos
continua no atalho da faixa direita; a remoção do logo da UFPA permanece.

**Regressões de contagem — 21/09/2026.** Por solicitação do autor, entraram em
Modelos de Regressão, depois da Regressão Logística Binária, a Regressão de
Poisson e a Regressão Binomial Negativa. Ambas aceitam múltiplos preditores e
offset opcional de esforço, área ou volume. Poisson calcula obrigatoriamente a
dispersão de Pearson e, quando ela fica bem acima de 1, encaminha explicitamente
para Binomial Negativa. As duas entregam razões de taxas com intervalo de
confiança, AIC, dispersão e gráfico de resíduos; o código e o replay do Projeto
R foram incluídos. Agente verificou o motor em cenários Poisson e
superdisperso, o teste focal, a sintaxe e a abertura da aplicação. A aprovação
visual e didática do autor ainda é necessária.

**Séries Temporais como família própria — 21/09/2026.** Por solicitação do
autor, Séries Temporais saiu de Modelos de Regressão e tornou-se menu de topo,
entre Regressão Não Linear e Estatística Multivariada. O conteúdo continua
exploratório e descritivo, exposto diretamente em três opções do menu:
Visualizar e suavizar, Decomposição e Autocorrelação. Os cálculos usam `ts()`,
`decompose()` e `acf()` do R base; os objetos são desenhados em `ggplot2` pelo
método do `ggfortify`, com tema Ocean. ETS, ARIMA e previsão ficam
explicitamente para a v2. Agente verificou a série mensal, os três objetos, as
três entradas do menu, a sintaxe e a abertura da aplicação. A aprovação visual
e didática do autor ainda é necessária.

**Reserva de k-means — 21/09/2026.** Por decisão do autor, Estatística
Multivariada agora exibe **Agrupamentos por k-means** após a Análise de
Agrupamentos hierárquicos. É somente uma entrada de percurso futuro: não há
cálculo, configuração, registro ou exportação implementados. A futura análise
será independente do dendrograma e pedirá ao aluno um número de grupos; seu
recorte de implementação continua dependente de autorização específica.

**Exploração Visual dos Dados — 21/09/2026.** Por solicitação do autor, o
antigo menu Visualizando Dados passou a se chamar **Exploração Visual dos
Dados** e foi movido para depois de Explorando os Dados. A coleção reúne
Histograma e densidade, Boxplot e violino, Dispersão e tendência, Duplo eixo Y,
Barras, Rosca, Matriz de dispersão e Mapa de calor de correlação. Linhas para
eixo ordenado foi preservado como apoio, sem duplicar a leitura temporal. As
telas novas limitam o gráfico a uma coluna central de até 960 px e oferecem
facetas onde a comparação por grupo faz sentido. O duplo eixo reescala a
segunda série e traz alerta explícito contra comparar alturas entre escalas.
Agente verificou sintaxe, posição do menu, interfaces e reescalonamento; falta
aprovação visual e didática do autor. O capítulo de visualização do livro EAPA
recebeu o crédito a *R for Data Science* (2ª ed.).

Atualizar esta seção ao retomar: data, atividade, avanço, evidência e próximo passo.
Separar “agente verificou” de “autor aprovou”. Não marcar conclusão por decurso de
tempo ou porque a tarefa foi sugerida. Se o autor apenas relatar que terminou,
registrar como conclusão informada por ele, sem inventar evidência técnica.

## Instruções para entregar a próxima pequena lista

Ler este estado e o escopo do plano principal. Perguntar pelo avanço apenas se
faltar informação essencial. Entregar duas tarefas, ou uma quando solicitado,
sem ultrapassar três pendentes. Não recriar tarefas concluídas, não despejar a
fila futura e não impor datas. Cada cartão deve conter título, uma ação,
critério de término, responsável e bloco sugerido de 25 ou 40 minutos.

Se a próxima revisão depender de código ainda inexistente, explicitar a dependência
e pedir autorização para aquele recorte antes de programar. Aguardar o pedido
do autor para novas atividades; não criar automações, conversas ou tarefas no
Focus por conta própria. Não publicar ou instalar como efeito deste planejamento.
