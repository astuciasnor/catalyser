# CatalyseR — roteiro de pequenas atividades

Registrado em 21/09/2026. Planejamento autorizado; implementação ainda não autorizada.

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

### F01 — Etapa 1: testar Preparar Dados · 40 minutos

O autor percorre o caminho de Preparar Dados e relata os problemas observados.
O agente organiza cada relato como defeito comprovado, dúvida de uso ou melhoria
de clareza, reproduz o que for possível e prepara evidências e um recorte de
correção para autorização. Não mudar menus nem código neste bloco.

**Pronto quando:** os problemas do percurso estiverem registrados com o ponto em
que surgem, efeito observado e próximo passo pequeno. A atividade pode pausar
com uma lista parcial; não precisa resolver tudo em um bloco.

### F02 — Escolher o exemplo de uma proporção · 25 minutos

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

**Etapa atual:** estrutura visual do novo grupo aplicada e aguardando conferência
do autor; F01 ainda não iniciou o relato de problemas de Preparar Dados.
**Autorização para código neste roteiro:** nenhuma.
**Atividades disponíveis:** F01 e F02. Não presumir que já foram inseridas no Focus.

| Atividade | Estado | Evidência / decisão |
|---|---|---|
| F01 | Pendente — relato e teste de Preparar Dados | Em 21/09/2026, o autor redefiniu a primeira etapa para testar Preparar Dados antes de decidir a ordem dos menus. Aguardar os problemas observados; o agente ainda não altera código nesse recorte. |
| F02 | Agendada — próxima no Focus | Agente verificou em 21/09/2026 `EAPADados::lagostas_kelp_sexo`: 1.457 lagostas (663 fêmeas, 794 machos), origem documentada e limite de armadilhas/sítios. Aguardar escolha ou ajuste da pergunta pelo autor. |

**Registro da retomada — 21/09/2026.** O agente preparou as propostas de F01 e
F02 sem alterar código. Verificação técnica não equivale à aprovação científica ou
didática do autor. Próximo passo pequeno: o autor revisa os dois cartões abaixo;
se uma decisão não for tomada, registrar a preferência já definida e retomar só o
ponto restante em outro bloco. F01 foi iniciada pelo autor como teste de Preparar
Dados; F02 fica reservada como a próxima atividade, sem criar outras pendências.

**Preparação visual autorizada — 21/09/2026.** Antes de F01/F02, o agente
adicionou somente a estrutura vazia de **Frequências e Proporções** após
Preparar Dados (ícone de gráfico de pizza) e moveu o atalho de Ajuda ao lado de
Sobre. Não foram criadas análises nem módulos. A sintaxe de `app.R` foi conferida;
aguardar a conferência visual do autor na CatalyseR local. Esta verificação técnica
não aprova a ordem pedagógica do catálogo.

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
