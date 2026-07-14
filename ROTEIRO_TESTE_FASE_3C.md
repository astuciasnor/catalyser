# Roteiro de teste — Fase 3C: registro explícito de execuções

## Objetivo

Confirmar que uma prévia só entra no novo registro quando o usuário clica em
**Adicionar aos resultados**, e que várias configurações da mesma análise são
preservadas como execuções independentes.

A Fase 3C cobre:

- Estatística Descritiva;
- Regressão Linear Simples;
- Regressão Logística;
- Teste t de Student;
- Gráfico de Linhas;
- Qui-quadrado de independência;
- PCA;
- Análise de Agrupamentos (HCA).

## Conjunto de dados

Use `inst/app/dados/Treino-Transformacoes.xlsx`.

- aba `biometria`: descritiva, regressões, teste t, qui-quadrado, PCA e HCA;
- aba `desembarques_largo`: gráfico de linhas depois de arrumar para o formato
  longo (ela fornece um único Y, a captura).

Para o teste mais importante de duas respostas no mesmo gráfico de linhas, a
forma imediata é usar a aba `biometria`: mantenha `comprimento_cm` no eixo X e
registre primeiro `peso_g` e depois `cpue` no eixo Y. Se quiser reproduzir
exatamente o exemplo temporal, use uma planilha *tidy* própria com as colunas
`ano`, `captura_t` e `esforco_h`.

Você também pode usar os ramos preparados nos roteiros das Fases 3B.2 e 3B.3.

## Teste 1 — visitar não registra

1. abra um dos módulos cobertos;
2. altere variáveis e parâmetros várias vezes;
3. observe o cartão **Registrar execução**.

Resultado esperado:

- o contador continua em `0 registradas`;
- aparece **Prévia ainda não registrada**;
- nenhuma troca de variável cria item automaticamente.

O exportador consolidado antigo ainda possui rastreamento por visita. Ele é um
componente legado e só será substituído na Fase 3E; este teste se refere ao novo
cartão de registro.

## Teste 2 — adicionar uma execução

1. obtenha uma prévia válida;
2. confira ou edite o título sugerido;
3. clique em **Adicionar aos resultados**.

Resultado esperado:

- surge um ID como `execucao_0001`;
- o contador passa para `1 registrada`;
- o detalhe mostra a base usada e as saídas disponíveis;
- o estado aparece como **Atualizada**;
- o item passa a oferecer **Atualizar resultado**, **Salvar como novo** e
  **Remover dos resultados**.

## Teste 3 — dois gráficos com o mesmo X e respostas diferentes

No Gráfico de Linhas, usando a aba `biometria`:

1. escolha uma base e use `comprimento_cm` no eixo X;
2. use `peso_g` como primeira resposta Y, gere a prévia e registre;
3. troque somente Y para `cpue`;
4. clique em **Salvar como novo**.

Resultado esperado:

- existem dois IDs diferentes;
- o primeiro conserva sua resposta Y original;
- o segundo guarda a nova resposta;
- escolher um item no dropdown mostra seu título, base e versão;
- nenhum item é sobrescrito apenas porque a prévia mudou.

Opcionalmente, repita com `profundidade_m` como terceira resposta. Para testar
o caso temporal, faça o mesmo com uma planilha que contenha `ano`, `captura_t`
e `esforco_h`.

## Teste 4 — atualizar não duplica

1. selecione uma execução registrada;
2. altere a prévia;
3. clique em **Atualizar resultado**.

Resultado esperado:

- o ID permanece igual;
- a versão do item aumenta;
- o contador não aumenta;
- as outras execuções permanecem intactas.

Depois clique em **Salvar como novo** e confirme que, desta vez, o contador e o
ID aumentam.

## Teste 5 — vínculo com bases

Registre uma análise primeiro com `dados_analise` e depois com uma base derivada
pronta e atualizada.

Confirme que os detalhes mostram respectivamente:

- `dados_analise`;
- o nome R do ramo, como `base_reg_logistica` ou `base_graficos`.

Alternar entre bases não deve recalcular o ramo nem alterar o horário de sua
última prévia.

## Teste 6 — dependência fica desatualizada

Com uma execução vinculada a uma base derivada:

1. reabra o ramo ou altere uma etapa da Trilha compartilhada;
2. retorne ao módulo e selecione a execução registrada.

O item deve continuar no registro, mas seu estado deve mudar para **Precisa
atualizar**. A prévia analítica corrente pode voltar para `dados_analise`, sem
apagar o registro histórico.

Recalcule/finalize o ramo, obtenha novamente a análise e use **Atualizar
resultado**. O item deve voltar a **Atualizada** e aumentar sua versão.

## Teste 7 — fontes do Qui-quadrado

Registre três execuções, quando possível:

1. fonte **Duas variáveis**: deve guardar a base escolhida no seletor;
2. fonte **Tabela preparada**: deve indicar `tabela_contingencia`;
3. fonte **Entrada manual**: deve indicar `tabela_manual`.

A entrada manual não deve ficar desatualizada quando a Trilha compartilhada
mudar. A matriz de contagens faz parte dos parâmetros registrados.

## Teste 8 — remover com confirmação

1. selecione uma execução;
2. clique em **Remover dos resultados**;
3. cancele uma vez e confirme que o item permanece;
4. repita e confirme a remoção.

A prévia atual da análise deve permanecer na tela. Somente o registro é removido.

## Teste 9 — troca de conjunto de dados

Depois de registrar algumas execuções, carregue outro arquivo ou outra fonte de
dados brutos.

O registro e o contador devem voltar a zero. Isso impede que resultados de dois
projetos diferentes sejam misturados na mesma sessão.

## Critérios de aprovação

- visitar ou explorar um módulo não registra nada;
- cada clique para salvar como novo cria ID monotônico próprio;
- alterar a prévia não sobrescreve registros;
- atualizar preserva o ID e incrementa a versão;
- título, base, parâmetros e saídas disponíveis são congelados no clique;
- o registro não duplica `data.frame`, modelo ou gráfico;
- alternar bases não executa replay;
- mudanças de origem/receita marcam dependências sem apagar itens;
- o Qui-quadrado registra corretamente suas três fontes;
- remover exige confirmação;
- trocar os dados brutos limpa o registro;
- os oito módulos continuam calculando suas prévias normalmente.

## Fora desta fase

- lista conjunta e reordenação no menu Comunicação;
- escolha do que entra no Word;
- renderização do relatório integrado;
- Projeto R orientado pelo novo registro;
- substituição do rastreamento legado do consolidado.

Esses itens pertencem às Fases 3D e 3E.
