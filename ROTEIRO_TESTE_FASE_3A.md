# Roteiro de teste — Fases 3A e 3A.1: Registro e cache de Bases Derivadas

## Objetivo

Validar no Zorin OS a infraestrutura de bases derivadas antes de conectar os
ramos aos módulos analíticos. O roteiro testa cadastro, topologia em estrela,
cache lazy, recálculo manual, estados e operações de gerenciamento.

Nesta etapa, bases novas ainda não recebem transformações próprias. Com zero
etapas, sua prévia deve ser idêntica a `dados_analise`. Isso é esperado.

## Conjunto de dados recomendado

Use o arquivo incluído no próprio app:

```text
inst/app/dados/Treino-Transformacoes.xlsx
```

Selecione a aba:

```text
biometria
```

Motivos para usar esse conjunto:

- já pertence à CatalyseR e não depende de internet;
- tem dimensões pequenas, facilitando a conferência visual;
- possui variáveis numéricas e categóricas de pesca;
- contém NA, duplicatas e textos inconsistentes para as fases seguintes;
- poderá ser reutilizado na Fase 3B para ramos de logística, gráficos e
  qui-quadrado.

## Preparação

1. Inicie a CatalyseR no Zorin.
2. Importe `Treino-Transformacoes.xlsx`.
3. Escolha a aba `biometria`.
4. Não aplique tratamentos no primeiro ciclo.
5. Abra **Preparando Dados → Bases Derivadas**.

## Teste 1 — estado inicial

Confirme que:

- o Registro de Bases está vazio;
- aparece a mensagem de que `dados_analise` continua sendo a base compartilhada;
- não existe seletor de origem para criar um ramo;
- a interface informa que todo ramo nasce de `dados_analise`.

## Teste 2 — criar três ramos irmãos

Crie estas bases:

| Nome amigável | Finalidade | Nome R esperado |
|---|---|---|
| Logística da maturidade | Regressão logística | `base_logistica_maturidade` |
| Gráficos de biometria | Gráficos | `base_graficos_biometria` |
| Contingência por local | Qui-quadrado | `base_contingencia_local` |

Confirme que:

- todas têm origem `dados_analise`;
- todas começam como `rascunho`;
- todas têm zero etapas;
- nenhuma pode escolher outra base derivada como origem;
- a coluna Cache mostra `Não calculada`;
- as dimensões ainda aparecem vazias, pois nenhum replay foi solicitado.

## Teste 3 — validação de nomes

Tente criar:

1. outra base com nome R `base_graficos_biometria`;
2. uma base chamada `graficos_sem_prefixo`;
3. uma base chamada `base_gráficos com espaços`;
4. uma base sem nome amigável.

Todos os casos devem ser recusados com mensagem clara. O registro não pode
ganhar linhas inválidas.

## Teste 4 — prévia e código

Selecione cada ramo, mas ainda não clique em **Recalcular**, e confira:

- alternar entre linhas não muda o estado `Não calculada`;
- nenhuma dimensão é preenchida apenas por visitar ou selecionar a base;
- a aba de prévia pede o recálculo manual.

Depois clique em **Recalcular esta base** para cada ramo e confira:

- a prévia abre sem erro;
- a prévia é igual a `dados_analise` nesta fase;
- o Cache muda para `Atualizada`;
- as dimensões passam a ser exibidas;
- o código começa com `dados <- dados_analise`;
- o objeto final recebe o nome R correto;
- aparece o aviso de que ainda não há transformação específica.

Exemplo esperado:

```r
dados <- dados_analise
# Nenhuma transformação específica registrada.
base_graficos_biometria <- dados
```

## Teste 5 — ciclo de estados

Para `base_logistica_maturidade`:

1. confirme que seu cache está `Atualizada`;
2. clique em **Finalizar preparo**;
3. confirme estado `pronta`;
4. clique em **Reabrir como rascunho**;
5. confirme estado `rascunho`, sem perder o cache atualizado;
6. finalize novamente.

Crie também uma base temporária e tente finalizá-la sem recalcular. A ação deve
ser recusada e orientar o uso do botão **Recalcular esta base**.

## Teste 6 — renomear sem perder identidade

Renomeie:

```text
Gráficos de biometria
```

para:

```text
Gráficos biométricos por espécie
```

e altere o objeto R para:

```text
base_graficos_especie
```

Confirme que:

- continua existindo apenas uma linha para esse ramo;
- finalidade, estado e dimensões são preservados;
- o código passa a usar o nome novo;
- tentar renomeá-lo para um nome R já usado é recusado.

## Teste 7 — exclusão segura

1. selecione `base_contingencia_local`;
2. clique em **Excluir base derivada**;
3. cancele e confirme que nada mudou;
4. repita e confirme a exclusão.

Resultados esperados:

- somente o ramo selecionado é removido;
- `dados_analise` não é alterado;
- os demais ramos continuam no registro;
- a exclusão sempre exige confirmação.

## Teste 8 — cache lazy e reação à Trilha compartilhada

1. mantenha pelo menos um ramo criado;
2. vá à Trilha de Preparo;
3. aplique uma transformação compartilhada simples, como padronizar texto de
   `especie`;
4. volte a Bases Derivadas;
5. confira o estado do ramo.

Resultados esperados antes do recálculo:

- o badge muda para `Desatualizada`;
- a última prévia válida continua visível;
- o horário da última prévia válida não muda;
- selecionar ou abrir abas não recalcula o ramo;
- a base desatualizada está bloqueada para os futuros seletores analíticos.

Clique em **Recalcular esta base**. Depois disso:

- somente o ramo selecionado volta a `Atualizada`;
- a prévia passa a refletir a nova versão de `dados_analise`;
- outros ramos continuam `Desatualizada` até serem recalculados;
- não ocorre dupla aplicação da Trilha.

## Teste 9 — metadados não invalidam o cache

1. com uma base `Atualizada`, anote o horário da prévia válida;
2. altere apenas seu nome amigável e sua descrição;
3. confirme que o cache continua `Atualizada`;
4. confirme que o horário e as dimensões não mudaram.

Renomear metadados não modifica a receita nem deve executar replay.

## Teste 10 — troca de conjunto de dados

1. com ramos ainda registrados, troque a aba ou importe outro arquivo;
2. volte a Bases Derivadas.

O registro deve estar vazio. Essa limpeza é intencional: impede que bases de um
projeto sejam reaplicadas silenciosamente a outro conjunto de dados.

## Critérios de aprovação da Fase 3A

- nenhum travamento do Shiny;
- nenhum ramo de ramo;
- nomes inválidos ou duplicados recusados;
- cache `Atualizada` para ramos vazios recalculados;
- nenhuma seleção ou visita de aba dispara replay;
- cache ausente impede finalização;
- mudança compartilhada marca caches como `Desatualizada`;
- recálculo manual afeta somente o ramo selecionado;
- metadados não invalidam nem recalculam o cache;
- estados e metadados preservados corretamente;
- exclusão confirmada e restrita ao ramo;
- mudança compartilhada refletida nas prévias somente após recálculo manual;
- troca da origem limpa o registro;
- módulos analíticos continuam usando `dados_analise`, sem mudança de
  comportamento nesta fase.

## Fora deste teste

Não espere ainda:

- editar transformações dentro do ramo;
- escolher uma base derivada no menu de análise;
- registrar gráficos ou modelos;
- enviar resultados registrados à Comunicação.

Esses comportamentos começam na Fase 3B e seguintes.
