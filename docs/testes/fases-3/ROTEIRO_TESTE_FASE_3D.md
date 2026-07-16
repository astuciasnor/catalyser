# Roteiro de teste — Fase 3D

> Roteiro histórico da branch `feature/fase-3d-comunicacao-resultados`. Consulte
> também a [matriz e a ordem completa de homologação](README.md).

## Objetivo

Confirmar que a Comunicação de Resultados usa somente execuções registradas,
mostra a base de cada uma, permite ordenar e selecionar o conteúdo do Word sem
apagar o acervo completo que será preservado no Projeto R.

## Dados sugeridos

Use primeiro o conjunto padrão carregado pela CatalyseR, com as variáveis
`id`, `sexo`, `comp_cm` e `peso_g`. Ele permite testar regressão, teste t e dois
gráficos de linhas sem importar outro arquivo.

Depois, repita o teste de atualização com
`inst/app/dados/Treino-Transformacoes.xlsx`, aba `biometria`, caso queira
exercitar também a Trilha e bases derivadas.

## 1. Estado inicial

1. Inicie a CatalyseR e abra **Comunicação de Resultados → Projeto de Comunicação**.
2. Confirme a mensagem **Nenhuma execução registrada**.
3. Confirme o contador `0 no Word / 0 no Projeto R`.
4. Confirme que o esboço já mostra Preparação, Introdução, Métodos, Resultados,
   Discussão e Conclusão.
5. Confirme que **Gerar relatório e Projeto R — Fase 3E** está desabilitado.

## 2. Registrar quatro execuções

### Regressão

1. Abra **Modelos de Regressão → Regressão Linear**.
2. Use `peso_g` como resposta e `comp_cm` como preditora.
3. Clique **Executar análise**.
4. Dê o título `Peso em função do comprimento`.
5. Clique **Adicionar aos resultados**.

### Dois gráficos da mesma base

1. Abra **Descrevendo Dados → Gráfico de Linhas**.
2. Faça `id` no eixo X e `comp_cm` no eixo Y.
3. Clique **Executar análise** e registre como `Comprimento por observação`.
4. Troque somente o eixo Y para `peso_g`.
5. Clique **Executar novamente**.
6. Use **Salvar como novo** e registre `Peso por observação`.

### Teste t

1. Abra **Testes Paramétricos → Teste t**.
2. Configure `comp_cm` como resposta e `sexo` como grupo.
3. Clique **Executar análise** e depois **Adicionar aos resultados**.

## 3. Conferir o estúdio conjunto

1. Volte à Comunicação de Resultados.
2. Confirme quatro cartões, cada um com ID, título, base e badge `Atualizada`.
3. Confirme o contador `4 no Word / 4 no Projeto R`.
4. Em **Bases do projeto**, confirme `dados_analise` e a quantidade de execuções
   vinculadas. Se houver ramos derivados, eles também devem aparecer, usados ou não.
5. Na regressão, deixe `Narrativa`, `Tabela` e `Gráfico`; desmarque
   `Pressupostos`, `Diagnósticos` e `Console`.
6. Nos gráficos de linhas, confirme que somente `Gráfico` é oferecido.
7. No teste t, selecione `Narrativa`, `Tabela` e `Pressupostos`, sem o gráfico.
8. Confirme no esboço que cada item mostra exatamente os componentes escolhidos.

## 4. Ordenação e separação Word × Projeto R

1. Use **Subir/Descer** para colocar o teste t antes dos gráficos.
2. Confirme que o esboço acompanha a nova ordem.
3. Desmarque **Incluir esta execução no Word** em `Peso por observação`.
4. O contador deve mudar para `3 no Word / 4 no Projeto R`.
5. Confirme que o item desapareceu do esboço do Word, mas continua na lista de
   execuções e no Manifesto editorial com `Word: não`.

Esse é o teste central da fase: escolher o conteúdo do Word não pode apagar uma
execução que deverá existir no Projeto R.

## 5. Seções globais

1. Escreva uma frase curta em Introdução, Métodos gerais, Discussão e Conclusão.
2. Abra a aba **Manifesto editorial**.
3. Confirme que o total de execuções e a ordem continuam corretos.

## 6. Dependência após mudança dos dados

1. Volte ao preparo e crie uma transformação simples, como
   `peso_kg = peso_g / 1000`, promovendo o resultado para as análises.
2. Retorne à Comunicação de Resultados.
3. As execuções antigas devem permanecer listadas, mas mostrar
   **Precisa atualizar**.
4. Retorne a uma análise, execute novamente com a base atual e use
   **Atualizar resultado**.
5. Na Comunicação, confirme que somente essa execução voltou a `Atualizada`.

## 7. Critérios de aprovação

- visitar uma aba sem registrar não cria item na Comunicação;
- cada execução registrada aparece uma única vez;
- novas execuções entram no fim;
- a ordem e as seleções sobrevivem à atualização de uma execução;
- excluir uma execução no módulo analítico também a remove da Comunicação;
- desmarcar do Word não remove do Projeto R;
- saídas indisponíveis nunca aparecem como opção;
- dependências alteradas são sinalizadas, sem recálculo automático;
- nenhum arquivo é exportado nesta fase — essa integração pertence à 3E.
