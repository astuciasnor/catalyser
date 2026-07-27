# Roteiro V15 — novo estúdio pedagógico de preparo

## Objetivo

Validar a reorganização do menu **Preparando Dados** sem repetir as análises:

1. **Pivotar e Separar Dados** reúne `pivot_longer()`, `pivot_wider()` e
   separação em colunas;
2. **Organizar Variáveis** reúne criação e arrumação;
3. cada sub-aba adiciona sua mudança à trilha da Base Compartilhada;
4. **Adicionar Tratamentos à Base** reúne tratamentos, trilha e a checagem
   final, sem repetir cálculo ou reescala.

Use `inst/app/dados/Treino-Transformacoes.xlsx`.

## 1. Empilhar Dados — `pivot_longer()`

1. Importe a aba `desembarques_largo`.
2. Abra **Pivotar e Separar Dados > Empilhar Dados**.
3. Em **Colunas a empilhar**, escolha:
   - `2022 - Captura (t)`;
   - `2023 - Captura (t)`;
   - `2024 - Captura (t)`.
4. Separador: `Hífen entre espaços ' - '`.
5. Em **Nomes das colunas que descrevem os cabeçalhos**, use `ano, medida`.
   Essas duas colunas guardarão os textos extraídos do cabeçalho: `2022` irá
   para `ano` e `Captura (t)` irá para `medida`.
6. Em **Nome da coluna que receberá os valores**, use `captura_t`. Essa coluna
   guardará os números `1240`, `1320` etc.; por isso, seu nome deve ser
   diferente de `ano` e `medida`.
7. Clique em **Aplicar transformação**.
8. Confira 12 linhas no formato longo: quatro portos × três anos.
9. Clique em **Adicionar Mudança à Trilha da Base Compartilhada**.

## 2. Alargar Dados — `pivot_wider()`

Sem reimportar:

1. abra **Alargar Dados**;
2. `names_from`: `ano`;
3. `values_from`: `captura_t`;
4. aplique a transformação;
5. confira as colunas `2022`, `2023` e `2024`;
6. adicione a mudança à trilha.

Esse encadeamento confirma que a segunda sub-aba lê a Base Compartilhada
produzida pela primeira, e não retorna silenciosamente à planilha original.

## 3. Separar Dados em Colunas

1. Reimporte a aba `biometria`.
2. Abra **Pivotar e Separar Dados > Separar Dados em Colunas**.
3. Coluna a quebrar: `amostra`.
4. Separador: `_`.
5. Nomes novos: `local_amostra, periodo`.
6. Mantenha a coluna original durante a conferência.
7. Aplique e adicione a mudança à trilha.

## 4. Criação de Variáveis

1. Abra **Organizar Variáveis > Criação de Variáveis**.
2. Escolha **Criar variável calculada** e **Expressão livre**.
3. Fórmula: `peso_g / comprimento_cm`.
4. Nome: `razao_peso_comprimento`.
5. Aplique.
6. Ainda nessa sub-aba, escolha **Reescalar por prefixo**.
7. Coluna: `peso_g`; prefixo manual: `k`; nome: `peso_kg`.
8. Aplique novamente.
9. Confira as duas colunas e adicione a mudança à trilha.

## 5. Arrumação de Variáveis

Em **Arrumação de Variáveis**:

1. renomeie ao menos uma variável;
2. recodifique um nível de `sexo` ou `especie`;
3. confira ou altere um tipo;
4. selecione as variáveis que permanecerão;
5. confira Resultado e Script gerado;
6. adicione a mudança à trilha.

## 6. Checagem Final da Base Compartilhada

1. Abra **Adicionar Tratamentos à Base > Checagem Final da Base
   Compartilhada**.
2. Confirme que os ajustes ficam na coluna estreita à esquerda.
3. Confirme que a tabela ocupa aproximadamente 80% da largura.
4. Troque o número de linhas exibidas entre 5, 10, 25, 50 e 100.
5. Em uma base com muitas colunas, use a rolagem horizontal.
6. Clique em **Renomear variáveis** e feche o formulário com Cancelar.
7. Confirme que **Remover variáveis**, **Recodificar níveis** e **Definir
   tipos** também estão visíveis sem rolagem interna.

## 7. Adicionar Tratamentos à Base

Abra **Adicionar Tratamentos à Base > Tratamentos e trilha** e confirme:

- não existe a antiga sub-aba **Calculadora guiada**;
- `Calcular variável` e `Reescalar` não aparecem em Tipo de tratamento;
- dados faltantes, dicotomização, padronização, classes, duplicatas e texto
  continuam disponíveis;
- a trilha existente continua reordenável.

## Critérios de aprovação

- as três sub-abas de pivotação aparecem e executam;
- as duas sub-abas de organização aparecem;
- as sub-abas **Tratamentos e trilha** e **Checagem Final da Base
  Compartilhada** aparecem em **Adicionar Tratamentos à Base**;
- todos os seis pontos de adição usam o texto **Adicionar Mudança à Trilha da
  Base Compartilhada**;
- cálculo e reescala vivem somente em **Criação de Variáveis**;
- a Checagem Final permanece em duas colunas 20/80 em 1366×768;
- a tabela final tem paginação, busca, filtros e rolagem horizontal;
- o número de linhas e o script permanecem coerentes depois de cada mudança.
