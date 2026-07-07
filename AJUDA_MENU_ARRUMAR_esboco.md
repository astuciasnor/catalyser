# Ajuda da IDE — Menu **Arrumar** (esboço)

> Esboço do conteúdo da Ajuda da CatalyseR para o módulo de arrumação de dados
> (`mod_arrumar.R`). Tom: conversa direta com o pesquisador, "no mouse".
> As marcações `[PRINT: …]` e `[ESQUEMA: …]` indicam onde entram capturas de tela
> e infográficos. Depois este texto vira uma entrada `help_topics` (HTML) no `app.R`,
> no mesmo padrão dos demais tópicos.

---

## O que este menu resolve

Quase nenhum conjunto de dados chega pronto para analisar. O caso clássico: a
planilha vem **larga**, com uma coluna por ano (`2021 - Captura_t`,
`2022 - Captura_t`…), ou com informações **grudadas** numa célula só
(`PARGO-2023-BRA`). O R não consegue plotar nem comparar enquanto os dados
estiverem nesse formato.

O menu **Arrumar** conserta isso **clicando** — e, no fim, entrega o **script `.R`**
que fez a arrumação, pronto para você guardar e reusar. É o "do mouse ao código"
na etapa mais ingrata do trabalho.

[ESQUEMA: planilha larga/suja → (empilhar / alargar / separar) → dados tidy → script .R]

---

## Três operações (você escolhe o verbo)

O menu tem duas telas irmãs — **Empilhar** e **Separar** — e, dentro de Empilhar,
duas operações:

- **Empilhar (largo → longo):** junta várias colunas numa só. Use quando o
  ano/medida está no **nome** da coluna. É o `pivot_longer()`.
- **Alargar (longo → largo):** espalha os níveis de uma coluna em várias. É o
  caminho de volta, o `pivot_wider()`. Útil depois de empilhar, para deixar cada
  medida em sua coluna.
- **Separar:** quebra uma coluna composta (`PARGO-2023-BRA`) em várias. Use quando
  a informação está **dentro** de uma célula.

No menu Empilhar, o seletor **"Operação desta etapa"** decide entre empilhar e
alargar. Um verbo por etapa.

[PRINT: seletor "Operação desta etapa" com Empilhar/Alargar]

---

## As etapas se acumulam (e dá para desfazer)

Esta é a chave: cada vez que você clica em **Aplicar transformação**, a operação
age **sobre o resultado da etapa anterior**, não sobre a planilha original. Assim
você encadeia quantas etapas quiser — separar uma coluna, depois outra; empilhar e
depois alargar; na ordem que precisar. O contador ao lado mostra quantas etapas já
foram aplicadas, e **Desfazer última etapa** remove a última.

Regra prática: **primeiro a estrutura, depois o polimento.** Faça todas as
separações/empilhamentos primeiro; renomear, recodificar, tipar e selecionar ficam
para o fim (eles reiniciam a cada nova etapa estrutural).

[PRINT: botão Aplicar + Desfazer + contador "2 etapa(s)"]

---

## Separar sem saber regex (o atalho)

Você **não precisa** aprender expressões regulares. Ao separar uma coluna (ou
empilhar extraindo do nome), escolha o método **"Por delimitador"** e clique em
**"Detectar separador automaticamente"**. A IDE olha os seus dados, descobre o
separador que divide tudo no mesmo número de partes e já preenche o separador, o
número de colunas e os nomes-rascunho. Você só confere e dá nomes melhores.

- Separadores comuns na lista: `_`, `-`, `.`, `;`, `,`, espaço e `" - "` (hífen
  entre espaços). Tem também "Outro (digitar)".
- Se o separador dividir os valores em **números diferentes** de partes (dado
  irregular), a IDE avisa e sugere o modo regex.

[PRINT: cartão "Separar em colunas" no método Delimitador, com o botão de detecção]

Só quando o caso é irregular — capturar só parte do valor, tratar o ano como
quatro dígitos — vale o método **"Por padrão / regex (avançado)"**. Ali há padrões
prontos e um botão **"?"** com um guia rápido de regex.

[ESQUEMA: exemplo "PARGO-2023-BRA" com setas → especie / ano / porto]

---

## Polimento: renomear, recodificar, tipar, selecionar

No painel **Exportar** (à direita), quatro botões dão o acabamento:

- **Renomear colunas:** troca nomes feios (`2021 - Captura_t`) por nomes limpos
  (`captura_t`). Dica: nomes *tidy* evitam espaços e símbolos.
- **Recodificar níveis:** padroniza os rótulos de uma coluna (o famoso `seca`,
  `Seca`, ` SECA ` que deviam ser um só). O botão **"Detectar variações
  automaticamente"** sugere as correções de caixa, acentos e espaços; junções por
  significado você faz à mão, no esquema "antigo → novo".
- **Tipar colunas:** define o tipo de cada coluna (texto, número, inteiro, fator,
  data). O padrão já reflete o tipo atual; mude só o que precisar (ex.: `ano` para
  Inteiro, `periodo` para Fator).
- **Selecionar variáveis:** marca quais colunas seguem para a análise.

[PRINT: painel Exportar com os quatro botões]
[PRINT: modal "Recodificar níveis" com antigo → novo e o botão Detectar]

---

## O resultado, em três abas

- **Resultado:** a tabela arrumada, atualizada a cada etapa.
- **Original:** a planilha de entrada, para comparar.
- **Script gerado:** o `.R` que reproduz **tudo** o que você fez no mouse —
  `pivot_longer`, `pivot_wider`, `separate_wider_delim`, `rename`, `recode`,
  `mutate`, `select`, na ordem certa.

Baixe o script (`.R`) e os dados arrumados (`.xlsx`) pelos botões do painel
Exportar. O script roda igual no ano que vem, com a planilha nova, sem repetir o
trabalho manual.

[PRINT: aba "Script gerado" mostrando o pipe com as etapas]

---

## Avisos que a IDE dá (e o que fazer)

- **"Selecione ao menos uma coluna de medida para empilhar."** Você está em
  *Empilhar* sem escolher as colunas. Se o que você quer é alargar dados já longos,
  troque a operação para **Alargar**.
- **Coluna nova toda vazia (NA):** o padrão não casou — confira o separador/regex.
- **"os valores não estão unicamente identificados" (ao alargar):** os
  identificadores não distinguem cada linha; revise antes de alargar.
- **Valores viraram NA ao tipar como número:** havia texto não numérico na coluna.

---

## Onde isto aparece no livro

O capítulo **"Preparando os dados"** (Unidade II) mostra *por que* arrumar e o
*ganho* de ir do largo ao *tidy*, com exemplos. Esta Ajuda mostra o *como fazer*,
clique a clique. Os dois se completam.
