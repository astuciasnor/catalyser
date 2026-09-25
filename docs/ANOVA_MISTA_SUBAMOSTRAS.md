# ANOVA com subamostras — primeiro recorte do módulo

**Status em 22/09/2026:** saiu da lista de expansões reservadas e entrou no menu
**Testes Paramétricos** como uma construção gradual.

## Problema que o módulo resolve

Uma linha pode representar um arrasto, um peixe ou outra medição feita dentro
de uma praia, tanque, pool ou unidade equivalente. Essas linhas melhoram a
descrição da unidade, mas não multiplicam o número de réplicas independentes.
O módulo conserva as subamostras e declara a unidade como intercepto aleatório,
evitando que a confiança seja inflada por pseudorreplicação.

## Escopo implementado

- uma resposta numérica;
- um fator fixo;
- uma coluna que identifica a unidade independente;
- uma coluna que identifica a subamostra dentro da unidade;
- `nlme::lme(resposta ~ fator, random = ~1 | unidade, method = "REML")`;
- ANOVA simples sobre as médias por unidade, apresentada lado a lado;
- ANOVA ingênua por subamostra somente como aviso didático;
- componentes de variância, correlação intraclasse, pressupostos, QQ-plot,
  código R e console;
- registro da execução e replay no Projeto R integrado.

As frases de pressupostos são condicionadas aos resultados calculados. A
independência não é transformada em teste: continua sendo justificada pelo
delineamento.

## Planejamento informando a análise

A ficha observacional passa a registrar os nomes previstos para a coluna da
unidade e da subamostra. Ao clicar em **Usar esta ficha nas análises**, ela envia
somente essas decisões — nunca dados — para um estado compartilhado. A ANOVA
com subamostras usa os nomes quando as colunas existem na base importada.

Na ficha de variáveis experimentais, o caminho hierárquico envia `UE` e
`id_subunidade`. Quando há mais de uma subamostra por unidade, a recomendação é
o modelo misto; com uma linha por unidade, a ficha recomenda a ANOVA comum.

## Referência executável e evidência

O primeiro benchmark usa
`EAPACadernos/projeto_anova_mista/dados/brutos/arrastos.csv`: 27 arrastos em
9 praias, três praias por condição. Nesse exemplo, a ANOVA das médias por praia
e o modelo misto retornam o mesmo teste do fator (`F = 27,556`, gl = 2 e 6,
`p < 0,001`). A ANOVA ingênua por arrasto retorna `F = 58,943`, tornando visível
a confiança artificial produzida pela pseudorreplicação.

## Evolução deliberadamente deixada para depois

- comparações múltiplas do modelo misto com `emmeans`;
- segundo nível de aninhamento, como `random = ~1 | praia/dia`;
- medidas repetidas no tempo e estruturas de correlação;
- inclinações aleatórias e modelos com mais de um fator fixo;
- exportador autônomo específico, no formato EAPACaderno e sem depender do
  replay do pacote CatalyseR.

O último item é uma distinção importante: o motor estatístico já usa `nlme`,
que acompanha a instalação padrão do R e não exige `lme4`. Nesta etapa, porém,
o Projeto R sai pelo exportador integrado existente e refaz a apresentação com
o pacote CatalyseR. Para cumprir literalmente o objetivo de um ZIP autônomo e
somente com pacotes do CRAN, a próxima etapa deve transformar o projeto anexado
num template próprio; não convém esconder essa diferença nem duplicar às pressas
o exportador geral.

