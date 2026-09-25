# CatalyseR — prioridades e expansões reservadas

Registrado em 21/09/2026, a pedido do autor.

## Status e limite de autorização

Este documento guarda decisões e propostas de trabalho. Não descreve funcionalidades
já implementadas e não autoriza alterações no código, instalação ou publicação.
O autor pediu uma lista viável e adiou as expansões. A implementação depende de
nova autorização, com escolha do lote. Nenhum item abaixo deve ser executado
automaticamente por estar neste plano.

O autor confirmou que domina: uma proporção com IC/teste binomial, comparação
de proporções e qui-quadrado de aderência. A inclusão do menu **Frequências e
Proporções** foi acolhida como uma das primeiras atividades quando autorizar
mudanças no código. Essa decisão específica atualiza a seleção de candidatos à
v1, sem liberar todas as ampliações sugeridas na pesquisa bibliográfica.

Optou-se por um documento de planejamento, ligado aos índices do projeto e do
ecossistema, em vez de uma skill: são prioridades e decisões a retomar, não um
procedimento reutilizável que deva disparar trabalho.

Para executar este plano em pequenas sessões, consultar o
[roteiro Focus — atividades e ponto de retomada](ROTEIRO_FOCUS_V1.md).
Ele mantém a fila guardada e libera somente duas ou três atividades por vez,
sem transformar expansões em obrigação para a v1.

## Primeiro lote proposto — muito alta prioridade

1. **Organizar Frequências e Proporções.** Definir as entradas e reaproveitar o
   qui-quadrado de independência/Fisher existentes, sem duplicar motores. A
   transferência dos acessos atuais precisa ser conferida na implementação.
   A tabela descritiva de frequências permanece em Descrever Variáveis.
2. **Uma proporção com IC e teste binomial.** Mostrar sucessos, total válido,
   proporção, nível de confiança e proporção de referência; explicitar o método
   do IC e os pressupostos. Não tratar percentuais sem denominador como contagens.
3. **Comparação de proporções.** Começar por dois grupos independentes, com
   estimativa da diferença, IC e método de teste explícito. Definir o tratamento
   de pequenas contagens antes de codificar; não confundir com dados pareados.
   A extensão a vários grupos e múltiplas comparações não está neste primeiro lote.
4. **Qui-quadrado de aderência.** Receber proporções esperadas definidas pela
   pergunta científica; conferir soma, categorias, contagens e frequências
   esperadas pequenas. Distinguir aderência de independência.
5. **Fechar o percurso das três análises.** Resultado, gráfico quando informativo,
   código legível, registro independente por configuração, Comunicação e Projeto
   R precisam concordar. Conferir limites e exemplos; solicitar revisão didática
   do autor após validação. Não considerar concluído apenas porque o teste roda.

Esse é o primeiro lote sugerido, não uma estimativa de prazo. Cada análise pode
ser entregue e revisada separadamente, respeitando o ciclo de refinamento vigente.

## Meta transversal — a análise respeita o desenho

Muito alta prioridade científica; desenvolver por etapas, sem prometer suporte
automático a todos os delineamentos.

- Identificar unidade amostral/experimental, população-alvo e cadastro de origem.
- Distinguir grupos independentes, pares, medidas repetidas, estratos, blocos e
  agrupamentos como tanque/local. Não deduzir independência só pelo tipo da coluna.
- Registrar desenho, semente, alocação e frações de inclusão quando disponíveis;
  manter essas informações nas bases derivadas, nas execuções e na exportação.
- Alertar quando o desenho não é atendido pelo método escolhido. Não emitir IC
  ou p-valor para amostragem independente como se isso resolvesse outro desenho.
- Distinguir pesos amostrais de contagens de uma tabela. Quando necessárias,
  ponderação e variância devem acompanhar o desenho; só ponderar uma média não basta.
- Fazer um mapa planejamento → análise: DBC, quadrado latino e parcelas
  subdivididas já aparecem no planejamento, mas não têm todos um caminho
  analítico dedicado no catálogo conferido em 21/09/2026. Isso é uma meta de
  compatibilidade, não autorização para implementar esses modelos agora.
- Não confundir peixes medidos com tanques experimentalmente replicados. Modelos
  mistos não recuperam replicação ausente no delineamento.

No primeiro lote, o mínimo é explicitar e validar o cenário suportado, preservar
a origem e não apresentar desenhos incompatíveis como atendidos. A infraestrutura
completa de inferência para desenhos complexos fica para uma etapa própria.

## Amostragem estratificada — alta prioridade, segundo lote a autorizar

Separar **dimensionar o total n**, **distribuir n entre estratos** e **sortear as
unidades**. Uma regra de alocação não determina, sozinha, o tamanho necessário.

| Opção | Regra para o estrato h | Interpretação |
|---|---|---|
| Quantidade igual | n_h aproximadamente igual entre estratos | Mantém a possibilidade de comparar grupos, como 200 por sexo. |
| Proporcional | n_h = n × N_h / soma(N_h) | Distribui o total conforme os tamanhos dos estratos. |
| Neyman | n_h = n × N_h S_h / soma(N_h S_h) | Considera tamanho e desvio-padrão da variável-alvo; para n fixo, custos iguais e amostragem aleatória simples dentro dos estratos. |

N_h é o tamanho do estrato na população/cadastro definido; S_h é o desvio-padrão,
raiz da variância, da variável escolhida. Neyman visa à precisão da média/total
global dessa variável, não necessariamente à melhor comparação entre grupos ou
ao melhor resultado para todas as variáveis. Custos distintos exigem outro problema
de alocação e ficam fora deste lote.

Sequência proposta:

1. Conferir o que o módulo de amostragem proporcional existente já oferece e
   conectar a mesma regra às bases derivadas; evitar dois cálculos divergentes.
2. Preservar o modo de quantidade fixa por grupo. Adicionar, quando autorizado,
   total n + alocação proporcional + prévia das quantidades por estrato.
3. Acrescentar Neyman em subetapa separada, escolhendo variável-alvo e origem de
   S_h: piloto, estudo anterior ou cadastro observado usado no exercício didático.
   Para planejar uma coleta real, não presumir conhecida a resposta ainda não medida.
4. Definir arredondamento para manter o total, limites n_h ≤ N_h, tamanhos mínimos,
   estratos muito pequenos e S_h nulo/indisponível; nenhuma exclusão silenciosa.
5. Guardar regra, estratos, N_h, n_h, fonte de S_h, semente e probabilidades/pesos
   calculáveis; validar o sorteio sem reposição e a reprodução na exportação.

**Cuidado com o abalone:** sortear proporcionalmente à base reproduz a composição
do cadastro de origem, não demonstra representatividade da população natural.
A amostra fixada em 200 de cada sexo tem proporção 1:1 por construção; não usar
sua frequência bruta para inferir a razão sexual natural. Nenhuma modalidade de
subamostragem corrige automaticamente vieses da coleta original.

## Outras prioridades altas — fila posterior, não no primeiro lote

- **Dimensionamento amostral:** precisão para média/proporção e, depois, poder
  a priori para desenhos delimitados; identificar unidades independentes e
  pressupostos. Não recomendar poder observado pós-teste como solução.
- **McNemar:** útil para resposta binária pareada, mas não indispensável às três
  primeiras análises. A localização decidida pelo autor é **Testes Não Paramétricos**;
  eventual orientação cruzada no novo menu não deve duplicar implementação.
  Sua execução depende de autorização de um lote posterior.
- **Regressão linear múltipla:** concluir a opção ainda em desenvolvimento,
  quando retomada especificamente; não ampliar a primeira entrega de proporções.
- **ANOVA de Welch e Games–Howell:** candidato a revisão da família ANOVA;
  não confundir com Welch no teste t, já presente.
- **Friedman:** candidato para blocos/condições pareadas adequados; não é uma
  solução universal para qualquer acompanhamento longitudinal.
- **Apresentação consistente:** aproveitar IC e tamanhos de efeito existentes,
  com denominadores e métodos explícitos. Não tratar Fisher, Dunn, Levene,
  Bartlett, Pearson/Spearman ou Shapiro como ausências a implementar novamente.

## Frente retirada da reserva em 22/09/2026

**ANOVA com subamostras por modelo misto.** A primeira etapa entrou no menu
**Testes Paramétricos** com um fator fixo e um nível de agrupamento. A tela pede
a resposta, o fator, a unidade independente e a identificação da subamostra;
ajusta `nlme::lme(resposta ~ fator, random = ~1 | unidade)` e apresenta lado a
lado a ANOVA das médias por unidade e o modelo misto. O caminho que trata cada
subamostra como réplica aparece apenas como alerta didático. A ficha de
planejamento pode enviar os nomes das colunas e a recomendação à análise.

A evolução permanece deliberadamente gradual: comparações múltiplas com
`emmeans`, segundo nível de aninhamento (`unidade/subnivel`), medidas repetidas
no tempo e estruturas de correlação não fazem parte deste primeiro recorte.

## Expansões reservadas — não fazer agora

| Frente | Conteúdo guardado | Condição de retomada |
|---|---|---|
| Regressão de contagem | Poisson e Binomial Negativa, com offset de esforço/área/volume e diagnóstico de superdispersão | Implementadas em 21/09/2026; aguardam revisão visual e didática do autor. |
| Medidas repetidas e modelos mistos ampliados | Tempo, segundo nível de aninhamento e estruturas de correlação; o caso de subamostras com um fator e um agrupamento já saiu da reserva | Retomar um desenho por vez, depois da revisão didática do primeiro módulo. |
| Ecologia de comunidades | Diversidade, números de Hill, rarefação, NMDS, PCoA, PERMANOVA/PERMDISP; depois RDA/CCA | Começar dentro de Multivariada; justificar distâncias e permutações. |
| Sobrevivência | Kaplan–Meier, log-rank; depois Cox | Distinguir tempo até evento/censura de proporção de sobreviventes. |
| Séries temporais | Previsões de referência, ETS/ARIMA e validação temporal | Ampliar a exploração já existente; não criar módulo duplicado. |
| Biologia pesqueira | Condição, comparação de leituras de idade, chave idade–comprimento, captura–recaptura, mortalidade e recrutamento | Trilha especializada, sem duplicar peso–comprimento e crescimento existentes. |
| GAM | Modelos aditivos e diagnóstico | Depois de modelos lineares e generalizados consolidados. |
| Bootstrap | Incerteza em análises delimitadas, respeitando a unidade de reamostragem | Opção dentro das análises, não menu genérico de métodos avançados. |
| Diagnósticos ampliados | Colinearidade, influência, dependência e heterogeneidade conforme cada modelo | Sem transformar normalidade em seletor automático de testes. |
| Descobrindo o Modelo | Comparação orientada por hipóteses, diagnóstico e incerteza; critérios apropriados ao objetivo | Explicitamente adiado pelo autor; não escolher automaticamente pelo maior R². |

Na futura comparação de modelos, exigir candidatos cientificamente justificáveis,
comparabilidade dos dados/respostas e diagnósticos. AIC/AICc não são substitutos
automáticos de R²; validação preditiva e inferência respondem a objetivos distintos.
Preservar a distinção entre descoberta exploratória e hipótese confirmatória.

## Referências para a retomada

Pesquisa orientada realizada nesta tarefa: sumários, resumos e conteúdos abertos,
não leitura integral de todos os livros nem revisão sistemática. As prioridades
são uma síntese para a CatalyseR, não recomendações de interface dos autores.

1. Quinn e Keough (2023), [Experimental Design and Data Analysis for Biologists](https://www.cambridge.org/highereducation/books/experimental-design-and-data-analysis-for-biologists/7AA1811FE2E249CE6065ACD6F3B68C41/exploratory-data-analysis/C45E87BB2DCF5C8A8851A9E32120FD82).
2. Crawley (2013), [The R Book](https://onlinelibrary.wiley.com/doi/book/10.1002/9781118448908).
3. Montgomery (2019), [Design and Analysis of Experiments](https://bcs.wiley.com/he-bcs/Books?action=contents&bcsId=11447&itemId=1119492440).
4. Agresti (2007), [An Introduction to Categorical Data Analysis](https://onlinelibrary.wiley.com/doi/book/10.1002/0470114754).
5. Conover (1999), [Practical Nonparametric Statistics](https://uat.store.wiley.com/en-us/practical-nonparametric-statistics-3rd-edition-p-9780471160687).
6. Efron e Tibshirani, [An Introduction to the Bootstrap](https://www.routledge.com/link/link/p/book/9780412042317).
7. Lumley (2010), [Complex Surveys](https://onlinelibrary.wiley.com/doi/book/10.1002/9780470580066).
8. Ogle (2016), [Introductory Fisheries Analyses with R](https://derekogle.com/IFAR/).
9. Haddon (2021), [Using R for Modelling and Quantitative Methods in Fisheries](https://haddonm.github.io/URMQMF/).
10. Zuur et al. (2009), [Mixed Effects Models and Extensions in Ecology with R](https://link.springer.com/book/10.1007/978-0-387-87458-6).
11. Roback e Legler (2021), [Beyond Multiple Linear Regression](https://bookdown.org/roback/bookdown-BeyondMLR/).
12. Wood (2017), [Generalized Additive Models](https://www.routledge.com/Generalized-Additive-Models-An-Introduction-with-R-Second-Edition/Wood/p/book/9781498728331).
13. Legendre e Legendre (2012), [Numerical Ecology](https://shop.elsevier.com/books/numerical-ecology/legendre/978-0-444-53868-0).
14. Borcard et al. (2018), [Numerical Ecology with R](https://link.springer.com/book/10.1007/978-3-319-71404-2).
15. Hyndman e Athanasopoulos (2021), [Forecasting: Principles and Practice](https://otexts.com/fpp3/).
16. Moore (2016), [Applied Survival Analysis Using R](https://link.springer.com/book/10.1007/978-3-319-31245-3).
17. Burnham e Anderson (2002), [Model Selection and Multimodel Inference](https://link.springer.com/book/10.1007/b97636).
18. Bolker et al. (2009), [Generalized linear mixed models: a practical guide for ecology and evolution](https://pubmed.ncbi.nlm.nih.gov/19185386/).
19. Harrison et al. (2018), [A brief introduction to mixed effects modelling and multi-model inference in ecology](https://pubmed.ncbi.nlm.nih.gov/29844961/).
20. Zuur et al. (2010), [A protocol for data exploration to avoid common statistical problems](https://onlinelibrary.wiley.com/doi/10.1111/j.2041-210X.2009.00001.x/full).
21. Chao et al. (2014), [Unifying Species Diversity ... Through Hill Numbers](https://www.annualreviews.org/content/journals/10.1146/annurev-ecolsys-120213-091540).
22. Lakens (2022), [Sample Size Justification](https://online.ucpress.edu/collabra/article/8/1/33267/120491/Sample-Size-Justification).
23. Anderson e Walsh (2013), [PERMANOVA, ANOSIM, and the Mantel test in the face of heterogeneous dispersions](https://esajournals.onlinelibrary.wiley.com/doi/10.1890/12-2010.1).

Complementos consultados em 21/09/2026: [R — McNemar](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/mcnemar.test.html),
[Penn State — amostragem estratificada](https://online.stat.psu.edu/stat506/Lesson06) e
[Statistics Canada — alocações proporcional e de Neyman](https://www150.statcan.gc.ca/n1/pub/12-001-x/2017001/article/14817/03-eng.htm).
