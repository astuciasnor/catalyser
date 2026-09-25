# Explorando os Dados — cinco perguntas para decidir o próximo passo

Implementação local de 20/09/2026, sobre a versão 0.1.8. Ampliação expressamente
solicitada pelo autor; inclui a exploração de Box-Cox antes reservada ao backlog.
Não aplica transformações nem muda a Trilha de Preparo.

## Caminho de uso

O menu chama-se **Explorando os Dados** e tem cinco opções: **Explorar Dataset**, **Conhecer as Variáveis**,
**Encontrar Relações**, **Avaliar Pressupostos** e **Transformar Variáveis**.
O nome da seção aparece no início da página, antes do seletor da base.
Cada uma tem suas sub-abas analíticas e uma aba separada
**Inserir análise**. Cada área aceita a base compartilhada ou uma derivada pronta,
inclusive a amostra de 200 adultos de cada sexo de `abalone_adultos`.

Escolha a variável e clique em **Executar análise**. Depois de trocar a variável,
execute novamente. As configurações executadas ficam disponíveis no seletor da
aba **Inserir análise**; selecione uma delas e adicione o resultado ao projeto.
Trocar a configuração volta o registrador para **Nova execução**. Atualizar um
registro anterior continua possível, mas exige selecioná-lo explicitamente.

### Conhecer as Variáveis e Visualização dos Dados

**Conhecer as Variáveis** concentra a leitura descritiva. Nas contínuas, uma tabela
com uma linha por variável reúne n válido, ausências, média, mediana, desvio-padrão,
mínimo e máximo. A linha escolhida controla o gráfico ao lado. Média e mediana
afastadas recebem destaque como pista de assimetria, sem transformar a pista em
diagnóstico automático.

A segunda aba constrói frequências por classes para contínuas e por categoria para
dados categóricos ou de contagem. As duas formas incluem proporções e acumuladas,
destacam a moda e apresentam uma frase curta de leitura. A acumulada de categorias
nominais vem acompanhada do aviso de que não há ordem substantiva. O número de
classes começa por Sturges e pode ser ajustado.

O painel usa aproximadamente 60% da largura para a tabela e 40% para o apoio
visual. **Explorando os Dados** entrega o gráfico pronto para interpretar;
**Visualização dos Dados** usa o mesmo motor `ggplot2` com controles e código R
visível para construir a figura. As frequências daqui são descritivas. Testes de
proporção e qui-quadrado permanecem em **Frequências e Proporções**.

As prévias pertencem à sessão, base, receita e revisão em que foram calculadas.
Uma mudança nessa origem limpa as prévias pendentes; os registros já inseridos
permanecem no projeto e seguem as regras existentes de atualização. Alterar um
ramo diferente não limpa as prévias da base atual. Seleções repetidas não criam
fotografias duplicadas; nunca há inserção automática no relatório.

## Primeiro olhar e interpretação

- Explorar Dataset pergunta **“Que dados eu tenho na mão?”** e entrega um único
  panorama: nome, tipo, valores ausentes e uma pista inicial própria para cada
  tipo de variável.
- Conhecer as Variáveis pergunta **“Que história cada variável conta?”**. Detecta
  categórica nominal, categórica ordinal, numérica discreta ou numérica contínua;
  mostra frequências e barras para a primeira família, ou centro, dispersão e
  distribuição para a segunda. A leitura pode ser corrigida pelo aluno.
- Encontrar Relações pergunta **“Uma coisa varia com a outra?”**. Para duas
  categóricas, mostra contingência e barras agrupadas; para categórica e numérica,
  resumo e boxplot por grupo; para duas numéricas, correlação e dispersão. Cada
  resultado expõe o cartão “Isto costuma pedir”, com atalho ao módulo sugerido.
- O botão **Mais medidas** mantém os detalhes recolhidos até serem úteis. Os
  registros antigos e suas técnicas continuam reproduzíveis pelo mesmo motor R.
- Avaliar Pressupostos: Shapiro-Wilk, QQ-plot, IQR e escore-z.
  Shapiro só é calculado para 3–5.000 valores não constantes,
  sem subamostragem silenciosa. O QQ-plot usa quantis beta das estatísticas de ordem
  convertidos pela normal com média/DP estimados: banda **pontual aproximada de
  95%**, não simultânea. Outliers são alertas, não exclusões.
- Transformar Variáveis: comparação exploratória de log, raiz e Box-Cox,
  antes reunida em Pressupostos. Não altera a base original. Box-Cox requer
  positividade e usa perfil de verossimilhança de um modelo só com intercepto,
  em uma grade de −2 a 2. Não é otimização do modelo científico nem seleção por
  p-valor. Para modelar, examine também os resíduos e o delineamento.

Referências dos procedimentos: documentação de
[Shapiro-Wilk](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/shapiro.test.html),
[cor.test](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/cor.test.html) e
[MASS::boxcox](https://stat.ethz.ch/R-manual/R-devel/library/MASS/html/boxcox.html).

## Código e compatibilidade

`R/descrevendo_dados.R` reúne catálogo, tipos e código canônico. A interface,
`catalyser_executar()` e o código de estudo exportado executam esse mesmo código.
O projeto mostra as escolhas e as funções estatísticas, com trechos nomeados pelo
método e variáveis. Nenhum cálculo depende da sessão Shiny.

Os novos registros usam `tipo = "descricao_exploratoria"` e identificadores
`descricao_<area>`. Os módulos e o replay de `estatistica_descritiva` antigos foram
preservados para compatibilidade. Correlação e dispersão passam a ser acessadas
em **Explorando os Dados > Encontrar Relações**, deixando de ocupar entradas duplicadas nos
menus de regressão e visualização. Os controles antigos de estética permanecem
nos módulos legados; na interface nova os gráficos usam Ocean e podem ser
personalizados pelo código exportado.

## Validação e homologação

A navegação começa em **Planejando sua Pesquisa**. **Comunicação de Resultados**
fica na penúltima posição dos botões visíveis, imediatamente antes de **Ajuda**.
A mudança de nomes e a separação de Transformar Variáveis conservam os 15 métodos;
registros anteriores continuam usando o mesmo tipo e código de cálculo.

Os testes `test_descrevendo_dados.R`, `test_descrevendo_interface.R` e
`test_descrevendo_exportacao.R` verificam cálculos, código independente, replay,
limites de validade, trocas de variável, dois registros independentes, mudanças
de base e exportação dos 15 métodos com a amostra de 400 adultos.

Para homologar no Windows: abra a CatalyseR local atualizada, carregue
`abalone_adultos`, execute comprimento e peso em Medidas-resumo e insira ambos.
Confira a mesma sequência com a derivada de 200 por sexo. Em Comunicação de
Resultados, exporte o Projeto R e renderize HTML/Word no RStudio. O título de cada
resultado deve identificar a variável; o tamanho da amostra deve corresponder à
base escolhida. Aprovação visual e didática do autor continua sendo uma etapa
separada dos testes automatizados. Nenhum commit, publicação ou atualização da
instalação de uso cotidiano é realizado por estes testes.

### Resultado da implementação inicial

Os três testes específicos passaram, incluindo as 15 sub-abas Shiny sobre a
amostra de 400 adultos. No navegador foram conferidos menu, sub-abas, resumo,
histórico e inserção de duas variáveis como registros independentes. Um Projeto R
com todos os 15 métodos gerou HTML e Word; o cache do Quarto exigiu permissão
adicional no ambiente de validação. A instalação de teste e o build passaram.
`R CMD check --no-manual --no-vignettes` terminou sem erros, com um aviso de
caracteres não ASCII e duas notas (Imports e uma variável global da ANOVA).

A suíte ampliada não ficou integralmente verde nessa primeira rodada: após repetir
os testes afetados pelos ajustes finais, 22 dos 29 arquivos passaram. Havia falhas em
`test_bases_derivadas.R`, `test_menu_preparando_dados.R`,
`test_exportacao_preparo.R`, `test_preparo_csv_datas.R`,
`test_logisticas_separadas.R`, `test_regressao_roteiro.R` e
`test_anova_preparo_projeto.R`. São asserções de interface/preparo e caminhos
dos projetos desses outros fluxos; não foram alteradas para forçar aprovação.
Assim, esta entrega pode ser revisada localmente, mas não representa homologação
geral da versão nem recomendação de publicar todo o conjunto de mudanças pendentes.

### Correção das falhas 1–4 e 6 — 20/09/2026

Por solicitação do autor, foram corrigidas as verificações desatualizadas das
bases derivadas, do menu de preparo, do aviso de receita desatualizada, das datas
exportadas e do roteiro de regressão. Os cinco testes passaram individualmente.
Não foi necessário alterar os cálculos nem os módulos da aplicação nesta rodada.

Os testes agora conferem o formulário bloqueado sem depender da ordem dos
atributos HTML, distinguem grupos de ações de abas e verificam que a mudança da
receita preserva a última execução, mas impede seu uso como base pronta.
Nas exportações, o script continua reconstruindo os dados com a conversão
canônica de datas; o relatório lê a base preparada. Valores, classes de data e
recortes são conferidos nos dois caminhos, com CSV e Excel.

Na regressão simples, o teste acompanha a organização atual: um script e dois
relatórios que o executam. Cada entrada roda em um processo R separado, e seus
coeficientes, intervalos de confiança, R² e tamanho da amostra são comparados
com o cálculo de referência. O projeto do barbo também gerou o relatório
completo em HTML e o artigo em Word, sem erro de renderização. Isso não substitui
a revisão visual e didática do autor.

As falhas 5 (`test_logisticas_separadas.R`) e 7
(`test_anova_preparo_projeto.R`) ficaram para a revisão específica dessas análises.
A instalação de uso cotidiano não foi atualizada.

Na repetição dos 29 arquivos de teste, executados individualmente, 27 passaram.
As duas falhas restantes são as adiadas: uma asserção antiga da interface
logística e a resolução do caminho de `R/funcoes.R` no teste do projeto de ANOVA.
O comando geral `run_tests.R` ainda para antes dos testes: sua checagem de sintaxe
tenta interpretar o template de ANOVA com os campos `{{...}}` ainda não
substituídos. Esse bloqueio também foi preservado para a revisão desse fluxo;
o resultado de 27/29 não significa que o comando geral passou.
