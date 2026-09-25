# Geração do Projeto R — fechamento da v1

## Roteiro de regressão consolidado — 18/09/2026

O exemplo `EAPACadernos/linear-morfometria-barbo/R/analise.R` incorporou a organização
revisada pelo autor. Depois de dados, modelo, diagnósticos, tabelas e gráficos,
a seção 9 reúne os resultados em frases destinadas aos relatórios. A posição
expressa duas ideias: os textos dependem dos objetos calculados anteriormente;
e, por decisão narrativa, a comunicação começa depois de o pesquisador examinar
também as figuras. A equação permanece na seção gráfica porque é uma anotação
da figura e precisa existir antes de `grafico_regressao`.

Cada frase é guardada em um objeto e apresentada com `print()` para facilitar
o estudo no console. Os QMDs executam o script em um chunk com `include: false`,
portanto essas impressões não aparecem no HTML nem no Word. O script produz
`texto_sintese_estatistica`, não uma conclusão científica. Os QMDs combinam
essa síntese com a interpretação escrita e revisada pelo pesquisador.

Essa decisão está validada no exemplo, mas ainda precisa ser levada ao template
do exportador durante a migração estrutural para dois QMDs.

## Referência didática para os próximos roteiros — 18/09/2026

**Revisão didática aprovada pelo autor.** Após os ajustes nos comentários e
na disposição do código, o autor considerou os arquivos e o roteiro do exemplo
`morfometria-barbo` fáceis de compreender. Em sua avaliação, um professor com
conhecimentos intermediários de R consegue explicar esse percurso aos alunos.
Fica concluída esta rodada de revisão didática do exemplo; as próximas análises
também devem passar pela leitura do autor. A migração completa do exportador
continua pendente, como descrito abaixo.

O autor aprovou `EAPACadernos/linear-morfometria-barbo/R/analise.R` como referência
de clareza para as próximas análises. Preservar etapas numeradas, nomes de
objetos expressivos, cálculos acompanháveis e comentários que expliquem as
decisões e as operações menos familiares. Os gráficos construídos em etapas
com ggplot2 ajudam a tornar o procedimento visível; usar funções de outros
pacotes quando forem necessárias, explicando seu papel.

Ao concluir e validar cada análise, entregar o script para revisão didática
do autor: ele pode ser compreendido por um iniciante com algum esforço e
explicado com facilidade por um professor? Registrar o retorno antes de
considerar encerrada a revisão didática. Isso não impede implementar e
verificar o trabalho previamente autorizado.

## Aprendizagem progressiva nos Projetos R — 18/09/2026

**Primeiro exemplo preparado:** `EAPACadernos/descritiva-barbo`, com uma
variável do mesmo conjunto usado na regressão, tabela de cinco medidas e
histograma. O script tem 102 linhas (incluindo comentários e linhas vazias);
os QMDs completo e artigo têm 69 e 56 linhas. Ambos foram renderizados; o
Word tem duas páginas. Os cinco resumos foram comparados ao motor
`catalyser_resumo_descritivo()` e coincidem, assim como as contagens.
HTML e Word foram inspecionados visualmente. A revisão didática do autor
está pendente. O exemplo foi construído diretamente, sem alterar o exportador.

O autor propôs usar análises mais enxutas como degraus de aprendizagem.
Uma análise de estatística descritiva ou um teste t de uma amostra pode
introduzir toda a organização do Projeto R com poucas operações: localizar
os dados, acompanhar o script, examinar objetos e gerar os relatórios.
Nos projetos seguintes, o aluno reconhece os papéis dos arquivos e acumula
novos conhecimentos de R e de estatística sobre essa base familiar.

A regressão linear simples permanece como referência de clareza e organização.
A quantidade de código, de comentários e de saídas deve acompanhar a necessidade
de cada análise. Não reproduzir todas as etapas da regressão em exemplos mais
simples nem esconder cálculos em funções apenas para reduzir o número de linhas.
Poucas operações compreensíveis são mais úteis que código comprimido.

Descritiva e teste t de uma amostra são candidatos ao primeiro degrau, sem
ordem definitiva escolhida. A sequência e o nível de explicação serão calibrados
com o autor conforme os roteiros forem construídos e revisados. Esta orientação
não registra uma nova análise implementada nem altera o estado da migração.

## O percurso integrado como diferencial — 18/09/2026

Os ganhos recentes devem ser preservados como partes de um único percurso, e
não tratados como recursos independentes. O menu de preparo registra as mudanças
feitas nos dados; a Base Compartilhada mantém a origem comum; as Bases Derivadas
abrem ramos para perguntas específicas; várias análises podem ser reunidas no
mesmo Projeto R; e o script analítico documenta de onde vieram os resultados que
os relatórios comunicam.

Nesse desenho, `R/analise.R` é a fonte da verdade de cada análise. Sua seção
final de textos recolhe resultados antes espalhados pelo console e cria a ponte
para o HTML e o Word. Os QMDs precisam apenas carregar o script e apresentar os
objetos pertinentes a cada público. O HTML documenta o percurso do pesquisador;
o Word se aproxima do artigo. Interpretação biológica, discussão e conclusão
científica permanecem sob revisão humana.

O projeto didático curto acrescenta uma porta de entrada para essa arquitetura.
Ele apresenta as mesmas pastas e os mesmos papéis com poucas linhas; a regressão
linear simples acrescenta exploração, pressupostos, diagnóstico e comunicação;
as análises seguintes acumulam conhecimento sem exigir que o aluno reaprenda a
organização do projeto. Essa progressão materializa a CatalyseR como ponte entre
o estudo dos métodos estatísticos e a prática da pesquisa reprodutível: começa
na interface, torna o código legível e termina em documentos que podem ser
recalculados e avaliados.

## Legibilidade do roteiro e recorte do artigo — 18/09/2026

O exemplo `EAPACadernos/linear-morfometria-barbo` recebeu uma primeira revisão de
legibilidade: mapa de objetos, seções numeradas, conferências e resumos em
etapas, textos numéricos com interpolação legível e exportação de uma única
base processada com IDs. O artigo conserva as dicas no fonte e deixa de
imprimi-las; mantém a síntese dos diagnósticos que afetam a interpretação.
APA permanece como o único CSL, conforme a escolha do autor.

O roteiro de regressão gerado pela CatalyseR também recebeu o cabeçalho,
comentários didáticos e nomes para os gráficos. Isso melhora o script atual;
não equivale à migração para dois QMDs, que continua pendente. As decisões
estão detalhadas no refinamento de 18/09 da especificação de comunicação.

## Próxima estrutura aprovada — 16/09/2026

O autor aprovou um script analítico comentado e dois QMDs separados: caderno
completo em HTML e artigo em Word, ambos executando a mesma análise em R.
A decisão, a árvore de pastas, os cuidados com bases preparadas e os critérios
de validação estão na seção "Direção aprovada — 16/09/2026" de
`MODULO_COMUNICACAO_RESULTADOS.md`. A migração do gerador ainda está pendente;
as validações registradas abaixo se referem ao exportador com QMD único.
O livro recebeu a fundamentação em compêndios de pesquisa, com referência a
Marwick, Boettiger e Mullen (2018), distinguindo o princípio científico da
adaptação didática em dois documentos.

## Renderização pelo desktop resolvida — 15/09/2026

A falha nativa relatada abaixo foi isolada: `PROCESSOR_ARCHITECTURE` não chegava
ao processo R iniciado pelo desktop. O pacote `cli` 3.6.6 consultava a variável
ausente ao encerrar. Informar a arquitetura real (`AMD64` neste computador x64)
permitiu encerrar normalmente, com o mesmo R 4.6.1 e a mesma biblioteca usados
no RStudio. Não foi necessário reinstalar R ou pacotes.

O utilitário `APOIO/scripts/renderizar-quarto-desktop.ps1`, na pasta-mãe, faz
esse ajuste somente no processo e restaura o ambiente depois. O Quarto precisa
também poder escrever no seu cache local para compilar o tema HTML. Com isso,
o projeto de revisão `regressao_barbo_20260915_a420655d1013/regressao_barbo`, em
`APOIO/temp/`, gerou `relatorio.docx` e `relatorio.html` sem erro de renderização,
incluindo os textos de apoio e o diagnóstico de alavancagem.

## Textos de apoio e revisão dos diagnósticos da reta — 15/09/2026

O relatório com uma regressão simples passa a oferecer sugestões editáveis de
Introdução, Material e métodos, Discussão e Conclusão nas seções não preenchidas
pelo autor. Resultados e síntese da conclusão vêm dos números recalculados no
Render. O texto do autor prevalece; relatórios com várias análises não recebem
uma conclusão global baseada apenas na última reta. O contexto do barbo só entra
quando a origem registrada é `EAPADados::morfometria_barbo`: medidas corrigidas,
associação de forma e cuidado com as cinco populações, sem alegar causalidade,
crescimento individual ou independência comprovada.

A revisão considerou Zuur, Ieno e Elphick (2010), DOI
10.1111/j.2041-210X.2009.00001.x, a documentação de diagnósticos do NIST
(https://www.itl.nist.gov/div898/software/dataplot/refman1/auxillar/regrdiag.htm)
e a documentação de `performance::check_model`. O complemento adotado foi
resíduos padronizados versus alavancagem, junto de Cook, no caderno HTML.
Os limites são referências de triagem; nenhum ponto é excluído automaticamente.
Os rótulos remetem às linhas da base preparada antes da retirada de pares
incompletos. Permanecem Shapiro, Breusch-Pagan, resíduos versus ajustados, Q-Q e
Durbin-Watson apenas quando há ordem real. Não se acrescentam testes redundantes
nem se usa VIF numa reta com um único preditor.

Conferência: asserções do roteiro concluídas para coeficientes, IC, alavancagem
contra `hatvalues`, rastreio de linhas com NA, conclusão sem associação e com
diagnósticos desfavoráveis, texto autoral, contexto da origem e múltiplas retas.
O projeto exportado manteve script e QMD sincronizados, e o novo gráfico foi
inspecionado. A renderização nesta sessão executou os 35 passos, mas a falha
nativa de encerramento do R impediu confirmar o Word e HTML finais. O autor
havia confirmado ambos os formatos antes destas alterações.

## Navegação entre resultados da regressão — 15/09/2026

A troca de aba atualizava os campos compartilhados de título e eixos e, por isso,
marcava uma execução válida como pendente. Cada gráfico agora conserva seus
próprios campos; navegar não escreve nos parâmetros da reta. Os rótulos da reta
também permanecem no código registrado quando uma aba de diagnóstico está aberta.

O teste de interface com `morfometria_barbo` reproduz a devolução das mensagens
dos campos pelo navegador: falha no código anterior e conclui as asserções após
a correção. Confere tabela, reta, resíduos e Q-Q, preservação do modelo e do
registro, e invalidação por mudanças reais nos dados, no IC e nos rótulos da reta.
As asserções passaram; o processo R ainda terminou com a falha nativa de saída
observada neste ambiente, portanto isso não equivale a uma execução com código
de saída zero nem substitui a conferência visual na aplicação reiniciada.

## Exemplo principal da regressão: morfometria do barbo — 15/09/2026

O autor escolheu `morfometria_barbo` para conduzir a revisão da regressão,
seguindo o modo de trabalho da ANOVA. Configuração inicial: resposta
`comprimento_cabeca`, preditor `distancia_pre_peitoral`, reta global sem
agrupamento, IC de 95% e autocorrelação desativada enquanto não houver ordem
real de coleta informada.

O teste do roteiro e o projeto de exemplo gerado por ele passam a usar esse
conjunto como referência principal. Camarão e `cars` permanecem como casos
com sinais de inadequação para conferir a honestidade do texto automático.
Os rótulos do exemplo identificam as medidas como corrigidas pelo tamanho;
a introdução explica que a relação descreve forma corporal, não crescimento.

Conferência com `lm`, `confint`, `shapiro.test` e `performance`: 100 observações,
inclinação 0,98974518, R² 0,89655657, Shapiro p = 0,44001152 e Breusch-Pagan
p = 0,42550214. Asserções de cálculo e sincronização script–relatório concluídas.
Esses p-valores não comprovam pressupostos, e as cinco populações exigem leitura
do delineamento para avaliar independência. Esta escolha de exemplo não encerra
a revisão da regressão nem substitui a conferência visual do autor.

## Regressão linear simples — consolidação do caminho único de 15/09/2026

A regressão carregava três caminhos de exportação em paralelo. O ciclo consolidado
tem um só, o mesmo da ANOVA: `exportacao_comunicacao.R`. Foram retirados, a pedido
do autor, com a ANOVA de um fator como inspiração e preservando as
especificidades da reta:

- os botões **Baixar Relatório Word (.docx)** e **Exportar Projeto R (.zip)** do
  menu, que geravam um projeto de outra geração (`scripts/analise.R`,
  `relatorios/relatorio_regressao.qmd`, `README.txt`) sem passar pelo registro de
  execuções e sem `R/analise.R` comentado nem `R/funcoes.R`;
- o modal órfão `export_qmd` e o gerador `qmd_code_text()`, que **não tinham botão
  que os acionasse** em nenhum ponto do aplicativo;
- `templates/relatorio_regressao.qmd` e `templates/funcoes_regressao.R`, que só
  esses caminhos usavam.

O card do menu passa a trazer o mesmo aviso da ANOVA e conserva apenas **Ver
Código R**, que mostra o código de consulta da configuração em vigor e registra
`codigo_r` na execução. Esse registro é preservado de propósito: o exportador
recorre a ele como reserva para tipos de análise sem código passo a passo, e as
análises irmãs gravam o mesmo campo. Retirá-lo é decisão do autor, não deste ciclo.

**Parâmetros que a interface não alimentava.** O roteiro já era parametrizado, mas
três escolhas não chegavam ao Projeto R:

- o **nível de confiança** era gravado fixo em 0,95. Agora há `sliderInput` de 80 a
  99, como na ANOVA, e o valor escolhido alimenta a tabela de coeficientes da tela
  (`broom::tidy(conf.level=)`), o rótulo da coluna, o código de consulta e o
  roteiro exportado;
- o **tema do gráfico** não integrava a assinatura de execução: trocá-lo não
  invalidava o resultado e o roteiro caía no tema padrão, ignorando a escolha.
  Passa a integrar a assinatura e chega ao roteiro como `tema_grafico`;
- **título e rótulos personalizados** não eram propagados aos parâmetros. Agora
  chegam ao roteiro como `titulo_analise`, `rotulo_resposta` e `rotulo_preditor`,
  no mesmo contrato da ANOVA: o rótulo muda só a apresentação — título e eixos do
  gráfico e a narrativa —, enquanto o nome da coluna continua no código que lê e
  calcula. Rótulo em branco volta ao nome da variável, e sem título informado o
  gráfico não recebe título nenhum.

**Diagnóstico no relatório.** As quatro verificações da reta já eram emitidas no
`relatorio.qmd`, mas **sem legenda**: elas apareciam mudas e não numeradas. Passam
a ter `fig-cap`, no padrão didático do EAPACaderno — a legenda diz o que se procura
e o que é sinal de problema —, e dimensão declarada.

**Verificação executada em 15/09/2026.** R 4.6.1, `broom` 1.0.13, `performance`
0.17.1. O roteiro foi executado em memória sobre `camarao_vannamei_biometria`
(EAPADados 0.1.12) e conferido contra `lm`, `confint` e `shapiro.test`:
coeficientes, intervalos e p-valor idênticos; IC a 90% reproduz `confint(level=.90)`
(5,79 a 6,30); o tema escolhido é respeitado; os rótulos chegam aos eixos e à
narrativa sem alterar coeficiente, R² ou intervalo. Um projeto real foi gerado e o
par script–relatório conferido com `conferir_codigo()`.

Validação em camadas, na ordem da skill: (1) parse dos arquivos alterados; (2) teste
puro do cálculo — `test_regressao_roteiro.R` confere coeficientes, IC, Shapiro e
Cook contra o R base; (3) teste Shiny do estado explícito —
`test_regressao_interface.R`; (4) duas execuções independentes no mesmo projeto:
15 trechos por análise, 36 no total, nenhum rótulo de chunk duplicado e cada análise
com a sua própria `variavel_resposta`; (5) geração e inspeção do script numerado;
(6) execução de `R/analise.R` em processo R novo, fora da CatalyseR, com sucesso;
(7) renderização do `relatorio.qmd` para HTML: o Quarto encerrou com **código 0** e o
HTML traz **quatro figuras numeradas com legenda** — a reta e as três verificações
aplicáveis —, o título personalizado do gráfico, os rótulos nos eixos e o texto
registrando o desvio de normalidade dos resíduos do camarão. O gráfico de ordem não
aparece porque a autocorrelação não foi solicitada, como deve ser.
Passam também os testes da ANOVA, do registro de execuções, da comunicação e da
exportação enxuta. O Word segue pendente de conferência no RStudio.

**Limitações desta rodada.** A falha nativa do R ao encerrar (`-1073741819`,
`ucrtbase.dll`) persiste e impede declarar saída zero da suíte. O `APOIO/temp` do
ambiente de trabalho recusa gravação, e por isso `test_regressao_roteiro.R` não
conclui ali; fora dele, conclui. A suíte inteira por `run_tests.R` para antes, na
validação de sintaxe, por um erro **pré-existente e alheio** a esta rodada: o
`templates/anova_um_fator/analise.R` contém `{{RESPOSTA_R}}`, que só é R válido
depois da substituição, e o verificador tenta parsear o template cru. No ambiente
de trabalho, o Quarto só renderiza com acesso ampliado: ele captura a saída do
`Rscript` por pipe, e a política de arquivos bloqueia esse stdio. O Render de
Word do projeto exportado continua pendente de conferência no RStudio do autor.

## Regressão linear simples — primeira rodada de 14/09/2026

Aplicado o roteiro fornecido pelo autor à reta global: coeficientes com EP,
IC, t e p; métricas com R² ajustado, F, graus de liberdade e AIC; Shapiro-Wilk
e Breusch-Pagan; Durbin-Watson opcional, condicionado à ordem real de coleta.
A tela, o código de consulta e o Projeto R recebem essas informações.

O Projeto R usa `broom::tidy()`, `glance()` e `augment()`, com o ajuste e as
frases explícitos em `R/analise.R`. O QMD recebe o código pelos mesmos vínculos
`# fonte:` da ANOVA, e os gráficos de diagnóstico ficam no caderno HTML.
O texto informa associação, EP e IC, sem afirmar causalidade ou confirmar
pressupostos por ausência de significância. Retas por grupo conservam sua rota
anterior; esta rodada trata a regressão linear simples com uma reta global.

Conferência com `cars`: β = 3,93240876; R² = 0,65107938; Shapiro-Wilk
p = 0,02152458; Breusch-Pagan (`performance`) p = 0,03104933. Portanto, o texto
do exemplo deve registrar evidência contra normalidade e variância constante.

Asserções de resultados, exportação geral e interface concluídas. O Quarto
executou os 33 passos do exemplo, mas encerrou antes do Word com erro nativo
do R `-1073741819`. Render final no RStudio e aprovação do autor permanecem
pendentes. Detalhes em `docs/testes/fases-3/REGRESSAO_LINEAR_2026-09-14.md`.

## ANOVA concluída pelo autor — 14/09/2026

O autor declarou concluída a ANOVA de um fator para resposta métrica.
Essa aprovação encerra a etapa, preservando o histórico das verificações
automatizadas e de suas limitações, sem reabrir a análise por acabamento.
A próxima etapa anunciada é regressão linear; a primeira rodada está registrada acima.

O guia [Código que Fala](../APOIO/documentacao/codigo-que-fala-guia-estilo.md),
fornecido pelo autor, fica como referência de legibilidade para as próximas
saídas de código do ecossistema. As notas de aplicação estão no
[índice da documentação](../APOIO/documentacao/README.md).

## Simplificação final do script — 14/09/2026

A pedido do autor, as letras agora usam `multcompLetters4()` e as frases
e a classe do efeito usam `case_when()`, com p-valores brutos extraídos antes
da formatação. O restante do script segue a mesma organização comentada.
A versão curta exige níveis de grupo sem hífen, com orientação explícita
no código e no README. Cabeçalhos de colunas com espaços continuam aceitos.
Essa decisão substitui a matriz explícita descrita nos registros anteriores.
Validados resultados, nomes, limites das decisões e sincronização do código;
permanece a conferência final do Render no RStudio.

## Fechamento didático da ANOVA de um fator — 14/09/2026

O gerador da CatalyseR passa a exportar preparo, pressupostos e Tukey em
passos numerados, com comentários explicativos e variáveis intermediárias
legíveis. Os nomes da resposta e do fator vêm das escolhas da execução.
A mudança vale para ANOVA isolada ou acompanhada de outras execuções;
os projetos já baixados não foram alterados.

O diagnóstico de Cook foi retirado do roteiro básico a pedido do autor.
Continuam os testes de Shapiro-Wilk e Levene, o gráfico Q-Q e o gráfico de
resíduos versus ajustados, com a independência discutida pelo delineamento.
Cook é uma investigação complementar, não uma etapa necessária para calcular
a ANOVA ou Tukey.

**Registrado para a v2:** desenvolver no Projeto R o roteiro didático de
alternativas quando a ANOVA clássica não for adequada, incluindo a integração
da exportação de Kruskal-Wallis, já disponível no menu da IDE. A normalidade
avaliada é a dos resíduos; um p-valor de Shapiro isolado não deve acionar
automaticamente a troca de método. Sem implementação dessas alternativas na v1.

Verificações de exportação, reprodução numérica, gráficos e sincronização
script–relatório concluíram as asserções para uma e duas ANOVAs. O R voltou
a encerrar com código 1 após os marcadores de sucesso; o Render final de
Word/HTML permanece para conferência no RStudio.

## Acabamento aprovado e correção da origem CSV — 13/09/2026

O autor aprovou tabela e paginação à esquerda, gráfico de barras com média ± DP
e nomes exportados ao trocar de aba. Pediu retirar o gráfico final de médias com
IC: removido da tela, script e QMD, incluindo projetos com várias ANOVAs.

O Excel bruto de CSV agora usa o nome do CSV, sem herdar `regressao` do seletor
de abas. Conferidos ZIP, dados, nome, leitura, gráficos remanescentes e vínculo
script–relatório. O autor considera a ANOVA de um fator para resposta métrica
quase concluída. R permanece com saída 1 após as asserções; não houve novo Render
automatizado. A aprovação visual desta fase vem dos testes e prints do autor.

## Revisão do fim da tarde de 13/09 — simplicidade do código

Aplicados: retirada da pasta de metadados e da tabela que dependia de `bases.csv`;
Excel bruto nomeado pela aba; preparo simples encadeado; moda apenas quando usada;
ANOVA explícita tanto isolada quanto acompanhada. Execuções desmarcadas ficam
como código de estudo. As duas execuções idênticas do milho foram preservadas.
O autor aprovou os cinco ajustes da interface e conferiu os dados exportados.

Originais comparados: ZIP de milho das 09:45 e pasta `EAPACaderno/milho` da tarde.
O primeiro usa o modelo isolado; a segunda tinha acionado o modelo geral por conter
duas execuções. Valores iguais, nome da resposta diferente. Cópia revisada e
comparação em `../projeto-exemplo/milho-simplificado-20260913/`.

Asserções de reprodução, bases derivadas, componentes, gráficos e sincronização
concluídas. Mantém-se a pendência do Render completo de Word/HTML no RStudio;
o problema local de encerramento do R impede declarar homologação final.
Detalhes no registro de aprendizados. Nenhuma reinstalação ou commit nesta rodada.

## Ajustes solicitados e aplicados em 13/09/2026

Após conferir o conjunto milho, o autor confirmou nome do projeto, nome do
arquivo bruto e exportação somente da aba utilizada. Pediu encurtar o QMD
carregando as bases preparadas. Esta decisão substitui a reconstrução do preparo
durante o Render descrita no registro de 12/09 abaixo.

O ZIP passa a incluir RDS da compartilhada e de cada derivada utilizada.
O QMD lê a base apropriada; o script conserva a importação, a receita de preparo
e a conferência da compartilhada, agora sem duplicação. Para adotar mudanças
no preparo, o pesquisador confere e salva os RDS pelas linhas comentadas do
script. O Render e `atualizar_codigo()` não salvam dados. Os Excel permanecem
como fotografias da exportação original.

Na ANOVA isolada, o chunk de análise foi dividido em modelo, pressupostos,
Tukey e preparação do texto. As letras de Tukey agora têm um laço comentado
por dupla de grupos. A figura final acrescenta média ± DP ao lado do ponto
da média, preservando IC nas barras e explicando ambos na legenda.

Interface: seletores de aba Excel e dataset ampliados, com rolagem; cinco tipos
visíveis no dropdown do modal; tabelas do preparo compactas à esquerda; sub-aba
renomeada para “Adicionar ao Projeto R”. Novas execuções de ANOVA começam com
todos os componentes editoriais marcados. Desmarcações posteriores são preservadas.

Conferidos visualmente dataset, modal de tipos, tabela e nome da sub-aba numa
sessão de desenvolvimento separada. Executados os chunks dos dois projetos por
RDS e comparados os coeficientes da ANOVA com a base salva. Figura de teste
inspecionada. Na interface, os sete componentes da ANOVA apareceram marcados.
As verificações de exportação geral, ANOVA, vínculo script–QMD, registro e preparo
completo concluíram suas asserções em execuções isoladas. A tentativa DOCX com o
modelo novo executou todos os 39 passos e parou de avançar após `relatorio.knit.md`;
ao encerrar a tentativa, o Quarto confirmou novamente a falha nativa
`-1073741819`, sem documento final. Amostras em `../projeto-exemplo/ajustes-20260913/`. O teste final de
Word e HTML no RStudio permanece separado desta verificação: a falha nativa do
R ao encerrar ainda impede concluir o Render pela linha de comando neste ambiente.

## Registro do plano aprovado em 12/09/2026

Atualizado em 12/09/2026. Plano aprovado pelo autor, incluindo a instalação de
CatalyseR, EAPADados e dos pacotes de leitura, preparo e análise. O menu Preparar
Dados está aprovado para a v1 e fica fora desta revisão.

## Referências e alcance

Este plano combina as decisões da conversa, o registro
`APRENDIZADOS_PREPARO_E_EXPORTACAO_2026-09-11.md`, o código atual do exportador
e o documento do autor `../instrucoes_relatorio_R_01.docx`.

O anexo orienta retirar os dois arquivos de renderização auxiliar, levar apenas
a aba utilizada ao projeto e avaliar a fonte do código, as funções de texto e a
organização das bases. As propostas abaixo distinguem essas orientações das
escolhas de implementação registradas abaixo. Nenhuma nova análise ou
operação de preparo entra nesta fase.

## Código para estudar e para alimentar o relatório

A decisão explicitada na conversa é usar `R/analise.R` para estudar e como
fonte do código de `relatorios/relatorio.qmd`. O texto científico e as legendas
continuam no relatório. O script recebe os comentários sobre R; o relatório
combina texto e código executável.

| Fonte do código | Vantagem | Custo para o pesquisador |
|---|---|---|
| `analise.R` | Código comentado em uma sequência contínua, útil para estudar e experimentar; alimenta os chunks do relatório. | Depois de editar e salvar o script, é preciso atualizar os chunks. |
| `relatorio.qmd` | Texto e código são editados no mesmo arquivo. | Manter também um script de estudo exige copiá-lo novamente quando a análise muda. |

Decisão aplicada: manter `analise.R` como fonte do código, conforme a decisão da
conversa. Os trechos têm marcadores como `## ---- tratar ----`; cada chunk
indica os trechos que recebe em `# fonte: tratar, conferir-dados`.

O fluxo cabe em três ações: editar e salvar o script; executar o chunk
`atualizar`; escolher a saída no Render. A atualização copia as linhas de
código, preserva o texto e as opções dos chunks e não executa a análise.
`conferir_codigo()` verifica a correspondência no início do Render e informa
quais trechos estão desatualizados. Os chunks continuam preenchidos e podem
ser executados linha a linha no RStudio.

A implementação usa as funções já existentes no projeto, em R base. Revisar
os comentários para que a ligação seja compreensível, sem criar um novo sistema
de execução ou instalar ferramentas para essa cópia.

## Renderização escolhida pelo pesquisador

Orientação do anexo: retirar `R/gerar_word.R` e `_quarto.yml` do projeto
exportado. O YAML do próprio `relatorio.qmd` mantém os formatos HTML e Word.
O README deve explicar a escolha de cada formato na seta do Render do RStudio.
Para atualizar as duas saídas, o pesquisador renderiza cada uma explicitamente.

É preciso revisar também as instruções que hoje prometem atualizar o Word ao
gerar o HTML e o link de outros formatos, para não apontar para um documento
que ainda não foi produzido. A remoção foi aplicada aos projetos exportados;
as instruções agora explicam que cada Render atualiza somente o formato escolhido.

## Dados brutos e bases preparadas

Orientação do anexo: `dados/brutos/` recebe somente a aba usada na análise,
em um Excel com uma aba, antes das escolhas de preparo. O arquivo de origem do
pesquisador permanece intacto. O arquivo exportado representa os valores lidos
pela IDE; não deve ser descrito como cópia integral da pasta de trabalho Excel.
Registrar o nome do arquivo de origem e da aba utilizada.

Decisão aplicada: usar nomes coerentes, `base_compartilhada.xlsx` e
`base_compartilhada.rds`, explicando a finalidade da fotografia em RDS; exportar
também cada base derivada utilizada, em um Excel com nome legível. Evitar RDS
adicionais para cada derivada se não houver uma finalidade concreta.

Separar a fotografia da exportação dos dados reconstruídos no Render: uma
conferência não pode sobrescrever o próprio arquivo que usa como referência.
Os Excel preparados permanecem como fotografia da IDE, sem atualização pelo
Render. Os dois modelos comparam a base reconstruída com o RDS compartilhado.

Mais arquivos RDS não aceleram, por si, o fluxo atual. O Render refaz a
importação, o preparo compartilhado e o ramo de cada análise. Manter esse
percurso na v1, sem cache, é a proposta: ele torna verificável a passagem do
mouse ao código. Medir o tempo de renderização antes de propor atalhos.

## Funções auxiliares e redação dos resultados

O modelo ANOVA já combina três peças: números extraídos dos objetos do R,
formatadores como `fmt()` e `formatar_p()`, e frases escolhidas conforme os
resultados (`frase_anova`, `frase_normalidade`, `frase_levene`). O `.qmd` insere
esses objetos no texto. Nos projetos com várias análises, a apresentação também
usa funções do pacote CatalyseR. Essa diferença precisa ficar clara no manual.

Revisar o que cada função recebe, o que devolve e um exemplo curto. Manter a
análise no script e as definições reutilizáveis em `R/funcoes.R`. Frases que
aparecem uma vez e são fáceis de ler podem continuar no trecho de preparação
do texto, sem virar uma função genérica.

Avaliar `glue` quando ele tornar a montagem de frases mais legível; o pacote
insere valores em textos por nomes entre chaves. A alternativa `stringr::str_glue()`
aproveita uma dependência já usada no projeto. Documentação:
[glue](https://glue.tidyverse.org/) e
[interpolação de texto](https://glue.tidyverse.org/reference/glue.html).

Para funções públicas do pacote CatalyseR, manter a documentação com roxygen2:
argumentos, retorno e exemplos próximos do código. Isso é uma ferramenta de
desenvolvimento do pacote, não uma exigência adicional para o aluno renderizar
o projeto. Referência: [documentar funções](https://roxygen2.r-lib.org/articles/rd-functions.html).

Preservar a decisão recente sobre datas: o código exportado chama
`catalyser::converter_datas()`; a definição continua no pacote. Identificar
essa dependência quando a receita usar datas, inclusive no modelo ANOVA.
Não prometer funcionamento apenas com CRAN nesses casos.

## Ordem de conclusão e evidências

1. Consolidar as orientações adicionais do autor e a árvore final de arquivos.
2. Concluir a ligação script–relatório e revisar as instruções de edição.
3. Retirar a renderização auxiliar e exportar somente a aba utilizada.
4. Definir e aplicar a organização dos Excel e das fotografias das bases.
5. Revisar funções auxiliares e textos automáticos nas análises já disponíveis.
6. Extrair um ZIP e executar a importação, o preparo e as análises em sessão R
   nova; comparar dados e resultados com a IDE. Repetir para ANOVA isolada e
   para projeto com várias execuções, incluindo uma fora do relatório.
7. No RStudio, abrir o `.Rproj`, atualizar o código e renderizar Word e HTML;
   verificar tabelas, figuras, legendas, referências e diagnósticos exclusivos
   do HTML. Registrar essa conferência separadamente dos testes do código.

## Situação desta rodada

Ajustado no código-fonte: o modelo ANOVA passa a receber seus 14 chunks de
análise a partir do script, com atualização e conferência comuns ao exportador
geral. As orientações de edição foram alinhadas. O modelo dedicado fica
restrito a uma execução isolada: quando há outras execuções fora do relatório,
o caminho geral preserva o acervo nos metadados.

O teste novo extraiu um ZIP, confirmou os vínculos, alterou o preparo no script,
detectou a divergência e atualizou o cálculo no relatório sem mudar o texto ou
as opções dos chunks. Também confirmou a preservação de uma execução
desmarcada. A bateria anterior de preparo completo concluiu os quatro casos
de exportação geral/ANOVA e manutenção/remoção da coluna original.

Também foram aplicados: retirada de `_quarto.yml` e `R/gerar_word.R` da saída;
Excel bruto com somente a aba utilizada; nomes coerentes da base compartilhada;
Excel de cada derivada utilizada; receita de instalação comum aos dois modelos;
orientações de edição, dependências e escolha explícita do formato. A instalação
inclui os componentes utilizados do tidyverse, `lubridate` e `readxl`, além de
`catalyser` e `EAPADados`. O trecho tem `eval: false`: instalar é uma ação inicial
do pesquisador, nunca um efeito colateral do Render. As funções auxiliares
mantêm argumentos, retorno e exemplos comentados; um intervalo textual passou
a usar `stringr::str_glue()`, sem criar uma nova camada de funções.

Dois ZIPs foram gerados e extraídos em
`../projeto-exemplo/fechamento-v1-20260912/`: ANOVA isolada e ANOVA com gráfico
de linhas. Conferidos aba única, RDS compartilhado e Excel da derivada. Os testes
`test_exportacao_comunicacao.R`, `test_anova_script_relatorio.R`,
`test_exportacao_preparo.R` e `test_preparo_comunicacao_completo.R` chegaram às
mensagens finais de sucesso, sem falhas de asserção após os ajustes.

**Ainda não declarar o fechamento aprovado.** O processo R sofre uma falha nativa
ao encerrar (`0xc0000005`, saída `-1073741819`), reproduzida até com `library(cli)`
isolado e nas instalações R 4.6.0 e 4.6.1. O log do Windows aponta `ucrtbase.dll`.
O computador é Intel/x64; a sugestão automática do Quarto sobre Windows ARM não
se aplica. A causa ainda não foi determinada e nenhuma instalação foi alterada.

O Render DOCX da ANOVA e o Render HTML do projeto com várias análises executaram
todos os chunks, mas o Quarto parou nessa falha antes de produzir as saídas finais.
Não houve inspeção visual de Word/HTML finais nem medição de renderização bem-sucedida.
A revisão automática bloqueou ativar a sessão RStudio `milho`, de outro projeto;
a tentativa de lançar uma janela separada não criou outra sessão. A conferência
pelo botão Render permanece pendente. O encerramento dos testes com erro nativo
também impede declarar a suíte integralmente aprovada.

Próxima etapa: resolver a falha local do R e renderizar os dois formatos em uma
sessão nova do RStudio, verificando tabelas, figuras, referências e conteúdo
exclusivo do HTML. Sem commit, push ou reinstalação nesta rodada.
