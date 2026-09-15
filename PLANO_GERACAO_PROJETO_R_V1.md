# Geração do Projeto R — fechamento da v1

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
