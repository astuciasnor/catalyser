# Aprendizados da conversa — preparo e exportação da CatalyseR

## Encerramento da ANOVA e guia para o ecossistema — 14/09/2026

O autor declarou a ANOVA concluída e anunciou a regressão linear como próxima
etapa. Não houve nova alteração do código da ANOVA nem início da regressão.
O documento fornecido `codigo-que-fala-guia-estilo.md` foi lido e copiado,
sem modificar seu conteúdo, para `APOIO/documentacao/`, com igualdade de
conteúdo conferida por SHA-256. O índice dessa pasta registra a referência
e distingue suas recomendações das convenções existentes do ecossistema.

Aplicar nas próximas análises: intenção nos comentários, decisões legíveis,
valores intermediários nomeados, constantes com significado, pipe na ordem
do raciocínio e erros que orientam. Preservar `if` nas validações, os contratos
das funções existentes e a separação entre script que executa e arquivo de
funções que apenas define. A aprovação do autor encerra esta etapa; as
limitações dos testes locais continuam documentadas como histórico.

## Tukey curto e decisões com case_when — 14/09/2026

Nova orientação do autor substitui a matriz explícita de p-valores no código
didático por `multcompView::multcompLetters4(modelo, tukey)$fator$Letters`.
A restrição indicada pelo autor foi mantida: níveis de grupo sem hífen.
O script informa como corrigir esses nomes antes da chamada de letras,
em vez de produzir a mensagem interna do pacote. Não há renomeação silenciosa.
Cabeçalhos com espaços na resposta ou no fator usam um modelo auxiliar com
nomes simples, tanto no Tukey quanto nas letras, preservando a base original.

O trecho de texto guarda p-valores brutos e usa `dplyr::case_when()` para a
conclusão da ANOVA, a classe do efeito e as frases de normalidade e variância.
Os valores completos determinam as decisões; a formatação serve ao texto.
A mesma construção substitui o `ifelse()` das células vazias da tabela ANOVA.
Importação, pacotes, preparo, modelo, tabelas e gráfico receberam comentários
e passos coerentes com esse estilo, sem alterar o desenho aprovado das figuras.
Os `if` que validam a entrada ou escolhem objetos de teste foram preservados.

Validação: o teste novo `test_anova_codigo_didatico.R` compara as letras com
a matriz anterior, executa quatro combinações de cabeçalhos simples/com espaços,
confere a orientação para hífen e as decisões em NA e nos limites de p e eta².
Também concluíram as asserções de `test_anova_script_relatorio.R` e
`test_exportacao_enxuta.R` para ZIP, uma/duas ANOVAs, gráficos, nomes CSV e
sincronização script–QMD. Os processos continuam encerrando com código 1 após
os marcadores de sucesso. Sem novo Render Word/HTML, instalação ou commit;
somente o gerador foi alterado, não os projetos já baixados.

## Código da ANOVA explicado por etapas — 14/09/2026

O autor forneceu a organização desejada para preparo, pressupostos e Tukey
e pediu aplicá-la à saída da CatalyseR, sem editar os projetos já baixados.
O modelo `inst/app/templates/anova_um_fator/analise.R` agora traz títulos,
passos numerados e comentários sobre o motivo de cada operação. A mensagem
de resposta não numérica identifica a variável escolhida. A base anterior à
exclusão, a contagem de incompletos e a escolha das cores ficaram explícitas.

Shapiro ganhou `n_residuos` e `shapiro_valido`; o resultado NA está explicado
como teste não calculado. Tukey ganhou `nomes_comparacoes` e
`medias_por_grupo`, com a matriz simétrica e a união das letras pelo nome
comentadas em passos separados. Compartilhar uma letra continua significando
diferença não detectada a 5%, sem afirmar igualdade entre grupos.

Retirado `diagnostico-influencia` (Cook) do script e do QMD da ANOVA básica,
inclusive na exportação acompanhada. Permanecem os dois gráficos de resíduos,
Shapiro-Wilk, Levene e a orientação sobre independência pelo delineamento.
Alternativas à ANOVA clássica registradas para v2 no plano de geração,
sem troca automática de método com base apenas no teste de normalidade.

Verificação: `test_anova_script_relatorio.R` e `test_exportacao_enxuta.R`
concluíram todas as asserções, incluindo ZIP, nomes de origem CSV, uma e duas
ANOVAs, reprodução de coeficientes e Tukey, p-valor de Shapiro, matriz simétrica,
letras por grupo, gráficos e sincronização com o QMD. Ambos os processos
encerraram com código 1 após os marcadores de sucesso, como já observado
neste ambiente. Não houve novo Render de Word/HTML. Projetos do EAPACaderno
preservados; sem reinstalação ou commit.

## Distinção entre salvar a receita e recalcular — 14/09/2026

O autor pediu esclarecer por que adicionar/atualizar precede Recalcular.
Os controles agora explicitam a sequência: “1. Salvar etapa na receita”
(ou “1. Atualizar agrupamento/contingência na receita”) e “2. Recalcular dados”.
A orientação junto aos botões explica que salvar registra as escolhas e
recalcular executa as etapas salvas. As opções ainda não salvas ficam de fora.

Após salvar, a mensagem informa que os dados ainda não mudaram e aponta o
botão 2. Após recalcular, informa que os dados foram atualizados e orienta
conferir Dados preparados. O aviso de base desatualizada e as instruções de
agrupamento/contingência usam a mesma distinção, sem o termo técnico “cache”.
Mudança de textos apenas, com IDs, posição, bloqueios e comportamento mantidos.
Sintaxe e diff conferidos; sem nova inspeção visual no navegador.

## Botão de adicionar/atualizar junto de Recalcular — 13/09/2026

O autor confirmou que o nome do Excel exportado a partir de CSV funciona.
Em Bases Derivadas, pediu mover o botão destacado “Atualizar o agrupamento
existente” para o canto superior direito, junto de Recalcular. Trata-se do
mesmo controle `adicionar_etapa`, cujo texto varia conforme a ação.

Movido para o cabeçalho de Etapas do Preparo, imediatamente antes de Recalcular.
Os dois botões ficam agrupados à direita e podem quebrar linha em telas estreitas.
Preservados ID, rótulos dinâmicos e comportamento. Como o botão saiu do fieldset,
seu bloqueio agora é explícito: desativado sem base ou com preparo finalizado,
ativado durante a edição. Sintaxe e diff conferidos; não houve nova inspeção
visual no navegador. Sem alteração do Projeto R ou dos projetos já exportados.

## Aprovação do acabamento e últimos ajustes da ANOVA — 13/09/2026

O autor confirmou com prints: tabela e paginação juntas à esquerda funcionaram;
o gráfico de barras com média ± DP ficou aprovado. Testou outra aba com outro
conjunto de dados e confirmou os nomes exportados. Considera a ANOVA de um fator
para resposta métrica quase concluída.

Pediu retirar o último gráfico de médias com IC. Removidos os pontos com IC da
tela e o trecho/chunk `fig-grupos` dos próximos Projetos R, tanto para uma ANOVA
quanto para várias. O texto deixa de citar essa figura. Permanecem o gráfico de
barras aprovado, a exploração e os diagnósticos. A função auxiliar antiga de
pontos permanece disponível para compatibilidade, sem ser exibida ou exportada.

Corrigido o Excel bruto de origem CSV: `cultivo.csv` gera `cultivo.xlsx`, com aba
`dados`. A causa era `excel_sheet` herdado de uma planilha anterior ou preenchido
com o padrão `regressao`. O registro de importação deixa esse campo vazio para
CSV/TXT/TSV; o exportador também ignora a aba residual de registros antigos.
A sugestão de nome do projeto segue o CSV. Excel continua seguindo a aba
selecionada (`3.milho` → `milho.xlsx`), e datasets seguem o nome do conjunto.

Verificações: sintaxe; projetos com uma e duas ANOVAs; ausência de `fig-grupos`;
rótulos de média ± DP nas barras; atualização script–QMD e preservação do texto;
ZIP real a partir de CSV, nome e aba do Excel extraído, igualdade dos dados e
referência correta no script. CSV em maiúsculas, TXT, TSV, Excel e pacote também
conferidos. Asserções concluídas; processos R continuam encerrando com saída 1,
portanto não registrar suíte integral com saída zero. Sem novo Render ou mudança
nos projetos já baixados. Ajustes disponíveis após reiniciar a IDE e gerar novo ZIP.

## Retorno sobre alinhamento e rótulos das barras — 13/09/2026

O autor rejeitou a centralização após conferir a tela. Preferência atual:
tabela à esquerda, com a paginação também à esquerda, abaixo dela. Se esse
posicionamento não funcionar, aceita retornar à tabela ocupando a largura do
painel. Esta decisão substitui a centralização registrada na seção seguinte.

Retirada a centralização. Além do CSS, um callback do DT reúne os elementos de
contagem e paginação em um rodapé próprio, também após paginar/filtrar. Isso
evita depender apenas do `dom` e do alinhamento dos contêineres do Bootstrap.
Cabeçalho e corpo começam na margem esquerda. Sintaxe do módulo e diff
conferidos; visualização no navegador ainda indisponível por erro de conexão.
Não declarar o novo alinhamento visualmente homologado.

O autor pediu média ± DP na altura ou pouco acima das barras. Aplicado ao
gráfico da interface e ao trecho `fig-barras` do modelo exportado: texto na
altura da média, ligeiramente acima, à direita da haste. Letras de Tukey
continuam acima do IC, e as legendas distinguem DP e IC. Os valores vêm de cada
grupo; os números repetidos no print eram uma indicação de posição.

Gráficos da interface e do código exportado gerados com a base de `milho3`,
inspecionados em PNG e comparados quanto aos rótulos e às alturas. Imagens em
`../APOIO/saida-sandbox/barras-anova-20260913/`. Corrigida a quebra do subtítulo
da interface para evitar corte. Asserções concluídas; permanece saída 1 no
encerramento do R. Nenhum novo Render Word/HTML foi necessário nesta rodada.
Milho3 preservado; os ajustes entram na IDE reiniciada e nos próximos ZIPs.

## Tabela centralizada e referência milho3 — 13/09/2026

O autor informou que testou `EAPACaderno/milho3` e que o projeto está 98% como
deseja. A pasta contém Word e HTML gerados; o README e o QMD confirmam o fluxo
atual com bases RDS, vínculo script–relatório e quatro chunks analíticos.
Essa versão passa a ser a referência fornecida pelo autor para o acabamento.

O pedido desta rodada substitui a tentativa de paginação à esquerda: centralizar
a tabela de Dados Preparados da Base Compartilhada e manter a paginação à direita.
Aplicado um contêiner específico para centralizar corpo e cabeçalho separado pelo
scrollX do DT. O rodapé mantém a contagem à esquerda e os botões à direita, com
quebra de linha quando faltar largura. As tabelas de outros painéis não recebem
essa centralização. O projeto milho3 e a geração do Projeto R não foram alterados.

Sintaxe do módulo e diff conferidos. A inspeção visual automática ficou bloqueada
pela indisponibilidade do navegador (erro ao carregar a política de cabeçalhos da
conexão). Não declarar esta mudança visualmente homologada. Reiniciar a IDE pelo
código-fonte e recarregar a página para conferir o CSS novo. Sem commit ou instalação.

## Ajustes finais da interface e repetição em milho2 — 13/09/2026

A paginação dos Dados Preparados foi colocada à esquerda, abaixo da contagem
de linhas. No registro de resultados, o seletor de execução agora permanece
montado e tem somente as opções atualizadas: sua recriação reativa era a causa
identificada da oscilação após adicionar uma análise. Um segundo clique com a
mesma configuração seleciona a execução existente e informa que já foi adicionada.

O exportador também elimina cópias exatas de execuções antigas antes de gerar
script, QMD e ZIP. Parâmetros, bases, títulos ou escolhas de conteúdo diferentes
continuam sendo preservados. Não se remove código necessário da ANOVA: os quatro
chunks continuam completos. Derivadas mantêm o preparo comentado, partindo da
base compartilhada. O nome do Excel bruto acompanha a aba: `3.milho` gera
`milho.xlsx`; outra aba, como `biometria`, gera `biometria.xlsx`.

A segunda ANOVA de `EAPACaderno/milho2` era idêntica à primeira. Removida apenas
essa cópia de `R/analise.R` e `relatorios/relatorio.qmd`, com sincronização
conferida. Originais guardados em
`projeto-exemplo/milho2-antes-remover-repeticao-20260913/`, na pasta-mãe.
Os outros projetos, o ZIP baixado e os documentos já renderizados não mudaram.

Verificação visual em navegador com os módulos reais numa sessão de teste:
paginação à esquerda, seleção estável e segundo clique sem criar nova execução.
Asserções de registro e exportação concluídas, incluindo ANOVAs distintas,
preservação das escolhas de conteúdo, RDS, gráficos e vínculo script–QMD.
Permanece a ressalva de encerramento do R com saída 1 neste ambiente; não houve
novo Render Word/HTML. Sem reinstalação, commit ou alteração do `run.R`.
Para testar o próximo ZIP, reiniciar a IDE pelo código-fonte atualizado.
O autor pretende continuar a revisão da ANOVA em outra conversa.

## Garantia do código da ANOVA da manhã — 13/09/2026

O autor reforçou: dividir o antigo chunk `analise` em três ou quatro chunks,
preservando todas as linhas de código do projeto da manhã. Conferido diretamente
com `EAPACaderno/milho_2026-09-13.zip`: os quatro trechos analisar,
analisar-pressupostos, analisar-tukey e preparar-resultados-texto conservam todas
as linhas executáveis na mesma ordem, após substituir os nomes das variáveis.
Para cumprir literalmente essa referência, a escrita vetorizada original das
letras de Tukey foi restaurada; apenas comentários explicativos foram adicionados.
Isso substitui a reescrita com laço relatada anteriormente.

O QMD tem analise-modelo, analise-pressupostos, analise-tukey e analise-texto,
cada um ligado ao trecho correspondente. Tabelas e figuras mantêm chunks próprios.
Somente o preparo do QMD foi substituído pelos RDS, conforme pedido; no gráfico
permanece o acréscimo de média ± DP e espaço para os rótulos. O teste de uma/duas
ANOVAs, gráficos e sincronização concluiu as asserções após a restauração.
Mudanças para o próximo ZIP da IDE; sem reinstalação, commit ou novo Render.

## Comentários no preparo e novo teste pela IDE — 13/09/2026

O autor reafirmou a ANOVA da manhã como referência, com os ajustes pedidos no
gráfico e a retirada de código de preparo do QMD. Pediu comentários junto às
linhas encadeadas e o mesmo cuidado de simplificação para outras transformações.

O gerador agora comenta cada operação da cadeia (seleção, renomeação, tipagem,
cálculo, filtro, duplicatas e operações tidyr). Comentários ficam no script e
são retirados na sincronização dos chunks. O enxugamento também reconhece
remoção de NA, separar/empilhar/alargar e as conversões de data.frame; receitas
com lógica não reconhecida permanecem completas. Derivadas da ANOVA recebem
o mesmo tratamento. Nada mudou nos controles nem nas funções que executam o
preparo da interface.

Verificações concluídas: milho com uma/duas ANOVAs, sincronização e gráficos;
sequência de NA, texto, duplicatas, reescala, cálculo, centralização e filtro;
preparo completo com importação, separar, empilhar, alargar e derivadas. Código
enxuto comparado também ao código anterior ao enxugamento. A renumeração de
row.names pelo drop_na já existia e não representa alteração de valores, ordem
ou tipos das colunas. Processos terminaram com saída 1 após as mensagens finais,
mantendo a ressalva do ambiente; sem novo Render Word/HTML.

Próximo teste do autor: reiniciar a CatalyseR pelo `run.R` do código-fonte,
registrar uma ANOVA e gerar novo ZIP em outra pasta. Não reinstalado o pacote;
projetos já baixados e arquivos do EAPACaderno preservados. Sem commit ou push.

## Revisão das duas saídas de milho — 13/09/2026, fim da tarde

O autor aprovou a lista de abas, os cinco tipos visíveis, a tabela à esquerda,
o nome Adicionar ao Projeto R e as sete saídas inicialmente selecionadas. Conferiu
o Excel com aba única e a compartilhada em XLSX/RDS, incluindo os tipos no RDS.
Observou que a paginação continua à direita; a interface não foi alterada nesta rodada.

Comparados, sem alterar os originais: `../EAPACaderno/milho_2026-09-13.zip`
(09:45, uma ANOVA) e `../EAPACaderno/milho/` (duas execuções, ambas incluídas).
As duas execuções da tarde têm parâmetros iguais. Os valores das bases da manhã
e da tarde são iguais; o cabeçalho da resposta mudou de `Produção (ton)` para
`Producao`. O salto de complexidade veio da escolha de outro modelo quando havia
mais de uma execução; `funcoes.R` permaneceu praticamente igual.

Decisões que substituem as anteriores: retirar `metadados/` do ZIP e a leitura
de `bases.csv`; manter as execuções desmarcadas como código de estudo no script;
usar o nome da aba no Excel bruto (`3.milho` → `milho.xlsx`), guardando o nome
de origem em comentário. O preparo simples sai em uma cadeia de transformações,
sem aliases da interface. Operações com lógica própria conservam a receita
executável original. Moda só é chamada quando a imputação realmente a utiliza.

A ANOVA acompanhada agora usa os mesmos trechos didáticos do modelo isolado,
com nomes de chunks únicos por execução, sem `catalyser_executar()` para a ANOVA.
Preservados o RDS como entrada do relatório, os quatro chunks analíticos,
os ajudantes de apresentação, média ± DP e a atualização script–relatório.

Cópia para revisão: `../projeto-exemplo/milho-simplificado-20260913/milho/`,
com ZIP e comparação na pasta superior. Os dois resultados foram reproduzidos;
gráfico inspecionado. Testes chegaram às asserções finais para uma/duas ANOVAs,
ICs diferentes, seleção de componentes, imputação por moda, edição/sincronização,
projeto geral, ANOVA com gráficos e derivada, e preparo estrutural completo.
Persistem subprocessos R que não encerram normalmente, como registrado antes;
não declarar suíte integral com saída zero nem Word/HTML homologados. Nesta
rodada não houve novo Render completo, reinstalação, commit ou push.

## Retorno do teste com milho — 13/09/2026

O autor confirmou nome “milho”, preservação do nome original do arquivo Excel
e exportação somente da aba utilizada. Novos ajustes foram aplicados: menus de
origem maiores e roláveis, tipos completos no dropdown, tabela de preparo à
esquerda, “Adicionar ao Projeto R” e todo o conteúdo marcado por padrão em novas
execuções de ANOVA (sem desfazer escolhas posteriores).

Decisão que substitui o fluxo de 12/09: o relatório usa os RDS preparados da
compartilhada/derivadas. A receita e sua conferência ficam no script para estudo.
Alterar preparo exige conferir e salvar os RDS; atualizar código não grava dados.
Removida a conferência duplicada. ANOVA dividida em quatro chunks; letras de
Tukey explicadas por comparação; gráfico final com média ± DP ao lado da média
e barras de IC identificadas na legenda. ZIPs de teste e gráfico conferidos em
`../projeto-exemplo/ajustes-20260913/`. Detalhes no plano da geração.

## Planejamento da geração final — 12/09/2026

O autor retomou a saída completa e forneceu `../instrucoes_relatorio_R_01.docx`,
avisando que ainda trará mais orientações. Plano da fase:
[Geração do Projeto R — fechamento da v1](PLANO_GERACAO_PROJETO_R_V1.md).
O menu Preparar Dados permanece aprovado e fora desta revisão.

Decisão da conversa: `R/analise.R` serve para estudo e fornece o código aos
chunks do `relatorio.qmd`. Esse vínculo foi aplicado ao modelo ANOVA, usando
as funções de atualização/conferência já existentes. Um teste com ZIP confirmou
a detecção de código desatualizado, a atualização de uma mudança real no preparo
e a preservação do texto. Execuções desmarcadas também devem permanecer no
acervo: nesses casos o exportador usa o caminho geral com metadados.

O autor aprovou o plano e reforçou a instalação de CatalyseR, EAPADados e pacotes
modernos de R. Aplicados: retirada de `gerar_word.R` e `_quarto.yml` da exportação;
aba única no Excel bruto; `base_compartilhada.rds` e `.xlsx`; Excel das derivadas
utilizadas; instalação orientada, com componentes do tidyverse, lubridate e readxl;
Render separado de Word e HTML. As fotografias das bases não são sobrescritas.

Dois ZIPs de conferência estão em `../projeto-exemplo/fechamento-v1-20260912/`.
Os testes de exportação, atualização script–relatório e preparo completo chegaram
às asserções finais. Porém o R falha ao encerrar (`0xc0000005`, ucrtbase.dll), até
carregando somente cli, nas duas instalações locais. O computador é Intel/x64.
Render DOCX da ANOVA e HTML do projeto múltiplo executaram todos os chunks, mas
pararam nessa falha antes de concluir os documentos. A revisão automática bloqueou
ativar a sessão RStudio `milho`, por pertencer a outro projeto; a tentativa de
abrir janela separada não criou uma sessão nova. Não declarar a geração final
encerrada: falta Render bem-sucedido e inspeção visual das duas saídas. Detalhes no
plano. Sem commit, push ou reinstalação do pacote nesta rodada.

Registro de 11/09/2026, para retomar o trabalho na pasta `D:/Claude/EAPA-Ecossistema/CATALYSER`.

## Onde paramos

Atualização aprovada pelo usuário: `converter_datas()` agora usa `lubridate::parse_date_time(orders = c("dmy", "ymd"))`, com comentários explicativos. Aceita barras/traços/pontos e dia/mês sem zero à esquerda. Mantém Date/POSIXt, vazios e NA; uma verificação curta rejeita caracteres estranhos para não ignorar texto extra nas células. Erros listam os valores não reconhecidos e pedem correção nos dados originais; a notificação existente da IDE acrescenta o nome da coluna. `lubridate` foi movido de Suggests para Imports. Ajuda da IDE, Rd e guia alinhados. Testes isolados passaram para os seis exemplos e inválidos; teste Shiny confirmou aviso com coluna/valor, receita preservada e conversão após correção. Código-fonte atualizado; pacote local não reinstalado nesta rodada. Sem commit ou push.

Datas — decisão final: aceitar dia-mês-ano e ano-mês-dia com barra ou traço (12/09/2026, 12-09-2026, 2026/09/12, 2026-09-12). `converter_datas()` normaliza barras para traços antes das mesmas duas conversões; mantém verificação de datas impossíveis, ausentes e datas já reconhecidas. Dia/mês com dois dígitos e ano com quatro; não interpreta mês-dia-ano. Ajuda da IDE e do pacote alinhadas. Validação isolada passou para as quatro entradas, ausentes, Date/POSIXt, bissexto e entradas inválidas; casos novos adicionados à bateria existente. Sem reinstalação, commit ou push nesta rodada.

Fechamento de 12/09: usuário aprovou os dropdowns e o menu Preparar Dados. Função pública continua `converter_datas()` (plural), com o mesmo comportamento; comentários internos enxugados. Ajuda na IDE e no pacote apresenta 12/09/2026 e 2026-09-12 como a mesma data e explica a exibição ISO do R, com exemplo de conversão de coluna e acesso por `?catalyser::converter_datas`. Preservada a rejeição de datas impossíveis. Documentação atualizada no código-fonte para a próxima instalação; não reinstalado o pacote nesta rodada. Commit e push ficam para a próxima vez, conforme pedido explícito.

Dropdowns de Reestruturar Planilha refinados nas três abas: campos e opções mais legíveis, nomes longos com quebra, destaque de foco/seleção, lista de até 280 px com rolagem própria e abertura para cima quando falta espaço abaixo. Cartões não recortam os seletores. Empilhar explicita a busca e mantém a seleção múltipla aberta. Conferidos no navegador: abertura acima do campo, rolagem até Outro e sua seleção, busca/seleção de sexo em Alargar e peso_g em Separar Colunas. Sem novas operações de preparo ou commit.

Acabamento seguinte de 12/09: os botões Baixar script .R e Baixar dados arrumados (.xlsx) ficam acima das sub-abas Resultado/Original/Código R, no painel direito, nos três modos de reestruturação. Removida a caixa `pre` externa ao `verbatimTextOutput`, deixando uma única área de rolagem no código. Conferência visual em 1828 × 812 e inspeção dos três painéis confirmaram um único bloco de código por modo, sem caixas aninhadas. Processamento e ordem dos campos preservados; sem commit.

Último ajuste visual: o usuário aprovou a ordem dos campos, mas pediu a volta de Resultado/Original/Código R ao topo do painel direito. Posição restaurada, sem alterar os campos. Reduzida a margem sob o título de Reestruturar Planilha para subir Empilhar/Alargar/Separar Colunas. Conferência visual em 1828 × 812, sem transbordamento horizontal. Esta decisão substitui a faixa secundária descrita no registro anterior.

Refinamento seguinte, conforme montagem do usuário: Empilhar e Separar Colunas agora seguem 1. escolher coluna(s), 2. definir separação, 3. quantidade de colunas (delimitador), 4. nomes, aplicar/desfazer. Os passos 1–2 ficam à esquerda e 3–4 no centro. Resultado, Original e Código R passaram para uma faixa secundária recuada abaixo das abas Empilhar/Alargar/Separar Colunas; usam navegação nativa do Shiny e preservam os controles enquanto alternam a consulta à direita. Renderização dos três modos e sintaxe conferidas; navegação/visual em 1828 × 812, incluindo tabela Original e Código R, sem transbordamento horizontal da página. Nenhuma mudança no processamento ou nas datas; não foi repetida a bateria completa para este ajuste visual. Alterações desta rodada no código-fonte da IDE, sem commit.

Decisão mais recente de 12/09 — substitui a organização de datas descrita abaixo: o usuário pediu **somente a chamada `converter_datas()` nos arquivos exportados**, inclusive no .R avulso. A função pública comentada agora mora em `R/converter_datas.R`, com ajuda `?catalyser::converter_datas`; a IDE usa essa mesma definição. Downloads e projetos chamam `catalyser::converter_datas(coluna)`, sem incorporar a função em `R/funcoes.R` nem no preparo. Receitas anteriores com `converter_data()` são adaptadas na geração da sequência completa. A instalação local em `C:/R/R-4.6.1/library` foi atualizada e a chamada pública conferida. Reiniciar a sessão do RStudio antes de usar a versão atualizada.

Reabertura pontual do acabamento, a pedido do usuário: Reestruturar Planilha ganhou o título azul e uma descrição curta. Empilhar e Separar Colunas têm escolhas/nomes à esquerda, parâmetros/aplicar no centro e resultado/incorporar/downloads à direita. A ajuda longa ficou recolhida em “Como funciona”; prévias mostram cinco linhas por página, com seletor para ampliar. Nenhuma operação nova. Conferência visual em 1828 × 812 e separação da planilha de treino no navegador. `test_interface_saidas_preparo.R` concluiu empilhar, alargar, separar, downloads e derivadas; `test_preparo_csv_datas.R` concluiu Excel e os dois CSV, projetos geral/ANOVA e chamada da função pública sem definição embutida. Os processos continuaram terminando com saída 1 após as mensagens de conclusão; não declarar a suíte integralmente aprovada. A instalação padrão apresentou o mesmo problema ao encerrar o subprocesso de preparação: a cópia de teste foi completada, sua função pública validada e instalada localmente como binário. Não houve commit nem nova renderização de Word/HTML.

Atualização de 12/09: o usuário encerrou a revisão da interface de preparo para a v1 e avisou que pedirá o commit depois. Não fazer commit antecipadamente. O ajuste seguinte ficou restrito ao código de datas: a versão comentada enviada em `Downloads/converter_data.R` passou a ser a definição canônica em `inst/app/templates/converter_data.R`, usada pela IDE e copiada para `R/funcoes.R` nos projetos exportados (geral e ANOVA). O preparo desses projetos contém apenas as chamadas; o script avulso mantém a definição para continuar autossuficiente. Nenhum menu novo. Os arquivos originais de Downloads foram preservados.

Validação desse ajuste: equivalência com a função enviada (Date, POSIXt, formatos brasileiro e ISO, ausentes, vetor vazio e erros), remoção de definições repetidas preservando chamadas e marcadores, e sintaxe passaram em R isolado, saída 0. `test_preparo_csv_datas.R` concluiu todas as asserções de Excel, dois separadores CSV, compartilhada, derivada e projetos geral/ANOVA, incluindo a presença única da função comentada em `R/funcoes.R` e sua ausência no preparo; o processo terminou com saída 1, sem erro de asserção exibido. Manter a ressalva do ambiente, sem declarar a suíte integralmente aprovada. Não foi feita nova renderização Word/HTML.

O usuário conferiu os prints e considerou a interface de preparo bem refinada. Confirmou que o botão de incorporar a reestruturação ficou cinza, que a descrição estrutural apareceu na trilha e que o botão de adicionar uma etapa voltou ao cinza depois de salvar a renomeação. Pretende fazer outra revisão visual no dia seguinte. Isso não foi um pedido de lembrete ou de execução agendada.

Depois dessa aprovação, pediu para verificar se todo o código do preparo chega à Comunicação de Resultados e ao Projeto R. A auditoria encontrou uma lacuna no exportador geral, corrigiu-a e incorporou a correção à pasta principal da CatalyseR.

## Decisões de interface que devem ser preservadas

- **Estados dos botões:** azul quando há ajuste para adicionar; cinza e desativado quando não há pendências ou quando a mudança já foi incorporada. O botão de incorporar a reestruturação usa cinza-escuro. A confirmação consome a prévia e impede repetir a mesma mudança, mas preserva os downloads.
- **Finalizar/reabrir:** os controles de edição continuam visíveis depois de finalizar, desativados até reabrir o preparo.
- **Descrições das etapas:** manter o identificador técnico quando presente e acrescentar a operação concreta. O usuário preferiu detalhar, em vez de retirar o nome técnico. Exemplo: `reescalar — Reescalar peso_g → peso_kg (÷ 1.000)`.
- **Reestruturação:** mostrar a origem, os destinos, o destino da coluna original e o separador no mesmo item. Exemplo: `Separar amostra → local_amostra e estacao (manteve a coluna amostra; delimitador '_')`. Quando a original for excluída, escrever `removeu a coluna amostra`.
- **Uma faixa por etapa:** fundo discreto e faixas finas. Toda a descrição pertence ao mesmo item; a linha pode quebrar naturalmente se a janela for estreita. Não cortar informações para forçar uma única linha física.
- **Histórico estrutural:** aparece antes dos tratamentos, no bloco “Reestruturação incorporada — antes dos tratamentos”, com opção de consultar o código.
- **Download da compartilhada:** botão no cabeçalho, acessível nas três abas de consulta.
- **Ordem dos grupos da compartilhada:** Limpeza → Cálculos e transformações → Variáveis e categorias.
- **Ordem dos grupos das derivadas:** Limpeza → Recortes e resumos → Cálculos e transformações.

Clareza da interface e correção das saídas devem ser avaliadas juntas. Os cálculos já haviam sido considerados corretos pelo usuário; o foco desta rodada foi tornar as ações, estados e resultados mais fáceis de acompanhar. Sugestões adicionais de aparência devem ser apresentadas ao usuário antes de aplicá-las, conforme sua preferência expressa no histórico recuperado.

## Aprendizado central sobre a exportação

Conferir apenas a base final não basta para demonstrar que o preparo foi preservado. É preciso extrair o Projeto R, executar o código desde a planilha de entrada e comparar o resultado com os dados usados na IDE.

A receita deve percorrer: escolhas da importação → reestruturações → tratamentos compartilhados → preparo da derivada utilizada. Cada derivada nasce da compartilhada, sem reaplicar a trilha compartilhada nem aproveitar o resultado de outra derivada. Etapas desativadas não devem executar.

A ANOVA já recebia as escolhas de importação. O exportador geral ainda ignorava essas escolhas e carregava uma fotografia para as mudanças estruturais, mantendo seu código apenas como comentário. A correção fez ambos usarem o mesmo gerador da importação e levou a sequência estrutural executável também ao caminho geral.

O código é preservado em `R/analise.R` e nos chunks do documento-fonte do relatório. Isso não significa imprimir todo o código no Word: a apresentação continua seguindo as opções do modelo. No exportador geral, os chunks são preenchidos a partir do script; a ANOVA usa seu modelo autossuficiente.

Antes de gerar o projeto, a IDE agora confere também no caminho geral se o preparo reproduz a compartilhada e as derivadas das análises incluídas. Divergências interrompem a exportação. Tratamentos sem gerador de código provocam erro, em vez de serem omitidos. Sequências estruturais atuais com erro não são substituídas silenciosamente por uma fotografia. A planilha Excel original também é preservada no caminho geral.

## Evidências e limites

Foram testados quatro ZIPs: caminho geral e ANOVA, cada um com manutenção e remoção da coluna original. O script e o código de preparo do relatório foram executados a partir dos arquivos extraídos. Foram incluídos seleção, recodificação, tipos e filtros na importação; renomeação; separar → empilhar → alargar; reescala; renomeação na compartilhada; filtro e renomeação na derivada. Os resultados coincidiram com a IDE: 7 linhas na compartilhada e 5 na derivada. Esses projetos não dependiam de fotografia estrutural. Também foi comparado o arquivo Excel original com o incluído em cada ZIP.

Os testes verificaram que uma etapa estrutural perdida, uma derivada divergente ou um tratamento desconhecido impedem a exportação. O teste novo foi incluído na suíte. As verificações existentes de preparo, bases, comunicação e exportador geral também concluíram as asserções descritas no registro da auditoria.

Limites a conservar na próxima retomada:

- Não houve nova renderização final de Word/HTML nesta conversa. Executar os chunks confirma o código, mas não equivale a revisar os documentos renderizados.
- Registros legados sem sequência estrutural executável ainda usam a base salva e o código original comentado; essa limitação fica explícita no projeto. Não generalizar o resultado dos testes atuais para todo registro antigo.
- O R deste ambiente continua retornando status 1 mesmo após os marcadores de conclusão. Não declarar a suíte inteira aprovada com base apenas nesses marcadores.
- `test_anova_exportacao.R` falha numa exigência de frase literal, “a análise passo a passo”. A mesma falha foi reproduzida antes da correção; o teste não foi alterado nesta rodada.

## Próxima retomada

Reabrir a IDE pelo `run.R` desta pasta para carregar as alterações. Receber a revisão visual do usuário e ajustar somente os pontos identificados ou combinados. A renderização e a conferência final de Word/HTML continuam sendo uma verificação distinta, ainda pendente nesta rodada.

## Arquivos de referência

### Complemento de 12/09/2026 — fechamento enxuto da v1

Foram corrigidos a exposição dos separadores de CSV, a leitura equivalente no R exportado e o tratamento básico de datas (Excel, DD/MM/AAAA e AAAA-MM-DD, com rejeição de datas inválidas). O código de preenchimento de ausentes preserva a classe das datas. Nenhum menu ou ação de limpeza foi acrescentado. O guia foi alinhado ao comportamento real: não há limpeza automática com janitor.

Registro desta rodada: [ajustes e evidências](docs/testes/fases-3/AJUSTES_CSV_DATAS_V1_2026-09-12.md). Próxima ação: [seis testes curtos do usuário](docs/testes/fases-3/ROTEIRO_FINAL_PREPARO_V1_2026-09-12.md), anotando OK ou dificuldade com print. Os testes automatizados concluíram suas asserções, mas persiste encerramento anormal dos processos R; não declarar a suíte integralmente aprovada. A renderização final de Word/HTML permanece distinta e pendente.

A figura fornecida pelo usuário foi encontrada em `inst/app/www/percurso_catalyser.png`. Não foi alterada nem inserida na interface nesta rodada. Para corresponder aos menus, o cartão “Reestruturar” deve descrever empilhar, alargar e separar colunas; tipos, datas e filtros pertencem às ações de preparo.

### Registros anteriores

Refinamento aprovado da Base Compartilhada (12/09): a ordem definitiva das sub-abas é **Etapas do Preparo → Dados Preparados → Códigos R**, abrindo em Etapas. Para Limpeza e Cálculos, a coluna esquerda reúne Grupo de ações, Ação e seleção de colunas ou nome da nova variável; as opções específicas permanecem no centro, com a trilha à direita. IDs e comportamento preservados. Sintaxe dos dois módulos validada e disposição conferida no navegador em 1828 × 812. Esta ordem substitui a registrada abaixo.

Base Compartilhada alinhada ao acabamento das derivadas (12/09): três sub-abas — Dados preparados, Etapas do Preparo e Código R. Grupo de ações, ação, parâmetros e organização de variáveis ficam somente em Etapas do Preparo, em três áreas ao lado da trilha. Dados e código usam a largura inteira; downloads continuam no cabeçalho. Os controles existentes de mod_tratar foram distribuídos em partes, mantendo IDs e comportamento. Verificação no navegador (1828 × 812): inclusão de tratamento de ausentes, renomeação com prévia e incorporação, tabela atualizada e código contendo as duas etapas. Sem novos tratamentos nem mudança da exportação; manter as ressalvas já registradas sobre o ambiente R. Usuário aprovou a organização das derivadas para a v1.

Acabamento visual após F01 aprovado (12/09): usuário confirmou a funcionalidade e pediu apenas melhor aproveitamento da tela das derivadas. Cabeçalho compacto; escolha da ação, parâmetros e trilha distribuídos pela largura; Recalcular junto da trilha; controles finais em duas colunas. Conferência no navegador em 1828 × 812, tamanho do print: inclusão, recálculo e finalização mantidos; botões principais visíveis. Listas longas de etapas têm rolagem própria. A ajuda passou a “Como escrever as datas?”, com exemplos 25/09/2026 e 2026-09-25. Não expor converter_data() nessa ajuda; a conversão e o código exportado permanecem como já validados. Não acrescentar funcionalidades nem repetir a bateria completa para esta mudança visual.

Complemento após o Word `teste_transformações5.docx`: [feedback 5 e segunda bateria de três conferências](docs/testes/fases-3/REVISAO_FEEDBACK_5_2026-09-12.md). CSV e orientação de data inválida aprovados pelo usuário. Aplicados: aba de código da importação, remoção do atalho antigo de exportação, tabela com dimensões em português, ajuda de datas, estado concluído verde, menus fixos na rolagem, nomes e posição dos downloads, explicação de derivadas. Sem novo tratamento. Próxima ação: receber F01–F03; não presumir execução no RStudio a partir de aprovação apenas da facilidade de localizar os arquivos. Continua a ressalva do encerramento nativo do R.

- [Auditoria do preparo no relatório e no Projeto R](docs/testes/fases-3/VERIFICACAO_PREPARO_EXPORTACAO_2026-09-11.md).
- [Revisão dos feedbacks e do acabamento visual](docs/testes/fases-3/REVISAO_FEEDBACK_2_2026-09-11.md).
- [Roteiro de revisão da interface](docs/testes/fases-3/ROTEIRO_FEEDBACK_2_2026-09-11.md).
- [Teste completo de preparo e comunicação](inst/app/tests/test_preparo_comunicacao_completo.R).
- Implementação da auditoria: `inst/app/modules/exportacao_comunicacao.R`, `inst/app/modules/registro_bases.R` e inclusão do teste em `inst/app/tests/run_tests.R`.
