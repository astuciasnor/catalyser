# Comunicação de Resultados — especificação atual

## Da análise à comunicação — decisão consolidada em 18/09/2026

Nos novos Projetos R, os textos automáticos devem aparecer depois dos objetos
estatísticos, das tabelas e dos gráficos. Eles funcionam como uma retomada das
muitas saídas que o R pode produzir e ajudam o pesquisador a reconhecer o que
será comunicado. A dependência dos objetos anteriores determina a posição
mínima; vir depois das figuras é uma escolha narrativa para encerrar primeiro
o percurso analítico e só então preparar sua comunicação.

Os textos continuam sendo objetos simples, visíveis no console com `print()` e
consumidos pelos QMDs. Como a execução do script nos relatórios ocorre em chunk
oculto, as impressões não entram nos documentos. O R prepara uma síntese
estatística; introdução, discussão e conclusão científica permanecem nos QMDs,
sob revisão do pesquisador. Anotações próprias de figuras, como a equação da
reta, ficam junto do código do gráfico.

## Propósito didático confirmado pelo autor — 18/09/2026

O Projeto R exportado deve convidar o pesquisador a aprender programação.
Cada arquivo precisa ter um papel reconhecível, e o código deve permitir
acompanhar como os dados se tornam resultados. Quem começa pelos cliques
deve poder abrir o script, reconhecer suas escolhas, examinar os objetos e
entender o que a CatalyseR executou. A legibilidade faz parte da entrega.

O autor aprovou a clareza do exemplo `EAPACadernos/linear-morfometria-barbo` após
revisar seus comentários e a disposição do código. Considerou o material
compreensível para si e adequado para um professor com conhecimentos
intermediários de R explicar aos alunos. Essa aprovação orienta os próximos
roteiros, que devem ser apresentados ao autor para revisão didática depois
da implementação e da validação de funcionamento.

Manter cálculos e etapas visíveis, usar funções de pacotes com seu papel
explicado e distribuir as linhas conforme a complexidade da chamada.
Comentários devem esclarecer escolhas e operações menos familiares.
O exemplo aprovado orienta a evolução do gerador; sua aprovação didática
não significa que a migração do exportador para dois QMDs já foi implementada.

## Refinamento aprovado — 18/09/2026: legibilidade e saídas enxutas

O autor reafirmou a estrutura com `_quarto.yml`, `here`, script analítico e
dois QMDs. O HTML documenta o percurso; o Word apresenta o artigo, com os
resultados essenciais e uma síntese dos diagnósticos relevantes. Explicações
de como explorar e verificar pressupostos ficam no HTML. Dicas de redação
continuam no QMD do artigo como comentários, sem aparecer no Word.

No projeto exportado, `R/analise.R` deve ser legível por quem conhece o básico
de R: cabeçalho com pergunta e mapa dos objetos, seções numeradas reconhecidas
pelo RStudio, operações em etapas e comentários sobre o motivo das escolhas.
Cada gráfico recebe um nome. Evitar expressões aninhadas quando um objeto
intermediário torna o percurso mais claro; não criar uma infraestrutura de
funções genéricas para esconder os cálculos. A CatalyseR continua sendo a
origem canônica das análises; o script é a referência executável do projeto
que o pesquisador passa a editar.

O exemplo `EAPACadernos/linear-morfometria-barbo` foi refinado nessa direção. Seus
QMDs executam o script e consomem objetos da memória da sessão de renderização;
os CSVs e PNGs em `saida/` são cópias para compartilhamento, não entradas dos
QMDs. A sessão de Render não depende do Environment aberto no RStudio.

Nesse exemplo, `dados/processados/` passa a conter somente `base_regressao.csv`,
com os identificadores junto das duas medidas. A seleção das dez medidas
numéricas serve à exploração em memória, sem separar fisicamente os IDs nem
persistir um RDS que ninguém lê. Isso não altera a política de RDS do exportador
atual: a migração precisa respeitar a adoção explícita das bases preparadas.

APA é o único CSL fornecido no exemplo, conforme a edição do autor. No novo
exportador, incluir apenas o CSL escolhido; outros podem ser acrescentados
pelo pesquisador. CSL governa citações/referências; o modelo Word governa o
estilo do documento. `sessionInfo()` e a versão do Quarto documentam o ambiente,
mas não fixam nem reconstroem versões sozinhos.

**Aplicado ao gerador atual:** cabeçalho visual da regressão, mapa de objetos,
comentários didáticos e nomes para os gráficos de resíduos, Q-Q, ordem e Cook.
Os marcadores de trechos e a apresentação do exportador existente foram
preservados. **Ainda pendente:** a migração estrutural para os dois QMDs.

## Direção aprovada — 16/09/2026: um script e dois documentos

**Decisão de arquitetura; migração do exportador ainda pendente.** O autor
aprovou separar o caderno HTML e o artigo Word em dois QMDs, ambos alimentados
pela mesma análise em R. Esta direção substitui, para a próxima implementação,
o documento único com seções condicionais por formato e a cópia sincronizada
do código analítico. As revisões abaixo continuam descrevendo o exportador
existente até que a migração seja implementada e validada.

- `R/analise.R`: código comentado, na ordem do trabalho, com preparo,
  exploração, ajuste e diagnósticos. É onde o aluno acompanha e altera os
  cálculos. `R/funcoes.R` reúne apenas os auxiliares necessários.
- `relatorios/relatorio_completo.qmd`: percurso explicado, exploração,
  diagnósticos e resultados, para gerar o HTML.
- `relatorios/relatorio_artigo.qmd`: Introdução, Material e métodos,
  Resultados, Discussão, Conclusão e Referências, para gerar o Word.
- Os dois QMDs executam o mesmo script e usam seus objetos em pequenos chunks
  de apresentação e no texto dinâmico. Não mantêm uma segunda implementação
  dos cálculos. Cada Render deve funcionar numa sessão nova, sem depender de
  objetos criados manualmente no console.
- `dados/brutos/` preserva as entradas; `dados/processados/` guarda as bases
  tratadas; `imagens/` recebe fotos e esquemas fornecidos pelo pesquisador.
  `saida/tabelas/`, `saida/figuras/` e `saida/relatorios/` recebem os produtos
  regeneráveis. O projeto deve criar automaticamente as pastas necessárias.
- O README explica a execução, a origem dos dados e como atualizar o preparo;
  o projeto registra as versões do R, dos pacotes e do Quarto. Sem acrescentar
  infraestrutura ao projeto didático.

**Cuidado na migração:** executar o script no Render não pode sobrescrever
silenciosamente uma base preparada que o pesquisador decidiu conservar. A
implementação deve explicitar a etapa de adoção de mudanças no preparo,
preservando a decisão de 13/09 sobre os RDS. Também deve preservar execuções
selecionadas, textos autorais, rótulos, referências e diagnósticos próprios de
cada análise. A regra para execuções não selecionadas precisa ficar clara no
script: não devem entrar nos documentos por efeito colateral do `source()`.

O fundamento é a organização em compêndios de pesquisa de Marwick, Boettiger
e Mullen (2018), DOI 10.1080/00031305.2017.1375986. A separação em dois QMDs é
uma adaptação didática do EAPA, não uma prescrição do artigo. O livro registra
essa filosofia nos capítulos de organização de projetos e comunicação.

Validação da futura migração: abrir um projeto exportado em sessão nova,
gerar ambos os documentos, conferir que exibem os mesmos resultados para a
mesma base e configuração, e verificar a atualização após uma alteração
deliberada no script. A regressão com `morfometria_barbo` será o primeiro caso;
a ANOVA e os projetos com várias análises também precisam ser preservados.

**Revisão de 13/09, fim da tarde:** o projeto não exporta `metadados/` nem lê
`bases.csv`. Receitas e execuções (inclusive desmarcadas, para estudo) ficam no
script; o QMD contém somente as execuções selecionadas. O Excel bruto recebe
o nome da aba. Transformações simples são encadeadas com `|>`, sem objetos da
interface. A ANOVA usa os mesmos trechos explícitos do modelo isolado também
quando acompanhada por outras execuções; apenas os nomes dos chunks mudam.
Esta revisão prevalece sobre as referências históricas a metadados e ao replay
da ANOVA abaixo. Comparação e validação em `APRENDIZADOS_PREPARO_E_EXPORTACAO_2026-09-11.md`.

**Decisão de 13/09/2026:** o QMD passa a ler as bases preparadas em RDS,
incluindo cada derivada utilizada. Importação, preparo e conferência ficam no
script. Esta decisão substitui a reconstrução do preparo no Render descrita
abaixo. O pesquisador salva os RDS explicitamente se decidir adotar mudanças
no preparo. ANOVA isolada tem quatro chunks analíticos e gráfico final com
rótulos de média ± DP, preservando IC nas barras. Conteúdo editorial completo
vem selecionado por padrão para novas execuções ANOVA. Ver detalhes e validação
em `PLANO_GERACAO_PROJETO_R_V1.md`.

## Atualização aprovada — 12/09/2026

O fechamento da geração do Projeto R segue
`PLANO_GERACAO_PROJETO_R_V1.md`. Esta decisão prevalece sobre descrições
anteriores dos modelos: `R/analise.R` é a fonte comentada do código tanto na
ANOVA isolada quanto no projeto com várias análises. O `.qmd` conserva texto,
legendas e chunks preenchidos, ligados por `# fonte:`. `atualizar_codigo()`
copia o código e `conferir_codigo()` detecta divergências no Render.

O ZIP exporta somente a aba utilizada do Excel bruto; a base compartilhada em
RDS e Excel; e um Excel de cada derivada utilizada. São fotografias da IDE,
preservadas durante o Render, que reconstrói o preparo desde a entrada.
Não exporta `_quarto.yml` nem `R/gerar_word.R`: Word e HTML são escolhidos
separadamente no RStudio. O trecho `instalar`, executado manualmente uma vez,
orienta instalar CatalyseR, EAPADados e os pacotes de leitura, preparo e análise.
O relatório nunca instala pacotes durante o Render.

A ANOVA dedicada é usada somente quando há uma execução no projeto. Havendo
outras execuções, mesmo desmarcadas, o modelo geral mantém seu acervo nos
metadados. O menu Preparar Dados e o catálogo de análises permanecem no escopo
aprovado da v1. A validação final de Word/HTML ainda depende de resolver a falha
local do processo R ao encerrar; evidências e pendências estão no plano.

**Verificado em:** CatalyseR 0.1.5, commit `6aa407a`, 27/07/2026
**Fontes de verdade:** `mod_comunicacao.R`, `registro_comunicacao.R`,
`exportacao_comunicacao.R` e testes

A proposta e a implementação por fases foram preservadas em
`docs/historico/MODULO_COMUNICACAO_RESULTADOS_FASES_3.md`.

## Finalidade

Separar duas decisões:

1. quais execuções científicas o Projeto R deve preservar;
2. quais componentes editoriais devem aparecer no Word.

O registro acontece dentro de cada análise. A Comunicação organiza esse acervo;
ela não recalcula nem apaga resultados analíticos.

## Interface atual

### 1. Esboço do documento

Mostra a estrutura prevista e as seções globais:

- Introdução;
- Métodos gerais;
- Discussão;
- Conclusão.

O manifesto editorial técnico fica recolhido em um elemento expansível.

### 2. Execuções registradas

Permite:

- escolher se a execução entra no Word;
- escolher componentes disponíveis;
- ordenar resultados;
- conferir base e estado de dependência.

Desmarcar uma execução do Word não a remove do Projeto R.

### 3. Bases do projeto

Lista a Base Compartilhada e as Bases Derivadas vinculadas às execuções,
preservando a proveniência.

### 4. Saída planejada

Mostra a conferência antes da exportação e oferece um download só: o
Projeto R `.zip`. (Até a Fase D havia também o Word `.docx`, renderizado pela
IDE; saiu de propósito — ver "Fase D" abaixo.)

## Estado editorial

`estado_editorial_rv` guarda somente:

- ordem dos IDs;
- inclusão no Word;
- componentes selecionados;
- textos globais.

Resultados, modelos e dados continuam fora desse estado.

## Exportação

Antes de exportar, o sistema verifica se bases e execuções ainda correspondem
às revisões usadas. Uma dependência desatualizada bloqueia a geração.

O relatório do Projeto R recebe apenas o conteúdo editorial escolhido (os
campos internos ainda se chamam `incluir_word`/`saidas_word`). O Projeto R
recebe todas as execuções registradas, bases, receitas, script e metadados.

A IDE não precisa do Quarto: o Word e o caderno HTML nascem no RStudio do
pesquisador, no Render.

## Código humano

ANOVA e Gráfico de Linhas são os pilotos atuais. Desde a versão 0.1.5:

- script numerado e QMD compartilham `exportacao_codigo_estudo()`;
- o método científico aparece em código R legível;
- a configuração completa é lida de
  `metadados/registro_execucoes.rds`;
- a integração editorial fica separada do código principal de estudo.

Não generalizar essa humanização para outras análises sem testes equivalentes.

## Árvore do Projeto R exportado (Fase A, set/2026)

Desde a Fase A, o Projeto R exportado segue a árvore do **projeto-modelo** do
ecossistema (`D:\Claude\EAPA-Ecossistema\EAPACadernos/`), para que quem sai da
IDE reconheça o caminho a pé. (A árvore abaixo é a da Fase A; a Fase C, mais
adiante, removeu `R/` e levou o preparo para dentro do `.qmd`.)

```
projeto_<nome>/
├── projeto_analise.Rproj
├── README.md                      na voz do projeto-modelo
├── dados/
│   ├── brutos/<planilha>.xlsx     a planilha importada na IDE
│   └── processados/               dados_analise.rds (fotografia) + base_compartilhada.xlsx
├── R/
│   ├── 01_importar.R              planilha -> dados_brutos
│   ├── 02_tratar.R                chama o 01; estrutural + trilha + conferência -> dados_analise
│   └── 04_analisar_NN_<tipo>.R    um por execução (três partes)
├── relatorios/
│   ├── relatorio.qmd              caminhos com here(); chama o 02
│   └── custom-reference.docx
└── metadados/                     inalterado
```

Decisões: caminhos com `here()` (o `.Rproj` é a raiz); scripts encadeados por
`source()` em vez de arquivos intermediários, para o *Render* funcionar sozinho;
sem `03_explorar.R`, `05_graficos.R`, `rodar_tudo.R` nem `resultados/` (o Render
já refaz tudo; tabelas e figuras nascem no Word). O `here` entrou em `Suggests`
do DESCRIPTION.

## Relatório orgânico (Fase B, set/2026)

Decisão do autor: no projeto exportado, **a análise mora dentro do `relatorio.qmd`**,
como ele sempre usou o Quarto. (Diverge do projeto-modelo, onde scripts gravam em
`resultados/` e o relatório só lê. Divergência deliberada: o modelo ensina a
separação; o projeto exportado mostra a análise inteira num documento.)

Cada análise aparece em três chunks, todos `echo: false`:

- `<raiz>-base` — o salto de `dados_analise` até a base derivada (`dados_da_analise`);
- `<raiz>-analise` — o código passo a passo (`exportacao_codigo_estudo()`). Para os
  tipos com código validado (`exportacao_tipos_com_codigo_vivo`: ANOVA de um e dois
  fatores, gráfico de linhas) roda com `output: false`; para os demais fica
  `eval: false`, só leitura, até ganharem o mesmo tratamento;
- `<raiz>-resultado` — `catalyser_executar()` com `tipo`, `titulo` e `parametros`
  **escritos por extenso** (`exportacao_lista_r()`), sem ler
  `metadados/registro_execucoes.rds`. O objeto leva o nome da análise
  (`anova_profundidade_m`, `linhas_captura`), e os chunks de componente chamam
  `catalyser_mostrar(anova_profundidade_m[["tabela"]])`, cada um com uma linha de
  comentário dizendo o que mostra (`[[ ]]` e não `$`, para o R não completar
  `grafico` como `grafico_combinacoes`).

O `.qmd` não depende da pasta `metadados/` para as análises; ela fica para a
CatalyseR reabrir o projeto.

## Fase C (set/2026): planilha + qmd + metadados

O preparo também foi para dentro do relatório, e a pasta `R/` deixou de existir
no projeto exportado. A árvore final é a do EAPACaderno:

```
projeto_<nome>/
├── projeto_analise.Rproj
├── README.md
├── dados/brutos/<planilha>.xlsx          entrada
├── dados/processados/                    dados_analise.rds (fotografia) + base_compartilhada.xlsx
├── imagens/                              vazia, para fotos e esquemas
├── relatorios/relatorio.qmd              O PROJETO
├── relatorios/custom-reference.docx
└── metadados/                            memória da exportação (IDE)
```

No `relatorio.qmd`, a seção "Preparação dos dados" tem dois chunks gerados por
`exportacao_chunk_importar()` e `exportacao_chunk_tratar()` (ambos
`output: false`): `importar` lê a planilha com `readxl` e deixa `dados_brutos`;
`tratar` aplica `exportacao_bloco_estrutural()` + `exportacao_bloco_trilha()`
e confere com `catalyser_conferir_base()`. É a Seção 0 do relatório, a Trilha
de Preparo virando texto e código. Cada análise abre com **Pergunta:**
(`exportacao_pergunta()`), a base e a execução.

Saíram do gerador: `exportacao_codigo_importar/tratar`, `exportacao_arquivo(s)_execucao`,
`exportacao_codigo_execucao`, `exportacao_bloco_pacote_catalyser`. Os testes
rodam o relatório inteiro fora do Quarto com `knitr::purl()` + `sys.source()`,
como um aluno que executa os chunks um a um, e conferem a mensagem "idêntica à
fotografia".

## Fase D (set/2026): o par script + relatório, e sem Word na IDE

Duas decisões do autor:

1. **A CatalyseR não gera mais o Word.** Só o Projeto R. O Word e o caderno
   HTML nascem no RStudio, quando o pesquisador clica em Render — é aí que ele
   vê de onde cada tabela e cada frase saem. Saíram `exportacao_renderizar_word()`,
   o botão "Baixar Relatório Word" e o rádio de formato; a seleção editorial
   continua, com o rótulo "no relatório".
2. **O projeto exportado é o par do EAPACaderno.** O código mora em
   `R/analise.R`, comentado passo a passo, em trechos `## ---- nome ----`; o
   `relatorios/relatorio.qmd` recebe só as linhas de código, e a primeira linha
   de cada chunk diz de quais trechos ele vem (`# fonte: ...`). A ligação é o
   `R/funcoes.R` (template `inst/app/templates/funcoes.R`, o mesmo código da
   seção 5 do `funcoes.R` do EAPACaderno): `atualizar_codigo()` copia o código
   do script para os chunks; `conferir_codigo()` roda no chunk `codigo-do-script`
   e para o Render se o relatório estiver atrasado. O exportador gera o script,
   gera o `.qmd` com as cascas e chama o **mesmo** `atualizar_codigo()`.

Árvore:

```
projeto_<nome>/
├── projeto_analise.Rproj
├── README.md                       na voz do EAPACaderno, com "Onde o código mora"
├── dados/brutos, dados/processados
├── R/analise.R                     O CÓDIGO, comentado (trechos ## ---- nome ----)
├── R/funcoes.R                     atualizar_codigo(), conferir_codigo()
├── imagens/
├── relatorios/relatorio.qmd        texto + código limpo (chunks com # fonte:)
├── relatorios/custom-reference.docx
├── relatorios/ocean.scss           o caderno HTML, igual ao EAPACaderno
└── metadados/
```

Trechos do script, na ordem: `instalar`, `pacotes`, `importar`, `tratar`,
`bases-projeto` e, por execução incluída, `<raiz>-base`, `<raiz>-analise`,
`<raiz>-resultado` e `<raiz>-<componente>`. Todo comentário que explica R
(o "# 1. Declarar as variáveis..." da ANOVA, o "O QUE CONFERIR" do importar)
mora no script. No `.qmd`, os três trechos de cada análise viram **um chunk**
`<raiz>` (`output: false`) para os tipos com código validado; para os demais,
`<raiz>` reúne base + resultado e um `<raiz>-analise` à parte fica `eval: false`.
Os chunks de componente (`### Título` + `results: asis`) ficam separados.

A camada didática do `.qmd` são comentários HTML (`<!-- -->`, que não saem em
nenhuma saída) sobre **programação literária**, não sobre R: o que cada opção
`#|` faz, por que um chunk de trabalho não aparece no Word, como uma cerca
`when-format="html"` faz um trecho existir só no caderno. YAML igual ao do
EAPACaderno (docx + html, `echo: false` global e `echo: true` no HTML, código
dobrado, índice à esquerda). Seções: Introdução, Material e métodos (Os dados,
Preparo, Análise dos dados), Resultados, Discussão, Conclusão — as globais
vazias viram um lembrete em comentário.

Os testes conferem o par: `R/analise.R` com marcadores, chunks do `.qmd` sem
comentário além do `# fonte:` (exceto os dois de manutenção),
`conferir_codigo()` acusando um script editado e `atualizar_codigo()` trazendo
a mudança sem os comentários, e o `purl + sys.source` rodando o relatório
inteiro. Nenhum teste depende mais do Quarto.

## Arquivos principais

- `inst/app/modules/mod_comunicacao.R`;
- `inst/app/modules/registro_comunicacao.R`;
- `inst/app/modules/exportacao_comunicacao.R`;
- `inst/app/templates/funcoes.R`, `ocean.scss`, `custom-reference.docx`;
- `inst/app/tests/test_comunicacao_resultados.R`;
- `inst/app/tests/test_exportacao_comunicacao.R`;
- `inst/app/tests/test_anova_exportacao.R`.
