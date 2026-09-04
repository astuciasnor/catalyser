# Comunicação de Resultados — especificação atual

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

Mostra a conferência antes da exportação e oferece downloads separados:

- relatório Word `.docx`;
- Projeto R `.zip`.

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

O Word recebe apenas o conteúdo editorial escolhido. O Projeto R recebe todas
as execuções registradas, bases, receitas, scripts e metadados.

Sem Quarto, o Word não pode ser renderizado, mas o Projeto R continua sendo a
saída reproduzível.

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
ecossistema (`D:\Claude\eapa\EAPACaderno/`), para que quem sai da
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

**Decisão em aberto:** o projeto exportado não tem `R/funcoes.R` porque suas
funções vêm do pacote `catalyser`. Se, na Etapa 4, o código humanizado passar a
usar `fmt()`, `formatar_p()` e `flextable_ocean()` como o EAPACaderno, o
`R/funcoes.R` volta.

## Arquivos principais

- `inst/app/modules/mod_comunicacao.R`;
- `inst/app/modules/registro_comunicacao.R`;
- `inst/app/modules/exportacao_comunicacao.R`;
- `inst/app/templates/funcoes_projeto_integrado.R`;
- `inst/app/tests/test_comunicacao_resultados.R`;
- `inst/app/tests/test_exportacao_comunicacao.R`;
- `inst/app/tests/test_anova_exportacao.R`.
