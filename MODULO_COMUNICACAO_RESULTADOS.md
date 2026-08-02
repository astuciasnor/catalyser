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

## Arquivos principais

- `inst/app/modules/mod_comunicacao.R`;
- `inst/app/modules/registro_comunicacao.R`;
- `inst/app/modules/exportacao_comunicacao.R`;
- `inst/app/templates/funcoes_projeto_integrado.R`;
- `inst/app/tests/test_comunicacao_resultados.R`;
- `inst/app/tests/test_exportacao_comunicacao.R`;
- `inst/app/tests/test_anova_exportacao.R`.
