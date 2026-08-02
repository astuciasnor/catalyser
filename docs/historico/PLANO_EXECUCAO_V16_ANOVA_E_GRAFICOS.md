# Plano de execução da V16 — ANOVA completa e teste de dois valores de Y

> **Plano concluído e arquivado.** A V16 foi homologada e integrada; este
> documento permanece como registro do percurso e dos critérios usados.

- **Data de referência:** 27/07/2026
- **Base homologada:** CatalyseR V15, pacote `0.1.3`
- **Versão de entrega da V16:** pacote `0.1.4`
- **Branch de origem:** `chore/revisao-fases-3`
- **Commit de origem obrigatório:** `6511753bbbe1af981c68cad17519fd7bab0911b9`
- **Commit:** `6511753 feat: encerra V15 homologada de ponta a ponta`
- **Escopo:** ANOVA de um fator e teste do Gráfico de Linhas com duas escolhas
  sucessivas de Y
- **Artefato esperado:** `catalyser-anova-integrada-AAAAMMDD-v16.zip`

## 1. Objetivo

Este documento é um roteiro de passagem de serviço para outra IA implementar e
homologar a V16 da CatalyseR sem reabrir decisões já aprovadas na V15.

A entrega deve completar dois percursos:

1. integrar uma **ANOVA de um fator completa** ao pipeline comum
   `Base Derivada → Executar análise → Adicionar Novo Resultado → Comunicação
   → Word → Projeto R`;
2. usar uma **segunda Base Derivada independente** para executar o Gráfico de
   Linhas duas vezes, alternando a variável do eixo Y entre
   `comprimento_cm` e `peso_g`.

O segundo percurso não pede um gráfico com dois eixos Y simultâneos. Ele testa
duas configurações independentes do mesmo seletor Y. Cada configuração deve
produzir uma execução registrada, reproduzível e identificável no relatório e
no Projeto R.

## 2. Decisões fixas de escopo

### 2.1. O que entra

- ANOVA unifatorial para grupos independentes;
- resumo descritivo por grupo;
- tabela da ANOVA;
- tamanho de efeito;
- pressupostos e diagnósticos;
- comparações de Tukey;
- gráfico principal;
- narrativa automática em português;
- console bruto;
- seletor de Base Compartilhada ou Base Derivada;
- execução explícita;
- registro e Comunicação de Resultados;
- replay no Projeto R;
- código pedagógico explícito no QMD;
- relatório Word integrado;
- teste de duas escolhas sucessivas de Y no Gráfico de Linhas.

### 2.2. O que não entra

- ANOVA de dois fatores;
- medidas repetidas;
- ANOVA fatorial, MANOVA ou modelos mistos;
- transformação automática dos dados;
- troca automática para teste não paramétrico;
- gráfico com dois eixos Y simultâneos;
- novo tipo de gráfico;
- nova arquitetura de bases;
- alteração de teste *t*, regressão ou qui-quadrado sem que seja necessária
  para impedir uma regressão provocada pela V16.

### 2.3. Princípios preservados

- A CatalyseR continua sendo a fonte da verdade.
- Toda Base Derivada nasce diretamente de `dados_analise`.
- Não existem ramos de ramos.
- Alterar base ou parâmetro deixa a execução pendente.
- Nada é registrado silenciosamente.
- O Projeto R deve funcionar sem a CatalyseR.
- O QMD deve mostrar o código essencial no arquivo-fonte, sem despejá-lo no
  Word.
- A interface deve ser legível para iniciantes em 1366 × 768.

## 3. Diagnóstico do código atual

### 3.1. O que a ANOVA já possui

O arquivo `inst/app/modules/mod_anova.R` já contém:

- seletores `var_y` e `var_x`;
- cálculo reativo de ANOVA;
- tabela da ANOVA;
- Shapiro-Wilk e Bartlett;
- Tukey HSD;
- gráfico de médias;
- resíduos × ajustados e Q-Q plot;
- curva F;
- simulador didático;
- download próprio de Word;
- download próprio de Projeto R.

As funções atuais ficam em:

- `inst/app/templates/funcoes_anova.R`;
- `inst/app/templates/relatorio_anova.qmd`.

### 3.2. Lacunas que a V16 deve fechar

A ANOVA atual:

- lê diretamente `dados_analise`;
- não usa `mod_seletor_base_analise`;
- calcula automaticamente, sem o contrato de execução explícita;
- não devolve `estado_execucao`;
- não usa `mod_registrar_execucao`;
- não aparece no novo estúdio de Comunicação;
- não possui replay em `funcoes_projeto_integrado.R`;
- não possui código pedagógico no exportador integrado;
- mantém exportações legadas paralelas ao fluxo homologado na V15;
- não apresenta tamanho de efeito;
- usa texto inadequado como “H0 aceita” no simulador;
- conecta médias de categorias por uma linha, o que pode sugerir uma ordem
  inexistente entre os grupos.

### 3.3. Situação do Gráfico de Linhas

O Gráfico de Linhas já possui:

- seletor de base;
- execução explícita;
- registro;
- replay integrado;
- código pedagógico;
- saída na Comunicação.

Arquivos principais:

- `inst/app/modules/mod_viz_extra.R`;
- `inst/app/templates/funcoes_projeto_integrado.R`;
- `inst/app/modules/exportacao_comunicacao.R`.

Em princípio, o gráfico requer testes de regressão e correção somente se a
alternância de Y revelar defeito. Não reescrever esse módulo antecipadamente.

## 4. Ordem obrigatória de trabalho

### Fase 0 — proteger a V15

Executar antes de editar:

```powershell
Set-Location D:\Claude\eapa\catalyser
git status --short
git branch --show-current
git rev-parse HEAD
git log -1 --oneline
```

Condições para continuar:

- árvore limpa;
- branch `chore/revisao-fases-3`;
- `HEAD` em `6511753bbbe1af981c68cad17519fd7bab0911b9`.

Criar uma branch exclusiva:

```powershell
git switch -c feature/v16-anova-integrada
```

Não alterar, reescrever ou apagar o commit homologado da V15.

### Fase 1 — obter uma linha de base

Usar o R informado para o projeto:

```powershell
$R = "C:\R\R-4.6.1\bin\Rscript.exe"
& $R inst/app/tests/run_tests.R
```

Também analisar sintaticamente todos os arquivos R antes e depois da mudança.
Se algum teste já falhar na linha de base, registrar a falha e não atribuí-la à
V16.

## 5. Implementação estatística canônica da ANOVA

### 5.1. Fonte única

Refatorar `inst/app/templates/funcoes_anova.R` para que o cálculo, a
arrumação e a narrativa sejam funções puras reutilizáveis pela interface e pelo
Projeto R.

Separar, no mínimo:

```r
calcular_anova()
arrumar_descritivos_anova()
arrumar_tabela_anova()
arrumar_tamanho_efeito_anova()
arrumar_pressupostos_anova()
arrumar_tukey_anova()
relatar_anova()
grafico_anova()
grafico_diagnosticos_anova()
```

É permitido manter `mostrar_anova()`, `mostrar_tukey()` e
`mostrar_pressupostos()` como wrappers de compatibilidade durante a migração.

### 5.2. Validações obrigatórias

Antes do ajuste:

- confirmar que a resposta existe e é numérica;
- confirmar que o fator existe;
- remover somente os casos incompletos das duas variáveis usadas;
- informar `n` analisado e número de casos excluídos;
- converter o fator explicitamente;
- exigir pelo menos dois níveis válidos;
- exigir pelo menos duas observações em cada nível;
- avisar, sem bloquear necessariamente, quando algum grupo tiver `n < 5`;
- impedir resposta e fator iguais;
- apresentar mensagens com ação corretiva.

Nenhuma exclusão de NA pode ficar oculta na narrativa.

### 5.3. Componentes do resultado

#### Resumo por grupo

Exibir:

- grupo;
- `n`;
- ausentes excluídos;
- média;
- desvio-padrão;
- mediana;
- mínimo;
- máximo.

#### Tabela da ANOVA

Exibir:

- fonte de variação;
- graus de liberdade;
- soma de quadrados;
- quadrado médio;
- F;
- p-valor.

#### Tamanho de efeito

Calcular e exibir:

- eta quadrado (`η²`);
- ômega quadrado (`ω²`);
- intervalo de confiança quando a função usada o fornecer.

Usar `effectsize` com namespace explícito. Se o pacote estiver indisponível,
produzir mensagem orientadora, sem falsificar valor.

#### Pressupostos

Combinar inspeção gráfica e testes:

- resíduos × ajustados;
- Q-Q plot;
- Shapiro-Wilk dos resíduos, quando aplicável;
- Levene com centro na mediana, por `car::leveneTest()`.

O teste de Bartlett atual pode ser preservado como informação adicional, mas
Levene deve ser o teste principal de homogeneidade. Não concluir que um
pressuposto foi comprovado apenas porque `p ≥ 0,05`.

#### Comparações múltiplas

Usar Tukey HSD ou `emmeans` com ajuste de Tukey, mantendo:

- par comparado;
- diferença estimada;
- limite inferior do IC 95%;
- limite superior do IC 95%;
- p-valor ajustado;
- indicação textual de evidência de diferença.

As comparações podem ser calculadas sempre para reprodutibilidade, mas a
narrativa deve deixar claro que sua interpretação principal decorre da ANOVA
global e do plano analítico.

#### Gráfico principal

Mostrar:

- observações individuais;
- distribuição de cada grupo;
- média por grupo;
- IC 95% claramente identificado;
- cores Ocean Gradient;
- eixos e unidades legíveis.

Não conectar por linha as médias de espécies ou de outros fatores nominais.

### 5.4. Narrativa

A narrativa automática deve informar:

- pergunta respondida;
- base e variáveis;
- `n` analisado e casos excluídos;
- número e nomes dos grupos;
- médias ou resumo relevante;
- F e graus de liberdade;
- p-valor;
- `η²` e/ou `ω²`;
- resultado dos diagnósticos sem linguagem absoluta;
- pares de Tukey com evidência de diferença;
- conclusão estatística sem extrapolação causal.

Nunca usar “H0 aceita”. Usar:

- “rejeitou-se H0”; ou
- “não houve evidência suficiente para rejeitar H0”.

## 6. Integrar a ANOVA ao fluxo comum

### 6.1. Finalidade da Base Derivada

Em `inst/app/modules/registro_bases.R`, acrescentar:

```r
"ANOVA" = "anova"
```

Não esconder outras bases válidas; essa finalidade apenas prioriza a lista.

### 6.2. Interface em `app.R`

No painel `ANOVA (Análise de Variância)`:

```r
mod_analise_registravel_ui(
  "fluxo_anova",
  tagList(
    mod_seletor_base_analise_ui("base_anova"),
    mod_anova_ui("anova")
  ),
  mod_registrar_execucao_ui("registrar_anova")
)
```

No servidor:

```r
seletor_anova <- mod_seletor_base_analise_server(
  "base_anova",
  dados_analise,
  registro_bases_rv,
  cache_bases_rv,
  revisao_dados_analise_rv,
  finalidade_preferida = "anova",
  nome_analise = "A ANOVA"
)

anova_resultado <- mod_anova_server(
  "anova",
  seletor_anova$dados,
  import_info
)
```

Depois, ligar `anova_resultado$estado_execucao` e
`seletor_anova$contexto` a `mod_registrar_execucao_server()`.

Preservar os IDs `anova-var_y` e `anova-var_x`.

### 6.3. Execução explícita

Aplicar em `mod_anova.R` o mesmo contrato usado por teste *t* e regressão:

- `execucao_explicita_controles_ui(ns)`;
- `execucao_explicita_resultados_ui(...)`;
- `execucao_explicita_downloads_ui(...)`, enquanto downloads legados ainda
  existirem;
- assinatura contendo base, resposta, fator e opções de apresentação;
- `eventReactive()` disparado por `gatilho_execucao`;
- `execucao_explicita_server(...)`.

Requisitos:

- executar corretamente no primeiro clique;
- alteração de Y, X ou base deixa a execução pendente;
- resultado antigo não pode parecer atual;
- configuração inválida não deve produzir resultado parcial;
- o botão “Adicionar Novo Resultado” só fica disponível após execução válida.

### 6.4. Organização pedagógica da ANOVA

Usar divulgação progressiva:

1. **Resultado principal** — narrativa, resumo por grupo, tabela ANOVA e
   gráfico;
2. **Comparações** — Tukey;
3. **Pressupostos e diagnósticos** — tabela, resíduos e Q-Q plot;
4. **Console R** — saída bruta;
5. **Laboratório didático** — curva F e simulador, claramente separados dos
   dados observados.

Evitar alturas fixas e rolagens internas. Não empilhar todas essas seções em
uma única tela.

### 6.5. Estado canônico registrado

O módulo deve devolver algo equivalente a:

```r
list(
  analise_id = "anova",
  tipo = "anova_um_fator",
  titulo = "Profundidade de captura entre espécies",
  parametros = list(
    resposta = "profundidade_m",
    fator = "especie",
    nivel_confianca = 0.95,
    ajuste_comparacoes = "tukey"
  ),
  saidas_disponiveis = c(
    "narrativa",
    "descritivos",
    "tabela",
    "comparacoes",
    "grafico",
    "pressupostos",
    "diagnosticos",
    "console"
  ),
  resultado_resumo = list(
    n = 68L,
    grupos = 5L,
    f = 2.831,
    gl_1 = 4L,
    gl_2 = 63L,
    p = 0.0318
  ),
  codigo_r = "..."
)
```

Os números acima são checkpoints aproximados do dataset de homologação, não
valores a serem codificados.

### 6.6. Componentes editoriais adicionais

Em `inst/app/modules/registro_comunicacao.R`, acrescentar rótulos gerais para:

```r
descritivos = "Resumo por grupo"
comparacoes = "Comparações múltiplas"
```

Não alterar o significado dos componentes já existentes.

## 7. Replay, QMD, Word e Projeto R

### 7.1. Replay integrado

Em `inst/app/templates/funcoes_projeto_integrado.R`:

- implementar `catalyser_anova(dados, p)`;
- incluir `anova_um_fator` em `catalyser_executar()`;
- devolver componentes com os mesmos nomes do estado registrado;
- usar a base recebida e somente os parâmetros congelados;
- não consultar inputs Shiny;
- manter objeto do modelo em componente interno para inspeção.

O replay deve devolver:

```r
list(
  narrativa = ...,
  descritivos = ...,
  tabela = ...,
  comparacoes = ...,
  grafico = ...,
  pressupostos = ...,
  diagnosticos = ...,
  console = ...,
  objeto = ...
)
```

### 7.2. Código pedagógico no QMD

Em `inst/app/modules/exportacao_comunicacao.R`, acrescentar
`anova_um_fator` a `exportacao_codigo_estudo()`.

O QMD deve conter, em um chunk de estudo não avaliado:

```r
formula_anova <- stats::reformulate("especie", response = "profundidade_m")
modelo_anova <- stats::aov(formula_anova, data = dados)

summary(modelo_anova)
stats::TukeyHSD(modelo_anova)
car::leveneTest(profundidade_m ~ as.factor(especie), data = dados)
stats::shapiro.test(stats::residuals(modelo_anova))
effectsize::eta_squared(modelo_anova)
effectsize::omega_squared(modelo_anova)
```

O código precisa ficar visível no arquivo `.qmd`, com:

```yaml
eval: false
include: false
```

Assim o aluno encontra o caminho “mouse → código”, mas o Word não fica
poluído.

### 7.3. Word integrado

No estúdio de Comunicação, a execução ANOVA deve permitir selecionar:

- narrativa;
- resumo por grupo;
- tabela ANOVA;
- comparações;
- gráfico;
- pressupostos;
- diagnósticos;
- console.

As tabelas referenciadas devem usar o tema Ocean. O Word deve apresentar
legendas, não cortar colunas, não repetir títulos e não misturar resultados
observados com o simulador.

### 7.4. Exportações legadas

Os downloads próprios de ANOVA em `mod_anova.R` e
`relatorio_anova.qmd` são legados.

Procedimento:

1. primeiro alcançar equivalência no exportador integrado;
2. cobrir o novo fluxo com testes;
3. retirar os botões legados da interface para não oferecer dois caminhos
   concorrentes;
4. apagar código legado somente em commit separado e somente se nenhuma
   referência permanecer.

O fluxo oficial da V16 deve ser a Comunicação de Resultados.

## 8. Dataset e duas Bases Derivadas da homologação

### 8.1. Arquivo

Usar:

```text
inst/app/dados/Treino-Transformacoes.xlsx
```

Planilha:

```text
biometria
```

### 8.2. Base Compartilhada

Compor a Base Compartilhada com:

1. padronizar texto de `especie`: remover espaços extras;
2. padronizar texto de `especie`: minúsculas;
3. padronizar texto de `local`: remover espaços extras;
4. padronizar texto de `local`: minúsculas;
5. remover duplicatas exatas.

Revisar a tabela final e confirmar cerca de 68 observações após a remoção das
três duplicatas planejadas no dataset.

### 8.3. Base Derivada A — ANOVA

Criar:

- **Nome amigável:** `Profundidade de captura por espécie`
- **Objeto R:** `base_anova_profundidade_especie`
- **Finalidade:** `ANOVA`
- **Descrição:** `Compara a profundidade de captura entre as espécies.`

Receita:

1. remover linhas com NA em `profundidade_m`;
2. recalcular;
3. conferir a prévia;
4. finalizar o preparo.

Não agrupar nem sumarizar esta base antes da ANOVA. A análise precisa das
observações individuais.

### 8.4. Base Derivada B — gráficos

Criar:

- **Nome amigável:** `Biometria completa das corvinas`
- **Objeto R:** `base_graficos_corvina`
- **Finalidade:** `Gráficos`
- **Descrição:** `Observações de corvina com comprimento e peso completos.`

Receita:

1. filtrar `especie == "corvina"`;
2. remover linhas com NA em `comprimento_cm`;
3. remover linhas com NA em `peso_g`;
4. recalcular;
5. conferir a prévia;
6. finalizar o preparo.

Checkpoint esperado:

- 19 linhas;
- `comprimento_cm` entre aproximadamente 13,4 e 33,1;
- `peso_g` entre aproximadamente 33 e 340.

## 9. Roteiro manual da ANOVA

1. Abrir **Testes Paramétricos → ANOVA**.
2. Em **Base utilizada**, escolher
   `Profundidade de captura por espécie — base_anova_profundidade_especie`.
3. Escolher:
   - resposta Y: `profundidade_m`;
   - fator X: `especie`.
4. Confirmar que nada foi executado automaticamente.
5. Clicar uma vez em **Executar análise**.
6. Conferir resultado principal, comparações, pressupostos, diagnósticos e
   console.
7. Confirmar que o resultado usa 68 observações e cinco espécies.
8. Clicar em **Adicionar Novo Resultado**.
9. Usar o título `Profundidade de captura entre espécies`.

### 9.1. Benchmarks aproximados

Os valores abaixo servem para detectar troca de base, variável ou replay:

| Componente | Valor aproximado |
|---|---:|
| n | 68 |
| grupos | 5 |
| F | 2,831 |
| gl do fator | 4 |
| gl dos resíduos | 63 |
| p da ANOVA | 0,0318 |
| η² | 0,15 |
| ω² | 0,10 |
| p de Shapiro | 0,7035 |
| p de Levene | 0,2458 |

Médias aproximadas de `profundidade_m`:

| Espécie | n | Média |
|---|---:|---:|
| bagre | 6 | 28,1 |
| corvina | 26 | 19,7 |
| pargo | 7 | 26,5 |
| pescada amarela | 14 | 29,9 |
| sardinha | 15 | 21,6 |

No Tukey, o contraste `pescada amarela - corvina` deve ter p ajustado próximo
de `0,0300`. Pequenas diferenças de arredondamento são aceitáveis.

## 10. Roteiro manual dos dois valores de Y

1. Abrir **Visualizando Dados → Gráfico de Linhas**.
2. Selecionar
   `Biometria completa das corvinas — base_graficos_corvina`.
3. Configurar:
   - X: `id`;
   - Y: `comprimento_cm`;
   - série por cor: `Nenhuma`;
   - marcar pontos: sim.
4. Usar o título `Comprimento das corvinas por observação`.
5. Clicar uma vez em **Executar análise**.
6. Clicar em **Adicionar Novo Resultado**.
7. Alterar somente Y para `peso_g`.
8. Confirmar que a interface marca o resultado anterior como pendente.
9. Confirmar que o gráfico não muda silenciosamente antes do clique.
10. Usar o título `Peso das corvinas por observação`.
11. Clicar uma vez em **Executar análise**.
12. Confirmar que o gráfico agora usa `peso_g`.
13. Adicionar como uma segunda execução.

Resultado esperado:

- uma execução ANOVA;
- uma execução do gráfico com Y = `comprimento_cm`;
- uma execução do gráfico com Y = `peso_g`;
- as três vinculadas às Bases Derivadas corretas;
- as duas execuções gráficas preservadas separadamente.

O eixo `id` representa apenas a ordem das observações. Não interpretar o
gráfico como série temporal.

## 11. Comunicação de Resultados

Na Comunicação:

1. confirmar as três execuções;
2. confirmar as duas Bases Derivadas em **Bases do projeto**;
3. manter a ANOVA primeiro;
4. manter os dois gráficos depois;
5. selecionar todos os componentes relevantes da ANOVA;
6. incluir os dois gráficos no Word;
7. conferir o Esboço do documento;
8. conferir o Manifesto editorial;
9. gerar o Word;
10. gerar o Projeto R.

O Projeto R deve preservar todas as execuções registradas, mesmo que alguma
seja desmarcada no Word.

## 12. Testes automatizados obrigatórios

Criar, preferencialmente:

```text
inst/app/tests/test_anova_integrada.R
inst/app/tests/test_anova_exportacao.R
inst/app/tests/test_grafico_linhas_troca_y.R
```

Cobrir:

### 12.1. Núcleo da ANOVA

- validação de colunas;
- resposta não numérica;
- fator com um nível;
- grupo pequeno;
- remoção explícita de casos incompletos;
- tabela ANOVA;
- descritivos;
- efeito;
- pressupostos;
- Tukey;
- narrativa;
- gráfico;
- ausência da expressão “H0 aceita”.

### 12.2. Estado Shiny

- primeiro clique executa;
- configuração inicial não executa;
- mudança de Y deixa pendente;
- mudança de X deixa pendente;
- mudança de base deixa pendente;
- configuração inválida não produz estado registrável;
- `estado_execucao` contém todos os parâmetros;
- registro vincula a base correta.

### 12.3. Replay e exportação

- `catalyser_executar()` reconhece `anova_um_fator`;
- replay produz valores equivalentes aos da interface;
- `exportacao_codigo_estudo()` produz `stats::aov`;
- código pedagógico contém Tukey, Levene e tamanho de efeito;
- scripts numerados usam a Base Derivada correta;
- QMD contém o código essencial;
- Word renderiza;
- componentes adicionais aparecem com os rótulos corretos.

### 12.4. Troca de Y no gráfico

- primeira execução guarda `y = "comprimento_cm"`;
- mudança para `peso_g` deixa a execução pendente;
- segundo clique guarda `y = "peso_g"`;
- as duas execuções coexistem;
- o replay de cada uma usa sua própria variável Y;
- o QMD contém os dois códigos;
- nenhuma configuração é sobrescrita pela seguinte.

Adicionar os novos testes a `inst/app/tests/run_tests.R`.

## 13. Verificações finais locais

Executar:

```powershell
$R = "C:\R\R-4.6.1\bin\Rscript.exe"
& $R inst/app/tests/run_tests.R
& $R -e "fs <- list.files('inst/app', pattern='[.]R$', recursive=TRUE, full.names=TRUE); invisible(lapply(fs, parse)); cat(length(fs), 'arquivos R aprovados\n')"
git diff --check
git status --short
```

Depois:

- `R CMD build`;
- instalação em biblioteca vazia;
- `R CMD check --no-manual`;
- inspeção visual em 1366 × 768;
- teste completo na VM Windows 10;
- abertura do Projeto R no Windows 11;
- execução dos scripts na ordem;
- renderização do QMD para Word;
- comparação dos benchmarks entre CatalyseR e Projeto R.

Aceitar no `R CMD check` somente a nota já conhecida sobre dependências usadas
pelos arquivos Shiny de `inst/app`, se ela permanecer idêntica e documentada.

## 14. Sequência sugerida de commits

1. `test: fixa benchmarks e contrato da ANOVA integrada`
2. `refactor: separa cálculo e apresentação da ANOVA`
3. `feat: integra ANOVA a bases e execução explícita`
4. `feat: registra ANOVA na Comunicação de Resultados`
5. `feat: reproduz ANOVA no Projeto R e no QMD`
6. `test: cobre troca de variável Y no gráfico de linhas`
7. `docs: adiciona roteiro de homologação da V16`
8. `chore: gera artefato homologável da V16`

Cada commit deve deixar a aplicação executável. Não misturar a retirada de
código legado com a primeira integração funcional.

## 15. Critérios de aceitação

A V16 só pode ser considerada pronta quando:

- a ANOVA usa Base Compartilhada ou Base Derivada;
- a base ativa fica claramente visível;
- a ANOVA executa no primeiro clique;
- qualquer mudança relevante deixa a execução pendente;
- a análise oferece resumo por grupo, ANOVA, efeito, Tukey, pressupostos,
  diagnósticos, gráfico, narrativa e console;
- a execução pode ser adicionada aos resultados;
- a Comunicação permite escolher os componentes;
- o Word é legível e estatisticamente coerente;
- o Projeto R reproduz a ANOVA fora da CatalyseR;
- o QMD mostra o código essencial;
- os benchmarks conferem;
- a segunda Base Derivada alimenta o Gráfico de Linhas;
- a troca de `comprimento_cm` para `peso_g` funciona no primeiro clique;
- as duas execuções gráficas coexistem e não se sobrescrevem;
- a V15 continua passando em todos os testes;
- não há painel ou botão cortado em 1366 × 768;
- o usuário homologa o percurso na VM antes do encerramento da V16.

## 16. Entrega para homologação

Gerar:

```text
catalyser-anova-integrada-AAAAMMDD-v16.zip
LEIA-ME-TESTE-V16.txt
SHA256SUMS.txt
```

O `LEIA-ME` deve começar desde:

1. copiar o ZIP para a VM;
2. conferir SHA-256;
3. descompactar em pasta nova;
4. abrir a pasta correta;
5. iniciar a CatalyseR com R 4.6.1;
6. executar o roteiro das duas bases;
7. gerar Word e Projeto R;
8. copiar o Projeto R para o Windows 11;
9. executar os scripts;
10. renderizar o QMD;
11. registrar divergências com captura de tela.

Não fazer push da V16 antes da homologação, salvo autorização explícita do
usuário.

## 17. Arquivos que a outra IA deve ler primeiro

1. `../AGENTS.md`
2. este documento;
3. `PLANO_CONTINUIDADE_CATALYSER_APOS_V15.md`;
4. `EVOLUCAO_TRATAMENTO_DADOS.md`;
5. `MODULO_COMUNICACAO_RESULTADOS.md`;
6. `docs/testes/fases-3/HOMOLOGACAO_END_TO_END_WINDOWS_2026-07-26.md`;
7. `docs/testes/fases-3/ROTEIRO_TESTE_V15_DUAS_ANALISES.md`;
8. `inst/app/modules/mod_anova.R`;
9. `inst/app/templates/funcoes_anova.R`;
10. `inst/app/modules/mod_execucao_explicita.R`;
11. `inst/app/modules/mod_seletor_base_analise.R`;
12. `inst/app/modules/mod_registrar_execucao.R`;
13. `inst/app/modules/registro_execucoes.R`;
14. `inst/app/modules/registro_comunicacao.R`;
15. `inst/app/modules/exportacao_comunicacao.R`;
16. `inst/app/templates/funcoes_projeto_integrado.R`;
17. `inst/app/modules/mod_viz_extra.R`;
18. os trechos de integração em `inst/app/app.R`.

## 18. Prompt curto para passagem de serviço

> Continue a CatalyseR exatamente a partir do commit
> `6511753bbbe1af981c68cad17519fd7bab0911b9`. Implemente este plano sem ampliar
> o escopo: integre a ANOVA de um fator ao seletor de base, execução explícita,
> registro, Comunicação, Word, Projeto R e QMD pedagógico. Use
> `profundidade_m ~ especie` na Base Derivada
> `base_anova_profundidade_especie` e confira os benchmarks deste documento.
> Crie também `base_graficos_corvina`; no Gráfico de Linhas, registre uma
> execução com Y `comprimento_cm` e outra com Y `peso_g`, verificando que a
> troca deixa o estado pendente e que as duas execuções são preservadas.
> Mantenha a V15 sem regressões, faça mudanças pequenas com testes e só gere a
> V16 após o pipeline completo funcionar dentro da CatalyseR e no RStudio.
