# Revisão pré-main da V16 — 27/07/2026

## Resultado

A V16 está tecnicamente pronta para ser integrada à `main`, condicionada apenas
ao commit, à revisão final do diff e ao merge/push deliberados.

Base da revisão:

- branch: `feature/v16-anova-integrada`;
- V15 de origem: `6511753`;
- versão do pacote: `0.1.4`;
- homologação funcional completa informada pelo usuário em outra VM.

## Ajustes feitos durante a revisão

- corrigido o carregamento do motor de replay nos testes novos;
- corrigido o fixture do segundo gráfico, que usava `comprimento_cm` apesar do
  título de peso;
- garantido que o segundo gráfico e seu código exportado usem `peso_g`;
- reproduzidos no Projeto R o título, os rótulos e o tema escolhidos na ANOVA;
- tornado o código pedagógico de Levene seguro para nomes não sintáticos de
  colunas;
- centralizados os rótulos dos componentes registrados;
- removidos anexos globais desnecessários de `tibble` e `flextable` do motor
  canônico da ANOVA;
- corrigido o instalador oficial para atualizar a CatalyseR pela `main` e
  exigir `EAPADados >= 0.1.10`;
- corrigida no README a descrição da ANOVA, que é unifatorial.

## Evidências

- 70 arquivos R analisados sintaticamente;
- suíte completa de `inst/app/tests/run_tests.R` aprovada;
- benchmarks da ANOVA aprovados;
- troca de Y entre `comprimento_cm` e `peso_g` aprovada;
- Word integrado renderizado pelo Quarto;
- `R CMD build` aprovado;
- instalação do pacote em biblioteca vazia aprovada;
- pacote instalado carregado e seus 70 arquivos R analisados;
- `R CMD check --no-manual`: 0 erros, 0 avisos e 1 nota histórica.

A nota é a já conhecida sobre dependências declaradas em `Imports` e utilizadas
pelos arquivos Shiny localizados em `inst/app`, fora da inspeção normal do
namespace do pacote.

## Avisos não bloqueantes já existentes

- ajustes essencialmente perfeitos em dados artificiais de testes;
- estética `size` de linha depreciada em módulo legado;
- parâmetro `label.size` ignorado em anotação legada.

Esses avisos não foram introduzidos pela V16 e podem ser tratados durante o
refinamento gradual das análises piloto.

## Refinamentos deliberadamente adiados

- unificar o núcleo estatístico usado pela interface e pelo replay do Projeto R,
  hoje mantidos em funções paralelas para preservar a exportação já homologada;
- aprimorar gradualmente tabelas, gráficos diagnósticos e narrativas da ANOVA;
- eliminar os avisos legados de `ggplot2` sem misturar essa limpeza à integração
  funcional da V16.

Esses pontos não bloqueiam a integração: os testes de equivalência cobrem o
resultado da ANOVA e o percurso exportado foi executado com sucesso.

## Integração sugerida

A `main` é ancestral direta da branch de trabalho. Portanto, depois do commit
da V16, a integração pode ser feita por fast-forward, sem commit de merge:

```powershell
git switch main
git merge --ff-only feature/v16-anova-integrada
git push origin main
```

Não executar esses três comandos sem autorização explícita do usuário.

## Teste da instalação pela main

Depois do push:

```r
source("https://raw.githubusercontent.com/astuciasnor/catalyser/main/instalar_catalyser.R")
```

O instalador deve informar a atualização da CatalyseR e, ao final:

```r
packageVersion("catalyser")
# [1] '0.1.4'

catalyser::run_app()
```
