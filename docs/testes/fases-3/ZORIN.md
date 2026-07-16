# Roteiro de homologação no Zorin OS

Repita no Zorin somente depois de concluir e registrar o teste no Windows.

## 1. Criar um clone separado

Não use a pasta ou a biblioteca R empregada pelos alunos.

```bash
mkdir -p ~/Projetos/testes-catalyser
cd ~/Projetos/testes-catalyser
git clone https://github.com/astuciasnor/catalyser.git catalyser-fases
cd catalyser-fases
git fetch --all --prune
git status --short
```

`git status --short` deve ficar sem saída.

## 2. Isolar pacotes adicionais, se forem necessários

Primeiro tente usar as dependências já presentes, sem atualizar nada. Se faltar
um pacote, crie uma biblioteca exclusiva para os testes:

```bash
mkdir -p ~/R/catalyser-teste-library
export R_LIBS_USER=~/R/catalyser-teste-library
R -q -e '.libPaths()'
```

Instale eventual dependência ausente somente nessa biblioteca. Não altere a
biblioteca congelada do semestre.

## 3. Registrar o ambiente

```bash
git rev-parse --short HEAD
Rscript --version
quarto --version
R -q -e 'sessionInfo()'
```

## 4. Trocar de branch

Na primeira abertura de uma fase:

```bash
git switch --track origin/feature/fase-3a-registro-bases
```

Se a branch local já existir:

```bash
git switch feature/fase-3a-registro-bases
```

Use a sequência e os commits da [matriz](MATRIZ_BRANCHES.md). Depois de cada
troca:

```bash
git status --short
git rev-parse --short HEAD
```

## 5. Executar sem instalar

```bash
R --vanilla -q -e "shiny::runApp('inst/app', launch.browser = TRUE)"
```

Siga o roteiro manual da fase no [índice](README.md), encerre a aplicação e só
então passe à branch seguinte.

## 6. Bateria automática final

Na branch `chore/revisao-fases-3`:

```bash
Rscript inst/app/tests/run_tests.R
```

O Quarto deve gerar um Word real. O resultado final esperado é:

```text
Todos os testes automatizados das Fases 3 passaram.
```

## 7. Comparação Windows × Zorin

Compare especialmente:

- acentuação nos nomes de bases, narrativas e tabelas;
- leitura das abas do Excel;
- atualização dos seletores depois de recalcular uma base;
- nomes e separadores de caminhos no Projeto R exportado;
- abertura do `.Rproj`, execução do `relatorio.qmd` e geração do Word;
- conteúdo e ordem do relatório nos dois sistemas.

Uma diferença entre sistemas deve ser registrada como falha de portabilidade,
mesmo que a análise estatística esteja correta.
