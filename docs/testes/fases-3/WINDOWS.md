# Roteiro de homologação no Windows

## 1. Preparar uma pasta independente

Não use a pasta de desenvolvimento nem reinstale a versão já utilizada nas
atividades.

```powershell
New-Item -ItemType Directory -Force D:\Testes | Out-Null
Set-Location D:\Testes
git clone https://github.com/astuciasnor/catalyser.git catalyser-fases
Set-Location catalyser-fases
git fetch --all --prune
git status --short
```

O último comando deve ficar sem saída. Se houver arquivos modificados, pare e
descubra sua origem antes de trocar de branch.

Se o teste ocorrer na mesma máquina antes do push, use o clone local explicado
na [matriz de branches](MATRIZ_BRANCHES.md).

## 2. Registrar o ambiente

```powershell
git rev-parse --short HEAD
& "C:\R\R-4.6.1\bin\Rscript.exe" --version
quarto --version
```

Confirme também no R:

```r
sessionInfo()
```

## 3. Abrir cada branch

Na primeira vez:

```powershell
git switch --track origin/feature/fase-3a-registro-bases
```

Nas fases seguintes, substitua o nome conforme a
[matriz](MATRIZ_BRANCHES.md). Se a branch local já existir:

```powershell
git switch feature/fase-3a-registro-bases
```

Depois de cada troca, confirme:

```powershell
git status --short
git rev-parse --short HEAD
```

O primeiro comando deve ficar vazio e o segundo deve coincidir com o commit da
tabela.

## 4. Executar a aplicação sem instalar

Na raiz do clone:

```powershell
& "C:\R\R-4.6.1\bin\R.exe" --vanilla -q -e "shiny::runApp('inst/app', launch.browser = TRUE)"
```

Isso executa o código da branch diretamente. Não use `remove.packages()`,
`install_github()` ou o instalador da CatalyseR durante a homologação.

## 5. Teste manual fase a fase

1. abra o roteiro correspondente no [índice](README.md);
2. use `inst/app/dados/Treino-Transformacoes.xlsx` quando solicitado;
3. execute todos os itens do roteiro;
4. encerre a aplicação antes de trocar de branch;
5. registre aprovação ou falha e o commit testado.

Faça uma cópia nova dos arquivos Word/ZIP exportados em cada execução. Não
reutilize uma pasta de projeto exportado por outra branch.

## 6. Bateria automática final

Depois dos testes individuais, abra `chore/revisao-fases-3` e rode:

```powershell
& "C:\R\R-4.6.1\bin\Rscript.exe" inst/app/tests/run_tests.R
```

O teste final precisa localizar o Quarto e realmente gerar um `.docx`. Ao
terminar, deve aparecer:

```text
Todos os testes automatizados das Fases 3 passaram.
```

## 7. Critérios de aprovação no Windows

- nenhuma branch altera a instalação estável;
- a aplicação abre sem erro;
- todos os itens do roteiro da fase passam;
- bases desatualizadas não entram nas análises nem na exportação;
- resultados só aparecem depois de **Executar análise**;
- o Word respeita a seleção editorial;
- o Projeto R preserva todas as execuções registradas;
- os scripts do projeto exportado executam fora da CatalyseR;
- a bateria automática termina com status zero.
