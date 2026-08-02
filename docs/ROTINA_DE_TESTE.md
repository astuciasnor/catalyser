# Rotina de teste da CatalyseR

Como rodar a suíte e trazer o resultado para a conversa, com o mínimo de
atrito. Vale para o Windows 11 do autor — que é o ambiente de teste oficial do
projeto, conforme `DECISOES_PEDAGOGICAS_E_REFINAMENTO.md` §10.

## O ciclo, em uma linha

Duplo clique em **`rodar-testes-catalyser.bat`** (raiz de `D:\Claude\eapa`),
escolha o modo, espere. Depois é só dizer "rodei".

O `.bat` grava dois arquivos:

| Arquivo | Para quê |
|---|---|
| `ultimo-resultado.txt` | veredito curto: data, modo, PASSOU/FALHOU e as linhas de erro |
| `saida-testes.txt` | log completo |

O resumo existe para que a primeira leitura seja barata e para evitar o pior
erro possível: interpretar um log antigo como se fosse da rodada nova. A data
está sempre no topo.

## Os três modos

| Modo | Quando usar |
|---|---|
| **1 — Testes completos** | o de todo dia, depois de qualquer mudança |
| **2 — Só diagnóstico** | máquina nova, ou quando algo "sumiu" (Quarto, pacote) |
| **3 — Modo estrito** | antes de homologar: lacuna de ambiente vira falha |

Também funciona sem menu, útil para atalhos:

```powershell
.\rodar-testes-catalyser.bat 1
.\rodar-testes-catalyser.bat 2
.\rodar-testes-catalyser.bat 3
```

## Pelo terminal, se preferir

Da pasta `catalyser`:

```bash
"C:/R/R-4.6.1/bin/Rscript.exe" inst/app/tests/run_tests.R > ../saida-testes.txt 2>&1
```

O `run_tests.R` se localiza sozinho, então a pasta de onde você chama não
importa — mas o destino do log importa: precisa cair em `D:\Claude\eapa\`, que
é a pasta compartilhada.

## Pelo RStudio

Com `catalyser.Rproj` aberto, no Console:

```r
saida <- system2("C:/R/R-4.6.1/bin/Rscript.exe",
                 "inst/app/tests/run_tests.R",
                 stdout = TRUE, stderr = TRUE)
writeLines(saida, "D:/Claude/eapa/saida-testes.txt")
cat(saida, sep = "\n")
```

Não use `source()` direto no Console: o script dispara cada teste como processo
separado, e o `sink()` não captura a saída desses filhos — você ficaria sem log.

## Depois de mudanças grandes

Antes de commitar, feche o ciclo com build e check:

```bash
cd D:/Claude/eapa
"C:/R/R-4.6.1/bin/R.exe" CMD build catalyser > saida-check.txt 2>&1
"C:/R/R-4.6.1/bin/R.exe" CMD check --no-manual catalyser_0.1.6.tar.gz >> saida-check.txt 2>&1
```

Aceite apenas a nota conhecida — pacotes do `Imports` que o check não vê porque
são usados dentro de `inst/app/`. Qualquer outra nota merece investigação.

## O que essa rotina não cobre

A suíte protege contra **regressões nossas**. Ela não valida instalação em
máquina limpa: roda onde tudo já está instalado. O diagnóstico embutido reduz o
buraco — avisa quando algo falta e recusa passar em modo estrito — mas não
substitui uma instalação do zero de vez em quando, em VM.

Ver `INSTALACAO_AMBIENTE.md`.
