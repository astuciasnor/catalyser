# Instalação do ambiente da CatalyseR (Windows)

Receita para uma máquina limpa — VM de teste ou computador novo. Ao final,
um único comando diz se o ambiente está completo.

## Resposta curta: a ordem importa?

**Não.** Instalar o Quarto antes ou depois do RStudio dá no mesmo. O que
importa é outra coisa:

1. instalar o **Quarto CLI autônomo**, e não depender só do que vem no RStudio;
2. **reabrir** terminal e RStudio depois de instalar o Quarto.

O motivo é que o RStudio traz um Quarto embutido em
`...\RStudio\resources\app\bin\quarto\bin`, que ele usa internamente mas **não
coloca no PATH** — é um [problema conhecido](https://github.com/rstudio/rstudio/issues/15127).
Daí vem a armadilha mais confusa: o relatório renderiza quando você clica em
Render no RStudio e falha quando a CatalyseR chama o Quarto pelo PATH. O
sintoma parece um bug da IDE, mas é ambiente.

O instalador autônomo do Quarto, esse sim, adiciona ao PATH — só que a mudança
vale apenas para terminais e programas abertos **depois** da instalação. Por
isso o "reabrir" é parte da receita, não um detalhe.

## Passo a passo

### 1. R (obrigatório)

Versão mínima: a declarada em `Depends` no `DESCRIPTION` (hoje **R >= 4.3.0**).

Baixe de <https://cran.r-project.org/bin/windows/base/>.

### 2. RStudio (recomendado)

Baixe de <https://posit.co/download/rstudio-desktop/>.

Não é obrigatório — a CatalyseR roda de qualquer sessão R — mas é o ambiente
que os alunos usam e onde o Projeto R exportado é aberto.

### 3. Quarto CLI (necessário para o relatório em Word)

Baixe de <https://quarto.org/docs/get-started/>.

**Depois de instalar, feche e reabra o terminal e o RStudio.** Sem isso o
PATH da sessão antiga continua sem o Quarto.

Sem Quarto, todas as análises funcionam; só a exportação do `.docx` não.

### 4. Rtools (opcional)

Baixe de <https://cran.r-project.org/bin/windows/Rtools/>.

Só é necessário para **compilar pacotes da fonte**. O
`instalar_dependencias.R` tenta binários primeiro justamente para que a
instalação típica não dependa de Rtools. Instale se algum pacote não tiver
binário para a sua versão do R.

### 5. Pacotes R

Na pasta do pacote `catalyser`, no Console do R:

```r
source("instalar_dependencias.R")
```

O script lê a lista do `DESCRIPTION` — não há lista paralela para desatualizar —
instala o que falta, prefere binários, resolve o `EAPADados` pelo GitHub (ele
não está no CRAN) e informa o estado do Quarto.

Se preferir a linha única, sem o script:

```r
install.packages(c(
  "shiny", "later", "bslib", "ggplot2", "DT", "readxl", "readr",
  "markdown", "zip", "writexl", "flextable", "ggpubr", "tibble",
  "stringr", "dplyr", "tidyr", "tidyselect", "scales", "cowplot",
  "ggrepel", "car", "emmeans", "effectsize", "rstatix", "rcompanion",
  "vistributions"
), type = "binary")
remotes::install_github("astuciasnor/EAPADados")   # não está no CRAN
```

> São 26 do CRAN mais o `EAPADados` = os 27 do `Imports`. Prefira o script:
> esta lista é uma cópia e cópias envelhecem.

### 6. Conferir

```powershell
cd <pasta do catalyser>
& "C:\R\R-4.6.1\bin\Rscript.exe" inst\app\tests\run_tests.R --diagnostico
```

O diagnóstico mostra quatro blocos — R, pacotes, Quarto, dados de teste — e
fecha com um veredito: **COMPLETO**, **UTILIZÁVEL, COM LACUNAS** ou
**IMPEDITIVO**. Cada problema vem com a solução ao lado, incluindo o comando
pronto para colar.

### 7. Homologar

Quando o veredito for COMPLETO, rode a suíte inteira em modo estrito, que
transforma qualquer lacuna de ambiente em falha:

```powershell
& "C:\R\R-4.6.1\bin\Rscript.exe" inst\app\tests\run_tests.R --estrito
```

## Instalação por linha de comando (opcional)

Se preferir automatizar com o `winget`, o caminho mais direto passa pelo
[`Posit.rig`](https://winget.run/pkg/RStudio), gerenciador que instala R,
RStudio e Rtools:

```powershell
winget install Posit.rig
rig install R
rig system rtools add
winget install Posit.RStudio
```

O Quarto continua sendo instalação à parte, pelo site. Os IDs do winget mudam
com o tempo — se algum falhar, use os downloads da seção anterior, que são a
via oficial.

## Problemas comuns

| Sintoma | Causa provável | Solução |
|---|---|---|
| `Word não gerado`, mas Render funciona no RStudio | só o Quarto embutido do RStudio existe | instale o Quarto CLI autônomo |
| Quarto instalado e o diagnóstico não acha | terminal aberto antes da instalação | feche e reabra o terminal |
| Diagnóstico acha o Quarto "fora do PATH" | é o embutido do RStudio | use o comando `QUARTO_PATH` que o próprio diagnóstico imprime |
| Pacote "instalado mas falha ao carregar" | instalação corrompida ou sessão de R segurando o arquivo | feche todas as sessões de R e reinstale |
| `install.packages` pede compilação | não há binário para a sua versão do R | instale o Rtools, ou use um R para o qual exista binário |
| `EAPADados` não instala | não está no CRAN | `remotes::install_github("astuciasnor/EAPADados")` |

## Testar uma instalação do zero

Para saber se a receita acima funciona numa máquina virgem, há três caminhos —
em ordem de custo:

1. **Windows Sandbox.** Abre um Windows limpo em segundos e descarta tudo ao
   fechar. É o ambiente virgem mais barato que existe. Exige edição **Pro,
   Enterprise ou Education** — não existe no Home — mais virtualização ativada
   no BIOS, 4 GB de RAM e 2 núcleos.

   Na raiz do projeto:

   ```powershell
   .\verificar-sandbox.ps1     # diz se dá, e o que falta
   ```

   Se faltar apenas ativar o recurso, num PowerShell **como Administrador**:

   ```powershell
   Enable-WindowsOptionalFeature -FeatureName "Containers-DisposableClientVM" -All -Online
   ```

   Depois reinicie e clique duas vezes em **`sandbox-catalyser.wsb`**. Ele abre
   o Sandbox já com duas pastas na Área de Trabalho: `transferencia`
   (somente leitura, com o ZIP do pacote) e `saida` (leitura e escrita, o único
   lugar cujo conteúdo sobrevive ao fechar a janela).

   Dentro do Sandbox não há R, RStudio nem Quarto — é exatamente o cenário que
   você quer testar. Baixe e instale seguindo esta receita, e grave o log do
   diagnóstico em `saida`.
2. **Snapshot de VM.** Tire a foto antes, instale, teste, restaure.
3. **Limpar a máquina real.** Só se não houver alternativa: você perde a
   biblioteca de pacotes, o histórico e as configurações do RStudio.

Para o terceiro caso existe `limpar-ambiente-r.ps1`, na raiz do projeto. Ele é
**seguro por padrão**: sem argumentos, apenas mostra o que faria.

```powershell
.\limpar-ambiente-r.ps1                    # simulação
.\limpar-ambiente-r.ps1 -Confirmar         # executa
.\limpar-ambiente-r.ps1 -Confirmar -IncluirRegistro   # + chaves R-core
```

Antes de apagar qualquer coisa ele grava um inventário — lista de pacotes
instalados, PATH e variáveis de ambiente — em `backup-ambiente-r/`. Guarde essa
pasta: é com ela que você reinstala o que tinha.

O script **não desinstala programas**. Desinstalador interrompido no meio deixa
a máquina pior do que estava, então essa parte fica com o Windows: primeiro
desinstale R, RStudio, Rtools e Quarto por *Configurações → Aplicativos*, e só
então rode o script para varrer as sobras — bibliotecas de pacotes, cache,
configurações do RStudio, entradas de PATH e variáveis de ambiente.

Depois, **feche e reabra o terminal** e confirme:

```powershell
where.exe R
where.exe Rscript
where.exe quarto
```

Os três precisam responder que não encontraram nada.

## Por que não um instalador `.exe`

Decisão registrada do projeto: a CatalyseR é **100% R** e roda a partir do
RStudio. Um executável esconderia o ambiente do aluno, que é justamente o que
o curso quer tornar visível.
