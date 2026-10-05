# CatalyseR

<div style="display:flex;align-items:center;margin-bottom:1em">
<p style="font-size:1.2em;line-height:1.4;">
<strong>CatalyseR</strong> é uma IDE R Científica baseada em Shiny desenvolvida para facilitar a análise de dados estatísticos de forma interativa e visual. O aplicativo foi projetado para apoiar estudantes, professores e pesquisadores na aplicação de métodos bioestatísticos e análises multivariadas.
</p>
</div>

---

## 🚀 Funcionalidades Principais

A plataforma é dividida em módulos analíticos completos e independentes:

* **Estatística Descritiva:** Sumarização e exploração de variáveis, tabelas de frequência e geração de gráficos descritivos.
* **Testes Paramétricos:** Comparação de médias usando testes de hipótese (como o Teste t de Student).
* **Análise de Variância (ANOVA):** ANOVA de um fator e ANOVA de dois fatores com interação, comparações múltiplas de Tukey, tamanho de efeito e diagnósticos.
* **Regressão Linear:** Ajuste de modelos de regressão, diagnósticos de resíduos e visualização gráfica de ajuste.
* **Técnicas de Amostragem:** Ferramentas para determinação de tamanho amostral e seleção de amostras.
* **Análise Multivariada (PCA & HCA):**
  * **PCA:** Análise de Componentes Principais com gráficos de Biplot e contribuição de variáveis.
  * **HCA:** Análise de Agrupamento Hierárquico com dendrogramas customizáveis.
* **Tabelas de Contingência:** Testes de independência e medidas de associação (como o Teste Qui-Quadrado).

---

## 🛠️ Instalação

A forma **recomendada** instala tudo de uma vez — dados (EAPADados) e dependências, em **binário** (sem precisar de Rtools) — e abre a IDE ao final. No console do R/RStudio, rode:

```r
source("https://raw.githubusercontent.com/astuciasnor/catalyser/main/instalar_catalyser.R")
```

Ou, pela interface do RStudio: baixe o arquivo `instalar_catalyser.R`, abra-o e clique em **Source** (canto superior direito do editor). Pode rodar novamente quando quiser: o instalador atualiza a CatalyseR pela branch `main`, preserva os pacotes CRAN compatíveis e abre a IDE no navegador padrão. A instalação fica no computador; nas próximas sessões, basta executar `catalyser::run_app()`. É necessário ter R >= 4.3 instalado e internet para a instalação. Não é necessário instalar Git.

### Alternativa: instalação com `pak`

Quem já usa o `pak` pode instalar direto do GitHub. Ele resolve as dependências (inclusive o EAPADados) e usa pacotes binários do CRAN quando existem:

```r
install.packages("pak")
pak::pkg_install("astuciasnor/catalyser", upgrade = FALSE)
catalyser::run_app(launch.browser = TRUE)
```

Para instalar também os extras opcionais (mapas, hexágonos, teste de Nemenyi), troque a segunda linha por:

```r
pak::pkg_install("astuciasnor/catalyser", upgrade = FALSE, dependencies = TRUE)
```

A CatalyseR é escrita só em R e não precisa de Rtools. Se aparecer um aviso sobre o Rtools ao instalar a partir da pasta local (`install.packages(..., repos = NULL, type = "source")`), ele pode ser ignorado.

> **Menu Mapear e Analisar (opcional):** os mapas usam `sf`, `geobr` e `ggspatial`. No Windows e no macOS eles vêm em binário do CRAN; no Linux, `sf` pede bibliotecas do sistema (GDAL, GEOS, PROJ). As demais análises da CatalyseR **não** precisam desses pacotes.

---

## 💻 Como Executar

Após a instalação, para iniciar a IDE Científica diretamente no navegador
padrão:

```r
catalyser::run_app(launch.browser = TRUE)
```

Para iniciar o servidor sem abrir o navegador automaticamente:

```r
catalyser::run_app(launch.browser = FALSE)
```

Nesse caso, copie para o navegador o endereço local exibido no console.

---

## 📚 Documentação do projeto

No caderno HTML do teste t e da regressão linear, a exploração e a avaliação dos pressupostos aparecem antes dos resultados finais. O artigo Word conserva os componentes de resultados escolhidos, sem os gráficos de diagnóstico exclusivos do caderno.

Comece pelo [mapa da arquitetura](ARQUITETURA.md): ele localiza os módulos,
define os contratos entre telas e identifica as rotas de exportação legadas.

O [índice da documentação](docs/README.md) reúne as decisões vigentes, o
pipeline canônico, os roteiros de homologação, o histórico e a skill usada para
refinar duas análises por ciclo.

---

## ✅ Testes automatizados

A suíte oficial roda cada arquivo de teste em um processo R próprio e termina
com o resumo `X/Y OK`. Comando (a partir da raiz do pacote):

```r
Rscript inst/app/tests/run_tests.R
```

Opções: `--diagnostico` (só confere o ambiente) e `--estrito` (lacuna de
ambiente vira falha). A lista de arquivos vive no próprio `run_tests.R`; o
contrato do molde de Projeto R (CONTRATO_MOLDE_PROJETO_R.md, seção 9) descreve
o que a suíte cobre.

---

## 📄 Licença

Este projeto está licenciado sob a **Licença MIT** - consulte o arquivo [LICENSE.md](LICENSE.md) para obter mais detalhes.
