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
* **Análise de Variância (ANOVA):** ANOVA de um ou múltiplos fatores com testes de comparações múltiplas (Tukey, etc.).
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

Ou, pela interface do RStudio: baixe o arquivo `instalar_catalyser.R`, abra-o e clique em **Source** (canto superior direito do editor). Pode rodar de novo quando quiser — ele só reinstala o que faltar e reabre a IDE.

> **Menu Mapas (opcional):** exige os pacotes `sf` e `geobr`, que dependem de bibliotecas de fonte. No Windows pode ser preciso instalar o [Rtools](https://cran.r-project.org/bin/windows/Rtools/) antes. As demais análises da CatalyseR **não** precisam desses pacotes.

---

## 💻 Como Executar

Após a instalação, para iniciar a interface gráfica da IDE Científica no seu navegador, basta carregar o pacote e rodar a função `run_app()`:

```r
library(catalyser)

# Iniciar o aplicativo
run_app()
```

---

## 📄 Licença

Este projeto está licenciado sob a **Licença MIT** - consulte o arquivo [LICENSE.md](LICENSE.md) para obter mais detalhes.
