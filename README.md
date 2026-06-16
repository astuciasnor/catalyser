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

Como o **CatalyseR** está estruturado como um pacote R, você pode instalá-lo diretamente do GitHub executando os seguintes comandos no console do R/RStudio:

```r
# Instalar o pacote remotes (caso ainda não possua)
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# Instalar o CatalyseR a partir do GitHub
remotes::install_github("astuciasnor/catalyser")
```

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
