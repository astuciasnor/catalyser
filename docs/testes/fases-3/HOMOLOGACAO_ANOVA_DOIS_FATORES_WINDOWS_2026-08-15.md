# Homologação da ANOVA de dois fatores no Windows

- **Data:** 15/08/2026
- **Sistema:** Windows 11
- **Branch:** `main`
- **Commit:** `7be780b` (`feat: adiciona ANOVA de dois fatores`)
- **Versão da CatalyseR:** 0.1.7
- **R:** 4.6.1
- **Quarto:** 1.9.38
- **Conjunto:** `salvelino_formalina_remocao`
- **Situação:** **aprovada no cálculo, replay e Word**; permanecem divergências
  de importação e apresentação na interface.

## 1. Pré-condição automática

Com `LC_ALL=Portuguese_Brazil.utf8`, foi executado:

```powershell
& "C:\R\R-4.6.1\bin\Rscript.exe" inst/app/tests/run_tests.R
```

Resultado:

- ambiente completo: 27 de 27 pacotes, Quarto e dados de teste;
- sintaxe aprovada nos 73 arquivos R;
- 15 de 15 arquivos de teste aprovados;
- `test_anova_dois_fatores.R`, `test_anova_exportacao.R` e
  `test_exportacao_comunicacao.R` aprovados.

Este resultado confirma o núcleo analítico, mas não cobre adequadamente o
conteúdo efetivamente escrito no `.docx`.

## 2. Percurso manual executado

### 2.1. Importação

O objeto `EAPADados::salvelino_formalina_remocao` existe e possui 30 linhas e
7 colunas. Entretanto, em **Importando Dados → Pacote EAPADados**, o seletor da
IDE ofereceu apenas `artemia`.

Para prosseguir, foi carregado o mesmo conjunto curado pelo arquivo:

```text
EAPADados/data-raw/curados/salvelino_formalina_remocao.csv
```

**Divergência D1 — bloqueadora do roteiro pelo pacote:** o conjunto real não
aparece no seletor de datasets do EAPADados.

### 2.2. Configuração da ANOVA

- resposta: `sobrevivencia_eclosao_pct`;
- fator A: `formalin`;
- fator B: `remocao_semanal`;
- confiança: 95%;
- base: `dados_analise`.

Antes do clique, a IDE exibiu corretamente “Configure a análise e clique em
Executar análise”. Nenhum resultado foi calculado automaticamente.

### 2.3. Resultado analítico

O núcleo foi aprovado:

| Verificação | Resultado |
|---|---:|
| observações completas | 30 |
| excluídas | 0 |
| células | 12, 8, 3 e 7 |
| delineamento | desequilibrado |
| F de `formalin` | 1,504 |
| p de `formalin` | 0,2310 |
| F de `remocao_semanal` | 10,439 |
| p de `remocao_semanal` | 0,00334 |
| F da interação | 0,110 |
| p da interação | 0,74325 |
| Shapiro-Wilk, W | 0,84454 |
| Shapiro-Wilk, p | 0,0004776 |
| Levene, F | 5,0158 |
| Levene, p | 0,007086 |

O gráfico de interação, a tabela de pressupostos, os dois gráficos de
diagnóstico e o console bruto foram produzidos na interface.

## 3. Divergências da interface

### D2 — narrativa com construção gramatical quebrada

Texto observado:

> O modelo fatorial encontrou não há evidência suficiente de interação.

O gerador precisa escolher uma construção completa, por exemplo “não encontrou
evidência suficiente de interação” ou “indicou que não há evidência suficiente
de interação”. O mesmo texto defeituoso chega ao Word.

### D3 — tabelas não seguem a convenção decimal em português

As tabelas da tela mostraram valores como `4.34`, `1.50`, `0.23` e `0.00`.
Além de usar ponto decimal, o p de `remocao_semanal` foi arredondado para
`0.00`, ocultando o valor informativo `0,00334`.

Também foram exibidos `NA` nas células não aplicáveis da tabela da ANOVA, em
vez de um marcador editorial como `—`.

### D4 — título personalizado da execução não foi preservado

Foi digitado:

```text
Sobrevivência à eclosão: formalina × remoção semanal
```

Após **Adicionar Novo Resultado**, `execucao_0001` recebeu o título automático:

```text
ANOVA de dois fatores: sobrevivencia_eclosao_pct por formalin e remocao_semanal
```

## 4. Comunicação de Resultados

A execução foi reconhecida como atualizada. Foram marcados os oito componentes:

- narrativa;
- médias por célula;
- tabela;
- tamanhos de efeito;
- comparações múltiplas;
- gráfico;
- pressupostos;
- diagnósticos.

A tela confirmou **1 no Word / 1 no Projeto R** e todas as dependências
atualizadas.

## 5. Exportações

Foram gerados:

```text
saida-sandbox/homologacao-anova2-2026-08-15/
├── relatorio_salvelino_formalina_remocao_2026-08-15.docx
├── projeto_salvelino_formalina_remocao_2026-08-15.zip
└── projeto/projeto_salvelino_formalina_remocao/
```

### 5.1. Projeto R — aprovado no replay

Os scripts `R/01_base_compartilhada.R` e
`R/02_execucao_01_anova_dois_fatores.R` foram executados em ordem. A base foi
reconstruída e conferida como idêntica à fotografia: 30 linhas e 7 colunas.
F, p-valores, Tukey, Shapiro-Wilk e Levene foram reproduzidos.

O `relatorio.qmd` foi renderizado novamente com sucesso pelo Quarto e gerou
`relatorio.docx`.

### 5.2. Word — aprovado na inspeção visual do usuário

O Word baixado diretamente da IDE e o Word regenerado pelo Projeto R foram
abertos no Word e comparados visualmente pelo usuário. As tabelas e o gráfico
de interação apareceram, e os resultados dos dois documentos foram iguais.

A conversão anterior do `.docx` para texto puro por Pandoc não exibiu os
objetos `flextable` nem a imagem do gráfico. Essa limitação da extração textual
foi inicialmente interpretada como ausência no documento; a inspeção visual
no Word demonstrou que essa conclusão era falsa.

## 6. Interface em 1366 × 768

O painel da ANOVA foi inspecionado com viewport de 1366 × 768. Configuração,
abas, tabela de pressupostos e gráficos de diagnóstico permanecem acessíveis;
não foi observado corte horizontal. A página usa rolagem vertical normal.

## 7. Verificação visual do Word e instalação limpa

O renderizador de DOCX não pôde gerar PNGs porque `soffice`/LibreOffice não está
instalado nesta máquina. A verificação foi concluída diretamente no Microsoft
Word pelo usuário, que confirmou tabelas, gráfico e igualdade dos resultados.

A instalação limpa da CatalyseR e os testes posteriores à instalação não foram
executados nesta rodada; ficaram explicitamente adiados.

## 8. Veredito e ordem recomendada de correção

1. **D1:** fazer o catálogo da IDE listar os datasets reais documentados do
   EAPADados;
2. **D2/D3:** corrigir narrativa e formatação numérica em português;
3. **D4:** preservar o título digitado no registro da execução;
4. executar futuramente a instalação limpa e repetir o roteiro pós-instalação.

A ANOVA de dois fatores está **aprovada no cálculo, no replay e na entrega
Word**. As divergências D1–D4 permanecem como refinamentos da interface.
