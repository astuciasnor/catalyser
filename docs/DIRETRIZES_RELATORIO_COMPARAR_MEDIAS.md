# Diretrizes do relatório: comparar médias (ANOVA e comparações múltiplas)

Guia de redação e de escolha dos elementos do relatório Word (`relatorio.qmd`)
para as análises feitas com `comparar_medias()`, `grafico_medias()` e
`escrever_resultados()`. É o primeiro de uma família: cada grupo de funções da
ClaRa (comparar médias, comparar medianas, relacionar variáveis...) deve ganhar
um guia como este.

> **A forma de apresentar os resultados não é fixa.** O que vem abaixo são
> diretrizes, não regras de software. O relatório gerado traz todas as peças
> (tabela da ANOVA, tabela de médias e gráfico) para o pesquisador escolher. A
> decisão final é do aluno e do professor, conforme a revista, a banca, o
> número de variáveis-resposta e o que se quer comunicar. Quando uma diretriz
> for deixada de lado, que seja por decisão consciente, de preferência
> explicada numa frase.

## 1. A lógica da apresentação: do geral ao específico

A apresentação segue a ordem da inferência:

1. **Teste global (ANOVA):** há alguma diferença entre as médias dos grupos?
2. **Comparações múltiplas (Tukey, letras):** quais grupos diferem entre si?
3. **Estimativas (médias, dispersão e incerteza):** quanto vale cada média,
   quanto as observações variam e com que precisão a média foi estimada.

Em geral, as letras de agrupamento só são discutidas quando o teste *F* da
ANOVA é significativo (*p* < α). Com *F* não significativo, a conclusão é a
ausência de evidência de diferença; as letras, se aparecerem, servem só para
descrição (é o que `textos$comparacoes` já escreve nesse caso).

Com **dois grupos** (`comparar_medias()` escolhe o teste *t*), não há etapa 2:
o próprio teste diz se as duas médias diferem, e o resultado se relata com a
diferença entre as médias e o seu IC.

## 2. A ANOVA: no texto ou em tabela?

Não há forma única; o critério é a complexidade do modelo.

**Um fator (o caso da ClaRa hoje).** Muitas revistas dispensam a tabela da
ANOVA no corpo do artigo e pedem o resultado na própria frase:

> Houve efeito da ração sobre o peso final das tilápias (*F*(3, 20) = 8,42;
> *p* < 0,001; η² = 0,56).

Os números entre parênteses são os graus de liberdade do tratamento (3) e do
resíduo (20). É exatamente o formato que `textos$teste` e `textos$efeito`
produzem, com vírgula decimal. A tabela da ANOVA (`tbl-anova`) pode ficar no
Word, ir para um apêndice ou sair, a critério do pesquisador; teses e
relatórios técnicos costumam mantê-la.

**Modelos com mais de um fator** (fatorial, parcelas subdivididas, blocos com
restrições). Quando há fatores e interações (A, B e A × B), recomenda-se a
tabela da ANOVA completa, com fonte de variação (FV), graus de liberdade (GL),
soma de quadrados (SQ), quadrado médio (QM), *F* e *p*. Esse caso ainda não
está na rota ClaRa; a diretriz fica registrada para quando entrar.

**O tamanho de efeito acompanha o *F*.** O *p* diz se há evidência de
diferença; o η² (e o ω²) diz quanto da variação o fator explica. Relatar os
dois juntos é prática cada vez mais exigida.

## 3. Tabela de médias OU gráfico de médias?

### A regra de ouro dos periódicos: não duplicar

A maioria das revistas proíbe apresentar os mesmos dados em tabela e em
gráfico. O relatório gerado traz as duas formas por conveniência: **escolha
uma para o corpo do manuscrito**, e a outra sai ou vai para um apêndice.

Essa é a regra que mais se presta a ser quebrada de propósito: num relatório
de aula, numa tese ou num documento técnico, professor e aluno podem decidir
manter as duas, por exemplo para treinar a leitura de cada uma. O que não se
deve é duplicar por descuido num artigo submetido.

### Quando escolher a tabela de médias

- Quando há várias variáveis-resposta no mesmo estudo (peso final, ganho de
  peso, conversão alimentar, sobrevivência), que cabem numa tabela compacta.
- Quando o leitor precisa dos valores exatos: médias, DP, EP ou IC com as
  casas decimais.
- Quando há muitos grupos: acima de 8, `grafico_medias()` já avisa que a
  figura fica carregada e sugere a tabela.

### Quando escolher o gráfico de médias

- Quando o foco é a comunicação visual: contrastes, tendências e hierarquia
  entre os grupos.
- **A vantagem do gráfico da ClaRa:** ele já mostra o rótulo numérico
  **média ± DP** e as letras de Tukey ao lado de cada barra, e ainda os pontos
  das observações e o IC da média. Cumpre a função da tabela de médias com
  mais informação, o que torna a tabela dispensável no corpo do artigo.

## 4. Padrões de formatação

### Se a escolha for a tabela de médias

1. **Colunas:** grupo | n | média ± DP (ou ± EP) | IC 95% | letra. A letra
   pode vir numa coluna própria ou sobrescrita à média (9,8 ± 1,2ᵃ).
2. **Diga sempre o que vem depois do ±.** DP descreve a dispersão das
   observações; EP descreve a precisão da média. "Média ± 1,2" sem dizer qual
   dos dois é uma das falhas mais comuns em artigos.
3. **Nota de rodapé obrigatória**, declarando o teste e o nível de
   significância. Exemplo:

   > Médias seguidas pela mesma letra não diferem entre si pelo teste de Tukey
   > ao nível de 5% de significância (α = 0,05). DP: desvio padrão; IC:
   > intervalo de confiança de 95% da média.

### Se a escolha for o gráfico de médias

1. **Diga o que é a haste.** A legenda precisa declarar se a haste é DP, EP
   ou IC 95%. Na ClaRa, o padrão é o IC da média (`haste = "ic"`); `"ep"` e
   `"dp"` são as alternativas, e a legenda tem de acompanhar a escolha.
2. **Explique cada elemento da figura.** O gráfico da ClaRa tem cinco: barra
   (a média a partir do zero), pontos (as observações), losango (a média),
   hastes (o IC) e rótulo (média ± DP), mais as letras. Exemplo de legenda,
   já ajustado ao nosso gráfico:

   > **Figura 1.** Peso final de tilápias (g) por tipo de ração. As barras e
   > os losangos indicam a média de cada grupo; os pontos, as observações
   > individuais; as hastes, o intervalo de confiança de 95% da média. O
   > rótulo ao lado de cada losango mostra média ± desvio padrão (DP). Letras
   > iguais indicam grupos que não diferem pelo teste de Tukey (α = 0,05).

3. **Sobreposição dos ICs não é teste.** Dois ICs que se tocam podem vir de
   médias que diferem pelo Tukey, e vice-versa. Quem decide são as letras.
   Ver [PADRAO_GRAFICOS_MEDIAS.md](PADRAO_GRAFICOS_MEDIAS.md).

## 5. Modelo de redação para o aluno

> A análise de variância indicou efeito do tipo de ração sobre o peso final
> das tilápias (*F*(3, 20) = 12,31; *p* < 0,001; η² = 0,65). Pelo teste de
> Tukey, a ração R3 proporcionou o maior peso médio (215,4 ± 11,2 g),
> diferindo das demais (*p* < 0,05); as rações R1 e R2 não diferiram entre si
> (Figura 1 / Tabela 1).

Três cuidados nessa frase: o *F* vem com os dois graus de liberdade e o
tamanho de efeito; a média vem com o que segue o ± (aqui, DP) e a unidade; e
a figura **ou** a tabela é citada, não as duas. Com *F* não significativo, a
frase muda de natureza:

> Não houve evidência de diferença no peso final entre os tipos de ração
> (*F*(3, 20) = 1,12; *p* = 0,364).

e, se `textos$poder` aparecer, ele entra na Discussão, lembrando que ausência
de evidência não é evidência de ausência.

---

## Aplicação na rota ClaRa (`inst/app/templates/anova_clara/relatorio.qmd`)

Confronto feito em 08/10/2026 e aplicado no mesmo dia, testado com os dados
dos bagres (`ClaRa/dados_treino/real_bagre_racoes.rds`).

**Aplicado:**

- **Guia de edição no início do `.qmd`**, com o que se escreve à mão: título,
  autor e seções de texto; a frase dos Resultados sobre o maior grupo e os
  que não diferiram (as frases automáticas só listam os pares); a revisão
  das frases automáticas; a escolha entre tabela e figura; as casas decimais.
- **Ordem do geral ao específico:** a frase do *F* e a tabela da ANOVA vêm
  primeiro; depois as comparações, a tabela de médias e a figura.
- **Tabela da ANOVA mantida**, com um comentário dizendo que ela pode sair
  num artigo de um fator e que está ali porque relatórios e teses costumam
  trazê-la.
- **Tabela de médias no formato da seção 4:** grupo | n | média ± DP com a
  letra sobrescrita | IC, e nota de rodapé com o teste, o α e as siglas.
- **Comentário "tabela de médias OU figura"**, deixando a escolha ao
  pesquisador.
- **`casas <- 1`** no chunk `preparar`, usado pela tabela e pelo rótulo da
  figura (e `casas = 1` no `R/analise.R`, para os dois baterem).
- **Legenda da figura e nota da tabela geradas a partir da `confianca`**
  (`legenda_figura`, `nota_tabela`; `fig-cap: !expr legenda_figura`).
- **Lembrete visível nos Resultados**, entre colchetes, para a frase que o
  aluno escreve.

**Pendente, para decidir:**

| Ponto | Situação | Sugestão |
|-------|----------|----------|
| Letras com *F* não significativo | As letras vêm sempre de `multcompLetters4()`, e a tabela e a figura as mostram mesmo com *F* n.s. O Tukey não depende do *F*, então as letras podem até contradizer a ANOVA; só `textos$comparacoes` trata o caso. | Esconder as letras quando *p* ≥ α, ou mantê-las com uma nota "apenas descritivas". |
| Haste trocada | Se o aluno trocar `haste = "ep"`, a legenda continua dizendo IC. O comentário no `preparar` avisa. | Gerar a frase da haste a partir de uma variável `haste`, como já se faz com o nível. |
| Redação das frases automáticas | `textos$teste` usa o rótulo do eixo como substantivo ("Em Peso (g), ..."). | Na ClaRa, um `rotulo_texto` ("o peso final") separado do rótulo do eixo. |
| Posição da legenda | O Quarto põe a legenda abaixo da figura no Word; a ABNT (NBR 14724) pede a identificação acima. | Avaliar `fig-cap-location: top`, conforme a norma do curso. |
| Rótulo sobre os pontos | No gráfico deitado, o rótulo média ± DP encosta nos pontos do grupo. | Revisar o `nudge_x` no padrão de [PADRAO_GRAFICOS_MEDIAS.md](PADRAO_GRAFICOS_MEDIAS.md). |
