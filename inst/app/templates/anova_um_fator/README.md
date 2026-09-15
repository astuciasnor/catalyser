# Projeto de análise: ANOVA de um fator

Uma planilha entra, um documento reúne a análise e dois formatos saem: o Word
para o leitor e o caderno HTML para o pesquisador. Este projeto foi exportado
pela CatalyseR e roda no RStudio com R, Quarto, `catalyser`, `EAPADados` e os
pacotes de leitura, preparo e análise indicados no trecho `instalar` do script.

## Por onde começar

1. Abra `projeto.Rproj` no RStudio e, em seguida, `relatorios/relatorio.qmd`.
2. No primeiro uso, execute manualmente as linhas do chunk `instalar` para
   instalar os pacotes que faltam. Ele tem `eval: false` e não roda no Render.
3. Reinicie a sessão R. Na seta ao lado de **Render**, escolha **Render HTML**
   para gerar o caderno; depois escolha **Render Word** para gerar o relatório.
   Cada escolha atualiza somente aquele formato, na pasta `relatorios/`.
4. Confira o título, o subtítulo e os autores preenchidos em Comunicação de
   Resultados. Eles são exportados no início do `.qmd`, onde também podem ser
   editados. Complete as afiliações e o resumo conforme seu estudo.
   Revise Introdução, Métodos, Discussão e Conclusão conforme seu estudo.
   Leia os resultados e ajuste o texto ao seu estudo. Para mudar o código da
   análise, edite e salve `R/analise.R`, execute o chunk `atualizar` do relatório
   e renderize novamente.

Você precisa ter R (4.3 ou posterior), RStudio e Quarto instalados. A instalação
inicial requer internet: `catalyser` e `EAPADados` vêm do GitHub; os demais
pacotes vêm do CRAN. O trecho `instalar` inclui `dplyr`, `tidyr`, `ggplot2`,
`stringr`, `purrr`, `lubridate`, `readxl` e as dependências de análise e saída.
Execute-o antes do primeiro Render. Depois de instalar ou atualizar pacotes,
reinicie o R. O Render executa a análise; ele não instala pacotes.

## O que fica em cada lugar

```text
projeto/
  projeto.Rproj
  README.md
  dados/
    brutos/
      {{PLANILHA}}
    processados/
      base_compartilhada.rds
      base_compartilhada.xlsx
      base_0001.rds             quando uma base derivada foi utilizada
      base_<nome>.xlsx          quando uma base derivada foi utilizada
  R/
    analise.R
    funcoes.R
  imagens/
  relatorios/
    relatorio.qmd
    abnt.csl
    referencias.bib
    custom-reference.docx
    ocean.scss
```

**`dados/brutos/`** guarda somente os valores da aba `{{ABA}}`, utilizada de
`{{ARQUIVO_ORIGEM}}`, em um Excel com uma aba. O arquivo original com todas
as abas permanece com você. Preserve a entrada exportada como veio; o
preparo fica registrado no código. **`dados/processados/`** guarda a base
tratada exportada pela CatalyseR, em RDS e Excel. Esses arquivos são cópias da
base no momento da exportação; editar o código não os atualiza automaticamente.
O RDS preserva os tipos do R e fornece os dados ao relatório. Cada derivada
utilizada também recebe seu RDS, identificado por `base_0001.rds`, por exemplo.
Os Excel permitem consultar a compartilhada e as derivadas fora do R.
O Render lê a base já preparada e refaz a análise, sem repetir os tratamentos.

**`R/analise.R` é a fonte do código e o material de estudo.** Cada trecho tem
um nome, como `## ---- analisar ----`, e comentários que explicam o que fazer
e o que conferir. É aqui que você experimenta, estuda e edita o código.

**`relatorios/relatorio.qmd` reúne o texto científico e o código do script.**
Os chunks já vêm preenchidos e podem ser executados linha a linha no RStudio.
Você edita o texto e as legendas aqui. Para mudar o código, volte ao script;
depois de salvá-lo, rode o chunk `atualizar` (Ctrl+Shift+Enter). Ele copia os
trechos indicados por `# fonte:`, sem as linhas de comentário, preservando o
texto e as opções dos chunks. Por exemplo, `# fonte: analisar-tukey` recebe
as comparações entre os grupos. Modelo, pressupostos, Tukey e texto têm chunks
separados para facilitar a execução por etapas.

As letras de Tukey usam a forma curta `multcompLetters4()`, que exige nomes
de grupos sem hífen. Se houver um nome como `A-sequeiro`, o script pede que
você o ajuste no preparo antes de continuar. Cabeçalhos de colunas com
espaços continuam permitidos.

Os trechos `importar` e `tratar` ficam somente no script, para estudar e conferir
o preparo. Se alterar essa receita, execute-a, confira os dados e use as linhas
comentadas de `saveRDS()` logo antes de `carregar-bases` para adotar a mudança.
Atualize a compartilhada e a derivada utilizada, quando houver. Só depois rode
o relatório. `atualizar_codigo()` copia código; não atualiza dados salvos.
Os Excel continuam representando a exportação original.

No Render, `conferir_codigo()` interrompe a geração se o código do relatório
estiver diferente do script e indica quais trechos precisam de atualização.
Rode `atualizar` e tente novamente. Mudanças apenas nos comentários do script
não exigem atualização do relatório.

**`R/funcoes.R`** define os ajudantes de apresentação, como resumos, tabelas,
cores e formatação de números, e as funções `atualizar_codigo()` e
`conferir_codigo()`. Carregar esse arquivo não executa a análise.

**`imagens/`** começa vazia e recebe suas fotos, esquemas e mapas. Os gráficos
estatísticos são gerados pelo código durante o Render. Para inserir uma foto,
guarde-a nessa pasta e acrescente ao `.qmd`, por exemplo:

```markdown
![Local de coleta.](../imagens/local_coleta.jpg){#fig-local-coleta}
```

## O que conferir antes de compartilhar

Compare o número de observações e os resultados com a análise registrada na
CatalyseR. Os filtros, a tipagem e os tratamentos aparecem como código no
preparo. O nível de confiança escolhido é exportado para os intervalos das
médias e do Tukey; o critério dos testes e das letras permanece em 5%.

As letras indicam comparações entre grupos, não uma classificação biológica.
Grupos que compartilham uma letra não tiveram diferença detectada pelo Tukey.
Leia o tamanho de efeito e os pressupostos junto com o contexto da coleta.

O trecho de preparo do script refaz, na ordem, as escolhas da importação, as reestruturações
(separar, empilhar ou alargar), os tratamentos da base compartilhada e o preparo
da base derivada escolhida. Etapas desativadas não executam. A derivada parte
da compartilhada, sem repetir seus tratamentos.

No script, `base_compartilhada` guarda o preparo comum. No relatório,
`base_da_anova` guarda a base lida do RDS antes da exclusão dos casos incompletos. `dados` contém os
casos completos usados no teste. O relatório informa quantas linhas ficaram de
fora; a resposta mantém o tipo numérico definido no preparo.

Somente registros antigos sem sequência executável podem precisar de
`dados/processados/base_resolvida.rds`. Nesse caso, o relatório informa a
limitação e preserva as operações antigas como comentários. Essa cópia é
anterior aos tratamentos; alterar a planilha original não a atualiza.
{{DESCRICAO_PREPARO}}

Não é necessário salvar e importar novamente os dados para baixar o projeto.
O exportador continua conferindo se a análise recebe os mesmos dados da IDE.

## Um relatório, duas saídas

Na seta do Render, gere `relatorio.html` e `relatorio.docx` separadamente.
O Word apresenta o texto, as tabelas e as figuras dos resultados. O HTML acrescenta o
código recolhido, a exploração e os diagnósticos, com orientações para ler os
gráficos e os testes dos pressupostos.

Na figura final, os rótulos ao lado dos pontos mostram média ± desvio padrão.
As barras mostram o intervalo de confiança da média; DP e IC têm significados
diferentes, por isso a legenda identifica ambos.

Edite o texto no `.qmd` e o código no script. As saídas Word e HTML
podem ser geradas novamente a partir dele. As referências ficam em
`referencias.bib`, com citações no padrão ABNT definido por `abnt.csl`;
`custom-reference.docx` define a apresentação do Word e `ocean.scss`, a do HTML.

Os dois formatos estão definidos no início do `.qmd`. O HTML é um arquivo
autônomo para compartilhar; o Word é entregue separadamente. Quando mudar
o texto ou atualizar os códigos, renderize novamente os dois formatos se
quiser manter ambos em dia.
