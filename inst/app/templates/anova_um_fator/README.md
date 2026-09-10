# Projeto de análise: ANOVA de um fator

Uma planilha entra, um documento reúne a análise e dois formatos saem: o Word
para o leitor e o caderno HTML para o pesquisador. Este projeto foi exportado
pela CatalyseR, mas roda com R, Quarto e pacotes do CRAN, sem depender da IDE.

## Por onde começar

1. Abra `projeto.Rproj` no RStudio e, em seguida, `relatorios/relatorio.qmd`.
2. No primeiro uso, execute manualmente as linhas do chunk `instalar` para
   instalar os pacotes que faltam. Ele tem `eval: false` e não roda no Render.
3. Reinicie a sessão R e clique em **Render**. O HTML é a saída principal,
   e o Word é atualizado automaticamente na mesma renderização. Aguarde o
   processo terminar e, no HTML, clique em **Outros formatos → MS Word**.
   Se o RStudio lembrar uma escolha anterior, selecione **Render HTML** na seta.
4. Confira o título, o subtítulo e os autores preenchidos em Comunicação de
   Resultados. Eles são exportados no início do `.qmd`, onde também podem ser
   editados. Complete as afiliações e o resumo conforme seu estudo.
   Revise Introdução, Métodos, Discussão e Conclusão conforme seu estudo.
   Leia os resultados e ajuste o texto ao seu estudo. Para mudar a análise,
   edite os chunks no próprio `relatorio.qmd` e renderize novamente.

Você precisa ter R, RStudio e Quarto instalados. A instalação inicial dos
pacotes requer conexão com a internet.

## O que fica em cada lugar

```text
projeto/
  projeto.Rproj
  README.md
  _quarto.yml
  dados/
    brutos/
      {{PLANILHA}}
    processados/
      dados_analise.rds
      base_compartilhada.xlsx
  R/
    analise.R
    funcoes.R
    gerar_word.R
  imagens/
  relatorios/
    relatorio.qmd
    abnt.csl
    referencias.bib
    custom-reference.docx
    ocean.scss
```

**`dados/brutos/`** guarda a planilha de entrada. Preserve-a como veio; o
preparo fica registrado no código. **`dados/processados/`** guarda a base
tratada exportada pela CatalyseR, em RDS e Excel. Esses arquivos são cópias da
base no momento da exportação; editar o relatório não os atualiza automaticamente.

**`relatorios/relatorio.qmd` é a fonte da verdade.** Ele reúne o texto e os
chunks que importam os dados, preparam a análise e produzem os resultados.

**`R/analise.R`** é uma cópia comentada para estudo e rascunho. Nesta versão,
não há sincronização automática: se experimentar uma mudança no script,
leve-a manualmente ao chunk correspondente do relatório. **`R/funcoes.R`**
contém apenas ajudantes de apresentação, como resumos, tabelas, cores e
formatação de números.

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

As edições de nomes, tipos e categorias são reproduzidas em R quando o código
registrado recompõe a base usada na análise. Outras reorganizações, como
empilhar tabelas, usam a cópia preparada em `dados/processados/base_resolvida.rds`.
O relatório informa quando usa essa cópia e mantém o registro das operações
como comentários no preparo. A planilha Excel original permanece em `dados/brutos/`.

A cópia preparada é anterior à trilha de tratamentos: os tratamentos e o preparo
da base derivada continuam sendo aplicados pelo código, uma única vez. Alterar o
Excel original não atualiza essa cópia; para refazer a reorganização, adapte o
código registrado no preparo ou gere um novo projeto pela CatalyseR.

Não é necessário salvar e importar novamente os dados para baixar o projeto.
O exportador continua conferindo se a análise recebe os mesmos dados da IDE.

## Um relatório, duas saídas

O Render gera `relatorio.html` e atualiza `relatorio.docx` em `relatorios/`.
O Word apresenta o texto, as tabelas e as figuras dos resultados. O HTML acrescenta o
código recolhido, a exploração e os diagnósticos, com orientações para ler os
gráficos e os testes dos pressupostos.

Edite o `.qmd` para manter o texto e os resultados juntos. As saídas Word e HTML
podem ser geradas novamente a partir dele. As referências ficam em
`referencias.bib`, com citações no padrão ABNT definido por `abnt.csl`;
`custom-reference.docx` define a apresentação do Word e `ocean.scss`, a do HTML.

A configuração `_quarto.yml` chama `R/gerar_word.R` depois do HTML para
atualizar o Word. Esses dois arquivos cuidam da geração dos documentos.
A análise continua no próprio `.qmd`. Pela seta do Render, você também pode
escolher apenas Word; nesse caso, o HTML não é atualizado.

Para compartilhar o HTML com o link do Word funcionando, envie os dois
arquivos juntos e mantenha-os na mesma pasta. O navegador poderá abrir ou
baixar o Word conforme suas configurações.
