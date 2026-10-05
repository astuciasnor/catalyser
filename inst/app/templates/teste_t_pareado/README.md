# {{TITULO}}

Este Projeto R mostra o percurso do teste t pareado: dados, exploração,
teste, diagnóstico e comunicação. Abra **{{PROJETO_RPROJ}}** no RStudio.

## O que você encontra

`R/analise.R` é a fonte dos cálculos, com comentários para estudar em ordem.
`R/funcoes.R` define a apresentação. Dois QMDs em `relatorios/` executam o
mesmo script: `relatorio_completo.qmd` gera o caderno HTML e
`relatorio_artigo.qmd` gera o Word. Os CSVs e PNGs de `saida/` são cópias para
compartilhar; os QMDs usam os objetos recém-calculados na memória.

`dados/brutos/` guarda a entrada intacta; `dados/processados/` guarda a base
adotada. `imagens/` recebe fotos ou esquemas do pesquisador.
`saida/tabelas/`, `saida/figuras/` e `saida/relatorios/` são regeneráveis.

| No script R | No relatório | Cópia salva para compartilhar |
|---|---|---|
| `tabela_t_exibir` | `flextable_ocean(tabela_t_exibir)` | `saida/tabelas/teste_t.csv` |
| `grafico_principal` | `grafico_principal` | `saida/figuras/principal.png` |
| `texto_teste` | expressão R inline | HTML e Word |

## Preparar o computador, uma vez

Instale R e RStudio com Quarto. No console do R:

```r
install.packages(
{{PACOTES_INSTALAR}}
)
remotes::install_github("astuciasnor/EAPADados")
remotes::install_github("astuciasnor/catalyser")
```

Nenhum pacote é instalado automaticamente. Os dois pacotes do ecossistema
são exigidos mesmo com dados locais; o script usa `catalyser_conferir_base`
para conferir o preparo. EAPADados é carregado por compatibilidade.

## Gerar e adaptar os documentos

Abra o .Rproj, reinicie o R e clique em Render em cada QMD. Para mudar
cálculos, edite `R/analise.R`; para interpretar o fenômeno, edite os textos
dos QMDs. Preserve a planilha original. Não use `quarto render --no-execute`:
os objetos precisam ser calculados e, sem isso, o relatório para com
`object '<nome>' not found`.

As escolhas de alternativa e confiança são preservadas. IC do teste unilateral
tem uma ponta infinita; os ICs descritivo e do efeito são bilaterais. DP é
dispersão; EP e IC descrevem incerteza. O poder observado não comprova H0
e não substitui planejar a amostra. Não há teste de Levene neste desenho.

No pareado, cada linha contém as duas medidas da mesma unidade. O par inteiro
é excluído se faltar uma medida. A diferença é medida 1 menos medida 2 e seu
DP é o denominador de d_z. Verifique o pareamento real antes de analisar:
uma ordem coincidente de linhas não demonstra que duas planilhas estejam pareadas.
As trajetórias preservam `linha_original`. Shapiro-Wilk avalia as diferenças.

## Origem dos dados

Registre aqui a origem da planilha, a licença e o período de coleta.
A CatalyseR não declara a proveniência no lugar do pesquisador.
Entrada preservada: `dados/brutos/{{ARQUIVO_BRUTO}}`.

## Ambiente computacional

{{AMBIENTE_COMPUTACIONAL}}

O Render registra versões e revisões disponíveis em `saida/sessionInfo.txt`, sem fixar versões. Uma revisão desconhecida aparece
como “não registrado”. APA é o estilo bibliográfico fornecido.

Para gerar os dois documentos, abra cada QMD de `relatorios/` no RStudio
e clique em Render. Cada relatório executa novamente a análise.
