# ANOVA de um fator após a revisão do preparo

Base inspecionada: CatalyseR 0.1.7, `main`, commit `8c911b4`, com os ajustes
locais de preparo e exportação de 11/09 preservados. Esta rodada não altera
a ANOVA de dois fatores nem reestrutura o exportador geral.

## Alterações

- A tela orienta a baixar o Projeto R em Comunicação de Resultados e gerar
  HTML/Word pelo Render no RStudio. Mensagens de validação usam os nomes
  atuais de Preparar Base Compartilhada, Variáveis e categorias e Limpeza.
- O preparo do modelo ANOVA mantém a resposta numérica recebida, sem passar
  novamente por texto. Tipo incompatível interrompe o código com orientação.
- `base_compartilhada` preserva o preparo comum; `base_da_anova` preserva a
  base escolhida antes da exclusão dos casos incompletos; `dados` contém os
  casos efetivamente analisados. A contagem de exclusões continua no texto.
- O README explica a sequência executável da importação, reestruturação,
  tratamentos e derivada. A fotografia estrutural fica descrita apenas como
  recurso para registros antigos sem sequência executável.
- A classificação de eta quadrado inclui “muito pequeno” abaixo de 0,01,
  como na interface. O script de estudo recebe também a mesma frase de
  conclusão da ANOVA já existente no relatório.
- O HTML acrescenta distância de Cook, com orientação para investigar a
  influência sem excluir observações automaticamente. Esse bloco permanece
  dentro da seção exclusiva do HTML.

Foi preservado o modelo autossuficiente de ANOVA do commit `229abec`: código
editável no QMD, script comentado para estudo e dependências do CRAN. Esta
rodada não introduz sincronização no modelo nem muda o critério de 5% dos
testes e das letras de Tukey.

## Verificações

`test_anova_integrada.R` concluiu as asserções de cálculo, validações,
benchmarks, execução explícita, pendência ao mudar parâmetros/base e registro.

`test_anova_exportacao.R` tinha uma falha anterior de capitalização em uma
frase esperada. A expectativa foi corrigida de “a análise” para “A análise”.
O teste então concluiu suas asserções e a execução do relatório misto.

`test_preparo_comunicacao_completo.R` concluiu as asserções dos quatro ZIPs:
caminho geral e ANOVA, mantendo/removendo a coluna original. Foram conferidos
importação, separar/empilhar/alargar, reescala, renomes, etapa desativada,
derivada e bloqueios por divergência da receita.

O novo `test_anova_preparo_projeto.R`, incluído na suíte, extrai e executa
script e chunks do ZIP. No exemplo de tilápias, compara 19 linhas na base
compartilhada, 18 na derivada e 17 casos completos. Base, F, p-valor e Tukey
com confiança de 99% coincidem com `calcular_anova()`; resumo e frase de
conclusão coincidem entre script e QMD.

## Limite da homologação

Os marcadores de conclusão das asserções não equivalem a processos encerrados
com sucesso: o ambiente apresenta saída 1 ou demora no encerramento do R,
problema também observado antes desta rodada. Não foi declarada aprovação
integral da suíte, e não houve build/check do pacote.

O Render HTML executou os 29 passos até produzir `relatorio.knit.md`, mas o
R encerrou com código `-1073741819`, impedindo concluir HTML/Word. O mesmo
ocorreu com R 4.6.1 e R 4.6.0 usando os pacotes já instalados. As tentativas
usaram variáveis de ambiente temporárias, sem alterar a configuração do usuário.
Não foi possível realizar a conferência visual final dos documentos.

Projeto de teste local: `APOIO/saida-sandbox/anova-2026-09-12/anova_preparo/`,
na pasta-mãe do ecossistema; ZIP ao lado. É uma saída de validação, não um
exemplo novo do pacote ou do livro.

Para a homologação final, reiniciar a CatalyseR pelo `run.R`, executar uma
ANOVA com a base preparada, registrar, exportar e abrir o projeto no RStudio.
Reiniciar o R e usar Render; conferir Word e HTML quando o problema de
encerramento do ambiente estiver resolvido. Alterações locais sem commit/push.
