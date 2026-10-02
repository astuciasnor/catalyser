# Avaliação manual no Windows, 01/10/2026

Registro do retorno do autor após avaliar, em outro computador Windows, a
versão enviada no kit da revisão `67eaa4f`, branch `molde-fase2`.
Este registro não altera o código dessa revisão.

## O que o autor confirmou

- As opções de teste t de uma amostra e pareado apareceram. Um dos dois foi
  testado; o retorno não identifica qual deles.
- O teste t independente apresentou as duas opções, Student e Welch.
- A ANOVA apresentou as três opções de método.
- Depois de gerar o Projeto R da ANOVA, os documentos QMD renderizaram bem.

O relato não identifica os métodos ANOVA efetivamente executados, os dados,
as versões das dependências, os formatos renderizados nem os resultados
numéricos. Nenhum log foi anexado a esse retorno. Não inferir que todas as
combinações foram conferidas manualmente ou que os três testes t renderizaram.

## Evidência local anterior

Os cinco tipos de projeto geraram HTML e Word nos testes locais. A rodada
completa aprovou 31 de 37 arquivos; duas verificações antigas da narrativa
ANOVA foram corrigidas e passaram em execuções separadas. Assim, há 33
arquivos com aprovação registrada, sem nova rodada completa depois dessas
correções. Permanecem quatro falhas da outra frente: interface descritiva,
estados da PCA, interface logística e ficha da ANOVA mista.

O contrato do molde registra as escolhas de método, as provas e os limites
estatísticos, incluindo o efeito aproximado no caminho Welch. Os logs e
relatórios detalhados estão em `APOIO/temp/` do repositório-mãe e não fazem
parte do pacote instalado.

## Preparação para disponibilização

A versão está preparada para disponibilização aos alunos como versão de
avaliação. Os commits de implementação já estavam registrados; este commit
acrescenta somente documentação do retorno do autor. Não houve alteração
dos cálculos após o teste relatado, nem homologação adicional no macOS.

Ainda falta a revisão científica e didática do autor e a conferência manual
dos casos que ficaram sem testar. O instalador público busca a `main`:
publicar apenas `molde-fase2` não atualiza esse instalador. Até a integração,
a revisão pode ser instalada explicitamente pela branch publicada ou pelo
kit local, sem confundi-la com a versão anterior da `main`.

Não integrar automaticamente sobre a `main` local com alterações de outra
frente. Preservar esse trabalho e revisar as sobreposições antes do merge.
