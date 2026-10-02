# Transparência estatística e beleza dos dados

Padrão visual e didático aprovado pelo autor em 02/10/2026, após ajustes nos gráficos de médias do teste t e da ANOVA na CatalyseR Fase 2.

## Uma característica do ecossistema EAPA

**Transparência estatística e beleza dos dados** é uma característica do ecossistema EAPA. A apresentação deve ser agradável e permitir que o leitor enxergue a grandeza dos valores, a dispersão das observações e a incerteza das estimativas. A beleza ajuda a leitura; a escolha das escalas e dos elementos preserva a interpretação dos resultados.

Nos gráficos de comparação de médias, a barra mostra a dimensão de cada média a partir do zero. Os pontos permitem ver as observações individuais, incluindo a dispersão e valores afastados. O losango localiza a média. As hastes mostram o intervalo de confiança (IC) bilateral da média. O rótulo **média ± desvio padrão (DP)** informa a média e a dispersão amostral em números. São informações complementares: o IC trata da incerteza da média; o DP descreve a dispersão das observações.

Esse conjunto passa a integrar a identidade visual e didática da CatalyseR e a orientar sua relação com os Projetos R e com o livro EAPA. O avanço registrado é reunir comparação, observações, dispersão e incerteza em uma figura legível, com a aprovação do autor. A documentação não atribui exclusividade ou ineditismo científico ao formato.

## Padrão aprovado para gráficos de médias

- Barras estreitas e transparentes, com base em zero. Para valores positivos, retirar a folga inferior para que encostem no eixo X.
- Pontos individuais visíveis, com deslocamento horizontal apenas para reduzir sobreposição, sem alterar seus valores no eixo Y.
- Losango na média e hastes de IC bilateral da média, com pontas horizontais curtas.
- Rótulo **média ± DP** em negrito, próximo do losango, na mesma altura do topo da barra, com fundo totalmente transparente e sem borda.
- Cores coerentes com Ocean Gradient, contraste legível e legenda que explique cada elemento.
- Letras de comparação somente quando derivadas do teste ou pós-teste efetivamente aplicado, com explicação do seu significado.

A configuração atual usa largura da barra 0,30; transparência `alpha = 0.22`; largura das pontas do IC 0,08; rótulo com `nudge_x = 0.05`, `hjust = 0`, `vjust = 0.5`, `fontface = "bold"` e `fill = NA`. Esses valores documentam a implementação aprovada; ajustes para outras dimensões de figura devem preservar a leitura e o significado estatístico.

## Fidelidade dos resultados

O padrão não autoriza cortar observações, alterar limites de IC, omitir resultados ou escolher escalas para acentuar uma conclusão. Valores negativos e ICs que ultrapassem zero precisam continuar visíveis; a base da barra permanece em zero, mas o eixo deve comportar todos os valores. A sobreposição dos ICs não substitui o teste de comparação. As letras e o texto devem respeitar o método, o nível de confiança e a ordem dos grupos usados na análise.

Quando houver pontos sobrepostos, o deslocamento horizontal pode não revelar sozinho quantas observações coincidem. A legenda, o tamanho amostral e os dados disponíveis devem permitir compreender essa limitação. O gráfico favorece uma comunicação transparente, mas a interpretação também depende do delineamento, dos pressupostos e das decisões da análise.

## Alcance e continuidade

O padrão está implementado nos gráficos de médias do teste t independente e da ANOVA de um fator, e nos moldes exportados de ANOVA e dos três testes t. Ele deve ser seguido em futuras criações ou revisões de gráficos de médias que reúnam esses elementos. Gráficos de contagem, proporção, distribuição, regressão ou outros objetivos precisam de uma representação apropriada; não recebem automaticamente média, DP ou IC de média.

O livro e outros módulos podem incorporar o padrão quando forem revisados. Este registro não declara uma migração já realizada em todo o ecossistema. A aprovação do autor é visual e didática; não substitui as verificações estatísticas nem a homologação das instalações.

Implementação final do fundo transparente: commit `f91904d`, branch `molde-fase2`. Prova local: `APOIO/temp/prova_rotulo_transparente.log`, com `EXIT=0`, e figura `APOIO/temp/teste_t_rotulo_transparente.png`. Conferência de camadas da ANOVA no painel e no projeto exportado aprovada.
