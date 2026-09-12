# Preparo no relatório e no Projeto R — 11/09/2026

A conferência encontrou e corrigiu uma diferença entre os exportadores. A ANOVA já gerava a receita da importação, mas o exportador geral não recebia essas escolhas e usava uma fotografia da base para as mudanças estruturais, deixando seu código apenas como comentário.

Agora ambos usam o mesmo gerador das escolhas de importação. O caminho geral também executa as reestruturações registradas na sequência, antes dos tratamentos compartilhados. O script e os chunks do relatório recebem essa receita, e cada derivada parte da compartilhada. A planilha Excel original é preservada também no caminho geral.

Antes de gerar o projeto, a IDE compara o preparo reconstruído com a compartilhada e as derivadas das análises incluídas. Divergências interrompem a exportação. Uma sequência estrutural atual que falhar não é substituída silenciosamente pela fotografia. Tratamentos sem gerador de código também provocam erro, em vez de desaparecerem do script.

## Conferência realizada

- Quatro projetos ZIP: exportador geral e ANOVA, cada um com exclusão e preservação da coluna original na separação.
- Arquivos extraídos do ZIP executados desde a planilha: seleção de colunas, recodificação, conversão de tipo, filtros da importação, renomeação, separar → empilhar → alargar, reescala, renomeação da compartilhada, filtro e renomeação da derivada.
- Resultado do script e do relatório igual ao da IDE em cada caso: compartilhada com 7 linhas, derivada com 5; valores, colunas e nomes conferidos. Não existe fotografia estrutural nesses ZIPs.
- Etapa desativada não executada; sequência estrutural quebrada e cache divergente da derivada impedem exportar; tratamento desconhecido não é omitido.
- Comparação binária do Excel original com o arquivo incluído em cada ZIP.
- Testes existentes de preparo (65/46 linhas), saídas da interface (43/58), bases derivadas, comunicação e exportador geral concluíram suas verificações. O teste do exportador geral também executou os chunks do relatório completo fora do Quarto.
- O novo teste `test_preparo_comunicacao_completo.R` foi incluído na suíte.

## Limites e pendências anteriores

Registros legados que não contêm a sequência estrutural executável continuam usando a base salva e o código original como referência comentada. Essa limitação fica explícita no projeto. As reestruturações da interface atual possuem a sequência e foram verificadas sem essa dependência.

Não foi realizada nova renderização final de Word/HTML; esta conferência executou o código dos arquivos exportados. O código do preparo fica no script e no documento-fonte do relatório; a apresentação no Word continua seguindo as opções do modelo.

O R deste ambiente continua encerrando com status 1 mesmo após os marcadores de conclusão dos testes. Por isso, os resultados acima se apoiam nas asserções executadas e nos marcadores finais, sem declarar a suíte inteira aprovada.

O teste antigo `test_anova_exportacao.R` falha ao exigir a frase literal “a análise passo a passo” no script. A mesma falha foi reproduzida na versão original, antes destas alterações. Esse teste não foi alterado nesta revisão.
