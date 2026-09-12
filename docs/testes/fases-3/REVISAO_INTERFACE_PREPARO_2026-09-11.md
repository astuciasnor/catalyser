# Interface e saídas do preparo — 11/09/2026

Implementação baseada nos testes do usuário (teste_transformações.docx) e no pedido de avaliar interface e saída juntas. As sugestões da outra IA foram consideradas como referência, sem mudar políticas estatísticas automaticamente.

## Alterações

- Derivadas em cinco sub-abas: Criar e gerenciar; Adicionar preparo; Finalizar/reabrir; Dados preparados; Código R. Criar e reabrir levam ao editor; os dados podem ser consultados durante o preparo.
- Nome e estado da base no topo; Compartilhada apresentada como origem na gestão. IDs novos derivada_01, derivada_02 etc. Registros existentes continuam compatíveis.
- Seletores principais com lista fora do painel, evitando cortes; largura maior para formulários.
- Histórico compartilhado em lista compacta, com estados Ativa/Inativa e explicação de desativar/remover.
- Aviso de mudança pendente junto aos controles de variáveis.
- Tabelas de preparo paginadas em 10 linhas, com total de colunas e rótulos em português. A prévia derivada não corta mais a base em 300 linhas.
- Excel da derivada selecionada e script completo desde o arquivo importado; saída desatualizada bloqueada até Recalcular. Finalizar e Reabrir respeitam o estado.
- Reestruturar preserva resultado e código para download após incorporar a mudança; uma origem diferente limpa a saída antiga. Fragmentos estruturais são encadeados para executar empilhar → alargar sem reimportar entre as operações.
- Cálculos e regras de transformação preservados. Não foram redistribuídos tratamentos estatísticos entre compartilhada e derivadas.

## Verificações

- Testes existentes: test_refinamentos_preparo.R, test_menu_preparando_dados.R, test_bases_derivadas.R, test_pivotar_organizar.R e test_exportacao_preparo.R concluíram seus pontos de conferência.
- Novo test_interface_saidas_preparo.R: Excel e execução do R antes/depois de incorporar Empilhar (12 × 4), Alargar (4 × 5) e Separar (71 × 12); limpeza da saída ao trocar origem; derivadas independentes com 43 e 58 linhas; IDs; Excel, código completo e bloqueio de saída desatualizada.
- Navegador Edge em sessão de teste: importação de biometria, menus abertos, remoção de duplicatas, g → kg, histórico compacto, criação/filtro/recalcular/finalizar/reabrir, cinco abas, tabela 43 × 11 e downloads reais de .xlsx/.R. Sem erros JavaScript ou mensagens de erro nas saídas verificadas. Inspeção visual em 1440 × 1000 e 1366 × 768.
- Projeto ANOVA: as verificações existentes reproduziram 65 e 46 linhas no script e no relatório, com os mesmos coeficientes do modelo. Essas contagens pertencem ao teste de exportação anterior, não ao roteiro manual de 68/43/58.
- Limitação do ambiente: Rscript retorna código 1 ao encerrar mesmo quando todas as asserções concluem; o comportamento também ocorreu num comando mínimo que apenas carrega shiny e imprime OK. Por isso foram registrados os marcadores de conclusão, separadamente do código de saída do processo. Nenhuma nova renderização HTML/Word foi executada nesta rodada.

## Repetir a verificação

A partir de inst/app, configure a localização antes de ler os arquivos UTF-8:

```r
Sys.setlocale("LC_ALL", "English_United States.utf8")
source("tests/test_interface_saidas_preparo.R", encoding = "UTF-8")
```

O roteiro para a avaliação manual da interface já aplicada está em ROTEIRO_CURTO_INTERFACE_PREPARO_2026-09-11.md.
