# CatalyseR 0.1.14 — entrega da versão unificada

Em 02/10/2026, o trabalho da Fase 2 foi mesclado com as melhorias da pasta principal. A pasta de trabalho passa a ser **D:/Claude/EAPA-Ecossistema/catalyser**, na branch **main**. O commit 14eb607 preserva as mudanças da principal anteriores à mesclagem; a integração preserva também o histórico da branch molde-fase2.

## O que foi reunido

A versão mantém os menus reorganizados, exploração dos dados, planejamento, preparo e módulos associados da principal. Reúne os três testes t (uma amostra, independentes e pareado), ANOVA de um fator clássica/Tukey e Welch/Games-Howell, regressão linear global ou por categoria, e os Projetos R com cálculos, HTML e Word. A opção Student/Welch, a hipótese e a confiança escolhidas no painel são preservadas na exportação.

O gráfico de médias segue o padrão aprovado pelo autor: barras estreitas transparentes a partir do zero, observações individuais, losango na média, hastes de IC bilateral e média ± DP em negrito junto ao topo da barra, com fundo totalmente transparente. A regressão por categoria mantém equação e R² próprios de cada ajuste, quando solicitados, e a tabela correspondente no projeto exportado.

A comparação dos arquivos confirmou que a principal unificada conserva o conteúdo da Fase 2. Além da nova versão, a diferença intencional é manter as quatro remoções feitas pelo autor: mod_contingency, mod_frequencia, mod_leaflet e mod_pizza, já sem ligação nos menus. Os arquivos anteriores ficam preservados no histórico e nas cópias de segurança.

## O que foi verificado nesta entrega

A instalação do pacote unificado em biblioteca local de validação terminou com código 0. Os painéis foram exercitados na pasta principal: R² global e por categoria, código recalculável, IC das médias e o padrão gráfico. As camadas da ANOVA no painel e no projeto exportado foram comparadas.

Sete projetos novos — ANOVA clássica, ANOVA Welch, t independente, t de uma amostra, t pareado, regressão global e regressão por categoria — produziram **14 documentos novos, HTML e Word**, com código 0 e sem referências quebradas. Os cálculos foram comparados com referências independentes do R nos caminhos indicados nos scripts de prova. Dados de categoria criados apenas para a prova não constituem um novo exemplo científico para o livro.

A suíte completa desta versão teve o resultado: **33 de 37 arquivos passaram; 4 falharam**. As quatro falhas anteriores estão nos arquivos de verificação da interface descritiva, estados/PCA, interface logística e ficha da ANOVA mista. Elas não foram ocultadas nem contadas como aprovação; esta entrega não certifica todos os módulos da IDE. As provas dos três grupos de análises que estamos fechando foram aprovadas separadamente.

## Limites e próximos ciclos

Esta entrega encerra o ciclo atual de integração e comunicação de resultados do teste t, ANOVA de um fator e regressão linear. A aprovação visual e didática do autor foi registrada. A revisão científica de um estudo específico e a conferência de uma instalação em outro computador continuam necessárias. O instalador Windows acompanha a fonte fixa desta versão; macOS permanece para etapa posterior.

Na regressão por categoria, os diagnósticos globais estão identificados e cada ajuste de categoria continua exigindo avaliação de seus pressupostos. O gráfico não testa a igualdade de inclinações. A rota de múltiplas análises conserva seu contrato anterior. As passagens de planejamento ainda obedecem às limitações documentadas na arquitetura; não declaramos uma validação completa de todo o ecossistema.

ANOVA de dois fatores e os demais módulos ficam para os próximos ciclos solicitados pelo autor, tomando o nível destas análises como referência e recebendo suas novas anotações de melhorias. O livro não foi migrado nesta entrega.

## Segurança e rastreabilidade

Cópias anteriores à mesclagem: APOIO/temp/mesclagem_20261002/principal_antes_da_mesclagem.zip, principal_alteracoes.patch e fase2_a8d4c78.zip. A branch molde-fase2 é preservada no Git mesmo depois de retirar o checkout extra. A pasta _backup_menus_20260929 foi arquivada com as cópias de segurança, fora da pasta de distribuição. O checkout CATALYSER_fase2 foi retirado após o autor salvar e fechar o RStudio; .Rhistory e .Rproj.user foram preservados em fase2_arquivos_locais.zip.

Evidências desta rodada: APOIO/temp/mesclagem_20261002/instalar_principal.log, suite_principal.log, projetos_principal.log e comparacao_final_fase2.json. Os sete projetos renderizados estão no caminho registrado em APOIO/temp/provas_integracao_ultimo.txt. Os registros anteriores permanecem como histórico, com seus caminhos originais.

O kit de alunos é APOIO/CatalyseR-Windows-0.1.14.zip. Sua fonte corresponde a uma revisão fixa da main; a revisão é indicada no LEIA_PRIMEIRO e no nome do ZIP interno. Dependências baixadas na instalação podem mudar e ainda precisam da conferência no computador de destino.
