# CatalyseR 0.1.18 — caderno completo da análise combinada

Data: 02/10/2026.

## Problema e correção

O molde isolado de teste t já tinha boxplot, resíduos, Q-Q e letras no gráfico de médias. A rota de várias análises usava outro motor, com menos elementos. Trocar o seu boxplot principal por barras não completou os objetos de estudo do HTML; as letras também não estavam implementadas nesse motor. A correção atua nessa diferença de rotas.

O teste t independente reconstruível agora fornece, além da figura de barras, boxplot com pontos, resíduos versus médias ajustadas, Q-Q por grupo e dispersão dos resíduos absolutos por grupo. A figura de barras usa letras determinadas pelo p-valor do teste escolhido e pelo alfa correspondente à confiança selecionada; elas não vêm da sobreposição dos ICs. O maior valor médio recebe a quando há evidência de diferença; sem evidência, os dois grupos recebem a. As barras conservam largura 0,30, transparência 0,22, pontos, IC bilateral de cada média e rótulo de média ± DP sem fundo.

A regressão reconstruível fornece resíduos versus ajustados, Q-Q padronizado, gráfico de dispersão para homocedasticidade e distância de Cook. Para regressões por categoria, cada painel corresponde ao seu próprio modelo. Esses gráficos sinalizam problemas a investigar; não removem observações nem alteram o método escolhido. Os diagnósticos do modelo global na rota nativa também entram no HTML completo, mesmo quando não foram selecionados para Word.

O exportador inclui os gráficos de estudo exclusivamente no HTML. O Word continua com uma figura de resultados do teste t e uma de regressão quando as duas análises estão selecionadas. Os gráficos agora usam dimensão de 6 × 4 polegadas na rota combinada, com títulos dos eixos de tamanho 10 e rótulos de média ± DP de tamanho 3,2. Isso evita a ampliação desproporcional observada na figura menor anterior.

Projetos integrados novos exigem catalyser >= 0.1.18. A instalação padrão foi atualizada; reinicie R antes de abrir a CatalyseR e gere o projeto em uma pasta vazia. Projetos baixados anteriormente não ganham os novos chunks apenas por atualizar o pacote.

## Verificação e limites

Um projeto novo com as configurações de teste t e regressão por tratamento usadas pelo autor renderizou HTML e Word com código final 0. O HTML incorpora dez figuras (duas de resultados e oito de estudo). O Word foi inspecionado como arquivo íntegro, contém apenas as duas figuras de resultados, e a figura de barras foi extraída para conferência visual de letras, transparência e fontes.

Os ICs do teste t foram comparados com chamadas diretas a t.test por grupo; o teste inferencial também foi comparado. Foram conferidas letras iguais em um exemplo sem diferença, letras diferentes no exemplo do autor, confiança de 99% e observações negativas. Os testes existentes test_funcoes_analise.R, test_exportacao_comunicacao.R e test_regressao_roteiro.R passaram.

Prova: APOIO/temp/prova_duas_0.1.18_130046/teste_t_e_regressao. Logs: APOIO/temp/validar_padrao_0.1.18.log e APOIO/temp/testes_foco_0.1.18.log. A suíte completa não foi repetida; o resultado anterior da 0.1.16 foi 33/37 após dois retestes e mantém quatro falhas antigas documentadas.

Esta correção não migra o código da rota combinada para o molde didático de cálculos nativos. Ela ainda usa funções catalyser e sincroniza chunks com o script. As funções didáticas experimentais não foram integradas, e o ajuste proposto de média ± DP ficou para revisão posterior.

Foi acrescentado à suíte o teste test_caderno_completo_duas_analises.R, que passou e verifica os objetos de estudo, as letras, os cálculos e os chunks exclusivos do HTML. A suíte passa a listar 38 arquivos; ela não foi executada integralmente nesta atualização.
