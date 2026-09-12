# Segunda revisão do preparo — 11/09/2026

Feedback: teste_transformações2.docx. Ajustes aplicados à interface existente e ao código exportado.

- Gestão vazia com mensagem própria, sem sobrepor botões. Seletor e ações de editar/excluir aparecem após criar a primeira derivada.
- Consulta paginada da Base Compartilhada no início da gestão.
- Aviso explícito de renomeação pendente. Ajustes são limpos imediatamente após a inclusão aceita, impedindo repetir a mesma etapa por outro clique.
- Editor de nomes, tipos, categorias e seleção na coluna esquerda de Finalizar/reabrir. Reutiliza o módulo existente, salva na receita da derivada e recalcula. Finalizar impede a perda de ajustes pendentes. Bases finalizadas precisam ser reabertas para edição.
- Botões de finalização na coluna do resumo, visíveis sem rolar a janela de 768 pixels de altura nas larguras verificadas.
- Reestruturações incorporadas aparecem no histórico antes dos tratamentos, com seus rótulos e código consultável.
- Novas importações registram classes e colunas originais. O exportador omite seleção e conversão que não alteram a entrada; mudanças reais permanecem reproduzíveis.

Verificações concluídas: teste integrado de saídas de preparo; teste de bases derivadas; teste de exportação do preparo; teste novo test_feedback_preparo_2.R. Conferidas as saídas de empilhar/alargar/separar, bases de 43/58 linhas e projetos de 65/46 linhas, além da equivalência entre script e relatório verificada pelo teste existente.

Navegador Edge: criação, filtro, recálculo, renomeação na derivada em Finalizar/reabrir, aviso pendente, inclusão, finalização, download e reabertura. Excel baixado e execução do R comparados: 43 × 11, com peso_final_kg. Sem erros JavaScript ou erros visíveis nas saídas verificadas. Capturas em 1920, 1366 e 1024 pixels; conferência final do botão Finalizar dentro da janela em todas as três larguras, com altura de 768 pixels. A avaliação humana do conforto e da clareza continua no roteiro da segunda revisão.

Limite da verificação: nenhuma nova renderização Word/HTML foi feita. O ambiente continua retornando código de saída 1 do Rscript após os marcadores de conclusão e as asserções; os testes de navegador encerraram com código 0. Não foi alterada a regra de etapa redutora final (agrupamento/contingência).

## Acabamento visual — terceiro feedback

Aplicado a partir de teste_transformações3.docx: etapas das derivadas em faixas finas com fundo discreto; controles de variáveis permanecem visíveis, desativados, após finalizar; Adicionar etapa cinza e desativado sem ajustes, azul com pendência, voltando ao cinza ao salvar; grupos da compartilhada em Limpeza, Cálculos e transformações, Variáveis e categorias.

Conferência no navegador: estados do botão sem/com pendência, salvar, finalizar mantendo os quatro controles visíveis e desativados, reabrir habilitando-os novamente e inspeção das faixas das etapas. Sem erros visíveis. O teste existente de renomeação sem duplicar etapa e simplificação do código concluiu seus marcadores. Nenhuma alteração nos cálculos.

Decisão do usuário: manter o identificador técnico e detalhar a operação no rótulo. Aplicado: reescala mostra o divisor real (ex.: peso_g → peso_kg, ÷ 1.000); organização explicita renomes, tipos, recodificações e seleção; preenchimento de NA por constante mostra o valor; padronização mostra a fórmula. Rótulos conferidos para divisores 1.000, 0,001 e 1.000.000. Alterações restritas às descrições.

## Reestruturação e acesso ao Excel — quarto feedback

Botão de incorporar a reestruturação desativado após confirmar, em cinza-escuro; a prévia é consumida e os downloads preservados. Rótulos estruturais descrevem colunas de origem e destino e informam se a original foi mantida ou removida; apresentados em faixas com o fundo das etapas. Download da Base Compartilhada movido para o cabeçalho, acessível nas três abas. Grupos das derivadas ordenados em Limpeza, Recortes e resumos, Cálculos e transformações.

Teste integrado de saídas concluiu as verificações de empilhar, alargar, separar e derivadas 43/58. Inspeção visual confirmou incorporação desativada, separação amostra → local_amostra e estacao com remoção da original e botão de download no topo da janela.
