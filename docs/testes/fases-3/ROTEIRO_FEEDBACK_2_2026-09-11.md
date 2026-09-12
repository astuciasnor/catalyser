# Preparar Dados — conferir a segunda revisão

Reabra a CatalyseR pelo run.R da pasta atual do projeto. Use seu tamanho habitual de janela e zoom de 100%. Faça primeiro os cinco pontos abaixo; depois repita as transformações do roteiro anterior.

1. **Criar e gerenciar, sem derivadas.** Importe biometria e abra Preparar Bases Derivadas. A mensagem de base vazia deve ter espaço próprio. O seletor de derivadas e os botões de editar/excluir só aparecem quando houver uma base. Abra **Conferir dados da Base Compartilhada**: espere 71 linhas e 10 colunas.
2. **Renomear na compartilhada.** Remova duplicatas (68 linhas), crie peso_kg a partir de peso_g com prefixo k e renomeie para massa_kg. Ao confirmar o diálogo, procure o aviso **Mudança pendente**. Adicione a etapa uma vez. O aviso deve desaparecer e a base deve continuar com 68 × 11. Um segundo clique não deve repetir a renomeação.
3. **Ajustar a derivada antes de finalizar.** Crie Pesos acima de 100 g, filtre peso_g > 100 e recalcule: 43 × 11. Em **3. Finalizar/reabrir**, use **Variáveis e categorias**, à esquerda, para renomear massa_kg para peso_final_kg. Adicione a etapa: o resultado é recalculado. Confira os 43 registros, finalize e baixe Excel e R. A compartilhada deve continuar com massa_kg; somente esta derivada usa peso_final_kg.
4. **Conferir o histórico estrutural.** Em outra sessão, importe biometria, separe amostra por _ em local_amostra e periodo, mantendo a original, e incorpore a mudança. Em **Preparar Base Compartilhada → Etapas do Preparo**, a separação deve aparecer no bloco de reestruturação, antes dos tratamentos. Espere 71 × 12. Essas operações estruturais mantêm sua ordem própria.
5. **Espaço e leitura.** Capture Criar e gerenciar, o aviso de renomeação e Finalizar/reabrir. Envie um print da janela inteira, sem recortar, e diga o zoom utilizado. Registre onde houve rolagem desnecessária, texto pequeno, opção cortada ou dúvida sobre o próximo clique.

Para cada ponto, basta escrever **claro**, **exigiu procura** ou **impediu**, seguido de uma frase. Um resultado correto e difícil de encontrar ainda merece ajuste.

## Sobre o R exportado

As seleções que mantêm todas as colunas na mesma ordem e as conversões para um tipo que a coluna já possui deixam de aparecer no código gerado por novas importações. Mudanças reais continuam registradas.

O projeto ainda lê a planilha original e reproduz o preparo até chegar à base compartilhada. Começar diretamente de uma fotografia dessa base exigiria trocar também a entrada do projeto; apagar apenas as linhas deixaria o resultado incompleto.

Os nomes dados, dados_arrumados, base_compartilhada e dados_analise são objetos na memória do R, não arquivos diferentes. Eles identificam o trabalho em andamento, o resultado da reestruturação, a origem das derivadas e a entrada usada pelas análises. Mantê-los não cria novas planilhas.

O editor final reutiliza as mesmas operações da compartilhada, mas salva uma etapa na receita da derivada. As restrições já existentes para receitas terminadas por agrupamento ou contingência permanecem: ajuste essas variáveis antes da etapa redutora.
