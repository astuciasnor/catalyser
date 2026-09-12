# CatalyseR — testar a nova interface de Preparar Dados

11/09/2026 · 8 testes · cerca de 25–35 minutos.

A interface deste roteiro já foi implementada. Feche a sessão anterior e reabra a CatalyseR pelo run.R do projeto. Use sua janela habitual, com zoom de 100%.

A trilha das derivadas agora é:
**1. Criar e gerenciar → 2. Adicionar preparo → 3. Finalizar/reabrir → 4. Dados preparados → 5. Código R.**

A seleção da base e seu estado ficam acima das abas. Você pode consultar dados e código antes de finalizar. Os cálculos foram preservados; esta rodada confere o caminho até o resultado e os arquivos baixados.

## 1. Importação e seletores

Importe **Treino-Transformacoes.xlsx** e escolha explicitamente a aba **biometria** (o importador pode selecionar guia inicialmente).

Espere **71 linhas × 10 colunas**. Abra Preparar Base Compartilhada e alterne Variáveis e categorias, Cálculos e transformações e Limpeza. Abra cada lista.

**Observe:** nomes completos, opções sem cortes, seleção fácil com mouse e teclado. Não é preciso repetir toda a limpeza de texto já aprovada.

## 2. Histórico, seleção e retorno

Na compartilhada, execute e adicione cada etapa:

- **Limpeza → Remover duplicatas**, deixando as colunas-chave vazias: **68 × 10**.
- **Cálculos e transformações → Reescalar unidades**: peso_g, prefixo manual k, nome peso_kg: **68 × 11**. Para id = 1, espere **0,601 kg**.
- **Variáveis e categorias → Renomear variáveis**: peso_kg para massa_kg. Confira o aviso de mudança pendente e adicione a etapa.
- Em **Selecionar variáveis**, retire só profundidade_m e adicione: **68 × 10**. Em Etapas do Preparo, desative essa etapa: **68 × 11**. Reative: **68 × 10**. Remova somente essa etapa para continuar com **68 × 11**.

**Observe:** histórico compacto, etapas fáceis de encontrar, diferenças claras entre desativar e remover; tabela com totais de linhas e colunas. Desativar mantém a etapa no histórico; remover retira a etapa.

## 3. Uma derivada, do começo ao resultado

Em **1. Criar e gerenciar**, crie **Pesos acima de 100 g**. Pode usar `base_pesos100` como nome no R. A primeira derivada recebe o ID `derivada_01`.

A criação abre **2. Adicionar preparo**. Escolha **Recortes e resumos → Filtrar linhas**: peso_g **> 100**. Adicione a etapa e clique em **Recalcular**.

Em **4. Dados preparados**, espere **43 × 11**. Volte a **3. Finalizar/reabrir**, confira o resumo e finalize. Reabra e confirme o retorno ao editor.

**Observe:** as cinco abas seguem uma sequência compreensível; nome e estado da base permanecem visíveis; coluna, operador e valor escolhidos estão corretos.

## 4. Duas bases independentes

Crie **Biometria completa**, nome R `base_completa`, a partir da compartilhada.

Em Limpeza → Tratar dados faltantes, escolha explicitamente **Remover as linhas** de peso_g. Adicione e recalcule: **62 linhas**. Repita para comprimento_cm: **58 × 11**. Finalize.

Alterne pelo seletor superior: Pesos acima de 100 g tem **43 linhas**; Biometria completa, **58**; a compartilhada continua com **68**.

**Observe:** nome, tabela e histórico acompanham a seleção. A segunda derivada nasce da compartilhada, mesmo que a primeira estivesse selecionada.

## 5. Estado, paginação e downloads

Na Biometria completa, confira **10 linhas por página**, busca e total **58 × 11**. Buscar um registro não muda permanentemente a base.

Na compartilhada, crie temporariamente peso_dobro com peso_g * 2. Volte à derivada: ela deve indicar que precisa de atualização. Recalcule: **58 × 12**. Remova essa última etapa da compartilhada e recalcule a derivada: **58 × 11**.

Em **3. Finalizar/reabrir**, baixe **Excel** e **sequência completa R**. Abra os arquivos: a planilha deve coincidir com a tabela; o código deve conter importação, preparo compartilhado e os dois tratamentos de ausentes. Guarde o Excel original junto ao script ou ajuste seu caminho de leitura.

**Observe:** avisos claros, Finalizar disponível apenas com resultado atualizado, downloads identificados pela base escolhida.

## 6. Reestruturar — Empilhar

Guarde os arquivos anteriores e abra uma sessão nova. Importe **desembarques_largo**: **4 × 4**.

Em Reestruturar Planilha → Empilhar:

- selecione as três colunas anuais de captura, mantendo porto;
- separador: hífen entre espaços ` - `;
- nomes descritivos: `ano, medida`;
- coluna dos valores: `captura_t`.

Aplique: **12 × 4**, com porto, ano, medida e captura_t. Soma das capturas: **13.660**.

Baixe Excel e R. Adicione a mudança à compartilhada e baixe novamente: a mensagem deve confirmar a incorporação e manter os downloads disponíveis. O R deve conter `pivot_longer`.

**Observe:** seleção múltipla legível, campos claros, prévia e saída persistentes depois da confirmação.

## 7. Reestruturar — Alargar

Continue na mesma sessão. Em Alargar, escolha **ano** para formar novas colunas e **captura_t** para os valores.

Aplique: **4 × 5** — porto, medida, 2022, 2023 e 2024. A soma continua **13.660**. A coluna medida permanece, por isso são cinco colunas.

Adicione à compartilhada e baixe Excel e R. O script deve conter **empilhamento e alargamento**, nessa ordem, sem reiniciar a leitura entre as duas operações.

**Observe:** as escolhas usam o resultado anterior; a tabela aproveita a largura; os downloads correspondem ao resultado alargado.

## 8. Reestruturar — Separar Colunas

Em sessão nova, importe biometria. Separe **amostra** por `_`, criando `local_amostra, periodo` e mantendo a coluna original.

Aplique: **71 × 12**. Caratateua_Seca deve gerar Caratateua e Seca. Adicione à compartilhada e baixe Excel e R; confira as duas colunas novas e o código de separação.

**Observe:** espaço para digitar nomes, legibilidade das opções e permanência das saídas após incorporar.

## Anotação rápida

Para cada teste, registre: **Claro / Exigiu procura / Impediu**, seguido de uma frase ou imagem.

Para encerrar esta rodada: menus sem cortes; base e estado claros; seleção e desfazer compreensíveis; contagens corretas; Excel e R disponíveis nas três reestruturações. Você avalia a experiência na sua janela habitual; a conferência automática dos cálculos e saídas já está registrada junto ao projeto.
