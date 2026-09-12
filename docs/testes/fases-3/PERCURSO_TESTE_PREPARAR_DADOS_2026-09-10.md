# Percurso de teste — Preparar Dados

**CatalyseR · Ecossistema EAPA · 10 de setembro de 2026**  
Referência da interface: revisão `8c911b4`. Planilha: `inst/app/dados/Treino-Transformacoes.xlsx`.

## O que vamos avaliar

Vamos acompanhar uma pessoa que chega com uma planilha razoavelmente organizada: cabeçalho único, colunas identificáveis e observações em linhas. Ela precisa acertar categorias, unidades, valores ausentes e alguns formatos antes de analisar.

O objetivo é verificar se você consegue preparar esses dados com segurança e sem esforço desnecessário. A variedade de ferramentas já parece compatível com esse escopo; este teste deve orientar ajustes de clareza e fluidez. A estimativa de atender mais de 95% dos usos permanece uma hipótese: uma planilha de treino não mede essa cobertura.

Este percurso usa os nomes atuais da interface. A aba `guia` do arquivo e o roteiro V15 contêm referências anteriores; use as instruções abaixo para conduzir esta conferência.

**Organização sugerida:** três encontros de 20–30 minutos, com o laboratório de transformações como complemento. Os tempos são estimativas. Faça primeiro o percurso; reserve os ajustes da interface para depois de registrar os atritos.

## Antes de começar

- [ ] Abra uma sessão nova da CatalyseR e use o tamanho de janela habitual, com zoom de 100%.
- [ ] Anote navegador, resolução/tamanho da janela e horário. Não ajuste a janela para esconder uma dificuldade de espaço.
- [ ] Reserve uma pasta para bases baixadas, códigos e registros do teste.
- [ ] Antes de ler as instruções detalhadas, procure por dois minutos onde importar, organizar categorias e criar uma base específica para uma análise. Anote onde procurou primeiro e onde hesitou.
- [ ] Confira os quatro destinos de **Preparar Dados**: **Importar Dados**, **Reestruturar Planilha**, **Preparar Base Compartilhada** e **Preparar Bases Derivadas**.

Durante o teste, registre duas coisas separadamente: **o resultado dos dados** e **a experiência para chegar até ele**. Um resultado correto pode ter sido difícil de obter.

> As contagens deste roteiro dependem da ordem indicada. Aqui removemos as duplicatas e preservamos os valores ausentes na base compartilhada. Portanto, não compare as contagens com testes anteriores que apenas removeram linhas com peso ausente.

## Percurso 1 — Uma base compartilhada pronta para trabalhar

Em **Importar Dados**, abra `Treino-Transformacoes.xlsx` e selecione **biometria**.

**Conferência inicial:** 71 linhas e 10 colunas. Existem 3 linhas integralmente duplicadas, 6 pesos ausentes e 5 comprimentos ausentes. As colunas são `id`, `especie`, `local`, `sexo`, `amostra`, `data_coleta`, `comprimento_cm`, `peso_g`, `cpue` e `profundidade_m`.

**Observe:** 

### 

Entre em **Preparar Base Compartilhada**. Em cada operação, configure os campos, clique em **Adicionar etapa do preparo** e confira o resultado antes de seguir.

| Passo | Operação | Resultado esperado |
|---|---|---|
| 2a | **Limpeza → Remover duplicatas**. Deixe as colunas-chave vazias para comparar a linha inteira. | 68 linhas; permanecem 6 pesos e 5 comprimentos ausentes. |
|  | **Padronizar texto** em `especie`: remover espaços extras; depois adicionar outra etapa para minúsculas. | 5 espécies. |
| 2c | Repetir as duas operações em `local`. | 3 locais. |

As espécies devem ser `corvina` (26), `sardinha` (15), `pescada amarela` (14), `pargo` (7) e `bagre` (6). Os locais devem ser `ajuruteua` (24), `caratateua` (23) e `braganca` (21).

**Observe:** a distinção entre grupo de ações e ação escolhida é clara? O formulário ocupa espaço proporcional ao que pede? Ao concluir, você consegue ver imediatamente o que mudou? Escolher apenas `especie` como chave de duplicação removeria observações legítimas: avalie se a interface ajuda a compreender essa escolha.

### 3. Organizar categorias e tipos

Em **Variáveis e categorias → Recodificar categorias**, trabalhe em `sexo`:

| Valores originais | Novo valor |
|---|---|
| `M`, `m`, `Macho` | `Macho` |
| `F`, `f`, `Femea` | `Fêmea` |

Confirme **Aplicar** no diálogo e depois **Adicionar etapa do preparo**. A base deve continuar com **68 linhas**, sendo **39 Macho e 29 Fêmea**.

Em **Definir tipos**, transforme `especie`, `local` e `sexo` em **Fator**. Confirme e adicione essa etapa separadamente. Confira que os rótulos permanecem legíveis e que as variáveis numéricas continuam numéricas.

**Observe:** fica claro que aplicar no diálogo prepara uma mudança e que adicionar a etapa a incorpora à base? Compare a prévia pendente com a **Base usada nas análises**. Se você concluir que já terminou quando ainda falta adicionar, registre essa ambiguidade.

A coluna `data_coleta` contém texto no formato dia/mês/ano. Mantenha-a como texto neste percurso; converter datas exige conferir a interpretação do formato. Não transforme uma leitura ambígua em resultado esperado.

### 4. Reescalar e renomear

Em **Cálculos e transformações → Reescalar unidades**, selecione `peso_g`, escolha o prefixo manual `k` e crie `peso_kg`. Adicione a etapa. Confira a divisão por mil: no registro `id = 1`, **601 g devem corresponder a 0,601 kg**. Os seis pesos ausentes continuam ausentes.

Em **Variáveis e categorias → Renomear variáveis**, renomeie somente `peso_kg` para `massa_kg`, confirme e adicione a etapa. O ponto de conferência agora é **68 linhas e 11 colunas**.

**Observe:** unidade e nome de saída estão próximos o bastante? O nome novo aparece na tabela, nas escolhas seguintes e no código? Você distingue uma coluna criada de uma coluna substituída?

### 5. Experimentar a seleção e desfazer com confiança

Em **Selecionar variáveis**, retire apenas `profundidade_m`, mantendo todas as demais. Confirme e adicione: espere **68 linhas e 10 colunas**.

Abra **Etapas do Preparo**, selecione essa última etapa e desative-a: a coluna deve voltar, chegando a **68 × 11**. Reative-a, confira a retirada e depois **remova somente essa etapa de seleção**, restaurando **68 × 11** para continuar o roteiro.

Escolha duas etapas independentes de padronização de texto, uma de `especie` e outra de `local`, e experimente **Subir/Descer**. O resultado final deve permanecer igual. Restaure a ordem para facilitar a leitura do histórico.

**Observe:** seleção da etapa, estado ativo e botões de ação são evidentes? A posição das sub-abas **Dados preparados**, **Etapas do Preparo** e **Código R** permanece previsível? Você precisa voltar à tabela para descobrir se a operação funcionou?

### 6. Guardar o primeiro ponto de conferência

Baixe a base preparada e as etapas em R. Confira **68 × 11**, os rótulos de sexo e `massa_kg` no arquivo. Leia o código procurando a mesma sequência realizada na tela; não é necessário programar para reconhecer as ações.

Use a busca da tabela para localizar um registro e depois limpe-a. A busca deve funcionar como consulta visual; confira se a interface evita confundi-la com uma filtragem permanente da base.

**Pausa de avaliação:** sem reler as instruções, descreva de onde vem a base compartilhada, quais alterações estão aplicadas e como desfazer uma delas.

## Percurso 2 — Bases diferentes para perguntas diferentes

Continue na mesma sessão, com a base compartilhada **68 × 11**. Em **Preparar Bases Derivadas**, abra **Criar uma base derivada**, preencha nome e finalidade compatível e use os nomes de código indicados abaixo.

Para cada operação, siga **Adicionar etapa do preparo → Recalcular → conferir**. Ao terminar uma base, use **Finalizar Preparo**. Todas as bases abaixo devem nascer da compartilhada, inclusive quando outra derivada estiver selecionada.

### 7. Recortar observações

Crie **Pesos acima de 100**, código `base_pesos100`. Em **Recortes e resumos → Filtrar linhas**, escolha `peso_g`, condição numérica, operador `>` e valor `100`.

**Esperado:** **43 linhas**. Pesos ausentes não satisfazem o filtro. Finalize, reabra e confira se o formulário volta a permitir alterações. Mude provisoriamente o operador ou valor, observe a resposta dos campos e restaure `> 100` antes de adicionar qualquer nova etapa.

**Observe:** coluna, operador e valor permanecem estáveis enquanto você preenche? O aviso de recálculo explica o próximo passo? O resultado de uma base finalizada fica visivelmente protegido contra alterações acidentais?

### 8. Preparar os casos completos

Crie **Biometria completa**, código `base_completa`. Em **Limpeza → Tratar dados faltantes**, escolha explicitamente **Remover as linhas** para `peso_g`. Recalcule: **62 linhas**. Acrescente uma etapa para remover linhas com `comprimento_cm` ausente: **58 linhas**.

**Atenção ao formulário:** o método inicial pode ser **Imputar mediana**. Avalie se essa escolha aparece com clareza. Para este exercício, a intenção é remover linhas, não preencher valores.

Confira que `base_pesos100` continua com 43 linhas e que a compartilhada continua com 68. Finalize a base completa.

### 9. Resumir por espécie

Crie **Resumo por espécie**, código `base_especies`. Em **Recortes e resumos → Agrupar e sumarizar**, agrupe por `especie`, selecione `peso_g` e peça contagem e média.

**Esperado:** **5 linhas**, uma por espécie; a soma de `n` é **68**. A contagem representa as linhas do grupo, enquanto a média desconsidera pesos ausentes. Ela não transforma `n` em quantidade de pesos válidos.

Acrescente mediana às opções e use **Atualizar agrupamento existente**, quando apresentado. Confira que há um único agrupamento na receita. Observe se o aviso de que o resumo deve encerrar a receita aparece antes de você tentar uma operação incompatível.

### 10. Construir uma contingência

Crie **Sexo por local**, código `base_sexo_local`. Em **Construir contingência**, escolha `sexo` nas linhas e `local` nas colunas. Comece com **Somente contagens**.

**Esperado:** base de saída com **6 combinações** e contagens abaixo; total **68**.

| Sexo | ajuruteua | caratateua | braganca | Total |
|---|---:|---:|---:|---:|
| Macho | 14 | 13 | 12 | 39 |
| Fêmea | 10 | 10 | 9 | 29 |

A tabela acima é um gabarito de leitura. A base produzida fica em formato longo, com uma linha por combinação, e não precisa ter esta mesma apresentação.

Atualize para percentual por linha: os percentuais devem somar aproximadamente 100% em cada sexo, admitindo arredondamento. Confira a atualização da etapa, sem criar duas contingências sucessivas.

### 11. Conferir navegação e atualização das bases

- [ ] Alterne as bases pelo seletor superior e depois pela tabela em **Gerenciar Bases**. Nome, tabela, histórico e código devem acompanhar a mesma seleção.
- [ ] Verifique se **Recalcular**, **Finalizar Preparo** e **Reabrir Preparo** são encontrados sem procurar em outra sub-aba.
- [ ] Volte à compartilhada e adicione uma etapa independente, centralizando `cpue` em `cpue_centro`. As bases derivadas existentes devem sinalizar que precisam de atualização.
- [ ] Volte às derivadas e recalcule. As contagens 43, 58, 5 e 6 permanecem iguais, pois a nova coluna não muda os recortes anteriores. Observe também se o estado de finalização é explicado.
- [ ] Remova essa última etapa da compartilhada para restaurar **68 × 11** e recalcule novamente as bases que utilizar a seguir.

**Pausa de avaliação:** você consegue dizer qual base está vendo e se está atualizada sem abrir **Gerenciar Bases**? Registre qualquer salto de posição, desaparecimento de escolha, rolagem excessiva ou mensagem que cubra um botão.

## Laboratório complementar — Experimentar as transformações

Crie uma derivada **Laboratório**, código `base_laboratorio`, a partir da compartilhada restaurada. Use-a para os exercícios abaixo. Adicione e recalcule cada etapa; quando houver remoção de linhas ou substituição de valores, remova essa etapa e recalcule antes do exercício seguinte.

| Ferramenta | Exercício | Conferência |
|---|---|---|
| Calcular variável | Criar `peso_dobro` com `peso_g * 2`. | `id = 1`: 1202; 68 linhas; ausentes preservados. |
| Calcular variável | Criar `indice_condicao` com `100 * peso_g / comprimento_cm^3`. | `id = 1`: aproximadamente 1,17694; ausência em qualquer entrada gera ausência na saída. É um exercício de fórmula. |
| Padronizar valores | Escore z de `cpue`, saída `cpue_z`. | Média aproximadamente 0 e desvio-padrão aproximadamente 1, verificáveis no resumo da coluna ou no código executado. |
| Padronizar valores | Normalizar `cpue` entre 0 e 1, saída `cpue_01`. | Mínimo 0, máximo 1, mesma ordem relativa dos valores. |
| Criar classes | `comprimento_cm`, quatro classes por amplitude igual, saída `classe_comprimento`. | 68 linhas; cinco ausentes preservados. Classes não precisam ter frequências iguais. |
| Dicotomizar por limiar | `comprimento_cm >= 25`, saída `comprimento_25`. | 27 valores 1, 36 valores 0 e 5 ausentes. O corte serve apenas ao teste, sem interpretação de maturidade. |
| Dicotomizar por níveis | `sexo`, nível positivo `Fêmea`, saída `sexo_femea`. | 29 valores 1 e 39 valores 0. Observe se a seleção permanece ao preencher o formulário. |
| Tratar dados faltantes | Em uma etapa descartável, preencher `peso_g` com mediana. | 68 linhas; nenhum peso ausente. Remova a etapa depois: os seis ausentes devem voltar. Isto testa o recurso, sem recomendar imputação para uma análise real. |

Para comparar os dois modos de cálculo, experimente também na **compartilhada** criar `peso_dobro` pelo modo guiado, escolhendo `peso_g`, multiplicação e número 2. Confira 1202 no `id = 1`, remova a etapa e restaure a base. Essa mudança exigirá recálculo das derivadas.

**Recuperação de erro:** no laboratório, tente adicionar `peso_g *` como fórmula incompleta. Registre se a mensagem permite corrigir a expressão e se a última base válida permanece disponível. Corrija para `peso_g * 2`, usando um nome de saída ainda não existente.

**Gestão:** crie uma base temporária adicional, edite seu nome e descrição e exclua somente essa base. Confira se as demais continuam disponíveis. Use **Limpar etapas** apenas no laboratório descartável, depois de guardar os registros, e confira o retorno à origem compartilhada.

## Percurso 3 — Ajustar a estrutura da planilha

Guarde os resultados anteriores. Inicie uma sessão nova para os testes de estrutura; mudar a origem pode reiniciar o preparo. Aqui as contagens partem novamente do arquivo original.

### 12. Empilhar os desembarques

Importe a aba **desembarques_largo**: espere **4 linhas e 4 colunas**. Em **Reestruturar Planilha → Empilhar**:

1. Selecione as três colunas de captura de 2022, 2023 e 2024, mantendo `porto` como identificação.
2. Escolha o delimitador **Hífen entre espaços ' - '**.
3. Informe os nomes de descrição `ano, medida`, nessa ordem, e a coluna de valores `captura_t`.
4. Clique em **Aplicar transformação**, examine a prévia e depois em **Adicionar Mudança à Trilha da Base Compartilhada**.

**Esperado:** **12 linhas e 4 colunas**: `porto`, `ano`, `medida`, `captura_t`. A soma de capturas é **13.660**. O porto Braganca aparece três vezes, com capturas 1240, 1320 e 1180 nos respectivos anos.

**Observe:** a posição de **Empilhar**, **Alargar** e **Separar Colunas** comunica que são alternativas de estrutura? Você percebe a diferença entre produzir a prévia e incorporar a mudança? Compare a expressão “Trilha da Base Compartilhada” com “etapa do preparo” usada nas outras telas e registre se a diferença de nomes confunde.

### 13. Alargar e conferir o retorno

Usando o resultado empilhado, abra **Alargar**. Escolha `ano` como coluna que vira novas colunas e `captura_t` como coluna de valores. Aplique, confira e incorpore a mudança.

**Esperado:** **4 linhas e 5 colunas**: `porto`, `medida`, `2022`, `2023`, `2024`. A coluna constante `medida` permanece; por isso, o total de colunas não volta a quatro. Os valores por porto e ano e a soma **13.660** devem permanecer iguais aos originais.

**Observe:** as escolhas descrevem a operação sem exigir conhecimento das expressões `names_from` e `values_from`? A tabela usa bem a largura disponível? Se a origem mostrada ainda for a planilha anterior, registre o problema antes de prosseguir.

### 14. Separar uma coluna composta

Em outra sessão nova, importe **biometria** e entre em **Separar Colunas**. Escolha `amostra`, delimitador `_`, novos nomes `local_amostra, periodo` e marque **Manter a coluna original**. Aplique e incorpore a mudança.

**Esperado:** **71 linhas e 12 colunas**. `Caratateua_Seca` deve gerar `local_amostra = Caratateua` e `periodo = Seca`; `amostra` continua disponível. Examine outra localidade e o outro período para confirmar a separação em mais de uma linha.

## Fechamento — Aparência, fluidez e organização

Faça uma última passagem pela janela habitual e por uma janela um pouco mais estreita. Não é necessário transformar o teste em uma avaliação de celular: o foco é o uso real no computador.

Use a escala **0 = impediu ou induziu ao erro; 1 = exigiu procura ou tentativa; 2 = foi claro na primeira tentativa**. A nota organiza a conversa; não é uma medida de cobertura funcional.

| Critério | Nota 0–2 | Evidência ou ajuste sugerido |
|---|---|---|
| Encontrei o destino e a ação sem explorar vários lugares. | | |
| Entendi a relação entre menu, grupos de ações e sub-abas de resultado. | | |
| Campos e botões seguiram uma ordem natural de leitura. | | |
| Tabela, formulário e mensagens usaram bem o espaço. | | |
| Texto, contraste, alinhamento e espaçamento favoreceram a leitura. | | |
| Estado atual, prévia pendente e necessidade de recálculo ficaram claros. | | |
| Consegui conferir, desfazer e continuar sem perder o contexto. | | |
| Dados preparados, histórico, código e arquivo baixado contaram a mesma história. | | |

Para cada atrito, registre um caso observável:

| Passo/tela | O que tentei | O que aconteceu | O que esperava | Impacto | Evidência |
|---|---|---|---|---|---|
| | | | | Bloqueia / atrapalha / polimento | Imagem, valor ou descrição |

**Critério para encerrar a conferência:** pontos numéricos conferidos; nenhuma ambiguidade que faça usar a base errada, perder uma etapa ou interpretar uma prévia como resultado aplicado; navegação viável na janela habitual; atritos restantes registrados com prioridade. Não introduza novas ferramentas apenas para completar uma lista: avalie se alguma ausência impede uma tarefa comum dentro do escopo desta versão.

**Extensão opcional de ponta a ponta:** depois de finalizar uma base usada em uma análise, exporte o projeto com um nome curto, como `treino_preparo`. Abra o `.Rproj` no RStudio, confira a reprodução do preparo e renderize o relatório. Registre execução do script e renderização separadamente: o teste de navegação não substitui essa conferência final.

## Situação deste roteiro

Os nomes das telas foram conferidos no código da revisão indicada, e as contagens de referência foram calculadas a partir da planilha original. Este documento prepara uma sessão manual de avaliação; os campos em branco ainda precisam ser preenchidos durante o uso. Ele não certifica que todas as ações descritas já passaram neste percurso.

