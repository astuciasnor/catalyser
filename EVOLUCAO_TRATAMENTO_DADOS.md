# Preparo de dados atual — contratos da Base Compartilhada e das Bases Derivadas

**Verificado em:** CatalyseR 0.1.5, commit `6aa407a`, 27/07/2026
**Fontes de verdade:** `inst/app/app.R`, módulos R e testes automatizados

Este documento descreve o que está implementado. O plano que originou a
arquitetura foi preservado em
`docs/historico/EVOLUCAO_TRATAMENTO_DADOS_PLANO_ORIGINAL.md`.

## Regra de autoridade

Ao trabalhar no preparo de dados:

1. conferir primeiro o código atual e os testes;
2. usar este arquivo como mapa, não como substituto da inspeção;
3. não usar documentos de `docs/historico/` como especificação vigente;
4. atualizar este mapa quando rótulos, locais ou contratos mudarem.

## Menu Preparando Dados

O menu atual contém, nesta ordem:

1. **Importação e Visualização**;
2. **Pivotar e Separar Dados**;
3. **Organizar Variáveis**;
4. **Adicionar Tratamentos à Base**;
5. **Bases Derivadas**.

### 1. Importação e Visualização

Carrega CSV, Excel ou um dataset do EAPADados. A tela possui as áreas
Carregamento de Dados, Visualização/Resumo e Status do Dataset. Depois da
importação, a própria interface encaminha o usuário para **Organizar
Variáveis** quando precisar selecionar, renomear, tipar ou recodificar.

### 2. Pivotar e Separar Dados

Possui três sub-abas:

- **Empilhar Dados — `pivot_longer()`**;
- **Alargar Dados — `pivot_wider()`**;
- **Separar Dados em Colunas**.

Cada operação mostra Resultado, Original e Script gerado. O botão
**Adicionar Mudança à Trilha da Base Compartilhada** confirma a prévia e
encadeia a mudança estrutural na Base Compartilhada.

### 3. Organizar Variáveis

Possui duas sub-abas:

- **Criação de Variáveis:** calcular uma variável e reescalar por prefixo;
- **Arrumação de Variáveis:** selecionar, renomear, definir tipos e recodificar
  níveis.

A Arrumação usa colunas 3–7–2: controles, prévia/script e ações. Essas mudanças
não filtram linhas. O botão **Adicionar Mudança à Trilha da Base
Compartilhada** confirma a prévia.

### 4. Adicionar Tratamentos à Base

Possui duas sub-abas:

- **Tratamentos e trilha**;
- **Checagem Final da Base Compartilhada**.

Em **Tratamentos e trilha**, a caixa Tipo de tratamento contém exatamente:

1. Dados faltantes (NA);
2. Dicotomizar (0/1);
3. Padronizar / Escalar;
4. Classes de tamanho (binning);
5. Remover duplicatas;
6. Padronizar texto.

Não contém **Filtrar linhas**, **Agrupar / Sumarizar** nem **Tabela de
Contingência**. As análises usam essa trilha automaticamente.

Na trilha compartilhada, a redução de linhas fica restrita à remoção de
duplicatas ou à remoção de linhas durante o tratamento de NA. Dicotomização,
padronização e classes criam colunas; padronização de texto uniformiza a coluna
escolhida.

Em **Checagem Final da Base Compartilhada**, o layout é aproximadamente 20%–80%:

- esquerda: Renomear, Selecionar, Remover, Recodificar, Definir tipos, Revisar
  categorias, Desfazer ajustes, Baixar base final e confirmar a mudança;
- direita: tabela paginada, com rolagem horizontal e opções de 5, 10, 25, 50 ou
  100 linhas.

### 5. Bases Derivadas

Possui duas sub-abas que compartilham a mesma base selecionada:

- **Criação e gestão da base**;
- **Receita da base**.

Na primeira, o layout é 3–7–2: criação, Registro/Prévia/Código e Ações. As ações
são renomear, recalcular, finalizar, reabrir e excluir; Excluir fica separado
das demais.

Na segunda, o topo mostra **Base ativa** e o botão **Recalcular a Base**. Sem
base selecionada, os controles permanecem desabilitados.

A caixa **Tratamento a adicionar** contém:

1. Dados faltantes (NA);
2. Dicotomizar (0/1);
3. Padronizar / Escalar;
4. Classes de tamanho (binning);
5. Remover duplicatas;
6. Padronizar texto;
7. Calcular variável;
8. Reescalar (prefixo SI);
9. Filtrar linhas;
10. Agrupar / Sumarizar;
11. Tabela de Contingência.

Portanto, Bases Derivadas não se limitam aos três últimos itens: elas também
podem repetir um tratamento geral quando ele for necessário apenas para uma
análise específica.

Agrupar/Sumarizar e Contingência são **etapas redutoras**:

- só pode haver uma delas em cada receita;
- ela precisa ser a última etapa;
- para mudar suas opções, a etapa final é atualizada, não duplicada.

## Estado reativo real

O código atual mantém duas camadas distintas:

```text
dados importados
  → mudanças estruturais confirmadas
  → base_resolvida
  → replay da trilha compartilhada
  → dados_analise
  → zero ou mais Bases Derivadas em estrela
```

- `dataset_ativo_rv`: resultado estrutural confirmado mais recente;
- `base_externa_rv`: proveniência e código acumulado das mudanças estruturais;
- `base_resolvida`: dados importados ou `dataset_ativo_rv`;
- `pipeline_rv`: somente as seis etapas da Trilha compartilhada;
- `dados_analise`: replay de `pipeline_rv` sobre `base_resolvida`;
- `registro_bases_rv`: receitas das Bases Derivadas;
- `cache_bases_rv`: resultados materializados dos ramos.

Embora os botões estruturais usem no rótulo a expressão “Adicionar Mudança à
Trilha”, essas mudanças são acumuladas em `base_externa_rv`; elas não são
entradas de `pipeline_rv`. Essa distinção técnica deve permanecer explícita na
documentação e na exportação.

## Invariantes

- dados importados não são modificados diretamente;
- mudanças estruturais leem `base_resolvida`, não `dados_analise`, evitando
  aplicar a Trilha duas vezes;
- a Trilha compartilhada é a última camada antes de `dados_analise`;
- toda Base Derivada nasce diretamente de `dados_analise`;
- não existe ramo de ramo;
- editar uma receita invalida o cache sem recalcular automaticamente;
- só bases finalizadas, recalculadas e atualizadas aparecem nos seletores das
  análises;
- mudar a finalidade ordena sugestões, mas não torna um ramo exclusivo daquela
  análise.

## Testes que sustentam este mapa

- `inst/app/tests/test_pivotar_organizar.R`;
- `inst/app/tests/test_organizar_variaveis.R`;
- `inst/app/tests/test_bases_derivadas.R`.
