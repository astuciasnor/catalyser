# Pipeline atual de dados, análises e relatórios da CatalyseR

**Verificado em:** CatalyseR 0.1.5, commit `6aa407a`, 27/07/2026
**Fontes de verdade:** código da `main` e testes automatizados

O histórico das Fases 3A–3E foi preservado em
`docs/historico/PIPELINE_DADOS_E_RELATORIOS_FASES_3.md`.

## Percurso implementado

```text
Importar
  → preparar a Base Compartilhada
  → criar Bases Derivadas quando necessário
  → escolher a base na análise
  → configurar e executar
  → adicionar ou atualizar um resultado
  → organizar a Comunicação
  → exportar Word e Projeto R
```

Esse percurso foi homologado dentro da CatalyseR e fora dela, no RStudio, com
renderização do QMD para Word.

## 1. Base Compartilhada

A Base Compartilhada é `dados_analise`. Ela resulta de:

```text
current_data
  → mudanças estruturais confirmadas em dataset_ativo_rv
  → base_resolvida
  → replay(pipeline_rv)
  → dados_analise
```

As mudanças estruturais vêm de **Pivotar e Separar Dados** e **Organizar
Variáveis**. A Trilha compartilhada contém seis tratamentos. Consulte
`EVOLUCAO_TRATAMENTO_DADOS.md` para o mapa exato da interface.

## 2. Bases Derivadas

Uma Base Derivada existe quando uma finalidade precisa de preparo próprio. Ela:

- nasce diretamente de `dados_analise`;
- possui ID, nome amigável, nome R, finalidade, descrição e receita;
- é materializada somente pelo botão de recálculo;
- passa pelos estados de receita e cache;
- só alimenta análises quando está finalizada e atualizada.

O cache é lazy. Criar, selecionar ou renomear uma base não executa a receita.
Falhas ficam isoladas no ramo e não derrubam a Base Compartilhada nem os outros
ramos.

Filtros, agrupamentos, sumarizações e contingência são próprios de Bases
Derivadas. Os demais tratamentos também podem ser usados no ramo quando forem
específicos daquela análise.

## 3. Seleção de base nas análises

O seletor mostra:

- **Base compartilhada — `dados_analise`**;
- Bases Derivadas finalizadas, recalculadas e atualizadas.

A finalidade compatível recebe uma estrela e aparece primeiro, mas outras bases
válidas continuam disponíveis. A seleção é feita pelo ID estável; a análise
recebe o `data.frame` resolvido e o contexto de proveniência.

Os seletores integrados atendem Estatística Descritiva, Regressão Linear,
Regressão Logística, Teste t, Gráfico de Linhas, Qui-quadrado, ANOVA, PCA e
Análise de Agrupamentos.

## 4. Execução e registro

Escolher base, variáveis ou opções cria um rascunho. O resultado só se torna
atual após **Executar análise**.

Depois da execução, o módulo oferece:

- **Adicionar Novo Resultado**;
- **Atualizar Resultado Anterior**;
- **Apagar Resultado Selecionado**.

Adicionar cria outro ID. Atualizar preserva o ID e incrementa a versão. Mudar
parâmetros altera somente a prévia até que uma dessas ações seja usada.

O `registro_execucoes_rv` guarda metadados leves, não cópias completas de
dados, modelos ou gráficos. Cada execução mantém vínculo com a base e sua
revisão.

## 5. Comunicação de Resultados

O estúdio possui quatro sub-abas:

1. Esboço do documento;
2. Execuções registradas;
3. Bases do projeto;
4. Saída planejada.

O Word segue a seleção editorial. O Projeto R preserva todas as execuções
registradas, inclusive as retiradas do Word. Dependências desatualizadas
bloqueiam a exportação até novo cálculo.

## 6. Projeto R

O projeto exportado contém, em essência:

```text
relatorio.qmd
custom-reference.docx
dados/
R/
metadados/
resultados/
```

Os scripts são ordenados por dependência:

1. importação;
2. mudanças estruturais;
3. Trilha compartilhada;
4. receitas das Bases Derivadas;
5. execuções analíticas.

`metadados/registro_execucoes.rds` preserva a configuração técnica. Na versão
0.1.5, ANOVA e Gráfico de Linhas usam `exportacao_codigo_estudo()` para mostrar
o método principal em código humano tanto nos scripts numerados quanto no QMD.

## 7. Cobertura do replay integrado

`inst/app/templates/funcoes_projeto_integrado.R` executa:

- Estatística Descritiva;
- Regressão Linear;
- Regressão Logística;
- Teste t: uma amostra, independentes e pareado;
- ANOVA de um fator;
- Gráfico de Linhas;
- Qui-quadrado;
- PCA;
- HCA.

Cobertura no menu e cobertura no replay integrado não são sinônimos. Uma análise
existir na interface não significa automaticamente que já participa da
Comunicação e do Projeto R.

## 8. Hierarquia de evidências

Em caso de divergência:

1. comportamento e testes da `main`;
2. `app.R` e módulos diretamente envolvidos;
3. este documento e as decisões atuais em `docs/`;
4. roteiros de homologação datados;
5. documentos de `docs/historico/`.

Não inferir comportamento atual a partir de um plano V15/V16 ou de uma fase
antiga.
