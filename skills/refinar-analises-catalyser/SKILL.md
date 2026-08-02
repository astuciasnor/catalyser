---
name: refinar-analises-catalyser
description: Refinar visual, estatística e pedagogicamente análises já integradas à CatalyseR, trabalhando exatamente duas análises por ciclo e preservando o pipeline homologado da interface ao Projeto R e ao Word. Usar ao melhorar saídas, narrativas, tabelas, gráficos, scripts R, relatorio.qmd, replay exportado ou testes de análises piloto; ao preparar uma dupla de análises para homologação; ou ao solicitar mini-refatorações seguras durante esses refinamentos.
---

# Refinar análises da CatalyseR

## Princípio

Tratar a CatalyseR como um projeto maduro. O percurso importar → preparar →
criar bases derivadas → analisar → comunicar → exportar Projeto R → executar no
RStudio → renderizar Word já funciona de ponta a ponta. Refinar sem reconstruir
o motor e sem ampliar o catálogo durante o ciclo.

Trabalhar em exatamente duas análises por vez. Levar a dupla até um nível bom de
uso, ensino, replay e apresentação antes de selecionar outra.

## Preparar o ciclo

1. Ler `AGENTS.md` da pasta-mãe e as instruções locais aplicáveis.
2. Registrar branch, commit e versão do `DESCRIPTION`.
3. Inspecionar primeiro o código e os testes da dupla; não deduzir o estado
   atual a partir de planos ou roteiros Markdown.
4. Ler `docs/DECISOES_PEDAGOGICAS_E_REFINAMENTO.md`, o registro permanente das
   decisões adotadas.
5. Ler [references/criterios-pedagogicos.md](references/criterios-pedagogicos.md).
6. Para a dupla ANOVA + gráfico de linhas, ler
   [references/piloto-anova-grafico-linhas.md](references/piloto-anova-grafico-linhas.md).
7. Inspecionar `git status` e a documentação factual aplicável.
8. Distinguir no documento do piloto o que já foi entregue do refinamento ainda
   pendente; não reimplementar a linha de base humanizada.
9. Registrar uma linha de base executando os testes específicos da dupla.
10. Confirmar que qualquer falha observada é reproduzível antes de alterar código.

Tratar `docs/historico/` somente como memória de decisões. Se um Markdown atual
divergir do código ou dos testes da `main`, corrigir o Markdown no mesmo ciclo e
registrar a versão/commit inspecionado.

Se a dupla tocar seleção de base, preparo ou exportação, ler também
`EVOLUCAO_TRATAMENTO_DADOS.md`, `PIPELINE_DADOS_E_RELATORIOS.md` ou
`MODULO_COMUNICACAO_RESULTADOS.md`, conforme o caso. Conferir os módulos R antes
de repetir qualquer rótulo ou localização descrita nesses mapas.

Não criar análise nova, não mudar regra estatística silenciosamente e não mexer
em módulos fora da dupla sem necessidade demonstrada.

## Refinar cada análise

Trabalhar na seguinte ordem:

1. **Contrato estatístico:** entradas, validações, método R canônico, objetos e
   resultados esperados.
2. **Interface:** nomes, hierarquia, estados vazios, mensagens e prevenção de
   cliques ambíguos.
3. **Saídas:** narrativa em português, tabelas, gráfico, pressupostos,
   diagnósticos e console cru quando pedagogicamente necessário.
4. **Código visível:** tornar o script R numerado semelhante ao que uma pessoa
   escreveria no RStudio.
5. **QMD:** organizar chunks por intenção científica; manter clara a relação
   entre base, variáveis, método, resultado e apresentação.
6. **Word:** incluir apenas conteúdo editorial escolhido, com tabelas e figuras
   legíveis no tema do projeto.
7. **Replay:** garantir resultados equivalentes fora da CatalyseR.

Preservar `inputId`, `outputId`, formatos registrados e compatibilidade com
projetos já exportados, salvo quando a mudança for indispensável e estiver
documentada com migração e testes.

## Humanizar o Projeto R

Fazer o arquivo numerado de cada execução ensinar o percurso:

1. identificar objetivo, base e variáveis;
2. carregar dados;
3. declarar parâmetros com nomes legíveis;
4. construir fórmula ou mapeamento estético;
5. executar a função R canônica;
6. guardar resultados em objetos com nomes claros;
7. exibir os resultados principais;
8. manter metadados técnicos e empacotamento do relatório em uma seção final.

Evitar apresentar `dput()` extenso, dispatcher genérico ou ambiente oculto como
o código principal do aluno. Esses mecanismos podem continuar internamente para
reprodutibilidade, mas devem ficar separados e explicados.

No `relatorio.qmd`, usar labels semânticos e comentários curtos. O código de
estudo deve ser executável quando copiado para o console. Não prometer que um
chunk oculto é pedagógico se ele apenas chama um replay opaco.

Nos pilotos ANOVA e gráfico de linhas, preservar o gerador compartilhado
`exportacao_codigo_estudo()` entre scripts numerados e QMD. Uma melhoria
pedagógica não deve recriar dois geradores divergentes.

## Solicitar mini-refatorações

Incluir mini-refatorações continuamente, uma por vez, quando reduzirem risco ou
duplicação na dupla atual. Exemplos:

- extrair um gerador comum de código;
- centralizar rótulos e nomes de saídas;
- separar cálculo estatístico de apresentação;
- substituir condicionais repetidas por uma função pequena;
- remover dependência global desnecessária;
- criar fixture compartilhado para os testes dos dois pilotos.

Manter cada refatoração pequena, coberta por teste e semanticamente neutra. Não
misturar uma reescrita arquitetural ao refinamento visual.

## Validar

Validar em camadas:

1. parse dos arquivos R alterados;
2. teste puro do cálculo;
3. teste Shiny do estado explícito;
4. teste do registro de duas execuções independentes;
5. geração e inspeção dos scripts numerados;
6. execução dos scripts fora da CatalyseR;
7. geração e renderização de `relatorio.qmd`;
8. comparação de valores essenciais entre interface e replay;
9. suíte completa quando o gerador compartilhado for alterado;
10. `R CMD build` e `R CMD check --no-manual` antes de integrar à `main`.

Não aceitar apenas a existência de arquivos. Verificar conteúdo, execução e
equivalência.

## Encerrar o ciclo

Entregar:

- resumo das melhorias de cada análise;
- mini-refatorações realizadas e por que são neutras;
- arquivos e contratos afetados;
- testes executados e resultados;
- limitações restantes;
- roteiro curto para homologação no Windows 11;
- recomendação explícita sobre prontidão para commit, push ou integração.

Não iniciar a próxima dupla enquanto houver erro funcional, código exportado
opaco nos caminhos principais ou divergência entre interface, Projeto R e Word.
