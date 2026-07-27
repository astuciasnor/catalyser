# Plano de continuidade da CatalyseR após a V15

- **Data de referência:** 26/07/2026
- **Versão atual:** 0.1.3 — V15 ampliada de teste
- **Branch atual:** `chore/revisao-fases-3`
- **Checkpoint funcional:** `025af6c` —
  `checkpoint-pipeline-e2e-2026-07-26`
- **Checkpoint V15:** `6084d92` —
  `checkpoint-v15-refinamentos-2026-07-26`
- **Checkpoint V15 ampliada:** `checkpoint-v15-ampliada-2026-07-26`

## 1. Finalidade deste documento

Este arquivo registra o rumo acordado para que o desenvolvimento possa ser
retomado depois, inclusive por outra IA, sem reabrir decisões já tomadas.

O objetivo imediato não é aumentar o catálogo da CatalyseR. O objetivo é tornar
um conjunto pequeno de análises completamente fluido, funcional, reprodutível,
pedagógico e visualmente agradável, do preparo da base ao relatório Word e ao
Projeto R.

## 2. Decisão de produto

### 2.1. Congelamento de novas funcionalidades

Até que teste t, regressão linear simples e ANOVA estejam maduros no percurso
completo, não devem ser acrescentados:

- novos testes estatísticos;
- novos tipos de gráficos;
- novos tratamentos de dados;
- novos formatos de exportação;
- novas áreas no menu.

Depois da V16, a prioridade continuará sendo aprimorar as análises existentes e
suas saídas. Só se avança para outra análise quando o ciclo anterior satisfizer
os critérios de qualidade deste documento.

### 2.2. Núcleo piloto

O núcleo usado para amadurecer o padrão será:

1. teste t de Student;
2. regressão linear simples;
3. ANOVA de um fator.

Essas três análises devem estabelecer o padrão que, mais tarde, será aplicado
gradualmente às demais análises já existentes na CatalyseR. Isso não autoriza
criar análises novas.

### 2.3. Fonte da verdade

A CatalyseR continua sendo a fonte da verdade das análises. Cada análise deve
ter uma definição canônica que alimente:

- cálculo na interface;
- resultado mostrado no Viewer;
- execução registrada;
- Comunicação de Resultados;
- código pedagógico no QMD;
- script reproduzível no Projeto R;
- relatório Word;
- exemplo correspondente no livro EAPA.

## 3. Princípios que não devem ser perdidos

### 3.1. Modelo base/ramos

- A Base Compartilhada contém tratamentos gerais que preservam as observações,
  exceto remoção de duplicatas ou de linhas com NA.
- Bases Derivadas recebem filtros, agrupamentos, sumarizações, tabelas de
  contingência e tratamentos específicos da pergunta analítica.
- Toda Base Derivada nasce diretamente de `dados_analise`.
- Não criar ramos de ramos.
- A base escolhida deve aparecer explicitamente em toda análise.

### 3.2. Execução explícita

O contrato visual e reativo é:

```text
Escolher base → configurar → Executar análise
              → interpretar a prévia → Adicionar aos resultados
              → Comunicação → Word + Projeto R
```

Alterar a base ou qualquer parâmetro deve marcar a prévia como pendente. Nada
deve ser recalculado ou registrado silenciosamente.

### 3.3. Projeto R como ponte do mouse ao código

O Projeto R deve ser executável sem a CatalyseR. O aluno precisa conseguir:

1. abrir o `.Rproj`;
2. executar os scripts na ordem numérica;
3. entender qual base e quais parâmetros foram usados;
4. localizar no QMD o código R essencial da análise;
5. renderizar o relatório Word.

Os chunks pedagógicos do QMD devem permanecer visíveis no arquivo-fonte, com
`eval: false` e `include: false`, para não poluir o Word.

### 3.4. Refatorações pequenas são permitidas

Pequenas refatorações podem acompanhar os ajustes quando:

- reduzem duplicação evidente;
- separam cálculo, apresentação e exportação;
- tornam o contrato de uma análise reutilizável;
- removem código legado que já foi substituído;
- melhoram nomes internos sem mudar `inputId` ou `outputId`;
- recebem teste antes ou junto da alteração.

Não fazer grandes reescritas simultâneas. Cada refatoração deve caber em um
commit compreensível e deixar o aplicativo executável.

## 4. Situação atual depois da V15

### 4.1. O que já está aprovado

- composição da Base Compartilhada;
- criação, receita, replay, cache e finalização de Bases Derivadas;
- seleção de Bases Derivadas nos oito módulos prioritários da Fase 3;
- execução explícita e registro independente;
- Comunicação de Resultados organizada em sub-abas;
- exportação de Word e Projeto R;
- reprodução dos scripts e do QMD fora da CatalyseR;
- nomes longos mais legíveis no seletor de bases;
- código essencial das análises explícito no QMD;
- atalho `Recalcular a Base` dentro da sub-aba `Receita da base`, ligado ao
  mesmo recálculo da sub-aba de gestão;
- estúdio `Pivotar e Separar Dados`, reunindo `pivot_longer()`,
  `pivot_wider()` e separação em colunas;
- módulo `Organizar Variáveis` dividido em `Criação de Variáveis` e
  `Arrumação de Variáveis`;
- cálculo e reescala centralizados em `Criação de Variáveis`;
- `Adicionar Tratamentos à Base` dividido em `Tratamentos e trilha` e
  `Checagem Final da Base Compartilhada`;
- Checagem Final em grade 20/80, com ajustes leves à esquerda e tabela
  paginada/rolável à direita;
- retirada desses quatro controles repetidos das interfaces de Importação,
  Empilhar e Separar, sem remover a compatibilidade interna existente;
- preservação do código das etapas estruturais anteriores quando
  `Organizar Variáveis` promove seu resultado para a Base Compartilhada;
- instalação limpa do pacote 0.1.3.

### 4.2. Evidências

- Todos os testes em `inst/app/tests/run_tests.R` passaram.
- A sintaxe dos 67 arquivos R foi aprovada.
- O teste de exportação renderizou o Word.
- O seletor foi inspecionado em 1366 × 768.
- `R CMD build` e a instalação em biblioteca vazia passaram.
- `R CMD check --no-manual` ficou sem erros e sem avisos, com uma nota conhecida
  sobre dependências consumidas pelos arquivos Shiny de `inst/app`.

### 4.3. ANOVA ainda não integrada

O arquivo `inst/app/modules/mod_anova.R` já contém:

- ANOVA de um fator;
- tabela da ANOVA;
- verificação de pressupostos;
- Tukey HSD;
- gráfico de médias;
- diagnóstico de resíduos;
- curva F;
- simulador didático;
- exportações próprias de Word e Projeto R.

Entretanto, a ANOVA ainda:

- lê diretamente `dados_analise`;
- não usa `mod_seletor_base_analise`;
- não possui o fluxo explícito em duas sub-abas;
- não devolve `estado_execucao`;
- não é ligada a `mod_registrar_execucao`;
- não aparece como execução no novo estúdio de Comunicação;
- não possui replay em `templates/funcoes_projeto_integrado.R`;
- não recebe código pedagógico específico em
  `exportacao_codigo_estudo()`;
- mantém exportações legadas próprias, paralelas ao novo exportador integrado.

Essas lacunas definem o trabalho da V16.

## 5. Ciclo V16 — integração e amadurecimento da ANOVA

### Etapa 1 — proteger o estado atual

Antes de alterar:

1. confirmar a branch e o commit atual;
2. executar `git status --short`;
3. preservar os checkpoints existentes;
4. não fazer `push` sem pedido explícito;
5. executar a suíte atual como linha de base.

### Etapa 2 — ligar a ANOVA às Bases Derivadas

Em `inst/app/app.R`:

- inserir `mod_seletor_base_analise_ui("base_anova")` no painel da ANOVA;
- criar `seletor_anova <- mod_seletor_base_analise_server(...)`;
- passar `seletor_anova$dados` para `mod_anova_server`;
- usar `seletor_anova$contexto` no registro da execução;
- preservar os namespaces e IDs existentes sempre que possível.

A finalidade preferida pode orientar a lista, mas não deve esconder bases
válidas. O seletor deve mostrar nome amigável, objeto R e dimensões.

### Etapa 3 — aplicar o contrato de execução explícita

Em `mod_anova.R`:

- organizar a interface nas sub-abas:
  1. `Configurar e executar`;
  2. `Adicionar aos resultados`;
- adicionar `execucao_explicita_controles_ui(ns)`;
- proteger resultados com `execucao_explicita_resultados_ui(...)`;
- proteger downloads legados enquanto existirem;
- construir uma assinatura com base, variáveis e configurações gráficas;
- chamar `execucao_explicita_server(...)`;
- calcular no primeiro clique efetivo;
- retornar `estado_execucao` e `estado_execucao_ui`.

Não duplicar o botão de registro dentro do módulo. O padrão continua sendo o
registro reutilizável já usado por teste t e regressão.

### Etapa 4 — definir o estado canônico da execução ANOVA

O estado registrado deve incluir, no mínimo:

```r
list(
  analise_id = "anova",
  tipo = "anova_um_fator",
  titulo = "...",
  parametros = list(
    resposta = "...",
    fator = "...",
    nivel_confianca = 0.95
  ),
  saidas_disponiveis = c(
    "narrativa", "tabela", "grafico",
    "pressupostos", "diagnosticos", "console"
  ),
  resultado_resumo = list(...),
  codigo_r = "..."
)
```

Os parâmetros congelados devem ser suficientes para repetir a análise sem
consultar os inputs da sessão Shiny.

### Etapa 5 — integrar o replay do Projeto R

Em `inst/app/templates/funcoes_projeto_integrado.R`:

- criar uma função de execução da ANOVA que receba somente `dados` e
  `parametros`;
- incluir `anova_um_fator` em `catalyser_executar()`;
- devolver os mesmos componentes conceituais mostrados na interface;
- usar funções `stats::` e namespaces explícitos sempre que possível;
- manter o resultado útil mesmo fora do RStudio.

O replay precisa produzir, no mínimo:

- objeto do modelo;
- tabela ANOVA arrumada;
- narrativa;
- Tukey, quando aplicável;
- pressupostos;
- gráfico principal;
- diagnósticos;
- console bruto.

### Etapa 6 — integrar Comunicação, QMD e Word

Em `inst/app/modules/exportacao_comunicacao.R`:

- adicionar o código pedagógico de `anova_um_fator` a
  `exportacao_codigo_estudo()`;
- mostrar no QMD um código essencial semelhante a:

```r
formula_modelo <- stats::reformulate("fator", response = "resposta")
modelo <- stats::aov(formula_modelo, data = dados)
summary(modelo)
stats::TukeyHSD(modelo)
```

- manter esse chunk fora do Word;
- preservar a execução integral no script numerado;
- verificar se as tabelas e figuras cabem no Word sem cortes.

Quando a ANOVA estiver integrada, suas exportações próprias antigas devem ser
avaliadas. Removê-las somente depois que o fluxo integrado cobrir tudo que elas
oferecem e houver teste de equivalência.

### Etapa 7 — reorganizar a interface da ANOVA

A interface atual oferece muitos painéis simultaneamente. Para iniciantes, usar
divulgação progressiva:

- configuração essencial primeiro;
- resultado principal antes dos diagnósticos;
- Tukey apresentado como consequência da ANOVA, não como teste isolado;
- pressupostos em uma aba própria e com explicação;
- simuladores claramente rotulados como recursos didáticos, separados do
  resultado observado;
- downloads e registro somente após execução válida.

Evitar barras de rolagem internas e painéis com altura fixa. Validar em
1366 × 768.

## 6. Padrão de qualidade das saídas

Cada análise madura deve oferecer os componentes abaixo.

### 6.1. Narrativa

A narrativa em português deve informar:

- pergunta respondida;
- amostra e variáveis utilizadas;
- estimativa principal;
- estatística do teste e graus de liberdade;
- p-valor;
- intervalo de confiança, quando aplicável;
- tamanho de efeito, quando aplicável;
- conclusão estatística sem exagerar a conclusão científica.

Nunca escrever “H0 aceita”. Preferir:

- “rejeitou-se H0”; ou
- “não houve evidência suficiente para rejeitar H0”.

### 6.2. Tabelas

- nomes de colunas em português;
- arredondamento consistente;
- `n` e ausentes explícitos;
- p-valores formatados;
- unidades preservadas nos rótulos;
- saída simples para inspeção;
- `flextable_ocean()` quando a tabela for referenciada no relatório;
- nenhuma coluna técnica desnecessária para o iniciante.

### 6.3. Gráficos

- título informativo;
- eixos e unidades explícitos;
- pontos observados sempre que forem úteis;
- cores Ocean Gradient;
- legenda somente quando necessária;
- indicação clara do que são barras de erro;
- tamanho legível em 1366 × 768 e no Word;
- versão reproduzível pelo código exportado.

### 6.4. Pressupostos e diagnósticos

- separar “resultado” de “qualidade do ajuste”;
- não decidir normalidade apenas pelo teste de Shapiro;
- combinar gráfico e teste quando apropriado;
- explicar a limitação de testes de pressupostos em amostras pequenas ou
  grandes;
- dar uma orientação prática quando o pressuposto parece inadequado;
- não trocar automaticamente de análise sem decisão do pesquisador.

### 6.5. Console

O console bruto deve permanecer disponível pelo menos uma vez no percurso
pedagógico de cada análise, para que o aluno reconheça a saída padrão do R fora
da CatalyseR.

### 6.6. QMD e Projeto R

- código essencial legível;
- script integral numerado;
- parâmetros congelados;
- nome da base explícito;
- comentários que expliquem a intenção estatística;
- execução possível fora da CatalyseR;
- Word limpo, sem despejo desnecessário de código.

## 7. Princípios pedagógicos para iniciantes

### 7.1. A interface deve ensinar o percurso

O aluno precisa reconhecer quatro momentos:

1. **Qual é minha pergunta?**
2. **Qual base e quais variáveis respondem a ela?**
3. **O que o resultado estatístico diz?**
4. **Como reproduzo isso em R?**

### 7.2. Linguagem da interface

- explicar termos na primeira ocorrência;
- usar “variável resposta (numérica)” e “fator ou grupo (categórico)”;
- não depender de jargão em inglês;
- manter o nome R visível sem substituir o nome amigável;
- escrever mensagens de erro com uma ação corretiva;
- nunca deixar um controle desabilitado sem explicar por quê.

### 7.3. Defaults seguros

- selecionar somente combinações válidas;
- não executar automaticamente;
- não esconder exclusões de NA;
- avisar quando um grupo tem poucas observações;
- avisar quando há apenas um nível de fator;
- não apresentar Tukey como necessário quando a comparação não faz sentido;
- manter dados simulados claramente separados dos dados observados.

### 7.4. Leitura em camadas

Organizar cada análise em camadas:

1. conclusão breve;
2. tabela e gráfico principais;
3. pressupostos e diagnósticos;
4. console;
5. código R e Projeto R.

Assim, o iniciante não recebe tudo ao mesmo tempo, mas o conteúdo avançado não
é removido.

## 8. Testes obrigatórios por análise

### 8.1. Testes automatizados

Toda análise integrada deve ter:

- teste de sintaxe;
- teste da função de cálculo;
- `testServer()` para o primeiro clique;
- teste de mudança de parâmetros deixando a execução pendente;
- teste de registro;
- teste com Base Compartilhada;
- teste com Base Derivada pronta;
- teste de base desatualizada;
- teste de replay pelo exportador integrado;
- teste do código pedagógico no QMD;
- teste de renderização do Word.

### 8.2. Testes manuais

Executar em Windows 10, 1366 × 768:

- nomes longos no seletor;
- ausência de cortes e rolagens internas desnecessárias;
- primeiro clique em Executar análise;
- troca de base;
- mensagens para configurações inválidas;
- leitura das tabelas e gráficos;
- registro de duas execuções diferentes.

Executar em Windows 11 com RStudio:

- abrir o Projeto R;
- executar os scripts numerados;
- conferir os objetos criados;
- abrir o QMD;
- localizar o código essencial;
- renderizar o Word;
- comparar resultados com a CatalyseR.

## 9. Cenário recomendado para a V16

O teste mínimo deve continuar pequeno:

- duas Bases Derivadas;
- duas análises registradas;
- uma delas obrigatoriamente ANOVA;
- a outra deve ser teste t ou regressão linear, usada como controle de que a
  integração anterior não regrediu.

Se o teste mínimo passar sem retrabalho importante, realizar uma segunda rodada
com as três análises piloto:

1. teste t;
2. regressão linear;
3. ANOVA.

Depois, gerar Comunicação, Word e Projeto R únicos.

## 10. Critérios para considerar a ANOVA concluída

A ANOVA só estará pronta quando:

- aceitar Base Compartilhada e Base Derivada;
- exibir claramente a base ativa;
- executar no primeiro clique efetivo;
- ficar pendente após qualquer mudança relevante;
- ser adicionada ao registro;
- coexistir com outra execução ANOVA com base ou parâmetros diferentes;
- aparecer na Comunicação;
- permitir escolha de conteúdos para o Word;
- possuir replay no Projeto R;
- exibir código essencial no QMD;
- produzir o mesmo resultado dentro e fora da CatalyseR;
- renderizar tabelas e gráficos adequadamente no Word;
- passar pela explicação pedagógica para iniciantes;
- passar por todos os testes automatizados e manuais.

## 11. Pequenas refatorações prioritárias

Realizar somente quando surgirem naturalmente durante os ajustes:

1. separar cálculo, arrumação, narrativa e apresentação em funções pequenas;
2. remover `library()` de dentro de reactives e usar namespaces;
3. centralizar formatação de p-valor, IC, números e rótulos;
4. centralizar componentes comuns das abas de análise;
5. reduzir exportadores legados depois da equivalência comprovada;
6. diminuir responsabilidades de `app.R`;
7. adicionar testes antes de dividir arquivos grandes;
8. corrigir avisos conhecidos de gráficos sem alterar as saídas.

Avisos atualmente observados na suíte e que podem ser tratados aos poucos:

- estética `size` de linhas depreciada em favor de `linewidth`;
- parâmetro `label.size` ignorado em uma anotação;
- avisos de ajustes essencialmente perfeitos em dados artificiais de teste.

## 12. Fluxo de versionamento

Para cada melhoria:

1. trabalhar a partir de um checkpoint conhecido;
2. manter mudanças pequenas;
3. executar testes focais;
4. executar a suíte completa;
5. revisar `git diff --check`;
6. fazer um commit descritivo;
7. criar tag local apenas em marcos testáveis;
8. não enviar ao GitHub sem autorização explícita;
9. gerar o ZIP a partir do commit testado;
10. criar um `LEIA-ME` externo com instruções da VM;
11. verificar o conteúdo do ZIP e registrar SHA-256.

Nome sugerido do próximo artefato:

```text
catalyser-anova-integrada-AAAAMMDD-v16.zip
```

## 13. Arquivos que a próxima IA deve ler primeiro

1. `AGENTS.md`
2. este arquivo;
3. `EVOLUCAO_TRATAMENTO_DADOS.md`;
4. `MODULO_COMUNICACAO_RESULTADOS.md`;
5. `docs/testes/fases-3/HOMOLOGACAO_END_TO_END_WINDOWS_2026-07-26.md`;
6. `docs/testes/fases-3/ROTEIRO_TESTE_V15_DUAS_ANALISES.md`;
7. `inst/app/modules/mod_anova.R`;
8. `inst/app/modules/mod_organizar_variaveis.R`;
9. `inst/app/modules/mod_execucao_explicita.R`;
10. `inst/app/modules/mod_seletor_base_analise.R`;
11. `inst/app/modules/registro_execucoes.R`;
12. `inst/app/modules/exportacao_comunicacao.R`;
13. `inst/app/templates/funcoes_projeto_integrado.R`;
14. trechos de integração dos módulos em `inst/app/app.R`.

## 14. Instrução curta para passagem de serviço

> Continue a CatalyseR a partir da tag
> `checkpoint-v15-ampliada-2026-07-26`, sem criar novas análises.
> Integre a ANOVA de um fator ao mesmo contrato já usado por teste t e regressão
> linear: seletor de base, execução explícita, registro, Comunicação, replay,
> QMD pedagógico e Word. Preserve os IDs e as regras estatísticas existentes,
> preserve também o módulo `Organizar Variáveis` e o recálculo disponível nas
> duas sub-abas de Bases Derivadas, faça refatorações pequenas com testes e gere
> a V16 somente depois de validar duas Bases Derivadas e duas análises, sendo uma
> delas ANOVA.

## 15. Regra final de decisão

Antes de aceitar qualquer alteração, perguntar:

> Isso torna o percurso de um aluno iniciante mais claro, confiável,
> reproduzível e agradável sem ampliar desnecessariamente o escopo?

Se a resposta não for claramente “sim”, a alteração deve esperar.
