# Pipeline de dados, análises e geração de relatórios da CatalyseR

## Finalidade

Esta é a especificação canônica do fluxo que conecta importação, preparo
reprodutível, bases derivadas, análises, registro de resultados, Comunicação de
Resultados e exportação do Projeto R.

> **Preparar a base → vincular à análise → registrar o resultado → escolher o
> que comunicar.**

A CatalyseR é a fonte da verdade das análises. Cada ação feita no mouse deve
produzir uma representação reprodutível em R.

## Estado de implementação

| Componente | Estado em julho de 2026 |
|---|---|
| Importação e visualização | Implementado |
| Arrumar (empilhar/separar) | Implementado |
| Calcular/reescalar | Implementado |
| Agrupar/sumarizar | **Implementado nesta etapa** |
| Trilha de Preparo compartilhada | Implementada (Fases 1 e 2) |
| `dados_analise` compartilhado | Implementado |
| Registro e gerenciamento de bases derivadas | **Implementado (Fase 3A)** |
| Cache lazy, estados e recálculo manual | **Implementado (Fase 3A.1)** |
| Transformações específicas dentro dos ramos | **Implementado (Fase 3B.1)** |
| Seletor de base nas análises prioritárias | **Implementado (Fases 3B.2 e 3B.3)** |
| Registro de execuções/resultados | **Implementado (Fase 3C)** |
| Execução analítica explícita e rascunho pendente | **Implementado (Fase 3C.1)** |
| Seleção fina do conteúdo do relatório | Casca existente; integração planejada |
| Exportação `.docx` e Projeto R | Motor prototipado; integração incremental |

## Arquitetura conceitual

```text
dados_brutos (originais e imutáveis)
    |
    +-- operações estruturais antes da trilha
    |     +-- Arrumar
    |     +-- Calcular/Reescalar
    |     +-- Agrupar/Sumarizar
    |     `-- resultado promovido -> base_resolvida
    |
    `-- Trilha de Preparo compartilhada (replay ordenado)
          `-- dados_analise
                +-- análises diretas
                +-- visualizações e resumos
                `-- bases derivadas futuras (um salto)
                      +-- base_reg_logistica
                      +-- base_qui_quadrado
                      +-- base_reg_multipla
                      `-- base_<finalidade>
```

Os dados brutos não são modificados. A base compartilhada é rederivada por
*replay*. Uma base derivada nasce somente de `dados_analise`; não se permitem
ramos de ramos na v1.

## Camadas e responsabilidades

### 1. Dados brutos

- arquivo local CSV/Excel ou dataset do `EAPADados`;
- preservado como origem auditável;
- troca de origem invalida bases promovidas e a trilha anterior;
- contém as informações necessárias para gerar o código de importação.

### 2. Operações estruturais

Operações que podem mudar a forma ou a unidade de observação da tabela:

- **Arrumar:** empilhar, separar e alargar;
- **Calcular/Reescalar:** criar colunas sem substituir as originais;
- **Agrupar/Sumarizar:** transformar várias observações em uma linha por grupo.

Esses módulos mostram uma prévia, geram script R e só promovem o resultado
quando o usuário clica em **Usar este resultado nas análises**. O resultado
promovido torna-se `base_resolvida`, sobre a qual a Trilha é executada.

### 3. Trilha de Preparo compartilhada

A Trilha é a camada mais externa da base compartilhada. Suas etapas são
ordenáveis e reaplicadas logicamente sobre `base_resolvida`. O produto é:

```r
dados_analise <- replay_pipeline(base_resolvida, pipeline)$df
```

Ela registra o preparo comum a todas as análises e deve alimentar a Seção 0 —
Métodos do relatório.

### 4. Bases derivadas (Fase 3)

Uma base derivada só deve ser criada quando uma análise exigir preparo que não
deve alterar todas as demais. Exemplos:

- dicotomizar resposta para regressão logística;
- organizar categorias para qui-quadrado;
- selecionar ou transformar preditores para um modelo;
- agregar dados para um gráfico quando a agregação não for compartilhada.

Não se cria `base_graficos` apenas porque será gerado um gráfico. Se
`dados_analise` já estiver adequado, o gráfico usa diretamente essa base.

#### Fluxo de criação e edição — Fases 3A a 3B.1 implementadas

1. Em `dados_analise`, clicar em **Criar base derivada**.
2. Informar finalidade e nome amigável.
3. A IDE sugerir um nome R válido, como `base_reg_logistica`.
4. No editor de receita, adicionar, ordenar, ativar ou remover tratamentos
   específicos do ramo.
5. Clicar em **Recalcular esta base** para materializar a receita.
6. Inspecionar a prévia e o código R do ramo.
7. Clicar em **Finalizar preparo**.
8. Reabrir, renomear ou excluir o ramo quando necessário.

Um ramo novo tem zero etapas e, portanto, é inicialmente idêntico a
`dados_analise`. A Fase 3B.1 permite editar sua receita usando o mesmo registro
canônico da Trilha: tratar NA, dicotomizar, padronizar, criar classes, remover
duplicatas, padronizar texto, calcular, reescalar e filtrar. A edição só é
permitida em rascunhos; bases prontas precisam ser reabertas.

Alterar a receita incrementa sua versão e torna o cache **Desatualizado**, sem
executar replay automático. A primeira etapa é validada contra `dados_analise`;
as seguintes são validadas contra o cache atual. Por isso, uma nova etapa só
pode ser adicionada depois de recalcular a anterior. Ordenar, ativar/desativar
ou remover permanece livre e apenas invalida o cache. A materialização continua
exclusiva do botão **Recalcular esta base**.

As Fases 3B.2 e 3B.3 disponibilizam bases prontas nos módulos analíticos sem
recalcular receitas durante a escolha.

Cada base deve possuir:

| Campo | Exemplo | Uso |
|---|---|---|
| ID interno imutável | `base_0042` | Vínculo estável |
| Nome R | `base_reg_logistica` | Código exportado |
| Nome amigável | Logística da ocorrência | Interface e relatório |
| Base de origem | `dados_analise` | Proveniência |
| Etapas próprias | dicotomizar, definir referência | Replay do ramo |
| Estado | rascunho, pronta, desatualizada | Controle de dependências |

Se uma base for alterada, as análises dependentes devem ser marcadas como
**precisam ser atualizadas**.

### Cache e replay sob demanda — Fase 3A.1

O registro guarda a **receita reproduzível**; o resultado materializado fica em
um cache separado, somente em memória. Abrir o painel, selecionar uma base,
renomeá-la ou consultar seu código não executa replay.

Cada entrada de cache registra:

- revisão de `dados_analise` usada no cálculo;
- versão da receita do ramo;
- última tentativa de recálculo;
- data da última prévia válida;
- dimensões da última prévia válida;
- erros isolados do ramo.

| Estado do cache | Significado | Pode alimentar análise? |
|---|---|---|
| Não calculada | ramo ainda não foi materializado | Não |
| Atualizada | origem e receita coincidem com o cache | Sim, se o preparo estiver `pronta` |
| Desatualizada | `dados_analise` ou a receita mudou | Não |
| Com erro | o replay do ramo falhou | Não |

O usuário recalcula somente a base selecionada com **Recalcular esta base**.
Não existe recomputação automática em cascata. Se uma nova tentativa falhar, um
resultado parcial nunca substitui a prévia: a IDE preserva somente a última
tabela que terminou sem erro e a bloqueia para análises.

Colunas opcionais devem ser protegidas com interseção defensiva. Quando uma
coluna for obrigatória para o significado da etapa, sua ausência deve marcar
somente o ramo como **Com erro**, sem derrubar o app ou os ramos irmãos.

## Contrato entre base e análise

Cada módulo de análise deverá conter um seletor persistente:

```text
Base utilizada: [ dados_analise ▼ ]
```

`dados_analise` é a opção padrão. Bases derivadas compatíveis podem ser
sugeridas, mas a escolha final é do usuário.

**Piloto implementado na Fase 3B.2:** a Regressão Logística oferece
`dados_analise` e somente bases derivadas simultaneamente `pronta` e com cache
`Atualizada`. Ramos de finalidade `reg_logistica` aparecem primeiro como
sugestão, mas outros ramos válidos permanecem disponíveis. A escolha altera de
fato os dados usados pelo modelo, tabelas e gráficos. Se o ramo escolhido for
reaberto, excluído ou ficar desatualizado, a IDE avisa e retorna explicitamente
para `dados_analise`; uma prévia antiga nunca alimenta o modelo.

O módulo também expõe internamente `base_contexto` — ID estável, nome do objeto
R e dados resolvidos — para o futuro registro de execuções.

**Expansão implementada na Fase 3B.3:** o mesmo contrato foi extraído para um
seletor reutilizável e conectado a Estatística Descritiva, Regressão Linear
Simples, Teste t, Gráfico de Linhas, Qui-quadrado, PCA e Análise de
Agrupamentos. A finalidade do ramo apenas muda a ordem das sugestões; não
restringe a escolha. Cada módulo mantém sua própria seleção e todos recebem o
`data.frame` já materializado no cache — alternar a base nunca executa replay.

No Qui-quadrado, a seleção vale quando a fonte é **Duas variáveis**. As fontes
**Tabela preparada** e **Entrada manual** continuam usando seus próprios dados,
o que fica explícito na interface. Os demais módulos continuam em
`dados_analise` até uma integração incremental posterior; isso evita uma
alteração ampla antes dos testes no Zorin.

Ao executar uma análise, a IDE deve registrar, no mínimo:

```r
list(
  id = "analise_007",
  tipo = "regressao_logistica",
  titulo = "Ocorrência de peixes e condições ambientais",
  base_id = "base_0042",
  base_objeto = "base_reg_logistica",
  parametros = list(
    resposta = "ocorrencia",
    preditores = c("temperatura", "salinidade")
  ),
  saidas_disponiveis = c("narrativa", "tabela", "grafico", "pressupostos", "diagnosticos")
)
```

Uma base pode alimentar várias análises. Cada execução deve ter um ID próprio.

**Implementado na Fase 3C:** o contrato acima virou um registro central em
memória e um componente reutilizável de confirmação. Ele cobre Estatística
Descritiva, Regressão Linear, Regressão Logística, Teste t, Gráfico de Linhas,
Qui-quadrado, PCA e Análise de Agrupamentos. Cada módulo expõe um estado leve
com parâmetros, saídas disponíveis, resumo pequeno do resultado e, quando já
existia no módulo, código R. O registrador acrescenta o ID da base, nome do
objeto R, revisão da origem e versão da receita.

O clique não duplica o `data.frame`, o modelo nem o gráfico no registro. Essa
decisão mantém a sessão leve: a execução será reconstruída a partir da base e
dos parâmetros durante a comunicação/exportação. Se a Trilha ou a receita do
ramo mudar, o item permanece preservado, mas recebe o estado **Precisa
atualizar**. Uma entrada manual do Qui-quadrado não depende da revisão de
`dados_analise`.

As ações implementadas são **Adicionar aos resultados**, **Atualizar
resultado**, **Salvar como novo** e **Remover dos resultados**. O registro é
limpo quando outro conjunto de dados bruto é carregado, evitando misturar
execuções de projetos diferentes.

**Implementado na Fase 3C.1:** escolher base, variáveis ou opções cria apenas um
rascunho. Os oito módulos prioritários não calculam nem mostram resultados até
o clique em **Executar análise**. Depois do clique, a configuração e a revisão
da base ficam assinadas. Qualquer mudança analítica ou de dados oculta a prévia
anterior, marca o rascunho como pendente e exige **Executar novamente**.

O registrador só habilita **Adicionar aos resultados**, **Atualizar resultado**
e **Salvar como novo** quando a prévia corresponde à configuração e à base
atuais. Assim, uma execução desatualizada nunca entra silenciosamente no fluxo
de comunicação. Os downloads antigos do próprio módulo também ficam bloqueados
até uma execução atual. Seleções que continuam válidas são preservadas ao sair e
retornar ao módulo; variáveis removidas exigem nova escolha.

## Rascunho, prévia executada e resultado registrado

Mudar variável ou parâmetro altera somente o rascunho. O fluxo explícito é:

> **Configurar → Executar análise → Examinar a prévia → Adicionar aos resultados**

Para evitar que explorações passageiras encham o relatório, o usuário deve
clicar em:

> **Adicionar aos resultados**

Esse clique captura a base, os parâmetros e as saídas daquela execução. Depois,
a interface oferece **Atualizar resultado**, **Salvar como novo** e **Remover dos
resultados**.

### Exemplo: vários gráficos da mesma base

| ID | Base | X | Y | Título |
|---|---|---|---|---|
| `grafico_001` | `dados_analise` | ano | produção | Produção por ano |
| `grafico_002` | `dados_analise` | ano | esforço | Esforço por ano |
| `grafico_003` | `dados_analise` | ano | CPUE | CPUE por ano |

Trocar o eixo Y não sobrescreve gráfico já registrado. Cada clique em
**Adicionar aos resultados** cria uma execução independente.

Na Fase 3C, a lista e os controles aparecem dentro do próprio módulo analítico.
A Fase 3C.1 acrescenta a execução explícita antes desse registro e bloqueia as
ações de salvar enquanto o rascunho estiver pendente.
A visão conjunta, a ordenação e a escolha do conteúdo do Word pertencem à Fase
3D. O exportador consolidado antigo ainda usa seu rastreamento legado por visita
à aba e só será substituído pelo novo registro na Fase 3E.

## Comunicação de Resultados

O menu Comunicação deve ser orientado pelas **execuções registradas**, mostrando
qual base alimentou cada uma. Para cada execução, o usuário escolhe:

- narrativa automática;
- tabela;
- gráfico principal;
- pressupostos;
- diagnósticos;
- saída bruta do console, quando pedagogicamente necessária.

É indispensável separar:

1. **o que foi executado e preservado no Projeto R**;
2. **o que foi selecionado para aparecer no relatório Word**.

Uma verificação de normalidade pode ser executada e registrada, mas comunicada
apenas por texto, sem incluir o QQ-plot.

## Projeto R exportado

Estrutura-alvo:

```text
projeto_analise/
├── relatorio.qmd
├── README.md
├── custom-reference.docx
├── dados/
│   └── dados_brutos.xlsx
├── R/
│   ├── 00_importar.R
│   ├── 01_operacoes_estruturais.R
│   ├── 02_preparo_compartilhado.R
│   ├── 03_base_reg_logistica.R
│   ├── 03_base_qui_quadrado.R
│   ├── 04_analise_reg_logistica_01.R
│   ├── 04_grafico_producao_01.R
│   └── funcoes_relatorio.R
└── resultados/
```

Os scripts são ordenados por dependência, não pela ordem acidental dos cliques.
O `.qmd` consome as execuções registradas e aplica as escolhas de comunicação.

## Módulo Agrupar/Sumarizar

### Localização e integração

- módulo: `inst/app/modules/mod_agrupar_sumarizar.R`;
- menu: **Preparando Dados → Agrupar / Sumarizar**;
- integração: `inst/app/app.R`;
- entrada: `base_resolvida`;
- saída promovida: nova `base_resolvida`, antes do replay da Trilha.

### Escopo v1

- agrupar por uma ou mais variáveis;
- resumir uma ou mais variáveis numéricas;
- número de observações, soma, média, mediana, mínimo, máximo e desvio-padrão;
- remoção de `NA` nos cálculos;
- ordenação opcional pelos grupos;
- prévia da entrada e do resultado;
- geração e download do script R;
- download da tabela em CSV;
- promoção explícita para as análises.

O módulo alerta que a sumarização muda a unidade de análise. Resumos
condicionais, pesos amostrais e expressões arbitrárias ficam fora do escopo v1.

## Infográficos canônicos

As imagens finais serão armazenadas em:

```text
inst/app/www/infografico-pipeline-tratamento-dados.png
inst/app/www/infografico-comunicacao-resultados.png
```

**Estado atual:** imagens criadas e presentes nesses caminhos. A integração
delas às telas de ajuda da Trilha de Preparo e da Comunicação será feita junto à
revisão dessas interfaces.

### Prompt 1 — tratamento e preparação dos dados

> Crie um infográfico didático horizontal, em português brasileiro, para a IDE
> estatística CatalyseR. Mostre à esquerda “Dados brutos — originais e
> imutáveis”. Em seguida, represente as operações estruturais “Importar e
> visualizar”, “Arrumar”, “Calcular/Reescalar” e “Agrupar/Sumarizar”. Em
> Agrupar/Sumarizar, mostre várias linhas se transformando em uma linha por grupo,
> com o exemplo “ano + espécie → n, soma, média e desvio-padrão”. Depois mostre a
> “Trilha de Preparo Compartilhada”, ordenável e reproduzível, contendo selecionar,
> filtrar, tipar, tratar dados faltantes, recodificar, dicotomizar, padronizar,
> criar classes, remover duplicatas e padronizar texto. O produto central deve ser
> “Base compartilhada — dados_analise”, protegida por uma âncora ou cadeado. A
> partir dela, apresente dois caminhos: análises diretas e o botão “Criar base
> derivada”. Mostre exemplos de derivados em estrela: “base_reg_logistica”,
> “base_qui_quadrado” e “base_reg_multipla”. Deixe explícito: “Use dados_analise
> por padrão; crie uma base derivada somente quando houver preparo específico” e
> “Ramos específicos não alteram a base compartilhada”. Não mostre ramos de
> ramos. Inclua a frase “Cada clique gera código R reproduzível”. Estilo acadêmico
> moderno, limpo e elegante, fundo claro, formato 16:9, setas organizadas, pouco
> texto e português correto. Use a identidade Ocean Gradient: navy #0F3B5F,
> teal #2E7D8F, seafoam #62B6B7, amber #E89B3C e coral #E76F51. Tipografia
> semelhante a Cambria nos títulos e Calibri no corpo. Evite aparência infantil
> e excesso de elementos decorativos.

### Prompt 2 — análises, resultados e comunicação

Use depois do primeiro, pedindo continuidade visual:

> Crie o segundo infográfico da mesma série da CatalyseR, mantendo exatamente a
> identidade visual, paleta, tipografia e estilo do infográfico anterior. À
> esquerda, mostre um “Registro de Bases” contendo “dados_analise” e bases
> derivadas como “base_reg_logistica” e “base_qui_quadrado”. No centro, mostre o
> “Menu de Análise” com o dropdown “Base utilizada”, incluindo dados_analise como
> opção padrão. Represente que uma base pode alimentar várias execuções. Inclua o
> exemplo de uma mesma base e o mesmo eixo X “ano” gerando três resultados
> independentes: “Gráfico 1 — produção”, “Gráfico 2 — esforço” e “Gráfico 3 —
> CPUE”. Mostre a sequência “Rascunho de configuração → Executar análise →
> Prévia executada → Adicionar aos resultados”: mudar parâmetros deixa o
> rascunho pendente; executar gera a prévia; adicionar registra uma execução
> reproduzível. Em seguida, mostre um “Registro de Vínculos” com o
> fluxo “Base → Análise/Gráfico → Resultado registrado”. À direita, crie o painel
> “Comunicação de Resultados”, listando cada execução individualmente e oferecendo
> caixas de seleção para narrativa, tabela, gráfico, pressupostos e diagnósticos.
> Deixe claro: “Executado e preservado no Projeto R” é diferente de “Selecionado
> para o Relatório Word”. Mostre como saídas finais “Relatório Word” e “Projeto R
> (.zip + .qmd)”. Inclua a frase principal “Preparar a base → Vincular à análise
> → Registrar o resultado → Escolher o que comunicar”. Estilo acadêmico moderno,
> fundo claro, formato 16:9, setas simples e legíveis, português brasileiro
> correto. Use Ocean Gradient: navy #0F3B5F, teal #2E7D8F, seafoam #62B6B7,
> amber #E89B3C e coral #E76F51; Cambria nos títulos e Calibri no corpo. Não
> represente cada tipo de análise como se exigisse obrigatoriamente uma base
> derivada.

## Desdobramento pedagógico futuro — curso de treinamento

Depois que o pipeline estiver estabilizado e testado, a CatalyseR poderá ser
ensinada como um percurso completo: preparar uma planilha *tidy* no Excel,
importar, tratar, criar variáveis, escolher bases e variáveis, executar análises,
selecionar o que comunicar e gerar o relatório. O fechamento do curso deve
destacar o **Projeto R**: os cliques feitos na CatalyseR produziram código R
reproduzível, que o pesquisador poderá abrir em outra IDE e reconhecer como a
linguagem que estava trabalhando por trás da interface.

## Regras que não podem se perder

1. Dados brutos são preservados.
2. A Trilha compartilhada é replayável e gera `dados_analise`.
3. `dados_analise` é a base padrão de todas as análises.
4. Base derivada só existe para preparo específico e nasce em um salto.
5. Uma base pode alimentar várias execuções.
6. Alterar o rascunho exige nova execução e não sobrescreve resultado registrado.
7. O Projeto R preserva as execuções; o Word contém somente o que foi escolhido.
8. Toda transformação e análise deve gerar código R canônico.
