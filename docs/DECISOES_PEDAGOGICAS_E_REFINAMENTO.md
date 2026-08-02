# Decisões pedagógicas e de refinamento da CatalyseR

**Status:** adotadas
**Última consolidação:** 02/08/2026

## 1. Estágio do projeto

A CatalyseR é um projeto maduro em sua infraestrutura principal. O percurso
importar → preparar → criar bases derivadas → analisar → comunicar → exportar
Projeto R → executar no RStudio → renderizar Word já foi homologado de ponta a
ponta.

O foco atual não é ampliar rapidamente o catálogo. É aprimorar a qualidade
estatística, visual e pedagógica das análises que já existem.

## 2. Papel pedagógico

A CatalyseR não deve substituir o aprendizado de R nem transformar a análise
numa caixa-preta. Deve reduzir a barreira inicial, revelar o código
correspondente às escolhas da interface e preparar o aluno para trabalhar com
autonomia no RStudio.

Princípio central:

> A CatalyseR faz junto, mostra como fez e entrega o código para que o aluno
> consiga fazer sozinho.

A interface é um corrimão. O destino continua sendo a compreensão científica e
a capacidade de ler, adaptar e executar código.

## 3. Refinamento em ciclos de duas análises

Trabalhar em exatamente duas análises por ciclo. Levar a dupla a um bom nível de
refinamento antes de iniciar outra.

Cada ciclo deve tratar:

- método e validações;
- resultados indispensáveis;
- pressupostos e diagnósticos;
- narrativa científica em português;
- tabelas e gráficos;
- código R visível;
- `relatorio.qmd`;
- relatório Word;
- replay fora da CatalyseR;
- testes automatizados e homologação humana.

O aprendizado obtido numa dupla deve ser aplicado à dupla seguinte. Padrões
novos e comprovados podem retornar às duplas anteriores. Esse movimento é um
refinamento em espiral, não uma sequência rígida e irreversível.

### 3.1. Fluxo oficial de cada ciclo e entrega dupla

O caminho de desenvolvimento de uma análise é:

1. aprimorar o método, as validações e as saídas da análise;
2. criar ou consolidar sua função pública `catalyser_*`, documentada no pacote;
3. integrar a mesma função à CatalyseR e ao replay do Projeto R;
4. testar a análise na IDE como um usuário, usando o pacote instalado;
5. acompanhar sua evolução no `relatorio.qmd` e no Word;
6. executar testes, `R CMD build` e `R CMD check` proporcionais à mudança;
7. commitar e enviar a versão instalável à `main`;
8. atualizar o projeto canônico de teste rápido, com bases e análises prontas,
   e renderizar seu Word.

O preparo de dados e a arquitetura de Base Compartilhada/Bases Derivadas estão
consolidados. Não reabrir esse subsistema durante os ciclos de análise, salvo
para corrigir um defeito comprovado. Quando uma análise exigir preparo
específico, usar os tratamentos e as receitas de base derivada já existentes.

Cada ciclo só é apresentado como pronto quando houver **duas entregas
verificadas**: (a) CatalyseR instalável pela `main` com `remotes` e (b) Projeto R
de teste rápido com o cenário preparado e o Word renderizado. Informar
explicitamente ao autor quando ambas estiverem disponíveis.

## 4. Código humano como produto

O script R numerado deve se aproximar do que uma pessoa escreveria:

1. indicar pergunta, base e análise;
2. carregar os dados;
3. declarar variáveis;
4. construir fórmula ou mapeamento;
5. executar a função R canônica;
6. nomear objetos intermediários;
7. exibir resultados;
8. separar a integração técnica com o relatório.

`dput()` extenso, dispatchers e ambientes internos podem existir para garantir
reprodutibilidade, mas não devem ocupar a superfície pedagógica principal.

O QMD deve mostrar a relação entre código, objetos, tabelas, gráficos e
narrativa. O código de estudo deve ser copiável e executável.

## 4.1. O caminho dos dados no projeto exportado

Decisão adotada para a estrutura do Projeto R:

1. A **planilha bruta** é exportada com o projeto e é o ponto de entrada. O
   projeto começa onde a análise começou.
2. **Um único script** — `R/01_base_compartilhada.R` — produz `dados_analise`.
   Ele percorre planilha → operações estruturais → trilha de tratamentos e
   **confere** o resultado contra a fotografia exportada, avisando se divergir.
   O relatório não constrói a Base Compartilhada: apenas chama esse script.
3. **Cada análise constrói a sua base derivada** num chunk do `.qmd`, com a
   receita visível no fonte e silenciosa no Word (`echo: false`). Não há mais
   script `03_` por base derivada — a receita vive num só lugar.
4. Os scripts `R/02_execucao_*` continuam, um por execução, para estudar ou
   rodar uma análise isolada fora do Quarto. Eles reconstruem a Base
   Compartilhada e, quando necessário, a receita da base derivada.
5. Quando há operação estrutural promovida, o script usa a fotografia da base
   resolvida e mantém o código estrutural como referência comentada. Motivo
   honesto: cada bloco promovido recomeça pela leitura da planilha, então blocos
   acumulados não rodam em sequência. Consertar isso nos módulos estruturais é
   trabalho de um ciclo próprio; até lá, a conferência protege o resultado.
6. **A pasta `dados/` é enxuta.** No caso comum ela tem três arquivos, cada um
   com um papel distinto: a planilha bruta (entrada), `dados_analise.rds`
   (conferência) e `base_compartilhada.xlsx` (entrega para fora do R).
   `base_resolvida.rds` aparece só quando houve mudança estrutural. Não há cópia
   dos dados brutos, `.csv` redundante nem fotografia de base derivada — o que o
   projeto sabe reconstruir, ele reconstrói. Consequência assumida: `readxl`
   passa a ser exigido, com mensagem de instalação clara.

## 4.2. Apresentação dos resultados da ANOVA

Formato aprovado em 28/07/2026, depois de conferido no Word. Serve de referência
para as próximas análises com comparação entre grupos:

1. **Gráfico de barras**, não boxplot. Barras com a média de cada grupo, hastes
   com o IC 95% da média e **letras de diferença** acima delas. O eixo Y começa
   em zero, porque em barras o que se compara é o comprimento; o piso é
   desativado quando há média negativa, para não mentir.
2. **Letras compactas** derivadas dos p ajustados de Tukey: grupos que
   compartilham uma letra não apresentaram evidência de diferença; a letra "a"
   fica com a maior média. O algoritmo é escrito no projeto — não se adota
   `multcompView` — porque o Projeto R exportado não pode exigir pacote fora do
   `Imports`.
3. **Tabela de resultados**: `Grupo | n | Média ± DP | IC da média | Diferença`.
   Mediana, mínimo e máximo saíram: competiam com a leitura das letras sem
   acrescentar decisão.
4. **Console fora do relatório.** A saída bruta continua disponível na interface,
   para o aluno reconhecer o R sem a camada da CatalyseR, mas não é oferecida
   como conteúdo do Word ou do QMD.

## 5. Interface e código livre

Não oferecer caixa para digitação de código R livre dentro da CatalyseR.

É desejável manter abas de código somente leitura que mostrem como o resultado
foi construído. A liberdade de edição pertence ao Projeto R exportado e ao
RStudio.

## 6. Inclusão de controles

Antes de adicionar um controle, responder:

1. É utilizado com frequência?
2. Ajuda a comunicar ou interpretar o resultado?
3. Gera código simples, legível e adaptável?
4. Seu uso incorreto pode distorcer a interpretação científica?

Se não houver justificativa suficiente, deixar a opção apenas no código
exportado.

Adicionar no máximo dois controles visuais por rodada à coluna lateral de um
gráfico. Evitar transformar a interface num catálogo de todos os argumentos do
`ggplot2`.

Subtítulo e fonte/observação permanecem no código porque `ggplot2::labs()` é
fácil de aprender. Ajustes de escala dos eixos X e Y são candidatos mais
relevantes, mas só devem ser implementados após observação dos pilotos.

Quando escalas personalizadas forem incluídas:

- manter automático como padrão;
- avisar quando observações ficarem fora da faixa visível;
- preferir `coord_cartesian()` para aproximação sem descartar dados;
- iniciar o eixo Y em zero em gráficos de barras;
- registrar a configuração no script, QMD e replay.

### 6.1. Personalização posterior no Projeto R

Registrar como evolução planejada a possibilidade de o usuário ajustar a
apresentação depois da exportação, diretamente no Projeto R. A prioridade é a
**formatação dos gráficos**; em seguida, a das tabelas.

Nos gráficos, o código exportado deve facilitar alterações de tema, paleta,
rótulos, escalas, limites visuais, tamanhos, posição da legenda e dimensões da
figura. Nas tabelas, deve permitir ajustar colunas exibidas, títulos, casas
decimais, alinhamento, largura e tema visual sem refazer o cálculo estatístico.

Primeiro preservar essa liberdade em código humano, legível e localizado no
Projeto R. Só promover para controles da interface as opções frequentes,
pedagogicamente úteis e estatisticamente seguras. A personalização visual não
deve mudar silenciosamente dados, modelo, estimativas ou decisões inferenciais.

## 7. Mini-refatorações

Solicitar e realizar pequenas refatorações durante cada ciclo quando elas:

- reduzirem duplicação;
- separarem cálculo e apresentação;
- centralizarem rótulos ou geradores;
- removerem dependências globais;
- tornarem testes e fixtures reutilizáveis;
- facilitarem a leitura do código exportado.

Executar uma mini-refatoração por vez, com teste e sem alterar silenciosamente
resultados ou regras estatísticas. Adiar reescritas arquiteturais amplas.

## 8. Relação com o livro EAPA

A CatalyseR continua sendo a fonte da verdade das análises. Depois que uma
análise atingir estabilidade, o livro deve explicar por que, quando e como
interpretá-la usando o mesmo código canônico.

Fluxo desejado:

> A CatalyseR mostra como fazer; o livro explica por que fazer, quando fazer e
> como interpretar.

As dificuldades pedagógicas encontradas ao escrever o livro devem retornar como
melhorias de interface, código, narrativa ou exemplos na CatalyseR.

## 9. Preparação de dados

Preservar o menu **Preparando Dados** como uma cozinha/oficina de dados:

- **Pivotar e Separar Dados** organiza a forma da tabela;
- **Organizar Variáveis** reúne criação e arrumação de variáveis;
- **Adicionar Tratamentos à Base** reúne Tratamentos e trilha e a Checagem
  Final da Base Compartilhada;
- a Base Compartilhada registra mudanças estruturais e tratamentos gerais em
  camadas técnicas distintas;
- as Bases Derivadas preparam finalidades específicas;
- cada análise recebe a base adequada;
- a trilha torna o percurso reproduzível e visível.

Não confundir o rótulo pedagógico “Adicionar Mudança à Trilha da Base
Compartilhada” com `pipeline_rv`: as mudanças estruturais formam
`base_resolvida`; os seis tratamentos compartilhados são reproduzidos depois,
em `pipeline_rv`.

Não enfraquecer a correspondência já funcional entre bases e análises durante o
refinamento das saídas.

## 10. Instalação e ambientes de teste

O Windows 11 do autor é o ambiente cotidiano de teste. Não presumir uso de VM
nem fornecer instruções específicas de VM sem indicação explícita.

Periodicamente, fazer instalação limpa ou atualização pela `main` em uma VM para
verificar:

- instalador da CatalyseR;
- instalação/atualização do EAPADados;
- abertura no navegador;
- pipeline completo;
- exportação do Projeto R;
- renderização do Word.

Essas verificações complementam, mas não substituem, testes automatizados,
execução externa dos scripts, `R CMD build` e `R CMD check`.

## 11. Estado das primeiras duplas

A primeira dupla de pilotos é:

- ANOVA de um fator;
- gráfico de linhas com duas execuções independentes após troca do eixo Y.

Na versão **0.1.5**, essa dupla concluiu a primeira consolidação do padrão de
código humano: scripts numerados e QMD compartilham o mesmo gerador, a
configuração técnica fica separada em metadados e o método principal aparece em
código R legível. O pipeline, o replay externo e o Word foram novamente
homologados.

Um segundo ciclo, ainda na 0.1.5, tratou as saídas dessa mesma dupla e fixou
quatro decisões:

1. **A narrativa não repete o que a tabela ao lado mostra.** Ela dá pergunta,
   amostra, exclusões, decisão, tamanho de efeito e síntese dos pares — e remete
   ao resumo por grupo e à tabela de pressupostos para o resto.
2. **Interface e replay contam a mesma história.** A narrativa do Word nasce do
   replay; quando as duas redações divergem, o relatório contradiz a tela. Elas
   são mantidas como textos irmãos, com teste comparando os marcadores.
3. **Número exibido é número em português.** Vírgula decimal e `< 0,001` em todas
   as tabelas, com casas decimais escolhidas pelo significado da coluna.
4. **Exclusão de dado faltante é sempre contada.** Nenhuma análise da dupla
   descarta linha em silêncio — nem a ANOVA, nem o gráfico de linhas, que antes
   dependia do aviso discreto do `ggplot2`.

Convenções de efeito (Cohen para η²) podem ser exibidas como leitura, sempre
rotuladas como convenção estatística, nunca como interpretação biológica.

O ciclo permanece aberto para refinar as saídas estatísticas, visuais e
pedagógicas da própria dupla. A seleção da próxima dupla deve ocorrer somente
depois de observar cuidadosamente tabelas, narrativas, pressupostos, gráficos e
relatório desses pilotos.
