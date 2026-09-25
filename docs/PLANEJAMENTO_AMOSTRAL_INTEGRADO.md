# Planejamento amostral integrado — rumo do menu

O menu responde primeiro a duas perguntas: **o que conta como repetição independente?**
e **o que você quer estimar ou comparar?** Só então aparece um cálculo de n.
Cada opção abre uma tela própria e começa com uma frase que diz quando usá-la.
O Planejamento de Variáveis continua sendo a ponte leve para a planilha de coleta.

## Estado após o primeiro passo

- **Disponível:** unidade experimental e pseudorrepetição; n por poder para teste
  *t* de dois grupos independentes e ANOVA balanceada de um fator; AAS,
  estratificada proporcional e sistemática; croqui experimental; variáveis de
  coleta observacional e experimental.
- **Navegação:** as abas internas que agrupavam planejamento observacional e
  experimental foram retiradas. Cada opção existente tem um texto inicial curto.
  O cabeçalho **Antes de contar** abre o menu. Sua opção **Conceitos antes da
  coleta** é uma porta de entrada com seis temas na lateral direita: pergunta,
  unidade experimental e pseudorrepetição, n, amostragem,
  delineamento e responsabilidade. Cada tema traz um exemplo, um esquema curto
  e uma indicação do próximo passo no menu. **Como sortear a amostra** reúne os
  três sorteios sobre o marco amostral; **Delineamentos observacionais** deixa visíveis seus cinco
  percursos: transversal, longitudinal, comparativo, por gradiente e de impacto
  (CI, BA, BACI). Todos usam a mesma estrutura de quatro abas, e cada um
  reaproveita uma instância da ficha de variáveis existente. **Delineamentos
  experimentais** também deixa visíveis DIC, DBC, DQL, fatorial e parcelas
  subdivididas, todos apoiados pelo mesmo gerador de croqui e ficha. As camadas
  operacionais ficam recolhidas: **Quantos coletar** reúne os cálculos de n e
  **Como sortear a amostra** reúne AAS, AE e AS em seletores internos. No
  experimental, a aba **Variáveis do experimento** vem depois de **Croqui**,
  sem uma opção duplicada no menu.
- **Ainda não integrado:** uma ficha persistente do delineamento que viaje até
  as telas de análise. O n por poder não deve ser apresentado como sugestão
  automática de teste enquanto essa ligação não existir.

## Como sortear a amostra (set/2026, v1)

Ferramenta de antes da coleta, distinta de "Sortear subamostra" em Preparar
Dados: aqui não há dado medido, só o **marco amostral** (uma linha por unidade
candidata, identificador obrigatório, demais colunas como atributos).

- **Entrada:** importar CSV/Excel, colar ou digitar, ou gerar de 1 a N. As três
  preenchem a mesma tabela, editável na tela (células, linhas, colunas novas
  como estrato ou ordem). Editar o marco invalida o sorteio anterior.
- **Métodos:** aleatória simples (com correção para população finita opcional),
  estratificada proporcional (alocação automática por maiores restos) e
  sistemática (coluna de ordem, intervalo k, partida sorteada). Semente
  obrigatória. Ordenação independente do idioma do sistema, para a mesma
  semente dar o mesmo sorteio no Windows e no Linux.
- **Conglomerados (em dois estágios):** coluna de conglomerado no marco (a
  embarcação ou o desembarque de cada peixe). 1º estágio sorteia grupos
  inteiros; 2º estágio mede todas as unidades (estágio único) ou sorteia m por
  grupo (dois estágios). Uma semente cobre os dois estágios em sequência; com a
  mesma semente, os grupos do 1º estágio são os mesmos nas duas opções.
  Grupo menor que m entra inteiro, com aviso. Quando sai mais de uma unidade
  por grupo, a tela avisa o aninhamento e a ficha registra `aninhamento`
  (conglomerado como efeito aleatório), `unidade_coluna` = conglomerado,
  `subamostra_coluna` = identificador e `analise_sugerida =
  "anova_mista_subamostras"`, o mesmo contrato da ANOVA com subamostras.
- **Função única:** `templates/funcoes_sorteio.R` (`sortear_marco()`), usada pela
  tela, copiada para o projeto R exportado e pronta para os delineamentos
  (aceita `alocacao = "igual"` para n por espécie no comparativo).
- **Saída:** sorteados com as colunas do marco, registro do sorteio e código R
  que o refaz. Em seguida, o Planejamento de Variáveis (modo `sorteio`, leve)
  gera a planilha de coleta (estruturais preenchidas + respostas em branco),
  com o dicionário e o registro em abas próprias. A ficha enviada às análises
  leva o tipo de cada variável e o registro do sorteio.
- **Delineamentos observacionais:** a aba Variáveis de coleta usa o último
  sorteio desta tela (substitui a antiga escolha AAS/AEP/AS).
- **Fica para depois:** sortear pontos dentro de uma área; sorteio embutido no
  Plano amostral de cada delineamento; validação da faixa dentro do Excel;
  leitura automática do dicionário ao importar a planilha de coleta.
- **Legado a decidir:** `templates/funcoes_sampling.R` e
  `relatorio_sampling.qmd` (estimadores de média e total a partir de dados já
  medidos) só são chamados pela exportação consolidada antiga, cujas abas
  "Amostrando uma AAS/AE/AS" não existem mais no menu.

## Delineamentos observacionais — primeiro recorte

As cinco opções já são navegáveis, mas **O delineamento** e **Plano amostral**
ainda explicam a estrutura sem registrar uma declaração; **Ficha do
delineamento** ainda é um destino sinalizado, não um resultado. Somente
**Variáveis de coleta** reutiliza a funcionalidade existente. Isso evita
apresentar como pronta uma recomendação de análise que ainda não foi ligada
ao eixo e à hierarquia do estudo.

Próximo passo coeso: declarar pergunta, resposta, eixo, sítio independente e
subamostra num único formulário comum; então gerar o plano de coleta tidy e
contar sítios como n verdadeiro. O croqui de interspersão espacial agora torna
visível a distribuição declarada dos sítios nos desenhos comparativo, por
gradiente e de impacto; ele não finge ser um mapa geográfico. A ponte com
biomassa/pools e a recomendação de Welch/Games-Howell dependem dessa ficha e
ficam para etapas posteriores. O planejador de biomassa citado pelo autor
ainda não foi localizado no repositório; não deve ser anunciado como
integração pronta.

## Próximos passos pequenos

1. **Ficha do delineamento:** um registro por sessão com pergunta, método usado
   para n, n por grupo/estrato, número de indivíduos e número de unidades
   independentes, unidade experimental, pool (sim/não), tamanho dos pools,
   desenho misto (sim/não), premissas e alerta de análise. A ficha deve ter
   confirmação explícita do usuário; não deve inferir unidade experimental
   apenas pelo nome de uma coluna. Deve sugerir as colunas mínimas da coleta
   (por exemplo, `id_tanque`, `id_peixe`, `tratamento`, `id_pool`) e encaminhá-las
   ao Planejamento de Variáveis, onde o usuário pode ajustá-las.
2. **Biomassa e pool:** conferir o esboço `mod_n_biomassa.R` antes de integrá-lo.
   Expor separadamente animais coletados, pools formados e observações
   independentes. O arquivo citado ainda não foi localizado no ambiente.
3. **Estimar uma média:** margem de erro, confiança e desvio-padrão esperado.
   Texto inicial: “Use quando sua pergunta é o valor médio de uma população,
   sem comparar grupos. Escolha a precisão que faria diferença na decisão.”
4. **Estimar uma proporção:** precisão, confiança e proporção esperada. Se não
   há estimativa prévia, oferecer 0,5 com explicação. Texto inicial: “Use para
   prevalência, maturidade ou proporção sexual quando a pergunta é uma fração
   da população.”
5. **População finita:** opção explicativa própria e um controle compartilhado
   nos dois cálculos de estimação. A mesma fórmula deve alimentar as três
   telas, evitando resultados divergentes. Usar somente quando o total de
   unidades elegíveis é conhecido e a amostragem é sem reposição.
6. **Estratos:** acrescentar alocação proporcional e de Neyman ao planejamento
   já existente. Neyman pede estimativas de variabilidade por estrato; se não
   houver, apresentar só a alocação proporcional.
7. **Peso-comprimento e crescimento:** definir primeiro o alvo do cálculo
   (precisão de um coeficiente, faixa de comprimentos ou poder para detectar
   uma relação). Não existe um n único para qualquer regressão ou curva.
8. **Curva do coletor:** tratar como avaliação de suficiência durante ou após
   a amostragem, com unidades amostrais e riqueza observada; a curva não fornece
   sozinha um n prospectivo universal.

## Regra para a ponte com a análise

A ficha pode **sugerir** um caminho e mostrar por quê. Ela não escolhe o teste
nem muda os dados. Se a ficha disser que houve pools ou desenho misto, a tela de
análise precisa reconhecer essa informação antes de oferecer “Executar”. A
ANOVA atual usa `aov()` e `TukeyHSD()`: ela ainda não executa Welch e
Games-Howell. A troca para esses métodos só resolve variâncias desiguais entre
observações independentes; não corrige, por si, pseudorrepetição, dependência
entre animais do mesmo tanque ou comparações entre medidas de natureza distinta.

## Critério para os próximos acréscimos

Uma opção nova entra quando (1) sua pergunta é distinta das opções atuais,
(2) o n corresponde à unidade que de fato será analisada, (3) suas premissas
são explicadas antes dos controles e (4) sua conclusão pode ser lida na ficha
e reconhecida na análise sem duplicar estado. A prioridade é o menor n válido,
em coerência com os 3Rs e com a pergunta científica.

## Referências de cálculo e desenho

- R `stats`: [power.t.test](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/power.t.test.html)
  e [power.anova.test](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/power.anova.test.html).
- [ARRIVE Guidelines — unidade experimental](https://arriveguidelines.org/arrive-guidelines/study-design/1b/explanation).
- [NC3Rs — tamanho de grupos e poder](https://eda.nc3rs.org.uk/experimental-design-group).
