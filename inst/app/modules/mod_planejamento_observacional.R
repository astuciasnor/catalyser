# Módulo de Planejamento de Delineamentos Observacionais — CatalyseR
# Unificação: Transversal Comparativo com tratamento de Amostras Compostas (Pool).
# Estrutura em 4 abas limpas:
# 1. O delineamento e Variáveis de resposta (dividida em 3 colunas equilibradas)
# 2. Ficha do delineamento (tabela tidy completa de coleta + 4 botões de exportação abaixo)
# 3. Metodologia para artigo (texto pré-formatado para Material e Métodos)
# 4. Modelo e cuidados (modelo estatístico recomendado e cuidados de coleta/análise)

catalogo_delineamentos_observacionais <- function() {
  list(
    transversal_comparativo = list(
      titulo = "Transversal comparativo",
      definicao = "Compara grupos preexistentes na natureza (espécies, sexos, locais ou fases) em um mesmo recorte temporal, usando unidades individuais ou amostras compostas (pool).",
      quando = "No estudo de bexigas natatórias ou desembarques pesqueiros, espécies ou categorias são níveis escolhidos de propósito; o sorteio alcança indivíduos elegíveis dentro de cada grupo. Se o tecido exigir pool, a réplica da medida será o pool independente (cada UA ocupa uma linha), e não cada peixe individual.",
      eixo = "Grupos preexistentes em um só momento",
      alerta = "Se as unidades amostrais (UAs) tiverem números diferentes de itens no pool conforme o grupo, a variância da média diminui com o tamanho do pool (Var = s²/k), gerando heterocedasticidade estrutural. Nesse caso, a análise indicada é a ANOVA de Welch com pós-teste de Games-Howell."
    ),
    gradiente = list(
      titulo = "Estudo de gradiente",
      definicao = "Estuda como uma resposta varia ao longo de uma faixa contínua (distância de uma fonte, salinidade, contaminação, altura na maré). Cada estação é uma unidade amostral, e a análise é uma regressão da resposta contra a variável do gradiente.",
      quando = "Quando a pergunta é como a resposta muda ao longo de uma variação contínua, como a abundância do caranguejo com a distância do manguezal, e não comparar grupos.",
      eixo = "Valor contínuo medido em cada estação independente",
      alerta = "Arrastos ou subamostras da mesma estação não aumentam o n. Distribua as estações por toda a faixa do gradiente, não apenas nos extremos."
    ),
    longitudinal = list(
      titulo = "Longitudinal Comparativo",
      definicao = "Acompanha as mesmas unidades em momentos sucessivos. O tempo é o eixo; cada unidade mantém seu identificador em todas as visitas.",
      quando = "Quando você quer saber como o crescimento de um peixe ou a condição de um tanque muda ao longo do tempo.",
      eixo = "Tempo, com as mesmas unidades repetidas",
      alerta = "Medições repetidas da mesma unidade não são réplicas independentes. A planilha deve ficar em formato longo: uma linha por unidade e momento."
    ),
    impacto = list(
      titulo = "De impacto (CI, BA, BACI)",
      definicao = "Reúne três perguntas sobre o efeito de uma intervenção nas condições do ambiente: Controle-Impacto compara sítios impactados e de referência; Antes-Depois compara o mesmo sítio antes e depois; BACI combina os dois. No BACI, o sinal de interesse é a interação entre local e tempo.",
      quando = "Quando a pergunta é o efeito de uma intervenção sobre condições do ambiente — um efluente de aquicultura, uma dragagem, uma barragem a montante — medida em sítios impactados e de referência, antes e depois da mudança.",
      eixo = "Local, tempo ou o cruzamento local × tempo",
      alerta = "Um único sítio de impacto não representa vários sítios independentes. Medidas antes e depois no mesmo sítio também permanecem relacionadas."
    )
  )
}

eixos_padrao_observacional <- function(tipo) {
  if (tipo %in% c("transversal", "comparativo", "transversal_comparativo")) return("grupos")
  switch(tipo,
    longitudinal = "tempo",
    gradiente = "gradiente",
    impacto = c("impacto", "tempo"),
    character()
  )
}

planejamento_observacional_painel <- function(tipo) {
  if (tipo %in% c("transversal", "comparativo")) tipo <- "transversal_comparativo"
  definicao <- catalogo_delineamentos_observacionais()[[tipo]]
  if (is.null(definicao)) stop("Delineamento observacional desconhecido.", call. = FALSE)
  bslib::nav_panel(
    title = definicao$titulo,
    icon = shiny::icon("clipboard-list"),
    mod_planejamento_observacional_ui(
      paste0("obs_", tipo), tipo,
      variaveis_ui = NULL
    )
  )
}

# ---- INTERFACE DO MÓDULO -----------------------------------------------------

mod_planejamento_observacional_ui <- function(id, tipo, variaveis_ui = NULL) {
  ns <- shiny::NS(id)
  tipo_resolvido <- if (tipo %in% c("transversal", "comparativo")) "transversal_comparativo" else tipo
  definicao <- catalogo_delineamentos_observacionais()[[tipo_resolvido]]
  if (is.null(definicao)) stop("Delineamento observacional desconhecido.", call. = FALSE)

  estilo <- shiny::tags$style(shiny::HTML("
    .obs-estudio .card, .obs-estudio .card-body, .obs-estudio .tab-content, .obs-estudio .tab-pane {
      overflow: visible !important; height: auto !important; min-height: 0; }
    .obs-estudio .card-body { display: block !important; padding: 16px 18px; }
    .obs-coluna-bloco { display: flex; flex-direction: column; gap: 12px; }
    .obs-card-interno {
      background: #ffffff;
      border: 1px solid #dbe5e8;
      border-radius: 10px;
      padding: 14px 16px;
      box-shadow: 0 1px 3px rgba(15, 59, 95, 0.03);
    }
    .obs-card-interno h5 {
      color: #0F3B5F;
      font-size: 0.95rem;
      font-weight: 700;
      margin-bottom: 8px;
    }
    /* Variante compacta da coluna 1: rótulos e margens apertados para ganhar
       espaço vertical sem mudar o contrato dos inputs. */
    .obs-estudio .obs-apertado .shiny-input-container { margin-bottom: 6px; }
    .obs-estudio .obs-apertado .control-label { font-size: 0.83rem; margin-bottom: 2px; }
    .obs-estudio .obs-apertado .form-control { padding: 3px 8px; font-size: 0.87rem; height: auto; }
    .obs-estudio .obs-apertado .shiny-input-radiogroup .radio { margin: 1px 0; }
    .obs-estudio .obs-apertado .shiny-input-radiogroup .radio label { font-size: 0.86rem; }
    .obs-pool-grid {
      display: grid;
      grid-template-columns: repeat(auto-fill, minmax(130px, 1fr));
      gap: 10px;
      margin-top: 8px;
    }
    .obs-botoes-exportacao .btn {
      font-weight: 600;
      font-size: 0.88rem;
      padding: 8px 12px;
    }
    /* Linhas compactas das variáveis de resposta: uma embaixo da outra, sem respiro extra */
    .obs-var-cab {
      display: flex; align-items: center; gap: 8px;
      padding: 0 8px; margin-bottom: 2px;
    }
    .obs-var-linha {
      display: flex; align-items: center; gap: 8px;
      padding: 3px 8px; margin-bottom: 4px;
    }
    .obs-var-linha .shiny-input-container { margin-bottom: 0 !important; width: 100%; }
    .obs-var-linha .form-control { padding: 2px 8px; font-size: 0.85rem; height: auto; }
    .obs-var-rotulo {
      min-width: 16px; text-align: right;
      font-weight: 700; font-size: 0.85rem; color: #0F3B5F;
    }
    /* Paginação e contador da tabela tidy encostados na tabela, sem respiro extra */
    .obs-estudio .dataTables_wrapper table.dataTable { margin-bottom: 0 !important; }
    .obs-estudio .dataTables_wrapper .dataTables_info { padding-top: 2px !important; }
    .obs-estudio .dataTables_wrapper .dataTables_paginate {
      margin-top: 0 !important; padding-top: 0 !important;
    }
    .obs-estudio .dataTables_wrapper > .row:last-child { margin-top: 0 !important; }
  "))

  shiny::div(class = "obs-estudio",
    estilo,
    shiny::uiOutput(ns("planejamento_atual")),
    shiny::div(class = "alert alert-light border mb-2 py-2 px-3",
      style = "border-left: 4px solid #2E7D8F !important;",
      shiny::tags$b(definicao$titulo), shiny::tags$br(),
      definicao$definicao
    ),
    bslib::navset_card_tab(
      id = ns("etapas"),
      
      # ABA 1: O DELINEAMENTO E VARIÁVEIS DE RESPOSTA (3 COLUNAS)
      bslib::nav_panel("O delineamento e Variáveis de resposta", icon = shiny::icon("compass-drafting"),
        bslib::card_body(fillable = FALSE,
          bslib::layout_columns(
            col_widths = c(4, 4, 4),
            gap = "20px",

            # COLUNA 1: Delineamento
            shiny::div(class = "obs-coluna-bloco",
              # No estudo de gradiente o eixo é contínuo: no lugar de fator e
              # níveis entram a variável do gradiente e as estações da faixa.
              if (identical(tipo_resolvido, "gradiente")) {
                shiny::tagList(
                  shiny::div(class = "obs-card-interno obs-apertado",
                    shiny::h5(shiny::icon("water"), " 1. Definição do gradiente"),
                    # Pergunta do estudo, já com um exemplo de gradiente.
                    shiny::textInput(
                      ns("pergunta"), "Pergunta do estudo:",
                      value = "A abundância do caranguejo diminui com a distância do manguezal?",
                      placeholder = "O que você deseja responder com este estudo?",
                      width = "100%"
                    ),
                    # Variável e unidade dividem a mesma linha: a unidade é
                    # curta e não precisa de uma linha inteira.
                    bslib::layout_columns(
                      col_widths = c(8, 4), gap = "10px",
                      # Nome da variável contínua; vira o nome da coluna na planilha.
                      shiny::textInput(
                        ns("gradiente_nome"), "Variável do gradiente (nome da coluna):",
                        value = "distancia_fonte",
                        placeholder = "Ex.: distancia_fonte, salinidade, altura_mare",
                        width = "100%"
                      ),
                      # Unidade do gradiente; vai ao dicionário e aos textos.
                      shiny::textInput(
                        ns("gradiente_unidade"), "Unidade de medida:",
                        value = "m",
                        placeholder = "Ex.: m, ‰, mg/kg",
                        width = "100%"
                      )
                    )
                  ),
                  shiny::div(class = "obs-card-interno obs-apertado",
                    shiny::h5(shiny::icon("signs-post"), " 2. Estações ao longo da faixa"),
                    # Duas formas de definir as estações; gerar espaçadas é o padrão.
                    # Rótulos curtos para não quebrar linha na coluna estreita.
                    shiny::radioButtons(
                      ns("modo_estacoes"), "Como definir os valores das estações?",
                      choices = c(
                        "Gerar estações espaçadas (padrão)" = "igual",
                        "Digitar valores livremente" = "livre"
                      ),
                      selected = "igual",
                      inline = FALSE
                    ),
                    # No modo gerado, a CatalyseR distribui os valores com seq().
                    # Início, fim e número de estações dividem uma única linha.
                    shiny::conditionalPanel(
                      condition = sprintf("input['%s'] == 'igual'", ns("modo_estacoes")),
                      bslib::layout_columns(
                        col_widths = c(4, 4, 4), gap = "10px",
                        shiny::numericInput(
                          ns("estacao_inicio"), "Início da faixa:",
                          value = 50, step = 1, width = "100%"
                        ),
                        shiny::numericInput(
                          ns("estacao_fim"), "Fim da faixa:",
                          value = 400, step = 1, width = "100%"
                        ),
                        shiny::numericInput(
                          ns("n_estacoes"), "Nº de estações:",
                          value = 8, min = 2, step = 1, width = "100%"
                        )
                      )
                    ),
                    # No modo livre, a planilha usa os valores digitados em ordem crescente.
                    shiny::conditionalPanel(
                      condition = sprintf("input['%s'] == 'livre'", ns("modo_estacoes")),
                      shiny::textInput(
                        ns("valores_livres"), "Valores das estações (separados por vírgula):",
                        value = "50, 100, 150, 200, 250, 300, 350, 400",
                        placeholder = "Ex.: 50, 100, 150, 200, 250",
                        width = "100%"
                      )
                    ),
                    # Lembretes de desenho amostral em texto pequeno.
                    shiny::p(class = "small text-muted mb-0",
                      "Distribua as estações por toda a faixa do gradiente, e não apenas nos extremos. Espace-as para que estações vizinhas não fiquem parecidas só pela proximidade."
                    )
                  )
                )
              } else {
                shiny::tagList(
              # Delineamentos de impacto: o formulário pede a condição dos
              # sítios (impactado × referência) e, conforme o tipo, os momentos
              # antes/depois — com exemplos de ambiente, não de bancada.
              if (identical(tipo_resolvido, "impacto")) {
                shiny::div(class = "obs-card-interno",
                  shiny::h5(shiny::icon("sliders"), " 1. Definição do impacto e dos sítios"),
                  shiny::textInput(
                    ns("pergunta"), "Pergunta do estudo:",
                    value = "A qualidade da água mudou depois da instalação dos tanques-rede, em comparação com trechos de referência?",
                    placeholder = "O que você deseja responder com este estudo?",
                    width = "100%"
                  ),
                  shiny::radioButtons(
                    ns("tipo_impacto"), "Qual a pergunta de impacto?",
                    choices = c(
                      "Controle–Impacto (CI): comparar sítios depois da mudança" = "ci",
                      "Antes–Depois (BA): comparar o mesmo sítio antes e depois" = "ba",
                      "BACI: antes–depois com sítios de controle (mais seguro)" = "baci"
                    ),
                    selected = "baci",
                    inline = FALSE
                  ),
                  # No BA (antes–depois) o próprio sítio é o seu controle, então
                  # a condição dos sítios não se aplica: escondemos o campo.
                  shiny::conditionalPanel(
                    condition = sprintf("input['%s'] !== 'ba'", ns("tipo_impacto")),
                    shiny::textInput(
                      ns("fator_nome"), "Nome da coluna da condição do sítio:",
                      value = "situacao",
                      placeholder = "Ex.: situacao, condicao, trecho",
                      width = "100%"
                    ),
                    shiny::textInput(
                      ns("fator_niveis"), "Condições dos sítios (separadas por vírgula):",
                      value = "Impacto, Referência",
                      placeholder = "Ex.: Impacto, Referência",
                      width = "100%"
                    )
                  ),
                  shiny::numericInput(
                    ns("n_uas"), "Sítios independentes por condição:",
                    value = 3, min = 1, step = 1, width = "100%"
                  )
                )
              } else {
              shiny::div(class = "obs-card-interno",
                shiny::h5(shiny::icon("sliders"), " 1. Definição do Fator e Amostragem"),
                shiny::textInput(
                  ns("pergunta"), "Pergunta do estudo:",
                  value = "Qual a composição lipídica das bexigas natatórias entre espécies de peixes?",
                  placeholder = "O que você deseja responder com este estudo?",
                  width = "100%"
                ),
                shiny::textInput(
                  ns("fator_nome"), "Nome do fator ou categoria:",
                  value = "especie",
                  placeholder = "Ex.: especie, sexo, local, estagio",
                  width = "100%"
                ),
                shiny::textInput(
                  ns("fator_niveis"), "Níveis ou grupos (separados por vírgula):",
                  value = "Tambaqui, Gurijuba, Pescada Amarela, Pargo, Camurupim",
                  placeholder = "Ex.: Tambaqui, Gurijuba, Pescada Amarela",
                  width = "100%"
                ),
                shiny::numericInput(
                  ns("n_uas"), "Unidades amostrais (UAs / réplicas) por grupo:",
                  value = 5, min = 1, step = 1, width = "100%"
                )
              )
              },
              # Cartão de momentos: o longitudinal acompanha unidades repetidas;
              # o impacto BA/BACI repete cada sítio antes e depois. No CI não há
              # segundo momento (a comparação é só entre sítios, depois da mudança).
              if (identical(tipo_resolvido, "longitudinal")) {
                shiny::div(class = "obs-card-interno",
                  shiny::h5(shiny::icon("clock"), " 2. Momentos e unidade repetida"),
                  shiny::p(class = "small text-muted mb-2",
                    "Liste os momentos em que cada unidade será medida de novo. Na planilha, cada unidade aparece uma vez por momento, sempre com o mesmo identificador."
                  ),
                  shiny::textInput(
                    ns("momentos"), "Momentos ou visitas (separados por vírgula):",
                    value = "0, 30, 60, 90 dias",
                    placeholder = "Ex.: 0, 15, 30, 45 dias",
                    width = "100%"
                  ),
                  shiny::textInput(
                    ns("coluna_unidade"), "Nome da coluna do identificador da unidade:",
                    value = "peixe",
                    placeholder = "Ex.: peixe, tanque, animal",
                    width = "100%"
                  )
                )
              } else if (identical(tipo_resolvido, "impacto")) {
                shiny::conditionalPanel(
                  condition = sprintf("input['%s'] !== 'ci'", ns("tipo_impacto")),
                  shiny::div(class = "obs-card-interno",
                    shiny::h5(shiny::icon("clock"), " 2. Momentos e sítio repetido"),
                    shiny::p(class = "small text-muted mb-2",
                      "Antes e depois: cada sítio é medido nos dois momentos, sempre com o mesmo identificador. No BACI, os sítios de referência também são medidos nos dois momentos."
                    ),
                    shiny::textInput(
                      ns("momentos"), "Momentos (separados por vírgula):",
                      value = "antes, depois",
                      placeholder = "Ex.: antes, depois",
                      width = "100%"
                    ),
                    shiny::textInput(
                      ns("coluna_unidade"), "Nome da coluna do identificador do sítio:",
                      value = "sitio",
                      placeholder = "Ex.: sitio, ponto, trecho",
                      width = "100%"
                    )
                  )
                )
              }
                )
              }
            ),

            # COLUNA 2: Composição da UA (Pool) e, no longitudinal, os momentos
            shiny::div(class = "obs-coluna-bloco",
              if (identical(tipo_resolvido, "gradiente")) {
                shiny::div(class = "obs-card-interno",
                  shiny::h5(shiny::icon("layer-group"), " 3. Composição da estação (Pool)"),
                  # Mesma regra do Transversal, com a estação como unidade amostral.
                  shiny::p(class = "small text-muted mb-2",
                    shiny::tags$b("Regra da CatalyseR: "),
                    "cada estação ocupa exatamente uma linha na planilha. Quando indivíduos pequenos são misturados para gerar massa de análise, essa mistura é uma amostra composta e os itens viram um número na coluna pool."
                  ),
                  shiny::radioButtons(
                    ns("tipo_pool"), "Como os itens compõem cada estação?",
                    choices = c(
                      "Todas as estações têm o mesmo pool (padrão = 1 item por estação)" = "igual",
                      "O pool varia conforme a estação (amostras compostas desiguais)" = "desigual"
                    ),
                    selected = "igual",
                    inline = FALSE
                  ),
                  shiny::conditionalPanel(
                    condition = sprintf("input['%s'] == 'igual'", ns("tipo_pool")),
                    shiny::numericInput(
                      ns("pool_unico"), "Número de itens em cada estação (para todas as estações):",
                      value = 1, min = 1, step = 1, width = "100%"
                    )
                  ),
                  shiny::conditionalPanel(
                    condition = sprintf("input['%s'] == 'desigual'", ns("tipo_pool")),
                    shiny::p(class = "small text-muted mb-1", "Informe a quantidade de itens agrupados em cada estação:"),
                    shiny::uiOutput(ns("ui_pools_grupos"))
                  ),
                  # No gradiente este espaço mostra o resumo do plano, porque a
                  # homogeneidade do pool não muda a análise (sempre regressão).
                  shiny::div(class = "mt-2",
                    shiny::uiOutput(ns("alerta_pool_diagnostico"))
                  )
                )
              } else {
              shiny::div(class = "obs-card-interno",
                # No impacto o número do cartão depende do tipo (CI esconde o
                # cartão de momentos), então o título é montado no servidor.
                if (identical(tipo_resolvido, "impacto")) {
                  shiny::uiOutput(ns("titulo_pool_impacto"))
                } else {
                  shiny::h5(shiny::icon("layer-group"),
                    if (identical(tipo_resolvido, "longitudinal")) " 3. Composição da Unidade Amostral (Pool)" else " 2. Composição da Unidade Amostral (Pool)")
                },
                shiny::p(class = "small text-muted mb-2",
                  shiny::tags$b("Regra da CatalyseR: "),
                  "cada UA ocupa exatamente uma linha na planilha. Quando indivíduos pequenos são misturados para gerar massa de análise, essa mistura é uma amostra composta e os itens viram um número na coluna pool."
                ),
                shiny::radioButtons(
                  ns("tipo_pool"), "Como os itens compõem cada UA?",
                  choices = c(
                    "Todos os grupos têm o mesmo pool (padrão = 1 item por UA)" = "igual",
                    "O pool varia conforme o grupo (amostras compostas desiguais)" = "desigual"
                  ),
                  selected = "igual",
                  inline = FALSE
                ),
                shiny::conditionalPanel(
                  condition = sprintf("input['%s'] == 'igual'", ns("tipo_pool")),
                  shiny::numericInput(
                    ns("pool_unico"), "Número de itens em cada UA (para todos os grupos):",
                    value = 1, min = 1, step = 1, width = "100%"
                  )
                ),
                shiny::conditionalPanel(
                  condition = sprintf("input['%s'] == 'desigual'", ns("tipo_pool")),
                  shiny::p(class = "small text-muted mb-1", "Informe a quantidade de itens agrupados em cada UA por grupo:"),
                  shiny::uiOutput(ns("ui_pools_grupos"))
                ),
                shiny::div(class = "mt-2",
                  shiny::uiOutput(ns("alerta_pool_diagnostico"))
                )
              )
              }
            ),
            
            # COLUNA 3: Variáveis de Resposta
            shiny::div(class = "obs-coluna-bloco",
              shiny::div(class = "obs-card-interno",
                if (identical(tipo_resolvido, "impacto")) {
                  shiny::uiOutput(ns("titulo_variaveis_impacto"))
                } else {
                  shiny::h5(shiny::icon("list-check"),
                    if (tipo_resolvido %in% c("longitudinal", "gradiente")) " 4. Variáveis de Resposta" else " 3. Variáveis de Resposta")
                },
                shiny::p(class = "small text-muted mb-2",
                  "Declare as variáveis que serão medidas em laboratório ou campo para cada UA. Cada nome vira uma coluna com células vazias na planilha de coleta."
                ),
                shiny::numericInput(
                  ns("n_vars_resposta"), "Quantidade de variáveis a medir:",
                  value = 2, min = 1, max = 10, step = 1, width = "150px"
                ),
                shiny::uiOutput(ns("ui_vars_resposta_campos")),
                shiny::div(class = "alert alert-light border small mt-3 mb-0",
                  style = "border-left: 4px solid #2E7D8F !important;",
                  shiny::tags$b("Atenção às réplicas analíticas: "),
                  "Duplicatas ou triplicatas de bancada no mesmo extrato medem apenas a precisão instrumental e devem ser resumidas pela média antes de entrar nesta planilha. Elas não aumentam o número de UAs."
                )
              )
            )
          )
        )
      ),
      
      # ABA 2: FICHA DO DELINEAMENTO (TABELA TIDY COMPLETA + BOTÕES DE EXPORTAÇÃO ABAIXO)
      # Sem botão de vínculo com as análises: os dados serão importados depois da
      # coleta e a estrutura da tabela pode mudar até lá.
      bslib::nav_panel("Ficha do delineamento", icon = shiny::icon("table"),
        bslib::card_body(fillable = FALSE,
          shiny::div(class = "d-flex justify-content-between align-items-center mb-2",
            shiny::h5(class = "fw-bold text-primary mb-0", shiny::icon("table"), " Planilha Tidy Completa de Coleta"),
            shiny::uiOutput(ns("badge_total_uas"))
          ),
          shiny::p(class = "small text-muted mb-2",
            "Uma linha por Unidade Amostral (UA). As colunas de resposta estão com células vazias prontas para o registro dos dados."
          ),
          DT::DTOutput(ns("tabela_tidy_coleta")),
          shiny::div(class = "obs-botoes-exportacao p-3 bg-light border rounded mt-3",
            bslib::layout_columns(
              col_widths = c(3, 3, 3, 3),
              shiny::downloadButton(ns("baixar_relatorio"), "Relatório Word (.docx)", class = "btn-success w-100"),
              shiny::downloadButton(ns("baixar_planilha"), "Planilha de coleta (.xlsx)", class = "btn-primary w-100"),
              shiny::downloadButton(ns("baixar_projeto"), "Projeto R (.zip)", class = "btn-success w-100"),
              shiny::downloadButton(ns("baixar_dicionario"), "Dicionário (.csv)", class = "btn-outline-primary w-100")
            )
          )
        )
      ),
      
      # ABA 3: METODOLOGIA PARA ARTIGO / RELATÓRIO
      bslib::nav_panel("Metodologia para artigo", icon = shiny::icon("paragraph"),
        bslib::card_body(fillable = FALSE,
          shiny::div(class = "obs-card-interno mb-3",
            shiny::h5(shiny::icon("paragraph"), " Seção de Metodologia para Artigo / Relatório"),
            shiny::p(class = "small text-muted mb-2",
              "Texto pré-formatado gerado dinamicamente com base nas decisões declaradas. Pronto para copiar diretamente para a seção de Material e Métodos da sua pesquisa:"
            ),
            shiny::div(
              class = "p-3 bg-light border rounded",
              # Texto em duas colunas fluidas (estilo artigo); abaixo de ~20rem
              # de largura disponível o CSS recolhe automaticamente para 1 coluna.
              style = paste(
                "font-family: Georgia, serif; line-height: 1.6; font-size: 0.95rem; color: #212529;",
                "column-width: 20rem; column-gap: 2.2rem; column-rule: 1px solid #d7e2e6;"
              ),
              shiny::uiOutput(ns("texto_metodologia_artigo"))
            )
          )
        )
      ),

      # ABA 4: MODELO ESTATÍSTICO E CUIDADOS
      bslib::nav_panel("Modelo e cuidados", icon = shiny::icon("calculator"),
        bslib::card_body(fillable = FALSE,
          # No gradiente a aba fica dividida em duas colunas: à esquerda o
          # modelo estatístico e quando usar; à direita os cuidados de coleta
          # e de análise. Nos demais delineamentos vale o empilhamento com o
          # modelo acima dos cuidados de pool.
          if (identical(tipo_resolvido, "gradiente")) {
            bslib::layout_columns(
              col_widths = c(6, 6),
              shiny::div(class = "obs-coluna-bloco",
                shiny::div(class = "obs-card-interno",
                  shiny::h5(shiny::icon("calculator"), " Modelo Estatístico Recomendado"),
                  shiny::uiOutput(ns("card_modelo_estatistico"))
                ),
                shiny::div(class = "obs-card-interno",
                  shiny::h5(shiny::icon("circle-question"), " Quando usar este delineamento"),
                  shiny::p(class = "small mb-0",
                    "Quando o interesse é como uma resposta muda ao longo de uma variação contínua, e não comparar grupos. Se você quer comparar categorias (ex.: local poluído × local limpo), use o Transversal comparativo com o fator condição."
                  )
                )
              ),
              shiny::div(class = "obs-coluna-bloco",
                shiny::div(class = "obs-card-interno",
                  shiny::h5(shiny::icon("clipboard-check"), " Cuidados na coleta"),
                  shiny::tags$ul(class = "mb-0 ps-3",
                    shiny::tags$li(class = "mb-2",
                      "A unidade amostral é a estação. Vários arrastos ou subamostras de uma mesma estação não são repetições; some-os no pool e registre uma linha por estação."
                    ),
                    shiny::tags$li(class = "mb-2",
                      "Cubra toda a faixa do gradiente, incluindo os valores intermediários, não apenas os extremos."
                    ),
                    shiny::tags$li(class = "mb-2",
                      "Espace as estações ao longo da faixa. Estações muito próximas tendem a ter valores parecidos só pela proximidade (autocorrelação espacial)."
                    ),
                    shiny::tags$li(class = "mb-0",
                      "Meça a variável do gradiente em cada estação, sem supor que os valores são exatamente os planejados."
                    )
                  )
                ),
                shiny::div(class = "obs-card-interno",
                  shiny::h5(shiny::icon("chart-line"), " Cuidados na análise"),
                  shiny::tags$ul(class = "mb-0 ps-3",
                    shiny::tags$li(class = "mb-2",
                      "A análise é uma regressão da resposta contra a variável do gradiente."
                    ),
                    shiny::tags$li(class = "mb-2",
                      "Repetir subamostras da mesma estação como se fossem independentes é pseudorréplica; a repetição verdadeira vem de ter várias estações ao longo da faixa."
                    ),
                    shiny::tags$li(class = "mb-2",
                      "Muitas estações espalhadas informam mais sobre a forma da relação do que poucos valores com muita repetição (Cottingham, Lennon e Brown, 2005)."
                    ),
                    shiny::tags$li(class = "mb-0",
                      "Uma única fonte limita a conclusão ao gradiente daquela fonte; generalizar exigiria várias fontes."
                    )
                  )
                )
              )
            )
          } else {
          shiny::tagList(
            shiny::div(class = "obs-card-interno mb-3",
              shiny::h5(shiny::icon("calculator"), " Modelo Estatístico Recomendado"),
              shiny::uiOutput(ns("card_modelo_estatistico"))
            ),
          shiny::div(class = "obs-card-interno",
            shiny::h5(shiny::icon("triangle-exclamation"), " Cuidados Metodológicos para o Pesquisador"),
            shiny::tags$ul(class = "mb-0 ps-3",
              shiny::tags$li(class = "mb-2",
                shiny::tags$b("Massa ou volume equitativo: "),
                "Cada item deve contribuir com a mesma quantidade (massa ou volume) para a amostra composta, para que nenhum item domine a mistura."
              ),
              shiny::tags$li(class = "mb-2",
                shiny::tags$b("Indivíduo único: "),
                "Nenhum item pode participar de mais de uma unidade amostral (evita dependência física entre UAs)."
              ),
              shiny::tags$li(class = "mb-2",
                shiny::tags$b("Réplicas analíticas de bancada: "),
                "Réplicas analíticas medem apenas a precisão da bancada e não aumentam o número de unidades amostrais independentes."
              ),
              shiny::tags$li(class = "mb-0",
                shiny::tags$b("Interpretação de variâncias sob pools desiguais: "),
                "As médias podem ser comparadas entre grupos, mas a variabilidade entre UAs não é diretamente comparável quando o pool difere, porque em alguns grupos ela reflete a variação entre itens individuais e em outros a variação entre misturas (Var = σ²/k)."
              )
            )
          )
          )
          }
        )
      )
    )
  )
}

# ---- SERVIDOR DO MÓDULO ------------------------------------------------------

mod_planejamento_observacional_server <- function(id, tipo, ficha_destino_rv = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tipo_resolvido <- if (tipo %in% c("transversal", "comparativo")) "transversal_comparativo" else tipo
    definicao <- catalogo_delineamentos_observacionais()[[tipo_resolvido]]
    if (is.null(definicao)) stop("Delineamento observacional desconhecido.", call. = FALSE)

    ou_vazio <- function(valor, padrao) {
      if (is.null(valor) || !length(valor) || (is.character(valor) && !nzchar(valor))) padrao else valor
    }

    # O tipo de pergunta de impacto (CI, BA ou BACI) só existe neste módulo;
    # fora dele vale o BACI, que é o desenho mais completo.
    tipo_impacto <- shiny::reactive({
      if (!identical(tipo_resolvido, "impacto")) return("baci")
      ou_vazio(input$tipo_impacto, "baci")
    })
    # BA e BACI repetem cada sítio nos momentos; o CI (só depois) não tem eixo de tempo.
    impacto_com_momentos <- shiny::reactive({
      identical(tipo_resolvido, "impacto") && !identical(tipo_impacto(), "ci")
    })

    # Cabeçalho da ficha se houver conexão. Sem ficha enviada, não mostra o
    # aviso "Ainda não há um planejamento atual": a linha some para ganhar espaço.
    output$planejamento_atual <- shiny::renderUI({
      if (is.function(ficha_destino_rv)) {
        ficha <- ficha_destino_rv()
        if (!is.null(ficha) && !is.null(ficha_nome_delineamento(ficha$tipo))) {
          ficha_cabecalho_ui(ficha)
        }
      }
    })

    # Vetor de grupos/níveis limpos
    grupos_lista <- shiny::reactive({
      # No impacto as "condições" são os sítios impactados e de referência; no
      # BA (antes–depois) não há controle, então só resta a condição de interesse.
      if (identical(tipo_resolvido, "impacto")) {
        if (identical(tipo_impacto(), "ba")) return("Impacto")
        txt <- ou_vazio(input$fator_niveis, "Impacto, Referência")
        partes <- trimws(unlist(strsplit(txt, ",")))
        partes <- partes[nzchar(partes)]
        if (!length(partes)) c("Impacto", "Referência") else partes
      } else {
        txt <- ou_vazio(input$fator_niveis, "Tambaqui, Gurijuba, Pescada Amarela, Pargo, Camurupim")
        partes <- trimws(unlist(strsplit(txt, ",")))
        partes <- partes[nzchar(partes)]
        if (!length(partes)) c("Grupo 1", "Grupo 2", "Grupo 3") else partes
      }
    })

    fator_nome_limpo <- shiny::reactive({
      txt <- ou_vazio(input$fator_nome, if (identical(tipo_resolvido, "impacto")) "situacao" else "especie")
      gerar_nome_reduzido(txt)
    })

    # No gradiente o eixo é uma variável contínua: nome da coluna e unidade.
    gradiente_coluna <- shiny::reactive({
      gerar_nome_reduzido(ou_vazio(input$gradiente_nome, "distancia_fonte"))
    })
    gradiente_unidade <- shiny::reactive({
      ou_vazio(input$gradiente_unidade, "m")
    })

    # Valores das estações ao longo da faixa: seq() igualmente espaçada (modo
    # padrão) ou lista digitada; a planilha segue a ordem crescente dos valores.
    estacoes_valores <- shiny::reactive({
      if (!identical(tipo_resolvido, "gradiente")) return(numeric())
      modo <- ou_vazio(input$modo_estacoes, "igual")
      if (identical(modo, "livre")) {
        txt <- ou_vazio(input$valores_livres, "50, 100, 150, 200, 250, 300, 350, 400")
        partes <- trimws(unlist(strsplit(txt, ",")))
        valores <- suppressWarnings(as.numeric(partes))
        valores <- valores[is.finite(valores)]
        if (!length(valores)) valores <- c(50, 100, 150, 200, 250)
        return(sort(valores))
      }
      inicio <- as.numeric(ou_vazio(input$estacao_inicio, 50))
      fim <- as.numeric(ou_vazio(input$estacao_fim, 400))
      n <- suppressWarnings(as.integer(ou_vazio(input$n_estacoes, 8)))
      if (!is.finite(inicio)) inicio <- 50
      if (!is.finite(fim) || fim <= inicio) fim <- inicio + 350
      if (!is.finite(n) || n < 2L) n <- 8L
      seq(inicio, fim, length.out = n)
    })

    # Códigos sequenciais das estações (E01, E02, ...), na ordem do gradiente.
    estacoes_codigos <- shiny::reactive({
      sprintf("E%02d", seq_along(estacoes_valores()))
    })

    # Unidades que recebem pool: os grupos nos delineamentos por comparação;
    # as estações no estudo de gradiente.
    unidades_pool <- shiny::reactive({
      if (identical(tipo_resolvido, "gradiente")) estacoes_codigos() else grupos_lista()
    })

    # Marca do aviso de poucas estações: com menos de 5 pontos fica difícil
    # enxergar a forma da relação ao longo do gradiente (aviso não bloqueia).
    poucas_estacoes <- shiny::reactive({
      identical(tipo_resolvido, "gradiente") && length(estacoes_valores()) < 5L
    })

    # Vetor de momentos limpos (longitudinal e impacto BA/BACI; vem do campo de texto).
    momentos_lista <- shiny::reactive({
      padrao <- if (identical(tipo_resolvido, "impacto")) "antes, depois" else "0, 30, 60, 90 dias"
      txt <- ou_vazio(input$momentos, padrao)
      partes <- trimws(unlist(strsplit(txt, ",")))
      partes <- partes[nzchar(partes)]
      if (!length(partes)) {
        if (identical(tipo_resolvido, "impacto")) c("antes", "depois") else c("início", "fim")
      } else partes
    })

    # Coluna do identificador da unidade: no longitudinal é o campo próprio; no
    # impacto é o sítio (repetido nos momentos) ou, no CI, a UA de sempre.
    unidade_coluna_nome <- shiny::reactive({
      if (identical(tipo_resolvido, "longitudinal")) {
        gerar_nome_reduzido(ou_vazio(input$coluna_unidade, "peixe"))
      } else if (identical(tipo_resolvido, "impacto")) {
        gerar_nome_reduzido(ou_vazio(input$coluna_unidade, "sitio"))
      } else {
        "ua"
      }
    })

    # Campos de pool desigual por unidade (grupo ou estação)
    output$ui_pools_grupos <- shiny::renderUI({
      unidades <- unidades_pool()
      gradiente <- identical(tipo_resolvido, "gradiente")
      impacto <- identical(tipo_resolvido, "impacto")
      valores <- if (gradiente) estacoes_valores() else NULL
      padroes_bexiga <- c(Tambaqui = 1, Gurijuba = 3, `Pescada Amarela` = 2, Pargo = 1, Camurupim = 1)

      campos <- lapply(seq_along(unidades), function(i) {
        u <- unidades[i]
        # No gradiente e no impacto o pool padrão de cada unidade é 1; nos
        # grupos comparativos valem os padrões das espécies de bexiga natatória.
        val_default <- if (gradiente || impacto) 1 else if (u %in% names(padroes_bexiga)) padroes_bexiga[[u]] else if (i == 2) 3 else 1
        shiny::div(class = "border rounded p-2 bg-light text-center",
          shiny::tags$b(class = "d-block text-truncate small mb-1", title = u, u),
          # Sob o código da estação aparece o valor dela no gradiente.
          if (gradiente) shiny::tags$span(class = "d-block small text-muted mb-1", paste(valores[i], gradiente_unidade())),
          shiny::numericInput(
            ns(paste0("pool_grupo_", i)), NULL,
            value = val_default, min = 1, max = 50, step = 1, width = "100%"
          )
        )
      })
      shiny::div(class = "obs-pool-grid", campos)
    })

    # Vetor numérico de pools por unidade (grupo ou estação)
    pools_por_grupo <- shiny::reactive({
      unidades <- unidades_pool()
      if (is.null(input$tipo_pool) || identical(input$tipo_pool, "igual")) {
        val <- as.integer(ou_vazio(input$pool_unico, 1))
        return(stats::setNames(rep(val, length(unidades)), unidades))
      }
      gradiente <- identical(tipo_resolvido, "gradiente")
      impacto <- identical(tipo_resolvido, "impacto")
      vals <- vapply(seq_along(unidades), function(i) {
        as.integer(ou_vazio(input[[paste0("pool_grupo_", i)]], if (!gradiente && !impacto && i == 2) 3 else 1))
      }, integer(1))
      stats::setNames(vals, unidades)
    })

    # Verificação de homogeneidade do pool
    pools_iguais <- shiny::reactive({
      pools <- pools_por_grupo()
      length(unique(pools)) <= 1
    })

    # Alerta visual de diagnóstico
    output$alerta_pool_diagnostico <- shiny::renderUI({
      # No gradiente este espaço é o resumo do plano: estações, linhas e a
      # análise indicada (regressão), com aviso quando há poucas estações.
      if (identical(tipo_resolvido, "gradiente")) {
        n <- length(estacoes_valores())
        coluna <- gradiente_coluna()
        if (poucas_estacoes()) {
          return(shiny::div(class = "alert alert-warning py-2 px-3 small mb-0",
            style = "border-left: 4px solid #E89B3C !important;",
            shiny::tags$b(sprintf("⚠ Resumo do plano: %d estações = %d linhas. ", n, n)),
            "Poucos pontos dificultam enxergar a forma da relação ao longo do gradiente. ",
            "Análise indicada: ", shiny::tags$b(sprintf("regressão da resposta contra %s", coluna)), "."
          ))
        }
        return(shiny::div(class = "alert alert-success py-2 px-3 small mb-0",
          shiny::tags$b(sprintf("✓ Resumo do plano: %d estações = %d linhas. ", n, n)),
          "Análise indicada: ", shiny::tags$b(sprintf("regressão da resposta contra %s", coluna)), "."
        ))
      }
      # No impacto a análise indicada depende do tipo de pergunta (CI, BA ou
      # BACI), então o diagnóstico do pool aponta para o modelo da aba 4 em vez
      # de cravar uma ANOVA específica.
      if (identical(tipo_resolvido, "impacto")) {
        pools <- pools_por_grupo()
        iguais <- pools_iguais()
        analise_txt <- switch(tipo_impacto(),
          ci = "teste t de amostras independentes (sítios impactados × referência, depois da mudança)",
          ba = "teste t pareado (antes × depois nos mesmos sítios)",
          baci = "interação local × tempo (mudança nos sítios impactados contra a dos de referência)"
        )
        if (iguais) {
          return(shiny::div(class = "alert alert-success py-2 px-3 small mb-0",
            shiny::tags$b(sprintf("✓ Pools homogêneos (k = %d itens por UA). ", pools[1])),
            "A variabilidade não foi distorcida pelo tamanho da mistura. ",
            "Análise indicada: ", shiny::tags$b(analise_txt), "."
          ))
        }
        return(shiny::div(class = "alert alert-warning py-2 px-3 small mb-0",
          style = "border-left: 4px solid #E89B3C !important;",
          shiny::tags$b("⚠ Atenção: unidades amostrais com pools diferentes. "),
          "A média de uma amostra composta varia menos quanto mais itens ela reúne (Var = σ²/k). ",
          "Análise indicada: ", shiny::tags$b(analise_txt),
          ", com a correção de Welch se as variâncias diferirem."
        ))
      }
      pools <- pools_por_grupo()
      iguais <- pools_iguais()
      
      if (iguais) {
        k <- pools[1]
        shiny::div(class = "alert alert-success py-2 px-3 small mb-0",
          shiny::tags$b("✓ Pools homogêneos (k = ", k, " itens por UA em todos os grupos). "),
          "A variabilidade intra-grupo não foi distorcida pelo tamanho amostral da mistura. ",
          "Análise indicada: ", shiny::tags$b("ANOVA de um fator clássica"), "."
        )
      } else {
        shiny::div(class = "alert alert-warning py-2 px-3 small mb-0",
          style = "border-left: 4px solid #E89B3C !important;",
          shiny::tags$b("⚠ Atenção: unidades amostrais com pools diferentes. "),
          "A média de uma amostra composta varia menos quanto mais itens ela reúne (Var = σ²/k), gerando heterocedasticidade estrutural. ",
          shiny::tags$br(),
          "Análise recomendada: ", shiny::tags$b("ANOVA de Welch com pós-teste de Games-Howell"), ", que não exige variâncias iguais."
        )
      }
    })

    # Títulos numerados dos cartões do impacto: o CI esconde o cartão de
    # momentos, então o número do pool e das variáveis muda conforme o tipo.
    output$titulo_pool_impacto <- shiny::renderUI({
      num <- if (identical(tipo_impacto(), "ci")) " 2." else " 3."
      shiny::h5(shiny::icon("layer-group"), num, " Composição da Unidade Amostral (Pool)")
    })
    output$titulo_variaveis_impacto <- shiny::renderUI({
      num <- if (identical(tipo_impacto(), "ci")) " 3." else " 4."
      shiny::h5(shiny::icon("list-check"), num, " Variáveis de Resposta")
    })

    # Campos de variáveis de resposta
    output$ui_vars_resposta_campos <- shiny::renderUI({
      n_vars <- max(1L, min(10L, as.integer(ou_vazio(input$n_vars_resposta, 2))))
      # No delineamento de impacto a resposta é uma condição do ambiente medida
      # nos sítios, então as sugestões acompanham o tipo de plano: variáveis
      # clássicas de qualidade de água, e não as de bancada dos comparativos.
      if (identical(tipo_resolvido, "impacto")) {
        padroes_nome <- c("oxigenio_dissolvido", "turbidez", "condutividade", "amonia_total", "solidos_suspensos")
        padroes_unid <- c("mg/L", "NTU", "µS/cm", "mg/L", "mg/L")
      } else {
        padroes_nome <- c("colesterol", "lipideos_totais", "umidade", "proteina_bruta", "cinzas")
        padroes_unid <- c("mg/100g", "g", "%", "%", "%")
      }
      
      itens <- lapply(seq_len(n_vars), function(i) {
        nome_sug <- if (i <= length(padroes_nome)) padroes_nome[i] else paste0("resposta_", i)
        unid_sug <- if (i <= length(padroes_unid)) padroes_unid[i] else "unidade"

        shiny::div(class = "obs-var-linha border rounded bg-light",
          shiny::tags$span(class = "obs-var-rotulo", i),
          shiny::textInput(ns(paste0("var_nome_", i)), NULL,
            value = nome_sug, placeholder = "Nome da coluna", width = "100%"),
          shiny::textInput(ns(paste0("var_unidade_", i)), NULL,
            value = unid_sug, placeholder = "Unidade", width = "100%")
        )
      })
      # Rótulos das colunas aparecem uma única vez, no topo da lista compacta.
      cabecalho <- shiny::div(class = "obs-var-cab",
        shiny::tags$span(class = "obs-var-rotulo", ""),
        shiny::tags$span(class = "small text-muted", style = "width: 100%;", "Nome da coluna"),
        shiny::tags$span(class = "small text-muted", style = "width: 100%;", "Unidade de medida")
      )
      do.call(shiny::tagList, c(list(cabecalho), itens))
    })

    # Tabela Tidy Completa
    tabela_coleta_dados <- shiny::reactive({
      # No gradiente há uma linha por estação, na ordem crescente dos valores,
      # com a coluna do gradiente preenchida e as respostas vazias.
      if (identical(tipo_resolvido, "gradiente")) {
        valores <- estacoes_valores()
        codigos <- estacoes_codigos()
        pools <- pools_por_grupo()
        df <- data.frame(
          estacao = codigos,
          valor_gradiente = valores,
          pool = unname(pools[codigos]),
          stringsAsFactors = FALSE
        )
        names(df)[names(df) == "valor_gradiente"] <- gradiente_coluna()
      } else if (identical(tipo_resolvido, "impacto") && identical(tipo_impacto(), "ba")) {
        # Antes–Depois (BA): só os sítios impactados, repetidos nos momentos.
        # Não há coluna de condição — o próprio sítio é o seu controle.
        n_sitios <- max(1L, as.integer(ou_vazio(input$n_uas, 3)))
        pools <- pools_por_grupo()
        unidade_col <- unidade_coluna_nome()
        df <- data.frame(
          ua = seq_len(n_sitios),
          pool = rep(unname(pools[1]), n_sitios),
          stringsAsFactors = FALSE
        )
        momentos <- momentos_lista()
        df <- df[rep(seq_len(nrow(df)), each = length(momentos)), , drop = FALSE]
        df$momento <- rep(momentos, times = n_sitios)
        rownames(df) <- NULL
        names(df)[names(df) == "ua"] <- unidade_col
      } else {
      grupos <- grupos_lista()
      n_uas <- max(1L, as.integer(ou_vazio(input$n_uas, if (identical(tipo_resolvido, "impacto")) 3 else 5)))
      fator_col <- fator_nome_limpo()
      pools <- pools_por_grupo()

      total_linhas <- length(grupos) * n_uas
      df <- data.frame(
        ua = seq_len(total_linhas),
        fator = rep(grupos, each = n_uas),
        replica = rep(seq_len(n_uas), times = length(grupos)),
        pool = rep(unname(pools[grupos]), each = n_uas),
        stringsAsFactors = FALSE
      )
      names(df)[names(df) == "fator"] <- fator_col

      # No longitudinal e no impacto (BA ou BACI) a planilha fica longa: cada
      # unidade repete a mesma linha de identificação em todos os momentos, com
      # o mesmo id. No impacto CI (só depois) a planilha fica como o transversal.
      repetir_momentos <- identical(tipo_resolvido, "longitudinal") ||
        (identical(tipo_resolvido, "impacto") && impacto_com_momentos())
      if (repetir_momentos) {
        momentos <- momentos_lista()
        df <- df[rep(seq_len(nrow(df)), each = length(momentos)), , drop = FALSE]
        df$momento <- rep(momentos, times = total_linhas)
        rownames(df) <- NULL
        names(df)[names(df) == "ua"] <- unidade_coluna_nome()
      } else if (identical(tipo_resolvido, "impacto")) {
        # CI: o identificador da UA é o sítio, sem eixo de tempo.
        names(df)[names(df) == "ua"] <- unidade_coluna_nome()
      }
      }

      n_vars <- max(1L, min(10L, as.integer(ou_vazio(input$n_vars_resposta, 2))))
      for (i in seq_len(n_vars)) {
        col_nome <- gerar_nome_reduzido(ou_vazio(input[[paste0("var_nome_", i)]], paste0("resposta_", i)))
        df[[col_nome]] <- ""
      }
      df
    })

    # Dicionário de variáveis
    dicionario_dados <- shiny::reactive({
      # No gradiente o eixo é quantitativo contínuo e a unidade é a estação.
      if (identical(tipo_resolvido, "gradiente")) {
        valores <- estacoes_valores()
        df_dict <- data.frame(
          coluna = c("estacao", gradiente_coluna(), "pool"),
          tipo = c("Identificador", "Quantitativa contínua", "Contagem (itens por estação)"),
          papel = c("Identificador da estação", "Eixo do gradiente (explanatória contínua)", "Amostra composta (pool)"),
          unidade = c("", gradiente_unidade(), "indivíduos/porções"),
          descricao = c(
            "Código da estação amostral ao longo do gradiente",
            sprintf("Posição da estação no gradiente (faixa de %g a %g %s)", min(valores), max(valores), gradiente_unidade()),
            "Quantidade de itens misturados para compor a estação (pool = 1 indica item único)"
          ),
          stringsAsFactors = FALSE
        )
      } else if (identical(tipo_resolvido, "impacto") && identical(tipo_impacto(), "ba")) {
        # Antes–Depois: sem coluna de condição, só sítio, pool e momento.
        unidade_col <- unidade_coluna_nome()
        df_dict <- data.frame(
          coluna = c(unidade_col, "pool", "momento"),
          tipo = c("Identificador", "Contagem (itens por UA)", "Qualitativa ordinal"),
          papel = c("Identificador do sítio", "Amostra composta (pool)", "Momento da medição (antes/depois)"),
          unidade = c("", "indivíduos/porções", ""),
          descricao = c(
            "Identificador único do sítio amostral",
            "Quantidade de itens misturados para compor a UA (pool = 1 indica item único)",
            paste("Momentos:", paste(momentos_lista(), collapse = ", "))
          ),
          stringsAsFactors = FALSE
        )
      } else {
      fator_col <- fator_nome_limpo()
      unidade_col <- unidade_coluna_nome()
      impacto <- identical(tipo_resolvido, "impacto")
      df_dict <- data.frame(
        coluna = c(unidade_col, fator_col, "replica", "pool"),
        tipo = c("Identificador", "Qualitativa nominal", "Contagem / Ordem", "Contagem (itens por UA)"),
        papel = c(
          if (impacto) "Identificador do sítio" else "Identificador da UA",
          if (impacto) "Condição do sítio (impacto ou referência)" else "Fator / Categoria",
          "Réplica do grupo",
          "Amostra composta (pool)"
        ),
        unidade = c("", "", "", "indivíduos/porções"),
        descricao = c(
          if (impacto) "Identificador único do sítio amostral" else "Identificador único da unidade amostral independente",
          paste(if (impacto) "Condições do sítio:" else "Níveis do fator:", paste(grupos_lista(), collapse = ", ")),
          "Número da réplica independente dentro de cada grupo",
          "Quantidade de itens misturados para compor a UA (pool = 1 indica item único)"
        ),
        stringsAsFactors = FALSE
      )
      # No longitudinal e no impacto BACI o dicionário ganha o momento.
      if (identical(tipo_resolvido, "longitudinal") ||
          (impacto && impacto_com_momentos())) {
        df_dict <- rbind(df_dict, data.frame(
          coluna = "momento",
          tipo = "Qualitativa ordinal",
          papel = "Momento da medição (repetição da mesma UA)",
          unidade = "",
          descricao = paste("Momentos:", paste(momentos_lista(), collapse = ", ")),
          stringsAsFactors = FALSE
        ))
      }
      }
      n_vars <- max(1L, min(10L, as.integer(ou_vazio(input$n_vars_resposta, 2))))
      for (i in seq_len(n_vars)) {
        col_nome <- gerar_nome_reduzido(ou_vazio(input[[paste0("var_nome_", i)]], paste0("resposta_", i)))
        unid <- ou_vazio(input[[paste0("var_unidade_", i)]], "unidade")
        df_dict <- rbind(df_dict, data.frame(
          coluna = col_nome,
          tipo = "Quantitativa contínua",
          papel = "Resposta",
          unidade = unid,
          descricao = paste("Medição de resposta:", col_nome, "em", unid),
          stringsAsFactors = FALSE
        ))
      }
      df_dict
    })

    # Badge de contagem
    output$badge_total_uas <- shiny::renderUI({
      # No gradiente o total é o número de estações, que é o número de linhas.
      if (identical(tipo_resolvido, "gradiente")) {
        n <- length(estacoes_valores())
        return(shiny::tags$span(class = "badge bg-primary fs-6 px-3 py-2",
          sprintf("%d estações = %d linhas", n, n)
        ))
      }
      # No impacto a unidade é o sítio; o total depende do tipo de pergunta.
      if (identical(tipo_resolvido, "impacto")) {
        n_sitios <- max(1L, as.integer(ou_vazio(input$n_uas, 3)))
        if (identical(tipo_impacto(), "ba")) {
          n_momentos <- length(momentos_lista())
          return(shiny::tags$span(class = "badge bg-primary fs-6 px-3 py-2",
            sprintf("%d linhas (%d sítios × %d momentos)", n_sitios * n_momentos, n_sitios, n_momentos)
          ))
        }
        total <- length(grupos_lista()) * n_sitios
        if (impacto_com_momentos()) {
          n_momentos <- length(momentos_lista())
          return(shiny::tags$span(class = "badge bg-primary fs-6 px-3 py-2",
            sprintf("%d linhas (%d sítios × %d momentos)", total * n_momentos, total, n_momentos)
          ))
        }
        return(shiny::tags$span(class = "badge bg-primary fs-6 px-3 py-2",
          sprintf("%d sítios no total (%d condições × %d sítios)", total, length(grupos_lista()), n_sitios)
        ))
      }
      grupos <- grupos_lista()
      n_uas <- max(1L, as.integer(ou_vazio(input$n_uas, 5)))
      total <- length(grupos) * n_uas
      if (identical(tipo_resolvido, "longitudinal")) {
        n_momentos <- length(momentos_lista())
        return(shiny::tags$span(class = "badge bg-primary fs-6 px-3 py-2",
          sprintf("%d linhas (%d UAs × %d momentos)", total * n_momentos, total, n_momentos)
        ))
      }
      shiny::tags$span(class = "badge bg-primary fs-6 px-3 py-2",
        sprintf("%d UAs no total (%d grupos × %d réplicas)", total, length(grupos), n_uas)
      )
    })

    # Tabela Tidy DT
    output$tabela_tidy_coleta <- DT::renderDT({
      df <- tabela_coleta_dados()
      DT::datatable(
        df,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = "tip",
          language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json")
        ),
        rownames = FALSE,
        class = "display compact cell-border stripe"
      )
    })

    # Texto formatado da seção de metodologia para artigo
    texto_metodologia_artigo_str <- shiny::reactive({
      # No gradiente o texto descreve as estações ao longo da faixa, o pool por
      # estação e a análise por regressão.
      if (identical(tipo_resolvido, "gradiente")) {
        valores <- estacoes_valores()
        n <- length(valores)
        nome_extenso <- ou_vazio(input$gradiente_nome, "distancia_fonte")
        unidade <- gradiente_unidade()
        pools <- pools_por_grupo()
        iguais <- pools_iguais()
        paragrafos <- c(sprintf(
          "Trata-se de um estudo de gradiente observacional. Foram estabelecidas %d estações ao longo da faixa de %s (%g a %g %s), cada estação correspondendo a uma unidade amostral independente, totalizando %d unidades amostrais.",
          n, nome_extenso, min(valores), max(valores), unidade, n
        ))
        if (iguais) {
          k <- pools[1]
          if (k > 1) {
            paragrafos <- c(paragrafos, sprintf(
              "Cada estação foi constituída por uma amostra composta de %d indivíduos, formada com contribuições equitativas de massa de cada indivíduo. Cada espécime integrou uma única estação. As réplicas analíticas foram resumidas pela média antes da análise estatística.",
              k
            ))
          } else {
            paragrafos <- c(paragrafos, "Cada estação foi constituída por um único espécime ou medida individual.")
          }
        } else {
          faixa_pool <- paste(range(pools), collapse = " a ")
          paragrafos <- c(paragrafos, sprintf(
            "Cada estação foi constituída por uma amostra composta de %s indivíduos (registrado na coluna pool), formada com contribuições equitativas de massa de cada espécime. Cada indivíduo integrou uma única estação. As réplicas analíticas de laboratório foram resumidas pela média dentro de cada estação, não sendo consideradas repetições na análise estatística.",
            faixa_pool
          ))
        }
        paragrafos <- c(paragrafos, sprintf(
          "A relação entre a resposta e %s foi analisada por regressão linear simples, tomando cada estação como uma observação independente. Caso a relação se afastasse de uma reta, seriam consideradas a transformação da resposta ou a inclusão de um termo quadrático.",
          nome_extenso
        ))
        return(paste(paragrafos, collapse = "\n\n"))
      }
      # No impacto o texto depende do tipo de pergunta: CI compara sítios num
      # só momento, BA compara o mesmo sítio antes e depois, BACI combina os dois.
      if (identical(tipo_resolvido, "impacto")) {
        ti <- tipo_impacto()
        grupos <- grupos_lista()
        n_sitios <- max(1L, as.integer(ou_vazio(input$n_uas, 3)))
        fator_extenso <- ou_vazio(input$fator_nome, "situação")
        momentos <- if (impacto_com_momentos()) momentos_lista() else NULL
        pools <- pools_por_grupo()
        iguais <- pools_iguais()

        pool_txt <- if (iguais && pools[1] > 1) {
          sprintf("Cada unidade amostral foi constituída por uma amostra composta de %d itens, com contribuições equitativas de cada item. Cada item integrou uma única unidade amostral.", pools[1])
        } else {
          "Cada unidade amostral foi constituída por um item individual."
        }

        if (identical(ti, "ci")) {
          paragrafos <- c(
            sprintf("Trata-se de um delineamento de impacto do tipo Controle–Impacto (CI). Foram amostrados %d sítios sob a condição de interesse (%s) e %d sítios de referência (%s), cada sítio correspondendo a uma unidade amostral independente, em um único recorte temporal após a mudança.",
                    n_sitios, grupos[1], n_sitios, paste(grupos[-1], collapse = ", ")),
            pool_txt,
            sprintf("A comparação entre as condições de %s foi conduzida por teste t de amostras independentes; sem medidas anteriores, o desenho não distingue o impacto de uma diferença que já existia.", fator_extenso)
          )
        } else if (identical(ti, "ba")) {
          paragrafos <- c(
            sprintf("Trata-se de um delineamento de impacto do tipo Antes–Depois (BA). Foram acompanhados %d sítios sob a condição de interesse, cada um medido nos momentos %s, com cada sítio constituindo uma unidade amostral independente.",
                    n_sitios, paste(momentos, collapse = " e ")),
            pool_txt,
            "A comparação entre os momentos foi conduzida por teste t pareado, com o sítio como unidade de pareamento; sem sítios de referência, mudanças naturais no período se confundem com o impacto."
          )
        } else {
          paragrafos <- c(
            sprintf("Trata-se de um delineamento de impacto do tipo BACI (Antes–Depois com Controle–Impacto). Foram amostrados %d sítios sob a condição de interesse (%s) e %d sítios de referência (%s), cada sítio medido nos momentos %s.",
                    n_sitios, grupos[1], n_sitios, paste(grupos[-1], collapse = ", "), paste(momentos, collapse = " e ")),
            pool_txt,
            sprintf("O sinal de impacto é a interação entre a condição de %s e o momento: a mudança nos sítios impactados, comparada à dos de referência. A análise foi conduzida por ANOVA de dois fatores com interação, ou modelo misto se o mesmo sítio for medido nos dois momentos.", fator_extenso)
          )
        }
        return(paste(paragrafos, collapse = "\n\n"))
      }
      grupos <- grupos_lista()
      n_uas <- max(1L, as.integer(ou_vazio(input$n_uas, 5)))
      total <- length(grupos) * n_uas
      fator_extenso <- ou_vazio(input$fator_nome, "espécie")
      pools <- pools_por_grupo()
      iguais <- pools_iguais()
      longitudinal <- identical(tipo_resolvido, "longitudinal")

      paragrafos <- c()
      if (longitudinal) {
        momentos <- momentos_lista()
        paragrafos <- c(paragrafos, sprintf(
          "Trata-se de um delineamento observacional longitudinal comparativo. Foram acompanhadas %d unidades amostrais por %s (%s), totalizando %d unidades amostrais independentes, cada uma medida nos mesmos %d momentos (%s).",
          n_uas, fator_extenso, paste(grupos, collapse = ", "), total,
          length(momentos), paste(momentos, collapse = ", ")
        ))
      } else {
        paragrafos <- c(paragrafos, sprintf(
          "Trata-se de um delineamento observacional transversal comparativo. Foram analisadas %d unidades amostrais por %s (%s), totalizando %d unidades amostrais independentes.",
          n_uas, fator_extenso, paste(grupos, collapse = ", "), total
        ))
      }
      
      if (iguais) {
        k <- pools[1]
        if (k > 1) {
          paragrafos <- c(paragrafos, sprintf(
            "Cada unidade amostral foi constituída por uma amostra composta de %d indivíduos, formada com contribuições equitativas de massa de cada indivíduo. Cada espécime integrou uma única unidade amostral. As réplicas analíticas foram resumidas pela média antes da análise estatística.",
            k
          ))
        } else {
          paragrafos <- c(paragrafos, "Cada unidade amostral foi constituída por um único espécime individual.")
        }
      } else {
        faixa_pool <- paste(range(pools), collapse = " a ")
        paragrafos <- c(paragrafos, sprintf(
          "Cada unidade amostral foi constituída por uma amostra composta de %s indivíduos, conforme a %s (registrado na coluna pool), formada com contribuições equitativas de massa de cada espécime. Cada indivíduo integrou uma única unidade amostral. As réplicas analíticas de laboratório foram resumidas pela média dentro de cada unidade, não sendo consideradas repetições na análise estatística.",
          faixa_pool, fator_extenso
        ))
      }
      
      if (longitudinal) {
        paragrafos <- c(paragrafos, sprintf(
          "Como as medições repetidas da mesma unidade amostral não são independentes, a comparação entre as categorias de %s ao longo do tempo foi conduzida por um modelo linear misto, com o momento, a %s e sua interação como efeitos fixos e a unidade amostral como efeito aleatório.",
          fator_extenso, fator_extenso
        ))
      } else if (iguais) {
        paragrafos <- c(paragrafos, sprintf(
          "Para a comparação das médias entre as diferentes categorias de %s, foi aplicada Análise de Variância (ANOVA) de um fator, seguida de teste post-hoc adequado se verificada significância estatística.",
          fator_extenso
        ))
      } else {
        paragrafos <- c(paragrafos, sprintf(
          "Em razão dos diferentes tamanhos de pool entre as categorias de %s, que induzem heterocedasticidade estrutural (a variância amostral da média é inversamente proporcional ao pool, Var = σ²/k), a comparação entre os grupos foi conduzida por meio da ANOVA de Welch, associada ao teste post-hoc de Games-Howell para comparações múltiplas com variâncias desiguais.",
          fator_extenso
        ))
      }
      
      paste(paragrafos, collapse = "\n\n")
    })

    output$texto_metodologia_artigo <- shiny::renderUI({
      texto <- texto_metodologia_artigo_str()
      paragrafos <- unlist(strsplit(texto, "\n\n"))
      shiny::tagList(lapply(paragrafos, function(p) shiny::p(class = "mb-2", p)))
    })

    # Card do Modelo Estatístico
    output$card_modelo_estatistico <- shiny::renderUI({
      # No gradiente a análise é uma regressão da resposta contra o eixo contínuo.
      if (identical(tipo_resolvido, "gradiente")) {
        return(shiny::div(class = "alert alert-info border mb-0",
          style = "border-left: 4px solid #2E7D8F !important;",
          shiny::h6(class = "alert-heading fw-bold mb-1", shiny::icon("chart-line"), " Regressão da resposta contra o gradiente"),
          shiny::p(class = "small mb-1",
            "Não há grupos a comparar: cada estação é um ponto ao longo da faixa contínua. A regressão descreve como a resposta muda com a variável do gradiente e permite interpolar valores não amostrados."
          ),
          shiny::p(class = "small mb-1",
            "Comece pela reta. Se o gráfico mostrar curvatura, considere transformar a resposta ou incluir um termo quadrático antes de interpretar."
          ),
          shiny::p(class = "small mb-0",
            shiny::tags$b("Função no R: "),
            shiny::tags$code(sprintf("lm(resposta ~ %s, data = dados)", gradiente_coluna()))
          )
        ))
      }
      if (identical(tipo_resolvido, "longitudinal")) {
        unidade_col <- unidade_coluna_nome()
        return(shiny::div(class = "alert alert-info border mb-0",
          style = "border-left: 4px solid #2E7D8F !important;",
          shiny::h6(class = "alert-heading fw-bold mb-1", shiny::icon("arrows-rotate"), " Modelo misto para medidas repetidas"),
          shiny::p(class = "small mb-1",
            "As medições da mesma unidade ao longo do tempo são relacionadas entre si. O modelo misto trata a unidade como efeito aleatório e testa o grupo, o momento e a interação dos dois como efeitos fixos."
          ),
          shiny::p(class = "small mb-0",
            shiny::tags$b("Função no R: "),
            shiny::tags$code(sprintf("nlme::lme(resposta ~ %s * momento, random = ~ 1 | %s, data = dados)",
                                     fator_nome_limpo(), unidade_col))
          )
        ))
      }
      # O modelo recomendado do impacto muda com o tipo de pergunta: CI compara
      # sítios num só momento, BA compara o mesmo sítio antes e depois, e o BACI
      # olha para a interação local × tempo (o verdadeiro sinal de impacto).
      if (identical(tipo_resolvido, "impacto")) {
        fator_col <- fator_nome_limpo()
        if (identical(tipo_impacto(), "ci")) {
          return(shiny::div(class = "alert alert-info border mb-0",
            style = "border-left: 4px solid #2E7D8F !important;",
            shiny::h6(class = "alert-heading fw-bold mb-1", shiny::icon("scale-balanced"), " Controle–Impacto: teste t de amostras independentes"),
            shiny::p(class = "small mb-1",
              "Compara sítios impactados e de referência num mesmo momento, depois da mudança. Cada sítio é uma observação independente."
            ),
            shiny::p(class = "small mb-1",
              "Sem medidas anteriores, este desenho não separa o impacto de uma diferença que já existia entre os sítios."
            ),
            shiny::p(class = "small mb-0",
              shiny::tags$b("Função no R: "),
              shiny::tags$code(sprintf("t.test(resposta ~ %s, data = dados)", fator_col)),
              " (ou ", shiny::tags$code("aov()"), " se houver mais de duas condições)"
            )
          ))
        }
        if (identical(tipo_impacto(), "ba")) {
          return(shiny::div(class = "alert alert-info border mb-0",
            style = "border-left: 4px solid #2E7D8F !important;",
            shiny::h6(class = "alert-heading fw-bold mb-1", shiny::icon("clock-rotate-left"), " Antes–Depois: teste t pareado"),
            shiny::p(class = "small mb-1",
              "Compara o mesmo sítio antes e depois. Como cada sítio é medido duas vezes, o par é o sítio: use o teste pareado sobre as médias por sítio."
            ),
            shiny::p(class = "small mb-1",
              "Sem sítios de referência, mudanças naturais no período se confundem com o impacto."
            ),
            shiny::p(class = "small mb-0",
              shiny::tags$b("Função no R: "),
              shiny::tags$code("t.test(resposta ~ momento, data = dados, paired = TRUE)")
            )
          ))
        }
        return(shiny::div(class = "alert alert-info border mb-0",
          style = "border-left: 4px solid #2E7D8F !important;",
          shiny::h6(class = "alert-heading fw-bold mb-1", shiny::icon("diagram-project"), " BACI: interação local × tempo"),
          shiny::p(class = "small mb-1",
            "Combina antes–depois com controle. O sinal de impacto é a interação: a mudança nos sítios impactados, comparada à dos de referência."
          ),
          shiny::p(class = "small mb-1",
            "Use vários sítios em cada condição. Um único sítio impactado não é uma réplica independente."
          ),
          shiny::p(class = "small mb-0",
            shiny::tags$b("Função no R: "),
            shiny::tags$code(sprintf("aov(resposta ~ %s * momento, data = dados)", fator_col)),
            " (ou modelo misto se o mesmo sítio for medido nos dois momentos)"
          )
        ))
      }
      iguais <- pools_iguais()
      if (iguais) {
        shiny::div(class = "alert alert-success border mb-0",
          shiny::h6(class = "alert-heading fw-bold mb-1", shiny::icon("check"), " ANOVA de 1 Fator Clássica"),
          shiny::p(class = "small mb-1", "Com tamanhos de pool homogêneos entre os grupos, a premissa de homogeneidade de variâncias decorrente do delineamento não é estruturalmente violada."),
          shiny::p(class = "small mb-0", shiny::tags$b("Fórmula no R: "), shiny::tags$code("lm(resposta ~ especie, data = dados)"), " ou ", shiny::tags$code("aov()"), " + TukeyHSD.")
        )
      } else {
        shiny::div(class = "alert alert-warning border mb-0",
          style = "border-left: 4px solid #E89B3C !important;",
          shiny::h6(class = "alert-heading fw-bold mb-1", shiny::icon("triangle-exclamation"), " ANOVA de Welch + Pós-teste de Games-Howell"),
          shiny::p(class = "small mb-1",
            "Pools desiguais geram variâncias populacionais estruturalmente distintas entre os grupos (Var = σ²/k). A ANOVA clássica subestima ou superestima erros padrão. A correção de Welch ajusta os graus de liberdade sem exigir homocedasticidade."
          ),
          shiny::p(class = "small mb-0",
            shiny::tags$b("Função no R: "), shiny::tags$code("rstatix::welch_anova_test(resposta ~ especie, data = dados)"), " e ", shiny::tags$code("rstatix::games_howell_test()")
          )
        )
      }
    })

    # Ficha persistente do delineamento
    ficha <- shiny::reactive({
      grupos <- if (!is.null(input$n_niveis) && is.null(input$fator_niveis)) {
        paste("Grupo", seq_len(as.integer(input$n_niveis)))
      } else {
        grupos_lista()
      }
      n_uas <- max(1L, as.integer(ou_vazio(input$n_uas, input$sitios_por_nivel %||% 5)))
      fator_col <- fator_nome_limpo()
      fator_extenso <- ou_vazio(input$fator_nome, "especie")
      pools <- pools_por_grupo()
      iguais <- pools_iguais()

      subamostras <- as.integer(input$subamostras_por_sitio %||% 1)
      tem_subamostras <- is.finite(subamostras) && subamostras > 1
      analise <- if (tem_subamostras) {
        "anova_mista_subamostras"
      } else if (!iguais && is.null(input$n_niveis)) {
        "welch_games_howell"
      } else {
        "anova_um_fator"
      }

      col_unidade <- unidade_coluna_nome()
      col_subamostra <- if (tem_subamostras) ou_vazio(input$coluna_subamostra, "subamostra") else ""

      # O gradiente grava o eixo contínuo com os valores das estações e já
      # declara o n planejado (número de estações) e a regressão sugerida.
      if (identical(tipo_resolvido, "gradiente")) {
        valores <- estacoes_valores()
        codigos <- estacoes_codigos()
        n_est <- length(valores)
        pools <- pools_por_grupo()
        return(ficha_mesclar(NULL, list(
          origem = paste("Planejamento observacional —", definicao$titulo),
          tipo = tipo_resolvido,
          pergunta = input$pergunta,
          resposta_coluna = NULL,
          # Eixo contínuo: sem níveis nem nomes de fator; niveis guarda o nº de
          # estações só para leitura (cabeçalho), e valores guarda a faixa.
          eixos = list(gradiente = list(
            coluna = gradiente_coluna(),
            continua = TRUE,
            unidade = gradiente_unidade(),
            niveis = n_est,
            valores = valores
          )),
          unidade_coluna = "estacao",
          hierarquia = list(
            estacoes = n_est,
            pool_por_estacao = stats::setNames(as.integer(pools[codigos]), codigos),
            subamostra_coluna = "",
            subamostras_por_sitio = NULL
          ),
          n_planejado = list(
            valor = n_est,
            total = n_est,
            unidade = "estações ao longo do gradiente",
            metodo = "declarado no delineamento de gradiente (número de estações)",
            premissas = NULL
          ),
          sorteio = NULL,
          analise_sugerida = "regressao_linear_simples"
        )))
      }

      # O longitudinal grava o eixo do tempo com os momentos declarados; a
      # análise de medidas repetidas ainda não tem passagem validada (costura).
      if (identical(tipo_resolvido, "longitudinal")) {
        momentos <- momentos_lista()
        return(ficha_mesclar(NULL, list(
          origem = paste("Planejamento observacional —", definicao$titulo),
          tipo = tipo_resolvido,
          pergunta = input$pergunta,
          resposta_coluna = input$resposta,
          eixos = list(
            grupos = list(coluna = fator_col, niveis = length(grupos), nomes = grupos),
            tempo = list(coluna = "momento", niveis = length(momentos), nomes = momentos)
          ),
          unidade_coluna = col_unidade,
          hierarquia = list(
            sitios_por_nivel = n_uas,
            niveis = length(grupos),
            subamostra_coluna = col_subamostra,
            subamostras_por_sitio = if (tem_subamostras) subamostras else NULL,
            momentos = momentos,
            repeticao_temporal = TRUE
          ),
          n_planejado = NULL,
          sorteio = NULL,
          analise_sugerida = NULL
        )))
      }

      # O impacto grava o eixo do local (condição dos sítios) e, no BA/BACI,
      # também o eixo do tempo (antes/depois); a análise segue NULL até haver
      # passagem validada para cada tipo (costura).
      if (identical(tipo_resolvido, "impacto")) {
        grupos <- grupos_lista()
        n_uas <- max(1L, as.integer(ou_vazio(input$n_uas, 3)))
        fator_col <- fator_nome_limpo()
        unidade_col <- unidade_coluna_nome()
        momentos <- if (impacto_com_momentos()) momentos_lista() else NULL

        eixos <- list()
        if (identical(tipo_impacto(), "ba")) {
          # Sem controle, o eixo da comparação é o tempo (antes × depois).
          eixos$tempo <- list(coluna = "momento", niveis = length(momentos), nomes = momentos)
        } else {
          eixos$impacto <- list(coluna = fator_col, niveis = length(grupos), nomes = grupos)
          if (impacto_com_momentos()) {
            eixos$tempo <- list(coluna = "momento", niveis = length(momentos), nomes = momentos)
          }
        }
        return(ficha_mesclar(NULL, list(
          origem = paste("Planejamento observacional —", definicao$titulo),
          tipo = tipo_resolvido,
          pergunta = input$pergunta,
          resposta_coluna = input$resposta,
          eixos = eixos,
          unidade_coluna = unidade_col,
          hierarquia = list(
            sitios_por_nivel = n_uas,
            niveis = length(grupos),
            subamostra_coluna = "",
            subamostras_por_sitio = NULL,
            momentos = momentos,
            repeticao_temporal = impacto_com_momentos()
          ),
          n_planejado = NULL,
          sorteio = NULL,
          analise_sugerida = NULL
        )))
      }

      ficha_mesclar(NULL, list(
        origem = paste("Planejamento observacional —", definicao$titulo),
        tipo = tipo_resolvido,
        pergunta = input$pergunta,
        resposta_coluna = input$resposta,
        eixos = list(grupos = list(coluna = fator_col, niveis = length(grupos), nomes = grupos)),
        unidade_coluna = col_unidade,
        hierarquia = list(
          sitios_por_nivel = n_uas,
          niveis = length(grupos),
          subamostra_coluna = col_subamostra,
          subamostras_por_sitio = if (tem_subamostras) subamostras else NULL
        ),
        n_planejado = NULL,
        sorteio = NULL,
        analise_sugerida = analise
      ))
    })

    # Downloads: Planilha Excel (.xlsx)
    output$baixar_planilha <- shiny::downloadHandler(
      # O nome do arquivo acompanha o delineamento.
      filename = function() {
        prefixo <- if (identical(tipo_resolvido, "gradiente")) "planilha_coleta_gradiente_" else "planilha_coleta_transversal_"
        paste0(prefixo, format(Sys.Date(), "%Y-%m-%d"), ".xlsx")
      },
      content = function(file) {
        coleta <- tabela_coleta_dados()
        dicionario <- dicionario_dados()
        ficha_tbl <- ficha_tabela(ficha())
        writexl::write_xlsx(list(coleta = coleta, dicionario = dicionario, ficha_planejamento = ficha_tbl), file)
      }
    )

    # Downloads: Dicionário CSV
    output$baixar_dicionario <- shiny::downloadHandler(
      filename = function() paste0("dicionario_dados_", format(Sys.Date(), "%Y-%m-%d"), ".csv"),
      content = function(file) {
        utils::write.csv(dicionario_dados(), file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )

    # Downloads: Relatório Word (.docx)
    output$baixar_relatorio <- shiny::downloadHandler(
      filename = function() paste0("relatorio_planejamento_observacional_", format(Sys.Date(), "%Y-%m-%d"), ".docx"),
      content = function(file) {
        pasta <- tempfile("relatorio_obs_")
        dir.create(pasta)
        antigo <- getwd()
        on.exit(setwd(antigo), add = TRUE)
        on.exit(unlink(pasta, recursive = TRUE), add = TRUE)
        
        texto_metodo <- texto_metodologia_artigo_str()
        coleta <- tabela_coleta_dados()
        dicionario <- dicionario_dados()
        
        utils::write.csv(coleta, file.path(pasta, "planilha_coleta.csv"), row.names = FALSE, fileEncoding = "UTF-8")
        utils::write.csv(dicionario, file.path(pasta, "dicionario.csv"), row.names = FALSE, fileEncoding = "UTF-8")
        
        # O Word recebe título e cuidados conforme o delineamento.
        titulo_doc <- if (identical(tipo_resolvido, "gradiente")) {
          "Ficha de Planejamento — Estudo de Gradiente"
        } else {
          "Ficha de Planejamento — Transversal Comparativo"
        }
        cuidados_doc <- if (identical(tipo_resolvido, "gradiente")) {
          c(
            "- **Unidade amostral:** cada estação ocupa uma linha; arrastos ou subamostras da mesma estação entram somados no pool.",
            "- **Cobertura da faixa:** distribuir as estações por toda a faixa do gradiente, incluindo os valores intermediários.",
            "- **Autocorrelação espacial:** estações muito próximas tendem a valores parecidos só pela proximidade; aumentar o espaçamento.",
            "- **Pseudorréplica:** subamostras da mesma estação não são repetições independentes; a repetição vem das várias estações."
          )
        } else {
          c(
            "- **Massa ou volume equitativo:** Cada item deve contribuir com a mesma quantidade de biomassa para o pool.",
            "- **Indivíduo único:** Nenhum item participa de mais de uma UA.",
            "- **Réplicas analíticas:** Duplicatas de bancada medem precisão de pipetagem e devem ser resumidas pela média.",
            "- **Interpretação de variâncias sob pools desiguais:** As médias são comparáveis, mas variâncias de grupos com pools diferentes refletem misturas distintas (Var = σ²/k)."
          )
        }
        qmd_lines <- c(
          "---",
          sprintf("title: \"%s\"", titulo_doc),
          "author: \"CatalyseR — Estatística Aplicada à Pesca e Aquicultura\"",
          sprintf("date: \"%s\"", format(Sys.Date(), "%d/%m/%Y")),
          "format:",
          "  docx: default",
          "---",
          "",
          "# Pergunta do Estudo",
          ou_vazio(input$pergunta, "Não declarada"),
          "",
          "# Material e Métodos (Texto para Artigo)",
          texto_metodo,
          "",
          "# Cuidados Metodológicos de Bancada e Coleta",
          cuidados_doc,
          "",
          "# Dicionário de Dados da Coleta",
          "```{r, echo=FALSE}",
          "dicionario <- read.csv('dicionario.csv', check.names=FALSE)",
          "knitr::kable(dicionario)",
          "```",
          "",
          "# Planilha de Coleta (Primeiras Linhas)",
          "```{r, echo=FALSE}",
          "planilha <- read.csv('planilha_coleta.csv', check.names=FALSE)",
          "knitr::kable(head(planilha, 25))",
          "```"
        )
        writeLines(qmd_lines, file.path(pasta, "relatorio.qmd"), useBytes = TRUE)
        setwd(pasta)
        system2("quarto", c("render", "relatorio.qmd", "--to", "docx"))
        resultado <- file.path(pasta, "relatorio.docx")
        if (!file.exists(resultado)) stop("Não foi possível gerar o Word.", call. = FALSE)
        file.copy(resultado, file, overwrite = TRUE)
      }
    )

    # Downloads: Projeto R (.zip)
    output$baixar_projeto <- shiny::downloadHandler(
      filename = function() paste0("projeto_planejamento_observacional_", format(Sys.Date(), "%Y-%m-%d"), ".zip"),
      content = function(file) {
        pasta_temp <- tempfile("proj_obs_")
        dir.create(pasta_temp)
        antigo_wd <- getwd()
        on.exit(setwd(antigo_wd), add = TRUE)
        on.exit(unlink(pasta_temp, recursive = TRUE), add = TRUE)
        
        proj_dir <- file.path(pasta_temp, "projeto_planejamento_observacional")
        dir.create(proj_dir)
        dir.create(file.path(proj_dir, "dados"))
        dir.create(file.path(proj_dir, "R"))
        
        # Dados da coleta
        coleta <- tabela_coleta_dados()
        dicionario <- dicionario_dados()
        writexl::write_xlsx(list(coleta = coleta, dicionario = dicionario), file.path(proj_dir, "dados", "planilha_coleta.xlsx"))
        utils::write.csv(coleta, file.path(proj_dir, "dados", "planilha_coleta.csv"), row.names = FALSE, fileEncoding = "UTF-8")
        
        # Script modelo R
        r_script <- c(
          "# Planejamento de Delineamento Observacional — CatalyseR",
          "# Importação e Análise Preliminar",
          "",
          "library(readxl)",
          "dados <- read_excel('dados/planilha_coleta.xlsx', sheet = 'coleta')",
          "",
          "# Visualização rápida da estrutura",
          "head(dados)",
          "summary(dados)"
        )
        writeLines(r_script, file.path(proj_dir, "R", "01_importar_coleta.R"))
        
        # Rproj
        rproj <- c("Version: 1.0", "RestoreWorkspace: Default", "SaveWorkspace: Default")
        writeLines(rproj, file.path(proj_dir, "projeto.Rproj"))
        
        setwd(pasta_temp)
        utils::zip(file, files = "projeto_planejamento_observacional")
      }
    )

    invisible(list(
      pergunta = shiny::reactive(input$pergunta),
      fator_nome = fator_nome_limpo,
      grupos = grupos_lista,
      ficha = ficha
    ))
  })
}
