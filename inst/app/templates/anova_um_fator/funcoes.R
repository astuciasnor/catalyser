# =============================================================================
#  FUNÇÕES PRÓPRIAS DO PROJETO
# =============================================================================
#
#  Este arquivo só DEFINE funções; ele não executa nada sozinho. O relatório
#  (relatorios/relatorio.qmd) carrega tudo daqui, no chunk `pacotes`, com:
#
#      source(here("R", "funcoes.R"))
#
#  Regra prática: se você copiou e colou o mesmo bloco de código duas vezes
#  no relatório, ele provavelmente merece virar uma função aqui.
#
#  Seções deste arquivo (Ctrl+Shift+O no RStudio mostra o sumário):
#    1. Resumos estatísticos ..... resumir_grupo()
#    2. Aparência das figuras .... tema_projeto(), cores_tratamento
#    3. Formatação de números .... fmt(), formatar_p()
#    4. Tabelas para o Word ...... flextable_ocean()
#    5. Manutenção do relatório .. acrescentada pelo exportador a partir do
#                                 template comum: atualizar_codigo(), conferir_codigo()
#
#  As funções das seções 3 e 4 têm o mesmo nome e o mesmo comportamento das
#  que o EAPADados e a CatalyseR usam. Quem vier de lá reconhece; quem sair
#  daqui para lá também.
#
#  Convenção: dentro das funções usamos pacote::funcao() (ex.: dplyr::mutate)
#  em vez de library(). Assim a função funciona em qualquer lugar, mesmo que
#  o pacote não tenha sido carregado.
# =============================================================================



# 1. Resumos estatísticos ------------------------------------------------------

# resumir_grupo() ---------------------------------------------------------
#
# O que faz: calcula, para cada grupo, o número de observações, a média,
#            o desvio padrão, o erro padrão e o intervalo de confiança da
#            média (baseado na distribuição t).
#
# Argumentos:
#   dados    - a tabela (data.frame ou tibble)
#   variavel - a coluna numérica a resumir (sem aspas, ex.: ganho_peso)
#   grupo    - a coluna que define os grupos (sem aspas, ex.: densidade)
#   conf     - nível de confiança do IC (padrão 0.95 = 95 %)
#
# Retorna: uma tabela com uma linha por grupo.
#
# Exemplo:  resumir_grupo(biometria, ganho_peso, densidade)
#
resumir_grupo <- function(dados, variavel, grupo, conf = 0.95) {

  dados |>
    # Separa a tabela em grupos. As chaves duplas {{ }} permitem passar o
    # nome da coluna sem aspas, do mesmo jeito que o dplyr faz.
    dplyr::group_by({{ grupo }}) |>

    # Calcula uma estatística por grupo. Cada linha abaixo cria uma coluna.
    dplyr::summarise(
      n     = sum(!is.na({{ variavel }})),          # observações válidas (sem NA)
      media = mean({{ variavel }}, na.rm = TRUE),   # média, ignorando NA
      dp    = sd({{ variavel }}, na.rm = TRUE),     # desvio padrão
      ep    = dp / sqrt(n),                         # erro padrão da média
      t_crit = qt(1 - (1 - conf) / 2, df = n - 1),  # t crítico (ex.: 2,57 para n = 6)
      ic_inf = media - t_crit * ep,                 # limite inferior do IC
      ic_sup = media + t_crit * ep,                 # limite superior do IC
      .groups = "drop"                              # desfaz o agrupamento ao final
    ) |>

    # O t crítico foi só um passo intermediário; não precisa aparecer na tabela.
    dplyr::select(-t_crit)
}


# 2. Aparência das figuras -----------------------------------------------------

# tema_projeto() ----------------------------------------------------------
#
# O que faz: define UM tema ggplot2 para todas as figuras do projeto.
#            Se quiser mudar a fonte ou o tamanho do texto, muda-se aqui
#            e todas as figuras acompanham.
#
# Argumentos:
#   tamanho_base - tamanho do texto em pontos (padrão 12)
#
# Exemplo:  ggplot(...) + geom_point() + tema_projeto()
#
tema_projeto <- function(tamanho_base = 12) {

  # Parte do theme_minimal (fundo branco, sem moldura) ...
  ggplot2::theme_minimal(base_size = tamanho_base) +

    # ... e ajusta alguns detalhes por cima dele.
    ggplot2::theme(
      panel.grid.minor   = ggplot2::element_blank(),   # tira as linhas de grade secundárias
      panel.grid.major.x = ggplot2::element_blank(),   # tira a grade vertical (sobra a horizontal)
      axis.line = ggplot2::element_line(colour = "grey40", linewidth = 0.4), # linha fina nos eixos
      legend.position     = "bottom",                  # legenda embaixo, não à direita
      plot.title.position = "plot",                    # título alinhado à borda da figura
      plot.background     = ggplot2::element_rect(fill = "white", colour = NA) # fundo branco no Word
    )
}

# cores_tratamento --------------------------------------------------------
#
# Paleta fixa para os níveis do tratamento, na identidade visual do
# ecossistema EAPA (Ocean Gradient): as mesmas cores da CatalyseR e do livro,
# para que uma figura feita aqui pareça da mesma família. A ordem é sempre a
# mesma, para que o nível "50" tenha a mesma cor em todas as figuras.
#
# Se o público tiver pessoas com daltonismo, a paleta Okabe-Ito é a
# alternativa segura: c("#0072B2", "#E69F00", "#009E73", "#CC79A7").
#
cores_tratamento <- c("#0F3B5F",   # azul-marinho (NAVY)
                      "#2E7D8F",   # azul-petróleo (TEAL)
                      "#62B6B7",   # verde-água (SEAFOAM)
                      "#E89B3C",   # âmbar (AMBER)
                      "#E76F51",   # coral (CORAL)
                      "#6A4C93",   # roxo
                      "#1B998B",   # verde-esmeralda
                      "#B23A48")   # vinho



# 3. Formatação de números -----------------------------------------------------

# fmt() -------------------------------------------------------------------
#
# O que faz: formata um número para o texto ou para uma tabela do relatório,
#            com vírgula decimal e um número fixo de casas. Evita o "3.1"
#            em português e o "3,14159" onde bastava "3,14".
#
# Argumentos:
#   x   - um número (ou um vetor de números)
#   dig - casas decimais (padrão 2)
#
# Exemplo:  fmt(3.14159)    ->  "3,14"
#           fmt(0.372, 3)   ->  "0,372"
#           fmt(NA)         ->  "-"
#
fmt <- function(x, dig = 2) {

  # formatC com format = "f" garante casas fixas (não vira notação científica)
  saida <- formatC(x, format = "f", digits = dig, decimal.mark = ",")

  # Um NA formatado vira "NA"; preferimos um traço, como em tabela de artigo.
  saida[is.na(x)] <- "-"

  saida
}

# formatar_p() ------------------------------------------------------------
#
# O que faz: deixa um valor de p apresentável, no padrão brasileiro:
#            "< 0,001" quando é muito pequeno, senão o número com vírgula e
#            3 casas (ex.: "0,032"). Com no_texto = TRUE, já vem com a
#            letra p e o sinal certo, pronto para o meio de uma frase.
#
# Argumentos:
#   p        - um valor de p (ou um vetor deles)
#   digitos  - casas decimais (padrão 3)
#   no_texto - FALSE (padrão) para tabela; TRUE para frase
#
# Exemplo:  formatar_p(0.0004)                  ->  "< 0,001"
#           formatar_p(0.0321)                  ->  "0,032"
#           formatar_p(0.0321, no_texto = TRUE) ->  "p = 0,032"
#           formatar_p(0.0004, no_texto = TRUE) ->  "p < 0,001"
#           formatar_p(NA)                      ->  "-"
#
formatar_p <- function(p, digitos = 3, no_texto = FALSE) {

  muito_pequeno <- !is.na(p) & p < 0.001           # onde p < 0,001 ...

  texto <- ifelse(
    muito_pequeno,
    "< 0,001",                                     # ... escreve "< 0,001";
    format(round(p, digitos),                      # senão arredonda ...
           decimal.mark = ",", nsmall = digitos)   # ... e troca o ponto por vírgula.
  )

  # Para o meio de uma frase: "p < 0,001" ou "p = 0,032".
  if (no_texto) {
    texto <- ifelse(muito_pequeno, paste("p", texto), paste("p =", texto))
  }

  # Onde não há p (a linha do resíduo na ANOVA, por exemplo), o format() acima
  # devolveria o texto "NA". Um traço é mais honesto, e é o mesmo que fmt() faz.
  texto[is.na(p)] <- "-"

  texto
}



# 4. Tabelas para o Word -------------------------------------------------------

# flextable_ocean() -------------------------------------------------------
#
# O que faz: transforma um data.frame numa tabela pronta para o Word, no
#            tema Ocean do ecossistema EAPA: cabeçalho azul-marinho com
#            letras brancas, sem grade interna, primeira coluna à esquerda.
#            É a mesma tabela que a CatalyseR põe no relatório dela. A
#            função foi copiada do EAPADados para este projeto não depender
#            de nada fora do CRAN (o flextable está no CRAN).
#
# Argumentos:
#   tab - um data.frame ou tibble já com os nomes de coluna finais
#
# Exemplo:  resumo_ganho |> flextable_ocean()
#
flextable_ocean <- function(tab) {

  flextable::flextable(tab) |>
    flextable::theme_booktabs() |>                              # linha só em cima e embaixo
    flextable::bg(part = "header", bg = "#0F3B5F") |>           # cabeçalho azul-marinho (NAVY)
    flextable::color(part = "header", color = "white") |>       # letras brancas no cabeçalho
    flextable::bold(part = "header") |>                         # ... em negrito
    flextable::font(fontname = "Times New Roman", part = "all") |>  # mesma fonte do modelo de Word
    flextable::fontsize(size = 10, part = "all") |>
    flextable::align(align = "center", part = "all") |>         # tudo centralizado ...
    flextable::align(j = 1, align = "left", part = "all") |>    # ... menos a primeira coluna
    flextable::padding(padding = 4, part = "all") |>
    flextable::autofit()                                        # largura conforme o conteúdo
}
