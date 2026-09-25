# =============================================================================
# funcoes_sorteio.R
# -----------------------------------------------------------------------------
# Sorteio de unidades a partir de um MARCO AMOSTRAL, antes da coleta.
#
# O marco amostral é uma tabela tidy com uma linha por unidade candidata
# (embarcação, estação, praia, ponto). Uma coluna identifica a unidade; as
# outras são atributos que os métodos usam:
#   - aleatória simples ("aas")         usa só o identificador e o total N;
#   - estratificada ("estratificada")   usa uma coluna de estrato;
#   - sistemática ("sistematica")       usa uma coluna de ordem e um intervalo k;
#   - conglomerados ("conglomerados")   usa uma coluna de conglomerado (embarcação,
#                                       desembarque) e sorteia em um ou dois estágios.
#
# Aqui ainda não há nada medido. Por isso estas funções só escolhem QUAIS
# unidades visitar; estimar médias e totais é trabalho da análise, depois.
#
# A mesma função serve a tela "Como sortear a amostra" e os delineamentos
# (por exemplo, sortear os indivíduos de cada espécie no comparativo).
#
# Seções:
#   1. Correção para população finita
#   2. Conferência do marco
#   3. Alocação entre estratos
#   4. Sorteio (função principal)
#   5. Registro e código para reproduzir
# =============================================================================


# ---- 1. Correção para população finita ---------------------------------------

# Reduz o n planejado quando a população é pequena e conhecida.
# n: tamanho calculado como se a população fosse infinita (ex.: em "Quantos coletar").
# N: total de unidades do marco amostral.
corrigir_n_finito <- function(n, N) {
  # A fórmula usual é n / (1 + (n - 1) / N), arredondada para cima.
  n_corrigido <- n / (1 + (n - 1) / N)
  # Arredondamos para cima para não ficar abaixo da precisão desejada.
  as.integer(ceiling(n_corrigido))
}


# ---- 2. Conferência do marco -------------------------------------------------

# Devolve uma lista de problemas do marco; lista vazia significa marco pronto.
conferir_marco <- function(marco, id) {
  # Guardamos cada problema encontrado neste vetor.
  problemas <- character()
  # Sem linhas não há o que sortear.
  if (is.null(marco) || nrow(marco) == 0) {
    return("O marco amostral está vazio.")
  }
  # A coluna de identificador é obrigatória.
  if (!id %in% names(marco)) {
    return(sprintf("A coluna de identificador '%s' não existe no marco.", id))
  }
  # Lemos os identificadores como texto para comparar sem surpresas.
  ids <- trimws(as.character(marco[[id]]))
  # Cada unidade precisa ter um identificador preenchido.
  if (any(is.na(ids) | ids == "")) {
    problemas <- c(problemas, "Há unidades sem identificador. Preencha ou remova essas linhas.")
  }
  # E nenhum identificador pode se repetir.
  repetidos <- unique(ids[duplicated(ids) & !is.na(ids) & ids != ""])
  if (length(repetidos)) {
    problemas <- c(problemas, sprintf(
      "Identificadores repetidos: %s. Cada linha deve ser uma unidade diferente.",
      paste(utils::head(repetidos, 5), collapse = ", ")
    ))
  }
  problemas
}


# ---- 3. Alocação entre estratos ----------------------------------------------

# Distribui n entre os estratos.
# tamanhos: vetor nomeado com o número de unidades de cada estrato (N_h).
# alocacao: "proporcional" (n_h segue o tamanho do estrato) ou "igual" (n em cada).
alocar_estratos <- function(tamanhos, n, alocacao = "proporcional") {
  # Na alocação igual, cada estrato recebe o mesmo n.
  if (identical(alocacao, "igual")) {
    return(stats::setNames(rep(as.integer(n), length(tamanhos)), names(tamanhos)))
  }
  # Na proporcional, cada estrato recebe a sua fração de n (pode sair quebrada).
  cota <- n * tamanhos / sum(tamanhos)
  # Começamos pela parte inteira de cada cota.
  n_h <- floor(cota)
  # Sobram algumas unidades para completar n.
  sobra <- n - sum(n_h)
  # Elas vão para os estratos com as maiores partes decimais (maiores restos).
  if (sobra > 0) {
    ordem_restos <- order(cota - n_h, decreasing = TRUE)
    n_h[ordem_restos[seq_len(sobra)]] <- n_h[ordem_restos[seq_len(sobra)]] + 1
  }
  # Devolvemos números inteiros, com o nome de cada estrato.
  stats::setNames(as.integer(n_h), names(tamanhos))
}


# ---- 4. Sorteio (função principal) -------------------------------------------

# Sorteia unidades do marco amostral com semente reprodutível.
#
# marco    : tabela com uma linha por unidade candidata.
# metodo   : "aas", "estratificada" ou "sistematica".
# semente  : número inteiro informado pelo pesquisador.
# id       : nome da coluna de identificador (obrigatória).
# n        : quantas unidades sortear (aas e estratificada).
# corrigir_finita : na aas, TRUE aplica a correção para população finita a n.
# estrato  : nome da coluna de estrato (estratificada).
# alocacao : "proporcional" (padrão da tela) ou "igual" (útil nos delineamentos).
# ordem    : nome da coluna que dá a sequência das unidades (sistemática).
# k        : intervalo entre unidades sorteadas (sistemática).
# conglomerado    : coluna que diz a qual grupo cada unidade pertence (conglomerados).
# n_conglomerados : quantos conglomerados sortear no primeiro estágio.
# estagios        : "unico" (medir todas as unidades dos conglomerados sorteados)
#                   ou "dois" (subamostrar m unidades dentro de cada um).
# m               : unidades por conglomerado no segundo estágio.
#
# Devolve uma lista com: sorteados (as linhas escolhidas, com todas as colunas
# do marco), alocacao (na estratificada), avisos e registro (o que foi feito).
sortear_marco <- function(marco, metodo, semente, id = names(marco)[1],
                          n = NULL, corrigir_finita = FALSE,
                          estrato = NULL, alocacao = "proporcional",
                          ordem = NULL, k = NULL,
                          conglomerado = NULL, n_conglomerados = NULL,
                          estagios = "unico", m = NULL) {
  # Trabalhamos com um data.frame simples, qualquer que seja a origem da tabela.
  marco <- as.data.frame(marco, stringsAsFactors = FALSE, check.names = FALSE)
  # Antes de sortear, o marco precisa ter identificadores únicos e preenchidos.
  problemas <- conferir_marco(marco, id)
  if (length(problemas)) stop(paste(problemas, collapse = " "), call. = FALSE)
  # A semente é obrigatória: é ela que permite refazer o mesmo sorteio.
  if (is.null(semente) || is.na(suppressWarnings(as.integer(semente)))) {
    stop("Informe a semente do sorteio (um número inteiro).", call. = FALSE)
  }
  # N é o total de unidades candidatas.
  N <- nrow(marco)
  # Os avisos não impedem o sorteio, mas precisam ser lidos.
  avisos <- character()
  # Fixamos a semente uma única vez, logo antes de qualquer sorteio.
  set.seed(as.integer(semente))

  # Cada método escolhe as posições (linhas) das unidades sorteadas.
  resultado <- switch(metodo,

    # Aleatória simples: todas as unidades têm a mesma chance.
    aas = {
      # O n precisa ser um inteiro positivo.
      n_pedido <- as.integer(n)
      if (length(n_pedido) != 1 || is.na(n_pedido) || n_pedido < 1) {
        stop("Informe quantas unidades sortear (n maior ou igual a 1).", call. = FALSE)
      }
      # Com a correção, o n planejado encolhe conforme o tamanho do marco.
      n_final <- if (isTRUE(corrigir_finita)) corrigir_n_finito(n_pedido, N) else n_pedido
      # Não é possível sortear mais unidades do que existem.
      if (n_final > N) {
        stop(sprintf("O marco tem %d unidades; não dá para sortear %d.", N, n_final), call. = FALSE)
      }
      # Sorteamos as posições sem reposição.
      posicoes <- sample.int(N, n_final)
      # Guardamos as posições na ordem do marco, que é mais prática no campo.
      list(posicoes = sort(posicoes), n_pedido = n_pedido, n_final = n_final, tabela_alocacao = NULL)
    },

    # Estratificada: sorteamos separadamente dentro de cada estrato.
    estratificada = {
      # A coluna de estrato precisa existir no marco.
      if (is.null(estrato) || !estrato %in% names(marco)) {
        stop("Escolha a coluna de estrato (por exemplo, porto ou região).", call. = FALSE)
      }
      # Lemos os estratos como texto.
      grupos <- as.character(marco[[estrato]])
      # Toda unidade precisa pertencer a um estrato.
      if (any(is.na(grupos) | trimws(grupos) == "")) {
        stop("Há unidades sem estrato. Preencha a coluna de estrato antes de sortear.", call. = FALSE)
      }
      # Estrato com uma unidade em cada linha não é estrato: é o identificador de novo.
      if (identical(estrato, id) || !anyDuplicated(grupos)) {
        stop("Cada estrato teria uma só unidade. Escolha uma coluna que agrupe as unidades, como porto ou região.", call. = FALSE)
      }
      # O n precisa ser um inteiro positivo.
      n_pedido <- as.integer(n)
      if (length(n_pedido) != 1 || is.na(n_pedido) || n_pedido < 1) {
        stop("Informe quantas unidades sortear (n maior ou igual a 1).", call. = FALSE)
      }
      # Listamos os estratos numa ordem que não depende do idioma do computador,
      # para que a mesma semente dê o mesmo sorteio no Windows, Linux ou Mac.
      nomes_estratos <- sort(unique(grupos), method = "radix")
      # Contamos as unidades de cada estrato.
      tamanhos <- vapply(nomes_estratos, function(h) sum(grupos == h), integer(1))
      # O total a sortear não pode passar do total do marco.
      if (identical(alocacao, "proporcional") && n_pedido > N) {
        stop(sprintf("O marco tem %d unidades; não dá para sortear %d.", N, n_pedido), call. = FALSE)
      }
      # Distribuímos n entre os estratos.
      n_h <- alocar_estratos(tamanhos, n_pedido, alocacao)
      # Nenhum estrato pode receber mais unidades do que tem.
      excedidos <- names(n_h)[n_h > tamanhos]
      if (length(excedidos)) {
        stop(sprintf("Estrato(s) com menos unidades do que o pedido: %s.",
                     paste(sprintf("%s (tem %d)", excedidos, tamanhos[excedidos]), collapse = ", ")),
             call. = FALSE)
      }
      # Um estrato com zero sorteados fica de fora da amostra: melhor avisar.
      vazios <- names(n_h)[n_h == 0]
      if (length(vazios)) {
        avisos <- c(avisos, sprintf(
          "Com n = %d, o(s) estrato(s) %s não recebe(m) nenhuma unidade. Aumente n se eles precisam estar na amostra.",
          n_pedido, paste(vazios, collapse = ", ")))
      }
      # Sorteamos dentro de cada estrato, um de cada vez, sempre na mesma ordem.
      posicoes <- unlist(lapply(names(n_h), function(h) {
        # Posições das unidades deste estrato no marco.
        candidatas <- which(grupos == h)
        # Sorteamos n_h delas (sample.int evita a armadilha de sample() com um só valor).
        candidatas[sample.int(length(candidatas), n_h[[h]])]
      }))
      # Tabela de alocação para mostrar ao pesquisador.
      tabela <- data.frame(estrato = names(n_h), N_h = as.integer(tamanhos),
                           n_h = as.integer(n_h), stringsAsFactors = FALSE)
      list(posicoes = sort(posicoes), n_pedido = n_pedido, n_final = length(posicoes),
           tabela_alocacao = tabela)
    },

    # Sistemática: uma unidade a cada k, a partir de um início sorteado.
    sistematica = {
      # A coluna de ordem precisa existir no marco.
      if (is.null(ordem) || !ordem %in% names(marco)) {
        stop("Escolha a coluna de ordem (por exemplo, a sequência ao longo do transecto).", call. = FALSE)
      }
      # O intervalo k precisa ser um inteiro entre 1 e N.
      k <- as.integer(k)
      if (length(k) != 1 || is.na(k) || k < 1 || k > N) {
        stop(sprintf("Informe um intervalo k entre 1 e %d.", N), call. = FALSE)
      }
      # Lemos a ordem; se todos os valores forem números, ordenamos como números.
      valores <- marco[[ordem]]
      como_numero <- suppressWarnings(as.numeric(as.character(valores)))
      chave <- if (!anyNA(como_numero[!is.na(valores)])) como_numero else as.character(valores)
      # Unidades sem posição na sequência não podem entrar na sistemática.
      if (anyNA(chave)) {
        stop("Há unidades sem valor na coluna de ordem. Preencha antes de sortear.", call. = FALSE)
      }
      # Posições do marco já arrumadas na sequência de campo (ordem sem depender do idioma).
      sequencia <- order(chave, method = "radix")
      # Sorteamos o ponto de partida entre 1 e k.
      partida <- sample.int(k, 1)
      # A partir dele, andamos de k em k até o fim da lista.
      passos <- seq(partida, N, by = k)
      # Se a ordem esconder um ciclo com o mesmo período de k, a amostra fica viciada.
      avisos <- c(avisos, "Confira se a ordem da lista não tem um ciclo que coincida com k (por exemplo, marés ou dias da semana).")
      list(posicoes = sequencia[passos], n_pedido = NA_integer_, n_final = length(passos),
           tabela_alocacao = NULL, partida = partida)
    },

    # Conglomerados: primeiro sorteamos grupos inteiros, depois (se for o caso) unidades dentro deles.
    conglomerados = {
      # A coluna de conglomerado precisa existir no marco.
      if (is.null(conglomerado) || !conglomerado %in% names(marco)) {
        stop("Escolha a coluna de conglomerado (por exemplo, a embarcação de cada peixe).", call. = FALSE)
      }
      # Lemos os conglomerados como texto.
      grupos <- as.character(marco[[conglomerado]])
      # Toda unidade precisa pertencer a um conglomerado.
      if (any(is.na(grupos) | trimws(grupos) == "")) {
        stop("Há unidades sem conglomerado. Preencha a coluna antes de sortear.", call. = FALSE)
      }
      # Se cada linha for um grupo, a coluna é o identificador, não um conglomerado.
      if (identical(conglomerado, id) || !anyDuplicated(grupos)) {
        stop("Cada conglomerado teria uma só unidade. Escolha a coluna que agrupa as unidades, como a embarcação.", call. = FALSE)
      }
      # Listamos os conglomerados numa ordem que não depende do idioma do computador.
      disponiveis <- sort(unique(grupos), method = "radix")
      # Quantos conglomerados sortear no primeiro estágio.
      n_grupos <- as.integer(n_conglomerados)
      if (length(n_grupos) != 1 || is.na(n_grupos) || n_grupos < 1 || n_grupos > length(disponiveis)) {
        stop(sprintf("Informe quantos conglomerados sortear, entre 1 e %d.", length(disponiveis)), call. = FALSE)
      }
      # Primeiro estágio: sorteamos os conglomerados, todos com a mesma chance.
      escolhidos <- sort(disponiveis[sample.int(length(disponiveis), n_grupos)], method = "radix")
      # Segundo estágio: dentro de cada conglomerado sorteado, todas as unidades ou m delas.
      dois_estagios <- identical(estagios, "dois")
      m_pedido <- if (dois_estagios) as.integer(m) else NA_integer_
      if (dois_estagios && (length(m_pedido) != 1 || is.na(m_pedido) || m_pedido < 1)) {
        stop("Informe quantas unidades sortear dentro de cada conglomerado.", call. = FALSE)
      }
      # Guardamos os conglomerados menores que m para avisar depois.
      pequenos <- character()
      posicoes <- unlist(lapply(escolhidos, function(g) {
        # Posições das unidades deste conglomerado no marco.
        candidatas <- which(grupos == g)
        # Estágio único, ou conglomerado menor que m: medimos todas as unidades.
        if (!dois_estagios || length(candidatas) <= m_pedido) {
          if (dois_estagios && length(candidatas) < m_pedido) pequenos <<- c(pequenos, g)
          return(candidatas)
        }
        # Dois estágios: sorteamos m unidades dentro do conglomerado.
        candidatas[sample.int(length(candidatas), m_pedido)]
      }))
      if (length(pequenos)) {
        avisos <- c(avisos, sprintf(
          "O(s) conglomerado(s) %s tem(têm) menos de %d unidades; todas foram incluídas.",
          paste(pequenos, collapse = ", "), m_pedido))
      }
      # Tabela por conglomerado: quantas unidades tinha e quantas entram.
      tabela <- data.frame(
        conglomerado = escolhidos,
        M_i = vapply(escolhidos, function(g) sum(grupos == g), integer(1)),
        m_i = vapply(escolhidos, function(g) sum(grupos[posicoes] == g), integer(1)),
        stringsAsFactors = FALSE, row.names = NULL
      )
      # Mantemos as unidades agrupadas por conglomerado, na ordem do marco dentro de cada um.
      posicoes <- posicoes[order(match(grupos[posicoes], escolhidos), posicoes)]
      list(posicoes = posicoes, n_pedido = NA_integer_, n_final = length(posicoes),
           tabela_alocacao = tabela, n_conglomerados = n_grupos,
           conglomerados_disponiveis = length(disponiveis), m = m_pedido)
    },

    # Qualquer outro nome de método é um engano de digitação.
    stop("Método desconhecido. Use 'aas', 'estratificada', 'sistematica' ou 'conglomerados'.", call. = FALSE)
  )

  # As linhas sorteadas levam consigo todas as colunas do marco.
  sorteados <- marco[resultado$posicoes, , drop = FALSE]
  # Em conglomerados, as unidades ficam aninhadas no grupo sempre que há mais de uma por grupo.
  e_conglomerado <- identical(metodo, "conglomerados")
  rownames(sorteados) <- NULL

  # O registro guarda tudo o que é preciso para refazer este sorteio.
  registro <- list(
    metodo = metodo,
    semente = as.integer(semente),
    id = id,
    N = N,
    n_pedido = resultado$n_pedido,
    n = resultado$n_final,
    corrigir_finita = isTRUE(corrigir_finita) && identical(metodo, "aas"),
    estrato = if (identical(metodo, "estratificada")) estrato else NULL,
    alocacao = if (identical(metodo, "estratificada")) alocacao else NULL,
    ordem = if (identical(metodo, "sistematica")) ordem else NULL,
    k = if (identical(metodo, "sistematica")) k else NULL,
    partida = resultado$partida,
    conglomerado = if (e_conglomerado) conglomerado else NULL,
    estagios = if (e_conglomerado) estagios else NULL,
    n_conglomerados = resultado$n_conglomerados,
    conglomerados_disponiveis = resultado$conglomerados_disponiveis,
    m = if (e_conglomerado && identical(estagios, "dois")) resultado$m else NULL,
    # Aninhamento: unidade dentro do conglomerado, que entra como efeito aleatório na análise.
    aninhado = e_conglomerado && any(resultado$tabela_alocacao$m_i > 1),
    data = format(Sys.Date(), "%Y-%m-%d")
  )

  list(sorteados = sorteados, alocacao = resultado$tabela_alocacao,
       avisos = avisos, registro = registro)
}


# ---- 5. Registro e código para reproduzir ------------------------------------

# Nome do método por extenso, para tabelas e textos.
nome_metodo_sorteio <- function(metodo) {
  dplyr::case_when(
    metodo == "aas" ~ "Aleatória simples",
    metodo == "estratificada" ~ "Estratificada",
    metodo == "sistematica" ~ "Sistemática",
    metodo == "conglomerados" ~ "Conglomerados",
    TRUE ~ metodo
  )
}

# Transforma o registro numa tabela de duas colunas, fácil de guardar no Excel.
tabela_registro_sorteio <- function(registro) {
  # Cada campo preenchido vira uma linha "campo | valor".
  campos <- c(
    "Método" = nome_metodo_sorteio(registro$metodo),
    "Semente" = registro$semente,
    "Coluna de identificador" = registro$id,
    "Unidades no marco (N)" = registro$N,
    "n informado" = if (!is.na(registro$n_pedido %||% NA)) registro$n_pedido else NA,
    "Correção para população finita" = if (identical(registro$metodo, "aas")) if (isTRUE(registro$corrigir_finita)) "sim" else "não" else NA,
    "Coluna de estrato" = registro$estrato %||% NA,
    "Alocação" = registro$alocacao %||% NA,
    "Coluna de ordem" = registro$ordem %||% NA,
    "Intervalo k" = registro$k %||% NA,
    "Ponto de partida" = registro$partida %||% NA,
    "Coluna de conglomerado" = registro$conglomerado %||% NA,
    "Conglomerados sorteados" = if (!is.null(registro$n_conglomerados)) sprintf("%d de %d", registro$n_conglomerados, registro$conglomerados_disponiveis) else NA,
    "Estágios" = if (!is.null(registro$estagios)) if (identical(registro$estagios, "dois")) "dois (subamostra no conglomerado)" else "único (todas as unidades)" else NA,
    "Unidades por conglomerado (m)" = registro$m %||% NA,
    "Aninhamento" = if (isTRUE(registro$aninhado)) sprintf("unidades aninhadas em %s (efeito aleatório)", registro$conglomerado) else NA,
    "Unidades sorteadas (n)" = registro$n,
    "Data do sorteio" = registro$data
  )
  # Descartamos os campos que não se aplicam ao método usado.
  campos <- campos[!is.na(campos)]
  data.frame(campo = names(campos), valor = as.character(campos), stringsAsFactors = FALSE)
}

# Escreve o comando R que refaz exatamente o mesmo sorteio.
codigo_sorteio <- function(registro, arquivo_marco = "dados/marco_amostral.xlsx") {
  # Texto entre aspas, no formato que o R entende.
  q <- function(x) encodeString(x, quote = '"')
  # Argumentos que todos os métodos usam.
  argumentos <- c(
    sprintf("metodo = %s", q(registro$metodo)),
    sprintf("semente = %d", registro$semente),
    sprintf("id = %s", q(registro$id))
  )
  # Argumentos próprios de cada método.
  extras <- switch(registro$metodo,
    aas = c(sprintf("n = %d", registro$n_pedido),
            if (isTRUE(registro$corrigir_finita)) "corrigir_finita = TRUE"),
    estratificada = c(sprintf("n = %d", registro$n_pedido),
                      sprintf("estrato = %s", q(registro$estrato)),
                      if (!identical(registro$alocacao, "proporcional")) sprintf("alocacao = %s", q(registro$alocacao))),
    sistematica = c(sprintf("ordem = %s", q(registro$ordem)),
                    sprintf("k = %d", registro$k)),
    conglomerados = c(sprintf("conglomerado = %s", q(registro$conglomerado)),
                      sprintf("n_conglomerados = %d", registro$n_conglomerados),
                      if (identical(registro$estagios, "dois")) c("estagios = \"dois\"", sprintf("m = %d", registro$m)))
  )
  c(
    sprintf("# Sorteio feito na CatalyseR em %s. Rodar de novo devolve as mesmas unidades.", registro$data),
    "library(readxl)",
    "source(\"R/funcoes_sorteio.R\")",
    "",
    "# O marco amostral: uma linha por unidade candidata.",
    sprintf("marco <- read_excel(%s, sheet = \"marco_amostral\")", q(arquivo_marco)),
    "",
    sprintf("# %s com semente %d.", nome_metodo_sorteio(registro$metodo), registro$semente),
    "sorteio <- sortear_marco(",
    paste0("  marco,\n  ", paste(c(argumentos, extras), collapse = ",\n  ")),
    ")",
    "",
    "# As unidades que serão visitadas em campo.",
    "sorteio$sorteados"
  )
}

# Operador "se vazio, use o padrão" (definido aqui para o arquivo funcionar sozinho).
if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a
