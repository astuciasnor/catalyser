# =============================================================================
# Kruskal-Wallis "completo" (versão moderna e robusta) — ecossistema EAPA
# Fluxo: omnibus -> pós-teste de Dunn -> letras (CLD) -> boxplot com as letras.
#
# Dados: EAPADados::cpue_tubarao — CPUE de tubarão por ano (1995–2007), REAIS.
# Resultado esperado (validado, dados completos): H = 172,13 | gl = 12 |
#   p ≈ 9,5e-27 (significativo); tamanho de efeito epsilon² ≈ 0,41 (grande);
#   Dunn/Bonferroni: os anos iniciais (1995–99) diferem dos finais (2004–07).
#
# install.packages(c("rstatix", "rcompanion", "ggplot2", "dplyr"))
# =============================================================================

library(EAPADados)
library(dplyr)
library(ggplot2)
library(rstatix)     # kruskal_test, kruskal_effsize, dunn_test
library(rcompanion)  # cldList (letras de significância a partir do pós-teste)

data(cpue_tubarao)

# 1) Preparar: Year como fator (usa todos os dados) --------------------------
#    Para replicar o filtro do estudo original, acrescente: |> filter(CPUE <= 28)
dados <- cpue_tubarao |>
  mutate(Year = factor(Year))

# 2) Kruskal-Wallis (omnibus) + tamanho de efeito -----------------------------
kw     <- kruskal_test(dados, CPUE ~ Year)
efeito <- kruskal_effsize(dados, CPUE ~ Year)   # epsilon²
print(kw)
print(efeito)

# 3) Pós-teste de Dunn (Bonferroni): QUAIS anos diferem entre si ---------------
dunn <- dunn_test(dados, CPUE ~ Year, p.adjust.method = "bonferroni")
print(dunn, n = Inf)

# 4) Letras de significância (CLD) — anos com a MESMA letra NÃO diferem --------
dunn$comparison <- paste(dunn$group1, dunn$group2, sep = " - ")
# ATENÇÃO: remove.zero = FALSE é essencial — senão o cldList apaga os zeros dos
# rótulos (2000 -> "2", 2001 -> "21"...) e o eixo ganha categorias fantasmas.
cld <- cldList(p.adj ~ comparison, data = dunn, threshold = 0.05, remove.zero = FALSE)
names(cld)[names(cld) == "Group"] <- "Year"
cld$y <- 49   # letras alinhadas numa linha única, no topo do gráfico

# 5) Boxplot por ano (uma cor por ano) + média (losango preto) + letras -------
p <- ggplot(dados, aes(x = Year, y = CPUE, fill = Year, color = Year)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.45, linewidth = 0.5) +
  geom_jitter(position = position_jitter(width = 0.2), alpha = 0.55, size = 1.6) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2.7,
               fill = "black", color = "black") +           # losango = média
  geom_text(data = cld, aes(x = Year, y = y, label = Letter), inherit.aes = FALSE,
            fontface = "bold", size = 5, color = "grey20") + # letras no topo
  coord_cartesian(ylim = c(0, 50)) +
  labs(x = NULL, y = "CPUE") +
  theme_classic(base_size = 13) +
  theme(legend.position = "none",                            # cores só decorativas
        panel.grid.major.x = element_blank(),
        axis.text = element_text(color = "grey20"))

print(p)
# ggsave("kruskal_tubarao.png", p, width = 9, height = 5, dpi = 300, bg = "white")
