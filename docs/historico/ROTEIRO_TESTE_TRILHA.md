# Roteiro de teste — Trilha de Preparo (CatalyseR)

> **Documento histórico.** O roteiro antecede a organização atual dos módulos
> **Pivotar e Separar Dados**, **Organizar Variáveis** e **Bases Derivadas**.
> Use os roteiros em `docs/testes/fases-3/` para homologação vigente.

Objetivo: ver a **trilha de transformações** crescer e reagir ao vivo.
Dados: `inst/app/dados/Treino-Transformacoes.xlsx`, aba **biometria** (consulte a
aba `guia` do próprio arquivo em caso de dúvida).

---

## 0. Importar
Preparando Dados → **Importação e Visualização** → Origem "Arquivo Local" →
escolher `Treino-Transformacoes.xlsx` → aba **biometria**.
👀 71 linhas; `especie`/`local`/`sexo` com grafias diferentes; `comprimento_cm` e
`peso_g` com células vazias (NA).

## 1. Abrir a Trilha
Preparando Dados → **Trilha de Preparo**. A trilha mostra só o nó **dados brutos**
(navy) e um "+ adicione um tratamento".

## 2. Padronizar texto  → chip 1
Tipo = **Padronizar texto** · coluna `especie` · Ação "Iniciais Maiúsculas" →
**Adicionar à trilha**.
👀 A caixa 1 aparece; na aba **Resultado**, "sardinha"/"SARDINHA" viram "Sardinha".
(Repita para `local` e `sexo`, se quiser ver a trilha somar caixas.)

## 3. Remover duplicatas  → chip 2
Tipo = **Remover duplicatas** · deixe as colunas-chave **vazias** (linha inteira) →
Adicionar.
👀 71 → **68 linhas** (as 3 duplicatas somem); o status atualiza.

## 4. Tratar NA  → chips 3–4
Tipo = **Dados faltantes** · coluna `comprimento_cm` · "Imputar a mediana" →
Adicionar. (repare na contagem de NA exibida antes de aplicar). Repita para `peso_g`.
👀 As células vazias são preenchidas; a contagem de NA vai a zero.

## 5. Padronizar (z-score)  → chip 5
Tipo = **Padronizar / Escalar** · coluna `comprimento_cm` · "Escore z" ·
nome `comprimento_cm_z` → Adicionar.
👀 Nova coluna com média ≈ 0.

## 6. Classes de tamanho (binning)  → chip 6
Tipo = **Classes de tamanho** · coluna `comprimento_cm` · 4 classes · "Amplitude
igual" · nome `comprimento_cm_classe` → Adicionar.
👀 Nova coluna categórica com faixas, ex.: (8,17], (17,26]…

## 7. Dicotomizar  → chip 7
Tipo = **Dicotomizar** · coluna `comprimento_cm` · limiar `>= 25` · nome `maturo` →
Adicionar.
👀 Coluna 0/1 (útil depois na regressão logística).

---

## VER a trilha viva

## 8. Reordenar (a "viagem no tempo")
Em "Etapa selecionada", escolha **Padronizar z-score** e clique **↑** algumas vezes.
👀 O **Resultado** re-deriva sozinho e a aba **Script de preparo** reordena junto —
como se aquela etapa sempre tivesse estado ali.

## 9. Desativar
Selecione **Classes de tamanho** e clique no botão **power** (ativar/desativar).
👀 A caixa fica **cinza riscada** e a coluna some do Resultado. Reative para voltar.

## 10. Provocar um erro (a rede de segurança) 🔴
Adicione: **Dicotomizar** categórica na coluna `comprimento_cm_classe` (escolha um
nível/faixa como "1") · nome `classe_alvo` → Adicionar. Funciona (fica no fim).
Agora selecione essa etapa e clique **↑** até ela ficar **ACIMA** de "Classes de
tamanho".
👀 A caixa fica **VERMELHA com !** — `comprimento_cm_classe` ainda não existe naquele
ponto. O status mostra "1 etapa com erro", mas o resto do resultado continua de pé.
Mova **↓** de volta → tudo volta ao normal.

## 11. Script + downloads
Aba **Script de preparo**: o R reprodutível das etapas ativas, na ordem lógica.
Botões **Baixar script (.R)** e **Baixar dados preparados (.xlsx)**.
👀 Nenhum clique em "usar nas análises" foi preciso — a trilha já alimenta tudo.

---

## 12. (Opcional) menus vizinhos — ainda SEPARADOS da trilha
Estes exercitam os outros menus, mas **ainda não aparecem no desenho da trilha**
(a unificação é a Fase 3):
- **Organizar Variáveis → Criação de Variáveis**: fator de condição
  `100*peso_g/comprimento_cm^3`;
  reescalar `peso_g` (g → kg).
- **Arrumar**: separar `amostra` pelo "_" (→ `local`, `estacao`); empilhar a aba
  `desembarques_largo` (colunas 2022/2023/2024).
- **Contingência**: `especie` × `local`.
