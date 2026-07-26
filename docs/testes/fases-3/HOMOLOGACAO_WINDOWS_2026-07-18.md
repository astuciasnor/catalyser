# Homologação no Windows — 18/07/2026

## Ambiente e sequência

- Clone isolado: `catalyser-fases`.
- Fase 3A/3A.1 (`d9aa689`): aprovada no Windows.
- Fase 3B.1 (`99a4b1a`): aprovada no Windows, com os achados abaixo.
- A tentativa de repetir em uma VM Windows 10 foi abandonada porque o sistema se
  mostrou muito lento para a homologação interativa.
- Próxima execução: repetir a Fase 3B/3B.1 na VM `Zorin 18.1 Core`, hospedada no
  mesmo Windows 11. O Zorin apresentou desempenho consideravelmente melhor.
- A pasta `D:\Claude\eapa\transferencia-vm` foi compartilhada com o nome
  `CatalyseR_Compartilhado`; clipboard, transferência de arquivos e arrastar/soltar
  foram ativados em ambas as direções para a sessão da VM.
- Motivo da estratégia: manter host, VM, roteiro e registros no mesmo computador,
  evitando a dificuldade operacional de homologar em máquinas físicas distintas.
- Estado: repetição no Zorin pendente; não avançar para a próxima fase até concluir
  essa repetição.

## Achados

### 1. Limiar do roteiro da Fase 3B.1

No dataset `Treino-Transformacoes.xlsx`, aba `biometria`, `cpue` varia de
0,19 a 4,44. Portanto, o limiar `cpue >= 5` não produz casos positivos e não
permite validar adequadamente o filtro e sua desativação.

Durante a homologação, o limiar foi alterado para `cpue >= 3`, que produz 7
casos positivos e 64 negativos.

### 2. Erro isolado por ordem lógica

Ao mover o filtro de `cpue_alta` para antes da etapa que cria essa variável, a
prévia mostrou `No data available`, sem travar a aplicação. Confirmar também o
estado `Com erro` no Registro de Bases e o retorno a `Atualizada` depois de
restaurar a ordem correta.

### 3. Tornar a Base Compartilhada explícita na interface

A interface ainda não comunica com clareza que `dados_analise` é a Base
Compartilhada. Refinamentos propostos:

- mostrar `Base compartilhada (dados_analise)` como origem dos ramos;
- trocar `Usar nas análises` por `Aplicar à Base Compartilhada` nos módulos que
  promovem resultados;
- criar o marco `Concluir preparo da Base Compartilhada` antes da criação dos
  ramos;
- mostrar dimensões, número de etapas e estado da Base Compartilhada;
- exigir `Reabrir preparo compartilhado` para editá-la depois da conclusão,
  avisando que os ramos ficarão desatualizados;
- elaborar tutorial orientado pelo fluxo completo, do dado ao relatório.

### 4. Agrupar/Sumarizar deve produzir bases derivadas nomeadas

Comportamento atual: `Gerar tabela agrupada` cria uma prévia temporária. O botão
`Usar este resultado nas análises` promove essa tabela para `base_resolvida`,
substituindo a camada que alimenta a Base Compartilhada; não há nome nem registro
de várias tabelas agrupadas.

Recomendação: como a sumarização reduz várias observações a uma linha por grupo
e muda a unidade de análise, o comportamento padrão deve ser salvar o resultado
como **Base Derivada nomeada**, diretamente a partir da Base Compartilhada. Deve
ser possível manter várias tabelas, por exemplo:

- `base_cpue_ano_especie`;
- `base_producao_mensal`;
- `base_desembarque_local`.

Cada tabela deve entrar no Registro de Bases com receita, código R, estado e
cache próprios, preservando a topologia em estrela. A promoção para Base
Compartilhada pode existir apenas como alternativa explícita, quando a mudança
da unidade de observação for realmente comum a todas as análises, acompanhada de
aviso forte.

### 5. Analogia pedagógica para o tutorial

Para usuários iniciantes, apresentar a Base Compartilhada como um **ingrediente
básico**, semelhante ao trigo: ela é preparada uma vez e pode alimentar várias
receitas. Cada Base Derivada é uma preparação específica feita com esse
ingrediente para uma finalidade determinada, sem alterar o ingrediente comum nem
as outras preparações.

Exemplo de narrativa:

> A Base Compartilhada é como o trigo disponível na cozinha: muitas receitas
> partem dele. Para uma análise específica, preparamos uma porção própria — uma
> Base Derivada — sem modificar o trigo usado pelas demais receitas.

A analogia deve acompanhar um diagrama simples da topologia em estrela e ser
retomada no tutorial ao criar o primeiro ramo.
