# Revisão recursiva das Fases 3 — 15/07/2026

## Escopo

Foram auditados os commits `d9aa689` até `e7b449d`, sempre como snapshots
isolados. Alterações locais do usuário não participaram da revisão.

## Evidências por fase

| Fase | Commit | Sintaxe | Carregamento integral do app | Testes existentes no próprio commit | Teste manual |
|---|---|---|---|---|---|
| 3A + 3A.1 | `d9aa689` | 52 arquivos R, OK | OK | ainda não existia | pendente |
| 3B.1 | `99a4b1a` | 52 arquivos R, OK | OK | ainda não existia | pendente |
| 3B.2 | `07c2cb8` | 52 arquivos R, OK | OK | ainda não existia | pendente |
| 3B.3 | `ced3fdb` | 53 arquivos R, OK | OK | ainda não existia | pendente |
| 3C | `e84e7b3` | 57 arquivos R, OK | OK | 2/2, OK | pendente |
| 3C.1 | `6115420` | 59 arquivos R, OK | OK | 3/3, OK | pendente |
| 3D | `4140e08` | 61 arquivos R, OK | OK | 4/4, OK | pendente |
| 3E | `e7b449d` | 65 arquivos R, OK | OK | 6/6, OK | pendente |

O teste de exportação da 3E renderizou um `.docx` real com Quarto. Na primeira
execução dentro da sandbox, o Quarto não recebeu permissão para criar seu cache
em `AppData`; repetido fora da sandbox, terminou com status zero. Portanto, não
foi classificado como defeito do código.

## Bateria consolidada na branch de revisão

Ambiente: Windows, R 4.6.1 e Quarto 1.9.38.

- sintaxe dos 62 arquivos R de `inst/app`: OK;
- `test_bases_derivadas.R`: OK;
- `test_registro_execucoes.R`: OK;
- `test_execucao_explicita.R`: OK;
- `test_estados_execucao.R`: OK;
- `test_comunicacao_resultados.R`: OK;
- `test_funcoes_projeto_integrado.R`: OK;
- `test_exportacao_comunicacao.R`, com Word real: OK;
- `R CMD build --no-manual`: OK, 100 entradas no pacote e nenhum arquivo de
  `docs/` ou roteiro de desenvolvimento incluído.

Os avisos observados são preexistentes nos dados sintéticos: ajustes lineares
essencialmente perfeitos, `size` depreciado no `ggplot2` e `label.size` ignorado.
Nenhum deles interrompeu a bateria.

## Invariantes revisadas

- toda base derivada nasce diretamente de `dados_analise`;
- editar uma receita aumenta sua versão e invalida o cache;
- o replay é lazy e acionado manualmente;
- erro em um ramo não derruba a aplicação nem valida prévia parcial;
- somente ramos prontos e atualizados entram nos seletores;
- mudar base ou parâmetros torna a análise pendente;
- visitar uma aba não registra automaticamente uma execução;
- cada execução recebe ID independente;
- Comunicação separa acervo completo e seleção do Word;
- dependência desatualizada bloqueia a exportação;
- Word é seletivo e Projeto R preserva todas as execuções.

## Achados e ajustes desta revisão

1. As fases 3A–3B.3 possuíam roteiros manuais, mas não um teste automatizado
   específico para topologia/cache. Foi acrescentado
   `test_bases_derivadas.R`.
2. O `.Rbuildignore` existia localmente, porém era ignorado pelo Git. Ele passou
   a ser versionado para que clones e builds usem as mesmas exclusões.
3. `EVOLUCAO_TRATAMENTO_DADOS.md` ainda dizia que a 3E estava pendente. O status
   foi atualizado.
4. Os roteiros estavam dispersos na raiz. Foram reunidos nesta pasta, sem mover
   as especificações canônicas de arquitetura.
5. Artefatos reproduzíveis de build e inspeção visual foram classificados para
   limpeza; configurações do RStudio e arquivos locais do usuário foram
   preservados.

## Limite da revisão automática

Esta revisão confirma contratos, sintaxe, carregamento, replay, registros e
exportação. Ela não substitui a homologação visual e interativa no Windows e no
Zorin. Até essa etapa terminar, as branches devem permanecer fora de `main`.
