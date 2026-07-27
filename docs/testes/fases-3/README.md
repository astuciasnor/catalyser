# Homologação das Fases 3

Esta pasta reúne a revisão técnica e os testes manuais do ciclo que liga:

```text
base compartilhada → bases derivadas → análise explícita
                   → comunicação → Word + Projeto R
```

## Regra de segurança

As branches são cumulativas, mas independentes da `main`. Receber `push` de uma
branch de fase **não altera** a versão usada pelos alunos. A `main` só muda por
merge ou por um push dirigido explicitamente a ela.

Faça os testes em um clone separado e execute a aplicação diretamente do
código-fonte. Não reinstale nem atualize a CatalyseR congelada durante o
semestre.

## Documentos

- [Matriz de branches](MATRIZ_BRANCHES.md): commits, dependências, conteúdo e
  situação local/remota.
- [Roteiro para Windows](WINDOWS.md): criação da pasta isolada, troca de branch,
  bateria automática e teste manual.
- [Roteiro para Zorin OS](ZORIN.md): o mesmo processo sem modificar a instalação
  usada pelos alunos.
- [Revisão recursiva de 15/07/2026](REVISAO_RECURSIVA_2026-07-15.md): evidências
  automáticas, achados e limites da revisão.
- [Homologação da Fase 3B.2 no Windows 10](HOMOLOGACAO_WINDOWS_2026-07-25_FASE_3B2.md):
  aprovação do contrato do seletor, correção do `glm` e pendências de interface.
- [Homologação ponta a ponta no Windows](HOMOLOGACAO_END_TO_END_WINDOWS_2026-07-26.md):
  percurso aprovado da Base Compartilhada ao Projeto R e ao relatório Word.
- [Roteiro V15 com duas bases e duas análises](ROTEIRO_TESTE_V15_DUAS_ANALISES.md):
  teste reduzido dos refinamentos de seleção, execução e QMD pedagógico.

## Roteiros manuais por fase

Execute-os nesta ordem:

1. [Fase 3A e 3A.1](ROTEIRO_TESTE_FASE_3A.md)
2. [Fase 3B.1](ROTEIRO_TESTE_FASE_3B.md)
3. [Fase 3B.2](ROTEIRO_TESTE_FASE_3B2.md)
4. [Fase 3B.3](ROTEIRO_TESTE_FASE_3B3.md)
5. [Fase 3C](ROTEIRO_TESTE_FASE_3C.md)
6. [Fase 3C.1](ROTEIRO_TESTE_FASE_3C1.md)
7. [Fase 3D](ROTEIRO_TESTE_FASE_3D.md)
8. [Fase 3E](ROTEIRO_TESTE_FASE_3E.md)

O dataset principal é
`inst/app/dados/Treino-Transformacoes.xlsx`. Cada roteiro informa a aba, as
variáveis e o resultado esperado.

## O que registrar durante a homologação

Para cada branch e sistema operacional, anote:

- data e commit exibido por `git rev-parse --short HEAD`;
- versão do R e do Quarto;
- aprovação ou falha da bateria automática;
- aprovação ou falha de cada bloco manual;
- mensagem completa, captura de tela e sequência mínima para reproduzir falhas;
- se o mesmo comportamento ocorre no outro sistema operacional.

Não corrija uma branch durante o teste sem antes registrar a falha. A correção
deve receber commit próprio, para que o resultado anterior continue auditável.
