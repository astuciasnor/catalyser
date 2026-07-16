# Matriz de branches das Fases 3

Situação verificada em **15/07/2026** após o envio das nove branches. A `main`
continua em `d804bdb`; cada branch abaixo possui agora uma referência remota
própria, sem merge na `main`.

| Ordem | Branch | Commit | Entrega principal | Inclui | Remoto | Teste manual |
|---:|---|---|---|---|---|---|
| 0 | `main` | `d804bdb` | versão estável anterior ao ciclo | base congelada | sim | referência |
| 1 | `feature/fase-3a-registro-bases` | `d9aa689` | 3A + 3A.1: registro em estrela, cache lazy, estados e recálculo manual | `main` | sim | pendente |
| 2 | `feature/fase-3b-transformacoes-ramos` | `99a4b1a` | 3B.1: editor de receitas específicas | anterior | sim | pendente |
| 3 | `feature/fase-3b2-seletor-base-piloto` | `07c2cb8` | 3B.2: seletor piloto na regressão logística | anterior | sim | pendente |
| 4 | `feature/fase-3b3-seletores-prioritarios` | `ced3fdb` | 3B.3: seletor reutilizável em sete módulos | anterior | sim | pendente |
| 5 | `feature/fase-3c-registro-execucoes` | `e84e7b3` | 3C: registro independente de execuções | anterior | sim | pendente |
| 6 | `feature/fase-3c1-execucao-explicita` | `6115420` | 3C.1: botão Executar e rascunho pendente | anterior | sim | pendente |
| 7 | `feature/fase-3d-comunicacao-resultados` | `4140e08` | 3D: estúdio editorial de comunicação | anterior | sim | pendente |
| 8 | `feature/fase-3e-exportacao-integrada` | `e7b449d` | 3E: Word seletivo e Projeto R completo | anterior | sim | pendente |
| 9 | `chore/revisao-fases-3` | `HEAD` da branch | documentação, limpeza e bateria final | 3E | sim | pendente |

## Dependência

```text
main d804bdb
└── 3A/3A.1 d9aa689
    └── 3B.1 99a4b1a
        └── 3B.2 07c2cb8
            └── 3B.3 ced3fdb
                └── 3C e84e7b3
                    └── 3C.1 6115420
                        └── 3D 4140e08
                            └── 3E e7b449d
                                └── revisão final
```

Testar somente a 3E valida o comportamento acumulado, mas não identifica com
precisão em qual fase uma regressão apareceu. Por isso a homologação manual deve
seguir a ordem da tabela.

## Antes de testar em outro computador

As referências precisam existir no remoto. Quando o autor decidir enviá-las,
execute na máquina de desenvolvimento, uma por vez:

```bash
git push -u origin feature/fase-3a-registro-bases
git push -u origin feature/fase-3b-transformacoes-ramos
git push -u origin feature/fase-3b2-seletor-base-piloto
git push -u origin feature/fase-3b3-seletores-prioritarios
git push -u origin feature/fase-3c-registro-execucoes
git push -u origin feature/fase-3c1-execucao-explicita
git push -u origin feature/fase-3d-comunicacao-resultados
git push -u origin feature/fase-3e-exportacao-integrada
git push -u origin chore/revisao-fases-3
```

Esses comandos criam referências remotas para as branches. Eles não fazem
merge e não modificam `main`.

Se o Windows de teste for a mesma máquina e não se desejar fazer push ainda, é
possível clonar diretamente a pasta local:

```powershell
git clone D:\Claude\eapa\catalyser D:\Testes\catalyser-fases
```

Nesse clone local, as branches aparecem como referências de `origin`, cujo
“remoto” será a pasta de desenvolvimento. Em outro computador, o push é
obrigatório para que as branches possam ser acessadas.
