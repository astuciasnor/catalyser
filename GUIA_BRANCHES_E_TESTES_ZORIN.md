# Guia de branches e testes seguros no Zorin

## Finalidade

Este arquivo registra as fases do pipeline de dados enquanto a versão usada
pelos alunos permanece congelada. Deve ser atualizado ao concluir cada fase.

Regras durante o semestre:

1. não mesclar as branches de desenvolvimento na `main`;
2. não reinstalar a CatalyseR usada pelos alunos;
3. testar as branches em uma pasta separada;
4. fazer push somente da branch que estiver pronta para chegar ao Zorin;
5. uma branch posterior inclui o histórico das fases anteriores.

## Registro cumulativo

| Branch | Ponto principal | Conteúdo acumulado | Estado |
|---|---|---|---|
| `main` | `d804bdb` | versão estável anterior às bases derivadas | congelada para os alunos |
| `feature/fase-3a-registro-bases` | `d9aa689` | cadastro de ramos em estrela, cache lazy, estados e recálculo manual | commit local; teste no Zorin pendente |
| `feature/fase-3b-transformacoes-ramos` | `99a4b1a` | tudo da 3A + editor das receitas específicas | commit local; teste no Zorin pendente |
| `feature/fase-3b2-seletor-base-piloto` | `07c2cb8` | tudo da 3B.1 + seletor piloto na Regressão Logística | commit local; teste no Zorin pendente |
| `feature/fase-3b3-seletores-prioritarios` | `HEAD` da branch | tudo da 3B.2 + seletor reutilizável em sete módulos prioritários | implementada; teste no Zorin pendente |

Nenhuma dessas branches deve alterar a `main` apenas por receber push. Fazer
push cria ou atualiza a referência remota da própria branch.

## Fases ainda previstas

Depois da Fase 3B.3, restam três mudanças moderadas:

| Fase | Entrega principal |
|---|---|
| 3C | registrar execuções independentes com base, parâmetros e saídas disponíveis |
| 3D | fazer a Comunicação de Resultados consumir as execuções e escolher o que entra no Word |
| 3E | integrar bases, receitas, análises e resultados no Projeto R e na exportação final |

Essa divisão pode receber pequenos ajustes depois dos testes, mas não deve ser
fundida em uma mudança grande enquanto a versão dos alunos estiver congelada.

### Dependência entre branches

As branches são cumulativas:

```text
main
  └── fase 3A/3A.1
        └── fase 3B.1
              └── fase 3B.2
                    └── fase 3B.3
```

Ao fazer push somente da 3B.3, o Git também envia os objetos dos commits
ancestrais necessários, mas não cria automaticamente referências remotas com os
nomes das branches anteriores. Para escolher cada fase diretamente no Zorin,
faça push também da referência da respectiva branch.

## Criar uma pasta de testes no Zorin

Não execute estes comandos dentro da pasta estável usada pelos alunos.

```bash
mkdir -p ~/Projetos
cd ~/Projetos
git clone git@github.com:astuciasnor/catalyser.git catalyser-teste
cd catalyser-teste
git fetch origin
git branch -r
```

Se o acesso SSH do Zorin usa um alias pessoal, substitua a URL pela configurada
na máquina. No Windows, o remoto atual está como
`git@github-pessoal:astuciasnor/catalyser.git`.

Essa operação cria:

```text
~/Projetos/catalyser-estavel   # instalação/pasta usada pelos alunos; não tocar
~/Projetos/catalyser-teste     # clone destinado às branches experimentais
```

O nome da pasta estável é apenas ilustrativo; não renomeie a instalação em uso
durante o semestre.

## Abrir uma fase no clone de testes

Primeiro confirme que está na pasta certa e que não há modificações:

```bash
cd ~/Projetos/catalyser-teste
git status
git fetch origin
```

Para testar a Fase 3A/3A.1:

```bash
git switch --track origin/feature/fase-3a-registro-bases
```

Para testar a Fase 3B.1:

```bash
git switch --track origin/feature/fase-3b-transformacoes-ramos
```

Para testar a Fase 3B.2:

```bash
git switch --track origin/feature/fase-3b2-seletor-base-piloto
```

Para testar a Fase 3B.3:

```bash
git switch --track origin/feature/fase-3b3-seletores-prioritarios
```

Se a branch local já existir, use somente:

```bash
git switch feature/fase-3b3-seletores-prioritarios
git pull --ff-only
```

`git fetch` apenas baixa referências e objetos. `git switch` modifica somente a
pasta `catalyser-teste`. Nenhum desses comandos instala o pacote ou altera a
pasta estável.

## Executar sem reinstalar a versão dos alunos

Com as dependências já existentes no Zorin, execute a aplicação diretamente do
clone de testes:

```bash
cd ~/Projetos/catalyser-teste
R -q -e "shiny::runApp('inst/app')"
```

Isso abre o código da branch atual sem reinstalar a CatalyseR usada pelos
alunos. Se alguma dependência estiver ausente, pare o teste e registre o pacote
faltante; não atualize a biblioteca congelada durante o semestre.

## Encerramento futuro

Depois de testar todas as fases:

1. registrar quais branches foram aprovadas;
2. corrigir cada fase em sua própria branch, quando necessário;
3. formar uma branch de integração a partir das fases aprovadas;
4. executar o teste completo no Zorin;
5. somente depois decidir a atualização da `main` e da versão instalada.
