# CatalyseR V15 corrigida — instalação e teste completo na VM

## 1. Arquivos a copiar para a VM

Copie para a VM Windows 10:

- `catalyser-refinamentos-qmd-20260726-v15.zip` — aplicação completa;
- `ROTEIRO_VM_V15_CORRIGIDA_20260727.md` — este roteiro;
- `catalyser_0.1.3-v15.tar.gz` — opcional, somente para testar a instalação
  como pacote R.

O arquivo `.tar.gz` não deve ser descompactado. Para testar a aplicação pelo
código-fonte, basta o arquivo `.zip`.

## 2. Criar uma pasta nova e descompactar

1. Feche qualquer CatalyseR anterior e encerre as sessões antigas do R.
2. Crie no Explorador:
   `C:\Users\odlav\Documents\catalyser-v15-corrigida-20260727`.
3. Copie o ZIP para essa pasta.
4. Clique com o botão direito no ZIP e escolha **Extrair Tudo**.
5. Ao terminar, localize a pasta `catalyser`.

O caminho recomendado é:

```text
C:\Users\odlav\Documents\catalyser-v15-corrigida-20260727\catalyser
```

Não sobreponha a V15 antiga. Uma pasta nova evita misturar arquivos de versões
diferentes e permite comparar os comportamentos.

## 3. Conferir e abrir no R

Abra o RStudio ou o R 4.6.1 e execute:

```r
setwd("C:/Users/odlav/Documents/catalyser-v15-corrigida-20260727/catalyser")
getwd()
file.exists("inst/app/app.R")
```

O último comando deve retornar `TRUE`. Depois execute:

```r
shiny::runApp("inst/app", launch.browser = TRUE)
```

Se o R informar que falta algum pacote, instale somente o pacote indicado e
execute `shiny::runApp()` novamente. Na VM que já executou a V15 anterior, a
instalação das dependências normalmente não precisa ser repetida.

## 4. Confirmar as correções antes do percurso completo

### 4.1 Execução no primeiro clique

1. Abra **Testes Paramétricos > Teste t de Student**.
2. Mantenha **Uma Amostra** e uma variável numérica.
3. Clique apenas uma vez em **Executar análise**.

Resultado esperado: aparece **Prévia executada e pronta para ser registrada**,
o botão muda para **Executar novamente** e a tabela de resultados é liberada.

### 4.2 Seleção das etapas da receita

Esse teste será concluído depois de criar a primeira Base Derivada. Quando ela
tiver duas ou mais etapas, o campo **Etapa selecionada** deverá permitir
escolher qualquer uma delas. Os botões **Subir**, **Descer**,
**Ativar/Desativar** e **Remover** devem atuar sobre a etapa escolhida.

### 4.3 Novo estúdio pedagógico de preparo

Antes das duas análises, execute o roteiro:

```text
docs\testes\fases-3\ROTEIRO_TESTE_PREPARO_PEDAGOGICO_V15.md
```

Ele valida as três sub-abas de **Pivotar e Separar Dados**, as duas sub-abas de
**Organizar Variáveis**, os seis botões de adição à trilha e a sub-aba
**Adicionar Tratamentos à Base > Checagem Final da Base Compartilhada** em
duas colunas 20/80.

## 5. Dados e Base Compartilhada

Use:

```text
inst\app\dados\Treino-Transformacoes.xlsx
```

Selecione a aba `biometria`.

Em **Preparando Dados**:

1. carregue a planilha;
2. use **Organizar Variáveis > Arrumação de Variáveis** para conferir nomes,
   tipos e categorias;
3. use **Adicionar Tratamentos à Base** para padronizar o texto de `especie`,
   `local` e `sexo`;
4. recodifique `sexo`, se necessário, para apenas `F` e `M`;
5. remova duplicatas;
6. confira o resultado em **Adicionar Tratamentos à Base > Checagem Final da
   Base Compartilhada**.

Não remova globalmente todos os NA. Cada Base Derivada tratará somente as
colunas necessárias à sua análise.

## 6. Base Derivada 1 e teste t

Em **Preparando Dados > Bases Derivadas**:

1. crie a base `CPUE de corvina por sexo`;
2. use o objeto R `base_t_cpue_corvina_sexo`;
3. na sub-aba **Receita da base**, adicione:
   - filtro: `especie` igual a `corvina`;
   - dados faltantes: remover NA de `cpue`;
   - dados faltantes: remover NA de `sexo`;
4. em **Etapa selecionada**, escolha as etapas 1, 2 e 3;
5. teste **Subir** ou **Descer** e devolva a receita à ordem acima;
6. clique em **Recalcular a Base** na própria sub-aba da receita;
7. volte a **Criação e gestão da base** e clique em **Finalizar preparo**.

Em **Testes Paramétricos > Teste t de Student**:

1. selecione a base `CPUE de corvina por sexo`;
2. escolha **Duas Amostras Independentes**;
3. resposta: `cpue`;
4. grupo: `sexo`;
5. clique uma vez em **Executar análise**;
6. abra **2. Adicionar aos resultados** e registre a execução.

Se houver mais de duas categorias em `sexo`, a mensagem deve listar as
categorias encontradas e orientar a padronização/recodificação.

## 7. Base Derivada 2 e regressão linear

Em **Bases Derivadas**:

1. crie `Peso e comprimento de corvina`;
2. use o objeto R `base_reg_peso_comprimento_corvina`;
3. adicione à receita:
   - filtro: `especie` igual a `corvina`;
   - remover NA de `comprimento_cm`;
   - remover NA de `peso_g`;
4. recalcule e finalize o preparo.

Em **Modelos de Regressão > Regressão Linear Simples**:

1. selecione `Peso e comprimento de corvina`;
2. resposta: `peso_g`;
3. preditor: `comprimento_cm`;
4. clique uma vez em **Executar análise**;
5. adicione a execução aos resultados.

## 8. Comunicação de Resultados

Abra **Comunicação de Resultados** e percorra as quatro sub-abas:

1. **Esboço do documento** — confira as duas análises na seção Resultados;
2. **Execuções registradas** — confirme que teste t e regressão estão
   separados e incluídos no Word;
3. **Bases do projeto** — confirme a Base Compartilhada e as duas Bases
   Derivadas;
4. **Saída planejada** — baixe:
   - Relatório Word;
   - Projeto R.

## 9. Testar o Projeto R fora da CatalyseR

1. Copie o ZIP do Projeto R para o Windows 11.
2. Descompacte-o em uma pasta nova.
3. Abra o arquivo `.Rproj` no RStudio.
4. Execute os scripts numerados na ordem.
5. Abra `relatorio.qmd`.
6. Confirme que o QMD contém o código essencial com `stats::t.test(...)` e
   `stats::lm(...)`.
7. Renderize o QMD para Word.

Resultado esperado: os scripts reproduzem as duas análises e o relatório Word
é criado. Os chunks pedagógicos permanecem legíveis no arquivo QMD, mas não
são impressos no Word.

## 10. Teste opcional do pacote R

Sem descompactar o `.tar.gz`, execute:

```r
install.packages(
  "C:/caminho/catalyser_0.1.3-v15.tar.gz",
  repos = NULL,
  type = "source"
)
library(catalyser)
packageVersion("catalyser")
catalyser::run_app()
```

A versão esperada é `0.1.3`.

## 11. Critérios para encerrar a V15

- a análise executa no primeiro clique;
- qualquer erro de configuração informa o campo ou condição problemática;
- qualquer etapa da receita pode ser selecionada, reordenada, desativada ou
  removida;
- o recálculo funciona dentro de **Receita da base**;
- as duas bases e análises chegam separadas à Comunicação;
- o Projeto R executa fora da CatalyseR;
- o QMD e o Word são gerados com os conteúdos esperados.

Se todos os itens forem aprovados, a V15 pode ser encerrada e versionada antes
do início dos refinamentos da ANOVA na V16.
