# Preparo da v1 — primeira conferência curta

Reabra a CatalyseR pelo `run.R` de `D:/Claude/EAPA-Ecossistema/catalyser` para carregar as correções. Use sua janela habitual e zoom de 100%. Esta bateria concentra-se em CSV, datas e um percurso de saída; não é necessário repetir todas as transformações de ontem.

Os três arquivos estão na pasta [arquivos_preparo_v1](arquivos_preparo_v1). São oito coletas fictícias, criadas apenas para esta conferência. Os dois CSVs representam os mesmos dados. O Excel tem a aba `coletas` e uma variante `data_invalida`, com um erro deliberado.

Quando funcionar e estiver claro, registre somente **T01 — OK**, por exemplo. Quando houver dificuldade, escreva o número, o que aconteceu e o que esperava; acrescente um print da janela inteira. Diga se o resultado ficou incorreto ou se estava correto, mas exigiu procura/ajuda. Não é preciso produzir um print para cada OK.

## T01 — CSV com ponto e vírgula

1. Em **Preparar Dados → Importar Dados**, abra [Coletas-ponto-e-virgula.csv](arquivos_preparo_v1/Coletas-ponto-e-virgula.csv).
2. Confira se os controles de separador aparecem após o envio.
3. Escolha **Ponto e Vírgula** para as colunas e **Vírgula** para o decimal. Mantenha o cabeçalho marcado.
4. Confira a tabela: **8 linhas e 5 colunas**. O primeiro `peso_g` vale **90,5**, mesmo que a tela o mostre como `90.5`. A terceira coleta não tem data.
5. A primeira observação deve conservar `rede, margem; setor #1`, e `Estação A` deve manter o acento.

**O que avaliar:** a orientação ajuda a reconhecer uma leitura incorreta e a corrigi-la? Depois do ajuste, os valores ficam claros?

Resposta: **T01 —**

## T02 — Trocar para CSV com vírgula

1. No mesmo painel, envie [Coletas-virgula.csv](arquivos_preparo_v1/Coletas-virgula.csv).
2. Escolha **Vírgula** para as colunas e **Ponto** para o decimal. As escolhas anteriores podem continuar selecionadas até você ajustá-las.
3. Espere novamente **8 linhas e 5 colunas**, com os mesmos pesos e observações do T01.

**O que avaliar:** consegue trocar o arquivo e recuperar a leitura sem se perder? A mensagem informa que o arquivo foi carregado e pede a conferência da prévia?

Resposta: **T02 —**

## T03 — Datas do Excel e datas escritas como texto

1. Envie [Conferencia-Datas.xlsx](arquivos_preparo_v1/Conferencia-Datas.xlsx). A primeira aba, **coletas**, deve ser selecionada. Espere **8 linhas e 6 colunas**.
2. Abra **Preparar Base Compartilhada → Variáveis e categorias → Definir tipos**.
3. `data_excel` deve ser reconhecida como **Data**. Escolha **Data** para `data_br` e `data_iso`. Mantenha os demais tipos.
4. Clique em **Aplicar**, confira a prévia e depois em **Adicionar etapa do preparo**.
5. As três colunas devem representar as mesmas datas: primeiro peixe em **1º de setembro de 2026**, último em **8 de setembro de 2026**. A coleta de `id_peixe = 3` continua sem data. A base permanece com **8 × 6**.

**O que avaliar:** fica claro o que está em prévia e o que foi incorporado? O botão volta ao estado sem pendências após adicionar?

Resposta: **T03 —**

## T04 — Uma data impossível

Faça este teste antes do percurso final, pois trocar a entrada reinicia o preparo.

1. Volte a **Importar Dados** e escolha a aba **data_invalida** do mesmo Excel.
2. Na base compartilhada, abra **Variáveis e categorias → Definir tipos** e tente converter `data_br` para **Data**.
3. Ao aplicar, deve aparecer um aviso identificando **31/02/2026**. A conversão não deve ser aceita nem virar uma etapa da trilha. Os valores originais dessa coluna devem permanecer preservados.
4. Cancele o diálogo. Volte à importação, escolha **coletas** e repita a conversão válida do T03.

**O que avaliar:** o aviso permite entender o problema e como corrigi-lo? Consegue continuar depois dele?

Resposta: **T04 —**

## T05 — Um percurso simples até a base derivada

Continue com a aba **coletas** e as datas já incorporadas, como ao final do T04.

1. Na compartilhada, em **Cálculos e transformações**, reescale `peso_g` para `peso_kg`, usando o prefixo **quilo (k)**, e adicione a etapa.
2. Espere **8 linhas e 7 colunas**. Para o primeiro peixe, `peso_kg = 0,0905`.
3. Em **Preparar Bases Derivadas**, crie **Pesos acima de 100 g**, com nome R `base_pesos`.
4. Em **Recortes e resumos → Filtrar linhas**, mantenha `peso_g > 100`, adicione a etapa e recalcule conforme os controles da tela.
5. Espere **5 linhas e 7 colunas**, com os peixes **2, 3, 5, 6 e 8**. A compartilhada continua com oito linhas. Finalize a derivada.

**O que avaliar:** consegue identificar a base que está preparando, quando precisa recalcular e quando o preparo está finalizado?

Resposta: **T05 —**

## T06 — Conferir Excel e R fora da IDE

1. Baixe o Excel e a sequência completa de R da compartilhada. Baixe também o Excel e o R da derivada finalizada.
2. Abra os Excel: confira **8 × 7** na compartilhada e **5 × 7** na derivada. As datas não devem aparecer como números seriais sem formatação, nem trocar dia e mês.
3. Guarde o Excel original junto dos scripts. No RStudio, abra cada script, confira o caminho de entrada indicado e execute desde o começo em uma sessão limpa. Se precisar, ajuste apenas o caminho do arquivo.
4. Confira os mesmos registros e pesos. As três colunas de datas devem continuar sendo datas; a ausência do peixe 3 permanece ausente.

**O que avaliar:** é fácil localizar os downloads e entender de qual base cada arquivo veio? Alguma instrução necessária está faltando?

Resposta: **T06 —**

## Retorno e segunda bateria

Envie seu Word com as respostas T01–T06 e os prints apenas dos problemas. Uma observação geral sobre o conforto de uso é suficiente ao final.

Depois das correções decorrentes desse retorno, a segunda bateria repetirá somente os testes afetados e o percurso T05–T06. A renderização visual final de Word/HTML é uma conferência distinta: executar o preparo dos projetos não equivale a revisar os documentos renderizados.
