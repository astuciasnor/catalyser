Sys.setlocale("LC_ALL", "English_United States.utf8")
source("app.R", encoding="UTF-8")
# Uma confirmação salva somente uma etapa, mesmo com um segundo clique.
raiz <- reactiveVal(data.frame(peso_g=c(100,200),peso_kg=c(.1,.2)))
trilha <- reactiveVal(list())
testServer(mod_organizar_variaveis_server,args=list(data_rv=raiz,on_etapa=function(et){trilha(c(isolate(trilha()),list(et))); TRUE}),{
 session$flushReact(); session$setInputs(abrir_renomear=1)
 session$setInputs(renomear_1='peso_g',renomear_2='massa_kg',confirmar_renomear=1)
 stopifnot(tem_alteracoes(),grepl('Mudança pendente',output$resumo_acoes$html))
 session$setInputs(usar_base=1);stopifnot(!tem_alteracoes(),length(trilha())==1)
 session$setInputs(usar_base=2);stopifnot(length(trilha())==1)
 resultado<-replay_pipeline(raiz(),trilha());stopifnot(!length(resultado$erros),'massa_kg'%in%names(resultado$df))
})
cat('PASSOU: renomear mostra pendência e não duplica etapa.\n')
# O gerador remove somente escolhas que não mudam a importação.
# Usa o manifesto mínimo aceito pelo gerador de sequência completa.
i <- list(source="local",file_name="dados.xlsx",excel_sheet="biometria",
 preparo_importacao=list(colunas=c("peso_g","sexo"),colunas_originais=c("peso_g","sexo"),
 tipos=list(peso_g="numeric",sexo="character"),classes_originais=list(peso_g="numeric",sexo="character")))
codigo <- preparo_codigo_completo(i)
stopifnot(!grepl("Colunas escolhidas",codigo),!grepl("gsub",codigo),!grepl("mutate",codigo))
i$preparo_importacao$tipos$sexo <- "factor"
stopifnot(grepl("factor",preparo_codigo_completo(i)))
cat("PASSOU: código omite operações neutras e preserva conversão solicitada.\n")
