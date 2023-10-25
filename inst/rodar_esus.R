pkgload::load_all()

url <- "https://github.com/eduardompeixoto/atualiza_esus_notifica/raw/main/inst/planilha_esus.RData"
load(url(url))

df_esus <- esus()

save(df_esus, file="inst/nowcast.RData")

commit_message <- paste0("", Sys.time())

writeLines(commit_message, "mensagem-comit.txt")
