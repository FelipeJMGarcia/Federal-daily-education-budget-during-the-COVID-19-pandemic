# Pacote curl (normalmente já vem instalado). Se não tiver:
# install.packages("curl")

setwd("D:\\daily_budget_educacao")

data_inicio <- as.Date("2025-11-10")
data_fim    <- as.Date("2025-12-31")   

pasta <- "dados_despesas"

pausa_normal   <- 60     # 60s entre um arquivo e outro
pausa_conexao  <- 120    # esperar no primeiro erro (2 min)
pausa_bloqueio <- 1800   # esperar se for bloqueio do portal (30 min)
max_tentativas <- 4      # quantas vezes tentar baixar o mesmo dia

# 2. PREPARAÇÃO

dir.create(pasta, showWarnings = FALSE)
arquivo_log    <- file.path(pasta, "log_downloads.txt")
arquivo_falhas <- file.path(pasta, "dias_com_falha.txt")

zip_valido <- function(arquivo) {
  teste <- try(unzip(arquivo, list = TRUE), silent = TRUE)
  !inherits(teste, "try-error")
}

datas   <- seq(data_inicio, data_fim, by = "day")
codigos <- format(datas, "%Y%m%d")  

if (is.null(curl::nslookup("portaldatransparencia.gov.br", error = FALSE))) {
  stop("O computador não encontra o portal (DNS). Verifique a internet antes de rodar.")
}

# 3. LOOP DE DOWNLOAD

# O "status" é o código que o servidor devolve:
#   200 = baixou normalmente
#   404 = o arquivo não existe (dia sem dados)
#   403 ou 429 = bloqueio do portal
#   0 = erro de conexão ou DNS (do seu lado)

for (codigo in codigos) {
  
  arquivo <- file.path(pasta, paste0(codigo, ".zip"))
  
  # Já baixado antes? Pula.
  if (file.exists(arquivo)) next
  
  url     <- paste0("https://portaldatransparencia.gov.br/download-de-dados/despesas/", codigo)
  parcial <- paste0(arquivo, ".parcial")   
  
  tentativa <- 1
  terminou  <- FALSE
  
  while (!terminou) {
    
    cat(codigo, "- tentativa", tentativa, "\n")
    
    # Baixa e guarda a resposta do servidor
    resposta <- try(curl::curl_fetch_disk(url, parcial))
    
    if (inherits(resposta, "try-error")) {
      status <- 0
    } else {
      status <- resposta$status_code
    }
    cat("  status:", status, "\n")
    
    if (status == 200 && zip_valido(parcial)) {
      file.rename(parcial, arquivo)
      cat("  OK\n")
      cat(codigo, format(Sys.time()), "\n", file = arquivo_log, append = TRUE)
      terminou <- TRUE
      
    } else if (status == 404) {
      cat("  Dia sem arquivo no site\n")
      terminou <- TRUE
      
    } else if (tentativa >= max_tentativas) {
      cat("  Desisti deste dia por enquanto\n")
      cat(codigo, "\n", file = arquivo_falhas, append = TRUE)
      terminou <- TRUE
      
    } else if (status == 0) {
      cat("  Erro de conexão/DNS. Esperando", pausa_conexao / 60, "minutos...\n")
      Sys.sleep(pausa_conexao)
      tentativa <- tentativa + 1
      
    } else {
      cat("  Provável bloqueio. Esperando", pausa_bloqueio / 60, "minutos...\n")
      Sys.sleep(pausa_bloqueio)
      tentativa <- tentativa + 1
    }
  }
  
  if (file.exists(parcial)) file.remove(parcial)   # apaga sobras de tentativas
  Sys.sleep(pausa_normal)
}

# 4. RESUMO

cat("\nFim. Zips na pasta:", length(list.files(pasta, pattern = "\\.zip$")), "\n")
cat("Dias que falharam estão em:", arquivo_falhas, "\n")
cat("Para tentar de novo, rode o script outra vez: os dias já baixados são pulados.\n")