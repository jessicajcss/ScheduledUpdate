### SOURCE DATA - MESSAGE
source('R/00-wrangled_data_whatsapp_bothThermo.R')



#####
channel_id <- Sys.getenv("WHATS_CHANNEL_ID")
whapi_token <- Sys.getenv("WHAPI_TOKEN")
###### MESSAGES ----


#### 1ª Mensagem = intro
observacao <- "\n 🧾 *Boletim de Qualidade do Ar* \n*(Lab-Air, UFPR)* \n"


#### 3ª Mensagem = referência
cidades_thermo <- data_thermo_instantaneo$Cidade[as.Date(lubridate::force_tz(data_thermo_instantaneo$date, tz = "America/Sao_Paulo")) == (as.Date(lubridate::force_tz(hoje, tz = "America/Sao_Paulo")))]

if ("Almirante Tamandaré" %in% cidades_thermo & "Rio Branco do Sul" %in% cidades_thermo) {
  referencia <- "\n🔎 Obs.: Valores registrados por sensor da Thermo GM-5000 em Almirante Tamandaré e Rio Branco do Sul, e sensores PurpleAir nas demais cidades. \n 🧐 Mais informações: https://rmcqualidadedoar.shinyapps.io/dados/ "

} else if ("Rio Branco do Sul" %in% cidades_thermo) {
  referencia <- "\n🔎 Obs.: Valores registrados por sensor da Thermo GM-5000 em Rio Branco do Sul e sensores PurpleAir nas demais cidades. \n 🧐 Mais informações: https://rmcqualidadedoar.shinyapps.io/dados/ "

} else if ("Almirante Tamandaré" %in% cidades_thermo) {
  referencia <- "\n🔎 Obs.: Valores registrados por sensor da Thermo GM-5000 em Almirante Tamandaré e sensores PurpleAir nas demais cidades. \n 🧐 Mais informações: https://rmcqualidadedoar.shinyapps.io/dados/ "

} else {
  referencia <- "\n🔎 Obs.: Valores registrados por sensores PurpleAir. \n 🧐 Mais informações: https://rmcqualidadedoar.shinyapps.io/dados/ "
}

#### 2ª Mensagem = boletim

out2 <- vector("list", nrow(unique(output)))

for (i in 1:nrow(output)) {
  out2[[i]]$texto <- paste0("\n📌 *", output$Cidade[i],"* \n", output$qualidade[i], "\n", output$message[i], "\n")
}

output2<- dplyr::bind_rows(out2)

texto_value <- paste(output2$texto, collapse = " ")

# changing divisor decimal de valores de '.' por ','
texto_value <- gsub("(?<=\\d)\\.(?=\\d)", ",", texto_value, perl = TRUE)

message <- paste0(observacao,
                  texto_value,
                  referencia)

print(message) #horário 14:45 ok







#############
# Load necessary library
library(httr)
library(lubridate)

# Function to send message
send_whatsapp_message <- function() {
  url <- "https://gate.whapi.cloud/messages/text"
  headers <- c(
    'accept' = 'application/json',
    'authorization' = paste0("Bearer ", whapi_token),
    'content-type' = 'application/json'
  )

  # Message body
  body <- list(
    typing_time = 0,
    to = paste0(channel_id, "@newsletter"),
    body = message
  )

  # Send the request
  response <- POST(url, add_headers(headers), body = body, encode = "json")

  # Check the response
  if (status_code(response) == 200) {
    message("Message sent successfully!")
  } else {
    message("Failed to send message. Status code: ", status_code(response))
  }
}

# Function to schedule messages
send_whatsapp_message()
