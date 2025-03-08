library(httr)

# Simulated value (replace with real data)
threshold <- 100
current_value <- 120  # Example value, replace with your dataset

# Load credentials from GitHub Secrets
bot_token <- Sys.getenv("TELEGRAM_BOT_TOKEN")
chat_id <- Sys.getenv("TELEGRAM_CHAT_ID")

# Check if threshold is exceeded
if (current_value > threshold) {
  message <- paste0("🚨 ALERT: Value exceeded threshold!\n",
                    "📊 Current Value: ", current_value,
                    "\n⚠️ Threshold: ", threshold)

  # Telegram API URL
  url <- paste0("https://api.telegram.org/bot", bot_token, "/sendMessage")

  # Send message
  response <- POST(url, body = list(chat_id = chat_id, text = message), encode = "form")

  print(content(response, "text"))  # Debugging
} else {
  print("✅ No alert needed, value is within safe limits.")
}
