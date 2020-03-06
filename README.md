# wl-bot

This is a program used to link groups between Telegram and QQ, written in Haskell.

## How does it work

The program uses the [coolq-http-api](https://github.com/richardchien/coolq-http-api) to communicate with Coolq, and get updates from Telegram through _Webhook_.

## What can it do

- Forward messages

## How to use this bot

1. Download the pre-build binary file from [latest release](https://github.com/Nutr1t07/wl-bot/releases/latest).

2. Create `config.json` under the same directory as `wl-bot-exe`:

   ```json
   {
      "tgbotToken":"",
      "cqServer":"",
      "port":,
      "forwardOn":true
      "groups":[
         [QQGroupIDHere, TGGroupIDHere]
      ]
      "searchOn":false
   }
   ```

| Key        | Description                | Example Value            |   |   |
|------------|----------------------------|--------------------------|---|---|
| admins     |                            |                          |   |   |
| port       |                            | 8443                     |   |   |
| thisServer |                            |                          |   |   |
| cqServer   | CoolQ                      | "http://localhost:5700"  |   |   |
| tgbotToken | Token for Telegram Bot API |                          |   |   |
| forwardOn  |                            | true                     |   |   |
| groups     |                            | [[124234231, -12514514]] |   |   |
| searchOn   |                            | false                    |   |   |


3. Enable Coolq **Http API** plugin, set the plugin:

   ```json
   "post_message_format": "array"
   "post_url": "http://yoursever:port/cq/"
   ```

4. Run `wl-bot-exe`:

   ```bash
   ./wl-bot-exe
   ```
