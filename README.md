# wl-bot

This is a program used to link groups between Telegram and QQ, written in Haskell.

## How does it work

The program uses the [coolq-http-api](https://github.com/richardchien/coolq-http-api) to communicate with Coolq, and get updates from Telegram through _Webhook_.

## What can it do

- Forward messages

## How to use this bot

1. Download the pre-build binary file at [latest release](https://github.com/Nutr1t07/wl-bot/releases/latest).

2. Create `config.json` under the same directory as `wl-bot-exe`:
   ```
   {
      "groups":[
         [QQgroupidhere, TGgroupidhere]
      ],
      "tgbotToken":"",
      "cqServer":"",
      "port":
   }
   ```
   For example:
   ```
   {
      "groups":[
         [124234231, -12514514]
         [434235235, -14353453]
      ],
      "tgbotToken":"telegramtokenhere",
      "cqServer":"http://localhost:5700",
      "port":8443
   }
   ```
3. Enable Coolq **Http API** plugin, set the plugin:
   ```
   "post_message_format": "array"
   "post_url": "http://yoursever:port/cq/"
   ```
  
4. Set Telegram webhook:
   Open url from browser: https://api.telegram.org/botyourbottoken/setWebhook?url=https://yourserver:port/telegram/
   
5. Run `wl-bot-exe`:
   ```
   ./wl-bot-exe
   ```
