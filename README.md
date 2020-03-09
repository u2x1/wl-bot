# wl-bot

This is a program used to provide tools for QQ and Telegram groups, written in Haskell.

## How it works

The program uses the [coolq-http-api](https://github.com/richardchien/coolq-http-api) to communicate with Coolq, and get updates from Telegram through _Webhook_.

## What it can do

- Forward messages
- Search entries from Baidu Baike

## How to use it

1. Download the pre-build binary file from [latest release](https://github.com/Nutr1t07/wl-bot/releases/latest).

2. Create `config.json` under the same directory as `wl-bot-exe`:

   ```json
   {
      "admins":[],
      "tgbotToken":"",
      "cqServer":"",
      "thisServer":"",
      "port":8443,
      "forwardOn":false,
      "groups":[
         []
      ],
      "searchOn":true
   }
   ```

| Key        | Description                          | Example Value            |
|------------|--------------------------------------|--------------------------|
| admins     | (WIP)                                |                          |
| port       | The port this program listen         | 8443                     |
| thisServer | This server address used for Webhook | "https://yourserver"     |
| cqServer   | CoolQ server address for API calling | "http://localhost:5700"  |
| tgbotToken | Token for Telegram Bot API           |                          |
| forwardOn  | Forward between groups if true       | true                     |
| groups     | Groups need to be linked             | [[124234231, -12514514]] |
| searchOn   | Search Baike using "/qr " if true    | false                    |


3. Enable Coolq **Http API** plugin, set the config of the plugin:

   ```json
   "post_message_format": "array"
   "post_url": "http://yoursever:port/cq/"
   ```

4. Run `wl-bot-exe`:

   ```bash
   nohup ./wl-bot-exe &
   ```
