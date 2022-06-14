---
id: version-1.9.2-ui-setup
title: Installing the DLC Wallet UI
original_id: ui-setup
---

# Easy setup

We have desktop installers for mac, windows and linux. This requires you to be signed into a github account to download.

The link to the installers is [suredbits.com/bitcoin-s](https://suredbits.com/bitcoin-s)

You can download the appropriate installer for your machine 

![Example banner](/img/installers.png)

The downside of this setup is it uses an old UI that we are working to get rid of.
This UI is missing new features that drastically improve UX. 

Once you download and install the wallet, deposit 100,000 sats and [find an event you want to bet on](https://oracle.suredbits.com/)!

The wallet will take roughly 20-30 minutes to synchronize with the bitcoin network. If you deposit funds before
the synchronization finishes, the funds may not show up right away. This is expected. They will show up when the sync is done.

For more information about building a DLC with this UI, checkout this youtube tutorial I recorded: 

https://youtu.be/oR0I0aHxNMM?t=219

# Advanced setup

This requires command line skills.

### Starting the backend

From the image above, download the `bitcoin-s-server` artifact.

![Example banner](/img/installers.png)

After unzipping, you will need to run `chmod +x ./bin/bitcoin-s-server` from the terminal make the file executable.

After making the file executable, you can start the server with 

```
unzip bitcoin-s-server.zip
cd bitcoin-s-server
chmod +x ./bin/bitcoin-s-server
./bin/bitcoin-s-server
```

This starts the backend and will begin synchronizing with the bitcoin network.


### Building and starting the frontend

Here are the instructions to build, you can paste this into your terminal

```
git clone https://github.com/bitcoin-s/bitcoin-s-ts.git
cd bitcoin-s-ts
cd wallet-server-ui && npm i && npm run build
cd ../wallet-server-ui-proxy && npm i && npm run startlocal
```

The last command, `npm run startlocal` should result in server starting with logs that look like this

```
> wallet-server-ui-proxy@1.9.0 startlocal
> DEFAULT_UI_PASSWORD=none BITCOIN_S_SERVER_RPC_PASSWORD=password npx ts-node server.ts

...

ConfigureServerURL() http://localhost:9999/
ConfigureAuthorizationHeader() Basic Yml0Y29pbnM6cGFzc3dvcmQ=
[HPM] Proxy created: /  -> http://localhost:9999/
[HPM] Proxy rewrite rule created: "^/api/v0" ~> ""
[HPM] Proxy created: /  -> ws://localhost:19999/events
[HPM] Proxy rewrite rule created: "^/websocket" ~> ""
2022-03-11T17:05:19.238Z info: starting HTTP server
2022-03-11T17:05:19.242Z info: Web Server started on port: 3002 ⚡
```

Now if you navigate to your web browser, you should see a page like this at `http://localhost:3002` 

![Alt text](/img/Screenshot%20from%202022-03-11%2011-20-17.png)

The password is `none`, enter that and you should see the wallet!

![Alt text](/img/Screenshot%20from%202022-03-11%2011-21-47.png)


Deposit 100,000 sats and [find an event you want to bet on](https://oracle.suredbits.com/)!

The wallet will take roughly 20-30 minutes to synchronize with the bitcoin network. If you deposit funds before
the synchronization finishes, the funds may not show up right away. This is expected. They will show up when the sync is done.

After the synchronization is done, you should be good to do a DLC! :tada:
