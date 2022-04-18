---
id: version-1.9.1-web-ui
title: Application server and Web UI
original_id: web-ui
---

## Application server and Web UI

### docker-compose 

To start the application server along with the Web UI you can use `docker-compose`.

We provide a `docker-compose.yml` (https://github.com/bitcoin-s/bitcoin-s/blob/master/docker-compose.yml) file that uses the latest stable versions of the application server and the Web UI.

The application server requires a password to be set in order to start. You can use the `APP_PASSWORD` environment variable to do so.

```shell
$ APP_PASSWORD=<your password> docker-compose up
```

The server is configured to work as a neutrino wallet, and it uses `neutrino.suredbits.com` as a peer. 
If you want to connect to another peer change the `BITCOIN_S_NODE_PEERS` variable in the `docker-conpose.yml` file. 