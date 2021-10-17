---
id: version-1.8.0-tor
title: Setting up Tor with Light Client
original_id: tor
---

Bitcoin-s node can connect to the p2p network over Tor.

Before setting up Bitcoin-s node to use Tor you must have Tor installed and running.

To install Tor use this command on Debian based Linux systems 

```shell
sudo apt install tor
```

or this command to install it on Mac OS X

```shell
brew install tor
```

You don't need a special configuration for Tor to be a SOCKS5 proxy for a Bitcoin-s node.
However, you might want to uncomment this line in your `/etc/tor/torrc` (Linux) or 
`/usr/local/etc/tor/torrc` (Mac OS X) file to prevent your Tor node from using your computer 
as an exit point to the clearnet:

```
ExitPolicy reject *:* # no exits allowed
``` 

Start Tor on Linux machines:

```shell
sudo systemctl start tor
```

or Mac OS X:

```shell
brew services start tor
```

Next you need to enable SOCKS5 proxy support in your `~/.bitcoin-s/bitcoin-s.conf` file:

```
bitcoin-s.node.proxy.enabled = true
```

See https://github.com/bitcoin-s/bitcoin-s/blob/master/db-commons/src/main/resources/reference.conf for other proxy configuration parameters.