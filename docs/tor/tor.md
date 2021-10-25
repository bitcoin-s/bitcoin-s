---
id: tor
title: Tor Setup
---

It is possible to run Bitcoin-S through tor.
[Tor](https://www.torproject.org/) is an onion routed private network that allows us to send and receive messages in an
anonymous manner. Using tor in conjunction with Bitcoin-S allows you to be more private when syncing the blockchain, as
well as allows for sending and receiving DLC messages without the need for a static IP address or opening/forwarding of
ports.

## Installing Tor

### Debian

You can install tor using `sudo apt install tor` if on a debian system.

After installing you can start it with `sudo systemctl start tor`

### Brew

You can install tor using `brew install tor` if on a mac osx system.

After installing you can start it with `brew services start tor`

### Other

Otherwise, you can install the Tor Browser from [here](https://www.torproject.org/download/).

## Starting Tor

To connect to onion addresses you need to enable the tor proxy. To do so you need to have tor currently running, this
can be checked by using the command `sudo systemctl status tor`. This should give you an output similar to:

```bash
$ sudo systemctl status tor
● tor.service - Anonymizing overlay network for TCP (multi-instance-master)
     Loaded: loaded (/lib/systemd/system/tor.service; enabled; vendor preset: enabled)
     Active: active (exited) since Wed 2021-07-28 13:06:42 CDT; 48min ago
   Main PID: 804 (code=exited, status=0/SUCCESS)
      Tasks: 0 (limit: 18696)
     Memory: 0B
     CGroup: /system.slice/tor.service
```

If the output says `Active: active`, then it is running and good to go.

On mac osx you can use the command `brew services list` to ensure tor is running. This should give you an output similar to: 

```bash
$ brew services list
Name    Status  User        Plist            
tor     started $username /Users/username/Library/LaunchAgents/homebrew.mxcl.tor.plist 
```

If tor satus is `started`, then it is running and good to go. 

## Enabling the Tor proxy

Enabling the tor proxy allows you to create outgoing connections over tor. This is needed if you want to sync the
blockchain over tor, or to accept DLCs over tor.

To enable the tor proxy you simply need to set a couple config options after you have tor running.

You need to enable the proxy and set the host and port configuration options. If you are using the default settings you
should only need to set `bitcoin-s.proxy.enabled = true`.
These modifications need to be made to `$HOME/.bitcoin-s/bitcoin-s.conf` file.
Create this file if it does not exist.

```$xslt
bitcoin-s {
    proxy {
        # You can configure SOCKS5 proxy to use Tor for outgoing connections
        enabled = true
        sock5 = "127.0.0.1:9050"
    }
}
```

You can override global proxy settings in subprojects, for example `bitcoin-s.dlcnode.proxy.enabled = true` 
will enable SOCKS5 proxy for `dlcnode`.

## Creating our own Tor hidden service

Enabling the tor hidden services allows for inbound connections over tor.
This is needed if you want to create DLCs over tor.

To enable the tor hidden services you need to set a couple config options after you have tor running in your bitcoin-s
config, as well as have tor configured for it.

### Configuring Tor

You may need to set up the Tor Control Port. On Linux distributions there may be some or all of the following settings
in `/etc/tor/torrc` for linux or `/opt/homebrew/etc/tor/torrc` for mac, generally commented out by default (if not, add
them):

```
ControlPort 9051
CookieAuthentication 1
CookieAuthFileGroupReadable 1
```

Add or uncomment those, save, and restart Tor (usually `systemctl restart tor`
or `sudo systemctl restart tor` on most systemd-based systems, including recent Debian and Ubuntu, `brew services restart tor` on mac osx, or just restart the
computer).

On some systems (such as Arch Linux), you may also need to add the following line:

```
DataDirectoryGroupReadable 1
```

You may also need permissions for the auth cookie file, this can be done doing

```bash
sudo usermod -a -G debian-tor $USER
```
or on mac
```
sudo chmod 755 /usr/local/var/tor
```

After changing these settings, you will need to restart your computer.

### Optional Settings
If you experience repeated connection issues make sure to check the bitcoin-s.log file. If the logs show that Bitcoin-s is able to connect through the tor proxy (`connected to neutrino.suredbits.com/:8333 via SOCKS5 proxy /127.0.0.1:9050`) but is not able to connect through the tor controller (`TorController refused to connect` or similar) you may need to make additional changes to your torrc file. Find the location of your tor control_auth_cookie file and add the pathname for this file to your torrc file as show below. 
For mac osx:
```
CookieAuthFile /usr/local/var/tor/control_auth_cookie
```

### Configuring Bitcoin-S

You need to enable tor and set the control option, `127.0.0.1:9051` is the default. If you are using the default
settings you should only need to set `bitcoin-s.tor.enabled = true`.
These modifications need to be made to `$HOME/.bitcoin-s/bitcoin-s.conf` file.
Create this file if it does not exist.

```$xslt
bitcoin-s {
    tor {
        # You can enable Tor for incoming connections
        enabled = true
        control = "127.0.0.1:9051"

        # The password used to arrive at the HashedControlPassword for the control port.
        # If provided, the HASHEDPASSWORD authentication method will be used instead of
        # the SAFECOOKIE one.
        # password = securePassword

        # The path to the private key of the onion service being created
        # privateKeyPath = /path/to/priv/key
    }
}
```
Similarly with proxy settings you can override global Tor settings in subprojects, 
for example `bitcoin-s.dlcnode.tor.enabled = true` will enable Tor for `dlcnode`.

### Manually Creating a Tor Hidden Service

Alternatively, you can manually create a tor hidden service.

You can also manually configure your node to be reachable from the Tor network. Add these lines to
your `/etc/tor/torrc` (or equivalent config file, mac is located at `/opt/homebrew/etc/tor/torrc`):

```
HiddenServiceDir /var/lib/tor/dlc-service/
HiddenServicePort 2862 127.0.0.1:2862
```

Then to get your host address simply do this after restarting your tor daemon.

```bash
sudo cat /var/lib/tor/dlc-service/hostname
```
