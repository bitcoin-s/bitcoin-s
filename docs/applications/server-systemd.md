---
id: server-systemd
title: Systemd installation
---

### Installation

#### Linux

For server side installation you'll need to install a Java Virtual Machine first.

```bash
sudo apt install openjdk-11-jdk-headless
```

Then build a Bitcoin-S server as described above and copy it into `/usr/local`:

```bash
sudo cp -r app/server/target/universal/stage /usr/local/bitcoin-s
sudo chmod +x /usr/local/bitcoin-s/bin/bitcoin-s-server
```

The server process will run in the background and use a separate user for security reasons.
This user does not have admin rights and cannot change the system configuration.

```bash
sudo adduser bitcoins
```

In this case you'll need to put the config file into `/home/bitcoins/.bitcoin-s/bitcoin-s.conf`.

To start the server as a daemon on system startup we'll need to configure a `systemd` service.
Create `bitcoin-s.service` file using your favorite text editor.

```bash
sudo nano /etc/systemd/system/bitcoin-s.service
```

Then copy this script into the editor, then save end exit.


```bash
[Unit]
Description=Bitcoin-S Node
After=network.target

[Service]
ExecStart=/usr/local/bitcoin-s/bin/bitcoin-s-server

User=bitcoins
Group=bitcoins

Type=simple
Restart=always
RestartSec=60

PrivateTmp=true
ProtectSystem=full
NoNewPrivileges=true
PrivateDevices=true

[Install]
WantedBy=multi-user.target
```

Enable the service:

```bash
sudo systemctl enable bitcoin-s.service
```

Start the server.

```bash
sudo systemctl start bitcoin-s.service
```

The server will write all logs into `/var/log/syslog`.