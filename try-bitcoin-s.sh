#!/bin/sh

# this script is cribbed from Shapeless
# https://github.com/milessabin/shapeless/blob/master/scripts/try-shapeless.sh

COURSIER_URL=https://git.io/coursier-cli
# check if coursier exists
# TODO: check version? only new-ish works with latest.version
test -e ~/.coursier/coursier || \
  # if not, download latest coursier-cli
  (mkdir -p ~/.coursier && curl -L -s --output ~/.coursier/coursier $COURSIER_URL && chmod +x ~/.coursier/coursier)

MSG="Welcome the Bitcoin-S REPL, powered by Ammonite
Check out our documentation and examples at
https://bitcoin-s.org/docs/getting-started"
IMPORT_STATEMENT="import org.bitcoins.core._; import currency._, \
protocol.blockchain._, number._, crypto._, hd._, protocol.script._, \
protocol.ln, wallet.builder._, protocol.transaction._, \
script._, config._, protocol._ " 
# launch Ammonite with bitcoin-s on the classpath
~/.coursier/coursier launch -q -P \
  com.lihaoyi:ammonite_2.12.10:1.9.2 \
  org.bitcoin-s:bitcoin-s-core_2.12:latest.release \
  -M ammonite.Main -- --banner "$MSG" \
  --predef-code "$IMPORT_STATEMENT" < /dev/tty
