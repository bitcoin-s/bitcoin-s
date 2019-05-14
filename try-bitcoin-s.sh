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
Check out our documentation at https://bitcoin-s.org"
# launch Ammonite with bitcoin-s on the classpath
~/.coursier/coursier launch -q -P \
  com.lihaoyi:ammonite_2.12.4:latest.release \
  org.bitcoin-s:core.12:latest.release \
-- --predef-code 'import org.bitcoins.core._' --banner $MSG < /dev/tty