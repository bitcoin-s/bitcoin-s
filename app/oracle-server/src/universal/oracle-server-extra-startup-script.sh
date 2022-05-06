if [[ "$OSTYPE" == "darwin21" ]]; then
  echo "RUNNING MAC"
  chmod +x jre/bin/java
  xattr -d com.apple.quarantine jre/bin/java
fi

