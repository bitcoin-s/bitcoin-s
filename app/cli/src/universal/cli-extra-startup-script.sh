chmod +x jre/bin/java #make sure java is executable
if [[ "$OSTYPE" == "darwin21" ]]; then
  #mac doesn't allow random binaries to be executable
  #remove the quarantine attribute so java is executable on mac
  xattr -d com.apple.quarantine jre/bin/java
fi