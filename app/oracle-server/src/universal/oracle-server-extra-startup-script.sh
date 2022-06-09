
if [[ "$OS" == "OSX" ]]; then
  #mac doesn't allow random binaries to be executable
  #remove the quarantine attribute so java is executable on mac
  xattr -d com.apple.quarantine jre/bin/java
fi

chmod +x jre/bin/java #make sure java is executable


if [[ "$OS" == "OSX" ]]; then
  #mac doesn't allow random binaries to be executable
  #remove the quarantine attribute so java is executable on mac
  xattr -d com.apple.quarantine jre/bin/java
fi
chmod +x jre/bin/java #make sure java is executable

chip=$(uname -m)

if [[ $chip == "arm64" || $chip == "aarch64" ]]; then
  # This is needed as a hack for now
  # This replaces overrides how sbt native packager finds java
  # on users machines when the user is running arm64.
  # This is needed because we don't have access to a CI environment with arm64
  # and the jlink jre we ship is broken because its built against x86
  # this simply copies the 'get_java` bash function and removes the first
  # check to see if there is a bundled_jvm built by jlink if the detected
  # computer arch is arm64 or aarch64. This means a user if they are running
  # arm64 / aarch64 will need to provide their own jre
  # see https://github.com/bitcoin-s/bitcoin-s/issues/4369!
  echo "Removing ARM jre as its not linked correctly, see https://github.com/bitcoin-s/bitcoin-s/issues/4369!"
  get_java_no_jlink() {
    if [[ -n "$JAVA_HOME" ]] && [[ -x "$JAVA_HOME/bin/java" ]];  then
      echo "$JAVA_HOME/bin/java"
    else
      echo "java"
    fi
  }

  # java_cmd is overrode in process_args when -java-home is used
  declare java_cmd=$(get_java_no_jlink)

  # if configuration files exist, prepend their contents to $@ so it can be processed by this runner
  [[ -f "$script_conf_file" ]] && set -- $(loadConfigFile "$script_conf_file") "$@"

  run "$@"
fi
