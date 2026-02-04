#!/bin/bash

if [[ -z "$DISABLE_JLINK" ]]; then
  if test -f "jre/bin/java"; then
    if [[ "$OS" == "OSX" ]]; then
      #mac doesn't allow random binaries to be executable
      #remove the quarantine attribute so java is executable on mac
      xattr -d com.apple.quarantine jre/bin/java
    fi
    chmod +x jre/bin/java #make sure java is executable
  fi


  if test -f "../jre/bin/java" ; then
    if [[ "$OS" == "OSX" ]]; then
      #mac doesn't allow random binaries to be executable
      #remove the quarantine attribute so java is executable on mac
      xattr -d com.apple.quarantine ../jre/bin/java
    fi
    chmod +x ../jre/bin/java #make sure java is executable
  fi
fi

get_java_no_jlink() {
  if [[ -n "$JAVA_HOME" ]] && [[ -x "$JAVA_HOME/bin/java" ]];  then
    echo "$JAVA_HOME/bin/java"
  else
    echo "java"
  fi
}

if [[ -n "$DISABLE_JLINK" ]]; then
  echo "jlink disabled by the DISABLE_JLINK environment variable, defaulting to JAVA_HOME for java"
  # java_cmd is overrode in process_args when -java-home is used
  declare java_cmd=$(get_java_no_jlink)

  # if configuration files exist, prepend their contents to $@ so it can be processed by this runner
  [[ -f "$script_conf_file" ]] && set -- $(loadConfigFile "$script_conf_file") "$@"

  run "$@"
fi

