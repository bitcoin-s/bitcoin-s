#!/bin/bash

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
