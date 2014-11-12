#!/usr/bin/env bash

if [ "`uname`" == "Linux" ]; then
    # -a="" : start the emacs daemon if it is not started yet
    # -c : create a new frame; no need to go to the original frame of Emacs
    emacsclient -c -a="" "$1"
    exit $?
else
    emacs "$1"
    exit $?
fi
