#!/bin/env bash

# -a="" : start the emacs daemon if it is not started yet
# -c : create a new frame; no need to go to the original frame of Emacs
emacsclient -c -a="" $*
