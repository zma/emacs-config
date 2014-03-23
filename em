#!/bin/env bash

# -a="" : start the emacs daemon if it is not started yet
emacsclient -a="" $*
