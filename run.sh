#!/usr/bin/env bash

WORK_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
export LD_LIBRARY_PATH="$(WORK_DIR)/lib/"

cd $WORK_DIR
./ball-z.bin ball-z.conf
