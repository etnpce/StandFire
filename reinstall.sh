#!/bin/bash

cd $(dirname "$BASH_SOURCE")
#cd /~/Desktop/standfire-master/apps/STANDFIRE_v1.1
echo y | pyinstaller standfire_mini_interface.spec
cp bin data pyfvs dist/standfire_mini_interface/ -r
echo
