#!/bin/bash

cd $(dirname "$BASH_SOURCE")

#cd /~/Desktop/standfire-master/apps/STANDFIRE_v1.1

# I thought _tkinter was necessary, but it appears not
#   --hidden-import="_tkinter", \

pyinstaller standfire_mini_interface.py -y \
    --hidden-import="pandas._libs.skiplist" \
    --hidden-import="pandas._libs.tslibs.np_datetime" \
    --hidden-import="pandas._libs.tslibs.nattype" \
    -i "sf_icon_64.ico"

cp bin data pyfvs dist/standfire_mini_interface/ -r
