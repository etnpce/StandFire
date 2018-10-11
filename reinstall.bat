cd "%~dp0"
pyinstaller standfire_mini_interface.py -y ^
    --hidden-import="pandas._libs.skiplist" ^
    --hidden-import="pandas._libs.tslibs.np_datetime" ^
    --hidden-import="pandas._libs.tslibs.nattype" ^
    -i "sf_icon_64.ico"
for %f in (bin, data, pyfvs) do xcopy %f dist/standfire_mini_interface/ /E /I
