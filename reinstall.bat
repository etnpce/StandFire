@SETLOCAL

cd "%~dp0"

@echo off

SET INSTNAME=pyinstaller.exe
for %%N in (%INSTNAME%) do SET INSTPATH=%%~$PATH:N
IF EXIST "%~f1\%INSTNAME%" SET INSTPATH="%~f1\%INSTNAME%"
IF EXIST "%~dp1\%INSTNAME%" SET INSTPATH="%~dp1\%INSTNAME%"

IF NOT EXIST %INSTPATH% do goto err

echo on

@REM The next line is required because pyinstaller crashes when recreating the folder directly after deleting it
@REM I have no clue why.  Maybe a weird buffered IO quirk
rmdir "dist/standfire_mini_interface" /S /Q

%INSTPATH% standfire_mini_interface.py -y ^
--hidden-import="pandas._libs.skiplist" ^
--hidden-import="pandas._libs.tslibs.np_datetime" ^
--hidden-import="pandas._libs.tslibs.nattype" ^
--hidden-import="pandas._libs.tslibs.timedeltas" ^
-i "sf_icon_64.ico"

del "dist\standfire_mini_interface\sqlite3.dll"

@for %%f in (bin, data, pyfvs) do xcopy %%f "dist/standfire_mini_interface/%%f" /S /E /I /Q

@goto :eof

:err
echo %INSTNAME% not found. Add it to the path, or pass a path to it as an argument.