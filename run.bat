@echo off
set WORK_DIR=%~dp0
set PATH=%WORK_DIR%lib\;%PATH%

cd %WORK_DIR%
ball-z.bin ball-z.conf
