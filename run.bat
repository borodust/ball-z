@echo off
set WORK_DIR=%~dp0
set PATH=%WORK_DIR%lib\;%PATH%

start /d "%WORK_DIR%" ball-z.bin ball-z.conf
