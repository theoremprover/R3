@echo off

SET WORKDIR=%~dp0
cd %WORKDIR%

where plink.exe > tmpFile
set /p GIT_SSH= < tmpFile
del tmpFile

git pull --no-edit origin master
if errorlevel 1 goto :err

stack build :analyzer-exe
if errorlevel 1 goto :err

stack exec

exit /B 0

:err
pause
exit /B
