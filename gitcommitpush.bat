@echo off

SET WORKDIR=%~dp0
cd %WORKDIR%

where plink.exe > tmpFile
set /p GIT_SSH= < tmpFile
del tmpFile

git commit -a -m "No Message"
if errorlevel 2 goto :err
git pull origin master
if errorlevel 1 goto :err
git push origin master
if errorlevel 1 goto :err
echo OK
exit /B 0

:err
pause
exit /B 1
