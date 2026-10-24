@echo off
rem Double-click this file to open StatGuide R AI in your web browser (Windows).
rem Keep this window open while you use the app. Close it to stop the app.

cd /d "%~dp0"

set "RSCRIPT="
where Rscript >nul 2>nul && set "RSCRIPT=Rscript"
rem R's installer does not add Rscript to PATH, so look in the default folder.
if not defined RSCRIPT for /d %%D in ("%ProgramFiles%\R\R-*") do if exist "%%D\bin\Rscript.exe" set "RSCRIPT=%%D\bin\Rscript.exe"

if not defined RSCRIPT (
  echo R is not installed yet.
  echo Download R from https://cran.r-project.org, install it, then double-click this file again.
  pause
  exit /b 1
)

echo Starting StatGuide R AI. Your browser will open in a moment.
echo Keep this window open while you use the app. Close it to stop the app.
"%RSCRIPT%" app.R
pause
