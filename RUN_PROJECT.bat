@echo off
cd /d "%~dp0"

echo ==========================================
echo AQI Project (R) - Running
echo ==========================================

"C:\Program Files\R\R-4.5.2\bin\Rscript.exe" run_project.R

echo.
echo Done.
pause