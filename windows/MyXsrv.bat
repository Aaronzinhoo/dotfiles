@ECHO OFF

REM Start WSL once to create WSL netowrk interface
wsl exit

REM Find IP for WSL network interface
SET WSL_IF_IP=
CALL :GetIp "vEthernet (WSL)" WSL_IF_IP
setx "WSL_IF_IP" "%WSL_IF_IP%"
setx "WSLENV" "WSL_IF_IP/u"

REM Change E:\VcXsrv to your VcXsrv installation folder
START /D "C:\Program Files\VcXsrv" /B vcxsrv.exe -clipboard -multiwindow -nowgl -ac
GOTO :EOF



:GetIp ( aInterface , aIp )
(
    SETLOCAL EnableExtensions EnableDelayedExpansion
    FOR /f "tokens=3 delims=: " %%i IN ('netsh interface ip show address "%~1" ^| findstr IP') DO (
        SET RET=%%i
    )
)
(
    ENDLOCAL
    SET "%~2=%RET%"
    EXIT /B
)
