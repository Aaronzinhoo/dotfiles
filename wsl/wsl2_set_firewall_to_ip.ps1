Import-Module -Name 'NetSecurity'

$wsl2Ip = wsl.exe /bin/bash -c "ip addr show eth0 | grep -oP '(?<=inet\s)\d+(\.\d+){3}'"

Get-NetFirewallRule -Direction Inbound | Where-Object {
    $_.DisplayName -eq "vcxsrv.exe" `
        -and ($_ | Get-NetFirewallPortFilter).Protocol -eq "TCP"
} | ForEach-Object {
    Set-NetFirewallRule -Name $_.Name -RemoteAddress $wsl2Ip
}