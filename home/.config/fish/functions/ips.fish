function ips
  if set ip_external (dig +short +time=1 myip.opendns.com @resolver1.opendns.com ^/dev/null)
    printf "$ip_external\n"
  end

  if command -s ip >/dev/null ^&1
    printf "%s\n" (ip route get 1 | head -1 | cut -d' ' -f7)
  else if command -s ipconfig >/dev/null ^&1
    printf "%s\n" (ipconfig getifaddr en0 )
  end
end
