function ips
  if set ip_external (dig +short +time=1 myip.opendns.com @resolver1.opendns.com ^/dev/null)
    printf "ext\t$ip_external\n"
  end

  if command -s ip >/dev/null ^&1
    ip -o -4 address show up |\
    grep -Po "(?<=^\d:\s)([^\s]+).*inet\s*([\d\.]*)" |\
    sed -e "s/\s*inet\s*/\t/"
  else if command -s ipconfig >/dev/null ^&1
    printf "en0\t%s\n" (ipconfig getifaddr en0 )
  end
end
