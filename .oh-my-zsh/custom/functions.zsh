ips() {
  dig +short myip.opendns.com @resolver1.opendns.com

  if command -v ip &>/dev/null; then
    ip addr | grep --color=never -oP "inet \K[\d.]+"
  elif command -v ifconfig &>/dev/null; then
    ifconfig | awk "/inet /{ print $2 }"
  fi
}


