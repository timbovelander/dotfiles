function update
  if command -s apt >/dev/null 2>&1
    sudo apt update
    sudo apt upgrade
  end

  if command -s zypper >/dev/null 2>&1
    sudo zypper update
  end

  if command -s brew >/dev/null 2>&1
    brew upgrade
  end

  if command -s snap >/dev/null 2>&1
    sudo snap refresh
  end

  if command -s npm >/dev/null 2>&1
    npm -g update
  end

  if command -s apm >/dev/null 2>&1
    apm upgrade -c false
  end

  if type -t fisher >/dev/null 2>&1
    fisher self-update
    fisher
  end

  if command -s git >/dev/null 2>&1
    test -d "$HOME/.dotfiles"
    and git -C "$HOME/.dotfiles" pull --rebase=preserve --prune 2>/dev/null

    test -d "$HOME/.emacs.d"
    and git -C "$HOME/.emacs.d" pull --rebase=preserve --prune 2>/dev/null

    test -d "$HOME/.nodenv"
    and git -C "$HOME/.nodenv" pull --rebase=preserve --prune 2>/dev/null

    test -d "$HOME/.nodenv/plugins/node-build"
    and git -C "$HOME/.nodenv/plugins/node-build" pull --rebase=preserve --prune 2>/dev/null

    test -d "$HOME/.nodenv/plugins/nodenv-default-packages"
    and git -C "$HOME/.nodenv/plugins/nodenv-default-packages" pull --rebase=preserve --prune 2>/dev/null

    test -d "$HOME/.rbenv"
    and git -C "$HOME/.rbenv" pull --rebase=preserve --prune 2>/dev/null

    test -d "$HOME/.rbenv/plugins/ruby-build"
    and git -C "$HOME/.rbenv/plugins/ruby-build" pull --rebase=preserve --prune 2>/dev/null

    test -d "$HOME/.rbenv/plugins/rbenv-gemset"
    and git -C "$HOME/.rbenv/plugins/rbenv-gemset" pull --rebase=preserve --prune 2>/dev/null
  end

  if test -f "/opt/firefox/firefox"
    set -l url "https://download.mozilla.org/?product=firefox-latest-ssl&os=linux64&lang=nl"
    set -l latest (curl -IS $url | grep Location)
    set -l match (echo $latest | grep (cat /opt/firefox/version))

    if test -z $match
      wget "$url" -O /tmp/firefox.tar.bz2
      tar -xjf /tmp/firefox.tar.bz2 -C /tmp
      sudo cp -R /tmp/firefox/* /opt/firefox/
      echo $match | sudo tee /opt/firefox/version
    end
  end

  test -f "$HOME/.dotfiles/scripts/symlinks.sh"
  and bash "$HOME/.dotfiles/scripts/symlinks.sh"
end
