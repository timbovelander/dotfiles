function update
  if command -s apt-get >/dev/null ^&1
    sudo apt-get update
    sudo apt-get dist-upgrade
  end

  if command -s zypper >/dev/null ^&1
    sudo zypper update
  end

  if command -s brew >/dev/null ^&1
    brew upgrade
  end

  if command -s npm >/dev/null ^&1
    npm -g update
  end

  if command -s apm >/dev/null ^&1
    apm upgrade -c false
  end

  if type -t fisher >/dev/null ^&1
    fisher update
  end

  if command -s git >/dev/null ^&1
    test -d "$HOME/.dotfiles"
    and git -C "$HOME/.dotfiles" pull --rebase=preserve --prune ^/dev/null

    test -d "$HOME/.emacs.d"
    and git -C "$HOME/.emacs.d" pull --rebase=preserve --prune ^/dev/null

    test -d "$HOME/.nodenv"
    and git -C "$HOME/.nodenv" pull --rebase=preserve --prune ^/dev/null

    test -d "$HOME/.nodenv/plugins/node-build"
    and git -C "$HOME/.nodenv/plugins/node-build" pull --rebase=preserve --prune ^/dev/null

    test -d "$HOME/.nodenv/plugins/nodenv-default-packages"
    and git -C "$HOME/.nodenv/plugins/nodenv-default-packages" pull --rebase=preserve --prune ^/dev/null

    test -d "$HOME/.rbenv"
    and git -C "$HOME/.rbenv" pull --rebase=preserve --prune ^/dev/null

    test -d "$HOME/.rbenv/plugins/ruby-build"
    and git -C "$HOME/.rbenv/plugins/ruby-build" pull --rebase=preserve --prune ^/dev/null

    test -d "$HOME/.rbenv/plugins/rbenv-gemset"
    and git -C "$HOME/.rbenv/plugins/rbenv-gemset" pull --rebase=preserve --prune ^/dev/null
  end

  if test -f "/opt/firefox/firefox"
    wget "https://download.mozilla.org/?product=firefox-latest-ssl&os=linux64&lang=nl" -O /tmp/firefox.tar.bz2
    tar -xjf /tmp/firefox.tar.bz2 -C /tmp
    set -l current (/opt/firefox/firefox -v)
    set -l latest (/tmp/firefox/firefox -v)
    if test $current != $latest
      set -e current
      set -e latest
      sudo cp -R /tmp/firefox/* /opt/firefox/
    end
  end

  test -f "$HOME/.dotfiles/scripts/symlinks.sh"
  and bash "$HOME/.dotfiles/scripts/symlinks.sh"
end
