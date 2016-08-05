function update
  if command -s apt-get >/dev/null ^&1
    sudo apt-get update
    sudo apt-get dist-upgrade
  end

  if command -s zypper >/dev/null ^&1
    sudo zypper update
  end

  if command -s npm >/dev/null ^&1
    npm -g update
  end

  if command -s apm >/dev/null ^&1
    apm upgrade -c false
  end

  if command -s git >/dev/null ^&1
    test -d "$HOME/.dotfiles"; and git -C "$HOME/.dotfiles" pull
    test -d "$HOME/.emacs.d"; and git -C "$HOME/.emacs.d" pull
    test -d "$HOME/.rbenv"; and git -C "$HOME/.rbenv" pull
    test -d "$HOME/.rbenv/plugins/ruby-build"
    and git -C "$HOME/.rbenv/plugins/ruby-build" pull
    test -d "$HOME/.rbenv/plugins/rbenv-gemset"
    and git -C "$HOME/.rbenv/plugins/rbenv-gemset" pull

    test -fx "$HOME/.dotfiles/scripts/symlinks.sh"
    and bash "$HOME/.dotfiles/scripts/symlinks.sh"
  end
end
