#!/bin/sh

# Install fonts
sudo dnf install -y google-droid-sans-mono-fonts

# Install install dependencies
sudo dnf install -y git rsync cmake python-devel

# Install ZSH
sudo dnf install -y zsh autojump-zsh
sudo chsh $USER -s /bin/zsh
wget https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O - | sh

# Install NodeJS
sudo dnf install -y nodejs npm
sudo npm install -g bower grunt-cli gulp jshint yo

# Install ViM
sudo dnf install -y vim-X11 vim-enhanced

# Clone dotfiles and copy
git clone https://github.com/timbovelander/dotfiles.git $HOME/.dotfiles
rsync -r --exclude-from "$HOME/.dotfiles/exclude-files" $HOME/.dotfiles/ $HOME/

# Install RVM
gpg2 --keyserver hkp://keys.gnupg.net --recv-keys 409B6B1796C275462A1703113804BB82D39DC0E3
wget https://get.rvm.io -O - | bash -s stable --ruby
source $HOME/.rvm/scripts/rvm
rvm @global do gem install scss-lint

# Install Vundles
git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim
vim -u $HOME/.dotfiles/.vimrc_vundle +PluginInstall +qall

# Build Vundle YouCompleteMe
mkdir /tmp/ycm_build && cd /tmp/ycm_build
cmake -G "Unix Makefiles" . ~/.vim/bundle/YouCompleteMe/third_party/ycmd/cpp
make ycm_support_libs

# Build Vundle tern_for_vim
cd $HOME/.vim/bundle/tern_for_vim
npm install
