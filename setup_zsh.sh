#!/usr/bin/env zsh

# Install oh my zsh
sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

# Install some plugins
git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
git clone https://github.com/zdharma-continuum/fast-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/fast-syntax-highlighting
git clone --depth=1 https://github.com/romkatv/powerlevel10k.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/themes/powerlevel10k

sed -i -e 's/^plugins=.*/plugins=(git z fast-syntax-highlighting zsh-autosuggestions)/g' ~/.zshrc
sed -i -e 's/^ZSH_THEME=.*/ZSH_THEME="powerlevel10k\/powerlevel10k"/g' ~/.zshrc


cat > $HOME/.custom.zsh <<- EOM
alias gd='git diff'
alias gs='git status'
alias gco='git checkout'
alias gcm='git commit'
alias gl='git lg'
alias gsh='git show'
alias gad='git add -A'
alias gr='git recent'
alias gp='git push'
alias gf='git fetch'
alias gfp='git pull'
alias gfm='gco master; gf'
alias grb='git rebase'
alias gpoh='gp -u origin HEAD'
alias gpah='gp -u anorwell HEAD'

export ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
export ZSHZ_NO_RESOLVE_SYMLINKS=1

find-name() { find . -name "*$1*" }
find-path() { find . -path "*$1*" }
find-tests() { find . -path "*test/*$1*test.rb" }
push-tests() { find-tests  | xargs spin push }

setopt inc_append_history
# Reloads the history whenever you use it
setopt share_history

HISTFILE="$HOME/.zsh_history"
HISTSIZE=10000000
SAVEHIST=10000000
EOM

cat >> $HOME/.zshrc <<- EOM
[ -f ~/.custom.zsh ] && source ~/.custom.zsh
EOM

cat > $HOME/.gitconfig <<- EOM
[alias]
        lg1 = log --graph --abbrev-commit --decorate --format=format:'%C(bold blue)%h%C(reset) - %C(bold green)(%ar)%C(reset) %C(white)%s%C(reset) %C(dim white)- %an%C(reset)%C(bold yellow)%d%C(reset)'
        lg2 = log --graph --abbrev-commit --decorate --format=format:'%C(bold blue)%h%C(reset) - %C(bold cyan)%aD%C(reset) %C(bold green)(%ar)%C(reset)%C(bold yellow)%d%C(reset)%n''          %C(white)%s%C(reset) %C(dim white)- %an%C(reset)'
        lg = !"git lg1"
        recent = "for-each-ref --sort=-committerdate refs/heads/ --format='%(HEAD) %(color:yellow)%(refname:short)%(color:reset) - %(color:red)%(objectname:short)%(color:reset) - %(contents:subject) - %(authorname) (%(color:green)%(committerdate:relative)%(color:reset))'" --count 12
[user]
        name = Arron Norwell
        email = anorwell@gmail.com
[core]
        editor = mg
        excludesfile = /Users/anorwell/.gitignore_global
EOM

source ~/.zshrc

p10k configure

# install fzf -- seems like it has to be after p10k in the .zsh file...
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf && ~/.fzf/install

