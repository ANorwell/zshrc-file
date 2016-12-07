# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH=/Users/anorwell/.antigen/repos/https-COLON--SLASH--SLASH-github.com-SLASH-robbyrussell-SLASH-oh-my-zsh.git


# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="candy"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git ruby scala sbt osx rails brew symfony2 redis-cli)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

alias emacs="/usr/local/Cellar/emacs/HEAD/Emacs.app/Contents/MacOS/Emacs"
alias em="emacsclient -n --alternate-editor=emacs"
alias be="bundle exec"


alias -g L='| less'
alias -g J='| jq "."'
alias -g HD='| hexdump -C'
alias -g X='| xmllint --format -'
alias -g G='| grep'


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
alias grb='git rebase'


#set up path
#PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
PATH=/usr/bin:/bin:/usr/sbin:/sbin
PATH=/usr/local/bin:$PATH
PATH=/usr/local/git/bin:$PATH
PATH=/opt/local/bin:/opt/local/sbin:$PATH
PATH="$HOME/.cask/bin:$PATH"
PATH="$HOME/.rbenv/bin:$PATH"
PATH="/usr/local/texlive/2014basic/bin/universal-darwin:$PATH"
PATH="$HOME/.chefdk/gem/ruby/2.1.0/bin:$PATH"
PATH="$HOME/work/go/bin:$PATH"
PATH=$HOME/bin:$PATH
PATH=$PATH:$HOME/.rbenv/versions/2.1.2/bin

export PATH

##rbenv
eval "$(rbenv init -)"

##GO
export PATH=$PATH:/usr/local/opt/go/libexec/bin
GOPATH=$HOME/work/go
export GOPATH

ulimit -n 10000

export PGHOST=localhost

function java8 {
    export JAVA_HOME=`/usr/libexec/java_home -v 1.8.0_51`
    java -version
}

function java7 {
    export JAVA_HOME=`/usr/libexec/java_home -v 1.7.0_60`
    java -version
}

java8

HISTFILE=~/.zsh-histfile
HISTSIZE=1000000
SAVEHIST=1000000

ssh-add -t 0 ~/.ssh/id_rsa &> /dev/null
ssh-add -t 0 ~/.ec2_backupify/backupify.pem &> /dev/null
ssh-add -t 0 ~/.ec2_backupifydev/backupifydev.pem &> /dev/null
