source ~/.antigen.zsh

[[ $EMACS = t ]] && unsetopt zle

#set up path
#PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
PATH=/usr/bin:/bin:/usr/sbin:/sbin
PATH=$PATH:$HOME/bin
PATH=/usr/local/bin:$PATH
PATH=/usr/local/git/bin:$PATH
PATH=/opt/local/bin:/opt/local/sbin:$PATH
PATH="$HOME/.cask/bin:$PATH"
PATH="$HOME/.rbenv/bin:$PATH"
PATH="/usr/local/texlive/2014basic/bin/universal-darwin:$PATH"
export PATH

##rbenv
eval "$(rbenv init -)"

##GO
GOPATH=$HOME/gopath
export GOPATH

SHELL=/usr/local/bin/zsh
export SHELL


#LD_LIBRARY_PATH=/usr/local/lib
#export LD_LIBRARY_PATH

#make sbt suck less
SBT_OPTS="-XX:MaxPermSize=378m -Djava.library.path=/usr/local/lib"
export SBT_OPTS

#pg dir
export PGDATA=/usr/local/var/postgres

##set up antigen theme
antigen-lib
antigen-bundle zsh-users/zsh-syntax-highlighting
antigen-bundle zsh-users/zsh-completions src

# Tracks your most used directories, based on 'frecency'. And its accompanying
# setup code.
antigen-bundle rupa/z
add-zsh-hook precmd _z_precmd
function _z_precmd {
    _z --add "$PWD"
}

antigen-theme candy
antigen-apply


##don't make wordchars so inclusive
export WORDCHARS=''

###Aliases
alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs"
alias em="emacsclient -n --alternate-editor=emacs"
alias be="bundle exec"


alias -g L='| less'
alias -g J='| python -m json.tool'
alias -g HD='| hexdump -C'
alias -g X='| xmllint --format -'
alias -g G='| grep'

safe-push() { ./script/parallel_test.rb && git push $*; }

fix-ivy() { find ~/.ivy2/cache/ -name 'ivydata*.properties' | xargs perl -p -i -e 's/resolver=inter-project/resolver=releases/g' }

export PYTHONPATH=/usr/local/lib/python2.7/site-packages:$PYTHONPATH
export GRAPHITE_ROOT=/Users/anorwell/work/misc/graphite
export PYTHONPATH=$PYTHONPATH:$GRAPHITE_ROOT/webapp

find-name() { find ${2-.} -name "*$1*" }

changed-tests() { git diff --name-only "$1" | grep '\(test/unit\|test/integration\|test/functional\).*rb' | xargs spin push }

###############
# zsh Customization
###############


#Color table from: http://www.understudy.net/custom.html
fg_black=%{$'\e[0;30m'%}
fg_red=%{$'\e[0;31m'%}
fg_green=%{$'\e[0;32m'%}
fg_brown=%{$'\e[0;33m'%}
fg_blue=%{$'\e[0;34m'%}
fg_purple=%{$'\e[0;35m'%}
fg_cyan=%{$'\e[0;36m'%}
fg_lgray=%{$'\e[0;37m'%}
fg_dgray=%{$'\e[1;30m'%}
fg_lred=%{$'\e[1;31m'%}
fg_lgreen=%{$'\e[1;32m'%}
fg_yellow=%{$'\e[1;33m'%}
fg_lblue=%{$'\e[1;34m'%}
fg_pink=%{$'\e[1;35m'%}
fg_lcyan=%{$'\e[1;36m'%}
fg_white=%{$'\e[1;37m'%}
#Text Background Colors
bg_red=%{$'\e[0;41m'%}
bg_green=%{$'\e[0;42m'%}
bg_brown=%{$'\e[0;43m'%}
bg_blue=%{$'\e[0;44m'%}
bg_purple=%{$'\e[0;45m'%}
bg_cyan=%{$'\e[0;46m'%}
bg_gray=%{$'\e[0;47m'%}
#Attributes
at_normal=%{$'\e[0m'%}
at_bold=%{$'\e[1m'%}
at_italics=%{$'\e[3m'%}
at_underl=%{$'\e[4m'%}
at_blink=%{$'\e[5m'%}
at_outline=%{$'\e[6m'%}
at_reverse=%{$'\e[7m'%}
at_nondisp=%{$'\e[8m'%}
at_strike=%{$'\e[9m'%}
at_boldoff=%{$'\e[22m'%}
at_italicsoff=%{$'\e[23m'%}
at_underloff=%{$'\e[24m'%}
at_blinkoff=%{$'\e[25m'%}
at_reverseoff=%{$'\e[27m'%}
at_strikeoff=%{$'\e[29m'%}

##use emacs mode
bindkey -e

### vcs in prompt

#setopt prompt_subst
#autoload -Uz vcs_info
zstyle ':vcs_info:*' actionformats \
    '%F{5}(%f%s%F{5})%F{3}-%F{5}[%F{2}%b%F{3}|%F{1}%a%F{5}]%f '
zstyle ':vcs_info:*' formats       \
    '%F{5}(%f%s%F{5})%F{3}-%F{5}[%F{2}%b%F{5}]%f '
zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat '%b%F{1}:%F{3}%r'

zstyle ':vcs_info:*' enable git cvs svn

# or use pre_cmd, see man zshcontrib
vcs_info_wrapper() {
  vcs_info
  if [ -n "$vcs_info_msg_0_" ]; then
    echo "%{$fg[grey]%}${vcs_info_msg_0_}%{$reset_color%}$del"
  fi
}


function __git_prompt {
  local DIRTY="%{$fg[yellow]%}"
  local CLEAN="%{$fg[green]%}"
  local UNMERGED="%{$fg[red]%}"
  local RESET="%{$terminfo[sgr0]%}"
  git rev-parse --git-dir >& /dev/null
  if [[ $? == 0 ]]
  then
    echo -n "["
    if [[ `git ls-files -u >& /dev/null` == '' ]]
    then
      git diff --quiet >& /dev/null
      if [[ $? == 1 ]]
      then
        echo -n $DIRTY
      else
        git diff --cached --quiet >& /dev/null
        if [[ $? == 1 ]]
        then
          echo -n $DIRTY
        else
          echo -n $CLEAN
        fi
      fi
    else
      echo -n $UNMERGED
    fi
    echo -n `git branch | grep '* ' | sed 's/..//'`
    echo -n $RESET
    echo -n "]"
  fi
}

#PROMPT="
#${fg_lgreen}%n@${at_underl}%m${at_underloff}${fg_white}[${fg_cyan}%~${fg_white}]
#[${fg_green}%T${fg_white}]:${at_normal}"

#RPROMPT=$'$(__git_prompt)'




#Set the auto completion on
autoload -U compinit colors
compinit
colors

#Lets set some options
setopt autocd
setopt auto_resume
setopt AUTO_PUSHD


HISTFILE=~/.zsh-histfile
HISTSIZE=1000000
SAVEHIST=1000000

setopt APPEND_HISTORY

# Killer: share history between multiple shells
setopt SHARE_HISTORY

# If I type cd and then cd again, only save the last one
setopt HIST_IGNORE_DUPS

# Even if there are commands inbetween commands that are the same, still only save the last one
setopt HIST_IGNORE_ALL_DUPS

# Pretty    Obvious.  Right?
setopt HIST_REDUCE_BLANKS

# If a line starts with a space, don't save it.
setopt HIST_IGNORE_SPACE
setopt HIST_NO_STORE

# When using a hist thing, make a newline show the change before executing it.
setopt HIST_VERIFY

# Save the time and how long a command ran
setopt EXTENDED_HISTORY

setopt HIST_SAVE_NO_DUPS
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_FIND_NO_DUPS

#search history with up and down keys
bindkey "^[[A" history-beginning-search-backward
bindkey "^[[B" history-beginning-search-forward


## Enables the extgended globbing features
setopt extendedglob

##disable correcting
unsetopt correct_all

#Set some ZSH styles
zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
zstyle ':completion:*:warnings' format '%BSorry, no matches for: %d%b'

zstyle ':completion:*' menu select

#zstyle ':completion:*' verbose yes
#zstyle ':completion:*:descriptions' format '%B%d%b'
#zstyle ':completion:*:messages' format '%d'
#zstyle ':completion:*:warnings' format 'No matches for: %d'
#zstyle ':completion:*' group-name ''
#zstyle ':completion:*' completer _oldlist _expand _force_rehash _complete
#zstyle ':completion:*' completer _expand _force_rehash _complete _approximate _ignored
## generate descriptions with magic.
#zstyle ':completion:*' auto-description 'specify: %d'

# Don't prompt for a huge list, page it!
#zstyle ':completion:*:default' list-prompt '%S%M matches%s'

# Don't prompt for a huge list, menu it!
#zstyle ':completion:*:default' menu 'select=0'

# Have the newer files last so I see them first
#zstyle ':completion:*' file-sort modification reverse

# color code completion!!!!  Wohoo!
zstyle ':completion:*' list-colors "=(#b) #([0-9]#)*=36=31"

unsetopt LIST_AMBIGUOUS
setopt  COMPLETE_IN_WORD

# Separate man page sections.  Neat.
#zstyle ':completion:*:manuals' separate-sections true

# Egomaniac!
# more errors allowed for large words and fewer for small words
##THIS ONE BREAKS cd completion...
#zstyle ':completion:*:approximate:*' max-errors 'reply=(  $((  ($#PREFIX+$#SUFFIX)/3  ))  )'

# Errors format
#zstyle ':completion:*:corrections' format '%B%d (errors %e)%b'

# Don't complete directory we are already in (../here)
zstyle ':completion:*' ignore-parents parent pwd

zstyle ':completion::approximate*:*' prefix-needed false

#}}}
setopt completealiases


_force_rehash() {
  (( CURRENT == 1 )) && rehash
  return 1  # Because we didn't really complete anything
}


## autojump
if [ -f `brew --prefix`/etc/autojump ]; then
  . `brew --prefix`/etc/autojump
fi


function ec2env {
        env=$1
        export EC2_KEYS_HOME=~/.ec2${env:+"_${env}"}
        source $EC2_KEYS_HOME/profile
}
ec2env backupifydev


ssh-add -t 0 ~/.ssh/id_rsa &> /dev/null
ssh-add -t 0 ~/.ec2_backupify/backupify.pem &> /dev/null
ssh-add -t 0 ~/.ec2_backupifydev/backupifydev.pem &> /dev/null
