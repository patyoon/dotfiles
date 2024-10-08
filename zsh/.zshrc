# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
export ZSH_THEME="powerlevel10k/powerlevel10k"

# Uncomment the following line to use case-sensitive completion.
#CASE_SENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(python rails zeus vagrant ruby rvm rsync brew coffee common-aliases autopep8 bundler fabric gem meteor node npm pip screen tmux tmuxinator)

source $ZSH/oh-my-zsh.sh

# User configuration

# You may need to manually set your language environment
export LANG=en_US.UTF-8

## Functions ##
zsh_stats() { history|awk '{print $2}'|grep -v zsh_stats|sort|uniq -c|sort -rn|head}
mcd() { [ -n "$1" ] && mkdir -p "$@" && cd "$1"; }
pacurl() { p -"$1" "$2" | grep "^URL" | sed -e 's/ //g' | cut -d ":" -f 2- | xclip -i }
slurpurl() { /usr/bin/slurpy "$1" "$2" | grep "^URL" | sed -e 's/ //g' | cut -d ":" -f 2- | xclip -i }
myip() { ifdata -pa "$1" }
speakermute() { ossmix jack.int-speaker.mute toggle }
xrandrvga1() { xrandr --output VGA1 --auto --right-of LVDS1 }
llg () { ls -lh --color=auto "$1" | grep -i "$2" }
showcolors() { for code in {0..255}; do echo -e "\e[38;05;${code}m $code: Test"; done }
countdeps() {
    LC_ALL=C pacman -Qi $1 | grep Required | sed -e 's/Required By    : \([a-z ]*\)/\1/' -e 's/  / /g' | wc -w
}

function search {
    pacman -Ss $@ ; rawr -s "$@"
}

function rcd {
    if [[ `whoami` == "root" ]]; then
        /etc/rc.d/"$1" "$2"
    elif [[ `whoami` == "ogion" ]]; then
        sudo /etc/rc.d/"$1" "$2"
    fi
}

function x {
    if [ -f $1 ]; then
        case $1 in
            *.tar.bz2)  tar -jxvf $1        ;;
            *.tar.gz)   tar -zxvf $1        ;;
            *.tar.xz)   tar -axvf $1        ;;
            *.bz2)      bzip2 -d $1         ;;
            *.gz)       gunzip -d $1        ;;
            *.tar)      tar -xvf $1         ;;
            *.tgz)      tar -zxvf $1        ;;
            *.zip)      unzip $1            ;;
            *.Z)        uncompress $1       ;;
            *.7z)       7z e $1             ;;
            *.rar)      unrar x $1          ;;
            *)          echo "'$1' Error. Please go away" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}

# END FUNCTIONS

DIRSTACKSIZE=${DIRSTACKSIZE:-20}
DIRSTACKFILE=${DIRSTACKFILE:-${HOME}/.zdirs}

function dirjump {
    emulate -L zsh
    autoload -U colors
    local color=$fg_bold[blue]
    integer i=0
    dirs -p | while read dir; do
        local num="${$(printf "%-4d " $i)/ /.}"
        printf " %s  $color%s$reset_color\n" $num $dir
        (( i++ ))
    done
    integer dir=-1
    read -r 'dir?Jump to directory: ' || return
    (( dir == -1 )) && return
    if (( dir < 0 || dir >= i )); then
        echo d: no such directory stack entry: $dir
        return 1
    fi
    cd ~$dir
}

## miscellaneous code ##

## Some quick Perl-hacks aka /useful/ oneliner
bew() { perl -le 'print unpack "B*","'$1'"' }
web() { perl -le 'print pack "B*","'$1'"' }
hew() { perl -le 'print unpack "H*","'$1'"' }
weh() { perl -le 'print pack "H*","'$1'"' }
pversion()    { perl -M$1 -le "print $1->VERSION" } # i. e."pversion LWP -> 5.79"
getlinks ()   { perl -ne 'while ( m/"((www|ftp|http):\/\/.*?)"/gc ) { print $1, "\n"; }' $* }
gethrefs ()   { perl -ne 'while ( m/href="([^"]*)"/gc ) { print $1, "\n"; }' $* }
getanames ()  { perl -ne 'while ( m/a name="([^"]*)"/gc ) { print $1, "\n"; }' $* }
getforms ()   { perl -ne 'while ( m:(\</?(input|form|select|option).*?\>):gic ) { print $1, "\n"; }' $* }
getstrings () { perl -ne 'while ( m/"(.*?)"/gc ) { print $1, "\n"; }' $*}
getanchors () { perl -ne 'while ( m/«([^«»\n]+)»/gc ) { print $1, "\n"; }' $* }
showINC ()    { perl -e 'for (@INC) { printf "%d %s\n", $i++, $_ }' }

# key bindings

bindkey -e

#bindkey "\e[1~": beginning-of-line
#bindkey "\e[4~": end-of-line
#bindkey "\e[5~": beginning-of-history
#bindkey "\e[6~": end-of-history
#bindkey "\e[7~": beginning-of-line
#bindkey "\e[3~": delete-char
#bindkey "\e[2~": quoted-insert
#bindkey "\e[5C": forward-word
#bindkey "\e[5D": backward-word
#bindkey "\e\e[C": forward-word
#bindkey "\e\e[D": backward-word
#bindkey "\e[1;5C": forward-word
#bindkey "\e[1;5D": backward-word
#bindkey '5D' emacs-backward-word
#bindkey '5C' emacs-forward-word

#testing for urxvt
bindkey "\eOd" emacs-backward-word
bindkey "\eOD" emacs-backward-word
bindkey "\e\e[D" emacs-backward-word
bindkey "\eOc" emacs-forward-word
bindkey "\eOC" emacs-forward-word
bindkey "\e\e[C" emacs-forward-word
bindkey "\e[7~" beginning-of-line
bindkey "\e[8~" end-of-line

# for inside tmux
bindkey "\e[1~" beginning-of-line
bindkey "\e[4~" end-of-line

# # for rxvt
#bindkey "\e[8~" end-of-line
#bindkey "\e[7~" beginning-of-line
# # for non RH/Debian xterm, can't hurt for RH/Debian xterm
#bindkey "\eOH" beginning-of-line
#bindkey "\eOF" end-of-line
# # for freebsd console
#bindkey "\e[H" beginning-of-line
#bindkey "\e[F" end-of-line
# # completion in the middle of a line
bindkey '^i' expand-or-complete-prefix

bindkey  "\e[A"    history-search-backward
bindkey  "\e[B"    history-search-forward

#zle-keymap-select () {
#if [ $KEYMAP = vicmd ]; then
#echo -ne "\033]12;Red\007"
#else
#echo -ne "\033]12;Grey\007"
#fi
#}
#zle -N zle-keymap-select
#zle-line-init () {
#zle -K viins
#echo -ne "\033]12;Grey\007"
#}
#zle -N zle-line-init
#bindkey -v

# support colors in less
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

# for setting history length see HISTSIZE and HISTFILESIZE
HISTFILE=~/.zsh_history
HISTSIZE=999999999
SAVEHIST=$HISTSIZE
# Get colors in manual pages
function man {
    env \
        LESS_TERMCAP_mb=$(printf "\e[1;31m") \
        LESS_TERMCAP_md=$(printf "\e[1;31m") \
        LESS_TERMCAP_me=$(printf "\e[0m") \
        LESS_TERMCAP_se=$(printf "\e[0m") \
        LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
        LESS_TERMCAP_ue=$(printf "\e[0m") \
        LESS_TERMCAP_us=$(printf "\e[1;32m") \
        man "$@"
}

# Change directory to the current Finder directory (OS X)
function cdf {
    target=`osascript -e 'tell application "Finder" to if (count of Finder windows) > 0 then get POSIX path of (target of front Finder window as text)'`
    if [ "$target" != "" ]; then
        cd "$target"; pwd
    else
        echo 'No Finder window found' >&2
    fi
}

# One command to update all.
function update {
    #local brew="brew update; brew upgrade;"
    local macport="port selfupdate; sudo port upgrade outdated;"
    local gisty="gisty pull_all; gisty sync_delete"
    local gem="gem update;"
    local pip="pip freeze --local | grep -v '^\-e' | cut -d = -f 1  | xargs pip install -U -q"
    sh -c $brew$gisty; sudo sh -c $gem$pip$macport
}

export TOOL_HOME=/Users/patrick/workspace/tools

# Bash function for custom grep
function wgrep {
    grep -rIn -C 2 $1 ./
}

function pyrgrep {
    grep -r $1 --include "*.py" $2
}

# de-dup history
setopt HIST_IGNORE_DUPS

# DIRSTACK

if [ ! -f $HOME/.cache/zsh/dirs ]; then
    mkdir -p $HOME/.cache/zsh
    touch $HOME/.cache/zsh/dirs
fi

DIRSTACKFILE="$HOME/.cache/zsh/dirs"
if [[ -f $DIRSTACKFILE ]] && [[ $#dirstack -eq 0 ]]; then
    dirstack=( ${(f)"$(< $DIRSTACKFILE)"} )
    [[ -d $dirstack[1] ]] && cd $dirstack[1]
fi
chpwd() {
    print -l $PWD ${(u)dirstack} >$DIRSTACKFILE
}

DIRSTACKSIZE=20

setopt autopushd pushdsilent pushdtohome

## Remove duplicate entries
setopt pushdignoredups

## This reverts the +/- operators.
setopt pushdminus

# Deletes given string from known known_hosts file.
function delete-known-host {
    sed "/$1/d" ~/.ssh/known_hosts > tmp_283497
    mv tmp_283497 ~/.ssh/known_hosts
}

bindkey -e
bindkey '^[[1;9C' forward-word
bindkey '^[[1;9D' backward-word

export VIRTUALENVWRAPPER_PYTHON=`which python`
export VIRTUALENVWRAPPER_VIRTUALENV=`which virtualenv`

export PYTHONSTARTUP=$HOME/.pythonstartup

cd $HOME

export VISUAL='emacsclient -c'

# TODO: Only run after checking OS.
defaults write NSGlobalDomain KeyRepeat -int 0.02

export JAVA_HOME=$(/usr/libexec/java_home -v 1.8)
export PATH=$JAVA_HOME/bin:$PATH

function get-instance-id() {
    echo $1 | xargs -I {} bash -c "curl -s 'https://billow.d.musta.ch/ec2?q=privateHostname==\"{}\"' | jq -r '.[].id'"
}
# source ~/gitstatus/gitstatus.prompt.zsh

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

export PATH="/usr/local/opt/bison/bin:$PATH"

# The next line updates PATH for Netlify's Git Credential Helper.
test -f '/Users/patrickyoon/Library/Preferences/netlify/helper/path.zsh.inc' && source '/Users/patrickyoon/Library/Preferences/netlify/helper/path.zsh.inc'

export PATH="/Users/patrick/workspace/wg-deploy/wbin:$PATH:/Users/patrickyoon/personal_workspace/flutter/bin:/usr/local/texlive/2022basic/bin/universal-darwin"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

for f in .zsh_*aliases; do
    echo "loading aliases: $f"
    source $f
done

for f in .zsh_*internal; do
    echo "loading internal rcs: $f"
    source $f
done
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

export PATH="/opt/homebrew/opt/postgresql@15/bin:$PATH"
export NVM_DIR="$HOME/.nvm"
[ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && \. "/opt/homebrew/opt/nvm/nvm.sh"  # This loads nvm
[ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completionexport PYENV_ROOT="$HOME/.pyenv"
