# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"

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

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git python rails zeus vagrant ruby rvm rsync brew coffee common-aliases autopep8 bundler fabric gem gitfest meteor node npm osx pip screen tmux tmuxinator)

source $ZSH/oh-my-zsh.sh

# User configuration

export PATH=$HOME/bin:/usr/local/bin:$PATH
# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
export ARCHFLAGS="-arch x86_64"

# ssh
export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
alias zshconfig="mate ~/.zshrc"
alias ohmyzsh="mate ~/.oh-my-zsh"

# Customize to your needs...

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

search() {
    pacman -Ss $@ ; rawr -s "$@"
}

rcd() {
    if [[ `whoami` == "root" ]]; then
        /etc/rc.d/"$1" "$2"
    elif [[ `whoami` == "ogion" ]]; then
        sudo /etc/rc.d/"$1" "$2"
    fi
}

x () {
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

dirjump() {
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
HISTSIZE=1000
HISTFILESIZE=2000

# Get colors in manual pages
man() {
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
cdf() {
    target=`osascript -e 'tell application "Finder" to if (count of Finder windows) > 0 then get POSIX path of (target of front Finder window as text)'`
    if [ "$target" != "" ]; then
        cd "$target"; pwd
    else
        echo 'No Finder window found' >&2
    fi
}

# One command to update all.
update() {
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
# source `which virtualenvwrapper.sh`

# JAVA_HOME for ec2 cli tools
export JAVA_HOME="`/usr/libexec/java_home`"
# ec2 api tools home.
export EC2_HOME=/Users/Yoon/tools/ec2-api-tools
# add ec2 api tools to path
export PATH=$PATH:$EC2_HOME/bin
export PYTHONSTARTUP=$HOME/.pythonstartup

cd $HOME

for f in .zsh_*aliases; do
    echo "Sourcing $f"
    source $f
done

if [ ! -f /Users/"$(whoami)"/.cache/zsh/dirs ]; then
  mkdir /Users/"$(whoami)"/.cache/zsh
  touch /Users/"$(whoami)"/.cache/zsh/dirs
fi

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

export EDITOR=emacs

# Emacs-like region copy and paste

# x-copy-region-as-kill () {
#     zle copy-region-as-kill
#     print -rn $CUTBUFFER | xsel -i
# }
# zle -N x-copy-region-as-kill
# x-kill-region () {
#     zle kill-region
#     print -rn $CUTBUFFER | xsel -i
# }
# zle -N x-kill-region
# x-yank () {
#     CUTBUFFER=$(xsel -o)
#     zle yank
# }
# zle -N x-yank
# bindkey -e '\eW' x-copy-region-as-kill
# bindkey -e '^W' x-kill-region
# bindkey -e '^Y' x-yank
