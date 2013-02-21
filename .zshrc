# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how many often would you like to wait before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
export PATH=/Users/Yoon/.opam/system/bin:/usr/texbin:/usr/local/share/npm/bin:/Users/Yoon/bin:/usr/local/bin:/usr/local/sbin:/usr/texbin:/usr/local/share/npm/bin:/Users/Yoon/bin:/usr/local/bin:/usr/local/sbin:/sw/bin:/sw/sbin:/usr/texbin:/usr/local/share/npm/bin:/Users/Yoon/bin:/usr/local/bin:/usr/local/sbin:/Users/Yoon/.rvm/gems/ruby-1.9.3-p362@rails3tutorial2ndEd/bin:/Users/Yoon/.rvm/gems/ruby-1.9.3-p362@global/bin:/Users/Yoon/.rvm/rubies/ruby-1.9.3-p362/bin:/Users/Yoon/.rvm/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/opt/X11/bin:/usr/texbin:/Users/Yoon/.rvm/bin:/usr/X11R6/bin

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

vimwhich() {
    which $1 && vim `which $1`
}
filewhich() {
    which $1 && file `which $1`
}

vimrcconf() {
    if [[ `whoami` == "root" ]]; then
        vim /etc/rc.conf
    elif [[ `whoami` == "ogion" ]]; then
        sudo vim /etc/rc.conf
    fi
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
#bew() { perl -le 'print unpack "B*","'$1'"' }
#web() { perl -le 'print pack "B*","'$1'"' }
#hew() { perl -le 'print unpack "H*","'$1'"' }
#weh() { perl -le 'print pack "H*","'$1'"' }
#pversion()    { perl -M$1 -le "print $1->VERSION" } # i. e."pversion LWP -> 5.79"
#getlinks ()   { perl -ne 'while ( m/"((www|ftp|http):\/\/.*?)"/gc ) { print $1, "\n"; }' $* }
#gethrefs ()   { perl -ne 'while ( m/href="([^"]*)"/gc ) { print $1, "\n"; }' $* }
#getanames ()  { perl -ne 'while ( m/a name="([^"]*)"/gc ) { print $1, "\n"; }' $* }
#getforms ()   { perl -ne 'while ( m:(\</?(input|form|select|option).*?\>):gic ) { print $1, "\n"; }' $* }
#getstrings () { perl -ne 'while ( m/"(.*?)"/gc ) { print $1, "\n"; }' $*}
#getanchors () { perl -ne 'while ( m/«([^«»\n]+)»/gc ) { print $1, "\n"; }' $* }
#showINC ()    { perl -e 'for (@INC) { printf "%d %s\n", $i++, $_ }' }
#vimpm ()      { vim `perldoc -l $1 | sed -e 's/pod$/pm/'` }
#vimhelp ()    { vim -c "help $1" -c on -c "au! VimEnter *" }

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
