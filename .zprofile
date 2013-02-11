if [ -f ~/.zshrc ]; then
   source ~/.zshrc
fi
##
# Your previous /Users/Yoon/.bash_profile file was backed up as /Users/Yoon/.bash_profile.macports-saved_2012-09-23_at_23:44:16
##

# MacPorts Installer addition on 2012-09-23_at_23:44:16: adding an appropriate PATH variable for use with MacPorts.
export PATH=/usr/texbin:/usr/local/share/npm/bin:$PATH
alias eniac='ssh yeyoon@eniac.seas.upenn.edu'
alias shepard='ssh 158.130.51.23 -l yeyoon'
alias shepardy='ssh -Y 158.130.51.23 -l yeyoon'
alias wwbp='ssh yeyoon@wwbp.org'
alias rms='ssh yeyoon@rms.wharton.upenn.edu'
alias emacsw='/Applications/Emacs.app/Contents/MacOS/Emacs'
alias killemacs='kill `ps -A | grep Emacs | sed "s/^ *//g" | cut -f1 -d " "`'
alias gem='sudo gem'
test -r /sw/bin/init.sh && . /sw/bin/init.sh
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
#virtualenv
export WORKON_HOME=$HOME/.virtualenvs
source /usr/local/bin/virtualenvwrapper.sh
eval `opam config -env`
