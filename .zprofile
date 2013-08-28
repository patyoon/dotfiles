if [ -f ~/.zshrc ]; then
   source ~/.zshrc
fi
##
# Your previous /Users/Yoon/.bash_profile file was backed up as /Users/Yoon/.bash_profile.macports-saved_2012-09-23_at_23:44:16
##

# MacPorts Installer addition on 2012-09-23_at_23:44:16: adding an appropriate PATH variable for use with MacPorts.
export PATH=/usr/texbin:/usr/local/share/npm/bin:/opt/local/bin:$PATH
alias eniac='ssh yeyoon@eniac.seas.upenn.edu'
alias cis505='ssh cis505@eniac.seas.upenn.edu'
alias shepard='ssh 158.130.51.23 -l yeyoon'
alias shepardy='ssh -Y 158.130.51.23 -l yeyoon'
alias wwbp='ssh yeyoon@ssh.wwbp.org'
alias rms='ssh yeyoon@rms.wharton.upenn.edu'
alias emacsw='/Applications/Emacs.app/Contents/MacOS/Emacs'
alias killemacs='kill `ps -A | grep Emacs | sed "s/^ *//g" | cut -f1 -d " "`'
alias ..='cd ..'
alias ...='cd ../../'
alias ....='cd ../../..'
alias ipython='ipython --no-banner --no-confirm-exit'
alias less='less -F'
test -r /sw/bin/init.sh && . /sw/bin/init.sh
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
#virtualenv
export WORKON_HOME=$HOME/.virtualenvs
source /usr/local/bin/virtualenvwrapper.sh
eval `opam config -env`
export TOOL_HOME=${HOME}/tools
#mount tc volume on startup
${TOOL_HOME}/mount_tc

# Show git branch in command line prompt
export PS1="\\w:\$(git branch 2>/dev/null | grep '^*' | colrm 1 2)\$ "

# Start Python HTTP server with directory
alias pyserver='python -m SimpleHTTPServer 8888'

# nginx short cuts
alias nginx-start='sudo launchctl load -w /Library/LaunchDaemons/org.macports.nginx.plist'
alias nginx-stop='sudo launchctl unload -w /Library/LaunchDaemons/org.macports.nginx.plist'
alias nginx-restart='sudo launchctl unload -w /Library/LaunchDaemons/org.macports.nginx.plist; sudo launchctl load -w /Library/LaunchDaemons/org.macports.nginx.plist'

function rye-job {
    python $TELLAPART_HOME/rye/scripts/rye_cluster_remote.py --run_script rye_jobs_launcher.py --jobs=$1 --start_date=$2 --end_date=$3
}

