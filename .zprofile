##
# Your previous /Users/Yoon/.bash_profile file was backed up as /Users/Yoon/.bash_profile.macports-saved_2012-09-23_at_23:44:16
##

# MacPorts Installer addition on 2012-09-23_at_23:44:16: adding an appropriate PATH variable for use with MacPorts.
export PATH=/usr/texbin:/usr/local/share/npm/bin:/opt/local/bin:/usr/bin:/usr/texbin:/usr/local/share/npm/bin:/usr/local/bin:/usr/local/sbin:/usr/texbin:/usr/local/share/npm/bin:/usr/local/bin:/usr/local/sbin:/sw/bin:/sw/sbin:/usr/texbin:/usr/local/share/npm/bin:/usr/local/bin:/usr/local/sbin://bin:/usr/sbin:/sbin:/usr/local/bin:/opt/X11/bin:/usr/texbin:/usr/X11R6/bin:$TELLAPART_HOME/tools/bin:/opt/local/bin:/Users/patrickyoon/tools:/:/opt/local/Library/Frameworks/Python.framework/Versions/2.6/bin:/opt/local/bin/:


test -r /sw/bin/init.sh && . /sw/bin/init.sh
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
#virtualenv
export WORKON_HOME=$HOME/.virtualenvs
source /usr/local/bin/virtualenvwrapper.sh
eval `opam config -env`
export TOOL_HOME=${HOME}/tools
#mount tc volume on startup
${TOOL_HOME}/mount_tc



export PYTHONPATH=/opt/local/Library/Frameworks/Python.framework/Versions/2.6/lib/python2.6/site-packages:/Library/Python/2.6/site-packages

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

if [ -f ~/.zshrc ]; then
    source ~/.zshrc
else
    echo ".zshrc not found"
fi
