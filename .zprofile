export PATH=/usr/local/Cellar/python26/2.6.9/bin:/usr/texbin:/usr/local/bin:/usr/local/share/npm/bin:/opt/local/bin:/usr/bin:/usr/texbin:/usr/local/share/npm/bin:/usr/local/bin:/sbin:/usr/local/sbin:/usr/texbin:/usr/X11R6/bin:$TELLAPART_HOME/tools/bin:/opt/local/bin:/Users/patrick/tools:/bin:/usr/sbin

test -r /sw/bin/init.sh && . /sw/bin/init.sh
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

#virtualenv
export WORKON_HOME=$HOME/.virtualenvs
if [ ! -d $WORKON_HOME ]; then
    mkdir -p $WORKON_HOME
fi
export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python2.6
export VIRTUALENVWRAPPER_VIRTUALENV=/usr/local/bin/virtualenv
export PROJECT_HOME=$HOME/workspace
source /usr/local/bin/virtualenvwrapper.sh
export TOOL_HOME=${HOME}/workspace/tools

#mount tc volume on startup
${TOOL_HOME}/mount_tc

#  Enable bash complete
autoload -U +X bashcompinit && bashcompinit

# Show git branch in command line prompt
export PS1="\\w:\$(git branch 2>/dev/null | grep '^*' | colrm 1 2)\$ "

if [ -f ~/.zshrc ]; then
    source ~/.zshrc
else
    echo ".zshrc not found"
fi
