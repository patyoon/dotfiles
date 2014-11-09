export PYTHONPATH=/usr/local/lib/python2.7/site-packages

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

#virtualenv
export WORKON_HOME=$HOME/.virtualenvs
if [ ! -d $WORKON_HOME ]; then
    mkdir -p $WORKON_HOME
fi

export VIRTUALENVWRAPPER_VIRTUALENV=/usr/local/bin/virtualenv
export PROJECT_HOME=$HOME/workspace

if [ -f /usr/local/bin/virtualenvwrapper.sh ]; then
		source /usr/local/bin/virtualenvwrapper.sh
else
    echo "virtualenvwrapper.sh not found"
fi

#  Enable bash complete
autoload -U +X bashcompinit && bashcompinit

# Show git branch in command line prompt
export PS1="\\w:\$(git branch 2>/dev/null | grep '^*' | colrm 1 2)\$ "
