[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*


export PATH=$HOME/bin:/usr/local/bin:$PATH
PATH=$PATH:$HOME/.rvm/bin:/usr/local/sbin # Add RVM to PATH for scripting

if [ -d "$HOME/workspace/bin" ]; then
    PATH=$PATH:$HOME/workspace/bin
fi

#virtualenv
export WORKON_HOME=$HOME/.virtualenvs
if [ ! -d $WORKON_HOME ]; then
    mkdir -p $WORKON_HOME
fi

export VIRTUALENVWRAPPER_VIRTUALENV=/usr/local/bin/virtualenv
export PROJECT_HOME=$HOME/workspace

# if [ -f /usr/local/bin/virtualenvwrapper.sh ]; then
# 	source /usr/local/bin/virtualenvwrapper.sh
# else
#     echo "virtualenvwrapper.sh not found"
# fi

#  Enable bash complete
autoload -U +X bashcompinit && bashcompinit
