#!/bin/sh
if [ "$#" -eq 0 ]
then
    IS_REMOTE=
else
    case "$1" in
        remote)
            IS_REMOTE=1
            ;;
        local)
            IS_REMOTE=
            ;;
        *)
            echo "value $1 not supported" >&2
            ;;
    esac
fi

# config for both remote and local
git config --global color.ui true
git config --global alias.top '!pwd -L'

# config for remote
if [ "$IS_REMOTE" ]
then
    git config --global core.editor vim
    ...
else
    git config --global core.editor 'subl -n -w'
    !sh -c 'git diff --name-status $1^ $1' -
    ...
fi
