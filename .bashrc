# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

export LC_TYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8

export EDITOR="emacs -Q -nw"

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoredups:ignorespace

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

PS1="[\D{%T %x}]\n\W \$ "

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}\007"'
    ;;
*)
    ;;
esac

SSH_ENV="$HOME/.ssh/environment"

function start_agent {
    echo "Initialising new SSH agent..."
    /usr/bin/ssh-agent | sed 's/^echo/#echo/' > "${SSH_ENV}"
    echo succeeded
    chmod 600 "${SSH_ENV}"
    . "${SSH_ENV}" > /dev/null
}

# Source SSH settings, if applicable

if [ -f "${SSH_ENV}" ]; then
    . "${SSH_ENV}" > /dev/null
    ps -ef | grep ${SSH_AGENT_PID} | grep ssh-agent$ > /dev/null || {
        start_agent;
    }
else
    start_agent;
fi
if ssh-add -l | cut -d ' ' -f 3 | grep ${HOME}/.ssh/id_rsa > /dev/null; then
    echo id_rsa already added to agent
else
    /usr/bin/ssh-add
fi

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'

fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias ag='ack-grep'
alias age='ack-grep --erlang'
alias kred='bin/kred -i -no_cron'
alias gt='source gt'
alias rebuild='make -j 12 myday && bin/kred -n master && bin/kred -i'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi


# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

export ERL_INETRC=${HOME}/.inetrc

# Paths ------------------------------------------------------------------------

if [[ -z $ORIG_PYTHONPATH ]]; then
   export ORIG_PYTHONPATH="${PYTHONPATH}"
fi
export PYTHONPATH=${ORIG_PYTHONPATH}


if [[ -z $ORIG_PATH ]]; then
   export ORIG_PATH="${PATH}"
fi
export PATH=${ORIG_PATH}

export OTP_PATH="${HOME}/erlang/install/current"
export PATH="${OTP_PATH}/bin:${PATH}"
export DIALYZER_PLT="${OTP_PATH}/dialyzer.plt"

#Chef
export PATH="/opt/chef/bin:/opt/chef/embedded/bin:${PATH}"
#Misc paths
export PATH="${PATH}:~/bin:~/scripts:${HOME}/.erlang.d/current/bin:${HOME}/src/rebar"

# Go
export PATH="${HOME}/src/golang/go/bin:${PATH}"
export GOPATH=~/src/golang/packages

# Ansible ----------------------------------------------------------------------
export ANSIBLE_HOME=${HOME}/src/ansible
export PATH=${ANSIBLE_HOME}/bin:${PATH}
export ANSIBLE_LIBRARY=${ANSIBLE_HOME}/library
export MANPATH=${ANSIBLE_HOME}/docs/man
export PYTHONPATH=${ANSIBLE_HOME}/lib:${PYTHONPATH}

# Klarna -----------------------------------------------------------------------

function cd {
  builtin cd "${@:1}"
  GIT_AUTHOR_EMAIL_ORIG=${GIT_AUTHOR_EMAIL}
  if [[ -n "${PWD}" ]]; then
    if [[ "$(readlink -f ${PWD})" == *"$HOME/klarna"* ]]; then
        GIT_AUTHOR_EMAIL_NEW=${KLARNA_EMAIL}
    else
        GIT_AUTHOR_EMAIL_NEW=${EMAIL}
    fi
    if [[ "${GIT_AUTHOR_EMAIL_NEW}" != "${GIT_AUTHOR_EMAIL}" ]]; then
        export GIT_AUTHOR_EMAIL=${GIT_AUTHOR_EMAIL_NEW}
        echo GIT_AUTHOR_EMAIL=${GIT_AUTHOR_EMAIL_NEW}
    fi
  fi
}
cd $PWD

# Paths
export PATH="${PATH}:~/klarna/fred/fred_platform/bin:~/klarna/fred/gitrdun/bin"
export PATH="${PATH}:~/klarna/chef/berksenv"
export PATH="${PATH}:~/klarna/cloudstack/orchid/bin"

# Env
# Make sure to use kernel_poll
export KRED_POLL=true
export IGNORE_FORCE_BACKUP=true
export KRED_SKIP_SUBMODULE_UPDATE=true
