# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

LastRC=$?

deactivate 2>/dev/null

export LC_TYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8

export EDITOR="emacs --no-splash -nw"

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

HISTFILE_DIR=${HOME}/.bash_history.d
HISTFILE=${HISTFILE_DIR}/${BASHPID}
function update_hist() {
    history -a ${HISTFILE}.local
    history -r ${HISTFILE_DIR}/common
    
    cat $(find ${HISTFILE_DIR} -name '*.local') | awk '!a[$0]++' > ${HISTFILE_DIR}/common
    cat ${HISTFILE}.local ${HISTFILE_DIR}/common | awk '!a[$0]++' > ${HISTFILE}
}

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

function aws_adfs_prompt() {
  if [[ -n $AWS_SESSION_EXPIRATION_TIME ]] && [[ $AWS_SESSION_EXPIRATION_TIME -gt $(date +%s) ]]; then
    echo "[$AWS_PROFILE ($(( ($AWS_SESSION_EXPIRATION_TIME - $(date -u +%s)) / 60)) minute(s) remaining)]"
  fi
}

source ~/.git-prompt.sh

set_prompt () {
    LASTRC=$?
    RESETCOLOR='\e[m'
    CYAN="\[\033[0;36m\]"
    MAGENTA="\[\033[0;35m\]"
    RED="\[\033[1;31m\]"
    YELLOW="\[\033[0;33m\]"
    BLUE="\[\033[34m\]"
    GREEN="\[\033[1;32m\]"

    GIT_PS1_SHOWDIRTYSTATE=true
    GIT_PS1_SHOWUPSTREAM=''
    GIT=$(__git_ps1 "%s")

    if [[ "$GIT" =~ \*$ ]]; then
        GIT_COLOR=$RED # Unstaged changes
    elif [[ "$GIT" =~ \+$ ]]; then
        GIT_COLOR=$YELLOW # Staged, uncommitted changes
    else
        GIT_COLOR=$GREEN # Clean state
    fi
    GIT="$GIT_COLOR$GIT$RESETCOLOR"

    if [[ $LASTRC == 0 ]]; then
        RES="$GREEN\342\234\223$RESETCOLOR"
    else
        RES="$RED\342\234\227$RESETCOLOR"
    fi
    DATE="$CYAN\D{%T %x}$RESETCOLOR"
    PS1="$RES [$DATE] ${GIT}\n\W \$ "
}

# If this is an xterm set the title to user@host:dir
PROMPT_COMMAND='set_prompt'
case "$TERM" in
xterm*|rxvt*)
    PROMPT_COMMAND=$PROMPT_COMMAND'; echo -ne "\033]0;${USER}@${HOSTNAME}\007"'
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

function ensure_gpg_agent_started {
    # Does `.gpg-agent-info' exist and points to a gpg-agent process accepting signals?
    if [ -f $HOME/.gpg-agent-info ] && \
           kill -0 $(cut -d: -f 2 $HOME/.gpg-agent-info) 2>/dev/null
    then
        # Yes, `.gpg-agent.info' points to valid gpg-agent process;
        # Indicate gpg-agent process
        source $HOME/.gpg-agent-info
    else
        # No, no valid gpg-agent process available;
        # Start gpg-agent
        eval $(gpg-agent --daemon --no-grab --write-env-file $HOME/.gpg-agent-info)
    fi
    export GPG_TTY=$(tty)
    export GPG_AGENT_INFO
}
ensure_gpg_agent_started


# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'

fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias gt='source gt'

alias grep='grep --color=auto'

function g () {
    find . -name "*.erl" -exec grep -rnH $@ {} \;
}



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

if [[ -z ${ORIG_MANPATH} ]]; then
    if [[ -z ${MANPATH} ]]; then
        export ORIG_MANPATH=$(manpath -q)
    else
        export ORIG_MANPATH=${MANPATH}
    fi
fi
export MANPATH=${ORIG_MANPATH}

export OTP_PATH="${HOME}/.erlang.d/current"
export PATH="${OTP_PATH}/bin:${PATH}"
export DIALYZER_PLT="${OTP_PATH}/dialyzer.plt"

# Chef
#export PATH="/opt/chef/bin:/opt/chef/embedded/bin:${PATH}"
# Misc paths
export PATH="${PATH}:~/bin:~/scripts:${HOME}/.erlang.d/current/bin"
#:${HOME}/src/rebar"

# Go
export GOROOT="${HOME}/src/golang/go"
export GOPATH="${HOME}/src/golang/packages"
export PATH="${GOROOT}/bin:${GOPATH}/bin:${PATH}"

# Stash CLI
export STASH_USER=thomas.jarvstrand
export PATH="${HOME}/src/stash-cli:${HOME}/klarna/stash:${PATH}"

# Ansible ----------------------------------------------------------------------
export ANSIBLE_HOME=${HOME}/src/ansible
export PATH=${ANSIBLE_HOME}/bin:${PATH}
export MANPATH=${MANPATH}:${ANSIBLE_HOME}/docs/man
export PYTHONPATH=${ANSIBLE_HOME}/lib:${PYTHONPATH}

# Java -------------------------------------------------------------------------
export JAVA_HOME=$(readlink -f /usr/bin/javac | sed "s:bin/javac::")
export PATH=${PATH}:${JAVA_HOME}/bin


# Riak -------------------------------------------------------------------------
ulimit -n 65536

# # Python -----------------------------------------------------------------------
. ${HOME}/.virtualenv/bin/activate

# Klarna -----------------------------------------------------------------------
export ERL_LIBS=${HOME}/klarna/quickcheck
export KRED_SKIP_SUBMODULE_UPDATE=TRUE
export esup=esup.cloud.internal.machines
export esup_oncall="lars.sjostrom johan.wiren martin.wilhelm mats.westin sandor.bodor jefferson.girao"
export burrus="fredrik.lindberg jimmy.zoger eduard.zamora"
export fred="vadym.khatsanovskyy samuel.strand enrique.fernandez nuno.marques andre.goncalves howard.beard-marlowe"
GIT_AUTHOR_NAME="Thomas Järvstrand"
GIT_COMMITTER_NAME="Thomas Järvstrand"
function cd_git {
  GIT_COMMITTER_EMAIL_ORIG=${GIT_COMMITTER_EMAIL}
  GIT_AUTHOR_EMAIL_ORIG=${GIT_AUTHOR_EMAIL}
  if [[ -n "${PWD}" ]]; then
    if [[ "$(readlink -f ${PWD})" == *"$HOME/klarna"* ]]; then
        GIT_COMMITTER_EMAIL_NEW=${KLARNA_EMAIL}
        GIT_AUTHOR_EMAIL_NEW=${KLARNA_EMAIL}
    else
        GIT_COMMITTER_EMAIL_NEW=${EMAIL}
        GIT_AUTHOR_EMAIL_NEW=${EMAIL}
    fi
    if [[ "${GIT_AUTHOR_EMAIL_NEW}" != "${GIT_AUTHOR_EMAIL}" ]]; then
        export GIT_COMMITTER_EMAIL=${GIT_COMMITTER_EMAIL_NEW}
        export GIT_AUTHOR_EMAIL=${GIT_AUTHOR_EMAIL_NEW}
        echo git email: ${GIT_AUTHOR_EMAIL_NEW}
    fi
  fi
}

function cd_otp {
  PATH_NO_OTP=$(echo $PATH |
                sed "s:${HOME}/.erlang.d/[^:]*::g" |
                tr -s ":")
  if [[ -n "${PWD}" ]]; then
    if [[ "$(readlink -f ${PWD})" == *"$HOME/klarna/fred"* ]]; then
        OTP_NEW="otp_17.5.6_kred"
    else
        if [[ "$(readlink -f ${PWD})" == *"$HOME/klarna/kred"* ]]; then
             OTP_NEW="18.3.4.5+kred1"
        else
        OTP_NEW="current"
        fi
    fi
    OTP_PATH_NEW="${HOME}/.erlang.d/${OTP_NEW}"
    if [[ "${OTP_NEW}" == "current" ]]
    then
        OTP_NAME=$(readlink ${OTP_PATH_NEW} | sed -r "s:.*/(.*)$:\1:")
        echo "OTP build: ${OTP_NAME} (current)"
    else
        echo "OTP build: ${OTP_NEW}"
    fi
    export PATH="${OTP_PATH_NEW}/bin:${PATH_NO_OTP}"
    # if [[ "${OTP_NEW}" != "${GIT_AUTHOR_EMAIL}" ]]; then
    #     export GIT_COMMITTER_EMAIL=${GIT_COMMITTER_EMAIL_NEW}
    #     export GIT_AUTHOR_EMAIL=${GIT_AUTHOR_EMAIL_NEW}
    #     echo git email: ${GIT_AUTHOR_EMAIL_NEW}
    # fi
  fi
}

function cd {
  builtin cd "${@:1}"
  cd_git
  cd_otp
}
cd $PWD

function aws-with-adfs-login
 {
    RES=$($(which aws-adfs-tool) login -r ${AWS_ADFS_ROLE} -a ${AWS_ADFS_ACCOUNT})
    RET=${?}
    if [[ "${RET}" == "0" ]]; then
        eval $(echo ${RES} | tee ${AWS_ENV})
    else
        echo ${RES}
        return ${RET}
    fi
}

export AWS_ENV=${HOME}/.aws/adfs-env
function aws-with-adfs {
    source ${AWS_ENV} 2>/dev/null
    EXPIRY=$(((${AWS_SESSION_EXPIRATION_TIME} - $(date -u +%s)) / 60))
    if [ ${EXPIRY} -le 0 ]; then
        aws-with-adfs-login
    fi
    EXPIRY=$(((${AWS_SESSION_EXPIRATION_TIME} - $(date -u +%s)) / 60))
    if [ $# -ne 0 ]; then
        $(which aws) ${@}
    fi
    >&2 echo "ADFS Session expires in ${EXPIRY} minutes"
}

alias aws='aws-with-adfs'
export AD_USERNAME=thomas.jarvstrand
export AWS_ADFS_ROLE=Klarna_ADFS_burrus
export AWS_ADFS_ACCOUNT=${AWS_ADFS_ACCOUNT:-klarna-non-production}
export AWS_DEFAULT_PROFILE=${AWS_ADFS_ROLE}@${AWS_ADFS_ACCOUNT}

function pr {
    stash pr create ${1:-${burrus}} ${@:2}
}

function otp {
    if [[ ! $1 ]]; then
        CURRENT=$(readlink "${HOME}/.erlang.d/current" | sed s:.*/::)
        OTP_VERSIONS=($(find ${HOME}/.erlang.d -maxdepth 1 -mindepth 1 -type d -printf %f\\n))
        COUNT=${#OTP_VERSIONS[@]}
        for i in $(seq 0 $((${COUNT} -1))); do
            OUT="$i. ${OTP_VERSIONS[${i}]}"
            if [[ "${CURRENT}" == "${OTP_VERSIONS[${i}]}" ]]; then
                DEFAULT=${i}
                OUT="\e[7m${OUT}\e[27m"
            fi
            echo -e "${OUT}"
        done
        read -p "Choose version ($DEFAULT): " VERSION
        if [[ ! ${VERSION} ]]; then
            VERSION=${DEFAULT}
        fi
        if [[ "${VERSION}" == "${DEFAULT}" ]]; then
            echo "Keeping OTP version ${OTP_VERSIONS[${VERSION}]}"
        else
            ln -sfvT ${HOME}/.erlang.d/${OTP_VERSIONS[${VERSION}]} ${HOME}/.erlang.d/current
            echo "Switched to OTP version ${OTP_VERSIONS[${VERSION}]}"
        fi
    fi
}

alias pulp-admin='docker run --net=host -it --rm -v ${HOME}/.pulp:/root/.pulp -v ${PWD}:/tmp/uploads klarna/pulp-admin'


# Paths
export PATH="${PATH}:${HOME}/klarna/fred/fred_platform/bin:~/klarna/fred/gitrdun/bin"
export PATH="${PATH}:${HOME}/klarna/chef/berksenv"
export PATH="${PATH}:${HOME}/klarna/cloud/orchid/bin"
# Demo remove me!
export PATH="${PATH}:${HOME}/klarna/chef/naked-chef"

# Env
# Make sure to use kernel_poll
export KRED_POLL=true
export IGNORE_FORCE_BACKUP=true
export KRED_SKIP_SUBMODULE_UPDATE=true

export DEV_REPOS=all

function tmux-session {
    if ! pidof tmux > /dev/null
    then
        exec tmux
    fi
    TMUX_SESSIONS=$(tmux list-sessions)
    TMUX_SESSION_COUNT=${#TMUX_SESSIONS[@]}
    echo default $DEFAULT_TMUX_SESSION
    for i in $(seq 0 $((${TMUX_SESSION_COUNT} -1))); do
        echo -e "${TMUX_SESSIONS[${i}]}"
    done
    read -p "Choose session [New]: " TMUX_SESSION
    if [[ -z "${TMUX_SESSION}" ]]; then
        exec tmux
    fi
    tmux attach -t ${TMUX_SESSION}

}

if [[ -z "${TMUX}" ]]
then
    tmux-session
fi
