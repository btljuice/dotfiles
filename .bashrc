set -o vi

source ~/.common-bash-zsh.rc

# Command that Bash executes just before displaying a prompt
export PROMPT_COMMAND=set_prompt

set_prompt() {
    # Capture exit code of last command
    local ex=$?

    #----------------------------------------------------------------------------#
    # Bash text colour specification:  \e[<STYLE>;<COLOUR>m
    # (Note: \e = \033 (oct) = \x1b (hex) = 27 (dec) = "Escape")
    # Styles:  0=normal, 1=bold, 2=dimmed, 4=underlined, 7=highlighted
    # Colours: 31=red, 32=green, 33=yellow, 34=blue, 35=purple, 36=cyan, 37=white
    #----------------------------------------------------------------------------#
    local color='\e[1;32m'
    local reset='\e[0m'

    # Set prompt content
    PS1="$\[$reset\] "
    # Add git branch to the prompt
    GIT_BRANCH=`git branch 2>/dev/null | grep '^*' | colrm 1 2`
    PS1="$GIT_BRANCH) $PS1 "
    GIT_REPO=`git remote get-url origin`
    GIT_REPO=`basename -- $GIT_REPO`
    GIT_REPO="${GIT_REPO%.*}"
    PS1="($GIT_REPO:$PS1"
    # Add current directory to the prompt
    PS1="\W $PS1 "
    # If exit code of last command is non-zero, prepend this code to the prompt
    PS1="[$ex] $PS1"
    # Set colour of prompt
    PS1="\[$color\]$PS1"
}
export PS1

[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion

# The following command is now set in .xprofile. Could be set in other desktop manager rc as well.
# setxkbmap -option -option grp:alt_shift_toggle -option ctrl:swapcaps -option terminate:ctrl_alt_bksp -layout us,ca
