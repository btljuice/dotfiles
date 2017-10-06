export EDITOR=vim
set -o vi

eval "$(fasd --init auto)"


alias ls="ls --color"
alias ll="ls -l"
alias la="ls -a"
alias lla="ls -la"
alias vo="vim -R"
alias info="info --vi-keys"
alias du="du -h"
alias du0="du -d 0 -h"

export PATH=$PATH:~/bin

# Only activate when x is available
setxkbmap -option -option grp:alt_shift_toggle -option ctrl:swapcaps -option terminate:ctrl_alt_bksp -layout us,ca
