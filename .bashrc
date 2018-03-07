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
alias du1="du -d 1 -h"
alias v="sf -e vim"
alias yaourt-nodeps="yaourt -Qdtq"
alias yaourt-update="yaourt -Syu --aur"

export PATH=$PATH:~/bin

# The following command is now set in .xprofile. Could be set in other desktop manager rc as well.
# setxkbmap -option -option grp:alt_shift_toggle -option ctrl:swapcaps -option terminate:ctrl_alt_bksp -layout us,ca
