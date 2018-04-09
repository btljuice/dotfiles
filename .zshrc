source /usr/share/zsh/share/antigen.zsh

antigen init ~/.antigenrc

HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt autocd beep nomatch
unsetopt notify

bindkey -v

zstyle ':completion:*' rehash true # Rehashes $PATH automatically

source ~/.common-bash-zsh.rc
