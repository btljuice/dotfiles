# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt autocd beep nomatch
unsetopt notify
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/btljuice/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

source ~/.bash_and_zsh_rc
