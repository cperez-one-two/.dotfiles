#
# Uhoh's Zoomer Shell
#
# git info for prompt
autoload -U colors && colors
autoload -Uz vcs_info
precmd() {
	vcs_info
}
setopt prompt_subst

# simple
PROMPT='%(?..%F{1}%B✗ %?%b %f)%B%F{14}%n@%m%f%b %B%F{5}%1~%f%b${vcs_info_msg_0_} %F{4}%#%f '
zstyle ':vcs_info:git:*' formats ' %B%F{3}%b%f'
#PROMPT='%(?..%F{1}%B✗ %?%b %f)%b%B%F{14}%n%f%b at %B%F{2}%m%f%b in %B%F{5}%3~%f%b ${vcs_info_msg_0_}%b
#%F{4}%#%f '
#zstyle ':vcs_info:git:*' formats 'on %B%F{3}%b%f'

# Pipes
#PROMPT='%(?.%F{8}┏━━%f.%F{8}┏━━[%f%F{9}✗ %?%f%F{8}]━━%f)%F{8}[%f%F{#abb2bf} %n%f%F{8}]━━[%f%F{13} %3~%f%F{8}]%f${vcs_info_msg_0_}
#%F{8}┗%f%F{12}→%f '
#zstyle ':vcs_info:git:*' formats '%F{8}━━[%f%F{3} %b%f%F{8}]%f'
# icons
#PROMPT='%B%(?..%F{9} ✗ %? %f)%F{13}  %3~ %f${vcs_info_msg_0_}
#%F{12} >%f%b '
#zstyle ':vcs_info:git:*' formats '%F{3}  %b%f'

setopt autocd                   # cd into directory just by typing it
stty stop undef                 # Disable C-s to freeze terminal
unsetopt BEEP                   # Disable that HORRIBLE beep

# History settings
HISTFILE=~/.cache/zsh/history
HISTSIZE=100000
SAVEHIST=100000

# Source aliases
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc"

# emacs mode
bindkey -e
export KEYTIMEOUT=1

# Auto/tab complete
autoload -Uz compinit
_comp_options+=(globdots)               # Include hidden files.
fpath+=~/.config/zsh/completions/_fnm   # fnm completions
compinit


# Keybinds
#bindkey '^[[P' delete-char                      # Delete key fix
#bindkey -s '^f' 'cd "$(dirname "$(fzf)")"\n'    # fzf

# Change cursor shape for different vi modes.
#function zle-keymap-select () {
#    case $KEYMAP in
#        vicmd) echo -ne '\e[2 q';;      # Solid Block
#        #viins|main) echo -ne '\e[1 q';; # Blinking Block
#        viins|main) echo -ne '\e[3 q';; # Beam
#    esac
#}
#zle -N zle-keymap-select
#zle-line-init() {
#    echo -ne "\e[3 q"
#    #echo -ne "\e[1 q"
#}
#zle -N zle-line-init
#echo -ne '\e[1 q'                       # Use beam shape cursor on startup.
#preexec() { echo -ne '\e[3 q' ;}        # Use beam shape cursor for each new prompt.

# fnm
eval "`fnm env`"


# direnv
#eval "$(direnv hook zsh)"

# Plugins
# zsh-fast-syntax-highlighting
source ~/.config/zsh/plugins/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh
# zsh-z :: Fast cd'ing into folders
source ~/.config/zsh/plugins/zsh-z/zsh-z.plugin.zsh

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
[[ -s "/home/uhoh/.local/sdkman/bin/sdkman-init.sh" ]] && source "/home/uhoh/.local/sdkman/bin/sdkman-init.sh"
