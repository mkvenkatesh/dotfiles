# Set NVM (Node Version Manager)
export NVM_DIR="$HOME/.nvm"
. "/usr/local/opt/nvm/nvm.sh"

# Update PATH
export PATH=/opt/oracle/instantclient_12_1:$PATH

# Emacs customization
export ALTERNATE_EDITOR="alternate-emacs"
export EDITOR="emacsclient -t"
export VISUAL="emacsclient -n"
alias kill_emacs_server='emacsclient -e "(kill-emacs)"'
alias e='emacsclient -n'
alias et='emacsclient -t'
alias er="SUDO_EDITOR=\"emacsclient -n\" sudo -e"

# Set environment vars for Oracle
export ORACLE_HOME=/opt/oracle/instantclient_12_1
export LD_LIBRARY_PATH=$ORACLE_HOME

# bash aliases
alias ls='ls -GFh'
alias ll='ls -l'
alias lsa='ls -a'
alias lla='ls -al'
alias pyserve='python -m SimpleHTTPServer 8000'
alias casuvpn='/opt/cisco/anyconnect/bin/vpn connect sslvpn.asu.edu; osascript -e '\''tell application "Finder" to set desktop picture to POSIX file "/Users/vmandala/Documents/spacex-vpn.jpg"'\'''
alias dasuvpn='/opt/cisco/anyconnect/bin/vpn disconnect; osascript -e '\''tell application "Finder" to set desktop picture to POSIX file "/Users/vmandala/Documents/spacex.jpg"'\'''
alias pgstart='pg_ctl -D /usr/local/var/postgres start' # postgres start
alias pgstop='pg_ctl -D /usr/local/var/postgres stop' # postgres stop
alias clrhistory='history -cw' # clear shell history

# RDP AppleScript with parameters to invoke Microsoft Remote Desktop
rdp_asu() {
    osascript /Users/vmandala/Dropbox\ \(Personal\)/code/applescript/rdpasu.scpt $1
}
alias rdpasu=rdp_asu

# Set Bash Prompt Format: http://xta.github.io/HalloweenBash/
export PS1="\[\033[36m\]\u\[\033[m\]@\[\033[32m\]\h:\[\033[33;1m\]\w\[\033[m\]\$ "

# load rbenv init automatically
eval "$(rbenv init -)"

# init bash-completion
[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion

# pyenv configuration
if which pyenv > /dev/null; then eval "$(pyenv init -)"; fi

# enable aws bash completion
complete -C '/usr/local/bin/aws_completer' aws
