#*********************************************************
# DO NOT ADD ANYTHING TO DO THIS FILE. Use .bashrc instead
#*********************************************************

if [ -f ~/.bashrc ]; then
   source ~/.bashrc
fi

# iterm2 shell integration script
test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"
