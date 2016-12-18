#!/bin/bash

# create symbolic links to $HOME directory
# ln opts
#   [-s]: create soft link
#   [-f]: if target file already exists, unlink and link it back
#   [-n]: if target file/dir is already a symlink, do not follow it
#   [-v]: verbose
#

ln -sfnv $HOME/Dropbox/dotfiles/.bashrc $HOME/.bashrc
ln -sfnv $HOME/Dropbox/dotfiles/.bash_profile $HOME/.bash_profile
ln -sfnv $HOME/Dropbox/dotfiles/.emacs $HOME/.emacs
ln -sfnv $HOME/Dropbox/dotfiles/.emacs.d $HOME/.emacs.d
ln -sfnv $HOME/Dropbox/dotfiles/.gitconfig $HOME/.gitconfig
cp $HOME/Dropbox/dotfiles/launch_agents/*.plist $HOME/Library/LaunchAgents/
launchctl load $HOME/Library/LaunchAgents/com.mkvenkatesh.brewlist.plist
launchctl load $HOME/Library/LaunchAgents/com.mkvenkatesh.oracletns.plist