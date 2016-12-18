# dotfiles

All dotfiles for my environment - bash, emacs etc.

dotfiles folder is stored in `$HOME/Dropbox/` so it synchronizes to
Dropbox and can be used in multiple personal machines without cloning
or fetching/pulling with git.

## Installation

```bash
git clone https://github.com/mkvenkatesh/dotfiles.git $HOME/Dropbox/dotfiles
cd $HOME/Dropbox/dotfiles
chmod ug+x setup.sh
./setup.sh
```

Setup.sh symlinks all the dotfiles from Dropbox to the $HOME
folder. It also copies `plist` files from `launch_agents` folder in
Dropbox to `$HOME/Library/LaunchAgents` and loads it.

## Emacs

See [.emacs](https://github.com/mkvenkatesh/dotfiles/blob/master/.emacs)

### ELPA Packages
* magit
* markdown-mode
* neotree
* reveal-in-osx-finder
* smart-mode-line
* smart-mode-line-powerline-theme
* smex
