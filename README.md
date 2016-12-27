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

* `brew install markdown` to install markdown pre-processor which is
  used in `markdown-mode` for compiling markdown files.

* `npm -g install js-beautify` to install js-beautify npm module used
  by `web-beautify` emacs package for beautifying HTML, CSS, JS and
  JSON.

* `gem install anbt-sql-formatter` to install the ruby version of
  `anbt-sql-formatter` to beautify SQL queries.

* `npm -g install tern` to get tern js for emacs-JS integration.

* `flycheck` configuration
  * `npm -g install eslint jslint eslint-plugin-json` for linting PY
    and JS files. Run `eslint --init` to define `.eslintrc.json` for
    your project or home directory so that the `javascript-eslint`
    flycheck checker is enabled in emacs. Specify `json` plugin in
    `.eslintrc` to use `eslint` to lint JSON files.
  * `gem install mdl` to install markdown linting tool
  * `npm -g install csslint` to install css linting tool
  * `brew install tidy-html5` to install html linting tool for `web-mode`
  * `gem install sqlint` to install SQL linting tool

* `elpy` emacs package requires the following python modules installed;
  * `pip install jedi` for auto-completion. No need for `company-jedi`
    emacs package since `elpy` comes with company auto-completion
    using `jedi`
  * `pip install flake8` for code checks
  * `pip install importmagic` for automatic imports
  * `pip install autopep8` for automatic PEP8 formatting
  * `pip install yapf` for code formatting

* For Emacs25 and Mac OSX, we need to follow proper instructions to
  set `gnupg` with emacs. Follow
  this [StackOverflow Link](http://tinyurl.com/z7osezq). This is
  required for emacs to properly encrypt/decrypt `.gpg`
  files. Otherwise I'm getting an error in emacs when decrypting
  `.gpg` files: `epa-file--find-file-not-found-function: Opening input
  file: Decryption failed,`

### ELPA Packages

* ace-jump-mode
* company
* company-restclient
* company-tern
* company-web
* elpy
* expand-region
* flycheck
* js-comint
* magit
* markdown-mode
* multiple-cursors
* neotree
* pbcopy
* restclient
* reveal-in-osx-finder
* smart-mode-line
* smart-mode-line-powerline-theme
* smooth-scrolling
* smex
* tern
* undo-tree
* unfill
* web-beautify
* web-mode
* yasnippet
