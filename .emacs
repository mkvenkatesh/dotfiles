;;; .emacs --- Venkatesh Mandalapa emacs init file

;;; Commentary:
;; see the README for more details

;;; Code:
;; add MELPA & Marmalade
(package-initialize)
(add-to-list
 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list
 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; add custom-el to load path
(add-to-list 'load-path "~/.emacs.d/custom-el/")

;; activate smart-mode-line
(setq sml/no-confirm-load-theme t)
(setq sml/theme 'light-powerline)
(sml/setup)

;; turn on automatic spell check for text/prog modes
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; change 'yes or no' to 'y or n' for any confirmations
(fset 'yes-or-no-p 'y-or-n-p)

;; press 'a' to open dir in same buffer instead of creating a new one
;; when navigating dirs in dired mode
(put 'dired-find-alternate-file 'disabled nil)

;; C-x C-c will prompt for a response instead of closing completely
(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed."
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
      (save-buffers-kill-terminal)
    (message "Canceled exit")))
(global-set-key (kbd "C-x C-c") 'ask-before-closing)

;; from emacsredux
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

;; function to copy the current buffer path (C-c p)
(defun copy-buffer-path ()
  "Copy buffer's full path to kill ring."
  (interactive)
  (when buffer-file-name
    (kill-new (file-truename buffer-file-name))
	(message "Buffer path copied to clipboard.")))
(global-set-key (kbd "C-c p") 'copy-buffer-path)

;; easier window movement commands (default uses SHIFT)
(windmove-default-keybindings)

;; set few other shortcuts
(global-set-key (kbd "C-c z") 'reveal-in-osx-finder)
(global-set-key (kbd "C-c n") 'neotree-toggle)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-x g") 'magit-status)



;; http://emacs.stackexchange.com/questions/10900/copy-text-from-emacs-to-os-x-clipboard
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;; https://www.iterm2.com/faq.html
(require 'mwheel)
(require 'mouse)
(xterm-mouse-mode t)
(mouse-wheel-mode t)
(global-set-key [mouse-4] 'previous-line)
(global-set-key [mouse-5] 'next-line)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(neo-smart-open t)
 '(package-selected-packages
   (quote
    (magit smex smart-mode-line-powerline-theme reveal-in-osx-finder neotree markdown-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
