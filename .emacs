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


;; flyspell configuration
; turn on automatic spell check for text/prog modes
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

;; function to copy the current buffer path
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

;; web-mode activation
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; enable yasnippet
(yas-global-mode 1)
; enable html yasnippets in web-mode
(add-hook 'web-mode-hook #'(lambda () (yas-activate-extra-mode 'html-mode)))

;; enable tern-mode for js
(add-hook 'js-mode-hook (lambda () (tern-mode t)))

;; js-comint sane setup
(defun inferior-js-mode-hook-setup ()
  (add-hook 'comint-output-filter-functions 'js-comint-process-output))
(add-hook 'inferior-js-mode-hook 'inferior-js-mode-hook-setup t)

;; company mode configuration
; company-tern configuration
(require 'company)
(add-to-list 'company-backends 'company-tern)
(setq company-tern-property-marker "")
(setq company-tern-meta-as-single-line t)
; setup shortcut to see snippet menu using company-mode
(global-set-key (kbd "C-c y") 'company-yasnippet)
; add company-web-html to company-backends only for web-mode
(add-hook 'web-mode-hook
	  (lambda ()
	    (set (make-local-variable 'company-backends) '(company-web-html))
	    (company-mode t)))

;; web-beautify setup
(eval-after-load 'js
  '(define-key js-mode-map (kbd "C-c b") 'web-beautify-js))
(eval-after-load 'json-mode
  '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))
(eval-after-load 'sgml-mode
  '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))
(eval-after-load 'web-mode
  '(define-key web-mode-map (kbd "C-c b") 'web-beautify-html))
(eval-after-load 'css-mode
  '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))

;; define front-end for anbt-sql-formatter
;; from http://www.emacswiki.org/emacs/SqlBeautify
(defun sql-beautify-region (beg end)
  "Beautify SQL in region between BEG and END."
  (interactive "r")
  (save-excursion
    (shell-command-on-region beg end "anbt-sql-formatter" nil t)))
(defun sql-beautify-buffer ()
 "Beautify SQL in buffer."
 (interactive)
 (sql-beautify-region (point-min) (point-max)))
(add-hook 'sql-mode-hook '(lambda () (define-key sql-mode-map (kbd "C-c b") 'sql-beautify-buffer)))

;; enable elpa for Python IDE support
(elpy-enable)

;; turn on flycheck (on-the-fly syntax checker) for all compatible
;; programming modes
(add-hook 'after-init-hook #'global-flycheck-mode)
; add html-tidy flycheck checker for web-mode rather than use
; handlebars default checker
(eval-after-load 'flycheck
   '(flycheck-add-mode 'html-tidy 'web-mode))

;; enable smooth scrolling
(smooth-scrolling-mode 1)

;; enable mouse scrolling https://www.iterm2.com/faq.html
(require 'mwheel)
(require 'mouse)
(xterm-mouse-mode t)
(mouse-wheel-mode t)
(global-set-key [mouse-4] 'previous-line)
(global-set-key [mouse-5] 'next-line)

;; enable pbcopy for macosx cliboard interactions
(turn-on-pbcopy)

;; enable undo-mode for all modes
(global-undo-tree-mode)

;; expand-region shortcut
(require 'expand-region)
(global-set-key (kbd "M-2") 'er/expand-region)

;; setup multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-x M-l") 'mc/edit-lines)
(global-set-key (kbd "C-x M->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-x M-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-x M-a") 'mc/mark-all-like-this)

;; sql mode configuration
; truncate long lines on the result set in SQLi buffer
(add-hook 'sql-interactive-mode-hook (lambda () (toggle-truncate-lines t)))
; from https://truongtx.me/2014/08/23/setup-emacs-as-an-sql-database-client/
(require 'sql-connections)
(defun my-sql-connect (product connection)
  ;; load the password
  (require 'sql-passwords "sql-passwords.el.gpg")
  ;; update the password to the sql-connection-alist
  (let ((connection-info (assoc connection sql-connection-alist))
        (sql-password (car (last (assoc connection my-sql-password)))))
    (delete sql-password connection-info)
    (nconc connection-info `((sql-password ,sql-password)))
    (setq sql-connection-alist (assq-delete-all connection sql-connection-alist))
    (add-to-list 'sql-connection-alist connection-info))
  ;; connect to database
  (setq sql-product product)
  (sql-connect connection))
; rename SQL buffers automatically
(add-hook 'sql-interactive-mode-hook 'sql-rename-buffer)

;;

;; M-x customize auto-generated code below
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
 '(bookmark-save-flag 0)
 '(column-number-mode t)
 '(company-dabbrev-downcase nil)
 '(company-idle-delay 0)
 '(company-tooltip-limit 20)
 '(confirm-nonexistent-file-or-buffer nil)
 '(delete-old-versions t)
 '(delete-selection-mode t)
 '(electric-pair-mode t)
 '(electric-pair-preserve-balance nil)
 '(epa-pinentry-mode (quote loopback))
 '(global-company-mode t)
 '(ido-auto-merge-work-directories-length -1)
 '(ido-create-new-buffer (quote always))
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(inhibit-startup-screen t)
 '(initial-major-mode (quote markdown-mode))
 '(initial-scratch-message nil)
 '(kept-new-versions 6)
 '(linum-format " %d ")
 '(major-mode (quote markdown-mode))
 '(neo-smart-open t)
 '(package-selected-packages
   (quote
	(js-comint multiple-cursors expand-region undo-tree pbcopy smooth-scrolling unfill flycheck elpy company-tern web-beautify company-web company yasnippet web-mode magit smex smart-mode-line-powerline-theme reveal-in-osx-finder neotree markdown-mode)))
 '(show-paren-mode t)
 '(tab-width 4)
 '(version-control t)
 '(web-mode-enable-auto-closing t)
 '(web-mode-enable-auto-expanding t)
 '(web-mode-enable-auto-opening t)
 '(web-mode-enable-auto-pairing t)
 '(web-mode-enable-auto-quoting t)
 '(web-mode-enable-block-face t)
 '(web-mode-enable-css-colorization t)
 '(web-mode-enable-current-column-highlight nil)
 '(web-mode-enable-current-element-highlight t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(web-mode-current-column-highlight-face ((t (:background "white"))))
 '(web-mode-current-element-highlight-face ((t (:background "white")))))
