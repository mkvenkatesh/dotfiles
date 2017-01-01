;;; .emacs --- Venkatesh Mandalapa emacs init file

;;; Commentary:
;; see the README for more details

;;; Code:

;; start emacs server if none is available
(require 'server)
(unless (server-running-p)
  (server-start))


;; add MELPA & Marmalade
(package-initialize)
(add-to-list
 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list
 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))


;; use Solarized-dark as default theme
(load-theme 'solarized-dark t)


;; switch between Solarized dark and light theme
(defun solarized-light ()
  (interactive)
  (load-theme 'solarized-light t))

(defun solarized-dark ()
  (interactive)
  (load-theme 'solarized-dark t))


;; set frame title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))


;; initialize exec-path-from-shell to copy environment variables
;; properly to Emacs windows app
(when (display-graphic-p)
  (exec-path-from-shell-initialize))


;; from emacs-redux. make use of recently opened files list
(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode +1)


;; from emacs-redux. delete currently visited file and buffer.
(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))
(global-set-key (kbd "C-c D")  'delete-file-and-buffer)


;; Run code below only for OS X
(if (eq system-type 'darwin)
	;; use gls from coreutils rather than Mac's ls command for dired mode
	(setq insert-directory-program (executable-find "gls")))


;; add custom-el to load path
(add-to-list 'load-path "~/.emacs.d/custom-el/")


;; additional hooks
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; flyspell configuration - turn on automatic spell check for
;; text/prog modes
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)


;; activate smart-mode-line
(setq sml/no-confirm-load-theme t)
(setq sml/theme 'respectful)
(sml/setup)


;; change 'yes or no' to 'y or n' for any confirmations
(fset 'yes-or-no-p 'y-or-n-p)


;; press 'a' to open dir in same buffer instead of creating a new one
;; when navigating dirs in dired mode
(put 'dired-find-alternate-file 'disabled nil)


;; ivy/swiper customization
(ivy-mode 1)
(require 'flx)
(global-set-key (kbd "C-s") 'counsel-grep-or-swiper)
(global-set-key (kbd "C-r") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "s-SPC") 'ivy-restrict-to-matches)
(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
; Use Enter on a directory to navigate into the directory, not open it
; with dired.
(define-key ivy-minibuffer-map (kbd "<return>") 'ivy-alt-done)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "C-c d") 'counsel-descbinds)
(setq ivy-initial-inputs-alist nil)
(setq ivy-re-builders-alist '((swiper . ivy--regex-plus)
							  (counsel-ag . ivy--regex-plus)
							  (counsel-grep-or-swiper . ivy--regex-plus)
							  (t . ivy--regex-fuzzy)))
(counsel-projectile-on)


;; enable projectile mode
(projectile-mode)

;; imenu-anywhere configuration
(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-.") nil))
(global-set-key (kbd "C-.") #'imenu-anywhere)


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


;; easier window movement commands (default uses SHIFT)
(windmove-default-keybindings)


;; set few other shortcuts
(global-set-key (kbd "C-c z") 'reveal-in-osx-finder)
(global-set-key [f8] 'neotree-toggle)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "s-$") 'ispell-word)
(global-set-key (kbd "s-x") 'tramp-term)
(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)


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
;; enable html yasnippets in web-mode
(add-hook 'web-mode-hook #'(lambda () (yas-activate-extra-mode 'html-mode)))


;; enable tern-mode for js
(add-hook 'js-mode-hook (lambda () (tern-mode t)))


;; js-comint sane setup
(defun inferior-js-mode-hook-setup ()
  (add-hook 'comint-output-filter-functions 'js-comint-process-output))
(add-hook 'inferior-js-mode-hook 'inferior-js-mode-hook-setup t)


;; company mode configuration
(require 'company)
;; add company-restclient to company backends
(add-to-list 'company-backends 'company-restclient)
;; company-tern configuration
(add-to-list 'company-backends 'company-tern)
(setq company-tern-property-marker "")
(setq company-tern-meta-as-single-line t)
;; setup shortcut to see snippet menu using company-mode
(global-set-key (kbd "C-c y") 'company-yasnippet)
;; add company-web-html to company-backends only for web-mode
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
;; add html-tidy flycheck checker for web-mode rather than use
;; handlebars default checker
(eval-after-load 'flycheck
   '(flycheck-add-mode 'html-tidy 'web-mode))


;; enable smooth scrolling
(smooth-scrolling-mode 1)


;; scroll conservatively when using mouse wheel or trackpad
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)


;; enable pbcopy for macosx cliboard interactions in terminal-mode
(when (eq (display-graphic-p) nil)
  (turn-on-pbcopy))


;; enable undo-mode for all modes
(global-undo-tree-mode)


;; expand-region shortcut
(require 'expand-region)
(global-set-key (kbd "M-2") 'er/expand-region)


;; setup multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "s-c") 'mc/edit-lines)
(global-set-key (kbd "s-n") 'mc/mark-next-like-this)
(global-set-key (kbd "s-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "s-a") 'mc/mark-all-like-this)


;; sql mode configuration
;; truncate long lines on the result set in SQLi buffer
(add-hook 'sql-interactive-mode-hook (lambda () (toggle-truncate-lines t)))
;; from https://truongtx.me/2014/08/23/setup-emacs-as-an-sql-database-client/
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
;; rename SQL buffers automatically
(add-hook 'sql-interactive-mode-hook 'sql-rename-buffer)


;; enable restclient-mode for .http files
(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))


;; ace-window customization - easily move between windows
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "s-w") 'ace-window)


;; avy configuration
(global-set-key (kbd "s-f") 'avy-goto-word-or-subword-1)


;; from emacs-redux. edit files as sudo using tramp
(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))
(global-set-key (kbd "C-x C-r") 'sudo-edit)


;; multi-term setup
(require 'multi-term)
;; cycle through existing or create new multi-term
(global-set-key (kbd "C-c t") 'multi-term-next)
(global-set-key (kbd "C-c T") 'multi-term) ; create a new one
(global-set-key (kbd "<f10>") 'multi-term-dedicated-toggle)
;; switch between line-mode and char-mode in multi-term
(define-key term-mode-map (kbd "C-j") 'term-char-mode)
(define-key term-raw-map (kbd "C-j") 'term-line-mode)
;; remove line highlight while in term-mode
(add-hook 'term-mode-hook (lambda ()
							(setq-local global-hl-line-mode
										nil)))
;; disable yasnippets for term-mode so tab completion works
(add-hook 'term-mode-hook (lambda()
                (yas-minor-mode -1)))


;; jekyll mode configuration
(add-to-list 'auto-mode-alist '("\\.md$" . jekyll-markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . jekyll-markdown-mode))


;; mmm-mode configuration for markdown files
(require 'mmm-mode)
(setq mmm-global-mode 'maybe)

;; from http://jblevins.org/log/mmm
(defun my-mmm-markdown-auto-class (lang &optional submode)
  "Define a mmm-mode class for LANG in `markdown-mode' using SUBMODE.
If SUBMODE is not provided, use `LANG-mode' by default."
  (let ((class (intern (concat "markdown-" lang)))
        (submode (or submode (intern (concat lang "-mode"))))
        (front (concat "^```" lang "[\n\r]+"))
        (back "^```"))
    (mmm-add-classes (list (list class :submode submode :front front :back back)))
    (mmm-add-mode-ext-class 'markdown-mode nil class)))

;; Mode names that derive directly from the language name
(mapc 'my-mmm-markdown-auto-class
      '("awk" "bibtex" "c" "cpp" "css" "html" "js" "latex" "lisp" "makefile"
        "markdown" "python" "r" "ruby" "sql" "stata" "xml"))

;; Mode names that differ from the language name
(my-mmm-markdown-auto-class "perl" 'cperl-mode)
(my-mmm-markdown-auto-class "shell" 'shell-script-mode)


;;


;; M-x customize auto-generated code below
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-sources (quote ("~/.emacs.d/.authinfo.gpg")))
 '(avy-all-windows nil)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
 '(bookmark-save-flag 0)
 '(column-number-mode t)
 '(company-dabbrev-downcase nil)
 '(company-idle-delay 0)
 '(company-tooltip-limit 20)
 '(confirm-kill-emacs (quote y-or-n-p))
 '(confirm-nonexistent-file-or-buffer nil)
 '(counsel-locate-cmd (quote counsel-locate-cmd-mdfind))
 '(custom-safe-themes t)
 '(delete-old-versions t)
 '(delete-selection-mode t)
 '(electric-pair-mode t)
 '(electric-pair-preserve-balance nil)
 '(epa-pinentry-mode (quote loopback))
 '(flycheck-indication-mode nil)
 '(global-company-mode t)
 '(global-hl-line-mode t)
 '(ibuffer-default-sorting-mode (quote major-mode))
 '(ibuffer-display-summary nil)
 '(ibuffer-expert t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(ivy-extra-directories (quote ("./")))
 '(ivy-use-virtual-buffers t)
 '(kept-new-versions 6)
 '(linum-format " %d ")
 '(magit-completing-read-function (quote ivy-completing-read))
 '(mmm-parse-when-idle t)
 '(multi-term-dedicated-select-after-open-p t)
 '(neo-smart-open t)
 '(ns-alternate-modifier (quote super))
 '(ns-command-modifier (quote meta))
 '(ns-function-modifier (quote hyper))
 '(ns-pop-up-frames nil)
 '(package-selected-packages
   (quote
	(yaml-mode jekyll-modes mmm-mode ag counsel-projectile projectile imenu-anywhere flx counsel swiper tramp-term multi-term backup-walker web-mode web-beautify unfill undo-tree solarized-theme smooth-scrolling smart-mode-line reveal-in-osx-finder pbcopy neotree multiple-cursors markdown-mode magit js-comint flycheck expand-region exec-path-from-shell elpy company-web company-tern company-restclient ace-window ace-jump-mode)))
 '(projectile-completion-system (quote ivy))
 '(ring-bell-function (quote ignore))
 '(show-paren-mode t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(tramp-default-method "ssh")
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
 '(default ((t (:height 140 :family "Source Code Pro"))))
 '(web-mode-current-column-highlight-face ((t (:background "white"))))
 '(web-mode-current-element-highlight-face ((t (:background "white")))))
