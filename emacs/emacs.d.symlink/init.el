;; Global key bindings
(global-set-key (kbd "C-h")	'backward-delete-char)
(global-set-key (kbd "C-+")	'text-scale-increase)
(global-set-key (kbd "C--")	'text-scale-decrease)
(global-set-key (kbd "C-x a r") 'align-regexp)
(global-set-key (kbd "C-o")	'dabbrev-expand)
(global-set-key (kbd "C-x l") 'goto-line)

(setq dabbrev-case-fold-search nil)

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell 
      (replace-regexp-in-string "[[:space:]\n]*$" "" 
        (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(set-exec-path-from-shell-PATH)

(defun preghc ()
  (let* ((ghc-version (read-from-minibuffer "preghc: " ""))
         (preghc (concat "preghc " ghc-version " 2>/dev/null"))
         (path-from-shell
          (replace-regexp-in-string "[[:space:]\n]*$" ""
           (shell-command-to-string
            (format "$SHELL --login -c 'source ~/.zshrc; %s; echo $PATH'" preghc)))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

;; Local site-lisp and requirements
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(add-to-list 'load-path "~/.emacs.d/auto-install")
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/auto-install/")
(auto-install-update-emacswiki-package-name t)

;; Internationalization
(setenv "LC_TIME" "C")
(coding-system-put 'utf-8 'category 'utf-8)
(set-language-info
  "Japanese"
  'coding-priority (cons 'utf-8
                         (get-language-info "Japanese" 'coding-priority)))
(set-language-environment "Japanese")
(set-buffer-file-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; Startup settings
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(xterm-mouse-mode 1)
(iswitchb-mode t)
(menu-bar-mode t)
(tool-bar-mode -1)

;; Display settings
(show-paren-mode t)
(setq show-paren-style 'mixed)
(transient-mark-mode t)
(setq highlight-nonselected-windows t)
(setq next-line-add-newlines nil)
(setq tab-width 4)
;; XFS fonts and colors
(cond (window-system
  (cond ((string-match "apple-darwin" system-configuration)
         (set-default-font "Monaco-11"))
        (t
         (set-default-font "Consolas-11")))
  (set-fontset-font (frame-parameter nil 'font)
		    'japanese-jisx0208
		    '("Hiragino Kaku Gothic Pro" . "unicode-bmp"))
  (set-foreground-color "AntiqueWhite1")
  (set-background-color "gray18")
  (set-cursor-color "Gray")))

(require 'font-lock)
(global-font-lock-mode t)
(font-lock-add-keywords 'haskell-mode
  '(("\\<\\(FIXME\\):" 1 font-lock-warning-face t)
    ("\\<\\(TODO\\):" 1 font-lock-warning-face t)
    ("\\<\\(XXX\\):" 1 font-lock-warning-face t)))

(setq-default indent-tabs-mode nil)
(setq completion-ignore t)
(setq read-file-name-completion-ignore-case t)

;;
;; Package configurations
;;
;; Helm (was anything)
(add-to-list 'load-path "~/.emacs.d/helm")
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "C-x b") 'helm-buffers-list)

;; popwin.el
(require 'popwin)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; cua-mode
(cua-selection-mode t)

;; Git integration
(add-to-list 'load-path "~/.emacs.d/egg")
(require 'egg)
(load "~/.emacs.d/egg/egg-grep")

;; Haskell
(load "~/.emacs.d/haskell-mode/haskell-site-file")

;; Haskell main editing mode key bindings.
(defun haskell-hook ()
  (preghc)

  ;; ghc-mod
  (autoload 'ghc-init "ghc" nil t)
  (ghc-init)
  (flymake-mode)

  ;; Use simple indentation.
  (turn-on-haskell-indent)

  (cond
   ((string-match "/workspace/" buffer-file-name)
    (custom-set-variables
     '(haskell-indent-after-keywords (quote (("where" 4 0) ("of" 4) ("do" 4) ("in" 4 0) ("{" 4) "if" "then" "else" "let")))
     '(haskell-indent-offset 4)
     '(haskell-indent-thenelse 4)))
   (t
    (custom-set-variables
     '(haskell-indent-after-keywords (quote (("where" 2 0) ("of" 2) ("do" 2) ("in" 2 0) ("{" 2) "if" "then" "else" "let")))
     '(haskell-indent-offset 2)
     '(haskell-indent-thenelse 2))))
  ;; Use cabal-dev for the GHCi session. Ensures our dependencies are in scope.
  (custom-set-variables
    '(haskell-notify-p t)
    '(haskell-process-path-cabal-dev "~/.cabal/bin/cabal-dev")
    '(haskell-process-type (quote cabal-dev))
    '(haskell-tags-on-save t))
  )

;; Useful to have these keybindings for .cabal files, too.
(defun haskell-cabal-hook ()
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)
  (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-cabal-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch))

(add-hook 'haskell-mode-hook 'haskell-hook)

;; Hamlet
(require 'hamlet-mode)

;; GNU Global
(autoload 'gtags-mode "gtags" "" t)

;; JavaScript
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$|\\.julius$" . js2-mode))

;; CoffeeScript
(autoload 'coffee-mode "coffee-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
;; http://d.hatena.ne.jp/antipop/20110508/1304838383
(setq flymake-coffeescript-err-line-patterns
  '(("\\(Error: In \\([^,]+\\), .+ on line \\([0-9]+\\).*\\)" 2 3 nil 1)))

(defconst flymake-allowed-coffeescript-file-name-masks
  '(("\\.coffee$" flymake-coffeescript-init)))

(defun flymake-coffeescript-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "coffee" (list local-file))))

(defun flymake-coffeescript-load ()
  (interactive)
  (defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
    (setq flymake-check-was-interrupted t))
  (ad-activate 'flymake-post-syntax-check)
  (setq flymake-allowed-file-name-masks
        (append flymake-allowed-file-name-masks
                flymake-allowed-coffeescript-file-name-masks))
  (setq flymake-err-line-patterns flymake-coffeescript-err-line-patterns)
  (flymake-mode t))

(add-hook 'coffee-mode-hook 'flymake-coffeescript-load)

;; auto-complete.el
(when (require 'auto-complete nil t)
  (require 'auto-complete-config)
  (ac-config-default)
  (global-auto-complete-mode t)
  (define-key ac-complete-mode-map "\C-n" 'ac-next)
  (define-key ac-complete-mode-map "\C-p" 'ac-previous)
  (set-default 'ac-sources
	       '(ac-source-abbrev
                 ac-source-words-in-buffer))
  (add-hook 'emacs-lisp-mode-hook
    (lambda ()
      (setq ac-sources
            '(ac-source-abbrev
              ac-source-words-in-buffer
              ac-source-symbols))))
  (setq ac-modes
      (append ac-modes
              '(haskell-mode literate-haskell-mode)))
  )

;; shell-mode
(add-hook 'shell-mode-hook
  (lambda ()
    (set-buffer-process-coding-system 'utf-8 'utf-8)))

;; markdown-mode
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.markdown$\\|\\.mkdn$\\|\\.md$" . markdown-mode) auto-mode-alist))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.

 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "google-chrome")
 '(coffee-tab-width 2)
 '(column-number-mode t)
 '(epa-file-name-regexp "\\.\\(asc\\|gpg\\)\\(~\\|\\.~[0-9]+~\\)?\\'")
 '(markdown-css-path "/home/maoe/.emacs.d/github.css")
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(size-indication-mode t))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
