;; Global key bindings
(global-set-key (kbd "C-h")	'backward-delete-char)
(global-set-key (kbd "C-+")	'text-scale-increase)
(global-set-key (kbd "C--")	'text-scale-decrease)
(global-set-key (kbd "C-x a r") 'align-regexp)
(global-set-key (kbd "C-o")	'dabbrev-expand)
(global-set-key (kbd "C-x l") 'goto-line)

(setq dabbrev-case-fold-search nil)
(add-to-list 'exec-path "/Users/maoe/bin")
(add-to-list 'exec-path "/Users/maoe/Library/Haskell/bin")
(add-to-list 'exec-path "/home/maoe/.cabal/bin")

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
  (set-default-font "Consolas-12")
  (set-fontset-font (frame-parameter nil 'font)
		    'japanese-jisx0208
		    '("Hiragino Kaku Gothic Pro" . "unicode-bmp"))
  (require 'color-theme)
  (color-theme-billw)
  ))

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
(require 'anything-startup)

;; popwin.el
(require 'popwin)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Git integration
(add-to-list 'load-path "~/.emacs.d/egg")
(require 'egg)

;; Haskell
(load "~/.emacs.d/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'haskell-hook)
(add-hook 'haskell-cabal-mode-hook 'haskell-cabal-hook)

;; Haskell main editing mode key bindings.
(defun haskell-hook ()
  ;; ghc-mod
  (autoload 'ghc-init "ghc" nil t)
  (ghc-init)
  (flymake-mode)

  ;; Use simple indentation.
  (turn-on-haskell-indent)
  ; (define-key haskell-mode-map (kbd "<return>") 'haskell-simple-indent-newline-same-col)
  ; (define-key haskell-mode-map (kbd "C-<return>") 'haskell-simple-indent-newline-indent)

  ;; ;; Load the current file (and make a session if not already made).
  ;; (define-key haskell-mode-map [?\C-c ?\C-l] 'haskell-process-load-file)
  ;; (define-key haskell-mode-map [f5] 'haskell-process-load-file)

  ;; ;; Switch to the REPL.
  ;; (define-key haskell-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch)
  ;; ;; “Bring” the REPL, hiding all other windows apart from the source
  ;; ;; and the REPL.
  ;; (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)

  ;; ;; Build the Cabal project.
  ;; (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  ;; ;; Interactively choose the Cabal command to run.
  ;; (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)

  ;; ;; Get the type and info of the symbol at point, print it in the
  ;; ;; message buffer.
  ;; (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  ;; (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)

  ;; ;; Contextually do clever things on the space key, in particular:
  ;; ;;   1. Complete imports, letting you choose the module name.
  ;; ;;   2. Show the type of the symbol after the space.
  ;; (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)

  ;; ;; Jump to the imports. Keep tapping to jump between import
  ;; ;; groups. C-u f8 to jump back again.
  ;; (define-key haskell-mode-map [f8] 'haskell-navigate-imports)

  ;; ;; Jump to the definition of the current symbol.
  ;; (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find)

  ;; ;; Save the current buffer and generate etags (a TAGS file) for the
  ;; ;; whole project.
  ;; (define-key haskell-mode-map (kbd "C-x C-s") 'haskell-mode-save-buffer-and-tags)

  ;; ;; Indent the below lines on columns after the current column.
  ;; (define-key haskell-mode-map (kbd "C-<right>")
  ;;   (lambda ()
  ;;     (interactive)
  ;;     (haskell-move-nested 1)))
  ;; ;; Same as above but backwards.
  ;; (define-key haskell-mode-map (kbd "C-<left>")
  ;;   (lambda ()
  ;;     (interactive)
  ;;     (haskell-move-nested -1)))
  )

;; Useful to have these keybindings for .cabal files, too.
(defun haskell-cabal-hook ()
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)
  (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-cabal-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch))

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

  ;; Use cabal-dev for the GHCi session. Ensures our dependencies are in scope.
 '(haskell-process-type 'cabal-dev)

 '(coffee-tab-width 2)
 '(column-number-mode t)
 '(haskell-indent-after-keywords (quote (("where" 4 0) ("of" 4) ("do" 4) ("in" 4 0) ("{" 4) "if" "then" "else" "let")))
 '(haskell-indent-offset 4)
 '(haskell-indent-thenelse 4)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(size-indication-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
