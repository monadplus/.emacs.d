;;; * Emacs 

(setq user-full-name "Arnau Abella")
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'load-path "~/.emacs.d/modes")

;;; * Package sources

(require 'package)
(setq package-enable-at-startup nil)
(setq 
  package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                     ("org" . "http://orgmode.org/elpa/")
                     ("melpa" . "http://melpa.org/packages/")
                     ("melpa-stable" . "http://stable.melpa.org/packages/"))
  package-archive-priorities '(("melpa" . 1)))

(package-initialize)


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package use-package-chords ; define key-chords in use package declarations
  :ensure key-chord
  :ensure t)

;;; * $PATH

(use-package exec-path-from-shell ; load path from bash
  :ensure t
  :if (and (eq system-type 'darwin) (display-graphic-p))
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "PS1")
  (exec-path-from-shell-copy-env "CELLAR"))

;;; * Appearence

(setq inhibit-startup-message t
      inhibit-splash-screen t
      ring-bell-function 'ignore)

(use-package planet-theme ;
  :ensure t
  :defer t)

(defun switch-theme ()
  "Disable any active themes, then load a new one"
  (interactive)
  (mapcar 'disable-theme custom-enabled-themes)
  (call-interactively 'load-theme))

(setq custom-safe-themes t)
(load-theme 'planet)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (planet)))
 '(custom-safe-themes
   (quote
    ("8ffdc8c66ceeaf7921f4510a70d808f01b303e6b4d177c947b442e80d4228678" "9f65322594fcff32425175b3c141ccd29f0cfb24d87b2ddea34f5d12499bfe31" "5c9bd73de767fa0d0ea71ee2f3ca6fe77261d931c3d4f7cca0734e2a3282f439" default)))
 '(dante-load-flags (quote ("-Wall" "-fno-warn-type-defaults" "+c")))
 '(dante-repl-command-line (quote ("ghci")))
 '(package-selected-packages
   (quote
    (java-imports flycheck-cask popup-imenu lsp-scala tide lsp-ui exec-path-from-shell use-package-chords key-chord markdown-mode yaml-mode restclient smartparens undo-tree dante lsp-mode dumb-jump use-package projectile neotree magit intero highlight-symbol goto-chg dirtree all-the-icons))))

;;; *  Miscelaneous settings 

(setq-default
 inhibit-startup-screen t
 create-lockfiles nil
 column-number-mode t
 scroll-error-top-bottom t
 show-paren-delay 0.5
 use-package-always-ensure t
 sentence-end-double-space nil
 indent-tabs-mode nil ;; Indent with no tabs
 tab-width 4
 c-basic-offset 4
 explicit-shell-file-name "/bin/zsh")

(fset 'yes-or-no-p 'y-or-n-p) ; Use y or n instead of yes and no
(global-auto-revert-mode t) ;; auto-refresh when buffer when disk changes
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8) ; prefer UTF-8

;;; * switch buffer
(defun switch-to-previous-buffer ()
      (interactive)
      (switch-to-buffer (other-buffer (current-buffer) 1)))

;;; * copy-line
(defun copy-line (arg)
    "Copy lines (as many as prefix argument) in the kill ring.
      Ease of use features:
      - Move to start of next line.
      - Appends the copy on sequential calls.
      - Use newline as last char even on the last line of the buffer.
      - If region is active, copy its lines."
    (interactive "p")
    (let ((beg (line-beginning-position))
          (end (line-end-position arg)))
      (when mark-active
        (if (> (point) (mark))
            (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
          (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
      (if (eq last-command 'copy-line)
          (kill-append (buffer-substring beg end) (< end beg))
        (kill-ring-save beg end)))
    (kill-append "\n" nil)
    (beginning-of-line (or (and arg (1+ arg)) 2))
    (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

;;; * smex
(use-package smex
  :ensure t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands))
  :chords ("fk" . smex))

;;; * Backups, autosaves

(let* ((create-dir-if-nonexistent (lambda (dir-name)
                                    (unless (file-exists-p dir-name)
                                      (make-directory dir-name)))))
  (funcall create-dir-if-nonexistent "~/.emacs.d/backups")
  (setq make-backup-files t
        version-control t
        delete-old-versions t
        backup-directory-alist `((".*" . ,"~/.emacs.d/backups"))))

;;; *  Kbds

(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)
(global-set-key (kbd "M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<down>") 'enlarge-window)
(global-set-key (kbd "M-<up>") 'shrink-window)
(global-set-key (kbd "S-<left>") 'backward-word)
(global-set-key (kbd "S-<right>") 'forward-word)
(global-set-key (kbd "C-x f") 'find-file)
(global-set-key "\C-c\C-d" "\C-a\C- \C-n\M-w\C-y")
(global-unset-key "\C-x\C-z") ;; unsed suspend-emacs
(global-set-key (kbd "M-r") 'pop-tag-mark)
(global-set-key (kbd "M-t") 'find-tag)
(global-set-key (kbd "C-b") 'switch-to-buffer)
(global-set-key (kbd "M-b") 'switch-to-previous-buffer)
(global-set-key (kbd "C-l") 'copy-line)
(global-set-key (kbd "M-l") 'kill-whole-line)

;;; * Icons and fonts

(require 'all-the-icons)

;;; * NeoTree 

(require 'neotree)
(setq neo-theme (if (display-graphic-p) 'icons 'arrows)) 
(setq projectile-switch-project-action 'neotree-projectile-action)
(setq neo-smart-open t)

;;; * Projectile

(use-package projectile
  :demand
  :init   (setq projectile-use-git-grep t)
  :config (projectile-global-mode t)
  :bind   (("ESC f" . projectile-find-file)
           ("ESC g" . projectile-grep)))


;;; * Goto Last Change

(use-package goto-chg
  :commands goto-last-change
  ;; complementary to
  ;; C-x r m / C-x r l
  ;; and C-<space> C-<space> / C-u C-<space>
  :bind (("C-x ." . goto-last-change)
         ("C-x ," . goto-last-change-reverse)))

;;; * Highlight symbol

(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :commands highlight-symbol
  :bind ("M-h" . highlight-symbol))

(global-set-key [f1] 'neotree-toggle)
(global-set-key [f2] 'haskell-hoogle)
(global-set-key [f3] 'comment-region)
(global-set-key [f4] 'uncomment-region)

;;; * Opup imenu

(use-package popup-imenu
  :commands popup-imenu
  :bind ("M-i" . popup-imenu))

;;;  * Better undo

(use-package undo-tree
  :ensure t
  ;; undo-tree has these bindings in a local
  ;; keymap only, causing various issues
  :bind (("C-/" . undo-tree-undo)
         ("C-?" . undo-tree-redo)
         ("C-x u" . undo-tree-visualize))
  :config
(global-undo-tree-mode t))

;;;  * Autocompletion

(use-package hippie-exp
  :ensure t
  :chords ("jj" . hippie-expand))

;;;  * Parentheses

(use-package smartparens
  :ensure t
  :config
  (setq sp-highlight-pair-overlay nil) ; Don't highlight current sexp
  (smartparens-global-mode t)
(show-smartparens-global-mode t))

;;;  * Version control

(use-package magit
  :ensure t
  :config
  (setq magit-visit-ref-behavior ; To make 'Enter' check out things in the 'y' panel
      '(create-branch
        checkout-any
        checkout-branch)))


(setq ediff-window-setup-function 'ediff-setup-windows-plain ; Diff in the current frame
      ediff-split-window-function (if (> (frame-width) 150)
                                          'split-window-horizontally
                                        'split-window-vertically))

;;;  * Rest

(use-package restclient
  :ensure t)

;;;  * Json

(use-package json
  :ensure t)

;;;  * Yaml

(use-package yaml-mode
  :ensure t)

;;;  * Markdown

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
:init (setq markdown-command "multimarkdown"))

;;; * Haskell 

(use-package dante
  :ensure t
  :after haskell-mode

  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'dante-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  (add-hook 'dante-mode-hook
    '(lambda () (flycheck-add-next-checker 'haskell-dante
                                           '(warning . haskell-hlint))))
  (add-hook 'haskell-mode-hook 
  (lambda ()
    (linum-mode 1))))

(setq flymake-no-changes-timeout nil)
(setq flymake-start-syntax-check-on-newline nil)
(setq flycheck-check-syntax-automatically '(save mode-enabled))

;;; * Scala

;; (use-package scala-mode
;;   :ensure t)

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package lsp-mode
  :ensure t)

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-scala
  :load-path "~/Documents/lsp-scala"
  :after scala-mode
  :demand t
  ;; Optional - enable lsp-scala automatically in scala files
  :hook (scala-mode . lsp)
  :init (setq lsp-scala-server-command "/usr/local/bin/metals-emacs"))

;;; * Html, CSS, JS

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;;; * Typescript

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save-mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
;;(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; TSX

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; enable typescript-tslint checker
;;(flycheck-add-mode 'typescript-tslint 'web-mode)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
