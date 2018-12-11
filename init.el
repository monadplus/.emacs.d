;;; * Emacs 

(setq user-full-name "Arnau Abella")

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-to-list 'exec-path "/usr/local/bin") ;; ensime installation

;;; * Package sources

(require 'package)
(setq package-enable-at-startup nil)
(setq 
  package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                     ("org" . "http://orgmode.org/elpa/")
                     ("melpa" . "http://melpa.org/packages/")
                     ("melpa-stable" . "http://stable.melpa.org/packages/"))
  package-archive-priorities '(("melpa-stable" . 1)))

(package-initialize)

(custom-set-variables
 '(custom-enabled-themes (quote (oceanic)))
 '(custom-safe-themes
   (quote
    ("9f65322594fcff32425175b3c141ccd29f0cfb24d87b2ddea34f5d12499bfe31" "5c9bd73de767fa0d0ea71ee2f3ca6fe77261d931c3d4f7cca0734e2a3282f439" default)))
 '(dante-repl-command-line (quote ("ghci")))
 '(package-selected-packages
   (quote
    (use-package-chords key-chord markdown-mode yaml-mode restclient smartparens undo-tree dante lsp-mode dumb-jump use-package projectile neotree magit intero highlight-symbol goto-chg ensime dirtree all-the-icons))))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package use-package-chords ; define key-chords in use package declarations
  :ensure key-chord
  :ensure t)

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

;;; * Backups, autosaves

(let* ((create-dir-if-nonexistent (lambda (dir-name)
                                    (unless (file-exists-p dir-name)
                                      (make-directory dir-name)))))
  (funcall create-dir-if-nonexistent "~/.emacs-backups")
  (setq make-backup-files t
        version-control t
        delete-old-versions t
        backup-directory-alist `((".*" . ,"~/.emacs-backups"))))


(let* ((full-path (lambda (dir-name)
                    (expand-file-name dir-name user-emacs-directory)))
       (create-dir-if-nonexistent (lambda (dir-name)
                                    (unless (file-exists-p dir-name)
                                      (make-directory dir-name))))
       (backup-dir (funcall full-path "backups"))
       (save-file-dir (funcall full-path "autosaves"))
       (desktop-dir (funcall full-path "desktop-saves")))
  (mapcar create-dir-if-nonexistent `(,backup-dir ,save-file-dir ,desktop-dir))
  (setq make-backup-files t
        version-control t
        delete-old-versions t
        backup-directory-alist `((".*" . ,backup-dir)))
  (setq auto-save-file-name-transforms `((".*" ,save-file-dir t)))
  (use-package desktop
    :init
    (setq desktop-path `(,desktop-dir))
(desktop-save-mode t)))

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
(global-unset-key "\C-x\C-z") ;; suspend-emacs
(global-set-key (kbd "M-r") 'pop-tag-mark)
(global-set-key (kbd "M-t") 'find-tag)

;;; * Icons and fonts

(require 'all-the-icons)

;;; * NeoTree 

(require 'neotree)
(setq neo-theme (if (display-graphic-p) 'icons 'arrows)) 
(setq projectile-switch-project-action 'neotree-projectile-action)

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

;;;  * Scala

(use-package ensime
  :ensure t
  :pin melpa-stable)

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
                '(warning . haskell-hlint)))))

(setq flymake-no-changes-timeout nil)
(setq flymake-start-syntax-check-on-newline nil)
(setq flycheck-check-syntax-automatically '(save mode-enabled))
