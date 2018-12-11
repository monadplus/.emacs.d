(require 'package)
;; must be before load custom theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
package-archive-priorities '(("melpa-stable" . 1)))


(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (oceanic)))
 '(custom-safe-themes
   (quote
    ("9f65322594fcff32425175b3c141ccd29f0cfb24d87b2ddea34f5d12499bfe31" "5c9bd73de767fa0d0ea71ee2f3ca6fe77261d931c3d4f7cca0734e2a3282f439" default)))
 '(dante-repl-command-line (quote ("ghci")))
 '(package-selected-packages
   (quote
    (dante lsp-mode dumb-jump use-package projectile neotree magit intero highlight-symbol goto-chg ensime dirtree all-the-icons))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'use-package)
(require 'magit)
(require 'all-the-icons)
(require 'neotree)
(setq neo-theme (if (display-graphic-p) 'icons 'arrows)) 
(setq projectile-switch-project-action 'neotree-projectile-action)

;; global variables
(setq
 inhibit-startup-screen t
 create-lockfiles nil
 make-backup-files nil
 column-number-mode t
 scroll-error-top-bottom t
 show-paren-delay 0.5
 use-package-always-ensure t
 sentence-end-double-space nil)

;; auto-refresh when buffer when disk changes
(global-auto-revert-mode t)

;; buffer local variables
(setq-default
 indent-tabs-mode nil
 tab-width 4
 c-basic-offset 4)

;; modes
(electric-indent-mode 0)

;; required by ensime installation
(add-to-list 'exec-path "/usr/local/bin")

;; ensime
;;(use-package ensime
;;  :ensure t
;;  :pin melpa-stable)

;; ;; intero
;; (package-install 'intero)
;; (add-hook 'haskell-mode-hook 'intero-mode)
;; (put 'dired-find-alternate-file 'disabled nil)

;; Key bindings
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
(global-set-key (kbd "ESC r") 'pop-tag-mark)
(global-set-key (kbd "ESC t") 'find-tag)
(define-key global-map (kbd "C-/") 'undo)

(setq-default explicit-shell-file-name "/bin/zsh")

(use-package projectile
  :demand
  :init   (setq projectile-use-git-grep t)
  :config (projectile-global-mode t)
  :bind   (("ESC f" . projectile-find-file)
           ("ESC g" . projectile-grep)))

(use-package goto-chg
  :commands goto-last-change
  ;; complementary to
  ;; C-x r m / C-x r l
  ;; and C-<space> C-<space> / C-u C-<space>
  :bind (("C-x ." . goto-last-change)
         ("C-x ," . goto-last-change-reverse)))

;; highlight all symbols in a file that match the current one
(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :commands highlight-symbol
  :bind ("ESC h" . highlight-symbol))

(global-set-key [f1] 'neotree-toggle)
(global-set-key [f2] 'haskell-hoogle)

;; (require 'lsp-mode)
;; (use-package sbt-mode
;;   :commands sbt-start sbt-command
;;   :config
;;   ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
;;   ;; allows using SPACE when in the minibuffer
;;   (substitute-key-definition
;;    'minibuffer-complete-word
;;    'self-insert-command
;;    minibuffer-local-completion-map))

;; (add-to-list 'load-path "~/Documents/lsp-scala")
;; (require 'lsp-scala)
;; ;; load lsp-scala on every scala file
;; ;; (add-hook scala-mode-hook #'lsp-scala-enable)

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

