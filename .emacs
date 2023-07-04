;(load "~/.emacs.d/custom.el")

(require 'package)
(add-to-list 'package-archives
	     '("melpa" .  "https://melpa.org/packages/") t)
(package-initialize)

;Disable Some UI Stuff - If you need these ui components, remove these
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-screen t)

;List contains all packages installed by user
(setq user-packages (list
		     'exec-path-from-shell
		     'flyspell
		     'kaolin-themes
		     'use-package
		     'lsp-mode
		     'rustic
		     'company
		     'lsp-ui
		     'tuareg
		     'merlin-company
		     'magit
		     'olivetti
		     'slime
		     'treemacs
		     ;'fstar-mode
		     ;'boogie-friends
		     ;'lsp-metals
		     ;'scala-mode
		     ;'gnu-apl-mode
		     'haskell-mode
		     'lsp-haskell
		     ;'lean-mode
		     ))

; Ensure everything installed
(dolist (pkg user-packages)
  (if (package-installed-p pkg) '() (package-install pkg)))
; Use-package config - always defer, for better loading
(setq use-package-always-ensure t
      use-package-always-defer t)

;; Theming
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  )

  (load-theme 'doom-one t)
  (setq doom-themes-treemacs-theme "doom-atom")

; Shell setup [GUI ONLY]
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

; Hide backup files, so they don't clutter file managers
(setq backup-directory-alist `(("." . ,(expand-file-name ".tmp/backups/"
                                                         user-emacs-directory))))
;General UI related setup
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(use-package treemacs
  :ensure t
  :defer t
  :init
  :config
  (setq treemacs-display-in-side-window t)
  :bind
  (:map global-map
	("C-x t t"   . treemacs)
	("C-x t d"   . treemacs-select-directory)
  )
)

;; Magit
(use-package magit)

; Latex Editing 
(setq TeX-PDF-mode t)
(require 'flyspell)
(dolist (hook '(latex-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

; Olivetti is convenient for making latex more readable in buffers
(use-package olivetti)
(global-set-key (kbd "C-c o") 'olivetti-mode)
(add-hook 'org-mode-hook 'olivetti-mode)
(add-hook 'latex-mode-hook 'olivetti-mode)

; Setup company-mode
(use-package company)
(add-hook 'prog-mode-hook 'global-company-mode)

;Setup LSP for programming
(use-package lsp-mode)
(use-package lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(global-set-key (kbd "C-c C-/") 'comment-region) ; hook for vscode-like comment/uncomment. C-u prefix to uncomment.
(global-set-key (kbd "C-c C-q") (lambda () (interactive) (load-file "~/.emacs")))

;Setup hooks for C++, C, Rust, and Scala
(add-hook 'c++-mode-hook 'lsp)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'rust-mode-hook 'lsp)
(add-hook 'scala-mode-hook 'lsp)
(add-hook 'haskell-mode-hook 'lsp)
(setq lsp-haskell-server-path "haskell-language-server-wrapper")

(use-package scala-mode
  :interpreter ("scala" . scala-mode))
(use-package lsp-metals)
(use-package rustic)

; Get syntax highlighting on Arduino files.
(add-to-list 'auto-mode-alist '("\\.ino$" . c++-mode))

;; Ocaml setup
(let ((opam-share (ignore-errors (car (process-lines "opam" "var" "share")))))
 (when (and opam-share (file-directory-p opam-share))
  (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
  (autoload 'merlin-mode "merlin" nil t nil)
  (add-hook 'tuareg-mode-hook #'merlin-mode t)
  (add-hook 'caml-mode-hook #'merlin-mode t)))

(use-package merlin-company)
(add-hook 'tuareg-mode-hook 'merlin-company)
(use-package dune)

; Setup SLIME for working with Common Lisp
(setq inferior-lisp-program "sbcl")

(add-to-list 'auto-mode-alist '("\\.cl$" . slime-mode))
(add-to-list 'auto-mode-alist '("\\.cl$" . lisp-mode))

; Hooks for dafny and f-star
(setq-default fstar-executable "~/.opam/default/bin/fstar.exe")
(setq-default fstar-smt-executable "~/.opam/default/bin/z3")
(setq flycheck-dafny-executable "/usr/bin/dafny")

; Org-mode configuration
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-agenda-files '("~/agenda/"))
(global-set-key (kbd "C-c q") 'org-agenda)
(global-set-key (kbd "C-c e") 'org-capture)
(eval-after-load "org" '(require 'ox-md nil t))

(setq org-capture-templates
      '(("t" "TODO" entry (file "~/agenda/todo.org")
	 "* TODO %?\n")
	) ;TODO - more templates
      )


