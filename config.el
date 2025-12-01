(setq inhibit-startup-message t)

(tool-bar-mode -1)
(menu-bar-mode 1)
(scroll-bar-mode -1)
(tooltip-mode nil)

(global-display-line-numbers-mode t)
(column-number-mode t)
(global-hl-line-mode t)

;; alerta visual
(setq visible-bell t)

;; espaçamento das bordas laterais
(set-fringe-mode 10)

;; espaços ao invés de tabs
(setq-default indent-tabs-mode nil)

;; habilita o fechamento de pares
(electric-pair-mode 1)

;; sai do mini buffer com esc
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; ajustes para facilitar
(global-unset-key (kbd "C-z")) ;; desabilita o suspend-frame (C-z)
(delete-selection-mode t) ;; o texto digitado substitui a seleção

;; Rolagem mais suave
(setq mouse-wheel-scroll-amount '(5 ((shift) . 1)) ;; 2 linhas por vez
	  mouse-wheel-progressive-speed nil ;; Não acelera a rolagem
	  mouse-wheel-follow-mouse 't ;; rola a janela do mouse
	  scroll-step 5 ;; rola 1 linha com teclado
	  )

;; Quebras de linha
(global-visual-line-mode 1)

;; Organizando backups
(setq backup-directory-alist `(("." . "~/.emacs-backups-files")))

(setq-default cursor-type 'bar)

(set-face-attribute 'default nil :font "Hasklug Nerd Font Mono" :height 150)

;; Função para criar um novo buffer
(defun silver-new-buffer ()
  "cria um novo buffer `sem nome`"
  (interactive)
  (let (silver/buf (generate-new-buffer "sem-nome")))
  (switch-to-buffer silver/buf)
  (funcall initial-major-mode)
  (setq buffer-offer-save t)
  silver/buf)

;; modo inicial
(setq initial-major-mode 'prog-mode)
(setq initial-buffer-choice 'silver-new-buffer)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(setq package-enable-at-startup nil)

(straight-use-package 'el-patch)

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

(use-package rainbow-mode
  :diminish
  :hook org-mode prog-mode)

(use-package diminish)

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package all-the-icons-completion
:after (marginalia all-the-icons)
:hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
:init
(all-the-icons-completion-mode))

;; Enable Vertico.
(use-package vertico
  :custom
   (vertico-scroll-margin 0) ;; Different scroll margin
   (vertico-count 20) ;; Show more candidates
   (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
   (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Emacs minibuffer configurations.
(use-package emacs
  :custom
  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode t)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

(use-package consult)

(use-package flycheck
:ensure t
:init (global-flycheck-mode))

(use-package company
  :defer 2
  :diminish
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (global-company-mode t)) 

(use-package company-box
  :after company
  :diminish
  :hook (company-mode . company-box-mode))

(use-package doom-themes
:ensure t
:custom
;; Global settings (defaults)
(doom-themes-enable-bold t)   ; if nil, bold is universally disabled
(doom-themes-enable-italic t) ; if nil, italics is universally disabled
;; for treemacs users
(doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
:config
(load-theme 'doom-tokyo-night t)

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)
;; Enable custom neotree theme (nerd-icons must be installed!)
(doom-themes-neotree-config)
;; or for treemacs users
(doom-themes-treemacs-config)
;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config))

(use-package nerd-icons
;; :custom
;; The Nerd Font you want to use in GUI
;; "Symbols Nerd Font Mono" is the default and is recommended
;; but you can use any other Nerd Font if you want
;; (nerd-icons-font-family "Symbols Nerd Font Mono")
)

(use-package doom-modeline
:ensure t
:hook (after-init . doom-modeline-mode))

(use-package magit)

(use-package diminish)

(use-package projectile
:diminish projectile-mode
:config
(projectile-mode))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are helm user
;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key
    :config
    (which-key-mode))

(add-to-list 'load-path "~/.emacs.d/neotree")
(require 'neotree)
(setq neo-smart-open t)
(global-set-key [f8] 'neotree-toggle)

;; adiciona o projecttile com o neotree
  (defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))

 (global-set-key [f7] 'neotree-project-dir)








