(defvar elpaca-installer-version 0.11)
  (defvar elpaca-directory
    (expand-file-name "elpaca/" user-emacs-directory))
  (defvar elpaca-builds-directory
    (expand-file-name "builds/" elpaca-directory))
  (defvar elpaca-repos-directory
    (expand-file-name "repos/" elpaca-directory))
  (defvar elpaca-order '(elpaca :repo
                          "https://github.com/progfolio/elpaca.git"
                                :ref nil :depth 1 :inherit ignore
                                :files
                                (:defaults "elpaca-test.el"
                                           (:exclude "extensions"))
                                :build
                                (:not elpaca--activate-package)))
  (let*
      ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
         (build
          (expand-file-name "elpaca/" elpaca-builds-directory))
         (order (cdr elpaca-order))
         (default-directory repo))
    (add-to-list 'load-path (if (file-exists-p build) build repo))
    (unless (file-exists-p repo)
      (make-directory repo t)
      (when (<= emacs-major-version 28) (require 'subr-x))
      (condition-case-unless-debug err
          (if-let*
              ((buffer
                (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                    ((zerop (apply #'call-process `("git" nil
                                                    ,buffer t
                                                    "clone"
                                                    ,@(when-let*
                                                          ((depth
                                                            (plist-get
                                                             order
                                                             :depth)))
                                                        (list
                                                         (format
                                                          "--depth=%d"
                                                          depth)
                                                         "--no-single-branch"))
                                                    ,(plist-get
                                                      order :repo)
                                                    ,repo))))
                    ((zerop (call-process "git" nil buffer t
                                          "checkout"
                                          (or
                                           (plist-get order :ref)
                                           "--"))))
                    (emacs
                     (concat invocation-directory invocation-name))
                    ((zerop (call-process emacs nil buffer nil
                                          "-Q" "-L" "." "--batch"
                                          "--eval"
                                          "(byte-recompile-directory \".\" 0 'force)")))
                    ((require 'elpaca))
                    ((elpaca-generate-autoloads "elpaca" repo)))
              (progn
                (message "%s" (buffer-string))
                (kill-buffer buffer))
            (error "%s"
                   (with-current-buffer buffer (buffer-string))))
        ((error) (warn "%s" err)
         (delete-directory repo 'recursive))))
    (unless (require 'elpaca-autoloads nil t)
      (require 'elpaca)
      (elpaca-generate-autoloads "elpaca" repo)
      (let ((load-source-file-function nil))
        (load "./elpaca-autoloads"))))
  (add-hook 'after-init-hook #'elpaca-process-queues)
  (elpaca `(,@elpaca-order))

  ;; (setq package-enable-at-startup nil)

  ;; use-package

  ;; Install a package via the elpaca macro
;; See the "recipes" section of the manual for more details.

;; (elpaca example-package)

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode)
   (setq elpaca-use-package-by-default t))

;;When installing a package used in the init file itself,
;;e.g. a package which adds a use-package key word,
;;use the :wait recipe keyword to block until that package is installed/configured.
;;For example:
;; (use-package general :ensure (:wait t) :demand t)

;; Expands to: (elpaca evil (use-package evil :demand t))
;; (use-package evil :ensure t :demand t)

;;Turns off elpaca-use-package-mode current declaration
;;Note this will cause evaluate the declaration immediately. It is not deferred.
;;Useful for configuring built-in emacs features.
(use-package emacs :ensure
  nil :config (setq ring-bell-function #'ignore))

(use-package all-the-icons
  :ensure
  t
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :hook
  (dired-mode . (lambda () (all-the-icons-dired-mode t))))

(set-face-attribute 'default nil
  :font "JetBrainsMono Nerd Font Mono"
  :height 120
  :weight 'medium)
(set-face-attribute 'variable-pitch nil
  :font "JetBrainsMono Nerd Font Mono"
  :height 120
  :weight 'medium)
(set-face-attribute 'fixed-pitch nil
  :font "JetBrainsMono Nerd Font Mono"
  :height 120
  :weight 'medium)
;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)

;; This sets the default font on all graphical frames created after restarting Emacs.
;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
;; are not right unless I also add this method of setting the default font.
(add-to-list 'default-frame-alist
             '(font . "JetBrainsMono Nerd Font Mono-12"))

;; Uncomment the following line if line spacing needs adjusting.
(setq-default line-spacing 0.12)

(use-package diminish)

(use-package general
    :ensure
    t
    :config
     (general-create-definer leader-def
       :prefix
       "C-c")

   (leader-def 
;; Atalhos de
     arquivo
    "f"  '(:ignore t :which-key "arquivos") ;; Grupo (requer which-key)
    "ff" 'find-file
    "fs" 'save-buffer
    "fr" 'recentf-open-files

    ;; Atalhos de buffer
    "b"  '(:ignore t :which-key "buffers")
    "bb" 'switch-to-buffer
    "bk" 'kill-current-buffer
    "bn" 'next-buffer
    "bp" 'previous-buffer

    ;; Atalhos de janela
    "w"  '(:ignore t :which-key "janelas")
    "w/" 'split-window-right
    "w-" 'split-window-below
    "wd" 'delete-window
    "wo" 'delete-other-windows

    "et" 'eshell

    "tt" 'neotree-toggle

    "//" '(comment-line :wk "Comment lines")
    )

   (general-define-key 
    "<f8>" 'neotree-toggle
    )
  )

(use-package company
  :defer
  2
  :diminish
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (global-company-mode t))

(use-package company-box
  :after
  company
  :diminish
  :hook (company-mode . company-box-mode))

(use-package dashboard
  :ensure
  t 
  :init
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title
        "Emacs Is More Than A Text Editor!")
  (setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  ;;(setq dashboard-startup-banner "/home/dt/.config/emacs/images/emacs-dash.png")  ;; use custom image as banner
  (setq dashboard-center-content nil) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 5)
                          (agenda . 5 )
                          (bookmarks . 3)
                          (projects . 3)
                          (registers . 3)))
  :custom
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book")))
  :config
  (dashboard-setup-startup-hook))

(use-package flycheck
:ensure
t
:defer 1
:diminish
:init (global-flycheck-mode))

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

(menu-bar-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-display-line-numbers-mode t)
(global-visual-line-mode t)

(setq backup-directory-alist '((".*" . "~/.Trash")))

;; espaços ao invés de tabs
(setq-default indent-tabs-mode nil)

;; alerta visual
(setq visible-bell t)

;; espaçamento das bordas laterais
(set-fringe-mode 10)

;; habilita o fechamento de pares
(electric-pair-mode 1)

;; sai do mini buffer com esc
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Rolagem mais suave
(setq mouse-wheel-scroll-amount '(5 ((shift) . 1)) ;; 2 linhas por vez
   mouse-wheel-progressive-speed nil ;; Não acelera a rolagem
   mouse-wheel-follow-mouse 't ;; rola a janela do mouse
;; rola 1 linha com teclado
   scroll-step 5)

;; ajustes para facilitar
(global-unset-key (kbd "C-z")) ;; desabilita o suspend-frame (C-z)
(delete-selection-mode t) ;; o texto digitado substitui a seleção

(global-visual-line-mode 1)

(setq-default cursor-type 'bar)



(use-package toc-org
  :commands
  toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(which-key-mode 1)

(use-package catppuccin-theme
:ensure
t
:config
 (load-theme 'catppuccin t))

(add-to-list 'default-frame-alist '(alpha-background . 90)) ; For all new frames henceforth

(electric-indent-mode -1)
(setq org-edit-src-content-indentation 0)

(require 'org-tempo)

(defun reload-init-file ()
(interactive)
(load-file user-init-file)
(load-file user-init-file))

(use-package sudo-edit
  :config
    (leader-def
      "fu" '(sudo-edit-find-file :wk "Sudo find file")
      "fU" '(sudo-edit :wk "Sudo edit file")))

(use-package rainbow-mode
  :hook 
  ((org-mode prog-mode) . rainbow-mode))

(use-package eshell-syntax-highlighting
  :after
  esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

;; eshell-syntax-highlighting -- adds fish/zsh-like syntax highlighting.
;; eshell-rc-script -- your profile for eshell; like a bashrc for eshell.
;; eshell-aliases-file -- sets an aliases file for the eshell.
  
(setq eshell-rc-script (concat user-emacs-directory "eshell/profile")
      eshell-aliases-file
      (concat user-emacs-directory "eshell/aliases")
      eshell-history-size 5000
      eshell-buffer-maximum-lines 5000
      eshell-hist-ignoredups t
      eshell-scroll-to-bottom-on-input t
      eshell-destroy-buffer-when-process-dies t
      eshell-visual-commands'("bash" "fish" "htop" "ssh" "top" "zsh"))

(use-package vterm
:config
(setq shell-file-name "/bin/fish"
      vterm-max-scrollback 5000))

(use-package vterm-toggle
  :after
  vterm
  :bind ("<f5>" . vterm-toggle)
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-scope 'project)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                     (let ((buffer (get-buffer buffer-or-name)))
                       (with-current-buffer buffer
                         (or (equal major-mode 'vterm-mode)
                             (string-prefix-p vterm-buffer-name
                                              (buffer-name buffer))))))
                  (display-buffer-reuse-window
                   display-buffer-at-bottom)
                  ;;(display-buffer-reuse-window display-buffer-in-direction)
                  ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                  ;;(direction . bottom)
                  ;;(dedicated . t) ;dedicated is supported in emacs27
                  (reusable-frames . visible)
                  (window-height . 0.3))))

(use-package exec-path-from-shell
  :ensure
  t
  :config
  ;; Garante que o PATH e outras variáveis sejam copiadas do shell do sistema
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package doom-modeline
  :ensure
  t
  :init (doom-modeline-mode 1))
  ;; :hook (after-init . doom-modeline-mode))

(use-package projectile
 :config
 (projectile-mode 1))

(use-package lua-mode)

(use-package elpy
:ensure
t
:init
(elpy-enable))

(use-package alchemist
:config
(setq 
   alchemist-mix-command
   "~/.local/share/mise/installs/elixir/1.19.4-otp-28/bin/mix"
   alchemist-execute-command
   "~/.local/share/mise/installs/elixir/1.19.4-otp-28/bin/elixir"
   alchemist-iex-program-name
   "~/.local/share/mise/installs/elixir/1.19.4-otp-28/bin/iex"
  
))

(use-package elisp-autofmt)

(use-package neotree
  :config
  (setq neo-smart-open t
        neo-show-hidden-files t
        neo-window-width 55
        neo-window-fixed-size nil
        inhibit-compacting-font-caches t
        projectile-switch-project-action 'neotree-projectile-action) 
        ;; truncate long file names in neotree
        (add-hook 'neo-after-create-hook
           #'(lambda (_)
               (with-current-buffer (get-buffer neo-buffer-name)
                 (setq truncate-lines t)
                 (setq word-wrap nil)
                 (make-local-variable 'auto-hscroll-mode)
                 (setq auto-hscroll-mode nil)))))
