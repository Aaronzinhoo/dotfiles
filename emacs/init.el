;;; -*- lexical-binding: t -*-
;;; package --- Summary
;;; Commentary:
;;;init.el --- Emacs configuration

;;; Code:

;; startup defaults
(setq user-full-name "Aaron Gonzales")
(setq user-init-file "~/.emacs.d/init.el")
(setq user-emacs-directory "~/.config/emacs")
(defvar home-directory (expand-file-name "~/.config/emacs"))
(defvar backup-dir (concat home-directory "/backups"))
(defvar autosave-dir (concat home-directory "/autosave"))
(defvar file-name-handler-alist-old file-name-handler-alist)
(defconst my/wsl (not (null (string-match "Linux.*Microsoft" (shell-command-to-string "uname -a")))))
;; font
(add-to-list 'default-frame-alist '(font . "-SRC-Hack-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"))
;; more defaults
(setq package-enable-at-startup nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      auto-window-vscroll nil
      global-auto-revert-mode t
      ad-redefinition-action 'accept
      calendar-latitude 33.916403
      calendar-longitude -118.352575
      create-lockfiles nil
      select-enable-clipboard t
      inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
;; enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                dashboard-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
(setq byte-compile-warnings '(cl-functions))
;; make dirs for saving and backing up
(if (not (file-exists-p backup-dir))
    (make-directory backup-dir))
(if (not (file-exists-p autosave-dir))
    (make-directory autosave-dir))
(setq make-backup-files t
      backup-by-copying t ;; safest method to backup
      delete-old-versions t ;; delete excess backups
      delete-by-moving-to-trash t
      kept-old-versions 0
      kept-new-versions 10
      auto-save-default t
      backup-directory-alist `((".*" . ,(concat user-emacs-directory "/backups"))))
;; we will call `blink-matching-open` ourselves...
(remove-hook 'post-self-insert-hook
             #'blink-paren-post-self-insert-function)
;; improve startup performance
;; disable double buffering if on Windows
(if (and (getenv "PATH") (string-match-p "Windows" (getenv "PATH")))
    (setq default-frame-alist
          (append default-frame-alist '((inhibit-double-buffering . t)))))

;;; custom functions
(defun my-minibuffer-exit-hook ()
  "Set the garbage can threshold back to default value."
  (setq gc-cons-threshold 800000))
(defcustom ccm-vpos-init '(round (window-text-height) 2)
  "This is the screen line position where the cursor initially stays."
  :group 'centered-cursor
  :tag "Vertical cursor position"
  :type '(choice (const :tag "Center" (round (window-text-height) 2))
                 (const :tag "Golden ratio" (round (* 21 (window-text-height)) 34))
                 (integer :tag "Lines from top" :value 10)
                 (const :tag "2 Lines above center" (- (round (window-text-height) 2) 2))))
(defun post-func-recenter (&rest args)
  "Recenter display after func using ARGS as input."
  (recenter))
(defun pop-local-mark-ring ()
  "Move cursor to last mark position of current buffer.
Call this repeatedly will cycle all positions in `mark-ring'.
URL `http://ergoemacs.org/emacs/emacs_jump_to_previous_position.html'
  Version 2016-04-04"
  (interactive)
  (set-mark-command t))
(defun split-and-follow-horizontally ()
  "Split window horizontally and follow with the previous buffer open."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1)
  (previous-buffer))
(defun split-and-follow-vertically ()
  "Split window vertically and follow with the previous buffer open."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1)
  (previous-buffer))
(defun display-line-overlay+ (pos str &optional face)
  "Display line at POS as STR with FACE.  FACE defaults to inheriting from default and highlight."
  (let ((ol (save-excursion
              (goto-char pos)
              (make-overlay (line-beginning-position)
                            (line-end-position)))))
    (overlay-put ol 'display str)
    (overlay-put ol 'face
                 (or face '(:inherit default :inherit highlight)))
    ol))

(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)
(toggle-frame-maximized)

;;; Random useful code
;; overlay to help display where other paren is unobtrusively
(let ((ov nil)) ; keep track of the overlay
  (advice-add
   #'show-paren-function
   :after
   (defun show-paren--off-screen+ (&rest _args)
     "Display matching line for off-screen paren."
     (when (overlayp ov)
       (delete-overlay ov))
     ;; check if it's appropriate to show match info,
     ;; see `blink-paren-post-self-insert-function'
     (when (and (overlay-buffer show-paren--overlay)
                (not (or cursor-in-echo-area
                         executing-kbd-macro
                         noninteractive
                         (minibufferp)
                         this-command))
                (and (not (bobp))
                     (memq (char-syntax (char-before)) '(?\) ?\$)))
                (= 1 (logand 1 (- (point)
                                  (save-excursion
                                    (forward-char -1)
                                    (skip-syntax-backward "/\\")
                                    (point))))))
       ;; rebind `minibuffer-message' called by
       ;; `blink-matching-open' to handle the overlay display
       (cl-letf (((symbol-function #'minibuffer-message)
                  (lambda (msg &rest args)
                    (let ((msg (apply #'format-message msg args)))
                      (setq ov (display-line-overlay+
                                (window-start) msg ))))))
         (blink-matching-open))))))
;; load packages and repos
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" home-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously  "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
                                     'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq use-package-always-demand t)

;;; Quick Defaults
(setq-default
 ;; make indent commands use space only (never tab character)
 indent-tabs-mode nil
 tab-width 4
 tab-stop-list (number-sequence 4 120 4)
 scroll-preserve-screen-position t
 scroll-conservatively 10000)
(setq ring-bell-function 'never)
(column-number-mode t) ;; enable column numbers globally
(global-visual-line-mode t) ;; cause lines to wrap
(scroll-bar-mode -1) ;;remove the scroll bar
(put 'erase-buffer 'disabled nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(defalias 'yes-or-no-p 'y-or-n-p)
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
;;; Packages
(use-package s :straight t)
(if (version< emacs-version "27.1")
    (use-package cl))
(use-package gh :straight t)
(use-package async :straight t)
(use-package f :straight t)
(use-package pcre2el :straight t)
(use-package bind-key :straight t)
(use-package dash
  :config
  (dash-enable-font-lock))
(use-package hl-line
  :straight nil
  :hook (prog-mode . hl-line-mode))
;; org-noter/pdf-tools dependency
(use-package tablist)
(use-package tramp
  :straight nil
  :custom
  (tramp-verbose 10)
  (tramp-debug-buffer t)
  (tramp-default-method "ssh"))
(use-package helpful
  :custom
  (help-window-select t)
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))
(use-package server
  :straight nil
  :init
  (server-start))
(use-package no-littering
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))
(use-package url)
(use-package xref
  :straight t)
(use-package frame-local
  :straight t)
(use-package compdef
  :straight t)
(use-package delight
  :defer t)
(use-package beginend
  :defer 2
  :hook (after-init . beginend-global-mode)
  :config
  (dolist (mode (cons 'beginend-global-mode (mapcar #'cdr beginend-modes)))
    (diminish mode)))
;; show matching parens by highlighting parens
(use-package paren
  :straight nil
  :hook (prog-mode . show-paren-mode)
  :custom
  (show-paren-style 'paren)
  (show-paren-delay 0.03)
  (show-paren-highlight-openparen t)
  (show-paren-when-point-inside-paren nil)
  (show-paren-when-point-in-periphery t)
  :config
  (setq blink-matching-paren 'show))
;; only use agency when windows detected
(use-package ssh-agency
  :if (string-equal system-type "windows-nt"))
(use-package xclip
  :straight t
  :init
  (defun wsl-copy (start end)
    "Copy currently selected text to the Windows clipboard"
    (interactive "r")
    (let ((default-directory "/mnt/c/"))
      (shell-command-on-region start end "clip.exe")))
  (defun wsl-paste ()
    "Paste contents of Windows clipboard to buffer"
    (interactive)
    (let ((coding-system-for-read 'dos)
          (default-directory "/mnt/c/" ))
      (insert (shell-command-to-string
               "powershell.exe -command 'Get-Clipboard'"))))
  :config
  (xclip-mode t)
  (global-set-key (kbd "C-c C-w") 'wsl-copy)
  (global-set-key (kbd "C-c C-y") 'wsl-paste))
;; sets up emacs process with keychain environment variables
(use-package keychain-environment
  :straight t
  :config
  (keychain-refresh-environment))
;; preview line for goto-line
(use-package goto-line-preview
  :commands (goto-line-preview)
  :config
  (global-set-key [remap goto-line] 'goto-line-preview))
(use-package benchmark-init
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))
;; TODO: once add projectile, have this hook to projectile

;;; CONTROL VERSION UTILS
(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode))
  :config
  (diff-hl-margin-mode t)
  (diff-hl-flydiff-mode t)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
(use-package git-timemachine
  :defer t
  :commands (git-timemachine))
(use-package hl-todo
  :config
  (global-hl-todo-mode))
;; easily fix conflicts
(use-package hydra)
(use-package smerge-mode
  :straight nil
  :config
  (defhydra smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (smerge-hydra/body)))))
(use-package magit-todos
  :hook (magit-status-mode . magit-todos-mode))
(use-package magit
  :commands (magit-status)
  :diminish
  :bind ("M-s" . 'magit-status)
  :custom
  (magit-completing-read-function 'ivy-completing-read)
  :config
  (add-hook 'after-save-hook 'magit-after-save-refresh-status))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package better-defaults
  :defer t)
(use-package grep
  :defer t)
(use-package wgrep
  :custom
  (wgrep-auto-save-buffer t))
;; Ripgrep
(use-package rg
  :commands (rg rg-dwim)
  :config
  (global-set-key (kbd "C-M-g") 'rg)
  (global-set-key (kbd "C-M-d") 'rg-dwim))
(use-package hungry-delete
  :straight t
  :config
  (global-hungry-delete-mode))
(use-package powerline
  :straight t
  :init
  (powerline-vc 'center))
(use-package dired
  :straight nil
  :hook ((dired-mode . dired-collapse-mode)
         (dired-mode . hl-line-mode)
         (dired-mode . all-the-icons-dired-mode))
  :custom
  (dired-listing-switches "-lXGh --group-directories-first"
                          dired-dwim-target t)
  (dired-auto-revert-buffer t)
  :config
  (use-package dired-single)
  (use-package dired-collapse))
(use-package dired-narrow
  :bind (("C-c C-n" . dired-narrow)))
(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              ("k" . dired-subtree-remove)
              ("<tab>" . dired-subtree-toggle)
              ("<backtab>" . dired-subtree-cycle)))
;; font-locking colors for dired
(use-package diredfl
  :after dired
  :config
  (diredfl-global-mode 1))
(use-package dired-recent
  :config
  (dired-recent-mode  1))
(use-package recentf
  :custom
  (recentf-exclude '("~$" "/tmp/" "/ssh:" "/sudo:" "/sftp:"))
  (recentf-max-menu-items 50)
  (recentf-max-saved-items 50)
  :config
  (recentf-mode 1))
(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)
         ("C-c I" . crux-find-user-init-file))
  :init
  (defun aaronzinho-delete-line ()
    "Delete from current position to end of line without pushing to `kill-ring'."
    (interactive)
    (delete-region (point) (line-end-position)))
  (defun aaronzinho-delete-whole-line ()
    "Delete whole line without pushing to kill-ring."
    (interactive)
    (delete-region (line-beginning-position) (line-end-position))
    (delete-blank-lines))
  (defun crux-smart-delete-line ()
    "Kill to the end of the line and kill whole line on the next call."
    (interactive)
    (let ((orig-point (point)))
      (move-end-of-line 1)
      (if (= orig-point (point))
          (aaronzinho-delete-whole-line)
        (goto-char orig-point)
        (aaronzinho-delete-line))))
  :config
  (global-set-key (kbd "C-k") 'crux-smart-delete-line))

;;; WINDOW CONTROL
(global-set-key (kbd "C-M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-<down>") 'shrink-window)
(global-set-key (kbd "C-M-<up>") 'enlarge-window)
(use-package winner
  :straight nil
  :config
  (winner-mode 1))
(use-package ace-window
  :commands ace-window
  :bind ("M-o" . ace-window)
  :custom
  (aw-ignore-current t)
  (aw-dispatch-always t)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package page-break-lines
  :defer t)
(use-package dashboard
  :custom
  (dashboard-set-init-info t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-banner-logo-title "Welcome to your Emacs Dashboard")
  (dashboard-items '((recents  . 5)
                     (projects . 5)
                     (bookmarks . 5)
                     (agenda . 5)))
  ;; Set the banner
  ;; Value can be
  ;; 'official which displays the official emacs logo
  ;; 'logo which displays an alternative emacs logo
  ;; 1, 2 or 3 which displays one of the text banners
  ;; "path/to/your/image.png" or "path/to/your/text.txt" which displays whatever image/text you would prefer
  (dashboard-startup-banner "~/.emacs.d/dashboard-images/rei_ayanami_render.png")
  ;; Content is not centered by default. To center, set
  (dashboard-center-content t)
  :config
  (dashboard-setup-startup-hook))
(use-package diminish
  :straight t)
(use-package beacon
  :straight t
  :diminish
  :config
  (setq beacon-color "#111FFF")
  (beacon-mode 1))
(use-package which-key
  :straight t
  :diminish
  :config
  (which-key-mode t))
(use-package default-text-scale
  :defer 2
  :bind (("C--" . text-scale-decrease)
         ("C-=" . text-scale-increase)))
(use-package eldoc
  :diminish eldoc-mode)
(use-package flycheck
  :diminish
  :commands flycheck-mode
  :hook (prog-mode . flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint c/c++-clang c/c++-cppcheck c/c++-gcc)))
  (flycheck-add-mode 'json-jsonlint 'json-mode)
  (flycheck-add-mode 'typescript-tslint 'typescript-mode)
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode))
(use-package aggressive-indent
  :straight t
  :diminish
  :config
  (global-aggressive-indent-mode 1)
  (append aggressive-indent-excluded-modes '( web-mode html-mode python-mode)))
;; use to highlight more characters with each use
(use-package expand-region
  :bind (("M-2" . er/expand-region)
         ("C-(" . er/mark-outside-pairs))
  :init
  (defun er/add-rjsx-mode-expansions ()
    (make-variable-buffer-local 'er/try-expand-list)
    (setq er/try-expand-list (append
                              er/try-expand-list
                              '(er/mark-html-attribute
                                er/mark-inner-tag
                                er/mark-outer-tag))))
  :config
  (delete-selection-mode 1)
  (er/enable-mode-expansions 'rjsx-mode 'er/add-rjsx-mode-expansions))
(use-package all-the-icons
  :straight t)
(use-package all-the-icons-dired
  :defer t
  :diminish)
(use-package emojify
  :if (display-graphic-p)
  :hook (after-init . global-emojify-mode))
(use-package yasnippet
  :defer t
  :diminish yas-minor-mode
  :commands yas-minor-mode
  :config
  (yas-reload-all))
(use-package yasnippet-snippets
  :defer t)
(use-package lsp-ivy)
(use-package lsp-mode
  :hook (((c-mode        ; clangd
           c++-mode  ; clangd
           java-mode      ; eclipse-jdtls
           go-mode
           sql-mode
           ) . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . yas-minor-mode))
  :custom
  (lsp-enable-indentation nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-prefer-flymake nil)
  (lsp-enable-symbol-highlighting t)
  (lsp-signature-auto-activate nil)
  (lsp-completion-enable t)
  :init
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook 'lsp-format-buffer)
    (add-hook 'before-save-hook 'lsp-organize-imports)
    (setq lsp-gopls-staticcheck t)
    (setq lsp-eldoc-render-all t)
    (setq lsp-gopls-complete-unimported t))
  :config
  (setq read-process-output-max (* 1024 1024)) ;;1MB
  (add-hook 'go-mode-hook 'lsp-go-install-save-hooks))
(use-package lsp-ui
  :commands lsp-ui-mode
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  :custom
  (lsp-ui-peek-enable t)
  (lsp-ui-doc-enable nil))
(use-package company-box
  :after company
  :diminish
  :hook (company-mode . company-box-mode)
  :init
  (setq company-box-enable-icon (display-graphic-p))
  :config
  (setq company-box-backends-colors nil))
(use-package company-posframe
  :after company
  :diminish company-posframe-mode
  :init (company-posframe-mode t)
  :config
  (setq company-posframe-show-indicator nil)
  (setq company-posframe-show-metadata nil))
(use-package company
  :defer 1
  :diminish company-mode
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-common-or-cycle)
        ("<backtab>" . company-select-previous))
  :init
  (defun company-preview-if-not-tng-frontend (command)
    "`company-preview-frontend', but not when tng is active."
    (unless (and (eq command 'post-command)
                 company-selection-changed
                 (memq 'company-tng-frontend company-frontends))
      (company-preview-frontend command)))
  (setq company-idle-delay 0.1
        company-echo-delay 0 ;; remove annoying blinking
        company-tooltip-flip-when-above t
        company-tooltip-limit 15
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-show-numbers nil
        company-minimum-prefix-length 1
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-transformers '(company-sort-by-backend-importance
                               company-sort-prefer-same-case-prefix
                               company-sort-by-occurrence))
  ;; (setq company-auto-complete #'my-company-visible-and-explicit-action-p)
  (setq company-frontends
        '(
          company-preview-if-not-tng-frontend
          company-pseudo-tooltip-unless-just-one-frontend
          company-preview-if-just-one-frontend
          company-echo-metadata-frontend))
  ;; -----------------------------------------------------------------
  :config
  (global-company-mode t))
(use-package company-quickhelp
  :after company
  :init
  (company-quickhelp-mode t)
  (setq company-quickhelp-delay 0.1))
;; use if only on terminal
(use-package company-quickhelp-terminal
  :if (not (display-graphic-p))
  :straight t)
(use-package company-shell
  :after company
  :config
  (add-to-list 'company-backends '(company-shell company-shell-env company-files)))
(use-package company-jedi
  :commands (jedi:goto-definition jedi-mode company-jedi)
  :bind (:map jedi-mode-map
              ("M-d" . jedi:goto-definition)
              ("M-b" . jedi:goto-definition-pop-marker)))
(use-package company-org-block
  :straight (:type git :host github :repo "aaronzinhoo/company-org-block" :branch "master"))
(use-package smex
  :straight t)
;; IF NEW MACHINE USE M-x all-the-icons-install-fonts
;; should load ivy and swiper automatically
(use-package flx  ;; Improves sorting for fuzzy-matched results
  :defer t
  :init
  (setq ivy-flx-limit 10000))
(use-package counsel
  :diminish (ivy-mode counsel-mode)
  :bind* (("M-x" . counsel-M-x)
          ("C-x b" . counsel-switch-buffer)
          ("C-s" . counsel-grep-or-swiper)
          ("C-x C-f" . counsel-find-file)
          ("C-x C-r" . counsel-recentf)
          ("C-c r" . ivy-resume)
          ("C-c m" . counsel-imenu)
          ("C-r" . counsel-rg)
          ("M-t" .  swiper-thing-at-point)
          :map ivy-minibuffer-map
          ("C-c o" . ivy-occur)
          ("M-i" . nil)
          ("C-j" . ivy-immediate-done))
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :custom
  (ivy-wrap t)
  (ivy-initial-inputs-alist nil)
  (swiper-action-recenter t)
  (enable-recursive-minibuffers t)
  (ivy-extra-directories nil)
  (ivy-use-virtual-buffers t)
  (ivy-count-format "%d/%d ")
  (ivy-display-style 'fancy)
  :config
  (ivy-set-occur 'counsel-rg 'counsel-grep-occur)
  (ivy-set-occur 'counsel-projectile-rg 'counsel-git-grep-occur))
(use-package counsel-projectile
  :config
  (counsel-projectile-mode t))
;; load before ivy-rich for better performance
(use-package all-the-icons-ivy-rich
  :hook (ivy-mode . all-the-icons-ivy-rich-mode))
(use-package ivy-rich
  :hook (all-the-icons-ivy-rich-mode . ivy-rich-mode)
  :init
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (defun ivy-rich-switch-buffer-icon (candidate)
    (with-current-buffer
        (get-buffer candidate)
      (let ((icon (all-the-icons-icon-for-mode major-mode)))
        (if (symbolp icon)
            (all-the-icons-icon-for-mode 'fundamental-mode)
          icon))))
  :custom
  (ivy-rich-parse-remote-buffer nil)
  :config
  ;; ;; All the icon support to ivy-rich
  (setq ivy-rich-display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-switch-buffer-icon (:width 2))
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand)))))
  (setq ivy-rich-path-style 'abbrev)
  (ivy-rich-mode 1))
(use-package prescient
  :straight t)
(use-package ivy-prescient
  :after (prescient)
  :config
  (setq ivy-prescient-enable-sorting t)
  (setq ivy-prescient-enable-filtering t))
(use-package company-prescient
  :after (company prescient)
  :config
  (company-prescient-mode t))
(use-package ag
  :defer 3)
(use-package avy
  :bind* ("M-SPC" . avy-goto-char)
  :config
  (advice-add 'avy-goto-char :after 'post-func-recenter))
(use-package electric
  :config
  (electric-pair-mode 1))
(use-package multiple-cursors
  :bind (("M-3" . 'mc/mark-next-like-this)
         ("M-1" . 'mc/mark-previous-like-this)
         ("M-m" . 'mc/mark-all-like-this)
         :map mc/keymap
         ("M-h" . 'mc-hide-unmatched-lines-mode)
         ("M-s n" . 'mc/skip-to-next-like-this)
         ("M-s p" . 'mc/skip-to-previous-like-this))
  :hook ((prog-mode . multiple-cursors-mode)
         (text-mode . multiple-cursors-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Org Support
;; sudo apt-get install texlive-latex-base texlive-fonts-recommended \
;; texlive-fonts-extra texlive-latex-extra

;; for exporting html documents
(use-package verb
  :defer t
  ;; C-C C-r C-k to kill buffers
  ;; C-c C-r C-r to view header
  )
(use-package htmlize
  :straight t)
(use-package ob-typescript)
;;; sudo apt install phantomjs
(use-package ob-browser)
(use-package org
  ;; org-plus-contrib is a feature so must be loaded within org
  :straight org-plus-contrib
  :mode (("\\.org$" . org-mode))
  :hook (org-mode . org-indent-mode)
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  (:map org-mode-map
        ("C-M-<return>" . org-insert-subheading))
  :custom
  (org-directory (concat (getenv "HOME") "/org"))
  (org-default-notes-file (concat org-directory "/references/articles.org"))
  (org-agenda-files (list org-directory))
  ;; TODO: look to make refile easier to use (refile and delete)
  ;; NOTE: refile adds heading section to another heading section of your choice
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  ;; Allow refile to create parent tasks with confirmation
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-refile-targets
   '(("~/org/notebook/programming/web-development.org" :maxlevel . 2)
     (nil :maxlevel . 2)
     (org-agenda-files :maxlevel . 3)
     ))
  ;; single key press for certain movements when at first * in a heading
  (org-use-speed-commands t)
  ;;hide the leading stars in org mode
  ;; (org-hide-leading-stars t)
  (org-confirm-babel-evaluate nil)
  ;; allow native font editing (highlighting)
  (org-src-fontify-natively t)
  ;; tab acts normally in src mode
  (org-src-tab-acts-natively t)
  (org-export-use-babel t)
  ;; use python-3 in org mode
  (org-babel-python-command "python3")
  ;; change ... to down arrow
  (org-ellipsis " ▾")
  :init
  ;; setup electric-pairs mode for org-mode
  (defvar org-electric-pairs '((?/ . ?/) (?= . ?=)) "Electric pairs for org-mode.")
  ;; set a local variable ot contain new pairs for org-mode buffers
  (defun org-add-electric-pairs ()
    (setq-local electric-pair-pairs (append electric-pair-pairs org-electric-pairs))
    (setq-local electric-pair-text-pairs electric-pair-pairs))
  (defun org-keyword-backend (command &optional arg &rest ignored)
    "Add completions in org-mode when prefix is ^#+"
    (interactive (list 'interactive))
    (cl-case command
      (interactive (company-begin-backend 'org-keyword-backend))
      (prefix (and (eq major-mode 'org-mode)
                   (company-grab-line "^#\\+\\(\\w*\\)"
                                      t)))
      (candidates (mapcar #'upcase
                          (cl-remove-if-not
                           (lambda (c) (string-prefix-p arg c))
                           (pcomplete-completions))))
      (ignore-case t)
      (duplicates t)))
  ;; view items using emacs browser
  (if my/wsl
      (progn
        (setq browse-url-browser-function 'browse-url-generic browse-url-generic-program "wslview")))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python     . t)
     (typescript . t)
     (js         . t)
     (browser    . t)
     (shell      . t)))
  (setq org-file-apps
        (quote
         ((auto-mode . emacs)
          ("\\.mm\\'" . default)
          ("\\.x?html?\\'" . default)
          ("\\.pdf\\'" . default))))
  ;; add js2 mode to the src languages for org-mode blocks
  (add-to-list 'org-src-lang-modes '("js" . js2))
  (add-to-list 'org-src-lang-modes '("python" . python))
  (add-to-list 'org-src-lang-modes '("ts" . typescript))
  (add-to-list 'org-src-lang-modes '("browser" . web))
  ;; add quick way to make code block with name "<s"[TAB]
  ;; arg: results: [output value replace silent]
  (add-to-list 'org-structure-template-alist '("html" . "src browser"))
  (add-to-list 'org-structure-template-alist '("js" . "src js"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
  ;; make company backend simple for org files
  (add-hook 'org-mode-hook
            '(lambda ()
               (set (make-local-variable 'company-backends)
                    '(company-capf company-org-block org-keyword-backend company-ispell company-dabbrev))))
  ;; activate local electric-pair mode for org-buffer
  ;; disable <> auto pairing in electric-pair-mode for org-mode
  (add-hook 'org-mode-hook 'org-add-electric-pairs)
  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local electric-pair-inhibit-predicate
                          `(lambda (c)
                             (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry (file+datetree "~/org/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a")
          ("a"               ; key
           "Article"         ; name
           entry             ; type
           (file+headline "~/org/references/articles.org" "Article")  ; target
           "* %^{Title} %(org-set-tags)  :article: \n:PROPERTIES:\n:Created: %U\n:Linked: %a\n:END:\n%i\nBrief description:\n%?"  ; template
           :prepend t        ; properties
           :empty-lines 1    ; properties
           :created t        ; properties
           )))
  ;; TODO add to bind-keymap
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
  ;; (org-reload)
  )
(use-package ivy-bibtex
  :custom
  (ivy-bibtex-bibliography "~/org/references/articles.bib")
  (ivy-bibtex-library-path "~/org/pdfs/")
  (ivy-bibtex-notes-path "~/org/notebook/")
  (ivy-set-display-transformer
   'org-ref-ivy-insert-cite-link
   'ivy-bibtex-display-transformer))
(use-package org-ref
  :after org
  :custom
  (org-ref-notes-directory "~/org/notebook/")
  (org-ref-default-bibliography '("~/org/references/articles.bib"))
  (org-ref-pdf-directory "~/org/pdfs/")
  (bibtex-completion-bibliography "~/org/references/articles.bib")
  :init ;;https://github.com/jkitchin/org-ref/blob/35711c02992413e1df8aee54af290ac8650dbb82/org-ref.org#customizing-how-pdfs-are-opened
  (defun my/org-ref-open-pdf-at-point ()
    "Open the pdf for bibtex key under point if it exists."
    (interactive)
    (let* ((results (org-ref-get-bibtex-key-and-file))
           (key (car results))
           (pdf-file (funcall org-ref-get-pdf-filename-function key)))
      (if (file-exists-p pdf-file)
          (find-file pdf-file)
        (message "No PDF found for %s" key))))

  :config
  (setq org-ref-completion-library 'org-ref-ivy-cite)
  ;; open pdfs with pdf-tools
  (setq org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point)
  ;; Tell org-ref to let helm-bibtex find notes for it
  (setq org-ref-notes-function
        (lambda (thekey)
          (let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
            (bibtex-completion-edit-notes
             (list (car (org-ref-get-bibtex-key-and-file thekey))))))))
(use-package doct
  :straight t)
(use-package asoc
  :straight (asoc :type git :host github :repo "troyp/asoc.el"))
(use-package org-capture-ref
  :straight (org-capture-ref :type git :host github :repo "yantar92/org-capture-ref")
  :init
  ;; create doct group of category Browser link
  (let ((templates (doct '( :group "Browser link"
 			                       :type entry
 			                       :file "~/org/references/articles.org"
 			                       :fetch-bibtex (lambda () (org-capture-ref-process-capture)) ; this must run first
			                       :bibtex (lambda () (org-capture-ref-get-bibtex-field :bibtex-string))
                                   :url (lambda () (org-capture-ref-get-bibtex-field :url))
                                   :type-tag (lambda () (org-capture-ref-get-bibtex-field :type))
			                       :title (lambda () (format "%s%s%s%s"
					                                         (or (when (org-capture-ref-get-bibtex-field :author)
                                                                   (let* ((authors (s-split " *and *" (org-capture-ref-get-bibtex-field :author)))
							                                              (author-surnames (mapcar (lambda (author)
										                                                             (car (last (s-split " +" author))))
										                                                           authors)))
                                                                     (unless (string= "article" (org-capture-ref-get-bibtex-field :type))
                                                                       (setq author-surnames authors))
						                                             (if (= 1 (length author-surnames))
                                                                         (format "%s " (car author-surnames))
                                                                       (format "%s, %s " (car author-surnames) (car (last author-surnames))))))
                                                                 "")
                                                             (or (when (org-capture-ref-get-bibtex-field :journal)
						                                           (format "[%s] " (org-capture-ref-get-bibtex-field :journal)))
                                                                 (when (org-capture-ref-get-bibtex-field :howpublished)
                                                                   (format "[%s] " (org-capture-ref-get-bibtex-field :howpublished)))
                                                                 "")
                                                             (or (when (org-capture-ref-get-bibtex-field :year)
                                                                   (format "(%s) " (org-capture-ref-get-bibtex-field :year)))
                                                                 "")
                                                             (or (org-capture-ref-get-bibtex-field :title)
                                                                 "")))
			                       :id (lambda () (org-capture-ref-get-bibtex-field :key))
                                   :extra (lambda () (if (org-capture-ref-get-bibtex-field :journal)
					                                     (s-join "\n"
						                                         '("- [ ] download and attach pdf"
						                                           "- [ ] check if bibtex entry has missing fields"
						                                           "- [ ] read paper"
						                                           "- [ ] check citing articles"
						                                           "- [ ] check related articles"
						                                           "- [ ] check references"))
                                                       ""))
			                       :template
			                       ("%{fetch-bibtex}* TODO %? %{title} :BOOKMARK:%{type-tag}:"
			                        ":PROPERTIES:"
			                        ":ID: %{id}"
			                        ":CREATED: %U"
			                        ":Source: [[%{url}]]"
			                        ":END:"
                                    ":BIBTEX:"
			                        "#+begin_src bibtex"
			                        "%{bibtex}"
			                        "#+end_src"
                                    ":END:"
                                    "%i"
                                    "%{extra}")
			                       :children (("Interactive link"
				                               :keys "b"
				                               )
				                              ("Silent link"
				                               :keys "B"
				                               :immediate-finish t))))))
    (dolist (template templates)
      (asoc-put! org-capture-templates
	             (car template)
	             (cdr  template)
	             'replace)))
  :config
  (defun aaronzinhoo--org-capture-finalize-hook ()
    (let ((key  (plist-get org-capture-plist :key))
          (desc (plist-get org-capture-plist :description)))
      (if org-note-abort
          (message "Template with key %s and description “%s” aborted" key desc)
        (delete-frame))))
  (add-hook 'org-capture-after-finalize-hook 'aaronzinhoo--org-capture-finalize-hook)
  )
(use-package bibtex-completion
  :defer t
  :custom
  (bibtex-completion-pdf-symbol "")
  (bibtex-completion-notes-symbol "")
  (bibtex-completion-library-path "~/org/pdfs/")
  (bibtex-completion-notes-path "~/org/notebook/")
  (bibtex-completion-bibliography '("~/org/references/articles.bib"))
  (bibtex-completion-display-formats '((t . "${=has-pdf=:1}${=has-note=:1} ${author:20} ${year:4} ${title:*} ${=type=:3}")))
  :config
  (setq bibtex-completion-format-citation-functions
        '((org-mode . bibtex-completion-format-citation-pandoc-citeproc)
          (latex-mode . bibtex-completion-format-citation-cite)
          (default . bibtex-completion-format-citation-default))
        ))
;; replsace all headlines with bullets
;; Add org-protocol for org-capture
(use-package org-protocol
  :straight nil
  :config
  ;; (add-to-list 'org-capture-templates
  ;;              '("p" "Protocol" entry (file "~/org/references/articles.org")
  ;;                "* %?[[%:link][%:description]] %U\n%i\n" :prepend t))
  ;; (add-to-list 'org-capture-templates
  ;;              '("L" "Protocol Link" entry (file+headline "~/org/references/articles.org" "Links:")
  ;;                "* %?[[%:link][%:description]] %U\n" :prepend t))
  )
(use-package org-sidebar
  :straight (org-sidebar :type git :host github :repo "alphapapa/org-sidebar"))
(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t))
;; autoload html files org
(use-package org-preview-html
  :straight t)
(use-package org-noter
  :custom
  (org-noter-default-notes-file-names '("machine_learning.org" "cognitive_science.org" "programming_languages.org" "finance.org"))
  (org-noter-notes-search-path '("~/org/notebook")))
;; use eldoc in org-mode
(use-package org-eldoc
  :straight nil
  :after (org))

;;; PDF support
(use-package pdf-tools
  :custom
  (pdf-view-display-size 'fit-width)
  :config
  (pdf-loader-install))
(use-package pdf-continuous-scroll-mode
  :straight (pdf-continuous-scroll-mode :type git :host github :repo "dalanicolai/pdf-continuous-scroll-mode.el" :branch "master")
  :hook (pdf-view-mode . pdf-continuous-scroll-mode))

;;; Application Framework
;; (use-package eaf
;;   :straight (:type git
;;                    :host github
;;                    :repo "manateelazycat/emacs-application-framework"
;;                    :files ("*.el" "*.py" "core" "app"))
;;   :load-path "/home/aaronzinho/.emacs.d/eaf/git/emacs-application-framework"
;;   :custom
;;   (browse-url-browser-function 'eaf-open-browser) ;; Make EAF Browser my default browser
;;   (eaf-browser-continue-where-left-off t)
;;   :config
;;   (require 'eaf-org)
;;   (defalias 'browse-web #'eaf-open-browser)
;;   (eaf-setq eaf-browser-default-zoom "1.25")
;;   (eaf-setq eaf-browser-dark-mode "false")
;;   (eaf-setq eaf-browser-enable-adblocker "true")
;;   (eaf-setq eaf-pdf-dark-mode "false")
;;   (eaf-setq eaf-browser-enable-autofill "true")
;;   ;; I already bind "RET", "<mouse-2>", "^" to `dired-find-alternate-file' in `init-dired.el'.
;;   ;; Comment this line out of you don't want to use EAF to open available files in dired.
;;   ;; (global-set-key [remap dired-find-alternate-file] #'eaf-file-open-in-dired)
;;   (eaf-bind-key nil "M-q" eaf-browser-keybinding)
;;   (eaf-bind-key open_link "C-M-s" eaf-browser-keybinding)
;;   (eaf-bind-key clear_cookies "C-M-q" eaf-browser-keybinding)
;;   (eaf-bind-key insert_or_recover_prev_close_page "X" eaf-browser-keybinding)
;;   (eaf-bind-key scroll_up "RET" eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key scroll_down_page "DEL" eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key scroll_down_page "u" eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key scroll_up_page "d" eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key scroll_to_end "M->" eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key scroll_to_begin "M-<" eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key quit-window "q" eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key zoom_in "C-=" eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key zoom_out "C--" eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key take_photo "p" eaf-camera-keybinding)
;;   (eaf-bind-key eaf-send-key-sequence "M-]" eaf-terminal-keybinding)
;;   )
;;; Programming/Project Management
;; commenting
(use-package evil-nerd-commenter
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))
;; Window Manager
(use-package burly
  :straight (burly :type git :host github :repo "alphapapa/burly.el"))
(use-package editorconfig
  :diminish
  :config
  (editorconfig-mode 1))
(use-package bookmark+
  :custom
  (bookmark-default-file "~/.emacs.d/bookmarks") ;;define file to use.
  (bookmark-save-flag t) ;;save bookmarks to .emacs.bmk after each entry
  )
(use-package projectile
  :custom
  (projectile-find-dir-includes-top-level t)
  (projectile-switch-project-action #'projectile-find-dir)
  ;; use .gitignore to exclude files from search
  (projectile-indexing-method 'alien)
  (projectile-sort-order 'recentf)
  (projectile-completion-system 'ivy)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;;; Languages Support

;; Debugging
(use-package realgud
  :defer t)
;; Yaml editing support and JSON
;; json-mode => json-snatcher json-refactor
(use-package yaml-mode
  :mode (("\\.ya?ml$" . yaml-mode)))
;; use json-mode from https://github.com/joshwnj/json-mode for json instead of js-mode or js2-mode
(use-package json-mode
  :mode ("\\.json" . json-mode)
  :config
  (setq js-indent-level 2))
(use-package dotenv-mode
  :mode ("\\.env\\'" . dotenv-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; WEB-DEV CONFIG
(use-package simple-httpd
  :defer t)
(use-package skewer-mode
  :defer t)
(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))
(use-package add-node-modules-path
  :hook ((js2-mode . add-node-modules-path)
         (json-mode . add-node-modules-path))
  :config
  (eval-after-load 'rjsx-mode
    (add-node-modules-path)))
(use-package emmet-mode
  :hook ((web-mode . emmet-mode)
         (ng2-html-mode . emmet-mode)
         (emmet-mode . emmet-preview-mode)))
(use-package company-web
  :init
  (require 'company-web-html)
  :hook ((web-mode . (lambda ()
                       (add-to-list 'company-backends 'company-css)
                       (add-to-list 'company-backends 'company-web-html)
                       (add-to-list 'company-backends 'company-web-slim)))
         (ng2-html-mode . (lambda ()
                            (set (make-local-variable 'company-backends)
                                 '((company-web-html company-tide company-dabbrev company-capf)))))))
(use-package web-mode
  :mode (("\\.css\\$" . web-mode)
         ("\\.html\\$" . web-mode))
  :custom
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-attr-indent-offset 2)
  (web-mode-attr-value-indent-offset 2)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-auto-opening t)
  (web-mode-enable-auto-closing t)
  (web-mode-enable-css-colorization t)
  (web-mode-enable-auto-expanding t)
  (web-mode-enable-current-column-highlight t)
  (web-mode-enable-current-element-highlight t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; JS/react config
;; completetion: tide+company
;; refactor: js-prettier
;; syntax: flycheck
;; linter: flycheck
;; for React development use (setq create-lockfiles nil) to avoid crashes
;; packages needed:
;;     npm install prettier
;;     npm install eslint --save-dev
;;     npx eslint --init
;;     npm install --save typescript
;;     npm install --save @types/browserify
;;     tsc --init

;;   (local-set-key (kbd "C-c d") 'tide-documentation-at-point))
(use-package tide
  :after ( typescript-mode company flycheck)
  :bind (:map typescript-mode-map
              ("C-c d" . tide-documentation-at-point)
              ("C-c '" . nil))
  :hook ((ng2-mode .        tide-setup)
         (rjsx-mode .       tide-setup)
         (typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode))
  :custom
  (tide-sync-request-timeout 5)
  (tide-server-max-response-length 204800)
  (typescript-indent-line 2)
  :config
  (if (file-exists-p (concat tide-project-root "node_modules/typescript/bin/tsserver"))
      (setq tide-tsserver-executable "node_modules/typescript/bin/tsserver")))
(use-package prettier-js
  :after (rjsx-mode json-mode markdown-mode)
  :hook ((markdown-mode . prettier-js-mode)
         (json-mode . prettier-js-mode)
         (rjsx-mode . prettier-js-mode))
  :config
  (setq prettier-js-args '("--trailing-comma" "all"
                           "--bracket-spacing" "false")))
(use-package js-comint
  :defer t
  :init
  (defun inferior-js-mode-hook-setup ()
    (add-hook 'comint-output-filter-functions 'js-comint-process-output))
  :config
  (add-hook 'inferior-js-mode-hook 'inferior-js-mode-hook-setup t)
  (add-hook 'js2-mode-hook
            (lambda ()
              (local-set-key (kbd "\C-c !") 'run-js)
              (local-set-key (kbd "\C-c\C-r") 'js-send-region)
              (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
              (local-set-key (kbd "C-c b") 'js-send-buffer)
              (local-set-key (kbd "C-c C-b") 'js-send-buffer-and-go)))
  ;;(setq inferior-js-program-command "node")
  )
;; angular setup
(use-package ng2-mode
  :config
  (flycheck-add-mode 'typescript-tslint 'ng2-ts-mode)
  (flycheck-add-mode 'typescript-tide 'ng2-ts-mode))
;; react setup
(use-package rjsx-mode
  :mode (("\\.js\\'" . rjsx-mode)
         ("\\.tsx\\'" . rjsx-mode))
  :config
  (setq js2-mode-show-strict-warnings nil)
  (setq js-indent-level 2)
  (add-hook 'js2-mode-hook #'prettier-js-mode))

(use-package exec-path-from-shell
  :if (string-equal system-type "gnu/linux")
  :custom
  ;;(exec-path-from-shell-arguments nil)
  (exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PYTHON CONFIG
;; PYTHON VERSION NEEDS TO BE ADJUSTED PER SETUP
;; linter/refractor: black
;; syntax on-the-fly: flycheck
;; style: flake8
;; completion: company
;; install black, flake8 ipython, jedi, rope, autopep8, yapf
(use-package python
  :delight " Py"
  :mode ("\\.py" . python-mode)
  :init
  (eval-after-load 'python
    (lambda ()
      (defun python-shell-completion-native-try ()
        "Return non-nil if can trigger native completion."
        (let ((python-shell-completion-native-enable t)
              (python-shell-completion-native-output-timeout
               python-shell-completion-native-try-output-timeout))
          (python-shell-completion-native-get-completions
           (get-buffer-process (current-buffer))
           nil "_")))))
  :config
  (add-to-list 'process-coding-system-alist '("python" . (utf-8 . utf-8)))
  (setq python-indent-offset 4))
(use-package pyenv-mode
  :hook (elpy-mode . pyenv-mode)
  :bind
  ("C-x p e" . pyenv-activate-current-project)
  :init
  (add-to-list 'exec-path "~/.pyenv/shims")
  (setenv "WORKON_HOME" "~/.pyenv/versions/"))
(use-package pyenv-mode-auto
  :after pyenv-mode)
;; MAY HAVE TO CHANGE PYTHON PATH
;; INSTALL PYENV, VIRTUALENVWRAPPER to be used by elpy
(use-package elpy
  :diminish ""
  :init (with-eval-after-load 'python (elpy-enable))
  :hook (elpy-mode . flycheck-mode)
  :config
  (add-to-list 'process-coding-system-alist '("elpy" . (utf-8 . utf-8)))
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt")
  (setq elpy-shell-starting-directory 'current-directory)
  ;;use flake8
  (setq python-check-command "flake8")
  ;;replace flycheck with flymake
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (setq elpy-modules (delq 'elpy-module-company elpy-modules ))
  ;; use jedi for completetions
  (defun company-elpy-setup ()
    (add-to-list 'company-backends 'elpy-company-backend))
  (add-hook 'python-mode-hook 'company-elpy-setup))
(use-package blacken
  :after elpy
  :delight " Bl"
  :hook (python-mode . blacken-mode)
  :config
  (setq blacken-line-length '80))

;; Golang Setup
;; export GO111MODULE="on" might be needed
;; need a package if not in GOPATH!
(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :init
  ;;Smaller compilation buffer
  (setq compilation-window-height 14)
  (defun my-compilation-hook ()
    (when (not (get-buffer-window "*compilation*"))
      (save-selected-window
        (save-excursion
          (let* ((w (split-window-vertically))
                 (h (window-height w)))
            (select-window w)
            (switch-to-buffer "*compilation*")
            (shrink-window (- h compilation-window-height)))))))
  (defun my-go-mode-hook ()
    ;; Customize compile command to run go build
    (if (not (string-match "go" compile-command))
        (set (make-local-variable 'compile-command)
             "go build -v -o ./main")))
  (setq compilation-read-command nil)
  :bind (:map go-mode-map
              ("M-," . compile)
              ("M-." . godef-jump)
              ("M-*" . pop-tag-mark))
  :config
  (setq compilation-scroll-output t)
  ;; (add-hook 'compilation-mode-hook 'my-compilation-hook)
  (add-hook 'go-mode-hook 'yas-minor-mode)
  (add-hook 'go-mode-hook 'my-go-mode-hook))
;; C++ / C
;; lsp-mode + ccls for debugging
;; configuration: use set(CMAKE_EXPORT_COMPILE_COMMANDS ON) in cmake file
;; cmake-mode + cmake-font-lock for editing cmake files
(use-package cmake-font-lock)
(use-package ccls
  :custom
  (ccls-args nil)
  (ccls-executable "ccls")
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp))))
(use-package cmake-mode
  :init
  (setq auto-mode-alist
        (append
         '(("CMakeLists\\.txt\\'" . cmake-mode))
         '(("\\.cmake\\'" . cmake-mode))
         auto-mode-alist)))
;; ----------------------------------------------------------------
(use-package moe-theme
  :straight (moe-theme-switcher :type git :host github :repo "kuanyui/moe-theme.el" :branch "dev")
  :config
  (require 'moe-theme-switcher)
  (setq moe-theme-highlight-buffer-id t)
  (powerline-moe-theme))

;; Helpful Defualt keys
;; C-h k <key> -> describe what key is binded to
;; M-DEL del backward one word
;; C-c ' edit code in buffer
;; C-c C-c run org code block

;;CUSTOM EMACS BUILT-IN KEYS
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "M-i") 'previous-line)
(global-set-key (kbd "M-j") 'backward-char)
(global-set-key (kbd "M-k") 'next-line)
(global-set-key (kbd "M-l") 'forward-char)
(global-set-key (kbd "M-q") 'yank)
(global-set-key (kbd "M-4") 'pop-local-mark-ring)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)
(global-set-key (kbd "M-[") 'backward-up-list)
(global-set-key (kbd "M-]") 'up-list)
;; delete pair of items
(global-set-key (kbd "C-c C-p") 'delete-pair)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; load custom faces and vars for packages
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file) (load custom-file))
;;; init.el ends here
