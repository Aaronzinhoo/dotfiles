;;; -*- lexical-binding: t -*-
;;; package --- Summary
;;; Commentary:
;;;init.el --- Emacs configuration

;;; Code:
;; startup defaults
(setq user-full-name "Aaron Gonzales")
(setq user-init-dir "~/.emacs.d")
(setq user-init-file "~/.emacs.d/init.el")
(setq user-emacs-directory "~/.config/emacs")
(defvar home-directory (expand-file-name "~/.config/emacs"))
(defconst backup-dir (concat home-directory "/backups"))
(defvar autosave-dir (concat home-directory "/autosave"))
(defvar file-name-handler-alist-old file-name-handler-alist)
(defconst my/wsl (not (null (string-match "Linux.*Microsoft" (shell-command-to-string "uname -a")))))
;; font
(add-to-list 'default-frame-alist '(font . "-SRC-Hack-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1"))
;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :weight 'regular)
;; more defaults
(setq package-enable-at-startup nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      auto-window-vscroll nil
      scroll-margin 4
      ad-redefinition-action 'accept
      calendar-latitude 33.916403
      calendar-longitude -118.352575
      create-lockfiles nil
      select-enable-clipboard t
      inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1) ;;remove the scroll bar
(scroll-lock-mode nil)
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
(setq make-backup-files t
      backup-by-copying t ;; safest method to backup
      delete-old-versions t ;; delete excess backups
      delete-by-moving-to-trash t
      kept-old-versions 0
      kept-new-versions 10
      auto-save-default t
      backup-directory-alist `((".*" . ,backup-dir)))
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
(defun post-func-recenter ()
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
  (next-buffer))
(defun split-and-follow-vertically ()
  "Split window vertically and follow with the previous buffer open."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1)
  (next-buffer))
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
(setq ring-bell-function 'ignore)
(column-number-mode t) ;; enable column numbers globally
(global-visual-line-mode t) ;; cause lines to wrap
(put 'erase-buffer 'disabled nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(defalias 'yes-or-no-p 'y-or-n-p)
;; UTF-8 as default encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
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
(use-package command-log-mode
  :commands (command-log-mode))
(use-package bind-key :straight t)
(use-package general
  :defer t)
(use-package dash
  :config
  (dash-enable-font-lock))
(use-package diminish
  :straight t)
(use-package hl-line
  :straight nil
  :hook (prog-mode . hl-line-mode))
;;org-noter/pdf-tools dependency
(use-package tablist)
;; required to be updated for company mode
(use-package pos-tip)
(use-package posframe
  :straight (:type git :host github :repo "tumashu/posframe" :branch "master"))
;;;;;;;;;;;;;;;;;;;;;;;;
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
;; Hydra
(use-package hydra
  :bind
  ("C-c f" . hydra-flycheck/body)
  ("C-c o" . hydra-org/body)
  ("C-c p" . hydra-projectile/body)
  ("C-c i" . hydra-ivy/body)
  :custom
  (hydra-default-hint nil))
(use-package major-mode-hydra
  :after hydra
  :preface
  (defun with-alltheicon (icon str &optional height v-adjust)
    "Displays an icon from all-the-icon."
    (s-concat (all-the-icons-alltheicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))
  (defun with-faicon (icon str &optional height v-adjust)
    "Displays an icon from Font Awesome icon."
    (s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))
  (defun with-fileicon (icon str &optional height v-adjust)
    "Displays an icon from the Atom File Icons package."
    (s-concat (all-the-icons-fileicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))
  (defun with-octicon (icon str &optional height v-adjust)
    "Displays an icon from the GitHub Octicons."
    (s-concat (all-the-icons-octicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))
  :config
  (pretty-hydra-define hydra-projectile
    (:hint nil :color teal :quit-key "q" :title (with-faicon "rocket" "Projectile" 1 -0.05))
    ("Buffers"
     (("b" counsel-projectile-switch-to-buffer "list")
      ("k" projectile-kill-buffers "kill all")
      ("S" projectile-save-project-buffers "save all"))
     "Find"
     (("d" counsel-projectile-find-dir "directory")
      ("D" projectile-dired "root")
      ("f" counsel-projectile-find-file "file")
      ("p" counsel-projectile-switch-project "project"))
     "Other"
     (("N" projectile-cleanup-known-projects)
      ("i" projectile-invalidate-cache "reset cache")
      ("c" projectile-compile-project "compile")
      ("v" projectile-run-vterm "run vterm"))
     "Search"
     (("r" projectile-replace "replace")
      ("R" projectile-replace-regexp "regexp replace")
      ("s" counsel-projectile-rg "search"))))
  (pretty-hydra-define hydra-flycheck
    (:hint nil :color teal :quit-key "q" :title (with-faicon "plane" "Flycheck" 1 -0.05))
    ("Checker"
     (("?" flycheck-describe-checker "describe")
      ("d" flycheck-disable-checker "disable")
      ("m" flycheck-mode "mode")
      ("s" flycheck-select-checker "select"))
     "Errors"
     (("<" flycheck-previous-error "previous" :color pink)
      (">" flycheck-next-error "next" :color pink)
      ("l" flycheck-list-errors "list"))
     "Other"
     (("r" recenter-top-bottom "recenter" :color pink)
      ("M" flycheck-manual "manual")
      ("v" flycheck-verify-setup "verify setup"))))
  (pretty-hydra-define hydra-org
    (:hint nil :color pink :quit-key "q" :title (with-fileicon "org" "Org" 1 -0.05))
    ("Action"
     (("a" org-agenda "agenda")
      ("c" org-capture "capture")
      ("d" org-decrypt-entry "decrypt")
      ("i" org-insert-link-global "insert-link")
      ("k" org-cut-subtree "cut-subtree")
      ("o" org-open-at-point-global "open-link")
      ("r" org-refile "refile")
      ("s" org-store-link "store-link")
      ("t" org-show-todo-tree "todo-tree"))))
  (pretty-hydra-define hydra-org-nav
    (:hint nil :color pink :quit-key "q" :title (with-fileicon "org" "Org" 1 -0.05))
    ("Navigation"
     (("p" org-previous-visible-heading "prev heading")
      ("n" org-next-visible-heading "next heading")
      ("B" org-previous-block)
      ("b" org-next-block)
      ("g" counsel-org-goto "goto"))
     "Links"
     (("l" org-next-link "next link")
      ("L" org-previous-link "prev link")
      ("o" org-open-at-point "open link at point")
      ("i" org-insert-link "insert link")
      ("s" org-store-link "store link"))
     "Outline"
     (("N" org-toggle-narrow-to-subtree "narrow/unarrow" :color blue)
      ("r" org-refile "refile")
      ("v" org-overview "overview" :color blue)
      ("a" outline-show-all "show-all" :color blue))
     "Other"
     (("RET" nil :color blue))))
  (pretty-hydra-define hydra-ivy
    (:hint nil :color teal :quit-key "q" :title (with-faicon "tree" "Ivy" 1 -0.05))
    ("Action"
     (("f" counsel-recentf "recent-file")
      ("t" counsel-faces "faces")
      ("i" counsel-imenu "imenu")
      ("l" counsel-find-library "library")
      ("r" ivy-resume "resume"))
     "Text"
     (("c" ivy-insert-current "current cand." :color red)
      ("w" ivy-yank-word "yank subword" :color red))
     "Other"
     (("s" counsel-info-lookup-symbol "symbol")
      ("u" counsel-unicode-char "unicode"))))
  (pretty-hydra-define hydra-lsp
    (:hint nil :color pink :quit-key "q" :title (with-faicon "cog" "LSP" 1 -0.05))
    ("Goto"
     (("r" lsp-find-references "refs")
      ("d" lsp-find-definition "defs")
      ("t" lsp-find-type-definition "type-def")
      ("b" xref-pop-marker-stack "pop back" :color red))
     "Refactor"
     (("F" lsp-format-buffer "format"))
     "UI"
     (("p" lsp-ui-peek-mode "peek-mode" :toggle t)
      ("R" lsp-ui-peek-find-references "peek-refs" :color red)
      ("D" lsp-ui-peek-find-definitions "peek-defs" :color red)
      ("i" lsp-ui-imenu "peek-menu"))
     "Server"
     (("s" lsp-describe-session "session")
      ("I" lsp-install-server "install")
      ("S" lsp-workspace-restart "restart")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CONTROL VERSION UTILS
(use-package git-gutter
  :after (all-the-icons)
  :straight (:type git :host github :repo "emacsorphanage/git-gutter" :branch "master")
  :hook (prog-mode . git-gutter-mode)
  :bind ("C-c g" . hydra-git-gutter/body)
  :commands (git-gutter-mode)
  :diminish git-gutter-mode
  :preface
  (pretty-hydra-define hydra-git-gutter
    (:hint nil :color "pink" :quit-key "q" :title (with-octicon "diff" "Diff" 1 -0.05))
    ("Nav Hunks"
     (("n" git-gutter:next-hunk "next")
      ("p" git-gutter:previous-hunk "prev")
      ("e" git-gutter:end-of-hunk "end"))
     "Edit Hunks"
     (("m" git-gutter:mark-hunk "mark")
      ("P" git-gutter:popup-hunk "popup")
      ("s" git-gutter:stage-hunk "stage")
      ("r" git-gutter:revert-hunk "revert"))
     "Other"
     (("q" nil "Quit" :color blue))))
  :custom
  (git-gutter:ask-p nil)
  (git-gutter:sign-width 1)
  (git-gutter:hide-gutter t)
  (git-gutter:window-width 2)
  (git-gutter:modified-sign (all-the-icons-octicon "diff-modified" :height 0.85 :width 0.85))
  (git-gutter:added-sign (all-the-icons-octicon "diff-added" :height 0.85 :width 0.85))
  (git-gutter:deleted-sign (all-the-icons-octicon "diff-removed" :height 0.85 :width 0.85))
  (git-gutter:update-interval 1))
(use-package git-timemachine
  :defer t
  :commands (git-timemachine))
(use-package hl-todo
  :config
  (global-hl-todo-mode))
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
(use-package git-identity
  :after magit
  :bind (:map magit-status-mode-map
              ("I" . git-identity-info))
  :custom
  (git-identity-list
   '(("aaron.gonzales@linquest.com"
      :domains ("github.km.spaceforce.mil")
      :dirs ("~/development/work")
      :username )
     ("aaronzinho@ucla.edu"
      :domains ("github.com")
      ;; The identity is applied if the remote URL contains this organization as directory
      :exclude-organizations ("kahless")
      :dirs ("~/.emacs.d" "~/personal"))))
  ;; Warn if the global identity setting violates your policy
  (git-identity-verify t)
  (git-identity-magit-mode t)
  ;; The default user name
  (git-identity-default-username "Aaron Gonzales"))
(use-package magit
  :commands (magit-status)
  :diminish
  :bind (("M-s" . magit-status)
         :map magit-status-mode-map
         ("RET" . magit-diff-visit-file-other-window))
  :hook (magit-mode . magit-auto-revert-mode)
  :custom
  (magit-completing-read-function 'ivy-completing-read)
  :config
  (add-hook 'after-save-hook 'magit-after-save-refresh-status))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package better-defaults
  :defer t)
(use-package grep
  :defer t)
;; convert elisp to reg-exp
(use-package rx)
;; convert reg-exp to elisp code
(use-package xr
  :straight (:type git :host github :repo "mattiase/xr" :branch "master"))
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
         (dired-mode . auto-revert-mode)
         (dired-mode . all-the-icons-dired-mode))
  :custom
  (dired-listing-switches "-lXGh --group-directories-first"
                          dired-dwim-target t)
  (dired-auto-revert-buffer t))
(use-package dired-single)
(use-package dired-collapse)
(use-package dired-narrow
  :bind (("C-c C-n" . dired-narrow)))
(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              ("k" . dired-subtree-remove)
              ("<tab>" . aaronzinhoo-dired-subtree-toggle)
              ("<backtab>" . aaronzinhoo-dired-subtree-toggle)
              ("C-n" . dired-subtree-next-sibling)
              ("C-p" . dired-subtree-previous-sibling))
  :preface
  (defun aaronzinhoo-dired-subtree-toggle ()
    (interactive)
    (dired-subtree-toggle)
    (revert-buffer)))
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
  :preface
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
(use-package resize-window
  :straight (:type git :host github :repo "dpsutton/resize-window" :branch "master")
  :bind ("C-M-w" . resize-window))
(use-package winner
  :straight nil
  :config
  (winner-mode 1))
(use-package ace-window
  :commands ace-window
  :bind ("C-x o" . ace-window)
  :custom
  (aw-ignore-current t)
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
(use-package beacon
  :straight t
  :diminish
  :custom
  (beacon-color "#111FFF")
  :config
  (beacon-mode 1))
(use-package which-key
  :straight t
  :diminish
  :custom
  (which-key-use-C-h-commands nil)
  :config
  (which-key-mode t))
(use-package default-text-scale
  :defer 2
  :bind (("C--" . text-scale-decrease)
         ("C-=" . text-scale-increase)))
(use-package eldoc
  :diminish eldoc-mode)
(use-package flycheck
  :straight (:type git :host github :repo "flycheck/flycheck" :branch "master")
  :diminish
  :commands flycheck-mode
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-stylelintrc "~/.stylelintrc")
  (flycheck-css-stylelint-executable "stylelint")
  (flycheck-yamllintrc "~/.yamllintrc")
  :config
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint c/c++-clang c/c++-cppcheck c/c++-gcc)))
  (flycheck-add-mode 'json-jsonlint 'json-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  ;; eslint requires you to be careful with the configuration
  ;; ensure to use .json files and setup accordingly
  ;; test with shell command
  ;; tide-typescript gives type errors so generally place first
  ;; tide-typescript not helpful for javascript unless checkJs true so can use just eslint
  (flycheck-add-mode 'javascript-eslint 'typescript-mode)
  (flycheck-add-mode 'css-stylelint 'css-mode))
(use-package aggressive-indent
  :straight t
  :diminish
  :config
  (global-aggressive-indent-mode 1)
  (setq aggressive-indent-excluded-modes (append aggressive-indent-excluded-modes '(web-mode dockerfile-mode docker-compose-mode))))
(use-package fix-word
  :bind (([remap capitalize-word] . fix-word-capitalize)
         ([remap upcase-word] . fix-word-upcase)))
(use-package easy-kill
  :preface
  (defun aaronzinhoo-open-line ()
    "Mark the current line."
    (interactive)
    (beginning-of-line-text)
    (open-line 1))
  :bind (([remap open-line] . aaronzinhoo-open-line)
         ([remap kill-ring-save] . easy-kill)))
(use-package expand-region
  :bind (("M-2" . er/expand-region)
         ("C-(" . er/mark-outside-pairs))
  :preface
  (defun aaronzinhoo-mark-line ()
    "Mark the current line."
    (interactive)
    (end-of-line)
    (set-mark (point))
    (beginning-of-line-text))
  (defun er/add-rjsx-mode-expansions ()
    (make-variable-buffer-local 'er/try-expand-list)
    (setq er/try-expand-list (append
                              er/try-expand-list
                              '(er/c-mark-statement
                                er/c-mark-fully-qualified-name
                                er/c-mark-function-call-1   er/c-mark-function-call-2
                                er/c-mark-statement-block-1 er/c-mark-statement-block-2
                                er/c-mark-vector-access-1   er/c-mark-vector-access-2
                                aaronzinhoo-mark-line
                                er/mark-html-attribute
                                er/mark-inner-tag
                                er/mark-outer-tag))))
  :config
  (delete-selection-mode 1)
  (er/enable-mode-expansions 'typescript-mode 'er/add-rjsx-mode-expansions)
  (er/enable-mode-expansions 'rjsx-mode 'er/add-rjsx-mode-expansions)
  (er/enable-mode-expansions 'web-mode 'er/add-web-mode-expansions))
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
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))
(use-package yasnippet-snippets)
(use-package lsp-mode
  :straight (:type git :host github :repo "emacs-lsp/lsp-mode" :branch "master")
  :hook (((c-mode        ; clangd
           c++-mode  ; clangd
           java-mode      ; eclipse-jdtls
           go-mode
           sql-mode
           html-mode
           ng2-ts-mode
           ng2-html-mode
           yaml-mode
           rustic-mode
           ) . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . yas-minor-mode))
  :bind
  (:map lsp-mode-map
        ("C-c l" . hydra-lsp/body))
  :custom
  (lsp-enable-indentation nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-prefer-flymake nil)
  (lsp-enable-symbol-highlighting t)
  (lsp-signature-auto-activate nil)
  (lsp-keymap-prefix nil)
  (lsp-completion-enable t)
  (lsp-yaml-schemas
   `((,(intern "https://json.schemastore.org/helmfile.json") . ["Chart.yaml" , "pipeline.yaml"])
     (,(intern
        "https://raw.githubusercontent.com/compose-spec/compose-spec/master/schema/compose-spec.json") . ["docker-compose.yml", "docker-compose.yaml"])
     (kubernetes . ["/proj_template.yaml"])))
  :init
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook 'lsp-format-buffer)
    (add-hook 'before-save-hook 'lsp-organize-imports)
    (setq lsp-gopls-staticcheck t)
    (setq lsp-eldoc-render-all t)
    (setq lsp-gopls-complete-unimported t))
  :config
  (setq read-process-output-max (* 1024 1024)) ;;1MB
  (add-hook 'go-mode-hook 'lsp-go-install-save-hooks)
  )
(use-package lsp-ui
  :commands lsp-ui-mode
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  :custom
  (lsp-ui-peek-enable t)
  (lsp-ui-doc-enable nil))
(use-package lsp-ivy
  :after (lsp-mode ivy))
;; (use-package company-box
;;   :after company
;;   :diminish
;;   :hook (company-mode . company-box-mode)
;;   :init
;;   (setq company-box-enable-icon (display-graphic-p))
;;   :config
;;   (setq company-box-backends-colors nil))
(use-package company
  :straight (company :files (:defaults "icons"))
  :diminish company-mode
  :bind
  ([remap indent-for-tab-command] . company-indent-or-complete-common)
  (:map company-active-map
        ("<tab>" . company-complete-common-or-cycle)
        ("<backtab>" . company-select-previous))
  :preface
  (defun company-preview-if-not-tng-frontend (command)
    "`company-preview-frontend', but not when tng is active."
    (unless (and (eq command 'post-command)
                 company-selection-changed
                 (memq 'company-tng-frontend company-frontends))
      (company-preview-frontend command)))
  :init
  (setq company-format-margin-function #'company-vscode-dark-icons-margin)
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
        '(company-preview-if-not-tng-frontend
          company-pseudo-tooltip-frontend
          company-preview-if-just-one-frontend
          company-echo-metadata-frontend))
  :config
  (global-company-mode t))
(use-package company-posframe
  :straight (:type git :host github :repo "tumashu/company-posframe" :branch "master")
  :diminish company-posframe-mode
  :hook (company-mode . company-posframe-mode)
  :config
  (setq company-posframe-show-indicator nil)
  (setq company-posframe-show-metadata nil))
(use-package company-bootstrap
  :straight (:type git :host github :repo "typefo/company-bootstrap" :branch "master"))
(use-package company-web
  :init
  (require 'company-web-html))
(use-package company-quickhelp
  :after company
  :straight (:type git :host github :repo "company-mode/company-quickhelp" :branch "master")
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
  :after ivy
  :init
  (setq ivy-flx-limit 10000))
(use-package counsel
  :diminish (ivy-mode counsel-mode)
  :bind* (("M-x" . counsel-M-x)
          ("C-s" . counsel-grep-or-swiper)
          ("C-r" . counsel-rg)
          ("M-t" .  swiper-isearch-thing-at-point)
          ("C-S-s" . swiper-isearch)
          ("C-x d" . counsel-dired)
          ("C-x D" . dired-jump)
          ("C-x b" . counsel-switch-buffer)
          ("C-x C-f" . counsel-find-file)
          ("C-x C-r" . counsel-recentf)
          ("C-c r" . ivy-resume)
          ("C-c m" . counsel-imenu)
          :map ivy-switch-buffer-map
          ("C-k" . ivy-switch-buffer-kill)
          :map ivy-minibuffer-map
          ("C-c o" . ivy-occur)
          ("M-i" . nil)
          ("C-j" . ivy-immediate-done)
          :map ivy-occur-grep-mode-map
          ("C-c h" . hydra-ivy-occur/body))
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :preface
  (defun ivy-update-candidates-dynamic-collection-workaround-a (old-fun &rest args)
    (cl-letf (((symbol-function #'completion-metadata) #'ignore))
      (apply old-fun args)))
  (pretty-hydra-define hydra-ivy-occur
    (:hint nil :color pink :quit-key "q" :title (with-faicon "tree" "Ivy-Occur" 1 -0.05))
    ("Navigation"
     (("n" ivy-occur-next-line "next")
      ("p" ivy-occur-previous-line "prev"))
     "Edit"
     (("w" ivy-wgrep-change-to-wgrep-mode "wgrep" :color teal)
      ("d" ivy-occur-delete-candidate "delete")
      ("o" ivy-occur-dispatch "dispatch")
      ("g" ivy-occur-revert-buffer))
     "View"
     (("v" ivy-occur-press "preview")
      ("RET" ivy-occur-press-and-switch "goto" :color teal))))
  :custom
  (imenu-auto-rescan t)
  (ivy-wrap t)
  (ivy-initial-inputs-alist nil)
  (swiper-action-recenter t)
  (enable-recursive-minibuffers t)
  (ivy-extra-directories nil)
  (ivy-use-virtual-buffers t)
  (ivy-count-format "%d/%d ")
  (ivy-display-style 'fancy)
  :config
  (setq swiper-use-visual-line-p (lambda (_) nil))
  ;; fix for async display of counsel-rg resuls
  (advice-add #'ivy-update-candidates :around #'ivy-update-candidates-dynamic-collection-workaround-a))
(use-package counsel-tramp
  :commands (counsel-tramp))
(use-package counsel-projectile
  :straight (:type git :host github :repo "ericdanan/counsel-projectile")
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
  :after (counsel)
  :config
  (prescient-persist-mode t))
(use-package ivy-prescient
  :after (prescient)
  :custom
  (ivy-prescient-enable-sorting t)
  (ivy-prescient-enable-filtering t)
  (ivy-re-builders-alist
   '((swiper-isearch . ivy-prescient-re-builder)
     (swiper . ivy-prescient-re-builder)
     (swiper-isearch-thing-at-point . ivy-prescient-re-builder)
     (t . ivy--regex-plus)))
  :config
  (ivy-prescient-mode t))
(use-package company-prescient
  :after (company prescient)
  :config
  (company-prescient-mode t))
(use-package ag
  :defer 3)
(use-package move-text
  :straight (:type git :host github :repo "emacsfodder/move-text" :branch "master")
  :init
  (move-text-default-bindings))
(use-package avy
  :bind* ("M-SPC" . avy-goto-char)
  :config
  (advice-add 'avy-goto-char :after 'post-func-recenter))
(use-package electric
  :config
  (electric-pair-mode 1))
(use-package multiple-cursors
  :straight (:type git :host github :repo "magnars/multiple-cursors.el" :branch "master")
  :bind (("M-m" . hydra-multiple-cursors/body))
  :hook ((prog-mode . multiple-cursors-mode)
         (text-mode . multiple-cursors-mode))
  :init
  (pretty-hydra-define hydra-multiple-cursors
    (:hint nil :color pink :quit-key "q" :title (with-faicon "key" "Multiple Cursors" 1 -0.05))
    ("Up"
     (("p" mc/mark-previous-like-this "Prev")
      ("P" mc/skip-to-previous-like-this "Skip Prev")
      ("M-p" mc/unmark-previous-like-this "Unmark Prev"))
     "Down"
     (("n" mc/mark-next-like-this "Next")
      ("N" mc/skip-to-next-like-this "Skip Next")
      ("M-n" mc/unmark-next-like-this "Unmark Next"))
     "Cycle"
     (("c" mc/cycle-forward "next cursor")
      ("C" mc/cycle-back "previous cursor"))
     "Misc."
     (("2" er/expand-region "Expand Region")
      ("h" mc-hide-unmatched-lines-mode "Hide lines" :toggle t)
      ("a" mc/mark-all-like-this "Mark All" :color blue)
      ("RET" nil "Quit"))))
  ;; This file is automatically generated by the multiple-cursors extension.
  ;; It keeps track of your preferences for running commands with multiple cursors.
  :config
  (setq mc/cmds-to-run-for-all
        '(abbrev-prefix-mark
          crux-smart-delete-line
          hungry-delete-backward
          hungry-delete-forward))
  (setq mc/cmds-to-run-once
        '(counsel-M-x
          mc/mark-previous-like-this
          hydra-multiple-cursors/mc-hide-unmatched-lines-mode
          hydra-multiple-cursors/mc/mark-all-like-this-and-exit
          hydra-multiple-cursors/mc/mark-next-like-this
          hydra-multiple-cursors/mc/skip-to-previous-like-this
          hydra-multiple-cursors/mc/skip-to-next-like-this
          hydra-multiple-cursors/mc/nil
          hydra-multiple-cursors/mc/mark-previous-like-this
          hydra-multiple-cursors/mc/edit-lines-and-exit)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Creating Diagrams
(use-package plantuml-mode
  :straight (:type git :host github :repo "skuro/plantuml-mode" :branch "master")
  :mode (("\\plantuml\\'" . plantuml-mode))
  :custom
  (plantuml-executable-path "plantuml")
  (plantuml-default-exec-mode 'executable)
  :config
  (add-hook 'plantuml-mode-hook (lambda ()
                                  (set (make-local-variable 'company-backends)
                                       '((company-capf company-dabbrev-code))))))
;;; Org Support
;; for exporting html documents
(use-package htmlize
  :defer t)
(use-package ob-typescript)
;;; sudo apt install phantomjs
(use-package ob-browser)
;; better way to test APIs (like postman but with org files!)
;; must keep here since org uses ob-verb
(use-package verb
  ;; C-C C-r C-k to kill buffers
  ;; C-c C-r C-r to view header
  )
(use-package org-contrib)
(use-package org
  :mode (("\\.org$" . org-mode))
  :hook ((org-mode . aaronzinhoo-org-setup)
         (org-mode . aaronzinhoo-org-font-setup))
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  (:map org-mode-map
        ("C-M-<return>" . org-insert-subheading)
        ("C-c h". hydra-org-nav/body))
  :preface
  (defun aaronzinhoo-org-inline-css-hook (exporter)
    "Insert custom inline css"
    (when (eq exporter 'html)
      (let* ((dir (ignore-errors (file-name-directory (buffer-file-name))))
             (path (concat dir "style.css"))
             (homestyle (or (null dir) (null (file-exists-p path))))
             (final (if homestyle (concat user-init-dir "/org/sakura-dark-theme.css") path)))
        (setq org-html-head-include-default-style nil)
        (setq org-html-head (concat
                             "<style type=\"text/css\">\n"
                             "<!--/*--><![CDATA[/*><!--*/\n"
                             (with-temp-buffer
                               (insert-file-contents final)
                               (buffer-string))
                             "/*]]>*/-->\n"
                             "</style>\n")))))
  (defun aaronzinhoo-org-setup ()
    (variable-pitch-mode t)
    (org-indent-mode t)
    (org-superstar-mode t))
  (defun aaronzinhoo-org-font-setup ()
    ;; Set faces for heading levels
    (dolist (face '((org-level-1 . 1.2)
                    (org-level-2 . 1.1)
                    (org-level-3 . 1.05)
                    (org-level-4 . 1.0)
                    (org-level-5 . 1.1)
                    (org-level-6 . 1.1)
                    (org-level-7 . 1.1)
                    (org-level-8 . 1.1)))
      (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

    ;; Ensure that anything that should be fixed-pitch in Org files appears that way
    (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))
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
     (nil :maxlevel . 4)
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
  (org-export-headline-levels 5)
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
  (add-hook 'org-export-before-processing-hook 'aaronzinhoo-org-inline-css-hook)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python     . t)
     (typescript . t)
     (plantuml   . t)
     (js         . t)
     (browser    . t)
     (verb       . t)
     (shell      . t)))
  (setq org-file-apps
        (quote
         ((auto-mode . emacs)
          ("\\.mm\\'" . default)
          ("\\.x?html?\\'" . default)
          ("\\.pdf\\'" . default))))
  ;; add modes to the src languages for org-mode blocks
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (add-to-list 'org-src-lang-modes '("js" . js2))
  (add-to-list 'org-src-lang-modes '("python" . python))
  (add-to-list 'org-src-lang-modes '("ts" . typescript))
  (add-to-list 'org-src-lang-modes '("browser" . web))
  (add-to-list 'org-src-lang-modes '("html" . web))
  (add-to-list 'org-src-lang-modes '("verb" . verb))
  ;; add quick way to make code block with name "<s"[TAB]
  ;; arg: results: [output value replace silent]

  (add-to-list 'org-structure-template-alist '("plantuml" . "src plantuml"))
  (add-to-list 'org-structure-template-alist '("html" . "src html"))
  (add-to-list 'org-structure-template-alist '("browser" . "src browser"))
  (add-to-list 'org-structure-template-alist '("js" . "src js"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
  (add-to-list 'org-structure-template-alist '("verb" . "src verb"))
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
          ("a"                          ; key
           "Article"                    ; name
           entry                        ; type
           (file+headline "~/org/references/articles.org" "Article") ; target
           "* %^{Title} %(org-set-tags)  :article: \n:PROPERTIES:\n:Created: %U\n:Linked: %a\n:END:\n%i\nBrief description:\n%?" ; template
           :prepend t                   ; properties
           :empty-lines 1               ; properties
           :created t                   ; properties
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
  ;; :config
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
;;                    :files ("*"))
;;   ;; :load-path (concat home-directory "/eaf/git/emacs-application-framework")
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

;; Terminal
(use-package vterm
  :commands vterm)

;; Programming/Project Management
;; commenting
(use-package evil-nerd-commenter
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))
;; Window Manager
(use-package burly
  :straight (burly :type git :host github :repo "alphapapa/burly.el"))
(use-package editorconfig
  :diminish
  :config
  (setq editorconfig-exclude-modes (append editorconfig-exclude-modes '(image-mode nxml-mode)))
  (editorconfig-mode 1))
(use-package bookmark+
  :custom
  (bookmark-default-file (concat home-directory "/bookmarks")) ;;define file to use.
  (bookmark-save-flag t) ;;save bookmarks to .emacs.bmk after each entry
  )
(use-package projectile
  :custom
  (projectile-find-dir-includes-top-level t)
  (projectile-switch-project-action #'projectile-find-dir)
  ;; use .gitignore to exclude files from search
  (projectile-indexing-method 'alien)
  (projectile-sort-order 'recentf)
  (projectile-completion-system 'ivy))

;;; Languages Support

;; Syntax Highlighting
(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  (cl-pushnew '(ng2-ts-mode . typescript) tree-sitter-major-mode-language-alist)
  (cl-pushnew '(ng2-html-mode . html) tree-sitter-major-mode-language-alist)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
(use-package tree-sitter-langs)

;; Debugging
(use-package realgud
  :defer t)
(use-package realgud-trepan-ni
  :straight (:type git :host github :repo "realgud/realgud-trepan-ni" :branch "master"))

;; Code Coverage
(use-package cov
  :defer t)
(use-package coverlay
  :commands (coverlay-mode))

;; Yaml editing support and JSON
;; json-mode => json-snatcher json-refactor
;; select yaml regex (^-[\s]*[A-Za-z0-9-_]*)|(^[A-Za-z_-]*:)
(use-package yaml-mode
  :mode (("\\.ya?ml$" . yaml-mode)
         ("\\.tpl$" . yaml-mode))
  :hook ((yaml-mode . aaronzinhoo-yaml-mode-hook))
  :preface
  (defun aaronzinhoo-company-yaml-mode-hook ()
    (set (make-local-variable 'company-backends) '((company-capf company-keywords company-dabbrev-code company-files))))
  (defun aaronzinhoo-yaml-expand-region-hook ()
    )
  (defun aaronzinhoo-yaml-mode-hook ()
    (flycheck-mode)
    (lsp)
    (hungry-delete-mode)
    (aaronzinhoo-company-yaml-mode-hook)
    (highlight-indentation-mode)
    (when (flycheck-may-enable-checker 'yaml-yamllint)
      (flycheck-select-checker 'yaml-yamllint))
    (flycheck-add-next-checker 'yaml-yamllint '(warning . lsp) 'append)))
;; use json-mode from https://github.com/joshwnj/json-mode for json instead of js-mode or js2-mode
(use-package json-mode
  :mode ("\\.json" . json-mode)
  :config
  (setq js-indent-level 2))
(use-package dotenv-mode
  :mode ("\\.env\\'" . dotenv-mode))
(use-package groovy-mode
  :defer t)
(use-package jenkinsfile-mode
  :mode ("\\Jenkinsfile\\'" . jenkinsfile-mode)
  :preface
  (defun aaronzinhoo-company-jenkinsfile-mode-hook ()
    (set (make-local-variable 'company-backends) '((company-capf company-keywords company-files))))
  :config
  (add-hook 'jenkinsfile-mode-hook 'aaronzinhoo-company-jenkinsfile-mode-hook))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DEVOPS CONFIG
(use-package docker
  :commands (docker)
  :bind ("C-c d" . docker))
(use-package docker-tramp
  :after (counsel-tramp))
(use-package docker-compose-mode
  :straight (:type git :host github :repo "aaronzinhoo/docker-compose-mode" :branch "master")
  :mode ("docker-compose\\'" . docker-compose-mode)
  :preface
  (defun aaronzinhoo-docker-compose-mode-hook ()
    (set (make-local-variable 'company-backends) '(company-capf company-keywords)))
  :config
  (add-hook 'docker-compose-mode-hook 'aaronzinhoo-docker-compose-mode-hook))
(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

;; WEB-DEV CONFIG

;; using verb instead because it is better
(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))
(use-package simple-httpd
  :defer t)
(use-package skewer-mode
  :defer t)
(use-package add-node-modules-path
  :hook ((rjsx-mode . add-node-modules-path)
         (typescript-mode . add-node-modules-path)
         (js2-mode . add-node-modules-path)
         (json-mode . add-node-modules-path)
         ;; add completion for css class names in html files
         (css-mode . add-node-modules-path)))
(use-package ac-html-csswatcher
  :hook (web-mode . company-web-csswatcher-setup)
  :config
  (ac-html-csswatcher-setup-html-stuff-async))
(use-package nxml-mode
  :straight nil
  :config
  (add-to-list 'auto-mode-alist
               (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss") t) "\\'")
                     'nxml-mode)))
(use-package emmet-mode
  :hook ((web-mode . emmet-mode)
         (ng2-html-mode . emmet-mode)))
(use-package helm-emmet)
(use-package html-check-frag
  :straight (:type git :host github :repo "TobiasZawada/html-check-frag" :branch "master")
  :hook (web-mode . html-check-frag-mode))
(use-package css-mode
  :straight nil
  :hook (css-mode . aaronzinhoo-company-css-mode-hook)
  :preface
  (defun aaronzinhoo-company-css-mode-hook ()
    (set (make-local-variable 'company-backends) '((company-bootstrap company-css company-files)))))
(use-package web-mode
  :straight (:type git :host github :repo "Aaronzinhoo/web-mode" :branch "master")
  :hook ((ng2-html-mode . web-mode)
         (web-mode . aaronzinhoo-company-web-mode-hook)
         )
  :mode (("\\.css\\$"  . web-mode)
         ("\\.html\\$" . web-mode)
         ("\\.component.html\\'" . web-mode)
         )
  :bind ((:map web-mode-map
               ("C-c h" . hydra-web/body)))
  :preface
  (defun aaronzinhoo-sgml-prettify-html ()
    """Use sgml to prettify HTML buffer and after pop the cursor to the original location"""
    (interactive)
    (mark-whole-buffer)
    (sgml-pretty-print (region-beginning) (region-end))
    (mark-whole-buffer)
    (indent-for-tab-command))
  (defun aaronzinhoo-delete-tag ()
    (interactive)
    (sgml-skip-tag-backward 1)
    (point-to-register 8)
    (sgml-skip-tag-forward 1)
    (backward-char)
    (web-mode-tag-beginning)
    (er/mark-outer-tag)
    (hungry-delete-backward 1)
    (jump-to-register 8)
    (er/mark-outer-tag)
    (hungry-delete-backward 1))
  ;; add company-capf to end otherwise lsp-mode will add it to the front of company-backends
  (defun aaronzinhoo-company-web-mode-hook ()
    (set (make-local-variable 'company-backends) '((company-capf company-web company-web-html company-bootstrap company-css company-files) company-capf)))
  (pretty-hydra-define hydra-web
    (:hint nil :title (with-octicon "globe" "Web Mode Control" 1 -0.05) :quit-key "q" :color pink)
    ("Navigation"
     (("a" sgml-skip-tag-backward "tag beginning | prev tag")
      ("e" sgml-skip-tag-forward "tag end | next tag")
      ("n" web-mode-element-next "next tag")
      ("p" web-mode-element-previous "previous tag")
      ("F" web-mode-element-children-fold-or-unfold "fold/unfold tag children")
      ("f" web-mode-fold-or-unfold "fold/unfold"))
     "Edit"
     (("t" aaronzinhoo-sgml-prettify-html "tidy html")
      ("d" aaronzinhoo-delete-tag "delete tag"))
     "Error"
     (("v" html-check-frag-next "next html error")
      ("E" web-mode-dom-errors-show "show errors"))
     "Action"
     (("w" web-mode-element-wrap "wrap element in tag" ));end action
     "Other"
     (("s" helm-emmet "Insert Emmet Snippet")
      ("RET" nil "Quit" :color blue))))
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
  (web-mode-enable-block-face t)
  (web-mode-enable-current-column-highlight t)
  (web-mode-enable-current-element-highlight t))
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))
(use-package grip-mode
  :custom
  ;; Use embedded webkit to previe
  ;; This requires GNU/Emacs version >= 26 and built with the `--with-xwidgets`
  ;; option.
  (grip-preview-use-webkit t))
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
(use-package ts-comint
  :commands (run-ts))
(use-package tide
  :straight (:type git :host github :repo "ananthakumaran/tide" :branch "master")
  :after (typescript-mode company flycheck)
  :bind (:map typescript-mode-map
              ("C-c h" . hydra-tide/body)
              ("C-c '" . nil))
  :hook (
         (tide-mode . eslintd-fix-mode)
         (rjsx-mode . aaronzinhoo-tide-rjsx-mode-hook))
  :preface
  (defun aaronzinhoo-tide-rjsx-mode-hook ()
    (aaronzinhoo-tide-mode-hook)
    (flycheck-select-checker 'javascript-tide))
  (defun aaronzinhoo-tide-ng2-mode-hook ()
    (aaronzinhoo-tide-mode-hook)
    (flycheck-add-mode 'typescript-tide 'ng2-ts-mode)
    (flycheck-add-next-checker 'typescript-tide '(warning . javascript-eslint) 'append))
  (defun aaronzinhoo-tide-mode-hook ()
    (tide-setup)
    (tide-hl-identifier-mode)
    (set (make-local-variable 'company-backends) '((company-tide company-files)))
    (setq company-idle-delay 0.1))
  :custom
  (typescript-indent-line 2)
  :init
  (pretty-hydra-define hydra-tide
    (:hint nil :title (with-fileicon "typescript" "Tide Control" 1 -0.05) :quit-key "q" :color pink)
    ("Navigation"
     (("j" tide-jump-to-definition "goto def")
      ("J" tide-jump-back "jump back")
      ("r" tide-references "get refs" :color blue))
     "Edit"
     (("f" tide-rename-file "rename current file"))
     "Imports"
     (("i" import-js-fix "fix")
      ("o" tide-organize-imports "organize"))
     "Error"
     (("e" tide-find-next-error "next error"))
     "Action"
     (("d" tide-documentation-at-point "documentation")
      ("R" run-ts "run TS REPL" :color blue))
     "Other"
     (("RET" nil "Quit" :color blue))))
  :config
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
  (flycheck-add-mode 'javascript-eslint 'ng2-ts-mode)
  (flycheck-add-mode 'typescript-tslint 'ng2-ts-mode)
  (flycheck-add-next-checker 'typescript-tide '(warning . javascript-eslint) 'append)
  ;; (flycheck-add-next-checker 'javascript-tide '(warning . javascript-eslint) 'append)
  (if (file-exists-p (concat tide-project-root "node_modules/typescript/bin/tsserver"))
      (setq tide-tsserver-executable "node_modules/typescript/bin/tsserver")))
(use-package eslintd-fix
  :defer t
  :config
  (setq flycheck-javascript-eslint-executable "eslint_d"))
(use-package import-js
  :hook ((rjsx-mode . aaronzinhoo-run-import-js-hook)
         (typescript-mode . aaronzinhoo-run-import-js-hook))
  :preface
  (defun aaronzinhoo-run-import-js-hook ()
    (run-import-js)))
(use-package prettier-js
  :diminish
  :hook ((markdown-mode . prettier-js-mode)
         (json-mode . prettier-js-mode)
         (css-mode . prettier-js-mode)
         (rjsx-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode))
  :config
  (setq prettier-js-args '("--bracket-spacing" "false")))
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
;;angular setup
(use-package typescript-mode
  :defer t)
(use-package ng2-mode
  :defer t
  :mode (("\\component.ts\\'" . ng2-mode)
         ("\\.service.spec.ts\\'" . ng2-ts-mode)
         ("\\.component.spec.ts\\'" . ng2-ts-mode)
         ("\\.view.spec.ts\\'" . ng2-ts-mode)
         ("\\component.html\\'" . web-mode)))
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
  (exec-path-from-shell-arguments nil)
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
  :preface
  (defun aaronzinho-python-buffer-setup ()
    (setq python-indent-offset 4)
    (setq-local highlight-indentation-offset 4))
  :custom
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "--simple-prompt")
  (python-check-command "flake8")
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
  (add-hook 'python-mode-hook 'aaronzinho-python-buffer-setup)
  (add-to-list 'process-coding-system-alist '("python" . (utf-8 . utf-8))))

;; MAY HAVE TO CHANGE PYTHON PATH
(use-package elpy
  :diminish ""
  :init (with-eval-after-load 'python (elpy-enable))
  :hook (elpy-mode . flycheck-mode)
  :preface
  (defun aaronzinhoo-company-elpy-setup ()
    (add-to-list 'company-backends 'elpy-company-backend))
  :custom
  (elpy-shell-echo-output nil)
  (elpy-rpc-virtualenv-path "~/.config/emacs/elpy/rpc-venv")
  (elpy-rpc-backend "jedi")
  (elpy-shell-starting-directory 'current-directory)
  (elpy-syntax-check-command "~/.config/emacs/elpy/rpc-venv/bin/flake8")
  :config
  (add-hook 'python-mode-hook 'aaronzinhoo-company-elpy-setup)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-to-list 'process-coding-system-alist '("elpy" . (utf-8 . utf-8))))
(use-package pyenv-mode
  :hook (elpy-mode . pyenv-mode)
  :bind
  ("C-x p e" . pyenv-activate-current-project)
  :init
  (add-to-list 'exec-path "~/.pyenv/shims")
  (setenv "WORKON_HOME" "~/.pyenv/versions/"))
(use-package pyenv-mode-auto
  :straight (:type git :host github :repo "ssbb/pyenv-mode-auto" :branch "master"))
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

;;; Rust
(use-package toml-mode)
(use-package rustic
  :custom
  (rustic-lsp-server 'rls))

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

;; load custom faces, vars, & keybindings for packages
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file) (load custom-file))
(load (concat user-init-dir "/aaronzinhoo-custom-keybindings.el"))
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
;;; init.el ends here
