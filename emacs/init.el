;; package --- Summary --- -*- lexical-binding: t -*-
;;; Commentary:
;;;init.el --- Emacs configuration

;;; Code:
;; load the early init file if this is not a recent emacs
(message "Initializing settings...")
(when (version< emacs-version "27")
  (load (concat user-emacs-directory "early-init.el")))
;; load init packages
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name (concat dir "elisp/init"))))
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name (concat dir "elisp/pair-navigation"))))
;; load the utils for some helper functions
(require 'init-constants)
(require 'init-defaults)
(require 'init-straight)
(require 'init-fonts)
(require 'init-keybindings)
(require 'init-utils)
(require 'pair-navigator)

(message "Loading packages")
;; overlay to help display where other paren is unobtrusively
;; ov is enclosed in show-paren--off-screen+
;; TODO: figure out where to put this damn function
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

;;; Packages

;; built-in
(use-package simple
  :straight nil
  :config
  (column-number-mode t)
  (global-visual-line-mode t))
(use-package delsel
  :straight nil
  :config
  (delete-selection-mode t))
(use-package paren
  :straight nil
  :custom
  (show-paren-style 'paren)
  (show-paren-delay 0.03)
  (show-paren-highlight-openparen t)
  (show-paren-when-point-inside-paren nil)
  (show-paren-when-point-in-periphery t)
  :config
  (show-paren-mode t))
(use-package system-packages
  :straight t
  :custom
  (system-packages-use-sudo nil)
  :init
  (when (eq system-type 'darwin)
    (setq system-packages-package-manager 'brew)))
(use-package display-line-numbers
  :straight nil
  :hook ((conf-mode . display-line-numbers-mode)
         (text-mode . display-line-numbers-mode)
         (prog-mode . display-line-numbers-mode))
  :config
  (dolist (mode '(org-mode-hook
                  term-mode-hook
                  dashboard-mode-hook
                  shell-mode-hook
                  treemacs-mode-hook
                  compilation-mode-hook
                  vterm-mode-hook
                  eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0)))))
(use-package elec-pair
  :demand t
  :straight nil
  :hook ((git-commit-mode . git-commit-add-electric-pairs)
         (org-mode . org-add-electric-pairs)
         (markdown-mode . markdown-add-electric-pairs)
         (yaml-mode . yaml-add-electric-pairs))
  :preface
  (defun git-commit-add-electric-pairs ()
    (setq-local electric-pair-pairs (append electric-pair-pairs '((?` . ?`) (?= . ?=))))
    (setq-local electric-pair-text-pairs electric-pair-pairs))
  ;; setup electric-pairs mode for org-mode
  (defun org-add-electric-pairs ()
    (setq-local electric-pair-pairs (append electric-pair-pairs '((?/ . ?/) (?= . ?=))))
    (setq-local electric-pair-text-pairs electric-pair-pairs))
  (defun markdown-add-electric-pairs ()
    (setq-local electric-pair-pairs (append electric-pair-pairs '((?` . ?`))))
    (setq-local electric-pair-text-pairs electric-pair-pairs))
  (defun yaml-add-electric-pairs ()
    (setq-local electric-pair-pairs (append electric-pair-pairs '((?( . ?)))))
    (setq-local electric-pair-text-pairs electric-pair-pairs))
  :init
  ;; disable <> auto pairing in electric-pair-mode for org-mode
  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local electric-pair-inhibit-predicate
                          `(lambda (c)
                             (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
  :config
  (electric-pair-mode t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package s :straight t)
(if (version< emacs-version "27.1")
    (use-package cl))
;; garbage collector magic
(use-package gcmh
  :straight t)
(use-package gh :straight t)
(use-package async :straight t)
(use-package f
  :straight (:type git :host github :repo "rejeep/f.el" :branch "master"))
(use-package pcre2el :straight t)
(use-package compat :straight t)
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
(use-package undo-fu-session
  :custom
  (undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  :config
  (global-undo-fu-session-mode))
(use-package undo-fu) ;; for hydra check hydra config
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
(use-package no-littering
  :demand t
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

;; SSH Config
(use-package ssh-agency
  :if (string-equal system-type "windows-nt"))
(use-package ssh-config-mode
  :hook ((ssh-config-mode . aaronzinhoo-ssh-config-mode-hook)
         (ssh-config-mode . er/add-ssh-config-mode-expansions))
  :preface
  (defun aaronzinhoo-ssh-config-mode-hook ()
    (set (make-local-variable 'company-backends) '((company-capf company-keywords company-dabbrev-code company-files)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  :straight (:type git :host github :repo "dholm/benchmark-init-el")
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))
;; TODO: once add projectile, have this hook to projectile
;; Hydra
(use-package block-nav
  :straight (:type git :host github :repo "nixin72/block-nav.el")
  :custom
  (block-nav-move-skip-shallower t)
  (block-nav-center-after-scroll t))

(use-package hydra
  :demand t
  :bind
  ("s-SPC" . hydra-nav/body)
  ("s-f" . hydra-flycheck/body)
  ("s-o" . hydra-org/body)
  ("s-p" . hydra-projectile/body)
  ("s-i" . hydra-ivy/body)
  ("s-/" . undo-and-activate-hydra-undo)
  ("s-B" . hydra-bookmark/body)
  :custom
  (hydra-default-hint nil))
(use-package major-mode-hydra
  :demand t
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
  (defun undo-and-activate-hydra-undo ()
    (interactive)
    (undo-fu-only-undo)
    (hydra-undo-fu/body))
  :config
  (pretty-hydra-define hydra-flycheck
    (:hint nil :color teal :quit-key "SPC" :title (with-faicon "plane" "Flycheck" 1 -0.05))
    ("Checker"
     (("?" flycheck-describe-checker "describe")
      ("d" flycheck-disable-checker "disable")
      ("m" flycheck-mode "mode")
      ("s" flycheck-select-checker "select"))
     "Errors"
     (("p" flycheck-previous-error "previous" :color pink)
      ("n" flycheck-next-error "next" :color pink)
      ("l" flycheck-list-errors "list errors"))
     "Other"
     (("r" recenter-top-bottom "recenter" :color pink)
      ("M" flycheck-manual "manual")
      ("v" flycheck-verify-setup "verify setup"))))
  (pretty-hydra-define hydra-org
    (:hint nil :color pink :quit-key "SPC" :title (with-fileicon "org" "Org" 1 -0.05))
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
    (:hint nil :color pink :quit-key "SPC" :title (with-fileicon "org" "Org" 1 -0.05))
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
     (("N" org-toggle-narrow-to-subtree "narrow/unarrow")
      ("r" org-refile "refile")
      ("v" org-overview "overview" :color blue)
      ("a" outline-show-all "show-all" :color blue))
     "Other"
     (("RET" nil :color blue))))
  (pretty-hydra-define hydra-ivy
    (:hint nil :color teal :quit-key "SPC" :title (with-faicon "tree" "Ivy" 1 -0.05))
    ("Counsel Functions"
     (("f" counsel-recentf "recent-file")
      ("t" counsel-faces "faces")
      ("m" counsel-imenu "imenu")
      ("l" counsel-find-library "library"))
     "Ivy Actions"
     (("e" ivy-call "execute command")
      ("r" ivy-resume "resume"))
     "Text"
     (("i" ivy-insert-current "insert current cand." :color red))
     "Other"
     (("s" counsel-info-lookup-symbol "symbol")
      ("u" counsel-unicode-char "unicode"))))
  (pretty-hydra-define hydra-lsp
    (:hint nil :color pink :quit-key "SPC" :title (with-faicon "cog" "LSP" 1 -0.05))
    ("Goto"
     (("r" lsp-find-references "refs")
      ("d" lsp-find-definition "defs")
      ("i" lsp-goto-implementation "implementation (interface)")
      ("t" lsp-find-type-definition "type-def")
      ("b" xref-pop-marker-stack "pop back" :color red))
     "Refactor"
     (("f" lsp-format-buffer "format")
      ("n" lsp-rename "rename"))
     "UI"
     (("p" lsp-ui-peek-mode "peek-mode")
      ("R" lsp-ui-peek-find-references "peek-refs" :color red)
      ("D" lsp-ui-peek-find-definitions "peek-defs" :color red)
      ("m" lsp-ui-imenu "peek-menu"))
     "Server"
     (("s" lsp-describe-session "session")
      ("I" lsp-install-server "install")
      ("S" lsp-workspace-restart "restart"))))
  (pretty-hydra-define hydra-undo-fu
    (:hint nil :color red :quit-key "SPC" :title (with-faicon "undo" "Undo/Redo" 1 -0.05))
    ("Action"
     (("/" undo-fu-only-undo "Undo")
      ("r" undo-fu-only-redo "Redo")
      ("RET" nil "Quit" :color blue))))
  (pretty-hydra-define hydra-nav
    (:hint nil :color amaranth :quit-key "SPC" :title (with-faicon "cog" "Navigation" 1 -0.05))
    ("Buffer"
     (("a" crux-move-beginning-of-line "Begin Line")
      ("z" end-of-visual-line "End Line")
      ("s" swiper "Search"))
     "Block"
     (("d" block-nav-previous-block "Block Up")
      ("c" block-nav-next-block "Block Down")
      ("C" block-nav-next-indentation-level "Indent Up")
      ("D" block-nav-previous-indentation-level "Indent Down"))
     "Avy"
     (("j" avy-goto-char-timer "Jump Char(s)")
      ("g" avy-goto-line "Jump Line"))
     "Pair"
     (("[" pair-navigator-backward-left-bracket "Up pair")
      ("]" pair-navigator-forward-right-bracket "Down pair")
      ("p" pair-navigator-goto-matching-bracket "Matching pair"))
     "Text"
     (("f" forward-word "Forward Word")
      ("v" backward-word "Backward Word"))
     "Copy/Paste"
     (("r" er/contract-region "Contract Region")
      ("e" er/expand-region "Expand Region")
      ("w" easy-kill "Copy")
      ("q" yank "Paste"))
     "Project"
     (("S" counsel-projectile-rg "Search" :color blue)
      ("n" counsel-projectile-find-file "Find File" :color blue)
      ("m" counsel-projectile-switch-to-buffer "Switch Buffer" :color blue))
     "Window"
     (("b" ace-window "Switch to window")
      ("x" delete-window "Delete Window"))))
  (pretty-hydra-define hydra-bookmark
    (:hint nil :color teal :quit-key "SPC" :title (with-faicon "bookmark" "Bookmark" 1 -0.05))
    ("Burly"
     (("o" burly-open-bookmark "Open Burly Bookmark")
      ("b" burly-open-last-opened-bookmark "Most Recently Opened Bookmark")
      ("s" burly-bookmark-windows "Bookmark Windows"))
     "Cycle"
     (("c" bmkp-cycle "Cycle Bookmarks" :color red))
     "Jump"
     (("j" counsel-bookmark "Jump to bookmark"))
     "List"
     (("l" bookmark-bmenu-list "List Bookmarks")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CONTROL VERSION UTILS
(use-package git-gutter
  :after (all-the-icons)
  :straight (:type git :host github :repo "emacsorphanage/git-gutter" :branch "master")
  :hook (prog-mode . git-gutter-mode)
  :bind ("s-g" . hydra-git-gutter/body)
  :commands (git-gutter-mode)
  :diminish git-gutter-mode
  :preface
  (pretty-hydra-define hydra-git-gutter
    (:hint nil :color "pink" :quit-key "SPC" :title (with-octicon "diff" "Diff" 1 -0.05))
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
  :hook (org-mode . hl-todo-mode)
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
(use-package magit-todos)
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
         ("RET" . magit-diff-visit-file-other-window)
         ("M-i" . magit-section-backward)
         ("M-k" . magit-section-forward)
         ("M-t" . magit-todos-mode))
  :hook (magit-mode . magit-auto-revert-mode)
  :custom
  (magit-completing-read-function 'ivy-completing-read)
  :config
  (setq magit-bind-magit-project-status nil)
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
  :straight (:type git :host github :repo "dajva/rg.el" :branch "master")
  :commands (rg rg-dwim rg-menu)
  :bind* ("s-r" . rg-menu)
  :hook (rg-mode . (lambda () (switch-to-buffer-other-window (current-buffer))))
  :custom
  (rg-executable "rg")
  :config
  (rg-enable-menu))
(use-package hungry-delete
  :demand t
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
  (dired-dwim-target t)
  (delete-by-moving-to-trash t)
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  ;; revert dired buffers but dont state it
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  (dired-auto-revert-buffer t)
  (dired-listing-switches "-lAXGh --group-directories-first")
  :init
  (when (memq window-system '(mac ns))
    (setq dired-use-ls-dired t
          insert-directory-program "gls")))
;;; use to search files in multiple directories and place in one
(use-package fd-dired
  :commands (fd-dired fd-name-dired fd-grep-dired)
  :config
  (setq fd-dired-program "fdfind"))
(use-package dired-single
  :preface
  ;; taken from https://github.com/ianyepan/.macOS-emacs.d/blob/master/init.el#L605
  (defun aaronzinhoo-dired-single-init ()
    (define-key dired-mode-map [return] #'dired-single-buffer)
    (define-key dired-mode-map [remap dired-mouse-find-file-other-window] #'dired-single-buffer-mouse)
    (define-key dired-mode-map [remap dired-up-directory] #'dired-single-up-directory))
  :config
  (if (boundp 'dired-mode-map)
      (aaronzinhoo-dired-single-init)
    (add-hook 'dired-load-hook #'aaronzinhoo-dired-single-init)))
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
  :demand t
  :custom
  (recentf-exclude '("~$" "/tmp/" "/ssh:" "/sudo:" "/sftp:"))
  (recentf-max-menu-items 500)
  (recentf-max-saved-items 500)
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
  :bind ("s-w" . resize-window))
(use-package winner
  :straight nil
  :config
  (winner-mode 1))
(use-package ace-window
  :commands ace-window
  :bind* ("s-b" . ace-window)
  :custom
  (aw-ignore-current t)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))
(use-package transpose-frame
  :commands (transpose-frame  rotate-frame-anticlockwise rotate-frame-clockwise))
;;; window management hydra?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package page-break-lines
  :defer t)
(use-package dashboard
  :demand t
  :straight t
  :custom
  (dashboard-set-init-info t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-banner-logo-title "Welcome to your Emacs Dashboard")
  (dashboard-items '((recents  . 5)
                     (projects . 5)
                     (bookmarks . 5)))
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
  (dolist (mode '(term-mode-hook
                  dashboard-mode-hook
                  shell-mode-hook
                  treemacs-mode-hook
                  compilation-mode-hook
                  vterm-mode-hook
                  eshell-mode-hook))
    (add-hook mode (lambda () (beacon-mode 0))))
  (beacon-mode 1))
(use-package which-key
  :straight t
  :diminish
  :custom
  (which-key-use-C-h-commands nil)
  :config
  (which-key-setup-side-window-right-bottom)
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
  :preface
  (defvar-local flycheck-local-checkers nil)
  (defun +flycheck-checker-get(fn checker property)
    (or (alist-get property (alist-get checker flycheck-local-checkers))
        (funcall fn checker property)))
  (advice-add 'flycheck-checker-get :around '+flycheck-checker-get)
  :custom
  (flycheck-stylelintrc "~/.stylelintrc")
  (flycheck-css-stylelint-executable "stylelint")
  (flycheck-yamllintrc "~/.yamllintrc")
  (flycheck-rust-cargo-executable (concat user-home-directory "/.cargo/bin/cargo"))
  :config
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint c/c++-clang c/c++-cppcheck c/c++-gcc)))
  (flycheck-add-mode 'yaml-yamllint 'docker-compose-mode)
  (flycheck-add-mode 'json-jsonlint 'json-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  ;; eslint requires you to be careful with the configuration
  ;; ensure to use .json files and setup accordingly
  ;; test with shell command
  (flycheck-add-mode 'javascript-eslint 'typescript-mode)
  (flycheck-add-mode 'css-stylelint 'css-mode)
  (flycheck-add-mode 'dockerfile-hadolint 'dockerfile-mode)
  (flycheck-add-mode 'sh-shellcheck 'sh-mode)
  )
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
  (er/enable-mode-expansions 'typescript-mode 'er/add-rjsx-mode-expansions)
  (er/enable-mode-expansions 'rjsx-mode 'er/add-rjsx-mode-expansions)
  (er/enable-mode-expansions 'web-mode 'er/add-web-mode-expansions))
(use-package all-the-icons
  :straight t)
(use-package emojify
  :if (display-graphic-p)
  :hook (prog-mode . (lambda () (emojify-mode 0)))
  :config
  (global-emojify-mode))
(use-package yasnippet
  :defer t
  :diminish yas-minor-mode
  :commands yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :config
  (use-package yasnippet-snippets)
  (yas-reload-all))
(use-package lsp-mode
  :straight (:type git :host github :repo "emacs-lsp/lsp-mode" :branch "master")
  :commands (lsp lsp-deferred)
  :hook (((c-mode        ; clangd
           c++-mode  ; clangd
           go-mode
           sql-mode
           html-mode
           web-mode
           typescript-mode
           rustic-mode
           dockerfile-mode
           sh-mode
           python-mode
           ) . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . yas-minor-mode))
  :bind
  (:map lsp-mode-map
        ("s-l" . hydra-lsp/body))
  :preface
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook 'lsp-format-buffer)
    (add-hook 'before-save-hook 'lsp-organize-imports)
    (setq lsp-gopls-staticcheck t)
    (setq lsp-eldoc-render-all t)
    (setq lsp-gopls-complete-unimported t))
  :custom
  (lsp-treemacs-sync-mode t)
  (lsp-auto-guess-root t)
  (lsp-log-io nil)
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
     (,(intern "https://raw.githubusercontent.com/compose-spec/compose-spec/master/schema/compose-spec.json") . ["*-compose.y*"])
     (,(intern "https://json.schemastore.org/kustomization.json") . ["kustomization.yaml"])
     (kubernetes . ["*.yaml"])))
  (lsp-clients-angular-language-server-command
   `("node"     ,(concat user-home-directory "/.nvm/versions/node/v14.19.0/lib/node_modules/@angular/language-server")
     "--ngProbeLocations"
     ,(concat user-home-directory "/.nvm/versions/node/v14.19.0/lib/node_modules")
     "--tsProbeLocations"
     ,(concat user-home-directory "/.nvm/versions/node/v14.19.0/lib/node_modules")
     "--stdio"))
  :config
  (push '(web-mode . "html") lsp-language-id-configuration)
  (push '(docker-compose-mode . "yaml") lsp-language-id-configuration)
  (push '(yaml-mode . "yaml") lsp-language-id-configuration)
  (setq gc-cons-threshold  100000000)
  (setq read-process-output-max (* 1024 1024)) ;;1MB
  (add-hook 'go-mode-hook 'lsp-go-install-save-hooks))
(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)
(use-package lsp-ivy
  :after (lsp-mode ivy))
(use-package lsp-ui
  :commands lsp-ui-mode
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  :custom
  (lsp-ui-sideline-show-code-actions nil)
  (lsp-ui-peek-enable t)
  (lsp-ui-doc-use-webkit t)
  (lsp-ui-doc-enable nil))
(use-package lsp-java
  :after (treemacs)
  :straight (:type git :host github :repo "emacs-lsp/lsp-java" :branch "master")
  :hook ((java-mode . lsp)
         (java-mode . lsp-lens-mode)
         (java-mode . lsp-java-boot-lens-mode))
  :bind (:map java-mode-map
              ("C-c h" . hydra-java-mode/body))
  :preface
  (pretty-hydra-define hydra-java-mode
    (:hint nil :color pink :quit-key "SPC" :title (with-alltheicon "java" "Java Mode" 1 -0.05))
    ("Class"
     (("cg" lsp-java-generate-getters-and-setters "Generate [S|G]etters")
      ("co" lsp-java-generate-overrides "Generate Overides")
      ("cu" lsp-java-add-unimplemented-methods "Add Unimplemented Methods")
      ("ct" lsp-java-add-throws "Add Throws"))
     "Import"
     (("a" lsp-java-add-import "Add")
      ("o" lsp-java-organize-imports "Organize"))
     "Notifications"
     (("n" lsp-java-resolve-actionable-notifications "Resolve Notifications"))
     "Project Management"
     (("ps" lsp-java-spring-initializr "Spring Init" :color blue)
      ("pd" lsp-dependency-list "List Dependencies"))
     "Test"
     (("tb" lsp-jt-browser "Test Browser" :color blue)
      ("tl" lsp-jt-lens-mode "Testing Lens Mode" :toggle t))))
  :config
  (require 'lsp-java-boot)
  (let ((lombok-file (concat user-init-dir-fullpath "/deps/lombok-1.18.12.jar")))
    (setq lsp-java-vmargs
          (list "-noverify"
                "-Xmx4G"
                "-XX:+UseG1GC"
                "-XX:+UseStringDeduplication"
                (concat "-javaagent:" lombok-file)))))
(use-package lsp-pyright
  :after (lsp)
  :straight (:type git :host github :repo "emacs-lsp/lsp-pyright" :branch "master")
  :if (executable-find "pyright")
  :custom
  (lsp-pyright-venv-path (concat user-home-directory ".pyenv/versions"))
  (lsp-pyright-python-executable-cmd "python3")
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-pyright)
                   (lsp-deferred))))
(use-package company
  :straight (company :files (:defaults "icons"))
  :diminish company-mode
  :hook ((prog-mode . company-mode)
         (org-mode . company-mode))
  :bind
  ([remap indent-for-tab-command] . company-indent-or-complete-common)
  (:map company-active-map
        ("<tab>" . company-complete-common-or-cycle)
        ("<backtab>" . company-select-previous))
  :custom
  (company-tooltip-idle-delay 0.1)
  (company-tooltip-minimum-width 40)
  (company-tooltip-maximum-width 80)
  :preface
  (defun company-yasnippet/disable-after-slash (fun command &optional arg &rest _ignore)
    (if (eq command 'prefix)
        (let ((prefix (funcall fun 'prefix)))
          (when (and prefix (not
                             (eq
                              (char-before (- (point) (length prefix)))
                              ?/)))
            prefix))
      (funcall fun command arg)))
  (defun company-yasnippet/disable-after-dot (fun command &optional arg &rest _ignore)
    (if (eq command 'prefix)
        (let ((prefix (funcall fun 'prefix)))
          (when (and prefix (not
                             (eq
                              (char-before (- (point) (length prefix)))
                              ?.)))
            prefix))
      (funcall fun command arg)))
  (defun company-preview-if-not-tng-frontend (command)
    "`company-preview-frontend', but not when tng is active."
    (unless (and (eq command 'post-command)
                 company-selection-changed
                 (memq 'company-tng-frontend company-frontends))
      (company-preview-frontend command)))
  :init
  (setq company-format-margin-function #'company-vscode-dark-icons-margin)
  (setq company-idle-delay 0.0
        company-echo-delay 0 ;; remove annoying blinking
        company-tooltip-flip-when-above t
        company-tooltip-limit 20
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
  (setq company-frontends
        '(company-preview-if-not-tng-frontend
          company-pseudo-tooltip-frontend
          company-preview-if-just-one-frontend
          company-echo-metadata-frontend))
  (setq company-backends '(company-capf company-keywords company-files company-ispell company-dabbrev))
  :config
  (advice-add #'company-yasnippet :around #'company-yasnippet/disable-after-dot)
  (advice-add #'company-yasnippet :around #'company-yasnippet/disable-after-slash)
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
  :after (company)
  :init
  (require 'company-web-html))
(use-package company-jedi
  :commands (jedi:goto-definition jedi-mode company-jedi)
  :bind (:map jedi-mode-map
              ("M-d" . jedi:goto-definition)
              ("M-b" . jedi:goto-definition-pop-marker)))
(use-package company-org-block
  :straight (:type git :host github :repo "aaronzinhoo/company-org-block" :branch "master"))
(use-package imenu-list
  :bind (("s-m" . imenu-list-smart-toggle))
  :custom
  (imenu-list-focus-after-activation t)
  (imenu-list-auto-resize t))
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
          :map ivy-switch-buffer-map
          ("C-k" . ivy-switch-buffer-kill)
          :map ivy-minibuffer-map
          ("C-c o" . ivy-occur)
          ("M-j" . nil)
          ("C-j" . ivy-immediate-done)
          ("DEL" . ivy-backward-delete-char)
          :map ivy-occur-grep-mode-map
          ("C-c h" . hydra-ivy-occur/body)
          :map swiper-map
          ("C-SPC" . swiper-avy))
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :preface
  (defun ivy-update-candidates-dynamic-collection-workaround-a (old-fun &rest args)
    (cl-letf (((symbol-function #'completion-metadata) #'ignore))
      (apply old-fun args)))
  (pretty-hydra-define hydra-ivy-occur
    (:hint nil :color pink :quit-key "SPC" :title (with-faicon "tree" "Ivy-Occur" 1 -0.05))
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
  (ivy-height 20)
  :config
  (setq swiper-use-visual-line-p (lambda (_) nil))
  ;; fix for async display of counsel-rg resuls
  (advice-add #'ivy-update-candidates :around #'ivy-update-candidates-dynamic-collection-workaround-a))
(use-package counsel-tramp
  :commands (counsel-tramp))
(use-package counsel-projectile
  :after projectile
  :straight (:type git :host github :repo "ericdanan/counsel-projectile")
  :config
  (counsel-projectile-mode t))
;; load before ivy-rich for better performance
(use-package all-the-icons-ivy-rich
  :hook (ivy-mode . all-the-icons-ivy-rich-mode))
(use-package ivy-avy
  :after (ivy)
  :bind* (:map ivy-minibuffer-map
               ("C-SPC" . ivy-avy)))
(use-package ivy-rich
  :after ivy
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
  ;; All the icon support to ivy-rich
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
           (lambda (cand) (get-buffer cand)))
          counsel-projectile-find-file
          (:columns
           ((ivy-rich-switch-buffer-icon (:width 2))
            (ivy-rich-candidate (:width 50)))
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
  :bind* ("M-SPC" . avy-goto-char-timer)
  :custom
  (avy-all-windows nil))
;; TODO add fix for mark outer-tag
(use-package multiple-cursors
  :straight (:type git :host github :repo "magnars/multiple-cursors.el" :branch "master")
  :bind (("M-m" . hydra-multiple-cursors/body))
  :hook ((prog-mode . multiple-cursors-mode)
         (text-mode . multiple-cursors-mode))
  :init
  (pretty-hydra-define hydra-multiple-cursors
    (:hint nil :color pink :quit-key "SPC" :title (with-faicon "key" "Multiple Cursors" 1 -0.05))
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
     "Mark All"
     (("a" mc/mark-all-like-this "Mark All")
      ("d" mc/mark-all-dwim "Mark All DWIM"))
     "Misc."
     (("2" er/expand-region "Expand Region")
      ("h" mc-hide-unmatched-lines-mode "Hide lines" :toggle t)
      ("RET" nil "Quit"))))
  :custom
  (mc/cmds-to-run-for-all
   '(abbrev-prefix-mark
     crux-smart-delete-line
     hungry-delete-backward
     hungry-delete-forward))
  (mc/cmds-to-run-once '(avy-goto-char-timer counsel-M-x dap-tooltip-mouse-motion hydra-multiple-cursors/body hydra-multiple-cursors/mc-hide-unmatched-lines-mode hydra-multiple-cursors/mc/edit-lines-and-exit hydra-multiple-cursors/mc/mark-all-dwim hydra-multiple-cursors/mc/mark-all-like-this hydra-multiple-cursors/mc/mark-all-like-this-and-exit hydra-multiple-cursors/mc/mark-next-like-this hydra-multiple-cursors/mc/mark-previous-like-this hydra-multiple-cursors/mc/nil hydra-multiple-cursors/mc/skip-to-next-like-this hydra-multiple-cursors/mc/skip-to-previous-like-this hydra-multiple-cursors/mc/unmark-next-like-this hydra-multiple-cursors/mc/unmark-previous-like-this mc/mark-previous-like-this wgrep-finish-edit)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Creating Diagrams
(use-package plantuml-mode
  :straight (:type git :host github :repo "Aaronzinhoo/plantuml-mode" :branch "master")
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
  :after (org)
  :defer t)
(use-package ob-typescript)
;;; sudo apt install phantomjs
(use-package ob-browser)
;; better way to test APIs (like postman but with org files!)
;; must keep here since org uses ob-verb
(use-package verb
  :after (org)
  :commands (verb-mode)
  :preface
  (pretty-hydra-define hydra-verb-mode
    (:hint nil :color pink :quit-key "SPC" :title (with-faicon "viacoin" "Verb Mode" 1 -0.05))
    ("Request"
     (("rs" verb-send-request-on-point-other-window-stay "Other Window (Stay)")
      ("ro" verb-send-request-on-point-other-window "Other Window")
      ("rc" verb-send-request-on-point "Current Window"))
     "Kill"
     (("k" verb-kill-all-response-buffers "All Response Buffers"))
     )))
(use-package org
  :mode (("\\.org$" . org-mode))
  :hook ((org-mode . aaronzinhoo-org-setup)
         (org-mode . aaronzinhoo-org-font-setup))
  :bind
  ("C-c l" . org-store-link)
  ("C-c A" . org-agenda)
  ("C-c c" . org-capture)
  (:map org-mode-map
        ("C-M-<return>" . org-insert-subheading)
        ("C-c h". hydra-org-nav/body)
        ("s-/" . undo-and-activate-hydra-undo)
        ("s-v" . hydra-verb-request/body))
  :preface
  (defun org-keyword-backend (command &optional arg &rest ignored)
    (interactive (list 'interactive))
    (cl-case command
      (interactive (company-begin-backend 'org-keyword-backend))
      (prefix (and (eq major-mode 'org-mode)
                   (cons (company-grab-line "#\\+\\(\\w*\\)" 1)
                         t)))
      (candidates (mapcar #'upcase
                          (cl-remove-if-not
                           (lambda (c) (string-prefix-p arg c))
                           (pcomplete-completions))))
      (ignore-case t)
      (duplicates t)))
  (defun aaronzinhoo-org-inline-css-hook (exporter)
    "Insert custom inline css"
    (when (eq exporter 'html)
      (let* ((dir (ignore-errors (file-name-directory (buffer-file-name))))
             (path (concat dir "style.css"))
             (homestyle (or (null dir) (null (file-exists-p path))))
             (final (if homestyle (concat user-init-dir "org/sakura-dark-theme.css") path)))
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
    (dolist (face '((org-level-1 . 1.75)
                    (org-level-2 . 1.5)
                    (org-level-3 . 1.25)
                    (org-level-4 . 1.15)
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
  (org-publish-project-alist
   `(("blog-pages"
      :base-directory ,(concat org-directory "/personal/blog/src")
      :base-extension "org"
      :publishing-directory ,(concat org-directory "/personal/blog/public")
      :publishing-function org-html-publish-to-html
      :recursive t
      :auto-sitemap t
      :sitemap-title "Blog Posts"
      :sitemap-filename "index.org"
      :sitemap-sort-files anti-chronologically)
     ("blog-static"
      :base-directory ,(concat org-directory "/personal/blog/src/assets/")
      :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
      :publishing-directory ,(concat org-directory "/personal/blog/public/assets/")
      :recursive t
      :publishing-function org-publish-attachment)
     ("blog"
      :components ("blog-pages" "blog-static"))))
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
  (org-export-with-section-numbers nil)
  (org-export-with-toc nil)
  (org-html-postamble t)
  (org-html-postamble-format
   '(("en" "<p class=\"footer\">%a &nbsp; | &nbsp; %e | &nbsp; %C</p>")))
  (org-html-link-home "/")
  (org-html-link-up ".")
  (org-html-use-infojs t)
  (org-html-infojs-options
   '((path . "/js/org-info.js")
     (view . "showall")
     (toc . "0")
     (ftoc . "0")
     (tdepth . "max")
     (sdepth . "max")
     (mouse . "underline")
     (buttons . "nil")
     (ltoc . "0")
     (up . :html-link-up)
     (home . :html-link-home)))
  (org-plantuml-exec-mode "plantuml")
  :init
  ;; view items using emacs browser
  (if my/wsl
      (progn
        (setq browse-url-browser-function 'browse-url-generic browse-url-generic-program "wslview")))
  (add-hook 'org-export-before-processing-hook 'aaronzinhoo-org-inline-css-hook)
  :config
  (define-key org-mode-map (kbd "s-v") verb-command-map)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python     . t)
     (C          . t)
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
                    '(company-capf company-org-block company-ispell org-keyword-backend company-dabbrev))))
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
  (require 'ox-publish)
  )
(use-package org-contrib
  :after org)
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
  :after org
  :straight (asoc :type git :host github :repo "troyp/asoc.el"))
(use-package org-capture-ref
  :after asoc
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
  (pdf-view-display-size 'fit-width))

;;; Application Framework
;; (use-package eaf
;;   :straight (:type git
;;                    :host github
;;                    :repo "manateelazycat/emacs-application-framework"
;;                    :files ("*"))
;;   ;; :load-path (concat user-emacs-directory "/eaf/git/emacs-application-framework")
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

;; Environment | Shell
(use-package exec-path-from-shell
  :if (or (memq window-system '(mac ns x)) (string-equal system-type "gnu/linux"))
  :custom
  (exec-path-from-shell-arguments nil)
  (exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))
(use-package list-environment
  :commands (list-environment))
;; depends on ctags install
;; update based on discussions from https://github.com/flycheck/flycheck/issues/1762
(use-package modern-sh
  :hook ((sh-mode . modern-sh-mode)
         (sh-mode . aaronzinhoo-sh-mode-setup)
         (sh-mode . (lambda () (setq flycheck-local-checkers '((lsp . ((next-checkers . (sh-shellcheck)))))))))
  :preface
  (defun aaronzinhoo-sh-mode-setup ()
    (set (make-local-variable 'company-backends) '((company-files company-capf company-dabbrev-code)))))
(use-package vterm
  :commands vterm)
(use-package multi-vterm
  :commands multi-vterm)
(use-package ansi-color
  :hook (compilation-filter . colorize-compilation-buffer)
  :preface
  ;; Support for ANSI escape color codes in emacs compilation buffer
  ;; Fixes build and test execution output in LSP and DAP
  ;; from https://github.com/kipcd/dotfiles/blob/main/emacs/.emacs.default/init.el#L97
  (defun colorize-compilation-buffer ()
    (when (derived-mode-p 'compilation-mode)
      (ansi-color-process-output nil)
      (setq-local comint-last-output-start (point-marker)))))

;; Programming/Project Management
;; commenting
(use-package turbo-log
  :straight (:type git :host github :repo "artawower/turbo-log.el")
  :config
  (setq turbo-console--prefix "LOG"))
(use-package evil-nerd-commenter
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))
;; Window|buffer Managers
(use-package burly
  :straight (burly :type git :host github :repo "alphapapa/burly.el")
  :commands (burly-open-bookmark burly-bookmark-frames))
(use-package iflipb
  :bind (("C-<tab>" . iflipb-next-buffer)
         ("C-<iso-lefttab>" . iflipb-previous-buffer))
  :custom
  (iflipb-wrap-around t)
  (iflipb-always-ignore-buffers '("^ " "magit*")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package editorconfig
  :diminish
  :config
  (setq editorconfig-exclude-modes (append editorconfig-exclude-modes '(image-mode nxml-mode)))
  (editorconfig-mode 1))
(use-package bookmark+
  :custom
  (bookmark-default-file (concat user-emacs-directory "/bookmarks")) ;;define file to use.
  (bookmark-save-flag t) ;;save bookmarks to .emacs.bmk after each entry
  )
(use-package projectile
  :after major-mode-hydra
  :bind
  ("s-p" . hydra-projectile/body)
  :preface
  (pretty-hydra-define hydra-projectile
    (:hint nil :color teal :quit-key "SPC" :title (with-faicon "rocket" "Projectile" 1 -0.05))
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
     "Search & Replace"
     (("r" projectile-replace "replace")
      ("R" projectile-replace-regexp "regexp replace")
      ("s" counsel-projectile-rg "search"))
     "Tests"
     (("ts" projectile-toggle-between-implementation-and-test "switch to test|implementation file")
      ("tt" projectile-test-project "run tests")
      ("tf" projectile-find-test-file "find test file"))))
  :custom
  (projectile-find-dir-includes-top-level t)
  (projectile-switch-project-action #'projectile-find-dir)
  ;; use .gitignore to exclude files from search
  (projectile-indexing-method 'alien)
  (projectile-enable-caching t)
  (projectile-sort-order 'recentf)
  (projectile-completion-system 'ivy))


;;; Languages Support

;; Indent Guides
(use-package highlight-indent-guides)
;; Syntax Highlighting
(use-package tree-sitter-langs
  :straight t)
(use-package tree-sitter
  :diminish
  :hook ((typescript-mode . tree-sitter-mode)
         (java-mode . tree-sitter-mode)
         (rustic-mode . tree-sitter-mode)
         (python-mode . tree-sitter-mode)
         (js2-mode . tree-sitter-mode)
         (rjsx-mode . tree-sitter-mode)
         (go-mode . tree-sitter-mode)
         (tree-sitter-after-on . tree-sitter-hl-mode)))

;; Debugging
(use-package dap-mode
  :after (lsp-mode)
  :config
  (require 'dap-java)
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra)))
  (dap-auto-configure-mode))
(use-package realgud
  :defer t)
(use-package realgud-trepan-ni
  :straight (:type git :host github :repo "realgud/realgud-trepan-ni" :branch "master")
  :defer t)
(use-package realgud-jdb
  :after (realgud))

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
  :hook ((yaml-mode . aaronzinhoo-yaml-mode-hook)
         (yaml-mode . (lambda () (setq flycheck-local-checkers '((yaml-yamllint . ((next-checkers . (yaml-ruby)))))))))
  :preface
  (defun aaronzinhoo-company-yaml-mode-hook ()
    (set (make-local-variable 'company-backends) '((company-capf company-keywords company-dabbrev-code company-files))))
  (defun aaronzinhoo-yaml-mode-hook ()
    (flycheck-mode)
    (setq-local lsp-java-boot-enabled nil)
    (lsp)
    (setq-local flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(lsp)))
    (hungry-delete-mode)
    (aaronzinhoo-company-yaml-mode-hook)))
;; use json-mode from https://github.com/joshwnj/json-mode for json instead of js-mode or js2-mode
(use-package jsonian
  :straight (:type git :host github :repo "iwahbe/jsonian" :branch "main")
  :hook (jsonian-mode . prettier-mode)
  :after (so-long flycheck)
  :config
  (jsonian-no-so-long-mode)
  (jsonian-enable-flycheck)
  (setq-local js-indent-level 2))
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
  :bind ("s-d" . docker))
(use-package docker-tramp
  :after (counsel-tramp))
(use-package docker-compose-mode
  :straight (:type git :host github :repo "aaronzinhoo/docker-compose-mode" :branch "master")
  :mode ("docker-compose\\'" . docker-compose-mode)
  :hook (docker-compose-mode . aaronzinhoo-docker-compose-mode-setup)
  :preface
  (defun aaronzinhoo-docker-compose-mode-setup ()
    (flycheck-mode)
    (lsp)
    (hungry-delete-mode)
    (set (make-local-variable 'company-backends) '(company-capf company-keywords company-files company-dabbrev-code))))
(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode)
  :bind (:map dockerfile-mode-map
              ("C-c h" . hydra-dockerfile-mode/body))
  :preface
  (pretty-hydra-define hydra-dockerfile-mode
    (:hint nil :title (with-fileicon "dockerfile" "Dockerfile Commands" 1 -0.05) :quit-key "SPC" :color pink)
    ("Build"
     (("b" dockerfile-build-buffer "Build Image")
      ("B" dockerfile-build-no-cache-buffer "Build Image W/O Cache")))))
;; kubernetes settings overview
(use-package kubernetes
  :defer t
  :commands (kubernetes-overview))

;; WEB-DEV CONFIG

;; formatting
(use-package unibeautify
  :straight (:type git :host github :repo "Aaronzinhoo/unibeautify" :branch "master")
  :commands unibeautify)

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
;; TODO add electric pair for < or add snippet
(use-package nxml-mode
  :straight nil
  :bind ((:map nxml-mode-map
               ("C-c h" . hydra-web/body)))
  :config
  (add-to-list 'auto-mode-alist
               (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss") t) "\\'")
                     'nxml-mode)))
(use-package emmet-mode
  :diminish
  :hook (web-mode . emmet-mode))
(use-package helm-emmet)
(use-package html-check-frag
  :straight (:type git :host github :repo "TobiasZawada/html-check-frag" :branch "master")
  :hook (web-mode . html-check-frag-mode))
(use-package css-mode
  :straight nil
  :hook ((css-mode . aaronzinhoo-company-css-mode-hook)
         (scss-mode . aaronzinhoo-company-css-mode-hook))
  :preface
  (defun aaronzinhoo-company-css-mode-hook ()
    (set (make-local-variable 'company-backends) '(company-capf company-keywords company-files company-ispell company-dabbrev))))
(use-package web-mode
  :straight (:type git :host github :repo "Aaronzinhoo/web-mode" :branch "master")
  :hook (web-mode . aaronzinhoo-company-web-mode-hook)
  :mode (("\\.html\\$" . web-mode)
         ("\\.component.html\\'" . web-mode))
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
    (set (make-local-variable 'company-backends) '((company-capf company-web-html company-bootstrap company-css company-files) company-capf)))
  (pretty-hydra-define hydra-web
    (:hint nil :title (with-octicon "globe" "Web Mode Control" 1 -0.05) :quit-key "SPC" :color pink)
    ("Navigation"
     (("a" sgml-skip-tag-backward "tag beginning | prev tag")
      ("e" sgml-skip-tag-forward "tag end | next tag")
      ("n" web-mode-element-next "next tag")
      ("p" web-mode-element-previous "previous tag")
      ("F" web-mode-element-children-fold-or-unfold "fold/unfold tag children")
      ("f" web-mode-fold-or-unfold "fold/unfold"))
     "Edit"
     (("r" unibeautify "refactor HTML")
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
  (web-mode-enable-current-element-highlight t)
  (web-mode-commands-like-expand-region '(web-mode-mark-and-expand er/expand-region er/contract-region mc/mark-all-like-this mc/mark-next-like-this mc/mark-previous-like-this previous-line next-line forward-char backward-char forward-word backward-word hydra-multiple-cursors/nil hydra-web/body hydra-multiple-cursors/body hydra-web/sgml-skip-tag-backward hydra-web/sgml-skip-tag-forward web-mode-element-previous web-mode-element-next mc/skip-to-next-like-this mc/skip-to-previous-like-this)))

;;; Markdown Support
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :bind (:map markdown-mode-map
              ("C-c h" . hydra-markdown-mode/body))
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :preface
  (pretty-hydra-define hydra-markdown-mode
    (:hint nil :title (with-octicon "markdown" "Markdown Mode Control" 1 -0.05) :quit-key "SPC" :color pink)
    ("Insert"
     (("it" markdown-insert-table "table")
      ("ii" markdown-insert-image "image")
      ("ib" markdown-insert-uri "uri")
      ("ic" markdown-insert-code-block "code block")
      ("id" markdown-insert-gfm-checkbox "checkbox"))
     "Preview"
     (("p" vmd-mode "Preview" :toggle t))
     "Action"
     (("o" markdown-open "Open" :color blue))
     ))
  :init (setq markdown-command "pandoc"))
;; prview for markdown profiles
(use-package ox-gfm
  :after org)
;; markdown visualization
(use-package vmd-mode
  :commands (vmd-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; JS/react/angular config
;; completetion: lsp+company
;; refactor: js-prettier
;; syntax: flycheck
;; linter: flycheck
;; for React development use (setq create-lockfiles nil) to avoid crashes
;; packages needed:
;;     npm i @angular-eslint/eslint-plugin (angular only)
;;     npm install prettier
;;     npm install eslint --save-dev
;;     npx eslint --init
;;     npm install --save typescript
;;     npm install --save @types/browserify
;;     tsc --init
(use-package ts-comint
  :commands (run-ts))
(use-package eslintd-fix
  ;;; why is this not being used with ng2-mode? Really need refactoring tool control....
  :defer t
  :config
  (setq flycheck-javascript-eslint-executable "eslint_d"))
(use-package import-js
  :hook ((rjsx-mode . aaronzinhoo-run-import-js-hook)
         (typescript-mode . aaronzinhoo-run-import-js-hook))
  :preface
  (defun aaronzinhoo-run-import-js-hook ()
    (run-import-js)))
(use-package prettier
  :diminish
  :hook ((markdown-mode . prettier-mode)
         (json-mode . prettier-mode)
         (css-mode . prettier-mode)
         (rjsx-mode . prettier-mode)
         (typescript-mode . prettier-mode)))
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
  )
;;angular setup
(use-package typescript-mode
  :delight " Ts"
  :hook (typescript-mode . typescript-company-mode-setup)
  :preface
  (defun typescript-company-mode-setup ()
    (set (make-local-variable 'company-backends) '((company-capf :with company-yasnippet company-files company-keywords) company-capf))))
(use-package rjsx-mode
  :mode (("\\.js\\'" . rjsx-mode)
         ("\\.tsx\\'" . rjsx-mode))
  :config
  (setq js2-mode-show-strict-warnings nil)
  (setq js-indent-level 2)
  (add-hook 'js2-mode-hook #'prettier-js-mode))
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
  :bind (:map python-mode-map
              ("C-c h" . hydra-python-mode/body))
  :hook ((python-mode . pyvenv-mode)
         (python-mode . (lambda () (aaronzinhoo--python-setup))))
  :preface
  (pretty-hydra-define hydra-python-mode
    (:hint nil :color pink :quit-key "SPC" :title (with-alltheicon "python" "Python Mode" 1 -0.05))
    ("Run"
     (("r" run-python "Python Shell")
      ("d" realgud:pdb "PDB" :color blue))
     "Eval"
     (("eb" python-shell-send-buffer "Run Buffer in Shell")
      ("er" python-shell-send-region "Run Region in Shell"))
     "Formatting"
     (("f" py-autopep8-mode "Autopep8 Mode" :toggle t))))
  (defun aaronzinhoo--python-buffer-setup ()
    (setq python-indent-offset 4)
    (setq-local highlight-indentation-offset 4))
  (defun aaronzinhoo--activate-python-shell-complettion ()
    "Return non-nil if can trigger native completion."
    (let ((python-shell-completion-native-enable t)
          (python-shell-completion-native-output-timeout
           python-shell-completion-native-try-output-timeout))
      (python-shell-completion-native-get-completions
       (get-buffer-process (current-buffer))
       nil "_")))
  ;;https://lists.gnu.org/archive/html/help-gnu-emacs/2021-09/msg00535.html
  ;; used to help pyright find venv folders
  (defun aaronzinhoo--pyvenv-workon ()
    (when (buffer-file-name)
      (let* ((python-version ".python-version")
             (project-dir (locate-dominating-file (buffer-file-name) python-version)))
        (when project-dir
	      (progn
	        ;; https://github.com/emacs-lsp/lsp-pyright/issues/62#issuecomment-942845406
	        (lsp-workspace-folders-add project-dir)
	        (pyvenv-workon
             (with-temp-buffer
               (insert-file-contents (expand-file-name python-version project-dir))
               (car (split-string (buffer-string))))))))))
  (defun aaronzinhoo--python-setup ()
    (aaronzinhoo--python-buffer-setup)
    (aaronzinhoo--pyvenv-workon))
  :custom
  (python-check-command "flake8")
  :init
  (setenv "WORKON_HOME" "~/.pyenv/versions")
  (setenv "VIRTUALENVWRAPPER_PYTHON" "~/.pyenv/shims/python")
  (setenv "VIRTUALENVWRAPPER_VIRTUALENV" "~/.pyenv/shims/python")
  (setenv "PIPENV_PYTHON" "~/.pyenv/shims/python")
  (add-to-list 'process-coding-system-alist '("python" . (utf-8 . utf-8)))
  (with-eval-after-load 'python (defun temp () (aaronzinhoo--activate-python-shell-complettion))))
(use-package pyvenv
  :straight t
  :init
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (when (executable-find "ipython3")
                  (setq python-shell-interpreter "ipython3"
                        python-shell-interpreter-args "-i --matplotlib=inline --automagic --simple-prompt --pprint"
                        ;; https://gitlab.com/python-mode-devs/python-mode/-/issues/112#note_699461188
                        py-ipython-command "ipython3"
                        py-ipython-command-args '("-i" "--matplotlib=inline" "--automagic" "--simple-prompt" "--pprint"))))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")))))
(use-package py-autopep8
  :commands (py-autopep8-mode)
  :custom
  (py-autopep8-options '("--max-line-length=140")))

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
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

;;; Rust
(use-package toml-mode)
(use-package rustic
  :custom
  (rustic-lsp-server 'rls))

;;; Java | C++ | C
(use-package cc-mode
  :straight nil
  :hook (java-mode . (lambda ()
                       (setq c-basic-offset 4
                             tab-width 4))))
(use-package protobuf-mode
  :mode (("\\.proto\\'" . protobuf-mode)))
;; ----------------------------------------------------------------

;;; Theme
(use-package moe-theme
  :demand t
  :straight (moe-theme :type git :host github :repo "kuanyui/moe-theme.el" :branch "dev")
  :config
  (require 'moe-theme-switcher)
  (setq moe-theme-highlight-buffer-id t)
  (powerline-moe-theme))

(message "Done loading packages")
;;; init.el ends here
