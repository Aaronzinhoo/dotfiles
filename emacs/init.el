;; package --- Summary --- -*- lexical-binding: t -*-
;;; Commentary:
;;;init.el --- Emacs configuration

;;; Code:
;; TODO add build for shell scripts, and add help menu for go
;; load the early init file if this is not a recent emacs
(message "Initializing settings...")
(let ((default-directory  (concat (expand-file-name (file-name-directory (or load-file-name buffer-file-name))) "elisp/")))
  ;; load all paths from default-directory recursively
  (normal-top-level-add-subdirs-to-load-path)
  ;; load the utils for some helper functions
  (require 'init-constants (concat default-directory "init/init-constants.elc"))
  (require 'init-defaults (concat default-directory "init/init-defaults.elc"))
  (require 'init-straight (concat default-directory "init/init-straight.elc"))
  (require 'init-fonts (concat default-directory "init/init-fonts.elc"))
  (require 'init-utils (concat default-directory "init/init-utils.elc"))
  (require 'init-keybindings (concat default-directory "init/init-keybindings.elc"))
  (require 'pair-navigator (concat default-directory "pair-navigation/pair-navigator.elc")))
(require 'custom)

(message "Loading packages")
;;; Packages
;; built-in
(use-package delsel
  :straight nil
  :config
  (delete-selection-mode t))
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
(use-package emacs
  :straight nil
  :hook (minibuffer-setup . cursor-intangible-mode)
  :bind* (("M-<up>" . move-text-up)
           ("M-<down>" . move-text-down)
           ("M-q" . yank)
           ("C-j" . avy-goto-char-timer))
  :custom
  (pixel-scroll-precision-mode t)
  (delete-selection-mode t)
  (enable-recursive-minibuffers t)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  (tab-always-indent 'complete)
  :preface
  (defun create-uuid ()
    "Return a newly generated UUID. This uses a simple hashing of variable data."
    (let ((s (md5 (format "%s%s%s%s%s%s%s%s%s%s"
                    (user-uid)
                    (emacs-pid)
                    (system-name)
                    (user-full-name)
                    user-mail-address
                    (current-time)
                    (emacs-uptime)
                    (garbage-collect)
                    (random)
                    (recent-keys)))))
      (format "%s-%s-3%s-%s-%s"
        (substring s 0 8)
        (substring s 8 12)
        (substring s 13 16)
        (substring s 16 20)
        (substring s 20 32))))
  (defun insert-uuid ()
    "Inserts a new UUID at the point."
    (interactive)
    (insert (create-uuid)))
  :init
  (define-advice json-parse-buffer (:around (old-fn &rest args) lsp-booster-parse-bytecode)
    "Try to parse bytecode instead of json."
    (or
      (when (equal (following-char) ?#)
        (let ((bytecode (read (current-buffer))))
          (when (byte-code-function-p bytecode)
            (funcall bytecode))))
      (apply old-fn args)))

  (define-advice lsp-resolve-final-command (:around (old-fn cmd &optional test?) add-lsp-server-booster)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
            (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
            lsp-use-plists
            (not (functionp 'json-rpc-connection))  ;; native json-rpc
            (executable-find "emacs-lsp-booster")
            (not (member 'ansible minor-mode-list)))
        (progn
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  :config
  (add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/")))
(use-package elec-pair
  :straight nil
  :hook ((git-commit-mode . git-commit-add-electric-pairs)
         (org-mode . org-add-electric-pairs)
         (markdown-mode . markdown-add-electric-pairs)
         (go-ts-mode . go-add-electric-pairs)
         (yaml-ts-mode . yaml-add-electric-pairs)
         (rust-ts-mode . rust-add-electric-pairs))
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
  (defun go-add-electric-pairs ()
    (setq-local electric-pair-pairs (append electric-pair-pairs '((?` . ?`))))
    (setq-local electric-pair-text-pairs electric-pair-pairs))
  (defun yaml-add-electric-pairs ()
    (setq-local electric-pair-pairs (append electric-pair-pairs '((?\( . ?\)))))
    (setq-local electric-pair-text-pairs electric-pair-pairs))
  (defun rust-add-electric-pairs ()
    (setq-local electric-pair-pairs (append electric-pair-pairs '((?' . ?'))))
    (setq-local electric-pair-text-pairs electric-pair-pairs))
  :init
  ;; disable <> auto pairing in electric-pair-mode for org-mode
  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local electric-pair-inhibit-predicate
                          `(lambda (c)
                             (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
  (electric-pair-mode t))
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
(use-package sh-mode
  :straight nil
  :hook ((sh-mode . (lambda () (setq flycheck-local-checkers '((lsp . ((next-checkers . (sh-shellcheck))))))))))
(use-package simple
  :straight nil
  :preface
  (defun aaronzinhoo-remove-empty-lines-buffer ()
    (save-excursion
      (beginning-of-buffer)
      (flush-lines "^\\s-*$" nil nil t)))
  :config
  (column-number-mode t)
  (global-visual-line-mode t))
(use-package tramp
  :straight nil
  :custom
  (tramp-verbose 10)
  (tramp-debug-buffer t)
  (tramp-default-method "ssh"))
(use-package tree-sitter
  :straight nil
  :init
  (setq treesit-language-source-alist
    '((bash "https://github.com/tree-sitter/tree-sitter-bash")
       (cmake "https://github.com/uyha/tree-sitter-cmake")
       (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
       (c "https://github.com/tree-sitter/tree-sitter-c")
       (css "https://github.com/tree-sitter/tree-sitter-css")
       (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
       (elisp "https://github.com/Wilfred/tree-sitter-elisp")
       (go "https://github.com/tree-sitter/tree-sitter-go")
       (gomod "https://github.com/camdencheek/tree-sitter-go-mod" "main" "src")
       (gosum "https://github.com/tree-sitter-grammars/tree-sitter-go-sum")
       (java "https://github.com/tree-sitter/tree-sitter-java")
       (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
       (json "https://github.com/tree-sitter/tree-sitter-json")
       (make "https://github.com/alemuller/tree-sitter-make")
       (markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src")
       (markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src")
       (proto "https://github.com/mitchellh/tree-sitter-proto" "main")
       (python "https://github.com/tree-sitter/tree-sitter-python")
       (rust "https://github.com/tree-sitter/tree-sitter-rust")
       (sql "https://github.com/m-novikov/tree-sitter-sql")
       (ssh-config "https://github.com/tree-sitter-grammars/tree-sitter-ssh-config")
       (toml "https://github.com/tree-sitter/tree-sitter-toml")
       (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
       (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
       (xml "https://github.com/tree-sitter-grammars/tree-sitter-xml" "master" "xml/src")
       (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")
       ))
  (dolist (lang treesit-language-source-alist)
    (unless (treesit-language-available-p (car lang))
      (treesit-install-language-grammar (car lang))))
  (dolist (mapping '((sh-mode . bash-ts-mode)
                      (c-mode . c-ts-mode)
                      (cc-mode . c++-ts-mode)
                      (c++-mode . c++-ts-mode)
                      (c-or-c++-mode . c-or-c++-ts-mode)
                      (css-mode . css-ts-mode)
                      (dockerfile-mode . dockerfile-ts-mode)
                      (go-dot-mod-mode . go-mod-ts-mode)
                      (go-mode . go-ts-mode)
                      (java-mode . java-ts-mode)
                      (json-mode . json-ts-mode)
                      (js-mode . js-ts-mode)
                      (python-mode . python-ts-mode)
                      (typescript-mode . typescript-ts-mode)
                      (toml-mode . toml-ts-mode)
                      (yaml-mode . yaml-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :custom
  (treesit-load-name-override-list
    '((c++ "libtree-sitter-cpp"))))
(use-package which-key
  :straight nil
  :diminish
  :custom
  (which-key-use-C-h-commands nil)
  :config
  (which-key-setup-side-window-right-bottom)
  (which-key-mode t))
(use-package winner
  :straight nil
  :config
  (winner-mode 1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package system-packages
  :straight t
  :custom
  (system-packages-use-sudo nil)
  :init
  (when (eq system-type 'darwin)
    (setq system-packages-package-manager 'brew)))
(use-package s :straight t
  :preface
  (defun snake-case-word (start end)
    "Change selected text to snake case format"
    (interactive "r")
    (if (use-region-p)
      (let ((camel-case-str (buffer-substring start end)))
        (delete-region start end)
        (insert (s-snake-case camel-case-str)))
      (message "No region selected"))))
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
(use-package compat
  :demand t
  :straight (:type git :host github :repo "emacs-compat/compat" :branch "main"))
;; log event/command history of all buffers
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
(use-package auto-compile
  :straight (:type git :host github :repo "emacscollective/auto-compile" :branch "main")
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))
;; for hydra check hydra config
;;;;;;;;;;;;;;;;;;;;;;;;
(use-package no-littering
  :demand t
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))
(use-package url)
(use-package xref
  :straight t
  ;;
  ;; :custom
  ;; xref at point always
  ;; (setq xref-prompt-for-indentifier nil)
  )
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
  :if (memq window-system '(windows)))
(use-package ssh-config-mode
  :hook ((ssh-config-mode . aaronzinhoo--ssh-config-mode-hook))
  :preface
  (defun aaronzinhoo--ssh-config-mode-hook ()
    (setq-local completion-at-point-functions
                (list #'cape-file #'ssh-config-completion-at-point #'cape-dabbrev))))
(use-package x509-mode
  :straight t
  :commands (x509-mode)
  :custom
  (x509-openssl-cmd "openssl")
  :config
  (dolist (mode '(x509-mode-hook))
    (add-hook mode (lambda () (emojify-mode 0)))))
;; edit in sudo mode, good when using tramp
(use-package sudo-edit
  :straight (:type git :host github :repo "nflath/sudo-edit")
  :commands (sudo-edit))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package xclip
  :if (string-equal system-type "windows-nt")
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
  ("s-o" . hydra-org/body)
  ("s-B" . hydra-bookmark/body)
  :custom
  (hydra-default-hint nil))
(use-package major-mode-hydra
  :demand t
  :after hydra
  :preface
  (defun with-faicon (icon str &optional height v-adjust)
    "Displays an icon from Font Awesome icon."
    (s-concat (nerd-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))
  (defun with-sucicon (icon str &optional height v-adjust)
    "Displays an icon from the Atom File Icons package."
    (s-concat (nerd-icons-sucicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))
  (defun with-octicon (icon str &optional height v-adjust)
    "Displays an icon from the GitHub Octicons."
    (s-concat (nerd-icons-octicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))
  (defun with-codicon (icon str &optional height v-adjust)
    "Displays an icon from the GitHub Octicons."
    (s-concat (nerd-icons-codicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))
  (defun with-mdicon (icon str &optional height v-adjust)
    "Displays an icon from the Atom File Icons package."
    (s-concat (nerd-icons-mdicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))
  :config
  (pretty-hydra-define hydra-org
    (:hint nil :color pink :quit-key "SPC" :title (with-sucicon "nf-custom-orgmode" "Org" 1 -0.05))
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
    (:hint nil :color pink :quit-key "SPC" :title (with-sucicon "nf-custom-orgmode" "Org" 1 -0.05))
    ("Navigation"
     (("p" org-previous-visible-heading "prev heading")
      ("n" org-next-visible-heading "next heading")
      ("B" org-previous-block)
      ("b" org-next-block)
      ("g" consult-org-heading "goto"))
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
  (pretty-hydra-define hydra-nav
    (:hint nil :color amaranth :quit-key "SPC" :title (with-mdicon "nf-md-navigation_variant_outline" "Navigation" 1 -0.05))
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
      ("q" yank "Paste"))))
  (pretty-hydra-define hydra-bookmark
    (:hint nil :color teal :quit-key "SPC" :title (with-codicon "nf-cod-bookmark" "Bookmark" 1 -0.05))
    ("Burly"
     (("o" burly-open-bookmark "Open Burly Bookmark")
      ("b" burly-open-last-opened-bookmark "Most Recently Opened Bookmark")
      ("s" burly-bookmark-windows "Bookmark Windows"))
     "Cycle"
     (("c" bmkp-cycle "Cycle Bookmarks" :color red))
     "Jump"
     (("j" consult-bookmark "Jump to bookmark"))
     "List"
     (("l" bookmark-bmenu-list "List Bookmarks")))))
(use-package helpful
  :after (major-mode-hydra)
  :custom
  (help-window-select t)
  :bind
  ("C-h" . helpful-hydra/body)
  ([remap describe-function] . helpful-callable)
  ([remap describe-command]  . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key]      . helpful-key)
  :pretty-hydra
  ((:color teal :quit-key "q")
   ("Helpful"
    (("f" helpful-callable "callable")
     ("v" helpful-variable "variable")
     ("k" helpful-key "key")
     ("c" helpful-command "command")
     ("d" helpful-at-point "thing at point")
     ("m" describe-mode "mode")))))
(use-package undo-fu-session
  :straight (:type git :host nil :repo "https://codeberg.org/ideasman42/emacs-undo-fu-session" :branch "main")
  :hook (emacs-startup . undo-fu-session-global-mode)
  :custom
  (undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))
(use-package undo-fu
  :after (major-mode-hydra)
  :straight (:type git :host nil :repo "https://codeberg.org/ideasman42/emacs-undo-fu" :branch "main")
  :bind
  ("C-/" . undo-fu-only-undo)
  ("s-/" . undo-fu-hydra/body)
  (:map org-mode-map
        ("s-/" . undo-and-activate-hydra-mode))
  :pretty-hydra
  (undo-fu-hydra
    (:hint nil :color red :quit-key "SPC" :title (with-faicon "nf-fa-undo" "Undo/Redo" 1 -0.05))
    ("Action"
     (("/" undo-fu-only-undo "Undo")
      ("r" undo-fu-only-redo "Redo")
      ("RET" nil "Quit" :color blue)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CONTROL VERSION UTILS
(use-package git-gutter
  :after (nerd-icons hydra)
  :straight (:type git :host github :repo "emacsorphanage/git-gutter" :branch "master")
  :hook (prog-mode . git-gutter-mode)
  :bind ("s-g" . hydra-git-gutter/body)
  :commands (git-gutter-mode)
  :diminish git-gutter-mode
  :preface
  (pretty-hydra-define hydra-git-gutter
    (:hint nil :color "pink" :quit-key "SPC" :title (with-octicon "nf-oct-diff" "Diff" 1 -0.05))
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
  (git-gutter:modified-sign (nerd-icons-codicon "nf-cod-diff_modified"))
  (git-gutter:added-sign (nerd-icons-codicon "nf-cod-diff_added"))
  (git-gutter:deleted-sign (nerd-icons-codicon "nf-cod-diff_removed"))
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
   '(("aaron.gonzales.ctr@linquest.com"
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
  :hook ((magit-mode . magit-auto-revert-mode)
         ((git-commit-setup . aaronzinhoo--git-commit-setup)))
  :preface
  (defun aaronzinhoo--git-commit-setup ()
    (setq-local fill-column 72)
    (setq-local completion-at-point-functions (list #'cape-file #'cape-dabbrev #'cape-dict)))
  :custom
  (magit-bind-magit-project-status nil)
  :config
  (add-hook 'after-save-hook 'magit-after-save-refresh-status))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package matching-paren-overlay
  :straight (:type git :host codeberg :repo "acdw/matching-paren-overlay.el" :branch "main")
  :hook (prog-mode . matching-paren-overlay-mode))
(use-package emojify
  :if (display-graphic-p)
  :hook ((prog-mode . (lambda () (emojify-mode 0)))
          (vterm-mode . emojify-mode)))
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
  :straight (:type git :host github :repo "mhayashi1120/Emacs-wgrep" :branch "master")
  :bind (:map grep-mode-map
              ("M-e" . wgrep-change-to-wgrep-mode))
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
(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("m" "/mnt/"                       "Drives")
     ("t" "~/.local/share/Trash/files/" "TrashCan")))
  :config
  ;; (dirvish-peek-mode) ; Preview files in minibuffer
  ;; (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes
        '(file-time file-size collapse subtree-state vc-state git-msg))
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-fd)
   :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
   ("DEL" . dired-up-directory)
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))
(use-package dired
  :straight nil
  :hook ((dired-mode . hl-line-mode)
         (dired-mode . auto-revert-mode))
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
(use-package dired-recent
  :config
  (dired-recent-mode  1))
(use-package recentf
  :demand t
  :custom
  (recentf-exclude '("~$" "/tmp/" "/ssh:" "/sudo:" "/sftp:" (expand-file-name "~/.config/emacs/eln-cache") (expand-file-name "~/.config/emacs/var") (expand-file-name "~/.config/emacs/straight")))
  (recentf-max-menu-items 500)
  (recentf-max-saved-items 500)
  :config
  (recentf-mode 1))
(use-package crux
  :bind* (("C-a" . crux-move-beginning-of-line)
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
  (dashboard-banner-logo-title "Welcome to your Emacs Dashboard!")
  (dashboard-items '((recents  . 5)
                     (projects . 5)
                     (bookmarks . 5)
                     (agenda . 5)))
  (dashboard-icon-type 'nerd-icons) ;; use `nerd-icons' package
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  :bind ("s-f" . flycheck-hydra/body)
  :pretty-hydra
  ((:hint nil :color teal :quit-key "SPC" :title (with-codicon "nf-cod-debug" "Flycheck" 1 -0.05))
    ("Checker"
     (("?" flycheck-describe-checker "describe")
      ("d" flycheck-disable-checker "disable")
      ("m" flycheck-mode "mode")
      ("s" flycheck-select-checker "select"))
     "Errors"
     (("p" flycheck-previous-error "previous" :color pink)
      ("n" flycheck-next-error "next" :color pink)
      ("l" flycheck-projectile-list-errors "list errors (proj)")
      ("L" flycheck-list-errors "list errors"))
     "Other"
     (("r" recenter-top-bottom "recenter" :color pink)
      ("M" flycheck-manual "manual")
      ("v" flycheck-verify-setup "verify setup"))))
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
  (flycheck-add-mode 'json-jsonlint 'json-ts-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  ;; eslint requires you to be careful with the configuration
  ;; ensure to use .json files and setup accordingly
  ;; test with shell command
  (flycheck-add-mode 'javascript-eslint 'typescript-mode)
  (flycheck-add-mode 'css-stylelint 'css-ts-mode)
  (flycheck-add-mode 'dockerfile-hadolint 'dockerfile-ts-mode)
  (flycheck-add-mode 'sh-shellcheck 'sh-mode))
(use-package flycheck-aspell
  :after flycheck
  :config
  ;; If you want to check Markdown/GFM buffers
  (add-to-list 'flycheck-checkers 'markdown-aspell-dynamic)
  ;; If you want to check HTML buffers
  (add-to-list 'flycheck-checkers 'html-aspell-dynamic))
(use-package flycheck-swagger-cli
  :after (flycheck)
  :straight (:type git :host github :repo "vercapi/flycheck-swagger-cli" :branch "master")
  :custom
  (flycheck-swagger-cli-executable "swagger-cli")
  :init
  (flycheck-add-mode 'swagger-cli 'yaml-ts-mode)
  (require 'flycheck-swagger-cli))
(use-package flycheck-projectile
  :commands (flycheck-projectile-list-errors))
;; (use-package aggressive-indent
;;   :straight t
;;   :diminish
;;   :config
;;   (global-aggressive-indent-mode 1)
;;   (setq aggressive-indent-excluded-modes (append aggressive-indent-excluded-modes '(web-mode dockerfile-mode docker-compose-mode))))
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
(use-package combobulate
  :after (expand-region)
  :commands (aaronzinhoo--mark-region-dwim)
  :straight (:type git :host github :repo "mickeynp/combobulate" :branch "master")
  :bind* (("M-2" . aaronzinhoo--mark-region-dwim)
          :map combobulate-proffer-map
          ("2" . next))
  :preface
  ;; You can customize Combobulate's key prefix here.
  ;; Note that you may have to restart Emacs for this to take effect!
  (setq combobulate-key-prefix "C-c o")

  ;; Optional, but recommended.
  ;;
  ;; You can manually enable Combobulate with `M-x
  ;; combobulate-mode'.
  (defun aaronzinhoo--mark-region-dwim ()
    (interactive)
    (unless (ignore-errors (or (combobulate-mark-node-dwim) t))
      (er/expand-region 1)))
  :hook ((python-ts-mode . combobulate-mode)
          (js-ts-mode . combobulate-mode)
          (css-ts-mode . combobulate-mode)
          (yaml-ts-mode . combobulate-mode)
          (json-ts-mode . combobulate-mode)
          (typescript-ts-mode . combobulate-mode)
          (tsx-ts-mode . combobulate-mode)))
(use-package expand-region
  :demand t
  :commands (er/mark-symbol)
  :bind (("M-3" . er/mark-outside-pairs))
  :preface
  (defun aaronzinhoo-mark-line ()
    "Mark the current line."
    (interactive)
    (end-of-line)
    (set-mark (point))
    (beginning-of-line-text))
  (defun er/add-rust-mode-expansions ()
    (make-variable-buffer-local 'er/try-expand-list)
    (setq er/try-expand-list (append
                              er/try-expand-list
                              '(er/c-mark-statement
                                er/c-mark-fully-qualified-name
                                er/c-mark-function-call-1   er/c-mark-function-call-2
                                er/c-mark-statement-block-1 er/c-mark-statement-block-2
                                er/c-mark-vector-access-1   er/c-mark-vector-access-2
                                aaronzinhoo-mark-line))))
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
  (eval-after-load 'yaml-ts-mode '(require 'yaml-mode-expansions))
  (er/enable-mode-expansions 'yaml-ts-mode 'er/add-yaml-mode-expansions)
  (er/enable-mode-expansions 'typescript-mode 'er/add-rjsx-mode-expansions)
  (er/enable-mode-expansions 'rjsx-mode 'er/add-rjsx-mode-expansions)
  (er/enable-mode-expansions 'web-mode 'er/add-web-mode-expansions)
  (er/enable-mode-expansions 'python-ts-mode 'er/add-python-mode-expansions)
  (er/enable-mode-expansions 'rust-ts-mode 'er/add-rust-mode-expansions))
(use-package yasnippet
  :defer t
  :diminish yas-minor-mode
  :commands yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :config
  (use-package yasnippet-snippets)
  (yas-reload-all))

;;; LSP

(use-package lsp-mode
  ;; :straight (:type git :host github :repo "emacs-lsp/lsp-mode" :branch "master")
  :commands (lsp lsp-deferred)
  :hook ((c-ts-mode . lsp-deferred)
          (c++-ts-mode . lsp-deferred)
          (go-ts-mode . lsp-deferred)
          (sql-mode . lsp-deferred)
          (html-ts-mode . lsp-deferred)
          (web-mode . lsp-deferred)
          (typescript-ts-mode . lsp-deferred)
          (rust-ts-mode . lsp-deferred)
          (dockerfile-ts-mode . lsp-deferred)
          (sh-mode . lsp-deferred)
          (bash-ts-mode . lsp-deferred)
          (yaml-ts-mode . lsp-deferred)
          (python-ts-mode . lsp-deferred)
          (conf-javaprop-mode . lsp-deferred)
          (lsp-mode . lsp-enable-which-key-integration)
          (lsp-completion-mode . aaronzinhoo--lsp-mode-setup-completion)
          (lsp-mode . yas-minor-mode))
  :bind (:map lsp-mode-map
              ("s-l" . lsp-hydra/body))
  :pretty-hydra
  (lsp-hydra
    (:hint nil :color pink :quit-key "SPC" :title (with-octicon "nf-oct-rocket" "LSP" 1 -0.05))
    ("Goto"
     (("r" lsp-find-references "refs")
      ("d" lsp-find-definition "defs")
      ("i" lsp-goto-implementation "implementation (interface)")
      ("t" lsp-find-type-definition "type-def")
      ("b" xref-pop-marker-stack "pop back" :color red))
     "Refactor"
     (("f" lsp-format-buffer "format")
       ("n" lsp-rename "rename")
       ("o" lsp-organize-imports "organize imports")
       ("c" lsp-execute-code-action "code action"))
     "UI"
     (("p" lsp-ui-peek-mode "peek-mode")
      ("R" lsp-ui-peek-find-references "peek-refs" :color red)
      ("D" lsp-ui-peek-find-definitions "peek-defs" :color red)
      ("m" lsp-ui-imenu "peek-menu"))
     "Server"
     (("s" lsp-describe-session "session")
      ("I" lsp-install-server "install")
      ("S" lsp-workspace-restart "restart"))))
  :preface
  (defun aaronzinhoo--lsp-mode-setup-completion ()
    "Replace the default `lsp-completion-at-point' with its
`cape-capf-buster' version. Also add `cape-file' and
`company-yasnippet' backends."
    (setq-local completion-at-point-functions
            (list #'cape-file (cape-capf-buster #'lsp-completion-at-point) #'cape-dabbrev #'cape-dict))
    (bind-key (kbd "TAB") 'corfu-next corfu-map)
    (setq-local completion-styles '(flex basic))
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(flex))) ;; Configure flex
  (defun lsp-go-hooks ()
    (add-hook 'before-save-hook 'lsp-format-buffer nil t)
    (add-hook 'before-save-hook 'lsp-organize-imports nil t)
    (setq-local lsp-gopls-staticcheck t)
    (setq-local lsp-eldoc-render-all t)
    (setq-local lsp-gopls-complete-unimported t))
  ;;https://lists.gnu.org/archive/html/help-gnu-emacs/2021-09/msg00535.html
  ;; used to help pyright find venv folders
  (defun aaronzinhoo-lsp-python-setup ()
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
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-completion-provider :none) ;; we use Corfu!
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
   `((,(intern "https://raw.githubusercontent.com/compose-spec/compose-spec/master/schema/compose-spec.json") . ["*-compose.y*"])
     (,(intern "https://json.schemastore.org/kustomization.json") . ["kustomization.yaml"])
     (kubernetes . ["*.yaml"])))
  (lsp-clients-angular-language-server-command
   `("node"     ,(concat node-home-folder "lib/node_modules/@angular/language-server")
     "--ngProbeLocations"
     ,(concat node-home-folder "lib/node_modules")
     "--tsProbeLocations"
     ,(concat node-home-folder "lib/node_modules")
     "--stdio"))
  :init
  (setenv "LSP_USE_PLISTS" "true")
  (add-hook 'python-ts-mode-hook 'aaronzinhoo-lsp-python-setup)
  :config
  (push '(protobuf-ts-mode . "protobuf") lsp-language-id-configuration)
  (push 'rustic-clippy flycheck-checkers)
  (push '(web-mode . "html") lsp-language-id-configuration)
  (push '(docker-compose-mode . "yaml") lsp-language-id-configuration)
  (push '(yaml-ts-mode . "yaml") lsp-language-id-configuration)
  (push '(bash-ts-mode . "sh") lsp-language-id-configuration)
  (setq gc-cons-threshold  100000000)
  (setq read-process-output-max (* 1024 1024)) ;;1MB
  )
(use-package lsp-docker
  :after (lsp-mode)
  :requires (lsp-mode)
  :after (lsp-mode)
  :straight (:type git :host github :repo "emacs-lsp/lsp-docker" :branch "master"))
(use-package lsp-treemacs
  :commands (treemacs lsp-treemacs-errors-list))
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
  :straight (:type git :host github :repo "emacs-lsp/lsp-java" :branch "master")
  :hook ((java-ts-mode . lsp-deferred)
         (java-ts-mode . lsp-java-boot-lens-mode))
  :config
  (require 'lsp-java-boot)
  (let ((lombok-file (concat user-init-dir-fullpath "deps/lombok-1.18.26.jar")))
    ;; current VSCode defaults
    (setq lsp-java-vmargs (list "-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx2G" "-Xms100m" (concat "-javaagent:" lombok-file)))))


(use-package lsp-pyright
  :requires (lsp pyvenv)
  :straight (:type git :host github :repo "emacs-lsp/lsp-pyright" :branch "master")
  :if (executable-find "pyright")
  :init
  (setq lsp-pyright-venv-directory (concat pyenv-root-folder "/versions"))
  (setq lsp-pyright-venv-path (concat pyenv-root-folder "/versions"))
  (setq lsp-pyright-python-executable-cmd "python3")
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-pyright))))

;;; Debugger Support
(use-package dap-mode
  :after (lsp-mode lsp-docker)
  :straight (:type git :host github :repo "emacs-lsp/dap-mode" :branch "master")
  :hook ((lsp-mode . dap-auto-configure-mode)
         ;; dap-stopped called after breakpoint hit
         (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra))))
  :custom
  (dap-python-debugger 'debugpy)
  :config
  (dap-register-debug-template "My Runner"
                             (list :type "java"
                                   :request "launch"
                                   :args ""
                                   :vmArgs "-ea -Dtileaccessservice.instance.name=tileaccessservice_1"
                                   :projectName "tileaccessservice"
                                   :mainClass "com.linquest.tileaccessservice.TileAccessServiceApplication"
                                   :env '(("DEV" . "1"))))
  (dap-register-debug-template "Python :: Test TileAccessService"
  (list :type "python"
        :args "-i"
        :cwd nil
        :env '(("DEBUG" . "1"))
        :target-module (expand-file-name "~/development/work/kahless/backend/user-management-service/main.py")
        :request "launch"
        :name "My App"))
  (dap-ui-controls-mode nil)
  (dap-ui-mode nil)
  (dap-tooltip-mode nil)
  (require 'dap-python)
  (require 'dap-dlv-go)
  (require 'dap-lldb)
  (require 'dap-gdb-lldb))
(use-package dap-java
  :after (lsp-java dap)
  :straight (dap-java :type git :host github :repo "emacs-lsp/lsp-java" :branch "master"))
(use-package dape
  :straight (:type git :host github :repo "svaante/dape" :branch "master")
  :commands (dape)
  ;; To use window configuration like gud (gdb-mi)
  :custom
  (dape-buffer-window-arrangment 'gud)
  (dape-cwd-fn 'projectile-project-root)
  :config
  (add-to-list 'dape-configs
               '(test-python
                 modes (python-ts-mode python-mode)
                 command "python -i "
                 command-args ("-m" "debugpy")
                 :type "executable"
                 :request "launch"
                 :module ""
                 :cwd dape-cwd-fn
                 :args ["-i" "run" dape-find-file-buffer-default]
                  ))

  ;; To not display info and/or buffers on startup
  ;; (remove-hook 'dape-on-start-hooks 'dape-info)
  ;; (remove-hook 'dape-on-start-hooks 'dape-repl)

  ;; To display info and/or repl buffers on stopped
  (add-hook 'dape-on-stopped-hooks 'dape-info)
  (add-hook 'dape-on-stopped-hooks 'dape-repl)

  ;; By default dape uses gdb keybinding prefix
  ;; (setq dape-key-prefix "\C-x\C-a")

  ;; Kill compile buffer on build success
  (add-hook 'dape-compile-compile-hooks 'kill-buffer)

  ;; Save buffers on startup, useful for interpreted languages
  ;; (add-hook 'dape-on-start-hooks
  ;;           (defun dape--save-on-start ()
  ;;             (save-some-buffers t t)))

  ;; Projectile users
  )

;;; Minibuffer Compleitions

;; icons!!
(use-package nerd-icons
  :straight t)

(use-package nerd-icons-corfu
  :after (nerd-icons corfu)
  :demand t
  :straight (:type git :host github :repo "LuigiPiucco/nerd-icons-corfu" :branch "master")
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))
(use-package treemacs-nerd-icons
  :after (treemacs)
  :config
  (treemacs-load-theme "nerd-icons"))
(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))
(use-package nerd-icons-completion
  :demand t
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :init
  (nerd-icons-completion-mode))
(use-package marginalia
  :demand t
  :after (vertico)
  :bind (:map vertico-map
              ("M-A" . marginalia-cycle))
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))
(use-package orderless
  :demand t
  :ensure t
  :config
  (orderless-define-completion-style orderless+initialism
    (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))
  ;; Define orderless style with initialism by default
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles basic partial-completion))
                                   (command (styles orderless+initialism))
                                   (symbol (styles orderless+initialism))
                                   (variable (styles orderless+initialism)))))
;; consult
;; Example configuration for Consult
(use-package consult-dir
  :straight (consult-dir :type git :host github :repo "karthink/consult-dir" :branch "master")
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :preface
  (defun consult-dir--tramp-docker-hosts ()
  "Get a list of hosts from Docker."
  (when (require 'tramp-container nil t)
    (mapcar (lambda (e)
	    (concat "/docker:" (format "%s" (cadr e)) ":/"))
	  (tramp-docker--completion-function))))
  :custom
  (consult-dir-project-list-function #'consult-dir-projectile-dirs)
  :config
  (defvar consult-dir--source-tramp-docker
    `(:name     "Docker"
                :narrow   ?d
                :category file
                :face     consult-file
                :history  file-name-history
                :items    ,#'consult-dir--tramp-docker-hosts)
    "Docker candiadate source for `consult-dir'.")

  ;; Adding to the list of consult-dir sources
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-docker t)
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t))
(use-package consult-flycheck
  :after (consult)
  :straight (consult-flycheck :type git :host github :repo "minad/consult-flycheck" :branch "main"))
(use-package consult-projectile
  :after (consult)
  :demand t
  :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master")
  :custom
  (consult-projectile-use-projectile-switch-project t))
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("s-s f" . consult-fd)
         ("s-s p" . consult-ripgrep-thing-at-point)
         ;; ("M-s D" . consult-locate)
         ;; ("M-s g" . consult-grep)
         ;; ("M-s G" . consult-git-grep)
         ("C-s" . aaronzinhoo--consult-ripgrep-or-line)
         ;; ("M-s L" . consult-line-multi)
         ;; ("M-s k" . consult-keep-lines)
         ;; ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ;; ("C-M-S" . consult-isearch-history)
         :map isearch-mode-map
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook
  (completion-list-mode . consult-preview-at-point-mode)
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  :preface
  (defun consult-ripgrep-thing-at-point (&optional dir given-initial)
  "Pass the region to consult-ripgrep if available.

DIR and GIVEN-INITIAL match the method signature of `consult-wrapper'."
  (interactive "P")
  (let ((initial
         (or given-initial
             (cond ((not (use-region-p))
                    (er/mark-symbol)
                    (buffer-substring-no-properties (region-beginning) (region-end)))
                   ((use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end)))))))
    (deactivate-mark)
    (consult-ripgrep (file-name-directory buffer-file-name) initial)))
  (defun consult--fd-builder (input)
  (let ((fd-command
         (if (eq 0 (process-file-shell-command "fdfind"))
             "fdfind"
           "fd")))
    (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
                 (`(,re . ,hl) (funcall consult--regexp-compiler
                                        arg 'extended t)))
      (when re
        (cons (append
               (list fd-command
                     "--color=never" "--full-path"
                     (consult--join-regexps re 'extended))
               opts)
              hl)))))

  (defun consult-fd (&optional dir initial)
    (interactive "P")
    (pcase-let* ((`(,prompt ,paths ,dir) (consult--directory-prompt "Fd" dir))
                 (default-directory dir))
      (find-file (consult--find prompt #'consult--fd-builder initial))))
  (defcustom aaronzinhoo--consult-ripgrep-or-line-limit 1000000
  "Buffer size threshold for `my-consult-ripgrep-or-line'.
When the number of characters in a buffer exceeds this threshold,
`consult-ripgrep' will be used instead of `consult-line'."
  :type 'integer)

  (defun aaronzinhoo--consult-ripgrep-or-line ()
    "Call `consult-line' for small buffers or `consult-ripgrep' for large files."
    (interactive)
    (if (or (not buffer-file-name)
            (buffer-narrowed-p)
            (ignore-errors
              (file-remote-p buffer-file-name))
            (jka-compr-get-compression-info buffer-file-name)
            (<= (buffer-size)
                (/ aaronzinhoo--consult-ripgrep-or-line-limit
                   (if (eq major-mode 'org-mode) 4 1))))
        (consult-line)
      (when (file-writable-p buffer-file-name)
        (save-buffer))
      (let ((consult-ripgrep-command
             (concat "rg "
                     "--null "
                     "--line-buffered "
                     "--color=ansi "
                     "--max-columns=250 "
                     "--no-heading "
                     "--line-number "
                     ;; adding these to default
                     "--smart-case "
                     "--hidden "
                     "--max-columns-preview "
                     ;; add back filename to get parsing to work
                     "--with-filename "
                     ;; defaults
                     "-e ARG OPTS "
                     (shell-quote-argument buffer-file-name))))
        (consult-ripgrep))))

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)
)
;; Enable vertico
(use-package embark
  :straight t
  :bind (:map minibuffer-mode-map
              ("M-e" . embark-act))
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))
;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
;;; minibuffer completion
(use-package vertico
  ;; Special recipe to load extensions conveniently
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-indexed
                                vertico-flat
                                vertico-grid
                                vertico-mouse
                                vertico-quick
                                vertico-buffer
                                vertico-repeat
                                vertico-reverse
                                vertico-directory
                                vertico-multiform
                                vertico-unobtrusive
                                ))
  :bind (:map vertico-map
              ("<tab>" . vertico-insert)
              ;; NOTE 2022-02-05: Cycle through candidate groups
              ("C-M-n" . vertico-next-group)
              ("C-M-p" . vertico-previous-group)
              ;; Multiform toggles
              ("<backspace>" . vertico-directory-delete-char)
              ("M-<backspace>" . vertico-directory-delete-word)
              ("RET" . vertico-directory-enter)
              ("C-i" . vertico-quick-insert)
              ("C-o" . vertico-quick-exit)
              ("C-s" . vertico-save)
              ("M-o" . aaronzinhoo--vertico-quick-embark))
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  (after-change-major-mode . aaronzinhoo--save-major-mode)
  :preface
  (defvar +vertico-transform-functions nil)

  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start &context ((not +vertico-transform-functions) null))
    (dolist (fun (ensure-list +vertico-transform-functions))
      (setq cand (funcall fun cand)))
    (cl-call-next-method cand prefix suffix index start))

  (defun +vertico-highlight-directory (file)
    "If FILE ends with a slash, highlight it as a directory."
    (if (string-suffix-p "/" file)
        (propertize file 'face 'marginalia-file-priv-dir) ; or face 'dired-directory
      file))
  (defun sort-directories-first (files)
    ;; Still sort by history position, length and alphabetically
    (setq files (vertico-sort-history-length-alpha files))
    ;; But then move directories first
    (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
           (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))
  (defun aaronzinhoo--vertico-highlight-enabled-mode (cmd)
    "If MODE is enabled, highlight it."
    (let ((sym (intern cmd)))
      (if (or (eq sym major-mode)
              (eq sym aaronzinhoo--last-major-mode)
              (and
               (memq sym minor-mode-list)
               (boundp sym)))
        (propertize cmd 'face 'font-lock-constant-face)
        cmd)))
  (defun aaronzinhoo--save-major-mode ()
    "Function to capture major mode of buffer."
    (when (not (or
                (eq 'minibuffer-mode major-mode)
                (eq 'fundamental-mode major-mode)
                (eq 'minibuffer-inactive-mode major-mode)
                (eq 'special-mode major-mode)))
      (setq aaronzinhoo--last-major-mode major-mode)))
  (defun aaronzinhoo--vertico-quick-embark (&optional arg)
    "Embark on candidate using quick keys."
    (interactive)
    (when (vertico-quick-jump)
      (embark-act arg)))
  :custom
  (vertico-scroll-margin 0)
  (vertico-count 20)                    ; Number of candidates to display
  (vertico-resize t)
  (vertico-grid-separator "       ")
  (vertico-grid-lookahead 50)
  (vertico-buffer-display-action '(display-buffer-reuse-window)) ; Default
  (vertico-multiform-categories                                  ; Choose a multiform
   '((file reverse
           (vertico-sort-function . sort-directories-first)
           (+vertico-transform-functions . +vertico-highlight-directory))
     (consult-grep buffer)
     (consult-location)
     (imenu buffer)
     (library reverse indexed)
     (org-roam-node reverse indexed)
     (jinx grid (vertico-grid-annotate . 20))
     (t reverse)
     ))
  (vertico-multiform-commands
   '((org-refile grid reverse indexed)
     (consult-yank-pop indexed)
     (consult-flycheck)
     (consult-lsp-diagnostics)
     (execute-extended-command reverse
                               (+vertico-transform-functions . aaronzinhoo--vertico-highlight-enabled-mode))
     ))
  :init
  ;; variable to enable highlighting major mode in minibuffer
  (setq aaronzinhoo--last-major-mode nil)
  (vertico-mode)
  (vertico-multiform-mode))
;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;;; Completetion outside of minibuffer

;; correct spelling mistakes
(use-package jinx
  :delight
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))
;; useful company backends
(use-package company-web
  :after (company)
  :init
  (require 'company-web-html))
(use-package company-org-block
  :straight (:type git :host github :repo "aaronzinhoo/company-org-block" :branch "master"))
;; See the Cape README for more tweaks!
(use-package cape
  :demand t
  :custom
  (cape-dabbrev-min-length 2))
(use-package corfu
  :straight (corfu :type git :host github :repo "minad/corfu" :files (:defaults "extensions/*"))
  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
              ("M-SPC"      . corfu-insert-separator)
              ("TAB"        . corfu-complete-common-or-next)
              ([tab]        . corfu-complete-common-or-next)
              ("S-TAB"      . corfu-previous)
              ([backtab]    . corfu-previous)
              ("<return>"   . corfu-insert)
              ("M-p"        . corfu-popupinfo-scroll-up)
              ("M-n"        . corfu-popupinfo-scroll-down))
  :hook ((vterm-mode . (lambda () (setq-local corfu-quit-at-boundary t
                                               corfu-quit-no-match t
                                               corfu-auto nil)
                          (corfu-mode)))
         (eshell-mode . (lambda () (setq-local corfu-quit-at-boundary t
                                               corfu-quit-no-match t
                                               corfu-auto t
                                               completion-at-point-functions (list (cape-capf-buster
                                                                                    (cape-capf-super
                                                                                     #'pcomplete-completions-at-point
                                                                                     #'cape-abbrev))
                                                                                   #'cape-file))
                          (corfu-mode))))
  ;; Optional customizations
  :custom
  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)       ; Always have the same width
  (corfu-count 15)
  (corfu-scroll-margin 4)

  ;; `nil' means to ignore `corfu-separator' behavior, that is, use the older
  ;; `corfu-quit-at-boundary' = nil behavior. Set this to separator if using
  ;; `corfu-auto' = `t' workflow (in that case, make sure you also set up
  ;; `; commentrfu-separator' and a keybind for `corfu-insert-separator', which my
  ;; configuration already has pre-prepared). Necessary for manual corfu usage with
  ;; orderless, otherwise first component is ignored, unless `corfu-separator'
  ;; is inserted.
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match t)
  (corfu-cycle t)                 ; Allows cycling through candidates
  (corfu-auto t)                  ; Enable auto completion
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.0)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-preview-current t)
  (corfu-preselect 'valid)
  (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets
  (corfu-popupinfo-max-height 20)
  (corfu-popupinfo-max-width 70)
  :preface
  (defun corfu-complete-common-or-next ()
    "Complete common prefix or go to next candidate."
    (interactive)
    (if (= corfu--total 1)
        (progn
          (corfu--goto 1)
          (corfu-insert))
      (let* ((input (car corfu--input))
             (str (if (thing-at-point 'filename) (file-name-nondirectory input) input))
             (pt (length str))
             (common (try-completion str corfu--candidates)))
        (if (and (> pt 0)
                 (stringp common)
                 (not (string= str common)))
            (insert (substring common pt))
          (corfu-next)))))
  :init
  ;; local settings for completion at point settings will override this
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode) ; Popup completion info
  )
(use-package imenu-list
  :bind (("s-m" . imenu-list-smart-toggle))
  :custom
  (imenu-list-focus-after-activation t)
  (imenu-list-auto-resize t))
(use-package ag
  :defer 3)
(use-package move-text
  :straight (:type git :host github :repo "emacsfodder/move-text" :branch "master")
  :init
  (move-text-default-bindings))
(use-package avy
  :bind (("M-SPC" . avy-goto-char-timer))
  :custom
  (avy-all-windows nil))
;; TODO add fix for mark outer-tag
(use-package multiple-cursors
  :straight (:type git :host github :repo "magnars/multiple-cursors.el" :branch "master")
  :bind (("M-m" . multiple-cursors-hydra/body))
  :hook ((prog-mode . multiple-cursors-mode)
         (text-mode . multiple-cursors-mode))
  :pretty-hydra
  (multiple-cursors-hydra
    (:hint nil :color pink :quit-key "SPC" :title (with-mdicon "nf-md-cursor_default_outline" "Multiple Cursors" 1 -0.05))
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
      ("RET" newline-and-indent "New Line"))))
  :custom
  (mc/cmds-to-run-for-all
   '(abbrev-prefix-mark
     crux-smart-delete-line
     hungry-delete-backward
     hungry-delete-forward))
  (mc/cmds-to-run-once '(avy-goto-char-timer dap-tooltip-mouse-motion hydra-multiple-cursors/body hydra-multiple-cursors/mc-hide-unmatched-lines-mode hydra-multiple-cursors/mc/edit-lines-and-exit hydra-multiple-cursors/mc/mark-all-dwim hydra-multiple-cursors/mc/mark-all-like-this hydra-multiple-cursors/mc/mark-all-like-this-and-exit hydra-multiple-cursors/mc/mark-next-like-this hydra-multiple-cursors/mc/mark-previous-like-this hydra-multiple-cursors/mc/nil hydra-multiple-cursors/mc/skip-to-next-like-this hydra-multiple-cursors/mc/skip-to-previous-like-this hydra-multiple-cursors/mc/unmark-next-like-this hydra-multiple-cursors/mc/unmark-previous-like-this mc/mark-previous-like-this wgrep-finish-edit)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Creating Diagrams
(use-package plantuml-mode
  :straight (:type git :host github :repo "Aaronzinhoo/plantuml-mode" :branch "master")
  :mode (("\\plantuml\\'" . plantuml-mode))
  :hook (plantuml-mode . aaronzinhoo--plantuml-setup-hook)
  :preface
  (defun aaronzinhoo-plantuml-setup-hook ()
    (setq-local completion-at-point-functions
                (list #'plantuml-completion-at-point #'cape-abbrev #'cape-dabbrev)))
  :custom
  (plantuml-executable-path "plantuml")
  (plantuml-default-exec-mode 'executable))
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
  :bind (:map org-mode-map
              ("s-v" . verb-hydra/body))
  :preface
  (defun aaronzinhoo--verb-kill-this-buffer ()
    (interactive)
    (kill-buffer (buffer-file-name)))
  :pretty-hydra
  (verb-hydra
   (:hint nil :color pink :quit-key "SPC" :title (with-mdicon "nf-md-web" "Verb Mode" 1 -0.05))
   ("Request"
    (("rs" verb-send-request-on-point-other-window-stay "Other Window (Stay)")
     ("ro" verb-send-request-on-point-other-window "Other Window")
     ("rc" verb-send-request-on-point "Current Window"))
    "Kill"
    (("k" aaronzinhoo--verb-kill-this-buffer  "This Response Buffers")
     ("K" verb-kill-all-response-buffers  "All Response Buffers and Windows")))))
(use-package swagg
  :straight (:type git :host github :repo "isamert/swagg.el" :branch "main")
  :commands (swagg-request swagg-request-with-rest-block))
;; add agenda commands to hydra
(use-package org
  :mode (("\\.org$" . org-mode))
  :hook ((org-mode . aaronzinhoo--org-setup)
          (org-mode . aaronzinhoo--org-font-setup))
  :bind
  ("C-c l" . org-store-link)
  ("C-c A" . org-agenda)
  ("C-c c" . org-capture)
  (:map org-mode-map
    ("C-M-<return>" . org-insert-subheading)
    ("s-h". hydra-org-nav/body))
  :preface
  ;; TODO get this to work with cape
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
  (defun aaronzinhoo--org-setup ()
    (variable-pitch-mode t)
    (org-indent-mode t)
    (setq-local completion-at-point-functions (list #'cape-file (cape-company-to-capf #'company-org-block) (cape-capf-super #'cape-dict #'cape-dabbrev))))
  (defun aaronzinhoo--org-font-setup ()
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
  (org-directory (concat (getenv "HOME") "/development/org"))
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
    '(("~/development/org/notebook/programming/web-development.org" :maxlevel . 2)
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
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)
  (org-todo-keywords
    '((sequence "TODO" "IN PROGRESS" "DONE" "DELEGATED")))
  ;; Org styling, hide markup etc.
  ;; (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-ellipsis "…")

  ;; Agenda styling
  (org-agenda-tags-column 0)
  (org-agenda-block-separator ?─)
  (org-agenda-time-grid
    '((daily today require-timed)
       (800 1000 1200 1400 1600 1800 2000)
       " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string
    "◀── now ─────────────────────────────────────────────────")
  (org-agenda-custom-commands
    '(("n" "Agenda and all TODOs" ((agenda "") (alltodo "")))
       ("z" ;key
         "TODO Agenda"           ;description
         agenda                   ;type
         ""                       ;match - empty string for agenda type
         ;; local settings...
         ((org-agenda-files '("~/development/org/gtd.org"))
           (org-deadline-warning-days 5))))
    )
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

  (setq org-capture-templates
    '(("t" "TODO" entry (file+headline "~/development/org/gtd.org" "Tasks")
        "* TODO %?\n  %i\n  %a")
       ("s" "TODO Item to be scheduled" entry (file+headline "~/development/org/gtd.org" "Tasks")
         "* TODO %?\n SCHEDULED: %t")
       ("d" "TODO item with a deadline" entry (file+headline "~/development/org/gtd.org" "Tasks")
         "* TODO %?\n DEADLINE: %t")
       ("j" "Journal" entry (file+datetree "~/development/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")
       ("a"                          ; key
         "Article"                    ; name
         entry                        ; type
         (file+headline "~/development/org/references/articles.org" "Article") ; target
         "* %^{Title} %(org-set-tags)  :article: \n:PROPERTIES:\n:Created: %U\n:Linked: %a\n:END:\n%i\nBrief description:\n%?" ; template
         :prepend t                   ; properties
         :empty-lines 1               ; properties
         :created t                   ; properties
         )))
  (require 'ox-publish)
  )
(use-package org-modern
  :hook (org-mode . org-modern-mode))
(use-package org-contrib
  :after org)
(use-package org-ref
  :after org
  :custom
  (org-ref-notes-directory "~/development/org/notebook/")
  (org-ref-default-bibliography '("~/development/org/references/articles.bib"))
  (org-ref-pdf-directory "~/development/org/pdfs/")
  (bibtex-completion-bibliography "~/development/org/references/articles.bib")
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
(use-package bibtex-completion
  :defer t
  :custom
  (bibtex-completion-pdf-symbol "")
  (bibtex-completion-notes-symbol "")
  (bibtex-completion-library-path "~/development/org/pdfs/")
  (bibtex-completion-notes-path "~/development/org/notebook/")
  (bibtex-completion-bibliography '("~/development/org/references/articles.bib"))
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
  ;;              '("p" "Protocol" entry (file "~/development/org/references/articles.org")
  ;;                "* %?[[%:link][%:description]] %U\n%i\n" :prepend t))
  ;; (add-to-list 'org-capture-templates
  ;;              '("L" "Protocol Link" entry (file+headline "~/development/org/references/articles.org" "Links:")
  ;;                "* %?[[%:link][%:description]] %U\n" :prepend t))
  )
(use-package org-sidebar
  :straight (org-sidebar :type git :host github :repo "alphapapa/org-sidebar"))
;; autoload html files org
(use-package org-preview-html
  :straight t)
(use-package org-noter
  :custom
  (org-noter-default-notes-file-names '("machine_learning.org" "cognitive_science.org" "programming_languages.org" "finance.org"))
  (org-noter-notes-search-path '("~/development/org/notebook")))
;; use eldoc in org-mode
(use-package org-eldoc
  :straight nil
  :after (org))

;;; PDF support
(use-package pdf-tools
  :custom
  (pdf-view-display-size 'fit-width))

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
;; NOTE C-c C-t vterm-copy-mode for copying vterm text!
(use-package vterm
  :commands vterm
  :preface
  (setq vterm-install t)
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 10000)
  (confirm-kill-processes nil)
  (hscroll-margin 0))
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
  :after (major-mode-hydra)
  :hook (dashboard-mode . projectile-mode)
  :bind ("s-p" . projectile-hydra/body)
  :pretty-hydra
  ((:hint nil :color teal :quit-key "SPC" :title (with-octicon "nf-oct-rocket" "Projectile" 1 -0.05))
   ("Buffers"
    (("b" consult-projectile-switch-to-buffer "list")
     ("k" projectile-kill-buffers "kill all")
     ("S" projectile-save-project-buffers "save all"))
    "Find"
    (("d" consult-projectile-find-dir "directory")
     ("D" projectile-dired "proj. root")
     ("f" consult-projectile-find-file "file")
     ("p" consult-projectile-switch-project "project")
     ("F" projectile-find-file-in-known-projects "file (all proj.)"))
    "Other"
    (("N" projectile-cleanup-known-projects)
     ("i" projectile-invalidate-cache "reset cache")
     ("c" projectile-compile-project "compile")
     ("v" projectile-run-vterm "run vterm"))
    "Search & Replace"
    (("r" projectile-replace "replace")
     ("R" projectile-replace-regexp "regexp replace")
     ("s" consult-ripgrep "search"))
    "Tests"
    (("ts" projectile-toggle-between-implementation-and-test "switch to test|implementation file")
     ("tt" projectile-test-project "run tests")
     ("tf" projectile-find-test-file "find test file"))))
  :custom
  (projectile-git-fd-args "-H -0 -E .git -tf")
  (projectile-generic-command "fd . -0 --type f --color=never")
  (projectile-find-dir-includes-top-level t)
  ;; use .gitignore to exclude files from search
  (projectile-indexing-method 'alien)
  (projectile-enable-caching t)
  (projectile-sort-order 'recentf))


;;; Languages Support

;; folding
;; (use-package ts-fold
;;   :straight (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold"))

;; Indent Guides
(use-package highlight-indent-guides)

;; Code Coverage
(use-package cov
  :defer t)
(use-package coverlay
  :commands (coverlay-mode))

;; Yaml editing support and JSON
;; json-mode => json-snatcher json-refactor
;; select yaml regex (^-[\s]*[A-Za-z0-9-_]*)|(^[A-Za-z_-]*:)
(use-package openapi-yaml-mode
  :straight (:type git :host github :repo "magoyette/openapi-yaml-mode" :branch "master")
  :ensure t
  :hook ((openapi-yaml-mode . yas-minor-mode)))
(use-package openapi-preview
  :commands (openapi-preview)
  :straight (:type git :host github :repo "merrickluo/openapi-preview" :branch "main")
  :custom
  (openapi-preview-redoc-command "redoc-cli"))
(use-package yaml-pro
  :commands (yaml-pro-mode yaml-pro-ts-mode)
  :straight (:type git :host github :repo "zkry/yaml-pro" :branch "master")
  )
(use-package yaml-mode
  :demand t)
(use-package yaml-ts-mode
  :straight nil
  :bind ((:map yaml-ts-mode-map
           ("s-h" . yaml-hydra/body)))
  :hook ((yaml-ts-mode . yaml-pro-mode)
         (docker-compose-mode . yaml-ts-mode)
         (yaml-pro-mode . yaml-pro-ts-mode)
         (yaml-ts-mode . aaronzinhoo-yaml-mode-hook)
         (yaml-ts-mode . flycheck-mode)
         (yaml-ts-mode . hungry-delete-mode)
         (yaml-ts-mode . (lambda () (setq-local tab-width 2)))
         (yaml-ts-mode . (lambda () (setq-local flycheck-local-checkers '((yaml-yamllint . ((next-checkers . (swagger-cli lsp)))))))))
  :bind (:map yaml-ts-mode-map ("<backtab>" . yaml-indent-line))
  :preface
  (defun aaronzinhoo-yaml-mode-hook ()
    (setq-local lsp-java-boot-enabled nil)
    (setq-local eldoc-mode nil)
    (setq-local completion-at-point-functions (list #'cape-file (cape-capf-super (cape-capf-buster #'lsp-completion-at-point) #'cape-dabbrev) #'cape-dict))
    (yaml-pro-mode nil))
  :pretty-hydra
  (yaml-hydra
    (:hint nil :title (with-faicon "nf-fa-yen" "Yaml Commands" 1 -0.05) :quit-key "q" :color red)
    ("Indent"
      (("i" yaml-pro-ts-indent-subtree "Indent")
        ("u" yaml-pro-ts-unindent-subtree "Unindent"))
      "Navigation"
      (("j" combobulate-avy-jump "Jump")
        ("n" yaml-pro-ts-next-subtree "Next Sibling Node")
        ("p" yaml-pro-ts-prev-subtree "Previous Sibling Node")
        ("N" block-nav-next-indentation-level "Next Child Node")
        ("P" yaml-pro-ts-up-level "Previous Parent Node"))
      "Fold"
      (("f" yaml-pro-fold-at-point "Fold")
        ("F" yaml-pro-unfold-at-point "Unfold")))))
(use-package json-ts-mode
  :straight nil
  :mode (("\\.json$" . json-ts-mode))
  :hook (json-ts-mode . prettier-mode)
  :after (flycheck)
  :config
  (setq-local js-indent-level 2))
(use-package dotenv-mode
  :mode ("\\.env\\'" . dotenv-mode))
(use-package jenkinsfile-mode
  :mode ("\\Jenkinsfile\\'" . jenkinsfile-mode)
  :preface
  (defun aaronzinhoo-company-jenkinsfile-mode-hook ()
    (setq-local completion-at-point-functions (list #'cape-file #'cape-keyword #'cape-dabbrev #'cape-dict)))
  :config
  (add-hook 'jenkinsfile-mode-hook 'aaronzinhoo-company-jenkinsfile-mode-hook))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DEVOPS CONFIG
(use-package docker
  :straight t
  :commands (docker)
  :bind ("s-d" . docker))
(use-package dockerfile-mode
  :commands (dockerfile-build-buffer dockerfile-build-no-cache-buffer)
  :straight (:type git :host github :repo "spotify/dockerfile-mode" :branch "master"))
(use-package dockerfile-ts-mode
  :straight nil
  :hook (dockerfile-mode . dockerfile-ts-mode)
  :mode ("Dockerfile\\'" . dockerfile-ts-mode)
  :bind (:map dockerfile-ts-mode-map
              ("s-h" . dockerfile-hydra/body))
  :pretty-hydra
  (dockerfile-hydra
   (:hint nil :title (with-mdicon "nf-md-docker" "Dockerfile Commands" 1 -0.05) :quit-key "SPC" :color pink)
   ("Build"
    (("b" dockerfile-build-buffer "Build Image")
     ("B" dockerfile-build-no-cache-buffer "Build Image W/O Cache")))))
;; kubernetes settings overview
(use-package kubel
  :after (vterm)
  :config (kubel-vterm-setup))
(use-package kubernetes
  :straight (:type git :host github :repo "kubernetes-el/kubernetes-el" :branch "master")
  :defer t
  :commands (kubernetes-overview)
  :custom
  (kubernetes-overview-custom-views-alist '((my-view . (context pods configmaps secrets deployments))))
  (kubernetes-default-overview-view 'my-view)
  :config
  (setq kubernetes-poll-frequency 5
        kubernetes-redraw-frequency 5))
(use-package kele
  :demand t
  :straight (:type git :host github :repo "jinnovation/kele.el" :branch "main")
  :config
  (define-key kele-mode-map (kbd "s-k") kele-command-map)
  (kele-mode t))
;;; WEB-DEV CONFIG

;; apache
(use-package apache-mode
  :straight (:type git :host github :repo "PommesSchranke/apache-mode" :branch "customizable-faces")
  :mode (("apache2\\.conf\\'" . apache-mode)
         ("httpd\\.conf\\'" . apache-mode))
  :hook ((apache-mode . aaronzinhoo-apache2-company-mode-setup))
  :preface
  (defun aaronzinhoo-apache2-company-mode-setup ()
    (setq-local completion-at-point-functions (list #'cape-file #'cape-keyword #'cape-dabbrev #'cape-dict))))

;; using verb instead because it is better
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
;; TODO add electric pair for < or add snippet
(use-package nxml-mode
  :straight nil
  :bind ((:map nxml-mode-map
               ("s-h" . hydra-web/body)))
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
  :hook ((css-mode . css-ts-mode)
         (css-ts-mode . aaronzinhoo--css-setup-hook)
         (scss-mode . aaronzinhoo--css-setup-hook))
  :preface
  (defun aaronzinhoo--css-setup-hook ()
    (setq-local completion-at-point-functions (list #'css-completion-at-point #'cape-file #'cape-dabbrev #'cape-dict)))
  :custom
  (css-indent-offset 2))
(use-package web-mode
  :straight (:type git :host github :repo "fxbois/web-mode" :branch "master")
  :hook (web-mode . aaronzinhoo--web-mode-hook)
  :mode (("\\.html\\'" . html-ts-mode)
         ("\\.component.html\\'" . html-ts-mode))
  :bind ((:map web-mode-map
               ("s-h" . web-mode-hydra/body)))
  :pretty-hydra
  ((:hint nil :title (with-octicon "nf-oct-globe" "Web Mode Control" 1 -0.05) :quit-key "SPC" :color pink)
   ("Navigation"
    (("a" sgml-skip-tag-backward "tag beginning | prev tag")
     ("e" sgml-skip-tag-forward "tag end | next tag")
     ("n" web-mode-element-next "next tag")
     ("p" web-mode-element-previous "previous tag")
     ("F" web-mode-element-children-fold-or-unfold "fold/unfold tag children")
     ("f" web-mode-fold-or-unfold "fold/unfold"))
    "Edit"
    (("d" aaronzinhoo-delete-tag "delete tag"))
    "Error"
    (("v" html-check-frag-next "next html error")
     ("E" web-mode-dom-errors-show "show errors"))
    "Action"
    (("w" web-mode-element-wrap "wrap element in tag" ));end action
    "Other"
    (("s" helm-emmet "Insert Emmet Snippet")
     ("RET" nil "Quit" :color blue))))
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
  (defun aaronzinhoo--web-mode-hook ()
    (setq-local completion-at-point-functions (list #'lsp-completion-at-point #'cape-file (cape-capf-super (cape-company-to-capf #'company-web-html) #'css-completion-at-point) #'cape-dabbrev #'cape-dict)))
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
              ("s-h" . markdown-mode-hydra/body))
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :hook ((markdown-mode . flycheck-mode)
         (markdown-mode . aaronzinhoo--markdown-mode-hook)
         (gfm-mode      . aaronzinhoo--markdown-mode-hook))
  :preface
  (defun aaronzinhoo--markdown-mode-hook ()
    (setq-local completion-at-point-functions
                (list #'cape-file #'cape-dabbrev #'cape-dict)))
  :pretty-hydra
  ((:hint nil :title (with-octicon "nf-oct-markdown" "Markdown Mode Control" 1 -0.05) :quit-key "SPC" :color pink)
   ("Insert"
    (("it" markdown-insert-table "table")
     ("ii" markdown-insert-image "image")
     ("ib" markdown-insert-uri "uri")
     ("ic" markdown-insert-code-block "code block")
     ("id" markdown-insert-gfm-checkbox "checkbox"))
    "Preview"
    (("p" impatient-showdown-mode "Preview" :toggle t))
    "Action"
    (("o" markdown-open "Open" :color blue))
    ))
  :custom
  (markdown-command "pandoc"))
;; org github-esque markdown export
(use-package ox-gfm
  :after org)
;; markdown visualization
(use-package impatient-showdown
  :after (markdown-mode)
  :custom
  (impatient-showdown-flavor 'github))


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
         (typescript-ts-mode . prettier-mode))
  :custom
  (prettier-mode-sync-config-flag t))
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
(use-package typescript-ts-mode
  :delight " Ts"
  :hook ((typescript-ts-mode . subword-mode)
          (typescript-ts-mode . aaronzinhoo--typescript-mode-hook))
  :preface
  (defun aaronzinhoo--typescript-mode-hook ()
    (setq-local completion-at-point-functions (list #'cape-file (cape-capf-super (cape-capf-buster #'lsp-completion-at-point) #'cape-dabbrev) #'cape-dict))))
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
  :straight nil
  :delight " Py"
  :bind (:map python-ts-mode-map
              ("s-h" . python-hydra/body))
  :hook ((python-ts-mode . pyvenv-mode)
         ;; (python-ts-mode . combobulate-mode)
         (python-ts-mode . (lambda () (aaronzinhoo--python-setup))))
  :pretty-hydra
  (python-hydra
   (:hint nil :color pink :quit-key "SPC" :title (with-mdicon "nf-md-language_python" "Python Mode" 1 -0.05))
   ("Run"
    (("sh" run-python "Python Shell")
     ("d" pdb "PDB" :color blue)
     ("ss" python-shell-switch-to-shell "Switch to sh" :color blue)
     ("v" projectile-run-vterm "run vterm"))
    "Run in Python Shell"
    (("rb" python-shell-send-buffer "Run Buffer")
     ("rf" python-shell-send-file "Run File")
     ("rc" aaronzinhoo--python-shell-send-current-file "Run Current File")
     ("rr" python-shell-send-region "Run Region"))
    "Formatting"
    (("i" python-fix-imports "Fix Imports")
     ("a" python-add-import "Add Import")
     ("f" py-autopep8-mode "Autopep8 Mode" :toggle t))
    "Navigation/Editing"
    (("c" combobulate-mode "Combobulate Mode" :toggle t)
     ("j" combobulate-avy "Jump")
     ("ed" combobulate-edit "Edit")
     ("en" combobulate-envelop "Envelop"))))
  :preface
  (defun aaronzinhoo--python-shell-send-current-file ()
    (interactive)
    (python-shell-send-file (buffer-file-name)))
  (defun aaronzinhoo--python-setup ()
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
  :custom
  (python-check-command "flake8")
  :init
  (setenv "WORKON_HOME" (concat pyenv-root-folder "/versions"))
  (setenv "VIRTUALENVWRAPPER_PYTHON" (concat pyenv-root-folder "/shims/python"))
  (setenv "VIRTUALENVWRAPPER_VIRTUALENV" (concat pyenv-root-folder "/shims/python"))
  (setenv "PIPENV_PYTHON" (concat pyenv-root-folder "/shims/python"))
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
  (py-autopep8-options '("--max-line-length 140")))

;; Golang Setup
;; export GO111MODULE="on" might be needed
;; need a package if not in GOPATH!
(use-package go-playground
  :commands (go-playground)
  :straight (:type git :host github :repo "grafov/go-playground" :branch "master")
  :config
  (defun my/go-playground-remove-lsp-workspace ()
    (when-let ((root (lsp-workspace-root))) (lsp-workspace-folders-remove root)))
  (add-hook 'go-playground-pre-rm-hook #'my/go-playground-remove-lsp-workspace))
(use-package go-mod-ts-mode
  :straight nil
  :mode ("\\.mod\\'" . go-mod-ts-mode))
(use-package go-ts-mode
  :straight nil
  :mode ("\\.go\\'" . go-ts-mode)
  :hook ((go-ts-mode . subword-mode)
         (go-ts-mode . yas-minor-mode))
  :custom
  (go-ts-mode-indent-offset 4)
  :init
  ;;Smaller compilation buffer
  (setq-local compilation-windowp-height 14)
  (defun my-compilation-hook ()
    (when (not (get-buffer-window "*compilation*"))
      (save-selected-window
        (save-excursion
          (let* ((w (split-window-vertically))
                 (h (window-height w)))
            (select-window w)
            (switch-to-buffer "*compilation*")
            (shrink-window (- h compilation-window-height)))))))
  (setq compilation-read-command nil)
  :bind (:map go-ts-mode-map
              ("M-," . compile)
              ("M-." . godef-jump)
              ("M-*" . pop-tag-mark))
  :config
  (setq-local compilation-scroll-output t))

;; C++ / C
;; lsp-mode + ccls for debugging
;; configuration: use set(CMAKE_EXPORT_COMPILE_COMMANDS ON) in cmake file
;; cmake-mode + cmake-font-lock for editing cmake files
(use-package ccls
  :custom
  (ccls-args nil)
  (ccls-executable "ccls")
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp))))
(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode))
  :hook (cmake-mode . cmake-ts-mode))

;;; Rust
(use-package rust-mode)
(use-package rustic)
(use-package rust-ts-mode
  :straight nil
  :mode ("\\.rs\\'" . rust-ts-mode)
  :preface
  (defun cargo-run-offline ()
    (interactive)
    (rustic-cargo-run-command "--offline")))
(use-package cargo-mode
  :hook
  (rust-ts-mode . cargo-minor-mode)
  (rust-ts-mode . (lambda () (setq-local flycheck-local-checkers '((rust-clippy . ((next-checkers . (rust-cargo)))))))))

;;; Java | C++ | C
(use-package groovy-mode
  :defer t)
(use-package conf-javaprop-mode
  :straight nil
  :mode ("\\.properties'" . conf-javaprop-mode))
(use-package java-ts-mode
  :demand t
  :straight nil
  :mode (("\\.java\\'" . java-ts-mode))
  :hook ((java-ts-mode . (lambda () (setq c-basic-offset 4 tab-width 4)))
          (java-ts-mode . subword-mode))
  ;; define the hydra with the mode since the mode-map may not be defined yet
  :bind (:map java-ts-mode-map
              ("s-h" . java-hydra/body))
  :pretty-hydra
  (java-hydra
   (:hint nil :color pink :quit-key "SPC" :title (with-mdicon "nf-md-language_java" "Java LSP Mode" 1 -0.05))
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
     ("tl" lsp-jt-lens-mode "Testing Lens Mode" :toggle t)))))

;; protobuf
(use-package protobuf-ts-mode
  :straight (:type git :host github :repo "emacsattic/protobuf-ts-mode" :branch "master")
  :mode (("\\.proto\\'" . protobuf-ts-mode)))
(use-package flycheck-buf-lint
  :straight t
  :hook ((protobuf-mode protobuf-ts-mode) . (lambda() (flycheck-buf-lint-setup))))

;;; SQL Mode
(use-package sqlformat
  :straight (:type git :host github :repo "purcell/sqlformat" :branch "master")
  :hook (sql-mode . sqlformat-on-save-mode)
  :custom
  (sqlformat-command 'pgformatter))

;;; Emacs Lisp Mode
(use-package emacs-lisp-mode
  :straight nil
  :hook (emacs-lisp-mode . aaronzinhoo--setup-elisp-mode)
  :preface
  (defun aaronzinhoo--setup-elisp-mode ()
    (setq-local lisp-indent-offset 2)
    (setq-local completion-at-point-functions (list #'cape-file (cape-capf-super #'elisp-completion-at-point #'cape-dabbrev) #'cape-dict))))
(use-package elisp-autofmt
  :commands (elisp-autofmt-mode elisp-autofmt-buffer)
  :hook (emacs-lisp-mode . elisp-autofmt-mode))

;;;
(use-package bash-ts-mode
  :straight nil
  :hook (bash-ts-mode . aaronzinhoo--setup-bash-ts-mode)
  :preface
  (defun aaronzinhoo--setup-bash-ts-mode ()
    (setq-local completion-at-point-functions (list #'cape-file (cape-capf-super #'lsp-completion-at-point #'sh-completion-at-point-function #'comint-completion-at-point #'cape-dabbrev) #'cape-dict))))

(use-package ansible
  :hook (yaml-ts-mode . ansible))

;;; Theme
(use-package doom-modeline
  :custom
  ;; If non-nil, cause imenu to see `doom-modeline' declarations.
  ;; This is done by adjusting `lisp-imenu-generic-expression' to
  ;; include support for finding `doom-modeline-def-*' forms.
  ;; Must be set before loading doom-modeline.
  (doom-modeline-support-imenu t)

  ;; How tall the mode-line should be. It's only respected in GUI.
  ;; If the actual char height is larger, it respects the actual height.
  (doom-modeline-height 25)

  ;; How wide the mode-line bar should be. It's only respected in GUI.
  (doom-modeline-bar-width 4)

  ;; Whether to use hud instead of default bar. It's only respected in GUI.
  (doom-modeline-hud nil)

  ;; The limit of the window width.
  ;; If `window-width' is smaller than the limit, some information won't be
  ;; displayed. It can be an integer or a float number. `nil' means no limit."
  (doom-modeline-window-width-limit 85)

  ;; How to detect the project root.
  ;; nil means to use `default-directory'.
  ;; The project management packages have some issues on detecting project root.
  ;; e.g. `projectile' doesn't handle symlink folders well, while `project' is unable
  ;; to hanle sub-projects.
  ;; You can specify one if you encounter the issue.
  (doom-modeline-project-detection 'auto)

  ;; Determines the style used by `doom-modeline-buffer-file-name'.
  ;;
  ;; Given ~/Projects/FOSS/emacs/lisp/comint.el
  ;;   auto => emacs/l/comint.el (in a project) or comint.el
  ;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
  ;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
  ;;   truncate-with-project => emacs/l/comint.el
  ;;   truncate-except-project => ~/P/F/emacs/l/comint.el
  ;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
  ;;   truncate-all => ~/P/F/e/l/comint.el
  ;;   truncate-nil => ~/Projects/FOSS/emacs/lisp/comint.el
  ;;   relative-from-project => emacs/lisp/comint.el
  ;;   relative-to-project => lisp/comint.el
  ;;   file-name => comint.el
  ;;   buffer-name => comint.el<2> (uniquify buffer name)
  ;;
  ;; If you are experiencing the laggy issue, especially while editing remote files
  ;; with tramp, please try `file-name' style.
  ;; Please refer to https://github.com/bbatsov/projectile/issues/657.
  (doom-modeline-buffer-file-name-style 'auto)

  ;; Whether display icons in the mode-line.
  ;; While using the server mode in GUI, should set the value explicitly.
  (doom-modeline-icon t)

  ;; Whether display the icon for `major-mode'. It respects `doom-modeline-icon'.
  (doom-modeline-major-mode-icon t)

  ;; Whether display the colorful icon for `major-mode'.
  ;; It respects `nerd-icons-color-icons'.
  (doom-modeline-major-mode-color-icon t)

  ;; Whether display the icon for the buffer state. It respects `doom-modeline-icon'.
  (doom-modeline-buffer-state-icon t)

  ;; Whether display the modification icon for the buffer.
  ;; It respects `doom-modeline-icon' and `doom-modeline-buffer-state-icon'.
  (doom-modeline-buffer-modification-icon t)

  ;; Whether display the time icon. It respects variable `doom-modeline-icon'.
  (doom-modeline-time-icon t)

  ;; Whether to use unicode as a fallback (instead of ASCII) when not using icons.
  (doom-modeline-unicode-fallback nil)

  ;; Whether display the buffer name.
  (doom-modeline-buffer-name t)

  ;; Whether highlight the modified buffer name.
  (doom-modeline-highlight-modified-buffer-name t)

  ;; Whether display the minor modes in the mode-line.
  (doom-modeline-minor-modes nil)

  ;; If non-nil, a word count will be added to the selection-info modeline segment.
  (doom-modeline-enable-word-count nil)

  ;; Major modes in which to display word count continuously.
  ;; Also applies to any derived modes. Respects `doom-modeline-enable-word-count'.
  ;; If it brings the sluggish issue, disable `doom-modeline-enable-word-count' or
  ;; remove the modes from `doom-modeline-continuous-word-count-modes'.
  (doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))

  ;; Whether display the buffer encoding.
  (doom-modeline-buffer-encoding t)

  ;; Whether display the indentation information.
  (doom-modeline-indent-info nil)

  ;; If non-nil, only display one number for checker information if applicable.
  (doom-modeline-checker-simple-format t)

  ;; The maximum number displayed for notifications.
  (doom-modeline-number-limit 99)

  ;; The maximum displayed length of the branch name of version control.
  (doom-modeline-vcs-max-length 12)

  ;; Whether display the workspace name. Non-nil to display in the mode-line.
  (doom-modeline-workspace-name t)

  ;; Whether display the `lsp' state. Non-nil to display in the mode-line.
  (doom-modeline-lsp t)

  ;; Whether display the GitHub notifications. It requires `ghub' package.
  (doom-modeline-github nil)

  ;; The interval of checking GitHub.
  (doom-modeline-github-interval (* 30 60))

  ;; Whether display the modal state.
  ;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
  (doom-modeline-modal t)

  ;; Whether display the modal state icon.
  ;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
  (doom-modeline-modal-icon t)

  ;; Whether display the battery status. It respects `display-battery-mode'.
  (setq doom-modeline-battery t)

  ;; Whether display the time. It respects `display-time-mode'.
  (doom-modeline-time t)

  ;; Whether display the misc segment on all mode lines.
  ;; If nil, display only if the mode line is active.
  (doom-modeline-display-misc-in-all-mode-lines t)

  ;; Whether display the environment version.
  (doom-modeline-env-version t)

  ;; Change the executables to use for the language version string
  (doom-modeline-env-python-executable "python") ; or `python-shell-interpreter'
  (doom-modeline-env-ruby-executable "ruby")
  (doom-modeline-env-perl-executable "perl")
  (doom-modeline-env-go-executable "go")
  (doom-modeline-env-elixir-executable "iex")
  (doom-modeline-env-rust-executable "rustc")

  ;; What to display as the version while a new one is being loaded
  (doom-modeline-env-load-string "...")
  :init
  (doom-modeline-mode t))
(use-package catppuccin-theme
  :straight (:type git :host github :repo "catppuccin/emacs" :branch "main" :local-repo "catppuccin-theme"))
(use-package nordtheme
  :straight (:type git :host github :repo "nordtheme/emacs" :branch "develop" :local-repo "nord-theme"))
(use-package moe-theme
  :demand t
  :straight (moe-theme :type git :host github :repo "kuanyui/moe-theme.el" :branch "dev")
  :custom
  (moe-theme-highlight-buffer-id t)
  :config
  (require 'moe-theme-switcher)
  (moe-theme-auto-switch))
(message "Done loading packages")

;;; init.el ends here

;; Local Variables:
;; jinx-local-words: "config"
;; End:
