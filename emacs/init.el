;;; init.el --- Aaron's Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Main Emacs configuration.
;; Generated files are redirected from `early-init.el'.

;;; Code:

(message "Loading init.el...")

;;;; Straight.el

;; Configure Straight before bootstrapping it.
(setq straight-check-for-modifications
  '(find-when-checking)
  straight-use-package-by-default t
  straight-built-in-pseudo-packages
  '(emacs
     flymake
     image-mode
     nadvice
     project
     python
     seq
     xref))

(defvar bootstrap-version)

(let ((bootstrap-file
        (expand-file-name
          "straight/repos/straight.el/bootstrap.el"
          straight-base-dir))
      (bootstrap-version 7))
  (unless
    (file-exists-p bootstrap-file)
    (with-current-buffer
      (url-retrieve-synchronously
        "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
        'silent
        'inhibit-cookies)
      (goto-char
        (point-max))
      (eval-print-last-sexp)))

  (load bootstrap-file nil 'nomessage))

;; Explicitly install use-package before using it.
(straight-use-package 'use-package)
(require 'use-package)

(setq use-package-always-defer t
      use-package-compute-statistics t)

;; Prevent Straight from installing external copies of these built-in
;; compatibility libraries.
(straight-use-package
  '(seq :type built-in))

;;;; Core Emacs behavior

(use-package emacs
  :straight nil
  :hook ((minibuffer-setup . cursor-intangible-mode)
         (prog-mode . aaronzinhoo--enable-trailing-whitespace-cleanup))
  :bind* (("M-<up>" . move-text-up)
          ("M-<down>" . move-text-down)
          ;; Navigation.
          ("C-j" . avy-goto-char-timer)
          ("M-h" . backward-char)
          ("M-j" . next-line)
          ("M-k" . previous-line)
          ("M-l" . forward-char)
          ("M-[" . backward-up-list)
          ("M-]" . up-list)
          ;; Yank and mark.
          ("M-q" . yank)
          ("M-4" . pop-to-mark-command)
          ;; Windows and buffers.
          ("M-," . previous-window-any-frame)
          ("C-x k" . kill-current-buffer)
          ("C-x C-k" . kill-buffer-and-window)
          ("C-x 2" . aaronzinhoo--split-window-below)
          ("C-x 3" . aaronzinhoo--split-window-right)
          ("C-<" . previous-buffer)
          ("C->" . next-buffer)
          ("s-<tab>" . iflipb-next-buffer)
          ("s-S-<tab>" . iflipb-previous-buffer)
          ;; Ignore system gestures.
          ("<pinch>" . ignore)
          ("<C-wheel-up>" . ignore)
          ("<C-wheel-down>" . ignore)
          ("<Scroll_Lock>" . ignore))
  :custom
  ;; Minibuffer.
  (enable-recursive-minibuffers t)
  (minibuffer-prompt-properties
   '(read-only t
               cursor-intangible t
               face minibuffer-prompt))
  ;; Editing.
  (tab-always-indent 'complete)
  (delete-pair-blink-delay 0)
  (default-input-method nil)
  ;; Persistence.
  (auto-save-default t)
  (backup-by-copying t)
  (create-lockfiles nil)
  (delete-by-moving-to-trash t)
  (delete-old-versions t)
  (kept-new-versions 10)
  (kept-old-versions 0)
  (make-backup-files t)
  ;; Display.
  (blink-cursor-blinks -1)
  (select-enable-clipboard t)
  :custom-face
  (variable-pitch
   ((t
     (:family "Cantarell"
							:weight regular))))
  :preface
  (defun aaronzinhoo--enable-trailing-whitespace-cleanup ()
    "Delete trailing whitespace when saving the current buffer."
    (add-hook
     'before-save-hook
     #'delete-trailing-whitespace
     nil
     t))
  (defun aaronzinhoo--wsl-p ()
    "Return non-nil when Emacs is running under WSL."
    (and
     (eq system-type 'gnu/linux)
     (or
      (getenv "WSL_DISTRO_NAME")
      (and
       (file-readable-p
        "/proc/sys/kernel/osrelease")
       (with-temp-buffer
         (insert-file-contents
          "/proc/sys/kernel/osrelease")
         (goto-char
          (point-min))
         (re-search-forward
          "microsoft"
          nil
          t))))))
  (defun aaronzinhoo--split-window-below ()
    "Split below, select the new window, and show its previous buffer."
    (interactive)
    (select-window
     (split-window-below))
    (switch-to-prev-buffer))
  (defun aaronzinhoo--split-window-right ()
    "Split right, select the new window, and show its previous buffer."
    (interactive)
    (select-window
     (split-window-right))
    (switch-to-prev-buffer))
  (defun aaronzinhoo-create-uuid ()
    "Return a newly generated UUID."
    (require 'subr-x)
    (let ((executable
           (executable-find "uuidgen")))
      (unless executable
        (user-error "The uuidgen executable is unavailable"))
      (with-temp-buffer
        (unless
						(zerop
             (call-process
              executable
              nil
              t))
          (user-error "Could not generate a UUID"))
        (downcase
         (string-trim
          (buffer-string))))))
  (defun aaronzinhoo-insert-uuid ()
    "Insert a newly generated UUID at point."
    (interactive)
    (insert
     (aaronzinhoo-create-uuid)))
  (defun aaronzinhoo--append-capfs (&rest capfs)
    "Append CAPFS to the buffer-local completion functions.

Preserve existing CAPFs and remove duplicate entries."
    (setq-local completion-at-point-functions
                (delete-dups
                 (append
                  (copy-sequence completion-at-point-functions)
                  capfs))))
  :init
  ;; Settings and modes needed during initialization.
  (setq-default
   user-full-name "Aaron Gonzales"
   user-mail-address "aarongonzales1@gmail.com"
   calendar-latitude 33.916403
   calendar-longitude -118.352575
   indent-tabs-mode nil
   scroll-up-aggressively 0.01
   scroll-down-aggressively 0.01
   tab-width 4
   tab-stop-list (number-sequence 4 120 4)
   cursor-in-non-selected-windows nil)
  (pixel-scroll-precision-mode 1)
  (minibuffer-depth-indicate-mode 1)
  ;; Only relevant to graphical X11 Emacs.
  (when
      (eq window-system 'x)
    (setq x-select-request-type
          '(UTF8_STRING
            COMPOUND_TEXT
            TEXT
            STRING)))
  (define-key
   key-translation-map
   (kbd "<menu>")
   #'event-apply-super-modifier)
  :config
  (global-auto-revert-mode 1)
  ;;;; Character encoding
  (set-language-environment "UTF-8")
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  ;;;; Enabled commands
  (put 'erase-buffer 'disabled nil)
  (put 'narrow-to-region 'disabled nil)
  (put 'downcase-region 'disabled nil))
(use-package delsel
  :straight nil
  :init
  (delete-selection-mode 1))
(use-package display-line-numbers
  :straight nil
  :hook
  ((conf-mode . display-line-numbers-mode)
   (prog-mode . display-line-numbers-mode)
   (text-mode . display-line-numbers-mode)))
(use-package window
  :straight nil
  :custom
  ;; Use pixel measurements when resizing graphical windows.
  (window-resize-pixelwise t)
  ;; Avoid rearranging dedicated windows such as Treemacs.
  (transpose-dedicated-windows nil)
  :preface
  (defun aaronzinhoo--window-layout-rotate-180 ()
    "Rotate the current frame's window layout by 180 degrees."
    (interactive)
    (window-layout-rotate-clockwise)
    (window-layout-rotate-clockwise))
  (defcustom aaronzinhoo--window-resize-step 5
    "Number of rows or columns used when resizing a window."
    :type 'positive-integer
    :group 'windows)
  :init
  (winner-mode 1))
(use-package hideshow
  :straight nil
  :commands(hs-minor-mode
            hs-toggle-hiding
            hs-hide-block
            hs-show-block
            hs-hide-all
            hs-show-all
            hs-hide-level)
  :hook
  ((bash-ts-mode . hs-minor-mode)
   (c-ts-mode . hs-minor-mode)
   (c++-ts-mode . hs-minor-mode)
   (css-ts-mode . hs-minor-mode)
   (go-ts-mode . hs-minor-mode)
   (html-ts-mode . hs-minor-mode)
   (java-ts-mode . hs-minor-mode)
   (js-ts-mode . hs-minor-mode)
   (python-ts-mode . hs-minor-mode)
   (rust-ts-mode . hs-minor-mode)
   (tsx-ts-mode . hs-minor-mode)
   (typescript-ts-mode . hs-minor-mode)
   (yaml-ts-mode . hs-minor-mode)

   ;; Custom modes based on YAML Tree-sitter.
   (helm-ts-mode . hs-minor-mode)
   (openapi-yaml-mode . hs-minor-mode))
  :preface
  (defun aaronzinhoo--hs-block-beginning ()
    "Move to the beginning of the innermost foldable block."
    (unless
      hs-minor-mode
      (user-error
        "Hideshow mode is not enabled"))
    (unless
      (hs-find-block-beginning)
      (user-error
        "No foldable block found")))
  (defun aaronzinhoo--hs-toggle-block ()
    "Toggle the innermost foldable block containing point."
    (interactive)
    (save-excursion
      (aaronzinhoo--hs-block-beginning)
      (hs-toggle-hiding)))
  (defun aaronzinhoo--hs-hide-block ()
    "Hide the innermost foldable block containing point."
    (interactive)
    (save-excursion
      (aaronzinhoo--hs-block-beginning)
      (hs-hide-block)))
  (defun aaronzinhoo--hs-show-block ()
    "Show the innermost folded block containing point."
    (interactive)
    (save-excursion
      (aaronzinhoo--hs-block-beginning)
      (hs-show-block))))
(use-package package
  :straight (:type built-in)
  :demand t
  :custom
  (package-enable-at-startup nil)
  (package-user-dir
    (expand-file-name
      "elpa/"
      aaronzinhoo-emacs-generated-directory))
  (package-gnupghome-dir
    (expand-file-name
      "elpa/gnupg/"
      aaronzinhoo-emacs-generated-directory)))
(use-package compat
  :straight (:type built-in)
  :demand t)
(use-package compile
  :straight (:type built-in)
  :commands (compile recompile)
  :custom
  (compilation-read-command t)
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  (compilation-scroll-output t)
  :bind (("C-c c" . compile)
          ("C-c r" . recompile)))
(use-package seq
  :straight (:type built-in)
  :demand t)
(use-package elec-pair
  :straight nil
  :hook ((org-mode
     markdown-mode
     go-mode
     go-ts-mode
     git-commit-setup)
    . aaronzinhoo--setup-electric-pairs)
  :preface
  (defvar-local aaronzinhoo--electric-pair-base-inhibit-predicate nil
    "Original Electric Pair inhibit predicate for the current buffer.")
  (defun aaronzinhoo--add-electric-pairs (&rest pairs)
    "Add PAIRS to the current buffer's Electric Pair configuration."
    (setq-local electric-pair-pairs
      (delete-dups
        (append pairs
          (copy-tree electric-pair-pairs))))

    (setq-local electric-pair-text-pairs
      (delete-dups
        (append pairs
          (copy-tree electric-pair-text-pairs)))))
  (defun aaronzinhoo--org-electric-pair-inhibit (character)
    "Prevent angle-bracket pairing in Org buffers.
Otherwise delegate to the predicate previously configured for the
current buffer."
    (or (eq character ?<)
      (and aaronzinhoo--electric-pair-base-inhibit-predicate
        (funcall
          aaronzinhoo--electric-pair-base-inhibit-predicate
          character))))
  (defun aaronzinhoo--setup-electric-pairs ()
    "Configure additional Electric Pair pairs for the current mode."
    (cond
      ;; `git-commit-mode' is a minor mode, so it cannot be tested with `derived-mode-p'.
      ((bound-and-true-p git-commit-mode)
        (aaronzinhoo--add-electric-pairs
          '(?` . ?`)))

      ;; org mode setup
      ((derived-mode-p 'org-mode)
        ;; Preserve the existing predicate while preventing <> pairing.
        (unless
          (eq electric-pair-inhibit-predicate
            #'aaronzinhoo--org-electric-pair-inhibit)
          (setq-local
            aaronzinhoo--electric-pair-base-inhibit-predicate
            electric-pair-inhibit-predicate))

        (setq-local
          electric-pair-inhibit-predicate
          #'aaronzinhoo--org-electric-pair-inhibit))

      ;; other mode setups
      ((derived-mode-p 'go-mode 'go-ts-mode 'markdown-mode)
        (aaronzinhoo--add-electric-pairs
          '(?` . ?`)))))
  :init
  (electric-pair-mode 1))
(use-package autorevert
  :straight nil
  :demand t
  :custom
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  :config
  (global-auto-revert-mode 1))
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
(use-package gcmh
  :demand t
  :custom
  (gcmh-high-cons-threshold (* 128 1024 1024))
  :config
  (gcmh-mode 1))
;;; Themeing
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
  (moe-theme-switcher-mode 1))
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
  ;; Put our directory first so grammar installation and discovery
  ;; prefer ~/.config/emacs/tree-sitter.
  (add-to-list 'treesit-extra-load-path aaronzinhoo-emacs-treesit-directory)
  (setq treesit-language-source-alist
    '((angular "https://github.com/dlvandenberg/tree-sitter-angular" "main" "src")
       (bash "https://github.com/tree-sitter/tree-sitter-bash")
       (cmake "https://github.com/uyha/tree-sitter-cmake")
       (c "https://github.com/tree-sitter/tree-sitter-c")
       (cpp "https://github.com/tree-sitter/tree-sitter-cpp" "v0.22.0")
       (css "https://github.com/tree-sitter/tree-sitter-css")
       (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
       (elisp "https://github.com/Wilfred/tree-sitter-elisp")
       (go "https://github.com/tree-sitter/tree-sitter-go" "master" "src")
       (gomod "https://github.com/camdencheek/tree-sitter-go-mod" "main" "src")
       (gosum "https://github.com/tree-sitter-grammars/tree-sitter-go-sum")
       (hcl "https://github.com/tree-sitter-grammars/tree-sitter-hcl")
       (helm "https://github.com/ngalaiko/tree-sitter-go-template" "master" "dialects/helm/src")
       (html "https://github.com/tree-sitter/tree-sitter-html" "master" "src")
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
  (dolist (mapping '((c-mode . c-ts-mode)
                      (c++-mode . c++-ts-mode)
                      (css-mode . css-ts-mode)
                      (dockerfile-mode . dockerfile-ts-mode)
                      (go-dot-mod-mode . go-mod-ts-mode)
                      (go-mode . go-ts-mode)
                      (java-mode . java-ts-mode)
                      (js-mode . js-ts-mode)
                      (json-mode . json-ts-mode)
                      (js-json-mode . json-ts-mode)
                      (python-mode . python-ts-mode)
                      (sh-mode . bash-ts-mode)
                      (sh-base-mode . bash-ts-mode)
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; garbage collector magic
(use-package gcmh
  :straight t)
(use-package gh :straight t)
(use-package async :straight t)
(use-package pcre2el :straight t)
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
;; automatically byte-compiles Emacs Lisp files (.el → .elc). The built-in compile library runs external commands such as make, linters, tests, and your bootstrap scripts in
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
;; (use-package ssh-agency
;;   :if (memq window-system '(windows)))
(use-package ssh-config-mode
  :hook ((ssh-config-mode . aaronzinhoo--ssh-config-mode-hook))
  :bind (("<backtab>" . indent-for-tab-command))
  :preface
  (defun aaronzinhoo--ssh-config-mode-hook ()
    "Configure completion in SSH configuration buffers."
    (aaronzinhoo--append-capfs
     #'ssh-config-completion-at-point
     #'cape-file
     #'cape-dabbrev)))
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
(use-package transient
  :straight t
  :demand t)
(use-package hydra
  :demand t
  :bind (("s-SPC" . hydra-nav/body)
         ("s-w" . hydra-window/body)
         ("s-o" . hydra-org/body)
         ("s-B" . hydra-bookmark/body))
  :custom
  (hydra-default-hint nil))
(use-package major-mode-hydra
  :demand t
  :after (hydra s nerd-icons)
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
      ("z" end-of-visual-line "End Line"))
     "Block"
     (("d" block-nav-previous-block "Block Up")
      ("c" block-nav-next-block "Block Down")
      ("C" block-nav-next-indentation-level "Indent Up")
      ("D" block-nav-previous-indentation-level "Indent Down"))
     "Avy"
     (("j" avy-goto-char-timer "Jump Char(s)")
      ("g" avy-goto-line "Jump Line"))
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
     (("l" bookmark-bmenu-list "List Bookmarks"))))
  (pretty-hydra-define hydra-window
    (:title "Windows and Frames"
            :color amaranth
            :quit-key "q")
    ("Select"
     (("h" windmove-left
       "left")
      ("j" windmove-down
       "down")
      ("k" windmove-up
       "up")
      ("l" windmove-right
       "right")
      ("o" other-window
       "other")
      ("a" ace-window
       "ace"
       :color blue))
     "Resize"
     (("H" (shrink-window-horizontally aaronzinhoo--window-resize-step) "shrink width")
      ("L" (enlarge-window-horizontally aaronzinhoo--window-resize-step) "grow width")
      ("J" (shrink-window aaronzinhoo--window-resize-step) "shrink height")
      ("K" (enlarge-window aaronzinhoo--window-resize-step) "grow height")
      ("=" balance-windows
       "balance")
      ("+" fit-window-to-buffer
       "fit to buffer"))
     "Create/Delete"
     (("v" split-window-right
       "split right")
      ("s" split-window-below
       "split below")
      ("d" delete-window
       "delete")
      ("1" delete-other-windows
       "delete others"))
     "Layout"
     (("t" window-layout-transpose
       "transpose")
      (">" window-layout-rotate-clockwise
       "rotate clockwise")
      ("<" window-layout-rotate-anticlockwise
       "rotate counterclockwise")
      ("|" window-layout-flip-leftright
       "flip left/right")
      ("_" window-layout-flip-topdown
       "flip top/down")
      ("2" aaronzinhoo--window-layout-rotate-180
       "rotate 180")
      ("]" rotate-windows
       "cycle buffers")
      ("[" rotate-windows-back
       "cycle buffers back"))
     "History"
     (("u" winner-undo
       "undo")
      ("r" winner-redo
       "redo"))
     "Frames"
     (("f" make-frame-command
       "new frame"
       :color blue)
      ("D" delete-frame
       "delete frame"
       :color blue)
      ("n" other-frame
       "next frame")
      ("m" toggle-frame-maximized
       "maximize"
       :color blue)
      ("F" toggle-frame-fullscreen
       "fullscreen"
       :color blue)))))
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
  :custom
  (undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  :config
  (undo-fu-session-global-mode))
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
;; depenedncy for magit
(use-package cond-let
  :straight t
  :demand t)
(use-package diff-hl
  :straight (:type git :host github :repo "dgutov/diff-hl")
  :after pretty-hydra
  :commands (diff-hl-mode
              diff-hl-next-hunk
              diff-hl-previous-hunk
              diff-hl-show-hunk
              diff-hl-stage-current-hunk
              diff-hl-revert-hunk)
  :hook ((prog-mode . diff-hl-mode)
          (dired-mode . diff-hl-dired-mode))
  :bind ("s-g" . diff-hl-hydra/body)
  :pretty-hydra
  (diff-hl-hydra
    (:hint nil
      :color pink
      :quit-key "SPC"
      :title (with-octicon
               "nf-oct-diff"
               "Diff"
               1
               -0.05))
    ("Navigate"
      (("n" diff-hl-next-hunk "Next hunk")
        ("p" diff-hl-previous-hunk "Previous hunk"))
      "Hunk"
      (("P" diff-hl-show-hunk "Show")
        ("s" diff-hl-stage-current-hunk "Stage")
        ("r" diff-hl-revert-hunk "Revert"))
      "Refresh"
      (("g" diff-hl-update "Refresh"))
      "Other"
      (("q" nil "Quit" :color blue))))
  :custom
  ;; avoid conflict with flycheck in the fringe
  (diff-hl-side 'left)
  ;; Avoid VC checks and processes on remote files.
  (diff-hl-disable-on-remote t)
  (diff-hl-ask-before-revert-hunk nil)
  :config
  ;; Refresh indicators after Magit changes repository state.
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))
(use-package git-timemachine
  :defer t
  :commands (git-timemachine))
(use-package hl-todo
  :config
  (global-hl-todo-mode 1))
(use-package smerge-mode
  :straight nil
  :after pretty-hydra
  :commands (smerge-mode
              smerge-hydra/body)
  :hook (magit-diff-visit-file . aaronzinhoo--maybe-open-smerge-hydra)
  :preface
  (defun aaronzinhoo--maybe-open-smerge-hydra ()
    "Open the Smerge Hydra when visiting a conflicted file."
    (when (bound-and-true-p smerge-mode)
      (smerge-hydra/body)))
  :pretty-hydra
  (smerge-hydra
    (:hint nil
      :color pink
      :quit-key "q"
      :post (smerge-auto-leave)
      :title (with-octicon
               "nf-oct-git_merge"
               "Resolve Conflicts"
               1
               -0.05))
    ("Navigate"
      (("n" smerge-next "Next")
        ("p" smerge-prev "Previous"))
      "Keep"
      (("b" smerge-keep-base "Base")
        ("u" smerge-keep-upper "Upper")
        ("l" smerge-keep-lower "Lower")
        ("a" smerge-keep-all "All")
        ("RET" smerge-keep-current "Current"))
      "Compare"
      (("<" smerge-diff-base-upper "Upper / base")
        ("=" smerge-diff-upper-lower "Upper / lower")
        (">" smerge-diff-base-lower "Base / lower")
        ("R" smerge-refine "Refine")
        ("E" smerge-ediff "Ediff"))
      "Resolve"
      (("C" smerge-combine-with-next "Combine next")
        ("r" smerge-resolve "Resolve")
        ("k" smerge-kill-current "Kill current"))
      "Finish"
      (("ZZ"
         (lambda ()
           (interactive)
           (save-buffer)
           (bury-buffer))
         "Save and bury"
         :color blue)
        ("q" nil "Quit" :color blue)))))
(use-package magit-todos
  :after magit
  :commands magit-todos-mode)
(use-package git-identity
  :after magit
  :bind (:map magit-status-mode-map
              ("I" . git-identity-info))
  :custom
  (git-identity-list
   '(("aaron.gonzales.ctr@linquest.com"
      :domains ("github.km.spaceforce.mil")
      :dirs ("~/development/work")
      :username "Aaron Gonzales")
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
  :commands (magit magit-status)
  :diminish
  :bind* (("M-s" . magit-status))
  :bind (:map magit-status-mode-map
         ("RET" . magit-diff-visit-file-other-window)
         ("M-i" . magit-section-backward)
         ("M-k" . magit-section-forward)
         ("M-t" . magit-todos-mode))
  :hook (git-commit-setup . aaronzinhoo--git-commit-setup)
  :preface
  (defun aaronzinhoo--delete-merged-branches ()
    "Delete local branches merged into a selected target branch."
    (interactive)
    (magit-fetch-all-prune)
    (let* ((detected-main
            (or (magit-main-branch)
                (magit-get-current-branch)))
           (target-branch
            (read-string
             "Merge target: "
             detected-main))
           (merged-branches
            (magit-git-lines
             "branch"
             "--format=%(refname:short)"
             "--merged"
             target-branch))
           ;; Never offer to delete the target or currently checked-out
           ;; branch.
           (protected-branches
            (delq nil
                  (list target-branch
                        (magit-get-current-branch))))
           (branches-to-delete
            (seq-remove
             (lambda (branch)
               (member branch protected-branches))
             merged-branches)))
      (if (null branches-to-delete)
          (message
           "No branches are merged into %s"
           target-branch)
        (when
            (yes-or-no-p
             (format
              "Delete branches merged into %s? [%s] "
              target-branch
              (mapconcat
               #'identity
               branches-to-delete
               ", ")))
          (magit-branch-delete branches-to-delete)))))
  (defun aaronzinhoo--git-commit-setup ()
    (setq-local fill-column 72)
    (aaronzinhoo--append-capfs
     #'cape-file
     #'cape-dabbrev
     #'cape-dict))
  :custom
  (magit-commit-show-diff t)
  (magit-bind-magit-project-status nil)
  ;; Do not scan every open buffer after Magit operations. This is the
  ;; important setting for preventing local Magit operations from
  ;; reconnecting unrelated TRAMP buffers.
  (auto-revert-buffer-list-filter #'magit-auto-revert-repository-buffer-p)
  :config
  (transient-append-suffix 'magit-branch "C"
    '("K" "delete all merged" aaronzinhoo--delete-merged-branches)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package matching-paren-overlay
  :straight (:type git :host codeberg :repo "acdw/matching-paren-overlay.el" :branch "main")
  :hook (prog-mode . matching-paren-overlay-mode))
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
  :custom
  (rg-executable "rg")
  :preface
  (defun aaronzinhoo--rg-select-results-window (&rest _)
    "Select the window displaying the current rg results."
    (when-let* ((buffer
                 (get-buffer
                  (rg-buffer-name)))
                (window
                 (get-buffer-window
                  buffer
                  (selected-frame))))
      (select-window window)))
  :config
  (rg-enable-default-bindings)
  (advice-add #'rg-run :after #'aaronzinhoo--rg-select-results-window))
(use-package hungry-delete
  :demand t
  :straight t
  :config
  (global-hungry-delete-mode))
(use-package dired
  :straight nil
  :commands (dired dired-jump)
  :bind (:map dired-mode-map
          ("s-h" . dired-hydra/body)
          ("<backspace>" . dired-up-directory)
          ("DEL" . dired-up-directory))
  :hook ((dired-mode . hl-line-mode)
          (dired-mode . dired-hide-details-mode))
  :custom
  ;; Suggest another visible Dired buffer as the copy/move target.
  (dired-dwim-target t)
  ;; Reuse the current Dired buffer during navigation.
  (dired-kill-when-opening-new-dired-buffer t)
  ;; Automatically refresh directory contents.
  (dired-auto-revert-buffer t)
  ;; File operations.
  (delete-by-moving-to-trash t)
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  ;; GNU ls formatting.
  (dired-listing-switches
    "-lAXGh --group-directories-first")
  :init
  ;; macOS BSD ls does not support all the listing options above.
  (when
    (eq system-type 'darwin)
    (if-let* ((gls
               (executable-find "gls")))
      (setq
        insert-directory-program gls
        dired-use-ls-dired t)
      (setq
        dired-use-ls-dired nil)
      (warn
        "GNU ls was not found; install Homebrew coreutils for full Dired support")))

  :config
  (require 'dired-aux)
  (require 'subr-x)
  ;; Sevenzip archive extraction.
  (when-let* ((sevenzip
               (or
                 (executable-find "7zz")
                 (executable-find "7z"))))
    (add-to-list
      'dired-compress-file-suffixes
      `("\\.7z\\'"
         ""
         ,(format
            "%s x -aoa -o%%o %%i"
            (shell-quote-argument sevenzip)))))
  :pretty-hydra
  (dired-hydra
    (:title "Dired"
      :color amaranth
      :quit-key "q")
    ("Navigation"
      (("n" dired-next-line
         "next")
       ("p" dired-previous-line
         "previous")
       ("RET" dired-find-file
         "open"
         :color blue)
       ("<backspace>" dired-up-directory
         "parent"
         :color blue)
       ("DEL" dired-up-directory
         "parent"
         :color blue)
       ("j" dired-goto-file
         "go to file")
       ("J" dired-jump
         "jump"
         :color blue))
     "Subtree"
      (("<tab>" dired-subtree-toggle
         "toggle")
       ("TAB" dired-subtree-toggle
         "toggle")
       ("<backtab>" dired-subtree-cycle
         "cycle")
       ("C-<tab>" dired-subtree-remove
         "remove"))
     "Mark"
      (("m" dired-mark
         "mark")
       ("u" dired-unmark
         "unmark")
       ("U" dired-unmark-all-marks
         "unmark all")
       ("t" dired-toggle-marks
         "toggle")
       ("r" dired-mark-files-regexp
         "regexp"))
     "Operate"
      (("C" dired-do-copy
         "copy")
       ("R" dired-do-rename
         "move")
       ("D" dired-do-delete
         "delete")
       ("M" dired-do-chmod
         "chmod")
       ("O" dired-do-chown
         "chown")
       ("d" dired-flag-file-deletion
         "flag")
       ("x" dired-do-flagged-delete
         "expunge")
       ("+" dired-create-directory
         "mkdir")
       ("Z" dired-do-compress
         "compress")
       ("c" dired-do-compress-to
         "compress to")
       ("!" dired-do-shell-command
         "shell")
       ("&" dired-do-async-shell-command
         "async"))
     "View/Edit"
      (("g" revert-buffer
         "refresh")
       ("h" dired-hide-details-mode
         "details")
       ("o" dired-omit-mode
         "omit")
       ("s" dired-sort-toggle-or-edit
         "sort")
       ("w" wdired-change-to-wdired-mode
         "editable"
         :color blue)
       ("i" dired-maybe-insert-subdir
         "insert directory")
       ("k" dired-kill-subdir
         "remove directory"))
     "Recursive Find"
      (("f" fd-dired
         "fd find"
         :color blue)
       ("N" find-name-dired "filename"
         :color blue)
        ("G" find-grep-dired "contents"
         :color blue)
       ("F" dired-do-find-regexp
         "marked regexp"
         :color blue)
       ("E" dired-do-find-regexp-and-replace
         "regexp replace"
         :color blue)))))
(use-package dired-x
  :straight nil
  :after dired)
(use-package dired-subtree
  :after dired
  :demand t
  :bind
  (:map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-cycle)
    ("C-<tab>" . dired-subtree-remove)))
(use-package fd-dired
  :commands (fd-dired fd-name-dired fd-grep-dired)
  :init
  (setq
    fd-dired-program
    (or
      (executable-find "fd")
      (executable-find "fdfind")
      "fd")))
(use-package dired-recent
  :demand t
  :config
  (dired-recent-mode 1))
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
(use-package ace-window
  :commands ace-window
  :bind* ("s-b" . ace-window)
  :custom
  (aw-ignore-current t)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))
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
(use-package default-text-scale
  :defer 2
  :bind (("C--" . text-scale-decrease)
         ("C-=" . text-scale-increase)))
(use-package eldoc
  :straight nil
  :diminish
  :custom
  (eldoc-idle-delay 0.3)
  (eldoc-echo-area-use-multiline-p t))
(use-package flycheck
  :straight (:type git :host github :repo "flycheck/flycheck" :branch "master")
  :diminish
  :commands flycheck-mode
  :hook ((prog-mode . flycheck-mode)
          (flycheck-mode . flycheck-annotate-mode))
  :bind ("s-f" . flycheck-hydra/body)
  :pretty-hydra
  ((:hint nil :color teal :quit-key "SPC" :title (with-codicon "nf-cod-debug" "Flycheck" 1 -0.05))
    ("Checker"
      (("?" flycheck-describe-checker "describe")
        ("d" flycheck-disable-checker "disable")
        ("m" flycheck-mode "mode")
        ("s" flycheck-select-checker "select"))
      "Errors"
      (("f" consult-flycheck "find errors (buffer)")
        ("p" flycheck-previous-error "previous" :color pink)
        ("n" flycheck-next-error "next" :color pink)
        ("l" flycheck-list-errors "list errors (buffer)")
        ("L" flycheck-projectile-list-errors "list errors (proj)"))
      "Other"
      (("r" recenter-top-bottom "recenter" :color pink)
        ("M" flycheck-manual "manual")
        ("v" flycheck-verify-setup "verify setup"))))
  :custom
  ;; Diff HL owns the fringe.
  (flycheck-indication-mode nil)

  ;; Highlight the diagnostic location.
  (flycheck-highlighting-mode 'symbols)
  (flycheck-highlighting-style 'level-face)

  ;; Show one compact inline diagnostic at point.
  (flycheck-annotate-current-line-style 'eol)
  (flycheck-annotate-other-lines-style nil)
  (flycheck-annotate-levels '(error warning))

  ;; Don’t repeat an inline message in the echo area.
  (flycheck-annotate-suppress-echo t)
  (flycheck-css-stylelint-executable "stylelint")
  (flycheck-rust-cargo-executable "cargo")
  :config
  (setq-default flycheck-disabled-checkers
    (append flycheck-disabled-checkers
      '(javascript-jshint
         c/c++-clang
         c/c++-cppcheck
         c/c++-gcc)))
  (flycheck-add-mode 'yaml-yamllint 'openapi-yaml-mode))
(use-package flycheck-aspell
  :after flycheck
  :config
  ;; If you want to check Markdown/GFM buffers
  (add-to-list 'flycheck-checkers 'markdown-aspell-dynamic)
  ;; If you want to check HTML buffers
  (add-to-list 'flycheck-checkers 'html-aspell-dynamic))
(use-package flycheck-projectile
  :commands (flycheck-projectile-list-errors))
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
  :commands (combobulate-avy-jump combobulate-python-indent-for-tab-command)
  :straight (:type git :host github :repo "mickeynp/combobulate" :branch "master")
  :config
  ;; You can customize Combobulate's key prefix here.
  ;; Note that you may have to restart Emacs for this to take effect!
  (setq combobulate-key-prefix "C-c o"))
(use-package expand-region
  :demand t
  :commands (er/expand-region)
  :bind* (("M-2" . er/expand-region)
           ("M-3" . er/mark-outside-pairs))
  :preface
  (defvar-local aaronzinhoo--treesit-extra-region-functions nil
    "Functions that provide additional semantic Tree-sitter regions.

Each function receives BEGINNING and END and returns a cons cell
`(NEW-BEGINNING . NEW-END)' or nil.")
  (defun aaronzinhoo--treesit-region-positions (beginning end)
    "Return useful parser positions for BEGINNING through END."
    (delete-dups
      (list
        beginning
        (aaronzinhoo--get-position-in-region
          beginning
          end)
        (if (> end beginning)
          (1- end)
          end))))
  (defun aaronzinhoo--region-size (bounds)
    "Return the width of BOUNDS."
    (- (cdr bounds)
      (car bounds)))
  (defun aaronzinhoo--treesit-structural-node-p
    (parser node)
    "Return non-nil when NODE is structurally useful for expansion.

Ignore Helm `text' nodes because they merely represent uninterpreted
YAML text between Helm actions. YAML structure is provided by the
YAML parser, while Helm action boundaries are provided separately by
the registered semantic-region functions."
    (not
      (and
        (memq
          (treesit-parser-language parser)
          '(helm go-template))
        (string-equal
          (treesit-node-type node)
          "text"))))
  (defun aaronzinhoo--strictly-larger-region-p
    (bounds beginning end)
    "Return non-nil when BOUNDS strictly contains BEGINNING through END."
    (and bounds
      (<= (car bounds) beginning)
      (>= (cdr bounds) end)
      (or
        (< (car bounds) beginning)
        (> (cdr bounds) end))))
  (defun aaronzinhoo--treesit-node-contains-region-p (node beginning end)
    "Return non-nil when NODE contains BEGINNING through END."
    (and node
      (<= (treesit-node-start node)
        beginning)
      (>= (treesit-node-end node)
        end)))
  (defun aaronzinhoo--treesit-node-matches-region-p (node beginning end)
    "Return non-nil when NODE exactly matches BEGINNING and END."
    (and node
      (= (treesit-node-start node)
        beginning)
      (= (treesit-node-end node)
        end)))

  (defun aaronzinhoo--get-position-in-region (beginning end)
    "Return a position inside BEGINNING through END. If END and BEGINNING are equal then return BEGINNING"
    (if (> end beginning)
      (+ beginning
        (/ (- end beginning) 2))
      beginning))

  (defun aaronzinhoo--treesit-next-bigger-node (parser beginning end)
    "Return PARSER's smallest non-root node larger than the region.

Inspect the beginning, middle, and end of the region so injected
languages do not prevent discovery of a containing host-language node."
    (let ((root
            (treesit-parser-root-node parser))
           candidates)

      (dolist (position
                (aaronzinhoo--treesit-region-positions
                  beginning
                  end))
        (when-let* ((node
                     (ignore-errors
                       (treesit-node-at
                         position
                         parser))))

          ;; Find an ancestor containing the complete selected region.
          (while
            (and node
              (not
                (aaronzinhoo--treesit-node-contains-region-p
                  node
                  beginning
                  end)))
            (setq node
              (treesit-node-parent node)))

          ;; Skip all wrapper nodes having the same textual bounds.
          (while
            (and node
              (aaronzinhoo--treesit-node-matches-region-p
                node
                beginning
                end))
            (setq node
              (treesit-node-parent node)))

          (when
            (and node
              (not
                (treesit-node-eq node root))
              (aaronzinhoo--treesit-node-contains-region-p
                node
                beginning
                end)
              (or
                (< (treesit-node-start node)
                  beginning)
                (> (treesit-node-end node)
                  end)))
            (push node candidates))))

      ;; A parser may produce the same node from several sampled positions.
      (setq candidates
        (delete-dups candidates))

      (car
        (sort
          candidates
          (lambda (left right)
            (let ((left-size
                    (aaronzinhoo--treesit-node-size left))
                   (right-size
                     (aaronzinhoo--treesit-node-size right)))
              (if (= left-size right-size)
                (>
                  (aaronzinhoo--treesit-node-depth left)
                  (aaronzinhoo--treesit-node-depth right))
                (< left-size right-size))))))))
  (defun aaronzinhoo--treesit-node-size (node)
    "Return the buffer width of NODE."
    (- (treesit-node-end node)
      (treesit-node-start node)))

  (defun aaronzinhoo--treesit-node-depth (node)
    "Return NODE's depth in its syntax tree."
    (let ((depth 0))
      (while
        (setq node
          (treesit-node-parent node))
        (setq depth
          (1+ depth)))
      depth))

  (defun aaronzinhoo--treesit-smallest-bigger-bounds
    (beginning end)
    "Return the smallest larger syntactic or semantic region."
    (let (candidates)

      ;; Ordinary structural parent-node candidates.
      (dolist (parser
                (treesit-parser-list))
        (when-let* ((node
                     (aaronzinhoo--treesit-next-bigger-node
                       parser
                       beginning
                       end)))
          (when
            (aaronzinhoo--treesit-structural-node-p
              parser
              node)
            (let ((bounds
                    (cons
                      (treesit-node-start node)
                      (treesit-node-end node))))
              (when
                (aaronzinhoo--strictly-larger-region-p
                  bounds
                  beginning
                  end)
                (push bounds candidates))))))

      ;; Helm action, containing-line, and indentation-parent regions.
      (dolist (function
                aaronzinhoo--treesit-extra-region-functions)
        (when-let* ((bounds
                     (funcall
                       function
                       beginning
                       end)))
          (when
            (aaronzinhoo--strictly-larger-region-p
              bounds
              beginning
              end)
            (push bounds candidates))))

      (car
        (sort
          (delete-dups candidates)
          (lambda (left right)
            (<
              (aaronzinhoo--region-size left)
              (aaronzinhoo--region-size right)))))))

  (defun aaronzinhoo--treesit-mark-bigger-node ()
    "Expand to the smallest larger syntactic or semantic region."
    (interactive)
    (let* ((beginning
             (if (region-active-p)
               (region-beginning)
               (point)))
            (end
              (if (region-active-p)
                (region-end)
                (point)))
            (bounds
              (aaronzinhoo--treesit-smallest-bigger-bounds
                beginning
                end)))
      (when bounds
        (goto-char
          (car bounds))
        (set-mark
          (cdr bounds))
        (activate-mark))))
  (defun aaronzinhoo--mark-assignment-list-item ()
    "Mark the comma-separated assignment item containing point or region.

For example, given:

  NAME=FIRST:value,SECOND:value

mark either `FIRST:value' or `SECOND:value'."
    (interactive)
    (let* ((current-beginning
             (if (region-active-p)
               (region-beginning)
               (point)))
            (current-end
              (if (region-active-p)
                (region-end)
                (point)))
            ;; Use the middle of the active region so expansion stays with the
            ;; currently selected item instead of moving to the next item.
            (position
              (aaronzinhoo--get-position-in-region
                current-beginning
                current-end))
            (line-beginning
              (save-excursion
                (goto-char position)
                (line-beginning-position)))
            (line-end
              (save-excursion
                (goto-char position)
                (line-end-position)))
            (assignment
              (save-excursion
                (goto-char line-beginning)
                (search-forward "=" line-end t))))

      ;; Only recognize comma-separated items on the right side of `='.
      (when (and assignment
              (>= position assignment))
        (let ((beginning
                (save-excursion
                  (goto-char position)
                  (if (search-backward "," assignment t)
                    (1+ (point))
                    assignment)))
               (end
                 (save-excursion
                   (goto-char position)
                   (if (search-forward "," line-end t)
                     (1- (point))
                     line-end))))

          ;; Exclude whitespace surrounding the item.
          (save-excursion
            (goto-char beginning)
            (skip-chars-forward " \t" end)
            (setq beginning
              (point)))

          (save-excursion
            (goto-char end)
            (skip-chars-backward " \t" beginning)
            (setq end
              (point)))

          ;; Only return a region larger than the current selection.
          (when (and
                  (<= beginning current-beginning)
                  (>= end current-end)
                  (or (< beginning current-beginning)
                    (> end current-end)))
            (goto-char beginning)
            (set-mark end)
            (activate-mark))))))
  (defun aaronzinhoo--mark-assignment-list ()
    "Mark the complete comma-separated assignment value.

For example, given:

  NAME=FIRST:value,SECOND:value

mark:

  FIRST:value,SECOND:value"
    (interactive)
    (let* ((current-beginning
             (if (region-active-p)
               (region-beginning)
               (point)))
            (current-end
              (if (region-active-p)
                (region-end)
                (point)))
            (position
              (aaronzinhoo--get-position-in-region
                current-beginning
                current-end))
            (line-beginning
              (save-excursion
                (goto-char position)
                (line-beginning-position)))
            (line-end
              (save-excursion
                (goto-char position)
                (line-end-position)))
            (assignment
              (save-excursion
                (goto-char line-beginning)
                (search-forward "=" line-end t))))

      (when (and assignment
              (>= position assignment))
        (let ((beginning assignment)
               (end line-end))

          ;; Exclude whitespace immediately after `='.
          (save-excursion
            (goto-char beginning)
            (skip-chars-forward " \t" end)
            (setq beginning
              (point)))

          ;; Exclude trailing whitespace.
          (save-excursion
            (goto-char end)
            (skip-chars-backward " \t" beginning)
            (setq end
              (point)))

          ;; Only offer a region that contains and expands the current one.
          (when (and
                  (<= beginning current-beginning)
                  (>= end current-end)
                  (or (< beginning current-beginning)
                    (> end current-end)))
            (goto-char beginning)
            (set-mark end)
            (activate-mark))))))
  (defun aaronzinhoo--add-shell-ts-mode-expansions ()
    "Add shell assignment and Tree-sitter expansions."
    (setq-local
      er/try-expand-list
      '(er/mark-word
         er/mark-symbol
         er/mark-outside-quotes
         aaronzinhoo--mark-assignment-list-item
         aaronzinhoo--mark-assignment-list
         aaronzinhoo--treesit-mark-bigger-node)))
  (defun aaronzinhoo--add-yaml-ts-mode-expansions ()
    "Add YAML, assignment-list, and Tree-sitter expansions."
    (setq-local
      er/try-expand-list
      '(er/mark-word
         er/mark-symbol
         ;; Comma-separated assignment structure.
         aaronzinhoo--mark-assignment-list-item
         aaronzinhoo--mark-assignment-list
         ;; Quoted scalar values.
         er/mark-outside-quotes
         ;; Generic Tree-sitter structure.
         aaronzinhoo--treesit-mark-bigger-node)))
  (defun aaronzinhoo--add-helm-ts-mode-expansions ()
    "Add Helm, YAML, and Tree-sitter expansions."
    (setq-local
      er/try-expand-list
      '(er/mark-word
         er/mark-symbol

         aaronzinhoo--mark-assignment-list-item
         aaronzinhoo--mark-assignment-list

         ;; Bridge from Helm into surrounding YAML.
         er/mark-outside-quotes

         ;; Ordinary Tree-sitter nodes plus the registered Helm semantic
         ;; regions: action, containing line, and indentation parent.
         aaronzinhoo--treesit-mark-bigger-node)))
  (defun aaronzinhoo--add-treesit-mode-expansions ()
    "Add generic Tree-sitter expansions."
    (setq-local
      er/try-expand-list
      '(er/mark-word
         er/mark-symbol
         aaronzinhoo--treesit-mark-bigger-node)))
  :config
  (er/enable-mode-expansions
    'helm-ts-mode
    #'aaronzinhoo--add-helm-ts-mode-expansions)
  (dolist (mode
            '(bash-ts-mode
               sh-mode))
    (er/enable-mode-expansions
      mode
      #'aaronzinhoo--add-shell-ts-mode-expansions))
  (dolist (mode
            '(yaml-ts-mode
               openapi-ts-mode))
    (er/enable-mode-expansions
      mode
      #'aaronzinhoo--add-yaml-ts-mode-expansions))
  (dolist (mode
            '(c-ts-mode
               c++-ts-mode
               css-ts-mode
               dockerfile-ts-mode
               go-ts-mode
               html-ts-mode
               java-ts-mode
               js-ts-mode
               json-ts-mode
               nxml-mode
               python-ts-mode
               rust-ts-mode
               terraform-mode
               toml-ts-mode
               tsx-ts-mode
               typescript-ts-mode))
    (er/enable-mode-expansions
      mode
      #'aaronzinhoo--add-treesit-mode-expansions))
  )
(use-package yasnippet
  :straight t
  :hook (prog-mode . yas-minor-mode)
  :bind (;; Ordinary Yasnippet mode.
         :map yas-minor-mode-map
         ;; Do not let Yasnippet capture Tab during normal editing.
         ("TAB" . nil)
         ("<tab>" . nil)
         ;; Choose a snippet through minibuffer completion.
         ("C-c C-y" . yas-insert-snippet)
         ;; Active snippet fields.
         :map yas-keymap
         ("TAB" . yas-next-field)
         ("<tab>" . yas-next-field)
         ("S-TAB" . yas-prev-field)
         ("<backtab>" . yas-prev-field)
         ;; Finish snippet editing and keep the expanded text.
         ("C-c C-e" . yas-exit-all-snippets)
         ;; Stop the active snippet session.
         ("C-g" . yas-abort-snippet))
  :custom
  (yas-choose-keys-first nil)
  (yas-prompt-functions '(yas-completing-prompt)))
(use-package yasnippet-snippets
  :straight t
  :after yasnippet)
;;; LSP
(use-package dap-mode
  :after (lsp-mode)
  :straight (:type git :host github :repo "emacs-lsp/dap-mode" :branch "master")
  :hook ((lsp-mode . dap-auto-configure-mode)
         ;; dap-stopped called after breakpoint hit
         (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra))))
  :custom
  (dap-python-debugger 'debugpy)
  :config
  (dap-ui-controls-mode nil)
  (dap-ui-mode nil)
  (dap-tooltip-mode nil)
  (require 'dap-python)
  (require 'dap-dlv-go)
  (require 'dap-lldb)
  (require 'dap-gdb-lldb))
(use-package lsp-mode
  ;; :straight (:type git :host github :repo "emacs-lsp/lsp-mode" :branch "master")
  :commands (lsp lsp-deferred)
  :hook
  ((c-ts-mode           . lsp-deferred)
    (c++-ts-mode         . lsp-deferred)
    (css-ts-mode         . aaronzinhoo--web-lsp-setup)
    (dockerfile-ts-mode  . lsp-deferred)
    (go-ts-mode          . lsp-deferred)
    (html-ts-mode        . aaronzinhoo--web-lsp-setup)
    (js-ts-mode          . aaronzinhoo--web-lsp-setup)
    (json-ts-mode        . lsp-deferred)
    (rust-ts-mode        . lsp-deferred)
    (sql-mode            . lsp-deferred)
    (tsx-ts-mode         . aaronzinhoo--web-lsp-setup)
    (typescript-ts-mode  . aaronzinhoo--web-lsp-setup)
    (lsp-mode            . lsp-enable-which-key-integration)
    (lsp-managed-mode    . aaronzinhoo--flycheck-add-lsp-chains)
    (lsp-completion-mode . aaronzinhoo--lsp-completion-setup))
  :bind (:map lsp-mode-map
          ("s-l" . lsp-hydra/body)
          ([remap xref-find-apropos] . consult-lsp-symbols))
  :pretty-hydra
  (lsp-hydra
    (:hint nil :color pink :quit-key "SPC" :title (with-octicon "nf-oct-rocket" "LSP" 1 -0.05))
    ("Goto"
      (("r" lsp-find-references "Refs")
        ("d" lsp-find-definition "Defs")
        ("i" lsp-goto-implementation "Implementation (interface)")
        ("t" lsp-find-type-definition "Type-def")
        ("D" consult-lsp-diagnostics "Diagnostics")
        ("s" consult-lsp-file-symbols "File Symbols")
        ("S" consult-lsp-symbols "Workspace Symbols")
        ("b" xref-pop-marker-stack "Pop back" :color red))
      "Refactor"
      (("f" lsp-format-buffer "Format")
        ("n" lsp-rename "Rename")
        ("o" lsp-organize-imports "Organize imports")
        ("c" lsp-code-actions-at-point "List code actions"))
      "UI"
      (("up" lsp-ui-peek-mode "Peek-mode")
        ("ur" lsp-ui-peek-find-references "Peek-refs" :color red)
        ("ud" lsp-ui-peek-find-definitions "Peek-defs" :color red)
        ("um" lsp-ui-imenu "Peek-menu"))
      "Lsp Server"
      (("LS" lsp-describe-session "Session")
        ("LI" lsp-install-server "Install")
        ("LR" lsp-workspace-restart "Restart"))))
  :preface
  (defvar aaronzinhoo--lsp-capf-backends
    (list
      #'lsp-completion-at-point
      #'cape-file)
    "Completion-at-point functions used in LSP-managed buffers.")
  (defun aaronzinhoo--flycheck-add-lsp-chains ()
    "Add secondary checkers after LSP creates its Flycheck checker."
    (when (and lsp-managed-mode
            (flycheck-valid-checker-p 'lsp)
            (not (get 'lsp 'aaronzinhoo-chains-added)))
      (flycheck-add-next-checker
        'lsp
        '(t . yaml-yamllint))
      (put 'lsp 'aaronzinhoo-chains-added t)))
  (defun aaronzinhoo--lsp-booster-json-parse (old-function &rest args)
    "Parse LSP Booster bytecode, or call OLD-FUNCTION with ARGS."
    (or
      (when (eq (following-char) ?#)
        (let ((bytecode (read (current-buffer))))
          (when (byte-code-function-p bytecode)
            (funcall bytecode))))
      (apply old-function args)))

  (defun aaronzinhoo--lsp-booster-final-command
    (old-function command &optional test?)
    "Wrap LSP COMMAND with emacs-lsp-booster when appropriate."
    (let ((resolved-command
            (funcall old-function command test?)))
      (if (and (not test?)
            (not (file-remote-p default-directory))
            lsp-use-plists
            (not (functionp 'json-rpc-connection))
            (executable-find "emacs-lsp-booster"))
        (progn
          (when-let* ((executable
                       (executable-find (car resolved-command))))
            (setcar resolved-command executable))
          (message "Using emacs-lsp-booster for %S"
            resolved-command)
          (cons "emacs-lsp-booster" resolved-command))
        resolved-command)))
  (defun aaronzinhoo--lsp-completion-setup ()
    "Configure LSP completion for Corfu, Cape, and Orderless."
    (let ((existing
            (remove #'lsp-completion-at-point
              completion-at-point-functions)))

      (setq-local completion-at-point-functions
        (delete-dups
          (append
            (copy-sequence
              aaronzinhoo--lsp-capf-backends)
            existing))))

    (setq-local completion-category-overrides
      (copy-tree completion-category-overrides))

    (setf (alist-get 'lsp-capf
            completion-category-overrides)
      '((styles orderless))))  ;; Configure orderless which can use flex
  (defun aaronzinhoo--angular-project-root ()
    "Return the nearest Angular workspace root."
    (when-let* ((file (or buffer-file-name default-directory)))
      (locate-dominating-file file "angular.json")))
  (defun aaronzinhoo--activate-project-node ()
    "Activate the project's NVM version and local executables."
    (when-let* ((file (or buffer-file-name default-directory))
                 (nvm-root
                   (locate-dominating-file file ".nvmrc")))
      (nvm-use-for nvm-root))

    (when-let* ((file (or buffer-file-name default-directory))
                 (package-root
                   (locate-dominating-file file "package.json"))
                 (bin-directory
                   (expand-file-name "node_modules/.bin"
                     package-root))
                 ((file-directory-p bin-directory)))
      (setq-local exec-path
        (cons bin-directory
          (delete bin-directory
            (copy-sequence exec-path))))
      (setq-local process-environment
        (copy-sequence process-environment))
      (setenv
        "PATH"
        (concat bin-directory
          path-separator
          (or (getenv "PATH") "")))))
  (defun aaronzinhoo--configure-angular-language-server ()
  "Configure the project-local Angular language server."
  (when-let* ((root (aaronzinhoo--angular-project-root))
              (node-modules
               (expand-file-name "node_modules" root))
              (ngserver
               (expand-file-name
                "node_modules/.bin/ngserver"
                root))
              ((file-executable-p ngserver)))
    (setq-local
     lsp-clients-angular-language-server-command
     (list ngserver
           "--stdio"
           "--tsProbeLocations" node-modules
           "--ngProbeLocations" node-modules))))
  (defun aaronzinhoo--web-lsp-setup ()
    "Prepare the Node environment, then start LSP."
    (aaronzinhoo--activate-project-node)
    (aaronzinhoo--configure-angular-language-server)
    (lsp-deferred))
  :custom
  ;; Startup and workspace
  (lsp-auto-configure t)
  (lsp-auto-guess-root t)
  (lsp-log-io nil)
  ;; Completion
  (lsp-completion-enable t)
  (lsp-completion-provider :none) ; Corfu consumes the CAPFs
  (lsp-enable-snippet t)
  ;; Diagnostics
  (lsp-diagnostics-provider :flycheck)
  (lsp-modeline-diagnostics-enable nil)
  ;; Eldoc and signatures
  (lsp-eldoc-enable-hover t)
  (lsp-eldoc-render-all nil)
  (lsp-signature-doc-lines 1)
  (lsp-signature-auto-activate nil)
  ;; Formatting and indentation
  (lsp-enable-indentation nil)
  (lsp-enable-on-type-formatting nil)
  ;; Highlighting and visual features
  (lsp-enable-semantic-highlighting nil)
  (lsp-semantic-tokens-enable nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-enable-text-document-color nil)
  (lsp-enable-folding nil)
  (lsp-lens-enable nil)
  (lsp-headerline-breadcrumb-enable nil)
  ;; Navigation and links
  (lsp-enable-xref t)
  (lsp-enable-links nil)
  ;; Modeline
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-workspace-status-enable nil)
  ;; Keymap
  (lsp-keymap-prefix nil)
  ;; client settings
  (lsp-disabled-clients '(ccls))
  ;; Rust Analyzer
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  :init
  (setq lsp-use-plists t)
  :config
  ;; terraform setup
  (add-to-list
   'lsp-language-id-configuration
   '(terraform-mode . "opentofu"))

  ;; Variable files use a separate language ID.
  (add-to-list
   'lsp-language-id-configuration
   '("\\.tfvars\\'" . "opentofu-vars"))

  (lsp-register-client
   (make-lsp-client
    :new-connection
    (lsp-stdio-connection
     '("tofu-ls" "serve"))

    :activation-fn
    (lsp-activate-on
     "opentofu"
     "opentofu-vars")
    ;; Prefer tofu-ls if another Terraform client also matches.
    :priority 1
    :server-id 'tofu-ls))
  ;; Install the JSON-parser advice after the JSON implementation is known.
  (require 'json)
  (let ((json-parser
          (if (fboundp 'json-parse-buffer)
            'json-parse-buffer
            'json-read)))
    (unless
      (advice-member-p
        #'aaronzinhoo--lsp-booster-json-parse
        json-parser)
      (advice-add
        json-parser
        :around
        #'aaronzinhoo--lsp-booster-json-parse)))
  ;; lsp-resolve-final-command exists now because :config runs after loading.
  (unless
    (advice-member-p
      #'aaronzinhoo--lsp-booster-final-command
      'lsp-resolve-final-command)
    (advice-add
      'lsp-resolve-final-command
      :around
      #'aaronzinhoo--lsp-booster-final-command))
  ;; setup additinal ignore directories for lsp
  (add-to-list
    'lsp-file-watch-ignored-directories
    (regexp-quote (expand-file-name "~/.config/pyenv")))
  (dolist (directory
            '("[/\\\\]\\.venv\\'"
               "[/\\\\]\\.direnv\\'"
               "[/\\\\]\\.terraform\\'"
               "[/\\\\]\\.terragrunt-cache\\'"
               "[/\\\\]node_modules\\'"
               "[/\\\\]dist\\'"
               "[/\\\\]coverage\\'"))
    (add-to-list 'lsp-file-watch-ignored-directories directory)))
(use-package lsp-yaml
  :straight nil
  :after lsp-mode
  :custom
  (lsp-yaml-schemas
   '((https://raw.githubusercontent.com/compose-spec/compose-spec/master/schema/compose-spec.json
      . ["compose.yaml"
         "compose.yml"
         "docker-compose.yaml"
         "docker-compose.yml"
         "docker-compose*.yaml"
         "docker-compose*.yml"])

     (https://json.schemastore.org/kustomization.json
      . ["kustomization.yaml"
         "kustomization.yml"])

     (https://spec.openapis.org/oas/3.1/schema/2022-10-07
      . ["*openapi.yaml"
         "*openapi.yml"])

     (file:///Users/agonzales/development/work/kahless/backend/kafka-provisioner/schema.json
      . ["/Users/agonzales/development/work/kahless/backend/kafka-provisioner/tests/scripts/*"])

     (kubernetes
      . ["*-k8s.yaml"
         "*-k8s.yml"
         "k8s/**/*.yaml"
         "k8s/**/*.yml"
         "manifests/**/*.yaml"
         "manifests/**/*.yml"])))
  ;; Temporary workaround for the upstream schema version.
  (lsp-yaml--built-in-kubernetes-schema
   '((name . "Kubernetes")
     (description
      . "Built-in Kubernetes manifest schema definition")
     (url
      . "https://raw.githubusercontent.com/yannh/kubernetes-json-schema/master/v1.35.2-standalone-strict/all.json")
     (fileMatch . ["*-k8s.yaml" "*-k8s.yml"])))
  ;; fixed upstream but cannot pull in upstream fix due lsp having issue in emacs 30
  (lsp-yaml--built-in-kubernetes-schema
    '((name . "Kubernetes")
       (description . "Built-in kubernetes manifest schema definition")
       (url . "https://raw.githubusercontent.com/yannh/kubernetes-json-schema/master/v1.35.2-standalone-strict/all.json")
       (fileMatch . ["*-k8s.yaml" "*-k8s.yml"]))))
(use-package lsp-treemacs
  :defer t
  :commands (lsp-treemacs-errors-list)
  :custom
  (lsp-treemacs-sync-mode t))
(use-package lsp-ui
  :after lsp-mode
  :commands
  (lsp-ui-mode
    lsp-ui-peek-find-definitions
    lsp-ui-peek-find-references)
  :hook
  (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
          ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
          ([remap xref-find-references] . lsp-ui-peek-find-references))
  :custom
  ;; Flycheck Annotate handles diagnostic messages.
  (lsp-ui-sideline-enable nil)
  ;; Keep Peek for definitions and references.
  (lsp-ui-peek-enable t)
  ;; Enable documentation popups.
  (lsp-ui-doc-enable nil)
  ;; Use the standard child-frame renderer.
  (lsp-ui-doc-use-webkit nil))
(use-package lsp-java
  :straight (:type git :host github :repo "emacs-lsp/lsp-java" :branch "master")
  :hook ((java-ts-mode . lsp-deferred)
          (java-ts-mode . lsp-java-boot-lens-mode)
          (java-ts-mode . aaronzinhoo--lsp-java-setup))
  :preface
  (defun aaronzinhoo--lsp-java-setup ()
    "Configure LSP and enable lenses in Java buffers."
    (when (derived-mode-p 'java-ts-mode)
      (setq-local lsp-lens-enable t)
      (lsp-lens-mode 1)))
    (defun aaronzinhoo--lsp-java-vmargs ()
    "Return JVM arguments used to start the Java language server."
    (let ((lombok-file
           (expand-file-name
            "deps/lombok.jar"
            user-emacs-directory)))
      (append
       '("-XX:+UseParallelGC"
         "-XX:GCTimeRatio=4"
         "-XX:AdaptiveSizePolicyWeight=90"
         "-Dsun.zip.disableMemoryMapping=true"
         "-Xmx2G"
         "-Xms100m")
       (when (file-readable-p lombok-file)
         (list (concat "-javaagent:" lombok-file))))))
  :init
  (setq lsp-java-vmargs (aaronzinhoo--lsp-java-vmargs))
  :config
  (require 'lsp-java-boot))
(use-package lsp-pyright
  :straight (:type git :host github :repo "emacs-lsp/lsp-pyright" :branch "master")
  :after lsp-mode
  :custom
  (lsp-pyright-langserver-command "basedpyright")
  ;; Let Ruff own import organization.
  (lsp-pyright-disable-organize-imports t)
  ;; let pyright handle import completion for missing imports
  (lsp-pyright-auto-import-completions t)
  (lsp-pyright-type-checking-mode "standard")
  ;; Use "openFilesOnly" if workspace-wide diagnostics become noisy or
  ;; expensive on large projects.
  (lsp-pyright-diagnostic-mode "workspace")
  ;; Resolve this after your project environment has been activated.
  (lsp-pyright-python-executable-cmd "python")
  ;; Register Pyright as a separate server per project.
  (lsp-pyright-multi-root nil)
  (lsp-pyright-diagnostic-mode "openFilesOnly"))
(use-package lsp-ruff
  :straight nil
  :after lsp-mode
  :custom
  (lsp-ruff-server-command '("ruff" "server"))
  (lsp-ruff-advertize-organize-imports t)
  (lsp-ruff-advertize-fix-all t)
  (lsp-ruff-lint-enable t)
  (lsp-ruff-log-level 'error)
  (lsp-ruff-show-notifications 'onError))
;;; Debugger Support
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

;; icons!!
(use-package nerd-icons
  :straight t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))
(use-package nerd-icons-corfu
  :straight t
  :demand t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
(use-package nerd-icons-dired
  :straight t
  :hook (dired-mode . nerd-icons-dired-mode))
(use-package nerd-icons-ibuffer
  :straight t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))
(use-package nerd-icons-completion
  :straight t
  :after marginalia
  :config
  (nerd-icons-completion-mode 1)
  :hook (marginalia-mode
          . nerd-icons-completion-marginalia-setup))
(use-package treemacs-nerd-icons
  :straight t
  :after treemacs
  :config
  (treemacs-nerd-icons-config))

;;; Minibuffer Compleitions
(use-package marginalia
  :after (vertico)
  :bind (:map vertico-map
              ("M-A" . marginalia-cycle))
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode 1))
(use-package orderless
  :demand t
  :ensure t
  :custom
  (orderless-matching-styles
    '(orderless-literal
       orderless-prefixes
       orderless-initialism
       orderless-regexp
       ;; orderless-flex                       ; Basically fuzzy finding. Works by adding ~ in front of search
       ;; orderless-strict-leading-initialism
       ;; orderless-strict-initialism
       ;; orderless-strict-full-initialism
       ;; orderless-without-literal          ; Recommended for dispatches instead
       ))
  :config
  ;; Define orderless style with initialism by default
  (setq completion-styles '(orderless))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles orderless))
                                   (command (styles orderless))
                                   (symbol (styles orderless))
                                   (variable (styles orderless)))))
(use-package consult-dir
  :straight (consult-dir :type git :host github :repo "karthink/consult-dir" :branch "master")
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :preface
  (defcustom consult-dir--tramp-container-executable "docker"
    "Default executable to use for querying container hosts."
    :group 'consult-dir
    :type 'string)

  (defcustom consult-dir--tramp-container-args nil
    "Optional list of arguments to pass when querying container hosts."
    :group 'consult-dir
    :type '(repeat string))
  (defvar aaronzinhoo--consult-dir-source-docker
    `(:name "Docker"
       :narrow ?d
       :category file
       :face consult-file
       :history file-name-history
       :items ,#'aaronzinhoo--consult-dir-docker-hosts)
    "Consult-dir source for running Docker containers.")
  (defun aaronzinhoo--consult-dir-docker-hosts ()
    "Return running Docker containers as TRAMP paths."
    (when-let* ((docker
                 (executable-find
                   consult-dir--tramp-container-executable)))
      (mapcar
        (lambda (container)
          (format "/docker:%s:/" container))
        (ignore-errors
          (apply #'process-lines
            docker
            (append
              consult-dir--tramp-container-args
              '("ps" "--format" "{{.Names}}")))))))
  :custom
  (consult-dir-project-list-function #'consult-dir-projectile-dirs)
  :config
  ;; Adding to the list of consult-dir sources
  (add-to-list 'consult-dir-sources 'aaronzinhoo--consult-dir-source-docker t)
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t))
(use-package consult-flycheck
  :after (consult)
  :straight (consult-flycheck :type git :host github :repo "minad/consult-flycheck" :branch "main"))
(use-package consult-lsp
  :after (consult)
  :commands (consult-lsp-diagnostics consult-lsp-symbols consult-lsp-file-symbols)
  :straight (:type git :host github :repo "gagbo/consult-lsp" :branch "main"))
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
          ("C-x r" . consult-recent-file)            ;; orig. bookmark-jump
          ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
          ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
          ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
          ;; Custom M-# bindings for fast register access
          ("M-#" . consult-register-load)
          ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
          ("C-M-#" . consult-register)
          ;; Other custom bindings
          ("M-y" . consult-yank-pop)                ;; orig. yank-pop
          ;; M-g bindings in `goto-map'
          ("M-g e" . consult-compile-error)
          ("M-g g" . consult-goto-line)             ;; orig. goto-line
          ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
          ("M-g m" . consult-mark)
          ("M-g f" . consult-fd)
          ("M-g M" . consult-global-mark)
          ("M-g i" . consult-imenu)
          ("M-g I" . consult-imenu-multi)
          ;; Search
          ("C-s" . aaronzinhoo--buffer-search)
          ;; M-s bindings in `search-map'
          ;; these should be using S not s, probably want better mapping before turning them on again
          ;; ("s-s p" . consult-ripgrep-thing-at-point)
          ;; ("M-s D" . consult-locate)
          ;; ("M-s g" . consult-grep)
          ;; ("M-s G" . consult-git-grep)
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
  (defcustom aaronzinhoo--consult-line-size-limit 1000000
    "Use Isearch instead of `consult-line' above this buffer size. The value is measured in buffer characters, not file bytes."
    :type 'integer
    :group 'consult)
  (defun aaronzinhoo--buffer-search ()
    "Search the current buffer or file using Consult."
    (interactive)
    (let ((initial
            (when (use-region-p)
              (buffer-substring-no-properties
                (region-beginning)
                (region-end)))))
      (deactivate-mark)
      (cond
        ;; `consult-line' includes unsaved buffer changes.
        ((<= (buffer-size) aaronzinhoo--consult-line-size-limit)
          (consult-line initial))

        ;; Ripgrep is fast, but searches the saved file on disk.
        ((and buffer-file-name
           (not (buffer-modified-p))
           (not (file-remote-p buffer-file-name)))
          (consult-ripgrep (list buffer-file-name) initial))

        ;; Unsaved, remote, and non-file buffers.
        (t
          (isearch-forward)
          (when initial
            (isearch-yank-string initial))))))
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
    consult-source-bookmark consult-source-file-register
    consult-source-recent-file consult-source-project-recent-file
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
          ("TAB" . vertico-insert)
          ([tab] . vertico-insert)
          ;; NOTE 2022-02-05: Cycle through candidate groups
          ("C-M-p" . vertico-previous-group)
          ("C-M-n" . vertico-next-group)
          ;; Multiform toggles
          ("<backspace>" . vertico-directory-delete-char)
          ("M-<backspace>" . vertico-directory-delete-word)
          ("RET" . vertico-directory-enter)
          ("C-'" . vertico-quick-exit)
          ("C-i" . vertico-quick-insert)
          ("M-S" . vertico-save)
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
  (defun aaronzinhoo--vertico-highlight-enabled-mode (command)
    "Highlight COMMAND when its corresponding mode is enabled."
    (let ((symbol (intern-soft command)))
      (if (and symbol
            (or (eq symbol major-mode)
              (eq symbol aaronzinhoo--last-major-mode)
              (and (boundp symbol)
                (symbol-value symbol))))
        (propertize command
          'face 'font-lock-constant-face)
        command)))
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
  (vertico-cycle t)
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
       (execute-extended-command reverse
         (+vertico-transform-functions . aaronzinhoo--vertico-highlight-enabled-mode))
       ))
  :init
  ;; variable to enable highlighting major mode in minibuffer
  (setq aaronzinhoo--last-major-mode nil)
  (vertico-mode)
  (vertico-multiform-mode))
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
(use-package cape
  :demand t
  :custom
  (cape-dabbrev-min-length 2))
(use-package corfu
  :straight (corfu :type git :host github :repo "minad/corfu" :files (:defaults "extensions/*"))
  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
          ("M-SPC"      . corfu-insert-separator)
          ("TAB"        . aaronzinhoo--corfu-complete-common-or-next)
          ([tab]        . aaronzinhoo--corfu-complete-common-or-next)
          ("S-TAB"      . corfu-previous)
          ([backtab]    . corfu-previous))
  :hook ((eshell-mode . aaronzinhoo--corfu-eshell-setup))
  ;; Optional customizations
  :custom
    ;; Candidate window
  (corfu-min-width 80)
  (corfu-max-width 80)
  (corfu-count 15)
  (corfu-scroll-margin 4)
  (corfu-cycle t)
  ;; Automatic completion
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1)
  ;; Candidate selection
  (corfu-preview-current t)
  (corfu-preselect 'valid)
  (corfu-on-exact-match nil)
  ;; Boundary handling
  ;; `nil' means to ignore `corfu-separator' behavior, that is, use the older
  ;; `corfu-quit-at-boundary' = nil behavior. Set this to separator if using
  ;; `corfu-auto' = `t' workflow (in that case, make sure you also set up
  ;; `; commentrfu-separator' and a keybind for `corfu-insert-separator', which my
  ;; configuration already has pre-prepared). Necessary for manual corfu usage with
  ;; orderless, otherwise first component is ignored, unless `corfu-separator'
  ;; is inserted.
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match t)
  :preface
  (defun aaronzinhoo--corfu-complete-common-or-next ()
    "Complete the common prefix or preview the next candidate."
    (interactive)
    (cond
      ;; Start completion when Corfu is not active.
      ((not completion-in-region-mode)
        (completion-at-point))

      ;; Immediately insert a sole candidate.
      ((= corfu--total 1)
        (corfu--goto 0)
        (corfu-insert))

      (t
        (let* ((input
                 (car corfu--input))
                (string
                  (if
                    (thing-at-point 'filename)
                    (file-name-nondirectory input)
                    input))
                (point
                  (length string))
                (common
                  (try-completion
                    string
                    corfu--candidates)))

          (if
            (and
              (> point 0)
              (stringp common)
              (not
                (string= string common)))
            ;; Only insert text shared by every candidate.
            (insert
              (substring common point))

            ;; Otherwise change the selected candidate. With
            ;; `corfu-preview-current' set to t, this remains an overlay.
            (corfu-next))))))
  (defun aaronzinhoo--corfu-eshell-setup ()
    "Configure Corfu and completion sources for Eshell."
    (setq-local
      ;; Manual completion is generally less disruptive in shells.
      corfu-auto nil
      corfu-quit-at-boundary t
      corfu-quit-no-match t
      completion-at-point-functions
      (list
        (cape-capf-buster
          (cape-capf-super
            #'pcomplete-completions-at-point
            #'cape-abbrev))
        #'cape-file))
    (corfu-mode 1))
  :init
  ;; local settings for completion at point settings will override this
  (global-corfu-mode 1)
  :config
  ;; use corfu send in shells
  (keymap-set corfu-map "RET" `(menu-item
                                 ""
                                 nil
                                 :filter
                                 ,(lambda (&optional _)
                                    (if
                                      (derived-mode-p
                                        'eshell-mode
                                        'comint-mode)
                                      #'corfu-send
                                      #'corfu-insert))))
  (corfu-history-mode 1)
  (corfu-popupinfo-mode 1) ; Popup completion info
  )
(use-package corfu-popupinfo
  :straight nil
  :after corfu
  :commands
  (corfu-popupinfo-mode
    corfu-popupinfo-toggle
    corfu-popupinfo-documentation
    corfu-popupinfo-location
    corfu-popupinfo-scroll-up
    corfu-popupinfo-scroll-down)
  :bind
  (:map corfu-popupinfo-map
    ("M-t"   . corfu-popupinfo-toggle)
    ("M-l"   . corfu-popupinfo-location)
    ("C-M-n" . corfu-popupinfo-scroll-up)
    ("C-M-p" . corfu-popupinfo-scroll-down)
    ("C-M->" . corfu-popupinfo-end)
    ("C-M-<" . corfu-popupinfo-beginning))
  :custom
  (corfu-popupinfo-delay '(nil . 0.3))
  (corfu-popupinfo-hide nil)
  (corfu-popupinfo-max-height 20)
  (corfu-popupinfo-max-width 80)
  :config
  (corfu-popupinfo-mode 1))
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
  :init
  (with-eval-after-load 'mc-hide-unmatched-lines-mode
    (when (bound-and-true-p mc-hide-unmatched-lines-mode)
      (mc-hide-unmatched-lines-mode -1))

    (define-key hum/hide-unmatched-lines-mode-map
      (kbd "C-'")
      nil)

    (define-key mc/keymap
      (kbd "C-'")
      #'aaronzinhoo--mc/complete-in-region))
  (setq mc/list-file
    (locate-user-emacs-file ".mc-lists.el"))
  :config
  ;; Load previously learned choices before adding configured choices.
  (mc/load-lists)
  (dolist (command
            '(abbrev-prefix-mark
               crux-smart-delete-line
               crux-move-beginning-of-line
               crux
               evilnc-comment-or-uncomment-lines))
    (add-to-list 'mc/cmds-to-run-for-all command))
  (dolist (command
            '(pixel-scroll-precision
               aaronzinhoo--mc/complete-in-region
               avy-goto-char-timer
               corfu-next
               corfu-previous
               corfu-complete
               corfu-quit
               corfu-popupinfo-scroll-up
               corfu-popupinfo-scroll-down
               dap-tooltip-mouse-motion
               multiple-cursors-hydra/body
               multiple-cursors-hydra-hide-unmatched-lines-mode
               wgrep-finish-edit))
    (add-to-list 'mc/cmds-to-run-once command))
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
      (("f" mc/cycle-forward "next cursor")
        ("b" mc/cycle-back "previous cursor"))
      "Mark All"
      (("a" mc/mark-all-like-this "Mark All")
        ("d" mc/mark-all-dwim "Mark All DWIM")
        ("s" symbol-overlay-mc-mark-all "Mark All Symbol"))
      "Misc."
      (("2" er/expand-region "Expand Region")
        ("c" aaronzinhoo--mc/complete-in-region "Autocomplete")
        ("h" mc-hide-unmatched-lines-mode "Hide lines" :toggle t)
        ("RET" newline-and-indent "New Line"))))
  :preface
  (defvar aaronzinhoo--mc-completion-candidate nil
    "Completion candidate shared while completing at multiple cursors.")
  (defun aaronzinhoo--completion-kind-icon (kind)
    "Return a Nerd Icon corresponding to completion KIND."
    (when (and kind
            (fboundp 'nerd-icons-codicon))
      (pcase kind
        ((or 'function 'method 'constructor)
          (nerd-icons-codicon
            "nf-cod-symbol_method"
            :face 'font-lock-function-name-face))

        ((or 'variable 'field 'property)
          (nerd-icons-codicon
            "nf-cod-symbol_variable"
            :face 'font-lock-variable-name-face))

        ('constant
          (nerd-icons-codicon
            "nf-cod-symbol_constant"
            :face 'font-lock-constant-face))

        ('class
          (nerd-icons-codicon
            "nf-cod-symbol_class"
            :face 'font-lock-type-face))

        ('interface
          (nerd-icons-codicon
            "nf-cod-symbol_interface"
            :face 'font-lock-type-face))

        ((or 'module 'namespace)
          (nerd-icons-codicon
            "nf-cod-symbol_namespace"
            :face 'font-lock-preprocessor-face))

        ('keyword
          (nerd-icons-codicon
            "nf-cod-symbol_keyword"
            :face 'font-lock-keyword-face))

        ('snippet
          (nerd-icons-codicon
            "nf-cod-symbol_snippet"
            :face 'font-lock-string-face))

        ('file
          (nerd-icons-codicon
            "nf-cod-file"
            :face 'font-lock-string-face))

        ('folder
          (nerd-icons-codicon
            "nf-cod-folder"
            :face 'font-lock-string-face))

        (_
          (nerd-icons-codicon
            "nf-cod-symbol_misc"
            :face 'font-lock-builtin-face)))))

  (defun aaronzinhoo--completion-candidate-kind
    (kind-function candidate category)
    "Return the kind of CANDIDATE.

Call KIND-FUNCTION when available, otherwise use CATEGORY."
    (or
      (when kind-function
        (condition-case nil
          (funcall kind-function candidate)
          (error nil)))
      category))

  (defun aaronzinhoo--completion-kind-name (kind)
    "Return a display name for completion KIND."
    (when kind
      (capitalize
        (replace-regexp-in-string
          "[-_]"
          " "
          (if (symbolp kind)
            (symbol-name kind)
            (format "%s" kind))))))
  (defun aaronzinhoo--completion-affixate (buffer kind-function annotation-function category candidates)
    "Add icons, kinds, and annotations to CANDIDATES.

Evaluate completion callbacks inside BUFFER."
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (mapcar
          (lambda (candidate)
            (let* ((kind
                     (aaronzinhoo--completion-candidate-kind
                       kind-function
                       candidate
                       category))
                    (icon
                      (aaronzinhoo--completion-kind-icon kind))
                    (kind-name
                      (aaronzinhoo--completion-kind-name kind))
                    (annotation
                      (when annotation-function
                        (condition-case nil
                          (funcall annotation-function candidate)
                          (error nil))))
                    (prefix
                      (if icon
                        (concat icon " ")
                        ""))
                    (suffix
                      (concat
                        (if kind-name
                          (propertize
                            (format "  %s" kind-name)
                            'face
                            'completions-annotations)
                          "")
                        (or annotation ""))))
              ;; An affixation function returns:
              ;; (CANDIDATE PREFIX SUFFIX)
              (list candidate prefix suffix)))
          candidates))))
  (defun aaronzinhoo--complete-in-region-minibuffer ()
    "Complete at the current cursor through the minibuffer.

The first cursor selects a candidate through Vertico. Subsequent
cursors reuse that candidate while calculating their own CAPF
replacement boundaries."
    (interactive)
    (let ((capf
            (run-hook-with-args-until-success
              'completion-at-point-functions)))
      (pcase capf
        (`(,start ,end ,collection . ,plist)
          (let* ((source-buffer
                   (current-buffer))
                  (initial
                    (buffer-substring-no-properties start end))
                  (predicate
                    (plist-get plist :predicate))
                  (exit-function
                    (plist-get plist :exit-function))
                  (kind-function
                    (plist-get plist :company-kind))

                  ;; Collect metadata while the source buffer is current.
                  (metadata
                    (completion-metadata
                      initial collection predicate))
                  (category
                    (completion-metadata-get
                      metadata 'category))
                  (annotation-function
                    (completion-metadata-get
                      metadata 'annotation-function))

                  ;; Materialize the completion table before entering the
                  ;; minibuffer. This is important for CAPE and LSP tables
                  ;; tied to the source buffer.
                  (candidates
                    (all-completions
                      initial collection predicate))

                  ;; Add icons, kinds, and annotations to the materialized
                  ;; candidate table.
                  (affixation-function
                    (apply-partially
                      #'aaronzinhoo--completion-affixate
                      source-buffer
                      kind-function
                      annotation-function
                      category))

                  (candidate-table
                    (completion-table-with-metadata
                      candidates
                      `((category . ,category)
                         (affixation-function
                           . ,affixation-function))))
                  completion)

            (setq completion
              (cond
                ;; Reuse the selection made at the first cursor.
                (aaronzinhoo--mc-completion-candidate
                  aaronzinhoo--mc-completion-candidate)

                ;; No matching candidates.
                ((null candidates)
                  nil)

                ;; Select a sole candidate without opening Vertico.
                ((null (cdr candidates))
                  (car candidates))

                ;; Select one candidate through Vertico.
                (t
                  (completing-read
                    "Completion: "
                    candidate-table
                    nil
                    t
                    initial))))

            (unless completion
              (user-error "No completion candidate"))

            ;; The same selection must be valid at every cursor.
            (unless (member completion candidates)
              (user-error
                "Candidate `%s' is unavailable at this cursor"
                completion))

            ;; Save the first selection for the remaining cursors.
            (unless aaronzinhoo--mc-completion-candidate
              (setq aaronzinhoo--mc-completion-candidate
                completion))

            ;; Replace the CAPF-determined region at this cursor.
            (completion--replace start end completion)

            ;; Notify LSP/CAPE that completion finished.
            (when exit-function
              (funcall exit-function completion 'finished))

            t))

        (_
          (user-error
            "No completion available at this cursor")))))
  (defun aaronzinhoo--mc/complete-in-region ()
    "Select one completion and apply it at every active cursor."
    (interactive)
    (let ((aaronzinhoo--mc-completion-candidate nil))
      (if (bound-and-true-p multiple-cursors-mode)
        (mc/execute-command-for-all-cursors
          #'aaronzinhoo--complete-in-region-minibuffer)
        (aaronzinhoo--complete-in-region-minibuffer))))
  )


;;; Creating Diagrams
(use-package plantuml-mode
  :straight (:type git :host github :repo "Aaronzinhoo/plantuml-mode" :branch "master")
  :mode (("\\plantuml\\'" . plantuml-mode))
  :hook (plantuml-mode . aaronzinhoo--plantuml-setup-hook)
  :preface
  (defun aaronzinhoo-plantuml-setup-hook ()
    (aaronzinhoo--append-capfs
     #'plantuml-completion-at-point
     #'cape-abbrev
     #'cape-dabbrev))
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
(use-package corg
  :straight (:type git :host github :repo "isamert/corg.el")
  :commands (corg-completion-at-point))
(use-package org
  :mode (("\\.org$" . org-mode))
  :hook ((org-mode . aaronzinhoo--org-setup)
          (org-mode . aaronzinhoo--org-font-setup))
  :bind
  ("C-c l" . org-store-link)
  ("C-c A" . org-agenda)
  (:map org-mode-map
    ("C-M-<return>" . org-insert-subheading)
    ("s-h". hydra-org-nav/body))
  :preface
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
    "Configure the current Org buffer."
    (variable-pitch-mode 1)
    (org-indent-mode 1)
    (aaronzinhoo--append-capfs
     #'corg-completion-at-point
     #'cape-file
     #'cape-dict
     #'cape-dabbrev))
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
  (org-plantuml-executable-path (executable-find "plantuml"))
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
  (if (aaronzinhoo--wsl-p)
    (progn
      (setq browse-url-browser-function 'browse-url-generic browse-url-generic-program "wslview")))
  (add-hook 'org-export-before-processing-hook 'aaronzinhoo-org-inline-css-hook)
  :config
  (require 'org-tempo)
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
    '((auto-mode . emacs)
       ("\\.mm\\'" . default)
       ("\\.x?html?\\'" . default)
       ("\\.pdf\\'" . default)))

  ;; Map Org source-block language names to Emacs major-mode names.
  (dolist (mapping
            '(("plantuml"   . plantuml)
               ("js"         . js-ts)
               ("python"     . python-ts)
               ("typescript" . typescript-ts)
               ("browser"    . js-ts)
               ("html"       . html-ts)
               ("verb"       . verb)))
    (add-to-list
      'org-src-lang-modes
      mapping))

  ;; Expand `<key TAB' into an Org structure template.
  (dolist (template
            '(("plantuml" . "src plantuml")
               ("html"     . "src html")
               ("browser"  . "src browser")
               ("js"       . "src js")
               ("py"       . "src python")
               ("ts"       . "src typescript")
               ("verb"     . "src verb")))
    (add-to-list
      'org-structure-template-alist
      template))

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
  (require 'ox-publish))
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
  :straight t
  :demand t
  :if (or
        (daemonp)
        (memq window-system '(mac ns x pgtk)))
  :custom
  (exec-path-from-shell-shell-name "/bin/zsh")
  (exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-variables '("PATH"
                                     "LIBRARY_PATH"
                                     "C_INCLUDE_PATH"
                                     "PKG_CONFIG_PATH"
                                     "MANPATH"
                                     "LANG"
                                     "SSH_AUTH_SOCK"

                                     "XDG_CONFIG_HOME"
                                     "XDG_CACHE_HOME"
                                     "XDG_DATA_HOME"
                                     "XDG_STATE_HOME"

                                     "CARGO_HOME"
                                     "RUSTUP_HOME"
                                     "GOENV_ROOT"
                                     "GOPATH"
                                     "GOBIN"
                                     "NVM_DIR"
                                     "PYENV_ROOT"
                                     "SDKMAN_DIR"
                                     "KREW_ROOT"))
  :config
  (exec-path-from-shell-initialize))
(use-package list-environment
  :commands (list-environment))
(use-package ghostel
  :straight t
  :commands (ghostel ghostel-project ghostel-project-list-buffers consult-ghostel-history consult-ghostel consult-ghostel-project)
  :bind (:map ghostel-semi-char-mode-map
              ("C-c t" . consult-ghostel-history))
  :hook ((ghostel-mode . aaronzinhoo--setup-ghostel-expansions))
  :custom
  (ghostel-kill-buffer-on-exit t)
  ;; Prefer live navigation over frozen copy mode.
  (ghostel-readonly-default-mode 'emacs)
  ;; Typing returns from read-only navigation to terminal input.
  (ghostel-readonly-fast-exit t)
  :preface
  (defun aaronzinhoo--setup-ghostel-expansions ()
    "Configure Expand Region for Ghostel buffers."
    (setq-local
     er/try-expand-list
     '(er/mark-word
       er/mark-symbol

       ;; Environment assignments and comma-separated values.
       aaronzinhoo--mark-assignment-list-item
       aaronzinhoo--mark-assignment-list

       er/mark-inside-quotes
       er/mark-outside-quotes
       er/mark-inside-pairs
       er/mark-outside-pairs
       er/mark-paragraph))))
(use-package ghostel-compile
  :straight nil
  :after ghostel
  :commands (ghostel-compile ghostel-recompile))
(use-package ghostel-comint
  :straight nil
  :after ghostel
  :hook (shell-mode . ghostel-comint-mode))
(use-package comint
  :straight (:type built-in)
  :bind (:map comint-mode-map
              ("C-M-i" . completion-at-point))
  :custom
  (comint-prompt-read-only t)
  (comint-input-ignoredups t)
  ;; Return to the prompt when typing.
  (comint-scroll-to-bottom-on-input 'this)
  ;; Follow output at the bottom, but allow scrolling back.
  (comint-move-point-for-output nil)
  ;; Keep the latest output at the bottom of the window.
  (comint-scroll-show-maximum-output t))
(use-package ansi-color
  :straight (:type built-in)
  :hook ((compilation-filter . ansi-color-compilation-filter)))
;; Programming/Project Management
;; commenting does not have support for native tree sitter yet
(use-package turbo-log
  :commands (turbo-log-print-immediately turbo-log-print)
  :straight (:type git :host github :repo "artawower/turbo-log.el")
  :config
  (setq turbo-console--prefix "LOG"))

(use-package evil-nerd-commenter
  :bind* ("C-;" . evilnc-comment-or-uncomment-lines))
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
(use-package project
  :straight nil
  :demand t
  :after (major-mode-hydra)
  :bind* ("s-p" . project-hydra/body)
  :pretty-hydra
  (project-hydra
    (:hint nil :color teal :quit-key "SPC" :title (with-octicon "nf-oct-rocket" "Project Menu" 1 -0.05))
    ("Buffers"
      (("b" consult-project-buffer "list")
        ("k" project-kill-buffers "kill all")
        ("S" aaronzinhoo--project-save-project-buffers "save all"))
      "Find"
      (("d" project-find-dir "directory")
        ("D" project-dired "Open proj. root")
        ("f" project-find-file "file")
        ("p" project-switch-project "project")
        ("F" project-or-external-find-file "find file ext. + proj"))
      "Other"
      (("C" project-forget-zombie-projects "Clear out old projects")
        ("c" project-compile "Compile")
        ("v" ghostel-project "Run ghostel")
        ("V" consult-ghostel-project "Run ghostel")
        ("R" project-remember-projects-under "Register Proj(s). under Dir"))
      "Search & Replace"
      (("r" project-query-replace-regexp "regexp replace")
        ("s" aaronzinhoo--project-consult-ripgrep-dwim "search"))))
  :preface
  (defun aaronzinhoo--project-consult-ripgrep-dwim (&optional given-initial)
    (interactive)
    (let ((initial
            (cond
              ((not (null given-initial)) given-initial)
              ((use-region-p)
                (buffer-substring-no-properties (region-beginning) (region-end))
                (deactivate-mark))
              (t ""))))
      (consult-ripgrep (project-root (project-current)) initial)))
  (defun aaronzinhoo--project-save-project-buffers ()
    "Save all project buffers."
    (interactive)
    (let* ((project (project-current))
            (current-project-name (project-name project))
            (modified-buffers (cl-remove-if-not (lambda (buf)
                                                  (and (buffer-file-name buf)
                                                    (buffer-modified-p buf)))
                                (project-buffers project))))
      (if (null modified-buffers)
        (message "[%s] No buffers need saving" current-project-name)
        (dolist (buf modified-buffers)
          (with-current-buffer buf
            (save-buffer)))
        (message "[%s] Saved %d buffer(s)" current-project-name (length modified-buffers))))))
;;; Languages Support
;; Code Coverage
(use-package cov
  :defer t)
(use-package coverlay
  :commands (coverlay-mode))

;; Shell
(use-package sh-script
  :straight nil
  :mode (("\\.sh\\'" . bash-ts-mode)
         ("\\.bash\\'" . bash-ts-mode)
         ("\\.alias\\'" . bash-ts-mode)
         ("\\.zsh\\'" . bash-ts-mode)
         ("/\\.zshenv\\'" . bash-ts-mode)
         ("/\\.zprofile\\'" . bash-ts-mode)
         ("/\\.zshrc\\'" . bash-ts-mode))
  :hook ((bash-ts-mode . aaronzinhoo--setup-bash-ts-mode)
         (bash-ts-mode . lsp-deferred)
         (sh-mode . lsp-deferred))
  :custom
  (sh-basic-offset 2)
  (sh-indentation 2)
  :interpreter
  (("bash" . bash-ts-mode))
  :preface
  (defun aaronzinhoo--setup-bash-ts-mode ()
    "Configure Bash Tree-sitter buffers."
    (setq-local tab-width 2)
    (defun aaronzinhoo--ssh-config-mode-hook ()
      "Configure completion in SSH configuration buffers."
      (aaronzinhoo--append-capfs
       #'sh-completion-at-point-function
       #'cape-file
       #'cape-dabbrev)))
  )
;; Yaml editing support and JSON
;; json-mode => json-snatcher json-refactor
;; select yaml regex (^-[\s]*[A-Za-z0-9-_]*)|(^[A-Za-z_-]*:)
(use-package dtrt-indent
  :straight t
  :config
  (add-to-list
    'dtrt-indent-hook-mapping-list
    '(yaml-ts-mode default
       (yaml-indent-offset tab-width)))
  (dtrt-indent-global-mode 1))
(use-package openapi-preview
  :commands (openapi-preview)
  :straight (:type git :host github :repo "merrickluo/openapi-preview" :branch "main")
  :custom
  (openapi-preview-redoc-command "redoc-cli"))
(use-package yaml-mode
  :straight t
  :defer t
  :bind ((:map yaml-mode-map
           ("s-h" . yaml-hydra/body))))
(use-package yaml-ts-mode
  :straight nil
  :after flycheck
  :bind ((:map yaml-ts-mode-map
           ("s-h" . yaml-hydra/body)
           ("TAB" . indent-for-tab-command)
           ("<tab>" . indent-for-tab-command)
           ("<backtab>" . yaml-indent-line)))
  :hook ((yaml-ts-mode . aaronzinhoo--yaml-mode-hook)
          (yaml-ts-mode . aaronzinhoo--yaml-completion-setup)
          (yaml-ts-mode . lsp-deferred)
          (yaml-ts-mode . hungry-delete-mode))
  :custom
  ;; Fallback when dtrt-indent cannot detect the indentation width.
  (yaml-indent-offset 2)
  :preface
  (defun aaronzinhoo--yaml-completion-setup ()
    "Add HTML-specific completion sources."
    (aaronzinhoo--append-capfs
      #'cape-keyword
      #'cape-dabbrev))
  (defun aaronzinhoo--yaml-mode-hook ()
    "Use yaml-mode's indentation logic in yaml-ts-mode."
    (require 'yaml-mode)
    (setq-local
      indent-line-function #'yaml-indent-line
      lsp-java-boot-enabled nil
      lsp-lens-mode nil))
  :pretty-hydra
  (yaml-hydra
    (:hint nil
      :title (with-faicon
               "nf-fa-yen"
               "YAML Commands"
               1
               -0.05)
      :quit-key "q"
      :color red)
    ("Indent"
      (("i" indent-rigidly "Indent Region"))
      "Navigation"
      (("N" block-nav-next-indentation-level
         "Next Child Node")
        ("P" block-nav-previous-indentation-level
          "Prev Parent Node") )
      "Fold"
      (("f" aaronzinhoo--hs-toggle-block
         "toggle block")
        ("c" aaronzinhoo--hs-hide-block
          "close block")
        ("o" aaronzinhoo--hs-show-block
          "open block")
        ("C" hs-hide-all
          "close all")
        ("O" hs-show-all
          "open all")
        ("L" hs-hide-level
          "close level"))
      "Schema"
      (("s" lsp-yaml-select-buffer-schema
         "Buffer Schema")
        ("d" lsp-yaml-download-schema-store-db
          "Download Schemastore")))))

;; web dev
(use-package helm-ts-mode
  :straight nil
  :load-path (lambda () (expand-file-name "elisp" user-emacs-directory))
  ;; Start LSP only after entering Helm mode.
  :hook ((helm-ts-mode . aaronzinhoo--setup-helm-treesit-regions)
          (helm-ts-mode . lsp-deferred))
  :bind ((:map helm-ts-mode-map
           ("s-h" . helm-hydra/body)))
  :mode
  (("/templates/.*\\.ya?ml\\'" . helm-ts-mode)
    ("/templates/.*\\.tpl\\'" . helm-ts-mode)
    ("\\.helm\\.ya?ml\\'" . helm-ts-mode))
  :pretty-hydra
  (helm-hydra
    (:hint nil
      :title (with-faicon
               "nf-fa-yen"
               "YAML Commands"
               1
               -0.05)
      :quit-key "q"
      :color red)
    ("Indent"
      (("i" indent-rigidly "Indent Region"))
      "Navigation"
      (("N" block-nav-next-indentation-level
         "Next Child Node")
        ("P" block-nav-previous-indentation-level
          "Prev Parent Node") )
      "Fold"
      (("f" aaronzinhoo--hs-toggle-block
         "toggle block")
        ("c" aaronzinhoo--hs-hide-block
          "close block")
        ("o" aaronzinhoo--hs-show-block
          "open block")
        ("C" hs-hide-all
          "close all")
        ("O" hs-show-all
          "open all")
        ("L" hs-hide-level
          "close level"))
      "Helm"
      (("e" helm-ts-mode-select-environment "Update Environment")
        ("t" helm-ts-mode-describe-parsers "Treesit Parsers")
        ("s" lsp-yaml-select-buffer-schema "Buffer Schema"))))
  :preface
  (defun aaronzinhoo--setup-helm-treesit-regions ()
    "Register Helm-specific semantic regions."
    (dolist (function
              '(helm-ts-mode-action-bounds
                 helm-ts-mode-indentation-parent-bounds
                 helm-ts-mode-containing-line-bounds))
      (add-to-list
        'aaronzinhoo--treesit-extra-region-functions
        function
        t)))
  :config
  ;; Tell LSP which language ID to send for this custom mode.
  (add-to-list
    'lsp-language-id-configuration
    '(helm-ts-mode . "helm-ls")))
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
    ("Create Request"
      (("c" swagg-request-with-rest-block "Swagg Create"))
      "Run Request"
      (("rr" verb-send-request-on-point-other-window-stay "Other Window (Stay)")
        ("ro" verb-send-request-on-point-other-window "Other Window")
        ("rc" verb-send-request-on-point "Current Window")
        ("rs" swagg-request "Swagg Request"))
      "Kill"
      (("k" aaronzinhoo--verb-kill-this-buffer  "This Response Buffer")
        ("K" verb-kill-all-response-buffers  "All Response Buffers and Windows")))))
(use-package swagg
  :straight (:type git :host github :repo "isamert/swagg.el" :branch "main")
  :commands (swagg-request swagg-request-with-rest-block))
;; demanding openapi-yaml-mode since need mode file config to be loaded after yaml
(use-package openapi-yaml-mode
  :straight nil
  :demand t
  :load-path "~/.emacs.d/elisp"
  :hook ((openapi-yaml-mode . lsp-deferred))
  :bind (:map openapi-yaml-mode-map
          ("s-h" . openapi-yaml-hydra/body))
  :pretty-hydra
  (openapi-yaml-hydra
    (:hint nil
      :title (with-faicon
               "nf-fa-yen"
               "YAML Commands"
               1
               -0.05)
      :quit-key "q"
      :color red)
    ("Indent"
      (("i" indent-rigidly "Indent Region"))
      "Navigation"
      (("N" block-nav-next-indentation-level
         "Next Child Node")
        ("P" block-nav-previous-indentation-level
          "Prev Parent Node") )
      "Fold"
      (("f" aaronzinhoo--hs-toggle-block
         "toggle block")
        ("c" aaronzinhoo--hs-hide-block
          "close block")
        ("o" aaronzinhoo--hs-show-block
          "open block")
        ("C" hs-hide-all
          "close all")
        ("O" hs-show-all
          "open all")
        ("L" hs-hide-level
          "close level"))
      "Openapi"
      (("v" openapi-preview "View in Browser")
        ("s" lsp-yaml-select-buffer-schema
          "Buffer Schema"))))
  :config
  (add-to-list
    'lsp-language-id-configuration
    '(openapi-yaml-mode . "yaml")))
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
  :hook (jenkinsfile-mode . aaronzinhoo--jenkinsfile-mode-hook)
  :preface
  (defun aaronzinhoo--jenkinsfile-mode-hook ()
    (setq-local completion-at-point-functions (list #'cape-file #'cape-keyword #'cape-dabbrev #'cape-dict))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DEVOPS CONFIG
(use-package docker
  :straight t
  :commands (docker)
  :bind ("s-d" . docker))
(use-package dockerfile-mode
  :commands (dockerfile-build-buffer dockerfile-build-no-cache-buffer)
  :straight (:type git :host github :repo "spotify/dockerfile-mode" :branch "master")
  :custom
  (dockerfile-use-buildkit t))
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
(use-package kubed
  :straight (:type git :host github :repo "eshelyaron/kubed" :branch "master")
  :bind* ("s-k" . kubed-transient))

;;; WEB-DEV CONFIG
;; for hosting a web server and development if needed
(use-package simple-httpd
  :defer t)
(use-package skewer-mode
  :defer t)
(use-package dotenv
  :straight (:type git :host github :repo "pkulev/dotenv.el" :branch "main")
  :hook
  (prog-mode . dotenv-update-current-env))
;; apache
(use-package apache-mode
  :mode (("\\(?:apache2\\|httpd\\)\\.conf\\'" . apache-mode)
         ("\\.htaccess\\'" . apache-mode)
         ("/ports\\.conf\\'" . apache-mode)
         ("/sites-\\(?:available\\|enabled\\)/[^/]+\\'" . apache-mode)
         ("/conf-\\(?:available\\|enabled\\)/[^/]+\\.conf\\'" . apache-mode)
         ("/mods-\\(?:available\\|enabled\\)/[^/]+\\.conf\\'" . apache-mode))
  :hook ((apache-mode . aaronzinhoo--apache-completion-setup))
  :preface
  (defconst aaronzinhoo--apache-value-completions
    '(("AllowOverride"
       "None" "All"
       "AuthConfig" "FileInfo" "Indexes" "Limit" "Nonfatal")
      ("AuthType"
       "Basic" "Digest")
      ("LogLevel"
       "emerg" "alert" "crit" "error"
       "warn" "notice" "info" "debug" "trace1" "trace2"
       "trace3" "trace4" "trace5" "trace6" "trace7" "trace8")
      ("Options"
       "None" "All" "ExecCGI" "FollowSymLinks" "Includes"
       "Indexes" "MultiViews" "SymLinksIfOwnerMatch")
      ("Require"
       "all" "env" "group" "host" "ip" "local"
       "method" "not" "user" "valid-user")
      ("RewriteEngine"
       "On" "Off")
      ("SSLEngine"
       "On" "Off" "optional")
      ("SSLHonorCipherOrder"
       "On" "Off")
      ("ServerSignature"
       "On" "Off" "EMail")
      ("ServerTokens"
       "Full" "OS" "Minimal" "Minor" "Major" "Prod")
      ("TraceEnable"
       "On" "Off" "extended")
      ("UseCanonicalName"
       "On" "Off" "DNS"))
    "Common values associated with Apache directives.")
  (defconst aaronzinhoo--apache-fallback-directives
    '("AcceptFilter"
      "AccessFileName"
      "AddDefaultCharset"
      "Alias"
      "AliasMatch"
      "AllowOverride"
      "AllowOverrideList"
      "AuthName"
      "AuthType"
      "CustomLog"
      "DeflateCompressionLevel"
      "DirectoryIndex"
      "DocumentRoot"
      "EnableSendfile"
      "ErrorDocument"
      "ErrorLog"
      "ExpiresActive"
      "ExpiresByType"
      "Header"
      "Include"
      "IncludeOptional"
      "KeepAlive"
      "KeepAliveTimeout"
      "LimitRequestBody"
      "Listen"
      "LoadModule"
      "LogFormat"
      "LogLevel"
      "MaxKeepAliveRequests"
      "Options"
      "ProxyPass"
      "ProxyPassMatch"
      "ProxyPassReverse"
      "Redirect"
      "RedirectMatch"
      "Require"
      "RewriteBase"
      "RewriteCond"
      "RewriteEngine"
      "RewriteRule"
      "ServerAdmin"
      "ServerAlias"
      "ServerName"
      "ServerRoot"
      "ServerSignature"
      "ServerTokens"
      "SetEnv"
      "SetEnvIf"
      "SSLCertificateFile"
      "SSLCertificateKeyFile"
      "SSLCipherSuite"
      "SSLEngine"
      "SSLHonorCipherOrder"
      "SSLProtocol"
      "Timeout"
      "TraceEnable"
      "UseCanonicalName"

      ;; Section directives
      "<Directory"
      "<DirectoryMatch"
      "<Files"
      "<FilesMatch"
      "<If"
      "<IfModule"
      "<IfVersion"
      "<Limit"
      "<LimitExcept"
      "<Location"
      "<LocationMatch"
      "<Proxy"
      "<VirtualHost")
    "Fallback Apache directives used when Apache is unavailable.")
  (defvar aaronzinhoo--apache-directive-cache nil
    "Cached directives reported by the local Apache installation.")
  (defvar aaronzinhoo--apache-directive-cache-initialized-p nil
    "Whether Apache directive discovery has been attempted.")
  (defun aaronzinhoo--apache-executable ()
    "Return an available Apache control executable."
    (seq-find
     #'executable-find
     '("apachectl"
       "apache2ctl"
       "httpd")))
  (defun aaronzinhoo--apache-installed-directives ()
    "Return directives supported by the installed Apache server."
    (unless aaronzinhoo--apache-directive-cache-initialized-p
      (setq aaronzinhoo--apache-directive-cache-initialized-p
            t)

      (setq aaronzinhoo--apache-directive-cache
            (when-let* ((executable
                         (aaronzinhoo--apache-executable)))
              (with-temp-buffer
                (when
                    (zerop
                     (call-process
                      executable
                      nil
                      t
                      nil
                      "-L"))
                  (goto-char
                   (point-min))
                  (let (directives)
                    (while
                        (re-search-forward
                         "^[[:blank:]]*\\([^[:blank:]\n]+\\)[[:blank:]]+("
                         nil
                         t)
                      (push
                       (match-string-no-properties 1)
                       directives))
                    (delete-dups directives)))))))

    aaronzinhoo--apache-directive-cache)
  (defun aaronzinhoo--apache-directives ()
    "Return available Apache completion candidates."
    (delete-dups
     (append
      (aaronzinhoo--apache-installed-directives)
      aaronzinhoo--apache-fallback-directives)))
  (defun aaronzinhoo--apache-refresh-directives ()
    "Clear and rebuild the installed Apache directive cache."
    (interactive)
    (setq aaronzinhoo--apache-directive-cache nil
          aaronzinhoo--apache-directive-cache-initialized-p nil)
    (aaronzinhoo--apache-installed-directives)
    (message "Apache directive completion refreshed"))
  (defun aaronzinhoo--apache-completion-at-point ()
    "Complete an Apache directive at the beginning of a line."
    (let ((end
           (point))
          beginning)
      (save-excursion
        (skip-chars-backward
         "[:alnum:]_<")
        (setq beginning
              (point)))

      (when
          (string-match-p
           "\\`[[:blank:]]*\\'"
           (buffer-substring-no-properties
            (line-beginning-position)
            beginning))
        (list
         beginning
         end
         (aaronzinhoo--apache-directives)
         :exclusive 'no
         :company-kind
         (lambda (_)
           'keyword)))))
  (defun aaronzinhoo--apache-value-completion-at-point ()
    "Complete common values for the directive on the current line."
    (save-excursion
      (let ((end
             (point))
            beginning
            directive)
        (skip-chars-backward
         "^ \t\n")
        (setq beginning
              (point))

        (goto-char
         (line-beginning-position))
        (when
            (looking-at
             "[[:blank:]]*\\([[:alnum:]]+\\)[[:blank:]]+")
          (setq directive
                (match-string-no-properties 1)))

        (when-let* ((values
                     (cdr
                      (assoc-string
                       directive
                       aaronzinhoo--apache-value-completions
                       t))))
          (list
           beginning
           end
           values
           :exclusive 'no)))))
  (defun aaronzinhoo--apache-completion-setup ()
    "Configure Apache completion in the current buffer."
    (setq-local completion-ignore-case t)
    (aaronzinhoo--append-capfs
     #'aaronzinhoo--apache-completion-at-point
     #'aaronzinhoo--apache-value-completion-at-point
     #'cape-file
     #'cape-dabbrev))
  )
(use-package add-node-modules-path
  :hook ((rjsx-mode . add-node-modules-path)
          (typescript-mode . add-node-modules-path)
          (json-mode . add-node-modules-path)
          (js-ts-mode . add-node-modules-path)
          (tsx-ts-mode . add-node-modules-path)
          (typescript-ts-mode . add-node-modules-path)
          ;; add completion for css class names in html files
          (css-mode . add-node-modules-path)))
(use-package nxml-mode
  :straight nil
  :mode (("\\.\\(?:xml\\|xsd\\|sch\\|rng\\|xslt\\|xsl\\|svg\\|rss\\)\\'" . nxml-mode))
  :bind (:map nxml-mode-map
          ("s-h" . hydra-xml/body))
  :hook ((nxml-mode . aaronzinhoo--nxml-setup-treesit)
          (nxml-mode . lsp-deferred)
          (nxml-mode . yas-minor-mode))
  :pretty-hydra
  (hydra-xml
    (:title "XML"
      :color amaranth
      :quit-key "q")
    ("Navigation"
      (("n" nxml-forward-element       "next element")
        ("p" nxml-backward-element      "previous element")
        ("u" nxml-backward-up-element   "parent element")
        ("d" nxml-down-element          "child element"))
      "Editing"
      (("c" nxml-balanced-close-start-tag-inline
         "close element")
        ("s" nxml-split-element
          "split element")
        ("f" nxml-finish-element
          "finish element")
        ("t" nxml-balanced-close-start-tag-block
          "close as block"))
      "Validation"
      (("v" rng-validate-mode
         "toggle validation")
        ("]" rng-next-error
          "next error")
        ("[" rng-previous-error
          "previous error")
        ("a" rng-auto-set-schema-and-validate
          "detect schema")
        ("S" rng-set-schema-file-and-validate
          "select schema"))
      "Tools"
      (("r" nxml-mode
         "restart mode")
        ("i" imenu
          "index"))))
  :preface
  (defun aaronzinhoo--nxml-setup-treesit ()
    "Create an XML Tree-sitter parser in the current NXML buffer."
    (when
      (treesit-ready-p 'xml t)
      (treesit-parser-create 'xml))))
(use-package css-mode
  :straight nil
  :hook ((scss-mode . aaronzinhoo--scss-completion-setup))
  :preface
  (defun aaronzinhoo--scss-completion-setup ()
    "Add HTML-specific completion sources."
    (aaronzinhoo--append-capfs
      #'cape-keyword
      #'cape-dabbrev))
  :custom
  (css-indent-offset 2))
(use-package html-ts-mode
  :straight nil
  :mode (("\\.html?\\'" . html-ts-mode))
  :hook ((html-ts-mode . aaronzinhoo--html-setup)
          (html-ts-mode . aaronzinhoo--html-completion-setup))
  :preface
  (defconst aaronzinhoo--html-ts-element-node-types
    '("element"
       "script_element"
       "style_element")
    "Tree-sitter node types representing complete HTML elements.")
  (defun aaronzinhoo--html-completion-setup ()
    "Add HTML-specific completion sources."
    (aaronzinhoo--append-capfs
      #'cape-keyword
      #'cape-dabbrev))
  (defun aaronzinhoo--html-setup ()
    "Configure the current HTML Tree-sitter buffer."
    ;; Disable LSP lenses without directly changing the minor-mode
    ;; state variable.
    (setq-local lsp-lens-enable nil))
  (defun aaronzinhoo--html-ts-element-node (&optional position)
    "Return the smallest HTML element containing POSITION."
    (let ((node
            (treesit-node-at
              (or position
                (point))
              'html
              t)))
      (while
        (and node
          (not
            (member
              (treesit-node-type node)
              aaronzinhoo--html-ts-element-node-types)))
        (setq node
          (treesit-node-parent node)))
      node))
  (defun aaronzinhoo--html-ts-parent-element ()
    "Move to the parent HTML element."
    (interactive)
    (if-let* ((node
                (aaronzinhoo--html-ts-element-node))
               (parent
                 (treesit-node-parent node)))
      (progn
        (while
          (and parent
            (not
              (member
                (treesit-node-type parent)
                aaronzinhoo--html-ts-element-node-types)))
          (setq parent
            (treesit-node-parent parent)))

        (if parent
          (goto-char
            (treesit-node-start parent))
          (user-error
            "No parent HTML element")))
      (user-error
        "Point is not inside an HTML element")))
  (defun aaronzinhoo--html-ts-child-element ()
    "Move to the first direct child HTML element."
    (interactive)
    (if-let* ((node
               (aaronzinhoo--html-ts-element-node)))
      (let ((index 0)
             (count
               (treesit-node-child-count node t))
             child)
        (while
          (and (< index count)
            (not child))
          (let ((candidate
                  (treesit-node-child node index t)))
            (when
              (member
                (treesit-node-type candidate)
                aaronzinhoo--html-ts-element-node-types)
              (setq child candidate)))

          (setq index
            (1+ index)))

        (if child
          (goto-char
            (treesit-node-start child))
          (user-error
            "No child HTML element")))
      (user-error
        "Point is not inside an HTML element")))
  (defun aaronzinhoo--html-ts-sibling-element (direction)
    "Move to an HTML sibling in DIRECTION.

DIRECTION must be either `next' or `previous'."
    (if-let* ((node
               (aaronzinhoo--html-ts-element-node)))
      (let ((sibling
              (if
                (eq direction 'next)
                (treesit-node-next-sibling node t)
                (treesit-node-prev-sibling node t))))
        ;; Skip text, comments, and other non-element siblings.
        (while
          (and sibling
            (not
              (member
                (treesit-node-type sibling)
                aaronzinhoo--html-ts-element-node-types)))
          (setq sibling
            (if
              (eq direction 'next)
              (treesit-node-next-sibling sibling t)
              (treesit-node-prev-sibling sibling t))))

        (if sibling
          (goto-char
            (treesit-node-start sibling))
          (user-error
            "No %s HTML sibling"
            direction)))
      (user-error
        "Point is not inside an HTML element")))
  (defun aaronzinhoo--html-ts-next-sibling ()
    "Move to the next sibling HTML element."
    (interactive)
    (aaronzinhoo--html-ts-sibling-element
      'next))
  (defun aaronzinhoo--html-ts-previous-sibling ()
    "Move to the previous sibling HTML element."
    (interactive)
    (aaronzinhoo--html-ts-sibling-element
      'previous))
  (defun aaronzinhoo--html-ts-element-beginning ()
    "Move to the beginning of the surrounding HTML element."
    (interactive)
    (if-let* ((node
               (aaronzinhoo--html-ts-element-node)))
      (goto-char
        (treesit-node-start node))
      (user-error
        "Point is not inside an HTML element")))
  (defun aaronzinhoo--html-ts-element-end ()
    "Move to the end of the surrounding HTML element."
    (interactive)
    (if-let* ((node
               (aaronzinhoo--html-ts-element-node)))
      (goto-char
        (treesit-node-end node))
      (user-error
        "Point is not inside an HTML element")))
  (defun aaronzinhoo--html-ts-mark-element ()
    "Mark the complete surrounding HTML element."
    (interactive)
    (if-let* ((node
               (aaronzinhoo--html-ts-element-node)))
      (progn
        (goto-char
          (treesit-node-start node))
        (set-mark
          (treesit-node-end node))
        (activate-mark))
      (user-error
        "Point is not inside an HTML element")))
  :pretty-hydra
  ((:hint nil
     :title
     (with-faicon
       "nf-fa-html5"
       "HTML Tree-sitter"
       1
       -0.05)
     :quit-key "SPC"
     :color pink)
    ("Navigation"
      (("n" aaronzinhoo--html-ts-next-sibling
         "next sibling")
        ("p" aaronzinhoo--html-ts-previous-sibling
          "previous sibling")
        ("u" aaronzinhoo--html-ts-parent-element
          "parent element")
        ("d" aaronzinhoo--html-ts-child-element
          "child element")
        ("a" aaronzinhoo--html-ts-element-beginning
          "element beginning")
        ("e" aaronzinhoo--html-ts-element-end
          "element end")
        ("m" aaronzinhoo--html-ts-mark-element
          "mark element"))
      "Fold"
      (("f" aaronzinhoo--hs-toggle-block
         "toggle element")
        ("C" hs-hide-all
          "close all")
        ("O" hs-show-all
          "open all")
        ("L" hs-hide-level
          "close level"))
      "Tags"
      (("t" sgml-tag
         "insert tag")
        ("/" sgml-close-tag
          "close tag")
        ("D" sgml-delete-tag
          "delete tag")
        ("A" sgml-attributes
          "attributes")
        ("E" sgml-electric-tag-pair-mode
          "electric pairs"))
      "Other"
      (("r" indent-region
         "indent region")
        ("v" sgml-validate
          "validate"
          :color blue)
        ("b" browse-url-of-buffer
          "open browser"
          :color blue)
        ("i" imenu
          "index"
          :color blue)
        ("RET" nil
          "quit"
          :color blue)))))

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
      (list #'cape-file #'cape-dict)))
  :pretty-hydra
  ((:hint nil :title (with-octicon "nf-oct-markdown" "Markdown Mode Control" 1 -0.05) :quit-key "SPC" :color pink)
   ("Insert"
     (("it" markdown-insert-table "table")
       ("ii" markdown-insert-image "image")
       ("il" markdown-insert-link "link")
       ("ic" markdown-insert-gfm-code-block "code block" :color blue)
       ("id" markdown-insert-gfm-checkbox "checkbox"))
    "Preview"
    (("p" impatient-showdown-mode "Preview" :toggle t))
    "Action"
    (("o" markdown-open "Open" :color blue))
    ))
  :custom
  (markdown-command "pandoc -t html5"))
;; org github-esque markdown export
(use-package ox-gfm
  :after org)
;; markdown visualization
(use-package impatient-showdown
  :after (markdown-mode)
  :custom
  (impatient-showdown-flavor 'github))


;; JS/react/angular config
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
          (tsx-ts-mode . prettier-mode)
          (js-ts-mode . prettier-mode)
          (typescript-ts-mode . prettier-mode))
  :custom
  (prettier-mode-sync-config-flag t))
(use-package js-comint
  :defer t
  :hook (inferior-js-mode . inferior-js-mode-hook-setup)
  :init
  (defun inferior-js-mode-hook-setup ()
    (add-hook 'comint-output-filter-functions 'js-comint-process-output))
  :config
  (add-hook 'js-ts-mode-hook
            (lambda ()
              (local-set-key (kbd "\C-c !") 'run-js)
              (local-set-key (kbd "\C-c\C-r") 'js-send-region)
              (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
              (local-set-key (kbd "C-c b") 'js-send-buffer)
              (local-set-key (kbd "C-c C-b") 'js-send-buffer-and-go)))
  )
(use-package js-mode
  :straight nil
  :hook (js-mode . js-ts-mode))
;;angular / typescript setup
(use-package nvm
  :straight (:host github :repo "rejeep/nvm.el")
  :commands (nvm-use-for nvm-use-for-buffer))
(use-package typescript-ts-mode
  :delight " Ts"
  :mode (("\\.ts\\'" . typescript-ts-mode)
          ("\\.tsx\\'" . tsx-ts-mode))
  :hook ((typescript-ts-mode . subword-mode)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PYTHON CONFIG
;; PYTHON VERSION NEEDS TO BE ADJUSTED PER SETUP
;; linter/refractor: black
;; syntax on-the-fly: flycheck
;; style: flake8
;; install black, flake8 ipython, jedi, rope, autopep8, sphinx-doc
(use-package python
  :straight nil
  :delight " Py"
  :bind (:map python-ts-mode-map
          ("s-h" . python-hydra/body))
  :hook ((python-ts-mode . aaronzinhoo--python-setup)
          (inferior-python-mode . corfu-mode))
  :pretty-hydra
  (python-hydra
    (:hint nil :color pink :quit-key "SPC" :title (with-mdicon "nf-md-language_python" "Python Mode" 1 -0.05))
    ("Run"
      (("sh" run-python "Python Shell")
        ("d" pdb "PDB" :color blue)
        ("ss" python-shell-switch-to-shell "Switch to sh" :color blue))
      "Run in Python Shell"
      (("rb" python-shell-send-buffer "Run Buffer")
        ("rf" python-shell-send-file "Run File")
        ("rc" aaronzinhoo--python-shell-send-current-file "Run Current File")
        ("rr" python-shell-send-region "Run Region"))
      "Imports"
      (("if" lsp-organize-imports "Fix Imports")
        ("ia" lsp-auto-execute-action "Add Import"))
      "Formatting"
      (("f" lsp-format-buffer "Format"))
      "Fold"
      (("f" aaronzinhoo--hs-toggle-block
         "toggle block")
        ("c" aaronzinhoo--hs-hide-block
          "close block")
        ("o" aaronzinhoo--hs-show-block
          "open block")
        ("C" hs-hide-all
          "close all")
        ("O" hs-show-all
          "open all")
        ("L" hs-hide-level
          "close level"))
      "Navigation/Editing"
      (("j" combobulate-avy "Jump")
        ("ei" combobulate-python-indent-for-tab-command "Indent"))))
  :preface
  (defun aaronzinhoo--python-version ()
    "Return the primary Python version from the nearest .python-version."
    (when-let* ((file
                  buffer-file-name)
                 (root
                   (locate-dominating-file
                     file
                     ".python-version"))
                 (version-file
                   (expand-file-name
                     ".python-version"
                     root))
                 ((file-readable-p
                    version-file)))
      (with-temp-buffer
        (insert-file-contents
          version-file)

        ;; Return the first nonempty, non-comment line.
        (goto-char
          (point-min))
        (when
          (re-search-forward
            "^[[:blank:]]*\\([^#[:blank:]\n]+\\)"
            nil
            t)
          (match-string-no-properties
            1)))))
  (defun aaronzinhoo--python-activate-environment ()
    "Configure the current buffer for its pyenv Python version."
    (when-let* ((environment
                 (aaronzinhoo--python-version)))
      ;; Environment variables and executable lookup become local to this
      ;; buffer. New subprocesses inherit these values.
      (setq-local
        process-environment
        (copy-sequence
          process-environment))

      (setq-local
        exec-path
        (copy-sequence
          exec-path))

      ;; Ask pyenv to resolve the selected version.
      (setenv
        "PYENV_VERSION"
        environment)

      (let* ((prefix
               (car
                 (process-lines
                   "pyenv"
                   "prefix")))
              (binary-directory
                (expand-file-name
                  "bin"
                  prefix))
              (python
                (expand-file-name
                  "python"
                  binary-directory)))

        (unless
          (file-executable-p python)
          (user-error
            "Python is unavailable for pyenv environment `%s'"
            environment))

        ;; Put this environment ahead of every other Python installation.
        (setq-local
          exec-path
          (cons
            binary-directory
            (delete
              binary-directory
              exec-path)))

        (setenv
          "PATH"
          (mapconcat
            #'identity
            exec-path
            path-separator))

        ;; Ensure the inferior Python process uses this exact interpreter.
        (setq-local
          python-shell-interpreter
          python))))
  (defun aaronzinhoo--python-lsp-setup ()
    "Start BasedPyright and Ruff in the current Python buffer."
    ;; Your project Python/pyenv activation must happen before this
    ;; function runs.
    (require 'lsp-pyright)
    (require 'lsp-ruff)
    (lsp-deferred))
  (defun aaronzinhoo--python-shell-send-current-file ()
    (interactive)
    (python-shell-send-file (buffer-file-name)))
  (defun aaronzinhoo--python-setup ()
    (setq-local python-indent-offset 4
      tab-width 4
      highlight-indentation-offset 4)
    (aaronzinhoo--python-activate-environment)
    (aaronzinhoo--python-lsp-setup))
  :custom
  (python-shell-interpreter "ipython3")
  (python-shell-interpreter-args "--no-color-info --matplotlib=inline --automagic --simple-prompt --pprint")
  (python-shell-completion-native-enable t)
  (python-shell-completion-native-output-timeout 2)
  :init
  (add-to-list 'process-coding-system-alist '("python" . (utf-8 . utf-8))))
(use-package sphinx-doc
  :hook (python-ts-mode . sphinx-doc-mode)
  :custom
  (sphinx-doc-include-types t))

;; Golang Setup
;; export GO111MODULE="on" might be needed
;; need a package if not in GOPATH!
(use-package go-playground
  :commands (go-playground)
  :straight (:type git :host github :repo "grafov/go-playground" :branch "master")
  :config
  (defun my/go-playground-remove-lsp-workspace ()
    (when-let* ((root (lsp-workspace-root))) (lsp-workspace-folders-remove root)))
  (add-hook 'go-playground-pre-rm-hook #'my/go-playground-remove-lsp-workspace))
(use-package go-mod-ts-mode
  :straight nil
  :mode ("\\.mod\\'" . go-mod-ts-mode))
(use-package go-ts-mode
  :straight nil
  :bind (:map go-ts-mode-map
          ("s-h" . go-hydra/body))
  :hook ((go-ts-mode . subword-mode)
          (go-ts-mode . aaronzinhoo--setup-go-mode))
  :preface
  (defun aaronzinhoo--setup-go-mode ()
    (setq-local go-ts-mode-indent-offset 4)
    (setq-local lsp-gopls-staticcheck t)
    (setq-local lsp-gopls-complete-unimported t))
  :pretty-hydra
  (go-hydra
   (:hint nil :color pink :quit-key "SPC" :title (with-mdicon "nf-md-language_go" "Go Mode" 1 -0.05))
    ("Jump"
      (("j" godef-jump "Jump to definition")
        ("b" pop-tag-mark "Jump back"))
      "Fold"
      (("f" aaronzinhoo--hs-toggle-block
         "toggle block")
        ("c" aaronzinhoo--hs-hide-block
          "close block")
        ("o" aaronzinhoo--hs-show-block
          "open block")
        ("C" hs-hide-all
          "close all")
        ("O" hs-show-all
          "open all")
        ("L" hs-hide-level
          "close level"))
      "Refactor"
      (("rf" lsp-format-buffer "Format buffer"))
      "Run"
      (("rr" go-run "Run buffer")
        ("rp" go-playground "Play ground"))
      "Other"
      (("d" godoc "Godoc" :color blue))
      "Test"
      (("tt" go-test-current-test "Test current test")
        ("tf" go-test-current-file "Test current file"))
    ))
  )

;; C++ / C
;; lsp-mode + clangd for debugging
;; configuration: use set(CMAKE_EXPORT_COMPILE_COMMANDS ON) in cmake file
;; cmake-mode + cmake-font-lock for editing cmake files
(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode))
  :hook (cmake-mode . cmake-ts-mode))

;;; Rust
;; Built-in tree-sitter Rust mode.
;; Optional Cargo command menu.
(use-package cargo-mode
  :after rust-ts-mode
  :commands cargo-minor-mode)
(use-package rust-ts-mode
  :straight nil
  :mode ("\\.rs\\'" . rust-ts-mode)
  :preface (defun cargo-run-offline ()
             "Run the current Cargo project in offline mode."
             (interactive)
             (rustic-cargo-run-command "--offline"))
  :hook ((rust-ts-mode . lsp-deferred)
          (rust-ts-mode . cargo-minor-mode))
  :bind (:map rust-ts-mode-map
          ("C-c C-r" . cargo-run-offline)))
;; Load Rustic for its Cargo commands, but do not use rustic-mode.
(use-package rustic
  :after rust-ts-mode
  :commands (rustic-cargo-run-command
              rustic-cargo-run
              rustic-cargo-test
              rustic-cargo-clippy
              rustic-cargo-build))
;;; Java | C++ | C
(use-package groovy-mode
  :defer t)
(use-package conf-javaprop-mode
  :straight nil
  :mode ("\\.properties\\'" . conf-javaprop-mode))
(use-package java-ts-mode
  :straight nil
  :hook ((java-ts-mode . aaronzinhoo--java-setup)
          (java-ts-mode . lsp-deferred)
          (java-ts-mode . lsp-lens-mode)
          (java-ts-mode . subword-mode))
  ;; define the hydra with the mode since the mode-map may not be defined yet
  :bind
  (:map java-ts-mode-map
    ("TAB" . indent-for-tab-command)
    ([tab] . indent-for-tab-command)
    ("s-h" . java-hydra/body))
  :preface
  (defun aaronzinhoo--java-setup ()
    "Configure indentation for the current Java buffer."
    (setq-local c-basic-offset 4)
    (setq-local tab-width 4)
    (setq-local lsp-lens-enable t)
    (lsp-lens-mode 1))
  :pretty-hydra
  (java-hydra
   (:hint nil :color pink :quit-key "SPC" :title (with-mdicon "nf-md-language_java" "Java LSP Mode" 1 -0.05))
    ("Class"
      (("cg" lsp-java-generate-getters-and-setters "Generate [S|G]etters")
        ("co" lsp-java-generate-overrides "Generate Overides")
        ("cu" lsp-java-add-unimplemented-methods "Add Unimplemented Methods")
        ("ct" lsp-java-add-throws "Add Throws"))
      "Fold"
      (("f" aaronzinhoo--hs-toggle-block
         "toggle block")
        ("c" aaronzinhoo--hs-hide-block
          "close block")
        ("o" aaronzinhoo--hs-show-block
          "open block")
        ("C" hs-hide-all
          "close all")
        ("O" hs-show-all
          "open all")
        ("L" hs-hide-level
          "close level"))
      "Imports"
      (("a" lsp-java-add-import "Add")
        ("o" lsp-java-organize-imports "Organize"))
      "Notifications"
      (("n" lsp-java-resolve-actionable-notifications "Resolve Notifications"))
      "Project Management"
      (("ps" lsp-java-spring-initializr "Spring Init" :color blue)
        ("pd" lsp-java-dependency-list "List Dependencies"))
      "Test"
      (("tb" lsp-jt-browser "Test Browser" :color blue)
        ("tl" lsp-jt-lens-mode "Testing Lens Mode" :toggle t)))))

;; protobuf
(use-package protobuf-ts-mode
  :straight (:type git :host github :repo "emacsattic/protobuf-ts-mode" :branch "master")
  :mode (("\\.proto\\'" . protobuf-ts-mode))
  :config
  (add-to-list 'lsp-language-id-configuration
    '(protobuf-ts-mode . "protobuf")))
(use-package flycheck-buf-lint
  :straight t
  :hook ((protobuf-mode protobuf-ts-mode) . (lambda() (flycheck-buf-lint-setup))))

;;; SQL Mode
(use-package sqlformat
  :straight (:type git :host github :repo "purcell/sqlformat" :branch "master")
  :hook (sql-mode . sqlformat-on-save-mode)
  :custom
  (sqlformat-command 'pgformatter))
;;; terraform
(use-package terraform-mode
  :mode (("\\.tf\\'" . terraform-mode)
         ("\\.tfvars\\'" . terraform-mode)
         ("\\.tofu\\'" . terraform-mode))

  :bind (:map terraform-mode-map
              ("s-h" . tofu-hydra/body)
              ("C-c C-d C-w" . aaronzinhoo-opentofu-browse-documentation)
              ("C-c C-d C-c" . aaronzinhoo-opentofu-copy-documentation-url)
              ("C-c C-d C-l" . aaronzinhoo-opentofu-browse-language-documentation)
              ("C-c C-d C-s" . aaronzinhoo-opentofu-browse-registry))
  :hook ((terraform-mode . aaronzinhoo--terraform-enable-treesit)
         (terraform-mode . lsp-deferred))
  :custom
  (terraform-indent-level 2)
  :preface
  ;; treesitter
  (defun aaronzinhoo--terraform-enable-treesit ()
    "Attach an HCL Tree-sitter parser to the current buffer."
    (when
        (treesit-ready-p 'hcl)
      (treesit-parser-create 'hcl)))
  ;; documentation setup
  (defvar aaronzinhoo--opentofu-schema-cache (make-hash-table :test #'equal)
    "Cached OpenTofu provider schemas by working directory.")
  (defun aaronzinhoo--opentofu-block-header (node)
  "Return NODE's block header as a list of strings.

For example:
  (\"resource\" \"aws_s3_bucket\" \"this\")"
  (let ((index 0)
        (count (treesit-node-child-count node t))
        header
        done)
    (while
      (and (< index count)
           (not done))
      (let* ((child
               (treesit-node-child node index t))
             (type
               (treesit-node-type child)))
        (pcase type
          ("block_start"
            (setq done t))

          ("identifier"
            (push
              (treesit-node-text child t)
              header))

          ("string_lit"
            ;; Block labels have surrounding double quotes.
            (push
              (substring
                (treesit-node-text child t)
                1
                -1)
              header))))
      (setq index (1+ index)))
    (nreverse header)))
  (defun aaronzinhoo--opentofu-block-information ()
    "Return (KIND NAME) for the nearest supported block at point.

Use the HCL Tree-sitter parser.  Return nil outside supported
blocks.  Skip nested blocks such as lifecycle and default_tags."
    (unless
        (treesit-ready-p 'hcl t)
      (user-error
       "Install the HCL grammar with M-x treesit-install-language-grammar"))

    ;; Reuses an existing HCL parser when one is already attached.
    (let* ((parser
            (treesit-parser-create 'hcl))
           (position
            (point))
           (node
            (treesit-node-at position parser))
           result)

      (while
          (and node
               (not result))
        (when
            (and
             (equal
              (treesit-node-type node)
              "block")
             ;; A nearby node is not necessarily an enclosing node.
             (<= (treesit-node-start node) position)
             (< position (treesit-node-end node)))

          (let* ((header
                  (aaronzinhoo--opentofu-block-header node))
                 (kind
                  (car header))
                 (name
                  (cadr header)))

            (setq result
                  (pcase kind
                    ((or "resource" "data" "backend" "provider")
                     (when name
                       (list
                        (intern kind)
                        name)))

                    ("required_providers"
                     '(required-providers nil))

                    ("terraform"
                     '(terraform nil))))))

        (setq node
              (treesit-node-parent node)))

      result))
  (defun aaronzinhoo--opentofu-schema-signature (directory)
    "Return a provider-cache signature for DIRECTORY."
    (mapcar
     (lambda (path)
       (when-let* ((attributes
                   (file-attributes path)))
         (file-attribute-modification-time
          attributes)))
     (list
      (expand-file-name
       ".terraform.lock.hcl"
       directory)
      (expand-file-name
       ".terraform"
       directory))))
  (defun aaronzinhoo--opentofu-read-provider-schemas
      (directory)
    "Read OpenTofu provider schemas from DIRECTORY."
    (let ((executable
           (or
            (executable-find "tofu")
            (user-error
             "The tofu executable is unavailable")))
          (default-directory directory)
          (error-file
           (make-temp-file
            "tofu-schema-errors-")))
      (unwind-protect
          (with-temp-buffer
            (let ((status
                   (process-file
                    executable
                    nil
                    (list t error-file)
                    nil
                    "providers"
                    "schema"
                    "-json")))
              (unless
                  (zerop status)
                (let ((error-message
                       (with-temp-buffer
                         (insert-file-contents
                          error-file)
                         (string-trim
                          (buffer-string)))))
                  (user-error
                   "Could not read OpenTofu schemas: %s"
                   (if
                       (string-empty-p error-message)
                       "run tofu init in the selected root module"
                     error-message))))

              (goto-char
               (point-min))

              (let* ((document
                      (json-parse-buffer
                       :object-type 'hash-table
                       :array-type 'list
                       :null-object nil
                       :false-object nil))
                     (provider-schemas
                      (gethash
                       "provider_schemas"
                       document)))

                (unless
                    (hash-table-p provider-schemas)
                  (user-error
                   "OpenTofu returned no provider schemas"))
                provider-schemas)))
        (delete-file error-file))))
  (defun aaronzinhoo--opentofu-provider-schemas ()
    "Return provider schemas for the selected OpenTofu root module."
    (let* ((directory
            (file-name-as-directory
             (aaronzinhoo--opentofu-working-directory)))
           (signature
            (aaronzinhoo--opentofu-schema-signature
             directory))
           (cached
            (gethash
             directory
             aaronzinhoo--opentofu-schema-cache)))
      (if
          (and
           cached
           (equal
            signature
            (car cached)))
          (cdr cached)
        (let ((schemas
               (aaronzinhoo--opentofu-read-provider-schemas
                directory)))
          (puthash
           directory
           (cons signature schemas)
           aaronzinhoo--opentofu-schema-cache)
          schemas))))
  (defun aaronzinhoo--opentofu-provider-address
      (kind type)
    "Return the provider registry address owning TYPE of KIND."
    (let* ((provider-schemas
            (aaronzinhoo--opentofu-provider-schemas))
           (schema-key
            (pcase kind
              ('resource
               "resource_schemas")
              ('data
               "data_source_schemas")))
           address)
      (maphash
       (lambda (provider-address provider-schema)
         (when-let* ((schemas
                     (gethash
                      schema-key
                      provider-schema)))
           (when
               (gethash type schemas)
             (setq address
                   provider-address))))
       provider-schemas)
      (or
       address
       (user-error
        "No initialized provider owns `%s'"
        type))))
  (defun aaronzinhoo--opentofu-documentation-name
      (type)
    "Return the registry documentation name for TYPE."
    (if-let* ((separator
              (string-match "_" type)))
        (substring
         type
         (1+ separator))
      type))
  (defun aaronzinhoo--opentofu-documentation-url ()
    "Return the documentation URL for the OpenTofu construct at point."
    (pcase
        (aaronzinhoo--opentofu-block-information)

      (`(backend ,name)
       (format
        "https://opentofu.org/docs/language/settings/backends/%s/"
        name))

      (`(provider ,_)
       "https://opentofu.org/docs/language/providers/configuration/")

      (`(required-providers nil)
       "https://opentofu.org/docs/language/providers/requirements/")

      (`(terraform nil)
       "https://opentofu.org/docs/language/settings/")

      (`(,kind ,type)
       (unless
           (memq kind '(resource data))
         (user-error
          "No documentation mapping for `%s'"
          kind))
       (let* ((provider-address
               (aaronzinhoo--opentofu-provider-address
                kind
                type))
              (source-parts
               (split-string provider-address "/" t)))
         ;; Accept namespace/provider or hostname/namespace/provider.
         (unless
             (memq (length source-parts) '(2 3))
           (user-error
            "Invalid provider address: %s"
            provider-address))
         (let* ((provider-parts
                 (last source-parts 2))
                (namespace
                 (car provider-parts))
                (provider
                 (cadr provider-parts))
                (section
                 (if
                     (eq kind 'resource)
                     "resources"
                   "datasources"))
                (documentation-name
                 (aaronzinhoo--opentofu-documentation-name
                  type)))
           (format
            (concat
             "https://search.opentofu.org/provider/"
             "%s/%s/latest/docs/%s/%s")
            namespace
            provider
            section
            documentation-name))))

      (_
       (user-error
        "No documented OpenTofu construct surrounds point"))))
  (defun aaronzinhoo-opentofu-browse-documentation ()
    "Open OpenTofu documentation for the block at point."
    (interactive)
    (browse-url
     (aaronzinhoo--opentofu-documentation-url)))

  (defun aaronzinhoo-opentofu-copy-documentation-url ()
    "Copy the OpenTofu documentation URL for the block at point."
    (interactive)
    (let ((url
           (aaronzinhoo--opentofu-documentation-url)))
      (kill-new url)
      (message
       "Copied OpenTofu documentation URL: %s"
       url)))

  (defun aaronzinhoo-opentofu-refresh-documentation-cache ()
    "Clear cached OpenTofu provider schemas."
    (interactive)
    (clrhash
     aaronzinhoo--opentofu-schema-cache)
    (message
     "Cleared OpenTofu provider-schema cache"))

  (defun aaronzinhoo-opentofu-browse-language-documentation ()
    "Open the OpenTofu language documentation."
    (interactive)
    (browse-url
     "https://opentofu.org/docs/language/"))

  (defun aaronzinhoo-opentofu-browse-registry ()
    "Open the OpenTofu Registry."
    (interactive)
    (browse-url
     "https://search.opentofu.org/"))
  ;; tofu commands for hydra
  (defvar aaronzinhoo--opentofu-lock-filename ".terraform.lock.hcl"
  "Name of lock file generated by tofu and terraform.")
  (defvar aaronzinhoo--opentofu-schema-cache (make-hash-table :test #'equal)
  "Cached OpenTofu provider schemas by working directory.")
  (defvar-local aaronzinhoo--opentofu-working-directory-override nil
    "OpenTofu working directory for the current buffer.")
  (defun aaronzinhoo--opentofu-working-directory ()
    "Return the OpenTofu working directory for the current buffer."
    (let ((starting-directory
           (or
            (and buffer-file-name
                 (file-name-directory buffer-file-name))
            default-directory)))
      (file-name-as-directory
       (or
        ;; An explicit buffer-local selection takes precedence.
        aaronzinhoo--opentofu-working-directory-override

        ;; Find the nearest initialized OpenTofu directory.
        (locate-dominating-file
         starting-directory
         aaronzinhoo--opentofu-lock-filename)

        ;; Support an explicit project-root marker.
        (locate-dominating-file
         starting-directory
         ".tofu-root")

        ;; Fall back to the current Emacs project.
        (when-let ((project
                    (project-current nil starting-directory)))
          (project-root project))

        starting-directory))))
  (defun aaronzinhoo--opentofu-project-directory ()
    "Return the current project root."
    (if-let* ((project
               (project-current nil)))
      (project-root project)
      default-directory))
  (defun aaronzinhoo--opentofu-select-working-directory ()
    "Select a root module for subsequent OpenTofu commands."
    (interactive)
    (setq-local
      aaronzinhoo--opentofu-working-directory-override
      (file-name-as-directory
        (read-directory-name
          "OpenTofu root module: "
          (or
            (and
              buffer-file-name
              (file-name-directory buffer-file-name))
            (aaronzinhoo--opentofu-project-directory))
          nil
          t)))
    (message
      "OpenTofu working directory: %s"
      (abbreviate-file-name
        aaronzinhoo--opentofu-working-directory-override)))
  (defun aaronzinhoo--opentofu-use-project-root-directory ()
    "Use the project root for subsequent OpenTofu commands."
    (interactive)
    (setq-local
      aaronzinhoo--opentofu-working-directory-override
      nil)
    (message
      "OpenTofu working directory: %s"
      (abbreviate-file-name
        (aaronzinhoo--opentofu-project-directory))))
  (defun aaronzinhoo--opentofu-show-working-directory ()
    "Display the current OpenTofu working directory."
    (interactive)
    (message
      "OpenTofu working directory: %s"
      (abbreviate-file-name
        (aaronzinhoo--opentofu-working-directory))))
  (defun aaronzinhoo--tofu-command (arguments)
    "Build an OpenTofu command using ARGUMENTS."
    (let ((executable
            (or
              (executable-find "tofu")
              (user-error
                "The tofu executable is unavailable"))))
      (string-join
        (cons
          (shell-quote-argument executable)
          arguments)
        " ")))
  (defun aaronzinhoo--opentofu-run
    (arguments &optional edit interactive)
    "Run OpenTofu with ARGUMENTS.

When EDIT is non-nil, allow the command to be edited first.
When INTERACTIVE is non-nil, use a Comint buffer so the command
can accept input."
    (let* ((default-directory
             (aaronzinhoo--opentofu-working-directory))
           (initial-command
             (aaronzinhoo--opentofu-command arguments))
           (command
             (if edit
               (read-shell-command
                 "OpenTofu command: "
                 initial-command)
               initial-command))
           (buffer-name
             (lambda (_mode)
               (format
                 "*tofu:%s*"
                 (file-name-nondirectory
                   (directory-file-name
                     default-directory))))))
      (compilation-start
        command
        (if interactive
          'comint-mode
          'compilation-mode)
        buffer-name)))
  (defun aaronzinhoo--opentofu-apply ()
    "Run `tofu apply' after confirmation."
    (interactive)
    (when
      (yes-or-no-p
        (format
          "Run tofu apply in %s? "
          (abbreviate-file-name
            (aaronzinhoo--opentofu-working-directory))))
      (aaronzinhoo--opentofu-run
        '("apply")
        t
        t)))
  (defun aaronzinhoo--opentofu-destroy ()
    "Run `tofu destroy' after confirmation."
    (interactive)
    (when
      (yes-or-no-p
        (format
          "Destroy resources managed from %s? "
          (abbreviate-file-name
            (aaronzinhoo--opentofu-working-directory))))
      (aaronzinhoo--opentofu-run
        '("destroy")
        t
        t)))
  (defun aaronzinhoo--opentofu-format-project ()
    "Recursively format the complete OpenTofu project."
    (interactive)
    (let ((aaronzinhoo--opentofu-working-directory
            (aaronzinhoo--opentofu-project-directory)))
      (aaronzinhoo--opentofu-run
        '("fmt" "-recursive"))))
  :pretty-hydra
  (tofu-hydra
   (:title "OpenTofu"
           :hint nil
           :quit-key "q"
           :color amaranth)

   ("Lifecycle"
    (("i"
      (aaronzinhoo--opentofu-run
       '("init")
       t)
      "init")
     ("v"
      (aaronzinhoo--opentofu-run
       '("validate"))
      "validate")
     ("p"
      (aaronzinhoo--opentofu-run
       '("plan")
       t)
      "plan")
     ("a"
      aaronzinhoo--opentofu-apply
      "apply"
      :color blue)
     ("t"
      (aaronzinhoo--opentofu-run
       '("test")
       t)
      "test"))
    "Documentation"
    (("b"
      aaronzinhoo-opentofu-browse-documentation
      "block docs"
      :color blue)
     ("y"
      aaronzinhoo-opentofu-copy-documentation-url
      "copy URL")

     ("l"
      aaronzinhoo-opentofu-browse-language-documentation
      "language docs"
      :color blue)
     ("S"
      aaronzinhoo-opentofu-browse-registry
      "registry"
      :color blue)
     ("U"
      aaronzinhoo-opentofu-refresh-documentation-cache
      "refresh schemas"))
    "Formatting"
    (("f"
      lsp-format-buffer
      "buffer via LSP")
     ("F"
      (aaronzinhoo--opentofu-run
       '("fmt"))
      "root module")
     ("R"
      aaronzinhoo--opentofu-format-project
      "recursive project"))

    "Inspect"
    (("o"
      (aaronzinhoo--opentofu-run
       '("output")
       t)
      "outputs")
     ("s"
      (aaronzinhoo--opentofu-run
       '("state" "list"))
      "state list")
     ("w"
      (aaronzinhoo--opentofu-run
       '("workspace" "list"))
      "workspaces")
     ("P"
      (aaronzinhoo--opentofu-run
       '("providers"))
      "providers")
     ("g"
      (aaronzinhoo--opentofu-run
       '("graph"))
      "graph")
     ("c"
      (aaronzinhoo--opentofu-run
       '("console")
       nil
       t)
      "console"
      :color blue))

    "Module"
    (("m"
      aaronzinhoo--opentofu-select-working-directory
      "select module")
     ("M"
      aaronzinhoo--opentofu-use-project-root-directory
      "project root")
     ("?"
      aaronzinhoo--opentofu-show-working-directory
      "show directory"))

    "LSP"
    (("d"
      lsp-find-definition
      "definition")
     ("r"
      lsp-find-references
      "references")
     ("n"
      lsp-rename
      "rename")
     ("e"
      flycheck-list-errors
      "errors")
     ("l"
      lsp-workspace-restart
      "restart server"))

    "Danger"
    (("D"
      aaronzinhoo--opentofu-destroy
      "destroy"
      :color blue)))))
;;; Emacs Lisp Mode
(use-package elisp-mode
  :straight nil
  :hook ((emacs-lisp-mode . aaronzinhoo--setup-elisp-mode)
          (lisp-interaction-mode . aaronzinhoo--setup-elisp-mode))
  :preface
  (defun aaronzinhoo--setup-elisp-mode ()
    "Configure editing and completion for Emacs Lisp."
    ;; Preserve Emacs Lisp's semantic indentation rules.
    (setq-local
      lisp-indent-offset nil
      tab-width 2)

    ;; `elisp-completion-at-point' is installed by Emacs Lisp mode.
    ;; Append fallback CAPFs without replacing it.
    (aaronzinhoo--append-capfs
     #'cape-file
     #'cape-dabbrev)))
(use-package elisp-autofmt
  :commands (elisp-autofmt-mode elisp-autofmt-buffer)
  :hook (emacs-lisp-mode . elisp-autofmt-mode))

;;;; Customize
(when
  (file-readable-p custom-file)
  (load custom-file nil 'nomessage))

(message "Done loading packages")

;;; init.el ends here

;; Local Variables:
;; jinx-local-words: "config"
;; End:
