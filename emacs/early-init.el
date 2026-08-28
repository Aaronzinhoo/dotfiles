;;; early-init.el --- early bird  -*- lexical-binding: t no-byte-compile: t -*-
;;
;; Filename: early-init.el
;; Description: Early initialization
;; Compatibility: emacs-version >= 27
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Emacs27 introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;
;;; Code:

;;; early-init.el --- Early initialization -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:

;; Configuration evaluated before package and graphical initialization.

;;; Code:

;;;; Directories

;; `user-emacs-directory' remains ~/.emacs.d/, which is symlinked to
;; the version-controlled dotfiles/emacs directory.

(defconst aaronzinhoo-emacs-generated-directory
  (file-name-as-directory
    (expand-file-name
      "emacs/"
      (or
        (getenv "XDG_CONFIG_HOME")
        "~/.config/")))
  "Directory containing generated Emacs data.")

(defconst aaronzinhoo-emacs-backup-directory
  (expand-file-name
    "backups/"
    aaronzinhoo-emacs-generated-directory))

(defconst aaronzinhoo-emacs-auto-save-directory
  (expand-file-name
    "auto-save/"
    aaronzinhoo-emacs-generated-directory))
;;;; Package-generated directories

;; no-littering reads these variables when it loads.
(setq no-littering-etc-directory
  (expand-file-name
    "etc/"
    aaronzinhoo-emacs-generated-directory))
(setq no-littering-var-directory
  (expand-file-name
    "var/"
    aaronzinhoo-emacs-generated-directory))

;; Built-in Tree-sitter installs grammars into the first directory in
;; `treesit-extra-load-path'.
(defconst aaronzinhoo-emacs-treesit-directory
  (expand-file-name
    "tree-sitter/"
    aaronzinhoo-emacs-generated-directory)
  "Directory containing compiled Tree-sitter grammars.")

(dolist (directory
          (list
            aaronzinhoo-emacs-generated-directory
            aaronzinhoo-emacs-backup-directory
            aaronzinhoo-emacs-auto-save-directory
            aaronzinhoo-emacs-treesit-directory))
  (make-directory directory t))

;; Keep Customize output in the version-controlled configuration.
(setq custom-file
  (expand-file-name
    "custom.el"
    user-emacs-directory))

;; Generated package data.
(setq package-user-dir
  (expand-file-name
    "elpa/"
    aaronzinhoo-emacs-generated-directory))

(setq package-gnupghome-dir
  (expand-file-name
    "elpa/gnupg/"
    aaronzinhoo-emacs-generated-directory))

;; This must be set before straight.el is bootstrapped.
(setq straight-base-dir
  aaronzinhoo-emacs-generated-directory)

;; Keep manually maintained themes in the repository.
(setq custom-theme-directory
  (expand-file-name
    "themes/"
    user-emacs-directory))

;; Keep native-compilation output outside the repository.
(when
  (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
    (expand-file-name
      "eln-cache/"
      aaronzinhoo-emacs-generated-directory)))

;; Backups and auto-save data.
(setq backup-directory-alist
  `(("." . ,aaronzinhoo-emacs-backup-directory)))

(setq auto-save-file-name-transforms
  `((".*" ,aaronzinhoo-emacs-auto-save-directory t)))

(setq auto-save-list-file-prefix
  (expand-file-name
    ".saves-"
    aaronzinhoo-emacs-auto-save-directory))

;; Finder-launched Emacs instances can otherwise start in /Applications.
(let ((home-directory
        (file-name-as-directory
          (expand-file-name "~"))))
  (setq default-directory home-directory)
  (setq-default default-directory home-directory))

;;;; Personal

;;;; Package initialization

;; straight.el manages third-party packages.
(setq package-enable-at-startup nil)


;;;; Startup performance

;; Temporarily reduce garbage collection during startup.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(defun aaronzinhoo--restore-garbage-collection ()
  "Restore reasonable garbage-collection settings after startup."
  (setq gc-cons-threshold
    (* 32 1024 1024))
  (setq gc-cons-percentage 0.1))

(add-hook
  'after-init-hook
  #'aaronzinhoo--restore-garbage-collection)

;; Increase the amount read from subprocesses in one operation.
(setq read-process-output-max
  (* 1024 1024))

;;;; Graphical startup

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t
      inhibit-startup-screen t
      initial-scratch-message nil
      use-dialog-box nil
      use-file-dialog nil)

(dolist (parameter
          '((menu-bar-lines . 0)
             (tool-bar-lines . 0)
             (vertical-scroll-bars . 0)
             (horizontal-scroll-bars . 0)
             (font . "Hack Nerd Font-15")
             (fullscreen . maximized)))
  (add-to-list
    'default-frame-alist
    parameter))

(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))

;;;; General startup behavior

(setq load-prefer-newer t
      auto-mode-case-fold nil
      message-log-max 16384
      ring-bell-function #'ignore)

(if
  (boundp 'use-short-answers)
  (setq use-short-answers t)
  (advice-add
    #'yes-or-no-p
    :override
    #'y-or-n-p))

;;;; macOS

(when
  (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-right-command-modifier 'control
        mac-option-modifier 'super))

;;;; LSP serialization
(setenv "LSP_USE_PLISTS" "true")

(provide 'early-init)
;;; early-init.el ends here
