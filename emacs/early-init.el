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

;; Setup variables
(defconst aaronzinhoo-emacs-directory (expand-file-name "~/.config/emacs"))
(defconst user-emacs-directory aaronzinhoo-emacs-directory)
(defconst user-full-name "Aaron Gonzales")
(defconst user-init-dir "~/.emacs.d/")
(defconst user-init-file "~/.emacs.d/init.el")
(defconst user-home-directory (getenv "HOME"))
(defconst user-mail-address "aaronzinho@g.ucla.edu")
(defconst user-init-dir-fullpath (file-truename user-init-dir))
(defconst backup-dir (concat user-emacs-directory "/backups"))
(defconst autosave-dir (concat user-emacs-directory "/autosave"))
(defconst calendar-latitude 33.916403)
(defconst calendar-longitude -118.352575)
(defconst node-home-folder (file-name-directory (getenv "NVM_BIN")) "Path to currently used nvm node version with trailing slash.")
(defconst nvm-home-folder (getenv "NVM_DIR") "Path to currently used nvm node version with trailing slash.")
(defconst pyenv-root-folder (getenv "PYENV_ROOT") "Path to pyenv root folder without trailing slash.")
(defconst my/wsl (not (null (string-match "Linux.*Microsoft" (shell-command-to-string "uname -a")))))
(setq package-user-dir (concat aaronzinhoo-emacs-directory "/elpa"))
(setq package-gnupghome-dir (concat aaronzinhoo-emacs-directory "/elpa/gnupg"))
(setq-default custom-theme-directory (concat aaronzinhoo-emacs-directory "/themes"))

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Disable Emacs 27's automatic package.el initialization before the init.el
;; file is loaded. I use straight.el instead of package.el.
(setq package-enable-at-startup nil)
;; Emacs comes with several built-in packages, such as Org-mode, that are
;; essential for many users. However, these built-in packages are often not the
;; latest versions available. Ensure that your built-in packages are always up
;; to date with:
(setq package-install-upgrade-built-in t)

(setq custom-file
      (concat
       (expand-file-name
        (file-name-directory (or load-file-name buffer-file-name)))
       "custom.el"))

;; UnsetFNHA
(defvar file-name-handler-alist-orginal file-name-handler-alist)
(setq file-name-handler-alist nil)
;; -UnsetFNHA

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
;; Disable unneeded UI modes
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars . 0) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(setq tool-bar-mode nil
      scroll-bar-mode nil)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))
;; Disable GUIs because theyr are inconsistent across systems, desktop
;; environments, and themes, and they don't match the look of Emacs.
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(unless (memq window-system '(mac ns))
  ;; (menu-bar-mode -1)
  (setq menu-bar-mode nil))

(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Always load newer code
(setq load-prefer-newer t)

;; AfterInitHook
(add-hook
 'after-init-hook
 (lambda ()
   (setq file-name-handler-alist file-name-handler-alist-orginal)))
;; -AfterInitHook

;; Allow for shorter responses: "y" for yes and "n" for no.
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add #'yes-or-no-p :override #'y-or-n-p))
(defalias #'view-hello-file #'ignore) ; Never show the hello file


;;; Native compilation and Byte compilation
;; Suppress compiler warnings and don't inundate users with their popups.
(setq
  native-comp-jit-compilation t
  native-comp-async-report-warnings-errors 'silent
  native-comp-warning-on-missing-source 'nil
  package-native-compile t)

(setenv "LSP_USE_PLISTS" "true")

;; Set-language-environment sets default-input-method, which is unwanted.
;; UTF-8 as default encoding
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-input-method nil)
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; Set of default values
(setq
  ;; A second, case-insensitive pass over `auto-mode-alist' is time wasted.
  ;; No second pass of case-insensitive search over auto-mode-alist.
  auto-mode-case-fold nil
  auto-save-default t
  ;;cursor options
  blink-cursor-blinks -1
  delete-pair-blink-delay 0
  message-log-max 16384
  ;; Disable warnings from the legacy advice API. They aren't useful.
  ad-redefinition-action 'accept
  ;; disable lockfiles since they cause some trouble
  create-lockfiles nil
  select-enable-clipboard t
  ;; Reduce *Message* noise at startup. An empty scratch buffer (or the
  ;; dashboard) is more than enough, and faster to display.
  inhibit-startup-screen t
  initial-scratch-message nil
  backup-directory-alist `((".*" . ,backup-dir))
  byte-compile-warnings nil
  byte-compile-verbose nil
  make-backup-files t
  backup-by-copying t ;; safest method to backup
  delete-old-versions t ;; delete excess backups
  delete-by-moving-to-trash t
  kept-old-versions 0
  kept-new-versions 10
  ring-bell-function 'ignore)

;; setq will only set for local-buffer so must use setq-default
;; for variables that are not buffer specific
(setq-default
 indent-tabs-mode nil
 tab-width 4
 tab-stop-list (number-sequence 4 120 4))

;; delete whitespace always... can use whitespace-mode to make efficient
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; let Emacs know youre a pro el-oh-el
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Suppress the vanilla startup screen completely. We've disabled it with
;; `inhibit-startup-screen', but it would still initialize anyway.
(advice-add #'display-startup-screen :override #'ignore)

(provide 'early-init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
