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

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Disable Emacs 27's automatic package.el initialization before the init.el
;; file is loaded. I use straight.el instead of package.el.
(setq package-enable-at-startup nil)

;; UnsetFNHA
(defvar file-name-handler-alist-orginal file-name-handler-alist)
(setq file-name-handler-alist nil)
;; -UnsetFNHA

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(tooltip-mode -1)
(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Always load newer code
(setq load-prefer-newer t)
;; load init packages recursively
(let ((default-directory  (expand-file-name (file-name-directory (or load-file-name buffer-file-name)))))
  ;; setup auto-compile
  (add-to-list 'load-path (concat default-directory "deps/auto-compile/auto-compile.el"))
  (require 'auto-compile (concat default-directory "deps/auto-compile/auto-compile.el"))
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;; I know what the scratch buffer is for ... or do I?
(setq initial-scratch-message "")

;; AfterInitHook
(add-hook 'after-init-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-orginal)
            ))
;; -AfterInitHook
(defalias 'yes-or-no-p 'y-or-n-p)

(defconst user-emacs-directory (expand-file-name "~/.config/emacs"))

(setq native-comp-jit-compilation t)
(setq native-comp-async-report-warnings-errors nil)

(provide 'early-init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
