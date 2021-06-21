;;; init-defaults.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-defaults.el
;; Description: Initialization defaultss
;; Compatibility: emacs-version >= 27
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Emacs27 introduces init-defaults.el which inits all default variable values for user.
;;

;;; Code:
(message "Loading defaults")
(setq message-log-max 16384
      auto-window-vscroll nil
      scroll-margin 4
      ad-redefinition-action 'accept
      create-lockfiles nil
      select-enable-clipboard t
      inhibit-startup-screen t
      backup-directory-alist `((".*" . ,backup-dir))
      byte-compile-warnings '(cl-functions)
      make-backup-files t
      backup-by-copying t ;; safest method to backup
      delete-old-versions t ;; delete excess backups
      delete-by-moving-to-trash t
      kept-old-versions 0
      kept-new-versions 10
      auto-save-default t
      blink-cursor-blinks -1
      ring-bell-function 'ignore)
;; setq will only set for local-buffer so must use setq-default
(setq-default
 indent-tabs-mode nil
 tab-width 4
 tab-stop-list (number-sequence 4 120 4)
 scroll-preserve-screen-position t
 scroll-conservatively 10000)

;; delete whitespace always... can use whitespace-mode to make efficient
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; let Emacs know youre a pro el-oh-el
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; load custom faces & vars here
(when (file-exists-p "~/.emacs.d/custom.el") (load "~/.emacs.d/custom.el"))

(message "Done loading defaults")

(provide 'init-defaults)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-defaults.el ends here
