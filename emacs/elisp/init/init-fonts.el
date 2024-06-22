;;; package --- Summary
;;; Commentary:
;;;init-fonts.el --- Emacs configuration

;;; Code:
(message "Loading fonts")
(add-to-list 'default-frame-alist '(font . "-SRC-Hack-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1"))
;; Set the variable pitch face
;;(set-face-attribute 'variable-pitch nil :font "Cantarell" :weight 'regular)
;; UTF-8 as default encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
(message "Done loading fonts")
(provide 'init-fonts)
;;; init-fonts ends here
