;;; init-straight.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-straight.el
;; Description: Initialization straight
;; Compatibility: emacs-version >= 27
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Emacs27 introduces init-straight.el which inits straight package manager.
;;

;;; Code:
(message "Loading straight package manager")
;; dont check all my straight repos for changes. Really slows down speed
;; keep eye on https://github.com/raxod502/straight.el/pull/694#issuecomment-805197632 for updates on watcher
(setq straight-check-for-modifications '(find-when-checking))
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
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
(message "Done loading straight package manager")

(provide 'init-straight)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-straight.el ends here
