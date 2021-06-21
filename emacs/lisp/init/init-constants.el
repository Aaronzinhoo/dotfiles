;;; init-constant.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-constant.el
;; Description: Initialization constants
;; Compatibility: emacs-version >= 27
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Emacs27 introduces init-constant.el which inits all constants needed for packages.
;;
;;; Code:
(message "Loading constants")
(defconst user-full-name "Aaron Gonzales")
(defconst user-init-dir "~/.emacs.d/")
(defconst user-init-file "~/.emacs.d/init.el")
(defconst user-emacs-directory "~/.config/emacs") ;; where the trash files go
(defconst home-directory (expand-file-name "~/.config/emacs"))
(defconst backup-dir (concat home-directory "/backups"))
(defconst autosave-dir (concat home-directory "/autosave"))
(defconst calendar-latitude 33.916403)
(defconst calendar-longitude -118.352575)
(defconst my/wsl (not (null (string-match "Linux.*Microsoft" (shell-command-to-string "uname -a")))))
(message "Done loading constants")

(provide 'init-constants)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-constants.el ends here
