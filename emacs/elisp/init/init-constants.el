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
(defconst user-emacs-directory "~/.config/emacs")
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
(message "Done loading constants")

(provide 'init-constants)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-constants.el ends here
