(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.config/emacs/var/bmkp/current-bookmark.el")
 '(company-dabbrev-ignore-case nil)
 '(doom-modeline-check-simple-format t nil nil "Customized with use-package doom-modeline")
 '(package-selected-packages
   '(keychain-environment company company-eclim go-mode lsp-ui
                          frame-local pyenv-mode-auto pyenv-mode rg
                          delight blacken powerline powermo
                          company-posframe company-postframe
                          company-box simpleclip
                          all-the-icons-ivy-rich tide flycheck
                          exec-path-from-shell company-web skewer-mode
                          simple-httpd js2 prettier-js rjsx
                          json-snatcher json-reformat js-comint
                          web-mode emmet-mode add-node-modules-path
                          yaml-mode lsp-mode dired-recent diredfl
                          ivy-rich all-the-icons beginend
                          default-text-scale company-prescient
                          ivy-prescient prescient benchmark-init flx
                          company-quickhelp-terminal crux
                          company-quickhelp counsel ace-jump
                          ace-jump-mode diminish auto-package-update
                          electric-pair-mode moe-theme-switcher
                          electric-pair ssh-agency jedi moe-theme
                          bind-map rjsx-mode ag company-tern
                          impatient-mode company-jedi smex
                          idle-highlight-in-visible-buffers-mode
                          idle-highlight-mode magit async git-commit
                          list-packages-ext use-package image+
                          gnu-elpa-keyring-update magithub pylint
                          python-black multiple-cursors material-theme
                          elpy better-defaults python))
 '(package-vc-selected-packages
   '((sqlformat :url "https://github.com/purcell/sqlformat" :branch
                "master")))
 '(safe-local-variable-values '((elisp-lint-indent-specs (when-let . 1)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dired-subtree-depth-1-face ((t unspecified)))
 '(dired-subtree-depth-2-face ((t unspecified)))
 '(dired-subtree-depth-3-face ((t unspecified)))
 '(git-gutter:added ((t (:background unspecified :foreground "limegreen"))))
 '(git-gutter:deleted ((t (:background unspecified))))
 '(git-gutter:modified ((t (:background unspecified :foreground "dodgerblue" :weight bold))))
 '(ivy-minibuffer-match-face-2 ((t (:background "steel blue" :foreground "#eeeeee" :weight bold))))
 '(swiper-background-match-face-2 ((t (:background "steel blue" :foreground "gainsboro"))))
 '(swiper-match-face-1 ((t (:background "light cyan" :foreground "dim gray" :weight bold))))
 '(swiper-match-face-2 ((t (:background "light coral" :foreground "white smoke" :weight bold)))))
(provide 'custom)
;;; custom.el ends here
