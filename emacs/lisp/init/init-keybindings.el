;;; package --- Summary
;;; Commentary:

;;; Code:

;; Helpful Defualt keys
;; C-h k <key> -> describe what key is binded to
;; M-DEL del backward one word
;; C-c ' edit code in buffer
;; C-c C-c run org code block
(message "Loading keybindings")
(define-key key-translation-map (kbd "<escape>") (kbd "C-c"))
(global-set-key (kbd "M-h") 'backward-char)
(global-set-key (kbd "M-j") 'next-line)
(global-set-key (kbd "M-k") 'previous-line)
(global-set-key (kbd "M-l") 'forward-char)
(global-set-key (kbd "M-q") 'yank)
(global-set-key (kbd "M-4") 'pop-local-mark-ring)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)
(global-set-key (kbd "C-<") 'previous-buffer)
(global-set-key (kbd "C->") 'next-buffer)
(global-set-key (kbd "M-[") 'backward-up-list)
(global-set-key (kbd "M-]") 'up-list)
;; delete pair of items
(global-set-key (kbd "C-c C-p") 'delete-pair)
;; this is your old m-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
;; need this otherwise on windows M-<tab> (changing windows)
;; will activate scroll-lock
(global-set-key (kbd "<Scroll_Lock>") 'ignore)
(message "Done loading keybindings")

(provide 'init-keybindings)
;;; init-keybindings ends here
