;;; package --- Summary
;;; Commentary:
;;; Code:
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "M-i") 'previous-line)
(global-set-key (kbd "M-j") 'backward-char)
(global-set-key (kbd "M-k") 'next-line)
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

(provide 'aaronzinhoo-custom-keybindings)
;;; aaronzinhoo-custom-keybindings ends here
