;;; package --- Summary
;;; Commentary:

;;; Code:
(message "Loading utils")
(defcustom ccm-vpos-init '(round (window-text-height) 2)
  "This is the screen line position where the cursor initially stays."
  :group 'centered-cursor
  :tag "Vertical cursor position"
  :type '(choice (const :tag "Center" (round (window-text-height) 2))
                 (const :tag "Golden ratio" (round (* 21 (window-text-height)) 34))
                 (integer :tag "Lines from top" :value 10)
                 (const :tag "2 Lines above center" (- (round (window-text-height) 2) 2))))

(defun pop-local-mark-ring ()
  "Move cursor to last mark position of current buffer.
Call this repeatedly will cycle all positions in `mark-ring'.
URL `http://ergoemacs.org/emacs/emacs_jump_to_previous_position.html'
  Version 2016-04-04"
  (interactive)
  (set-mark-command t))

(defun split-and-follow-horizontally ()
  "Split window horizontally and follow with the previous buffer open."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1)
  (next-buffer))

(defun split-and-follow-vertically ()
  "Split window vertically and follow with the previous buffer open."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1)
  (next-buffer))

(defun aaronzinhoo-frame-recenter (&optional frame)
  "Center FRAME on the screen.
FRAME can be a frame name, a terminal name, or a frame.
If FRAME is omitted or nil, use currently selected frame."
  (interactive)
  (unless (eq 'maximised (frame-parameter nil 'fullscreen))
    (let* ((frame (or (and (boundp 'frame) frame) (selected-frame)))
           (frame-w (frame-pixel-width frame))
           (frame-h (frame-pixel-height frame))
           ;; frame-monitor-workarea returns (x y width height) for the monitor
           (monitor-w (nth 2 (frame-monitor-workarea frame)))
           (monitor-h (nth 3 (frame-monitor-workarea frame)))
           (center (list (/ (- monitor-w frame-w) 2)
                         (/ (- monitor-h frame-h) 2))))
      (apply 'set-frame-position (flatten-list (list frame center))))))



(defun aaronzinhoo-safe-load-custom-file (file-path)
  "Load FILE-PATH if it exists otherwise do nothing."
  (when (file-exists-p file-path) (load file-path)))

;; we will call `blink-matching-open` ourselves...
(remove-hook 'post-self-insert-hook #'blink-paren-post-self-insert-function)
;; center and maximize frame at start
(add-hook 'after-init-hook #'aaronzinhoo-frame-recenter)
(add-hook 'after-make-frame-functions #'aaronzinhoo-frame-recenter)
(toggle-frame-maximized)
(message "Done loading utils")

(provide 'init-utils)
;;; init-utils.el ends here
