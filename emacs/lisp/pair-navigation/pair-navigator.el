;;; pair-navigator.el --- -*- lexical-binding: t -*-
;;
;; Filename: pair-navigator.el
;; Description: pair navigation
;; Compatibility: emacs-version >= 27
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; pair-navigator.el helps navigate through pairs defined in the file.  From  http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html

;;;
;;; Code:
(defvar pair-navigator-brackets nil "String of left/right brackets pairs.")
(defvar pair-navigator-brackets "()[]{}<>（）［］｛｝⦅⦆〚〛⦃⦄“”‘’‹›«»「」〈〉《》【】〔〕⦗⦘『』〖〗〘〙｢｣⟦⟧⟨⟩⟪⟫⟮⟯⟬⟭⌈⌉⌊⌋⦇⦈⦉⦊❛❜❝❞❨❩❪❫❴❵❬❭❮❯❰❱❲❳〈〉⦑⦒⧼⧽﹙﹚﹛﹜﹝﹞⁽⁾₍₎⦋⦌⦍⦎⦏⦐⁅⁆⸢⸣⸤⸥⟅⟆⦓⦔⦕⦖⸦⸧⸨⸩｟｠⧘⧙⧚⧛⸜⸝⸌⸍⸂⸃⸄⸅⸉⸊᚛᚜༺༻༼༽⏜⏝⎴⎵⏞⏟⏠⏡﹁﹂﹃﹄︹︺︻︼︗︘︿﹀︽︾﹇﹈︷︸")

(defconst pair-navigator-left-brackets '("(" "{" "[" "<" "〔" "【" "〖" "〈" "《" "「" "『" "“" "‘" "‹" "«" )
  "List of left bracket chars.")

(defconst pair-navigator-right-brackets '(")" "]" "}" ">" "〕" "】" "〗" "〉" "》" "」" "』" "”" "’" "›" "»")
  "List of right bracket chars.")

(defun pair-navigator-backward-left-bracket ()
  "Move cursor to the previous occurrence of left bracket.
The list of brackets to jump to is defined by `pair-navigator-left-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-01"
  (interactive)
  (re-search-backward (regexp-opt pair-navigator-left-brackets) nil t))

(defun pair-navigator-forward-right-bracket ()
  "Move cursor to the next occurrence of right bracket.
The list of brackets to jump to is defined by `pair-navigator-right-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-01"
  (interactive)
  (if (looking-at (regexp-opt pair-navigator-right-brackets))
      (forward-char))
  (re-search-forward (regexp-opt pair-navigator-right-brackets) nil t)
  (backward-char))

(defun pair-navigator-goto-matching-bracket ()
  "Move cursor to the matching bracket.
If cursor is not on a bracket, call `backward-up-list'.
The list of brackets to jump to is defined by `pair-navigator-left-brackets' and `pair-navigator-right-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2016-11-22"
  (interactive)
  (if (nth 3 (syntax-ppss))
      (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)
    (cond
     ((eq (char-after) ?\") (forward-sexp))
     ((eq (char-before) ?\") (backward-sexp ))
     ((looking-at (regexp-opt pair-navigator-left-brackets))
      (forward-sexp)
      (backward-char))
     ((looking-back (regexp-opt pair-navigator-right-brackets) (max (- (point) 1) 1))
      (backward-sexp))
     (t (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)))))

;;; pair-navigator.el ends here
