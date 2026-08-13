;;; openapi-yaml-mode.el --- Major mode for OpenAPI YAML files -*- lexical-binding: t; -*-

;; Author: Aaron Gonzales
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (flycheck "35"))
;; Keywords: languages, yaml, openapi
;; URL: https://github.com/your-name/openapi-yaml-mode

;;; Commentary:

;; `openapi-yaml-mode' is derived from `yaml-ts-mode' and therefore
;; inherits YAML syntax, indentation, font locking, hooks, and keybindings.
;;
;; This package also defines an optional Flycheck checker using Spectral.
;;
;; Expected Flycheck chain:
;;
;;   lsp -> yaml-yamllint -> openapi-spectral

;;; Code:
(require 'json)
(require 'yaml-ts-mode)

(defgroup openapi-yaml nil
  "Editing OpenAPI documents written in YAML."
  :group 'languages
  :prefix "openapi-yaml-")

;;; Customization
(defcustom openapi-yaml-spectral-executable "spectral"
  "Executable used to run Spectral."
  :type 'string
  :group 'openapi-yaml)
(defcustom openapi-yaml-spectral-arguments
  '("lint" "--format" "json" "--quiet")
  "Arguments passed to Spectral before the source filename."
  :type '(repeat string)
  :group 'openapi-yaml)


;;; Major mode
;;;###autoload
(define-derived-mode openapi-yaml-mode yaml-ts-mode "OpenAPI"
  "Major mode for editing OpenAPI documents written in YAML.

This mode derives from `yaml-ts-mode', so general YAML hooks and
configuration continue to apply."
  :group 'openapi-yaml
  ;; lsp-mode normally obtains this from
  ;; `lsp-language-id-configuration'. Keeping it buffer-local also makes
  ;; the intended language explicit.
  (setq-local lsp-buffer-language-id "yaml")
  ;; Refresh Imenu automatically when the document changes.
  (setq-local imenu-auto-rescan t)
  (font-lock-flush)
  (font-lock-ensure))


;;; File associations
;;;###autoload
(add-to-list 'auto-mode-alist '(".*\\(?:openapi\\|swagger\\).*\\.ya?ml\\'" . openapi-yaml-mode))
;;;###autoload
(add-to-list 'auto-mode-alist  '(".*\\(?:openapi\\|swagger\\)/.*\\.ya?ml\\'" . openapi-yaml-mode))

;;; OpenAPI detection
(defun openapi-yaml-buffer-p ()
  "Return non-nil when the current buffer uses `openapi-yaml-mode'."
  (derived-mode-p 'openapi-yaml-mode))


;;; LSP integration
(with-eval-after-load 'lsp-mode
  (add-to-list
    'lsp-language-id-configuration
    '(openapi-yaml-mode . "yaml")))

;;; Spectral Flycheck integration
(defun openapi-yaml--spectral-severity (severity)
  "Convert Spectral SEVERITY to a Flycheck error level."
  (pcase severity
    (0 'error)
    (1 'warning)
    (_ 'info)))
(defun openapi-yaml--spectral-error-parser (output checker buffer)
  "Parse Spectral JSON OUTPUT for CHECKER in BUFFER."
  (condition-case err
    (let ((results
            (json-parse-string
              output
              :object-type 'alist
              :array-type 'list
              :null-object nil
              :false-object nil)))
      (mapcar
        (lambda (result)
          (let* ((range (alist-get 'range result))
                  (start (alist-get 'start range))
                  (line
                    (1+ (or (alist-get 'line start) 0)))
                  (column
                    (1+ (or (alist-get 'character start) 0)))
                  (level
                    (openapi-yaml--spectral-severity
                      (alist-get 'severity result)))
                  (message
                    (or (alist-get 'message result)
                      "Unknown Spectral diagnostic"))
                  (code (alist-get 'code result))
                  (source (alist-get 'source result)))
            (flycheck-error-new-at
              line
              column
              level
              (if code
                (format "%s [%s]" message code)
                message)
              :checker checker
              :buffer buffer
              :filename source)))
        results))
    (error
      (message
        "Unable to parse Spectral output: %s"
        (error-message-string err))
      nil)))

(defun openapi-yaml-register-flycheck-checker ()
  "Register the OpenAPI Spectral Flycheck checker."
  (unless (flycheck-valid-checker-p 'openapi-spectral)
    (flycheck-define-checker openapi-spectral
      "Validate an OpenAPI document with Spectral."
      :command ("spectral"
                 (eval openapi-yaml-spectral-arguments)
                 source)
      :error-parser openapi-yaml--spectral-error-parser
      :modes (openapi-yaml-mode)))
  (add-to-list 'flycheck-checkers 'openapi-spectral t)

  ;; yamllint -> Spectral
  (flycheck-add-next-checker
    'yaml-yamllint
    '(t . openapi-spectral)))

(with-eval-after-load 'flycheck
  (openapi-yaml-register-flycheck-checker))

;;; Keymap
(defvar-keymap openapi-yaml-mode-map
  :parent yaml-ts-mode-map

  "C-c C-c" #'flycheck-buffer
  "C-c C-e" #'flycheck-list-errors
  "C-c C-i" #'imenu)

(provide 'openapi-yaml-mode)
;;; openapi-yaml-mode.el ends here
