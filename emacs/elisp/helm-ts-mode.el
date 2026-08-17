;;; helm-ts-mode.el --- Helm and YAML Tree-sitter mode -*- lexical-binding: t; -*-

;;; Commentary:

;; Multi-parser major mode for Helm templates.
;;
;; Primary parser:
;;   helm
;;
;; Embedded parser:
;;   yaml
;;
;; The Helm grammar parses template actions and exposes ordinary YAML
;; content through `text' nodes. The YAML parser is restricted to those
;; text ranges.

;;; Code:

(require 'treesit)
(require 'yaml-ts-mode)

(defgroup helm-ts-mode nil
  "Tree-sitter major mode for Helm templates."
  :group 'languages
  :prefix "helm-ts-mode-")

(defconst helm-ts-mode--range-settings
  (treesit-range-rules
   :embed 'yaml
   :host 'helm

   ;; The Helm grammar exposes non-template content as `text'.
   ;; All such ranges are combined into the embedded YAML parser.
   '((text) @capture))
  "Tree-sitter ranges that embed YAML in Helm text nodes.")

(defconst helm-ts-mode--helm-font-lock-keywords
  `(
     ;; Go-template delimiters.
     ("{{-?\\|-?}}"
       . font-lock-preprocessor-face)

     ;; Helm built-in objects.
     ("\\.\\(?:Values\\|Release\\|Chart\\|Capabilities\\|Template\\|Files\\)\\_>"
       . font-lock-variable-name-face)

     ;; Local template variables.
     ("\\$[[:word:]_-]+"
       . font-lock-variable-name-face)

     ;; Control forms following an opening delimiter.
     ("{{-?[[:space:]\n]*\\(if\\|else\\|end\\|range\\|with\\|define\\|template\\|block\\)\\_>"
       1 font-lock-keyword-face)

     ;; Frequently used functions following an opening delimiter or pipe.
     ("\\(?:{{-?[[:space:]\n]*\\||[[:space:]]*\\)\\(include\\|required\\|tpl\\|default\\|quote\\|squote\\|printf\\|toYaml\\|toJson\\|fromYaml\\|fromJson\\|indent\\|nindent\\|dict\\|list\\|merge\\|mergeOverwrite\\|lookup\\|fail\\|coalesce\\|ternary\\)\\_>"
       1 font-lock-function-name-face))
  "Supplemental Helm font-lock rules.")

(defcustom helm-ts-mode-main-values-file "values.yaml"
  "Main Helm values file.

The path may point outside the chart directory. Helm LS interprets
the path relative to the directory containing Chart.yaml."
  :type 'string
  :group 'helm-ts-mode)

(defcustom helm-ts-mode-lint-overlay-values-file "values.lint.yaml"
  "Values file merged with the main values file for `helm lint'.

The path may point outside the chart directory."
  :type 'string
  :group 'helm-ts-mode)

(defcustom helm-ts-mode-additional-values-files-glob "values*.yaml"
  "Glob used by Helm LS to find additional values files.

The glob may point outside the chart directory."
  :type 'string
  :group 'helm-ts-mode)

(defcustom helm-ts-mode-helm-ls-command
  '("helm_ls" "serve")
  "Command used to start Helm LS."
  :type '(repeat string)
  :group 'helm-ts-mode)

(defcustom helm-ts-mode-yamlls-command "yaml-language-server"
  "Executable used internally by Helm LS for YAML features."
  :type 'string
  :group 'helm-ts-mode)

(defcustom helm-ts-mode-values-root "../../values"
  "Path from the Helm chart directory to the external values root.

The values root is expected to contain a `base' directory and an
`environments' directory."
  :type 'string
  :group 'helm-ts-mode)

(defcustom helm-ts-mode-base-values-directory "base"
  "Directory containing base values files.

This is relative to `helm-ts-mode-values-root'."
  :type 'string
  :group 'helm-ts-mode)

(defcustom helm-ts-mode-environments-directory "environments"
  "Directory containing environment-specific values directories.

This is relative to `helm-ts-mode-values-root'."
  :type 'string
  :group 'helm-ts-mode)

(defcustom helm-ts-mode-yq-command "yq"
  "Executable used to merge values files for Helm LS."
  :type 'string
  :group 'helm-ts-mode)

(defvar-local helm-ts-mode-selected-environment nil
  "Environment currently selected for this Helm buffer.")

(make-variable-buffer-local 'helm-ts-mode-values-root)

(put 'helm-ts-mode-values-root
     'safe-local-variable
     #'stringp)

;; These settings can vary by chart through .dir-locals.el.
(make-variable-buffer-local
  'helm-ts-mode-main-values-file)

(make-variable-buffer-local
  'helm-ts-mode-lint-overlay-values-file)

(make-variable-buffer-local
  'helm-ts-mode-additional-values-files-glob)

;; Permit string values in .dir-locals.el without prompting repeatedly.
(put 'helm-ts-mode-main-values-file
  'safe-local-variable
  #'stringp)

(put 'helm-ts-mode-lint-overlay-values-file
  'safe-local-variable
  #'stringp)

(put 'helm-ts-mode-additional-values-files-glob
  'safe-local-variable
  #'stringp)

;;;###autoload
;;;###autoload
(define-derived-mode helm-ts-mode yaml-ts-mode "Helm"
  "Major mode for YAML files containing Helm Go templates."

  (helm-ts-mode--check-grammars)

  ;; `yaml-ts-mode' has already created the YAML parser. Create the
  ;; Helm parser and make it the primary parser.
  (let ((helm-parser
          (or
            (car
              (treesit-parser-list
                nil
                'go-template-helm))
            (treesit-parser-create
              'go-template-helm))))
    ;; Required for reliable multi-language font-lock in Emacs 30+.
    (setq-local treesit-primary-parser helm-parser))

  ;; Restrict the YAML parser to ordinary text ranges identified by
  ;; the Helm parser.
  (setq-local
    treesit-range-settings
    helm-ts-mode--range-settings)

  ;; Preserve the YAML Tree-sitter settings inherited from
  ;; `yaml-ts-mode', then supplement them with Helm styling.
  (font-lock-add-keywords
    nil
    helm-ts-mode--helm-font-lock-keywords
    'append)

  (setq-local comment-start "# ")
  (setq-local comment-end "")

  ;; Reinitialize Tree-sitter now that the primary parser and embedded
  ;; ranges are configured.
  (treesit-major-mode-setup)

  (font-lock-flush))

(defun helm-ts-mode--check-grammars ()
  "Signal an error when a required Tree-sitter grammar is unavailable."
  (unless (treesit-available-p)
    (user-error
     "This Emacs build does not support Tree-sitter"))

  (unless (treesit-language-available-p 'yaml)
    (user-error
     "The YAML Tree-sitter grammar is not installed"))

  (unless
      (treesit-language-available-p 'helm)
    (user-error
     (concat
      "The helm grammar is not installed; "
       "run M-x treesit-install-language-grammar"))))

(defun helm-ts-mode-describe-parsers ()
  "Display the Tree-sitter parsers active in this buffer."
  (interactive)
  (message
   "Primary: %S; parsers: %S"
   (and
    (boundp 'treesit-primary-parser)
    treesit-primary-parser)
   (mapcar
    (lambda (parser)
      (list
       (treesit-parser-language parser)
       (treesit-parser-root-node parser)))
    (treesit-parser-list))))

(defun helm-ts-mode-explore-helm-tree ()
  "Open Tree-sitter Explorer using the Helm parser."
  (interactive)
  (unless (derived-mode-p 'helm-ts-mode)
    (user-error "Not in `helm-ts-mode'"))

  ;; Put the Helm parser first for the explorer.
  (setq-local
   treesit-primary-parser
   (car
    (treesit-parser-list
     nil
     'go-template-helm)))

  (treesit-explore-mode 1))

(defun helm-ts-mode--chart-root ()
  "Return the Helm chart directory for the current buffer."
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))

  (or (locate-dominating-file
       buffer-file-name
       "Chart.yaml")
      (user-error
       "Could not find Chart.yaml above %s"
       buffer-file-name)))

(defun helm-ts-mode--values-root (chart-root)
  "Return the absolute values root for CHART-ROOT."
  (expand-file-name
   helm-ts-mode-values-root
   chart-root))

(defun helm-ts-mode--yaml-files (directory)
  "Return YAML files beneath DIRECTORY in lexical order."
  (unless (file-directory-p directory)
    (user-error
     "Values directory does not exist: %s"
     directory))

  (sort
   (directory-files-recursively
    directory
    "\\.ya?ml\\'")
   #'string-lessp))

(defun helm-ts-mode--environment-names (values-root)
  "Return environment directory names beneath VALUES-ROOT."
  (let ((environment-root
         (expand-file-name
          helm-ts-mode-environments-directory
          values-root)))
    (unless (file-directory-p environment-root)
      (user-error
       "Environment directory does not exist: %s"
       environment-root))

    (sort
     (mapcar
      #'file-name-nondirectory
      (seq-filter
       #'file-directory-p
       (directory-files
        environment-root
        t
        directory-files-no-dot-files-regexp)))
     #'string-lessp)))

(defun helm-ts-mode--merge-values-files
    (files destination)
  "Merge FILES with yq and write the result to DESTINATION.

Files are merged in the order given. Later files override earlier
files."
  (unless files
    (user-error
     "No YAML values files were found"))

  (unless (executable-find helm-ts-mode-yq-command)
    (user-error
     "Could not find `%s'; install it with `brew install yq'"
     helm-ts-mode-yq-command))

  (make-directory
   (file-name-directory destination)
   t)

  (with-temp-buffer
    (let ((status
           (apply
            #'process-file
            helm-ts-mode-yq-command
            nil
            t
            nil
            "eval-all"
            ". as $item ireduce ({}; . * $item)"
            files)))
      (unless (zerop status)
        (user-error
         "yq failed while merging values:\n%s"
         (buffer-string)))

      (write-region
       (point-min)
       (point-max)
       destination
       nil
       'silent))))

(defun helm-ts-mode--restart-workspace ()
  "Restart the LSP workspace for the current Helm buffer."
  (if (bound-and-true-p lsp-mode)
      (lsp-workspace-restart)
    (lsp-deferred)))

(defun helm-ts-mode--register-lsp-client ()
  "Register Helm LS with `lsp-mode'."
  (add-to-list
    'lsp-language-id-configuration
    '(helm-ts-mode . "helm"))

  (lsp-register-custom-settings
    '(("helm-ls.logLevel" "info")

       ("helm-ls.valuesFiles.mainValuesFile"
         helm-ts-mode-main-values-file)

       ("helm-ls.valuesFiles.lintOverlayValuesFile"
         helm-ts-mode-lint-overlay-values-file)

       ("helm-ls.valuesFiles.additionalValuesFilesGlobPattern"
         helm-ts-mode-additional-values-files-glob)

       ("helm-ls.helmLint.enabled" t t)

       ("helm-ls.yamlls.enabled" t t)

       ("helm-ls.yamlls.path"
         helm-ts-mode-yamlls-command)

       ("helm-ls.yamlls.diagnosticsLimit" 50)

       ;; Preserve your normal LSP/Flycheck diagnostics workflow.
       ("helm-ls.yamlls.showDiagnosticsDirectly" nil t)

       ("helm-ls.yamlls.config.completion" t t)

       ("helm-ls.yamlls.config.hover" t t)))

  (lsp-register-client
    (make-lsp-client
      :new-connection
      (lsp-stdio-connection
        helm-ts-mode-helm-ls-command)

      :activation-fn
      (lsp-activate-on "helm")

      :priority 1
      :multi-root t
      :server-id 'helm-ls)))

(defun helm-ts-mode-select-environment (environment)
  "Select ENVIRONMENT for Helm completion and diagnostics.

Merge the base values files and the selected environment files into
generated files, configure Helm LS to use them, and restart the LSP
workspace."
  (interactive
   (let* ((chart-root
           (helm-ts-mode--chart-root))
          (values-root
           (helm-ts-mode--values-root chart-root))
          (environments
           (helm-ts-mode--environment-names
            values-root)))
     (unless environments
       (user-error
        "No environment directories were found"))

     (list
      (completing-read
       "Helm environment: "
       environments
       nil
       t
       nil
       nil
       helm-ts-mode-selected-environment))))

  (let* ((chart-root
          (helm-ts-mode--chart-root))
         (values-root
          (helm-ts-mode--values-root chart-root))

         (base-directory
          (expand-file-name
           helm-ts-mode-base-values-directory
           values-root))

         (environment-directory
          (expand-file-name
           environment
           (expand-file-name
            helm-ts-mode-environments-directory
            values-root)))

         (base-files
          (helm-ts-mode--yaml-files
           base-directory))

         (environment-files
          (helm-ts-mode--yaml-files
           environment-directory))

         ;; Keep generated files beneath the external values root.
         (generated-directory
          (expand-file-name
           (format ".helm-ls/%s/" environment)
           values-root))

         (generated-base-file
          (expand-file-name
           "base.yaml"
           generated-directory))

         (generated-overlay-file
          (expand-file-name
           "overlay.yaml"
           generated-directory)))

    (unless (member
             environment
             (helm-ts-mode--environment-names
              values-root))
      (user-error
       "Unknown Helm environment: %s"
       environment))

    ;; Create the two files expected by Helm LS.
    (helm-ts-mode--merge-values-files
     base-files
     generated-base-file)

    (helm-ts-mode--merge-values-files
     environment-files
     generated-overlay-file)

    ;; Helm LS expects paths relative to the Chart.yaml directory.
    (setq-local
     helm-ts-mode-main-values-file
     (file-relative-name
      generated-base-file
      chart-root))

    (setq-local
     helm-ts-mode-lint-overlay-values-file
     (file-relative-name
      generated-overlay-file
      chart-root))

    ;; Limit completion and hover to the selected environment's
    ;; generated files.
    (setq-local
     helm-ts-mode-additional-values-files-glob
     (concat
      (file-relative-name
       generated-directory
       chart-root)
      "*.yaml"))

    (setq-local
     helm-ts-mode-selected-environment
     environment)

    (message
     "Helm environment: %s; base files: %d; overlay files: %d"
     environment
     (length base-files)
     (length environment-files))

    (helm-ts-mode--restart-workspace)))

;; This file can load before or after lsp-mode.
(with-eval-after-load 'lsp-mode
  (helm-ts-mode--register-lsp-client))

;; Start LSP only after entering Helm mode.
(add-hook 'helm-ts-mode-hook #'lsp-deferred)

(provide 'helm-ts-mode)

;;; helm-ts-mode.el ends here
