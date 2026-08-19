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

(defconst helm-ts-mode--treesit-range-settings
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

(defcustom helm-ts-mode-values-root nil
  "Location of the external Helm values directory.

When nil, search upward from the current chart until a directory named
`values' is found containing both `base' and `environments'
subdirectories.

When set to a string, it may be an absolute path or a path relative to
the current chart root."
  :type
  '(choice
     (const :tag "Search parent directories automatically" nil)
     (string :tag "Explicit path"))
  :group 'helm-ts-mode)

(defcustom helm-ts-mode-base-values-directory "base"
  "Directory containing base values files.

This is relative to `helm-ts-mode-values-root'."
  :type 'string
  :group 'helm-ts-mode)

(defcustom helm-ts-mode-environments-directory ""
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

;;;###autoload
(define-derived-mode helm-ts-mode yaml-ts-mode "Helm"
  "Major mode for YAML files containing Helm Go templates."

  (helm-ts-mode--check-grammars)

  ;; `yaml-ts-mode' has already created the YAML parser. Create the
  ;; Helm parser and make it the primary parser.
  (let ((helm-parser
          (or
            (car
              ;; isnt this always nil?
              (treesit-parser-list
                nil
                'helm))
            (treesit-parser-create
              'helm))))
    ;; Required for reliable multi-language font-lock in Emacs 30+.
    (setq-local treesit-primary-parser helm-parser))

  ;; Restrict the YAML parser to ordinary text ranges identified by
  ;; the Helm parser.
  (setq-local
    treesit-range-settings
    helm-ts-mode--treesit-range-settings)

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

(defun helm-ts-mode--values-directory-p (directory)
  "Return non-nil when DIRECTORY looks like the external values root."
  (and
    (file-directory-p directory)
    (file-directory-p
      (expand-file-name "base" directory))))

(defun helm-ts-mode--find-values-root (starting-directory)
  "Search upward from STARTING-DIRECTORY for the external values root.

The nearest `values' directory containing `base' is returned."
  (let ((directory
          (file-name-as-directory
            (expand-file-name starting-directory)))
         parent
         candidate
         found)
    (while (and directory
             (not found))
      (setq candidate
        (expand-file-name "values" directory))

      (if (helm-ts-mode--values-directory-p candidate)
        (setq found candidate)
        ;; update parent and directory if not found
        (setq parent
          (file-name-directory
            (directory-file-name directory)))
        ;; Stop after inspecting the filesystem root.
        (setq directory
          (unless (equal parent directory)
            parent))))

    (when found
      (file-name-as-directory found))))

(defun helm-ts-mode--values-root (chart-root)
  "Return the external values root associated with CHART-ROOT."
  (let ((values-root
          (if helm-ts-mode-values-root
            ;; Preserve support for explicit absolute or relative paths.
            (expand-file-name
              helm-ts-mode-values-root
              chart-root)
            ;; Automatically search this chart and its ancestors.
            (helm-ts-mode--find-values-root
              chart-root))))
    (unless values-root
      (user-error
        "Could not find a values directory containing a base/ directory"))
    (unless (helm-ts-mode--values-directory-p values-root)
      (user-error
        "Invalid Helm values root: %s"
        values-root))
    values-root))

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
     'helm)))

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
  "Restart the Helm LS workspace for the current buffer."
  (if-let* ((workspace
             (seq-find
               (lambda (candidate)
                 (eq
                   (lsp--client-server-id
                     (lsp--workspace-client candidate))
                   'helm-ls))
               (lsp-workspaces))))
    (lsp-workspace-restart workspace)
    (lsp-deferred)))

(defun helm-ts-mode-select-environment (environment)
  "Select ENVIRONMENT for Helm completion and diagnostics.

Merge the base values files and selected environment files into
generated files, configure Helm LS to use them, and restart the
Helm LS workspace."
  (interactive
    (let* ((chart-root
             (helm-ts-mode--chart-root))
            (values-root
              (helm-ts-mode--values-root
                chart-root))
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
           ;; Nearest chart containing the current template.
           ;; In this case, kahless-ui.
           (helm-ts-mode--chart-root))

          (lsp-chart-root
            ;; Outermost parent chart.
            ;; In this case, kahless-services.
            (helm-ts-mode--top-chart-root
              chart-root))

          (values-root
            ;; Continue locating the external values directory relative
            ;; to the nearest chart, as configured previously.
            (helm-ts-mode--values-root
              chart-root))

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

          ;; Store generated files under:
          ;;
          ;; values/.helm-ls/ENVIRONMENT/
          (generated-directory
            (expand-file-name
              (format
                ".helm-ls/%s/"
                environment)
              values-root))

          (generated-base-file
            (expand-file-name
              "base.yaml"
              generated-directory))

          (generated-overlay-file
            (expand-file-name
              "overlay.yaml"
              generated-directory)))

    (unless
      (member
        environment
        (helm-ts-mode--environment-names
          values-root))
      (user-error
        "Unknown Helm environment: %s"
        environment))

    ;; Merge base files in lexical order. Later files override
    ;; earlier files.
    (helm-ts-mode--merge-values-files
      base-files
      generated-base-file)

    ;; Merge the selected environment files in lexical order.
    (helm-ts-mode--merge-values-files
      environment-files
      generated-overlay-file)

    ;; These paths must be relative to the parent chart. Helm LS uses
    ;; the parent chart to resolve:
    ;;
    ;;   global:
    ;;   kahless-ui:
    ;;
    ;; for templates inside the kahless-ui subchart.
    (setq-local
      lsp-kubernetes-helm-ls-main-values-file-path
      (file-relative-name
        generated-base-file
        lsp-chart-root))

    (setq-local
      lsp-kubernetes-helm-overlay-values-file-path
      (file-relative-name
        generated-overlay-file
        lsp-chart-root))

    ;; Include both base.yaml and overlay.yaml in completion and hover.
    (setq-local
      lsp-kubernetes-helm-additional-values-files-pattern
      (concat
        (file-relative-name
          generated-directory
          lsp-chart-root)
        "*.yaml"))

    (setq-local
      helm-ts-mode-selected-environment
      environment)

    (message
      (concat
        "Helm environment: %s; "
        "template chart: %s; "
        "LSP chart: %s; "
        "base files: %d; "
        "environment files: %d")
      environment
      (abbreviate-file-name chart-root)
      (abbreviate-file-name lsp-chart-root)
      (length base-files)
      (length environment-files))

    (helm-ts-mode--restart-workspace)))

(defun helm-ts-mode--parent-chart-root (chart-root)
  "Return the immediate parent chart of CHART-ROOT, if one exists."
  (let* ((chart-root
           (file-name-as-directory
             (expand-file-name chart-root)))
          (charts-directory
            (file-name-directory
              (directory-file-name chart-root)))
          (possible-parent
            (and charts-directory
              (file-name-directory
                (directory-file-name charts-directory)))))
    (when
      (and possible-parent
        (equal
          (file-name-nondirectory
            (directory-file-name charts-directory))
          "charts")
        (file-exists-p
          (expand-file-name
            "Chart.yaml"
            possible-parent)))
      possible-parent)))

(defun helm-ts-mode--top-chart-root (chart-root)
  "Return the outermost parent chart containing CHART-ROOT."
  (let ((root chart-root)
         parent)
    (while
      (setq parent
        (helm-ts-mode--parent-chart-root root))
      (setq root parent))
    root))

(provide 'helm-ts-mode)

;;; helm-ts-mode.el ends here
