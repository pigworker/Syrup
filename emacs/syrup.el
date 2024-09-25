(require 'compile)

;; based on: http://ergoemacs.org/emacs/elisp_syntax_coloring.html

;; define several class of keywords
(setq syrup-keywords  '("where"))
(setq syrup-operators '("!" "&" "|"))
(setq syrup-symbols   '("=" "," ":" "->" "@"))

;; create the regex string for each class of keywords
(setq syrup-keywords-regexp  (regexp-opt syrup-keywords 'words))
(setq syrup-operators-regexp (regexp-opt syrup-operators))
(setq syrup-symbols-regexp   (regexp-opt syrup-symbols))
(setq syrup-types-regexp "\<[[:alpha:]][[:alnum:]]*\>")
(setq syrup-functions-regexp "\\([[:alpha:]][[:alnum:]]*\\)\(")
(setq syrup-experiments-regexp "\\(experiment\\|type\\|anf\\|simplify\\|print\\|display\\)[[:space:]]+\\([[:alpha:]][[:alnum:]]*\\)")
(setq syrup-bisimulations-regexp "\\(experiment\\)[[:space:]]+\\([[:alpha:]][[:alnum:]]*\\)[[:space:]]+=[[:space:]]+\\([[:alpha:]][[:alnum:]]*\\)")

;; clear memory
(setq syrup-keywords  nil)
(setq syrup-operators nil)
(setq syrup-symbols   nil)

;; create the list for font-lock.
;; each class of keyword is given a particular face
(setq syrup-font-lock-keywords
  `(
    (,syrup-keywords-regexp  . font-lock-keyword-face)
    (,syrup-symbols-regexp   . font-lock-builtin-face)
    (,syrup-operators-regexp . font-lock-builtin-face)
    (,syrup-types-regexp     . font-lock-type-face)
    (,syrup-functions-regexp . (1 font-lock-function-name-face))
    (,syrup-experiments-regexp (1 font-lock-keyword-face) (2 font-lock-function-name-face))
    (,syrup-bisimulations-regexp (1 font-lock-keyword-face) (2 font-lock-function-name-face) (3 font-lock-function-name-face))
))

;; syntax table
(defvar syrup-syntax-table nil "Syntax table for `syrup-mode'.")
(setq syrup-syntax-table
  (let ((synTable (make-syntax-table)))

  ;; comments
  (modify-syntax-entry ?-  ". 12" synTable)
  (modify-syntax-entry ?\n ">" synTable)

        synTable))


;; define the mode
(define-derived-mode syrup-mode fundamental-mode
  "Syrup mode"
  ;; handling comments
  :syntax-table syrup-syntax-table
  ;; code for syntax highlighting
  (setq font-lock-defaults '((syrup-font-lock-keywords)))
  (setq mode-name "syrup")
  ;; clear memory
  (setq syrup-keywords-regexp nil)
  (setq syrup-operators-regexp nil)
)

;; Customisation options

(defgroup syrup nil
  "A language to define circuit diagrams."
  :group 'languages)

(defcustom syrup-command "syrup"
  "The path to the syrup command to run."
  :type 'string
  :group 'syrup)

(defcustom syrup-options nil
  "Command line options to pass to syrup."
  :type 'string
  :group 'syrup)

;; Compilation mode for running syrup
;; (based on https://spin.atomicobject.com/2016/05/27/write-emacs-package/ )

(defun syrup-compilation-filter ()
  "Filter function for compilation output."
  (ansi-color-apply-on-region compilation-filter-start (point-max)))

(define-compilation-mode syrup-compilation-mode "Syrup"
  "Syrup compilation mode."
  (progn
    (set (make-local-variable 'compilation-error-regexp-alist)
         '(("\\(^[^[:space:]]*\\):\\([0-9]+\\):\\([0-9]+\\)-\\(\\([0-9]+\\):\\)?\\([0-9]+\\)$"
            1 (2 . 5) (3 . 6) 2)
           ("^Parse error \\(at\\|near\\) location: \\([^[:space:]]*\\):\\([0-9]+\\):\\([0-9]+\\)"
            2 3 (4 . 5) 2)
           ("^Warning: \\([^[:space:]]*\\):\\([0-9]+\\):\\([0-9]+\\)-\\(\\([0-9]+\\):\\)?\\([0-9]+\\)$"
           1 (2 . 5) (3 . 6) 1)
           ))
    (add-hook 'compilation-filter-hook 'syrup-compilation-filter nil t)))

(defface syrup-highlight-error-face
  '((t (:underline (:color "red" :style wave))))
  "The face used for errors.")

(defun syrup-run-on-file (syrup-file options)
  "Run syrup in a compilation buffer on SYRUP-FILE."
  (setq compilation-auto-jump-to-first-error t)
  (setq next-error-highlight-timer t)
  (setq next-error-highlight t)
  (setq syrup-error-highlight (make-overlay (point-min) (point-min)))
  (overlay-put syrup-error-highlight 'face 'syrup-highlight-error-face)
  (setq compilation-highlight-overlay syrup-error-highlight)
  (save-some-buffers compilation-ask-about-save
                     (when (boundp 'compilation-save-buffers-predicate)
                       compilation-save-buffers-predicate))

  (when (get-buffer "*syrup output*")
    (kill-buffer "*syrup output*"))
  (let ((syrup-command-to-run (concat syrup-command " " options " -f " syrup-file)))
    (with-current-buffer (get-buffer-create "*syrup output*")
      (compilation-start syrup-command-to-run 'syrup-compilation-mode (lambda (m) (buffer-name)))
      (overlay-put (make-overlay (point-min) (point-max) (current-buffer) nil t)
                   'face
                   `(:background "black",:foreground "white",:spacing "monospace", :extend t)))))

;;;###autoload
(defun syrup-run (override-options)
  "Run syrup on the current file."
  (interactive "P")
  (let ((opts (if override-options (read-string "Options: ") syrup-options)))
    (syrup-run-on-file (shell-quote-argument (buffer-file-name)) opts)))

(define-key syrup-mode-map (kbd "C-c C-l") 'syrup-run)

(add-hook 'compilation-mode-hook (lambda ()
                             (setq show-trailing-whitespace nil)))

(provide 'syrup-mode)
