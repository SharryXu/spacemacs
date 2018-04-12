(defun sharry/format-code (begin-position end-position)
  "Use spaces to substitute tabs, deleteing unnecessary whitespaces and indent all lines."
  (delete-trailing-whitespace)
  (indent-region begin-position
                 end-position)
  (untabify begin-position
            end-position))

(defun sharry/quick-format ()
  "Format code quickly."
  (interactive)
  (sharry/format-code (point-min)
                      (point-max))
  (message "Formatting..."))

(defun sharry/disable-c-toggle-auto-newline ()
  "Disable toggle auto-newline."
  (c-toggle-auto-newline -1))

(defun sharry/format-c-c++-code-type-brace ()
  "Format by clang-format when enter '}'."
  (interactive)
  (command-execute #'c-electric-brace)
  (let ((end-position (point-max))
        (begin-position (point-min)))
    (progn
      (sharry/format-code begin-position
                          end-position)
      (when (executable-find "clang-format")
        (progn
          (clang-format-region begin-position
                               end-position)
          (evil-force-normal-state)
          (save-buffer)
          (message "Formatting and Saving `%s'..." (buffer-name)))))))

(defun sharry/format-c-c++-code-type-semi&comma ()
  "Format by clang-format when enter ';'."
  (interactive)
  (command-execute #'c-electric-semi&comma)
  (let ((end-position (line-end-position))
        (begin-position (line-beginning-position)))
    (progn
      (clang-format-buffer)
      (sharry/format-code begin-position end-position)
      (when (executable-find "clang-format")
        (progn
          (clang-format-region begin-position
                               end-position)
          (message "Formatting `%s'..." (buffer-name)))))))

(defun sharry/compile-and-run-scheme ()
  "Quickly run scheme code."
  (interactive)
  (geiser-eval-region (point-min)
                      (line-end-position)))

(defun sharry/compile-current-file-and-run ()
  "Compile selected file and run."
  (interactive)
  (compile (concat "clang " (buffer-name))))

(defun sharry/close-compilation-window-if-no-errors (str)
  "Close compilation window if the result STR contains no error messages."
  (if (null (string-match ".*exited abnormally.*" str))
      (progn
        (run-at-time
         "0.5 sec" nil 'delete-windows-on
         (get-buffer-create "*compilation*"))
        (message "No errors.")))
  (async-shell-command "./a.out")
  (select-window (previous-window)))

(defun sharry/get-buffer-name (buffer-names pattern)
  "Get matched buffer name from BUFFER-NAMES using PATTERN."
  (cond
   ((null buffer-names) '())
   ((string-match-p (regexp-quote pattern) (car buffer-names))
       (car buffer-names))
   ((sharry/get-buffer-name (cdr buffer-names) pattern))))

(defun sharry/kill-async-shell-buffer ()
  "Kill buffer which name is *Async Shell Command*."
  (interactive)
  (let ((all-buffers-names (mapcar (function buffer-name) (buffer-list)))
        (async-shell-name sharry-async-shell-buffer-name))
    (setq-local async-shell-buffer (sharry/get-buffer-name all-buffers-names async-shell-name))
    (if async-shell-buffer
        (kill-buffer async-shell-buffer)
      (message "No buffer has name `%s'." sharry-async-shell-buffer-name))))

(defun sharry/configure-c-c++-mode ()
  (local-set-key (kbd ";")
                 'sharry/format-c-c++-code-type-semi&comma)
  (local-set-key (kbd "}")
                 'sharry/format-c-c++-code-type-brace)
  ;; Start debug
  (local-set-key (kbd "<f5>")
                 'sharry/compile-current-file-and-run)
  ;; Stop debug
  (local-set-key (kbd "<f8>")
                 'sharry/kill-async-shell-buffer)

  (require 'flycheck)
  (flycheck-mode 1)
  (sharry/disable-c-toggle-auto-newline)
  (semantic-mode 1))

(defun sharry/set-window-size-and-position ()
  "Setup window's size and position according to resolution."
  (interactive)
  (when (window-system)
    (progn
      (let ((systems-tool-bar-height 50))
        (setq initial-frame-alist
              '(
                (left . 30)
                (top . (+ 30 systems-tool-bar-height))
                (width . 200) ; chars
                (height . 60) ; lines))
                ))
        (evil-terminal-cursor-changer-activate)))))
