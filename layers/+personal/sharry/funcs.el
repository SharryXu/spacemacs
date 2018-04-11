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
  (message "%s" (shell-command-to-string "./a.out")))

(defun sharry/configure-c-c++-mode ()
  (local-set-key (kbd ";")
                 'sharry/format-c-c++-code-type-semi&comma)
  (local-set-key (kbd "}")
                 'sharry/format-c-c++-code-type-brace)
  (local-set-key (kbd "<f5>")
                 'sharry/compile-current-file-and-run)

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
