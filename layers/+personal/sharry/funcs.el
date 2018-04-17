(defun sharry/format-file-content (begin-position end-position)
  "Use spaces to substitute tabs, deleteing unnecessary whitespaces and indent all lines."
  (delete-trailing-whitespace)
  (indent-region begin-position
                 end-position)
  (untabify begin-position
            end-position))

(defun sharry/get-clang-format-config (folder)
  (if (locate-dominating-file folder sharry-default-clang-format-config-file-name)
      "file"
    sharry-default-clang-format-style))

(defun sharry/clang-format-buffer (begin-position end-position)
  (when (executable-find "clang-format")
    (clang-format-region begin-position
                         end-position
                         (sharry/get-clang-format-config (file-truename "~")))))

(defun sharry/quick-format ()
  "Format code quickly."
  (interactive)
  (if (string= major-mode "c-mode")
      (progn
        (sharry/clang-format-buffer (point-min)
                                    (point-max))
        (message "Formatting c code..."))
    (progn
      (sharry/format-file-content (point-min)
                                  (point-max))
      (message "Formatting `%s'..." (buffer-name)))))

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

(defun sharry/get-buffer--by-name (buffer-names pattern)
  "Get matched buffer name from BUFFER-NAMES using PATTERN."
  (cond
   ((null buffer-names) '())
   ((string-match-p (regexp-quote pattern) (car buffer-names))
    (car buffer-names))
   ((sharry/get-buffer--by-name (cdr buffer-names) pattern))))

(defun sharry/get-buffer-by-name (pattern)
  (let ((all-buffer-names (mapcar (function buffer-name) (buffer-list))))
    (cond
     ((null all-buffer-names) nil)
     ((sharry/get-buffer--by-name all-buffer-names name)))))

(defun sharry/kill-buffer-by-name (name)
  "Kill buffer which name is NAME."
  (interactive)
  (setq-local match-buffer (sharry/get-buffer-by-name name))
  (if match-buffer
      (kill-buffer match-buffer)
    (message "No buffer has name `%s'." name)))

(defun sharry/delete-window-by-name (name)
  (interactive)
  (setq-local match-buffer (sharry/get-buffer-by-name name))
  (if match-buffer
      (delete-windows-on match-buffer)
    (message "No window has name `%s'." name)))

(defun sharry/format-c-c++-code-type-brace ()
  "Format by clang-format when enter '}'."
  (interactive)
  (command-execute #'c-electric-brace)
;;  (let ((end-position (point))
  ;;      (begin-position (scan-lists (point) -1 0)))
    (let ((end-position (point-max))
          (begin-position (point-min)))
    (progn
      (sharry/format-file-content begin-position
                                  end-position)
      (sharry/clang-format-buffer begin-position
                                  end-position)
      (message "Formatting `%s'..." (buffer-name)))))

(defun sharry/format-c-c++-code-type-semi&comma ()
  "Format by clang-format when enter ';'."
  (interactive)
  (command-execute #'c-electric-semi&comma)
  (let ((end-position (line-end-position))
        (begin-position (line-beginning-position)))
    (progn
      (sharry/format-file-content begin-position
                                  end-position)
      (sharry/clang-format-buffer begin-position
                                  end-position)
      (message "Formatting `%s'..." (buffer-name)))))

(defun sharry/configure-common-c-c++-mode ()
  (local-set-key (kbd ";")
                 'sharry/format-c-c++-code-type-semi&comma)
  (local-set-key (kbd "}")
                 'sharry/format-c-c++-code-type-brace)
  ;; Start debug
  (local-set-key (kbd "<f5>")
                 'sharry/compile-current-file-and-run)
  ;; Stop debug
  (local-set-key (kbd "<f8>")
                 (lambda ()
                   (interactive)
                   (sharry/kill-buffer-by-name sharry-async-shell-buffer-name)))

  (setq indent-tabs-mode nil)

  (require 'flycheck)
  (flycheck-mode 1)
  (semantic-mode 1))

(defun sharry/configure-c-mode ()
  (sharry/configure-common-c-c++-mode)

  (c-toggle-auto-newline -1)

  (c-set-style "sharry")

  (setq c-c++-default-mode-for-headers 'c-mode)
  (setq c-c++-enable-c++11 nil)
  (setq flycheck-clang-language-standard "gnu99")
  (setq flycheck-gcc-language-standard "gnu99"))

(defun sharry/configure-c++-mode ()
  (sharry/configure-common-c-c++-mode)

  (c-set-style "stroustrup")

  (setq c-c++-default-mode-for-headers 'c++-mode)
  (setq c-c++-enable-c++11 t))

(defun sharry/set-window-size-and-position ()
  "Setup window's size and position according to resolution."
  (interactive)
  (setq initial-frame-alist
        '((left . 30)
          (top . (+ 30 50))
          (width . 200)
          (height . 60))))

(defun sharry/compile-and-run-scheme ()
  "Quickly run scheme code."
  (interactive)
  (if (geiser-repl--repl-list)
      (geiser-eval-region (point-min)
                          (line-end-position))
    (progn
      (run-chicken)
      (sharry/delete-window-by-name sharry-chicken-repl-buffer-name))))

(defun sharry/configure-geiser-mode ()
  (local-set-key (kbd "<f5>")
                 'sharry/compile-and-run-scheme)
  (local-set-key (kbd "C-x C-e")
                 'sharry/compile-and-run-scheme)
  (local-set-key (kbd "<f8>")
                 (lambda ()
                   (interactive)
                   (sharry/delete-window-by-name sharry-chicken-repl-buffer-name))))
