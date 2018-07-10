;;; package --- Summary

;;; Commentary:

;;; Code:

;; Default function browse-url-default-browser has some problems.
(defun sharry/open-url-or-file (url-or-file-name)
  "Open URL-OR-FILE-NAME in default browser."
  (interactive)
  (require 'browse-url)
  ;; TODO: Add --devtools
  (browse-url-default-browser url-or-file-name))

(defun sharry/open-html-in-browser ()
  "Open current html in the specific browser."
  (interactive)
  (if (string-match ".*\\.html$" (buffer-name))
      (sharry/open-url-or-file (buffer-name))
    (message "Current file is not html.")))

(defun sharry/format-file-content (begin-position end-position)
  "Use spaces to substitute tabs and delete unnecessary whitespaces for paragraph between BEGIN-POSITION and END-POSITION."
  (delete-trailing-whitespace)
  (indent-region begin-position
                 end-position)
  (untabify begin-position
            end-position))

(defun sharry/get-clang-format-config ()
  "Load custom clang-format configuration."
  (if (file-exists-p sharry-default-clang-format-config-file-path)
      "file"
    sharry-default-clang-format-style))

(defun sharry/clang-format-buffer (begin-position end-position)
  "Use clang-format to format region from BEGIN-POSITION to END-POSITION."
  (when (executable-find "clang-format")
    (clang-format-region begin-position
                         end-position
                         (sharry/get-clang-format-config))))

(defun sharry/quick-format ()
  "Format code quickly."
  (interactive)
  (let ((file-name (buffer-file-name (current-buffer))))
    (cond
     ((string-match ".*\\.html$" file-name)
      (web-beautify-html-buffer))
     ((string-match ".*\\.css$" file-name)
      (web-beautify-css-buffer))
     ((string-match ".*\\.js$" file-name)
      (web-beautify-js-buffer))
     ((string-match ".*\\.c$" file-name)
      (sharry/clang-format-buffer (point-min)
                                  (point-max)))
     ((sharry/format-file-content (point-min)
                                  (- (point-max) 1)))) ;; Remove the EOF char at the end of the file.
    (message "Formatting `%s'..." file-name)))

(defun sharry/compile-current-file-and-run ()
  "Compile selected file and run."
  (interactive)
  (compile (format "clang %s" (buffer-name))))

(defun sharry/close-compilation-window-if-no-errors (str)
  "Close compilation window if the result STR contain no error messages."
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
  "Get buffer based on PATTERN."
  (let ((all-buffer-names (mapcar (function buffer-name) (buffer-list))))
    (cond
     ((null all-buffer-names) nil)
     ((sharry/get-buffer--by-name all-buffer-names pattern)))))

(defun sharry/kill-buffer-by-name (name)
  "Kill buffer which name is NAME."
  (interactive)
  (defvar match-buffer (sharry/get-buffer-by-name name))
  (if match-buffer
      (kill-buffer match-buffer)
    (message "No buffer has name `%s'." name)))

(defun sharry/delete-window-by-name (name)
  "Close window which name is NAME."
  (interactive)
  (defvar match-buffer (sharry/get-buffer-by-name name))
  (if match-buffer
      (delete-windows-on match-buffer)
    (message "No window has name `%s'." name)))

(defun sharry/format-c-c++-code-type-brace ()
  "Format by clang-format when enter '}'."
  (interactive)
	(c-electric-brace 1)
  (let ((end-position (line-end-position))
        (begin-position (scan-lists (- (point) 1) -1 1)))
    (progn
      (sharry/format-file-content begin-position
                                  end-position)
      (sharry/clang-format-buffer begin-position
                                  end-position)
      (evil-force-normal-state)
      (message "Formatting `%s'..." (buffer-name)))))

(defun sharry/format-c-c++-code-type-semi&comma ()
  "Format by clang-format when enter ';'."
  (interactive)
	(c-electric-semi&comma 1)
  (let ((end-position (line-end-position))
        (begin-position (line-beginning-position)))
    (progn
      (sharry/format-file-content begin-position
                                  end-position)
      (sharry/clang-format-buffer begin-position
                                  end-position)
      (message "Formatting `%s'..." (buffer-name)))))

(defun sharry/configure-common-c-c++-mode ()
  "Configure common part of C and C++."
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

	(auto-complete-mode 1)

  (setq indent-tabs-mode nil))

(defun sharry/configure-c-mode ()
  "Configure C mode."
  (sharry/configure-common-c-c++-mode)

  (c-toggle-auto-newline -1)

  (c-set-style "sharry")

  (defvar c-c++-default-mode-for-headers 'c-mode)
  (defvar c-c++-enable-c++11 nil)
  (defvar flycheck-clang-language-standard "gnu99")
  (defvar flycheck-gcc-language-standard "gnu99")
  (defvar flycheck-cppcheck-standards "gun99"))

(defun sharry/configure-c++-mode ()
  "Configure C++ mode."
  (sharry/configure-common-c-c++-mode)

  (c-set-style "stroustrup")

  (defvar c-c++-default-mode-for-headers 'c++-mode)
  (defvar c-c++-enable-c++11 t))

(defun sharry/set-window-size-and-position ()
  "Setup window's size and position according to resolution."
  (interactive)
  (setq initial-frame-alist
        '((left . 30)
          (top . (+ 30 50))
          (width . 200)
          (height . 60))))

(defun sharry/prepare-geiser-repl ()
  "Run geiser in the background."
  (unless (geiser-repl--repl-list)
    (progn
      (run-chicken)
      (sharry/delete-window-by-name sharry-chicken-repl-buffer-name))))

(defun sharry/compile-and-run-scheme ()
  "Quickly run scheme code."
  (interactive)
  (sharry/prepare-geiser-repl)
  (let ((begin-parenthesis-position (scan-lists (point) -1 1))
        (end-parenthesis-position (line-end-position)))
    (geiser-eval-region begin-parenthesis-position
                        end-parenthesis-position)))

(defun sharry/clear-semantic-db ()
  "Clear semantic database."
  (interactive)
  (if (shell-command (format "rm -rf %s/semanticdb" spacemacs-cache-directory))
      (message "Clear semantic db finished.")
    (message "Something wrong happened.")))

(defun sharry/configure-geiser-mode ()
  "Configure geiser mode."
  (local-set-key (kbd "<f5>")
                 'sharry/compile-and-run-scheme)
  (local-set-key (kbd "C-x C-e")
                 'sharry/compile-and-run-scheme)
  (local-set-key (kbd "<f8>")
                 (lambda ()
                   (interactive)
                   (sharry/delete-window-by-name sharry-chicken-repl-buffer-name))))

(defun sharry/configure-web-mode ()
  "Configure web mode."
  (local-set-key (kbd "<f5>")
                 'sharry/open-html-in-browser))

(defun sharry/open-hexo-blog (blog-path)
  "Open folder BLOG-PATH as a hexo blog."
  (interactive "DHexo blog path: ")
  (hexo blog-path))

(defun sharry/hexo-server-run-in-background ()
  "Run hexo server in the background and focus on the origin buffer."
  (interactive)
  (let ((origin-buffer-name (buffer-name)))
    (hexo-server-run)
    ;; Change to normal view so that view log is more convenient.
    (evil-normal-state)
    (switch-to-buffer origin-buffer-name))
  (run-at-time
   "13.0 sec" nil 'sharry/open-url-or-file
   sharry-local-hexo-server-default-address))

(defun sharry/configure-hexo-mode ()
  "Configure hexo mode."
  (local-set-key (kbd "n")
                 'hexo-new)
  (local-set-key (kbd "R")
                 'hexo-command-rename-file)
  (local-set-key (kbd "i")
                 'hexo-command-show-article-info)

  ;; RET is the Return key in a terminal.
  (local-set-key (kbd "<RET>")
                 'hexo-command-open-file)
  ;; return is the Return key in GUI.
  (local-set-key (kbd "<return>")
                 'hexo-command-open-file)

  (local-set-key (kbd "<f5>")
                 'sharry/hexo-server-run-in-background)
  (local-set-key (kbd "<f6>")
                 'hexo-server-stop))

(defun sharry/configure-pdf-view-mode ()
  "Configure pdf tools."
  (nlinum-mode -1))

(defun sharry/configure-dired-mode ()
  "Configure dired mode."
  (require 'all-the-icons-dired)
  (all-the-icons-dired-mode 1)

  (require 'diff-hl)
  (diff-hl-dired-mode 1)

  (if (file-exists-p sharry-default-diredful-config-file-path)
      (progn
        (require 'diredful)
        (defvar diredful-init-file sharry-default-diredful-config-file-path)
        (diredful-mode 1))
    (message "The diredful file is not existed.")))

(provide 'funs)

;;; funcs.el ends here
