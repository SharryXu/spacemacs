(defconst sharry-async-shell-buffer-name "Async Shell Command")

(defconst sharry-chicken-repl-buffer-name "Chicken REPL")

(defconst sharry-default-clang-format-config-file-path "~/.clang-format")

(defconst sharry-default-diredful-config-file-path "~/.diredful-conf.el")

(defconst sharry-local-hexo-server-default-address "http://localhost:4000")

(defconst sharry-default-clang-format-style
  "{
     BasedOnStyle: LLVM,
     IndentWidth: 4,
     BreakBeforeBraces: Allman,
     AllowShortFunctionsOnASingleLine: false
  }"
  "Default Style. If no .clang-format file can be found.")

(defconst sharry-c-style
  '((indent-tabs-mode . nil)
    (c-basic-offset . 4)
    (tab-width . 4)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist . ((substatement-open before after)
                               (brace-list-open after)))
    (c-offsets-alist . ((statement-block-intro . +)
                        (substatement-open . 0)
                        (substatement-label . 0)
                        (label . 0)
                        (statement-cont . +)
                        (innamespace . 0)
                        (inline-open . 0)
                        ))
    (c-hanging-braces-alist .
                            ((brace-list-open)
                             (brace-list-intro)
                             (brace-list-entry)
                             (brace-list-close)
                             (brace-entry-open)
                             (block-close . c-snug-do-while)
                             ;; structs have hanging braces on open
                             (class-open . (after))
                             ;; ditto if statements
                             (substatement-open . (after))
                             ;; and no auto newline at the end
                             (class-close)
                             ))
    )
  "Sharry's own C Programming Style")

(c-add-style "sharry" sharry-c-style)
