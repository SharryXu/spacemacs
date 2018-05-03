;;; packages.el --- sharry layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sharry R Xu <sharry.r.xu@gmail.com>
;; URL: https://github.com/SharryXu/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq sharry-packages
      '(
        ;; Based on the changelog in master branch,
        ;; this package has some issues in some terminals.
        evil-terminal-cursor-changer
        osx-clipboard
        diredful
        dired-icon
        ac-geiser
        evil-smartparens
        diff-hl
        hexo
        evil-multiedit
        ))

(defun sharry/init-evil-terminal-cursor-changer ()
  (use-package evil-terminal-cursor-changer
    :defer t
    :config
    ;; evil-normal-state-cursor
    ;; evil-insert-state-cursor
    ;; evil-visual-state-cursor
    ;; evil-motion-state-cursor
    ;; evil-replace-state-cursor
    ;; evil-operator-state-cursor
    (setq evil-insert-state-cursor '((bar . 8) "blue")
          evil-normal-state-cursor '(box "blue"))
    :init
    (unless (window-system)
      (evil-terminal-cursor-changer-activate))))

(defun sharry/init-osx-clipboard ()
  (use-package osx-clipboard
    :defer t
    :init
    (require 'core-funcs)
    (when (spacemacs/system-is-mac)
      (osx-clipboard-mode))))

(defun sharry/init-diredful ()
  (use-package diredful))

(defun sharry/init-dired-icon ()
  (use-package dired-icon))

(defun sharry/init-ac-geiser ()
  (use-package ac-geiser
    :defer t))

(defun sharry/init-evil-smartparens ()
  (use-package evil-smartparens
    :init
    (require 'smartparens)
    (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)))

(defun sharry/init-diff-hl ()
  (use-package diff-hl
    :defer t))

(defun sharry/init-hexo ()
  (use-package hexo
    :defer t))

(defun sharry/init-evil-multiedit ()
  (use-package evil-multiedit
    :defer t
    :config
    (evil-multiedit-default-keybinds)))

;;; packages.el ends here
