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
        json-mode
        evil-terminal-cursor-changer
        wakatime-mode
        osx-clipboard
        emojify
        diredful
        dired-icon
        ac-geiser
        evil-smartparens
        ))

(defun sharry/init-json-mode ()
  (use-package json-mode
    :defer t
    :init
    (message "Initializing json mode...")))

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
    (unless (window-system)
      (setq evil-insert-state-cursor '((bar . 8) "blue")
            evil-normal-state-cursor '(box "blue")))))

(defun sharry/init-wakatime-mode ()
  (use-package wakatime-mode
    :defer t
    :init
    (global-wakatime-mode)))

(defun sharry/init-osx-clipboard ()
  (use-package osx-clipboard
    :defer t
    :init
    (require 'core-funcs)
    (when (spacemacs/system-is-mac)
      (osx-clipboard-mode))))

(defun sharry/init-emojify ()
  (use-package emojify
    :defer t
    :config
    ;; Avoid download emoji png files under the .emacs.d folder.
    (setq emojify-emojis-dir (file-truename sharry-default-emojis-dir))
    :init
    (global-emojify-mode)))

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

;;; packages.el ends here
