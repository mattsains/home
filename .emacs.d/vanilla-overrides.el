;; Make regex search the default
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Never use tabs for indent
(setq-default indent-tabs-mode nil)

;; Put the clipboard paste into the killring before emacs kill
(setq save-interprogram-paste-before-kill t)

;; Put backups in a more sensible place
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
"backups"))))
