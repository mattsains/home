(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(setq visible-bell t
      inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode
      transient-mark-mode nil)

;; Quick yes and no
(defalias 'yes-or-no-p 'y-or-n-p)
