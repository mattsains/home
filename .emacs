(load-file "~/.emacs.d/third-party/load-directory.el")

(setq files-to-load '("package-managers.el"
		      "smex.el"
		      "ido.el"
		      "ui.el"
		      "vanilla-overrides.el"))

(dolist (file files-to-load) (load-file (concat "~/.emacs.d/" file)))
