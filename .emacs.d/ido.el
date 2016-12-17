(ido-mode t)

(setq ido-enable-flex-matching t)

;; Show completions vertically
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))

 ; sort ido filelist by mtime instead of alphabetically
