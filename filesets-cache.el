(setq-default filesets-be-docile-flag 'nil)

(setq filesets-submenus '("" ("0 " ["Files: " (filesets-open (quote :files) (quote ""))] "---" ["0 Commands.org" (filesets-file-open nil (quote "c:/ORG/Computing/Emacs/Commands.org") (quote ""))] "---" ["Close all files" (filesets-close (quote :files) (quote ""))] ["Run Command" (filesets-run-cmd nil (quote "") (quote :files))] ["Add current buffer" (filesets-add-buffer (quote "") (current-buffer))] ["Remove current buffer" (filesets-remove-buffer (quote "") (current-buffer))] ["Rebuild this submenu" (filesets-rebuild-this-submenu (quote ""))])))

(setq filesets-menu-cache 'nil)

(setq filesets-ingroup-cache 'nil)

(setq filesets-cache-version "1.8.4")

