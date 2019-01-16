;; The GNU project defaults.  These are also the GNU Emacs defaults.
;; Re-asserting theme here, however, as a courtesy for setups that use
;; a global override.
((nil . ((fill-column . 72)
         (tab-width   .  8)))

 ;; For writing GNU C code, see
 ;; https://www.gnu.org/prep/standards/html_node/Writing-C.html
 (c-mode . ((c-file-style . "gnu")
            (indent-tabs-mode . nil)))

 ;; *OVERRIDE* to ... what is used while refactoring?
 (c-mode . ((c-file-style . "linux")
            (indent-tabs-mode . t))))
