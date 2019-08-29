;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;         main emacs config
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)

(setq debug-on-error t)

(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'init-env)
(require 'init-ui)
(require 'init-packages)
(require 'init-functions)
(require 'init-org-mode)
(require 'init-keybindings)

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;  Start server for emacsclient
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;(if (window-system)
;;    (server-start))

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

(provide 'init)
;;; init.el ends here
