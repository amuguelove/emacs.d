
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;  Customize Key Binding
;;  Notice: some key bingdings defined in init-packages.el
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


;; for zsh
(global-set-key (kbd "C-w") 'backward-kill-word)

(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)

;; 主动加载 Dired Mode
;; (require 'dired)
;; (defined-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)

;; 延迟加载
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))


(provide 'init-keybindings)
