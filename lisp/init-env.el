;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;  Setup pkg repo and install use-package
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(require 'package)
(setq package-enable-at-startup nil)

(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/") t))
(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/") t))
(unless (assoc-default "marmalade" package-archives)
  (add-to-list 'package-archives '("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t)


;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;  setup coding system and window property
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(prefer-coding-system 'utf-8)
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LC_CTYPE" "en_US.UTF-8")


;; setup titlebar appearance
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))


;; useful mode settings and better defaults
(display-time-mode 1)
(column-number-mode 1)
(show-paren-mode nil)
(display-battery-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(global-auto-revert-mode t)
(global-hl-line-mode nil)
; (global-linum-mode t)
(delete-selection-mode t)

(fset 'yes-or-no-p 'y-or-n-p)


;(toggle-frame-fullscreen)
(setq initial-frame-alist (quote ((fullscreen . maximized))))
; (set-frame-width (selected-frame) 120)
; (set-frame-height (selected-frame) 40)

;; file edit settings
(setq tab-width 4
      inhibit-splash-screen t
      initial-scratch-message nil
      sentence-end-double-space nil
      make-backup-files nil
      indent-tabs-mode nil
      make-backup-files nil
      auto-save-default nil
      ring-bell-function 'ignore
      )

(setq-default cursor-type 'bar
              abbrev-mode t)


;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;  dired mode settings
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

(put 'dired-find-alternate-file 'disabled nil)

;; if there is a dired buffer displayed in the next window, use its
;; current subdir, instead of the current subdir of this dired buffer
(setq dired-dwim-target t)


;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;  setup history of edited file
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(savehist-mode 1)
(setq savehist-file "~/.emacs.d/.savehist")
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))


;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;  operation system settings
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; (cond ((string-equal system-type "darwin")
;        (progn
;      ;; modify option and command key
;      (setq mac-command-modifier 'control)
;      (setq mac-option-modifier 'meta)

;      ;; batter copy and paste support for mac os x
;      (defun copy-from-osx ()
;        (shell-command-to-string "pbpaste"))
;      (defun paste-to-osx (text &optional push)
;        (let ((process-connection-type nil))
;          (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
;            (process-send-string proc text)
;            (process-send-eof proc))))
;      (setq interprogram-cut-function 'paste-to-osx)
;      (setq interprogram-paste-function 'copy-from-osx)

;      (message "Wellcome To Mac OS X, Have A Nice Day!!!"))))


;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;  coding font for english and chinese
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(set-face-attribute 'default nil
                    :family "Source Code Pro for Powerline"
                    :height 140
                    :weight 'medium
                    :width 'medium)

(if (display-graphic-p)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
            charset (font-spec :family "Microsoft Yahei"
                       :size 14)))
)


;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;  misc settings
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(setq custom-file "~/.emacs.d/lisp/custom.el")
(load custom-file t)


(provide 'init-env)
