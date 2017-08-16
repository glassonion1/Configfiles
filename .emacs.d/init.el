(require 'ucs-normalize)
(prefer-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8-hfs)
(setq local-coding-system 'utf-8-hfs)

; パッケージ管理
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
; Elisp置き場
(add-to-list 'load-path "~/.emacs.d/elisp/")
(let ((default-directory "~/.emacs.d/elisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;;----- MacでGUIの時、optionをmeta
(if window-system (progn
    (when (equal system-type 'darwin)
      (setq mac-option-modifier 'meta))
    ))

; General Settings
(global-set-key "\C-h" 'delete-backward-char)
(setq-default indent-tabs-mode nil)       ; インデントはタブではなくスペースを使用
(setq-default show-trailing-whitespace t) ; 行末の空白をハイライト
(global-font-lock-mode t)
(require 'font-lock)
(setq transient-mark-mode t)
(setq delete-auto-save-files t)
(setq backup-inhibited t)
(define-key minibuffer-local-completion-map "\C-w" 'backward-kill-word)
; Title
(setq frame-title-format (format "emacs@%s : %%f" (system-name)))
; Clipboard
(setq x-select-enable-clipboard t)
; テーマ
(unless (package-installed-p 'atom-dark-theme)
  (package-refresh-contents) (package-install 'atom-dark-theme))
(load-theme 'atom-dark t)

; 全角スペース タブ trailing-spacesを目立たせる
(setq whitespace-style
      '(tabs tab-mark spaces space-mark))
(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-display-mappings
      '((space-mark ?\x3000 [?\□])
        (tab-mark   ?\t   [?\xBB ?\t])
        ))
(require 'whitespace)
(global-whitespace-mode 1)
(set-face-foreground 'whitespace-space "LightSlateGray")
(set-face-background 'whitespace-space "DarkSlateGray")
(set-face-foreground 'whitespace-tab "LightSlateGray")
(set-face-background 'whitespace-tab "DarkSlateGray")

;; CSS mode
(autoload 'css-mode "css-mode")
(setq auto-mode-alist
      (cons '("\\.scss\\'" . css-mode) auto-mode-alist))

;(setq js-indent-level 2)

(add-hook
 'sgml-mode-hook
 (lambda ()
   (setq
    sgml-basic-offset 4
    )))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (plantuml-mode ## atom-dark-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; puml
(setq org-plantuml-jar-path "~/plantuml.jar")
(defun org-mode-init ()
  (org-babel-do-load-languages
   'org-babel-load-languages
   (add-to-list 'org-babel-load-languages '(plantuml . t))))
(add-hook 'org-mode-hook 'org-mode-init)
