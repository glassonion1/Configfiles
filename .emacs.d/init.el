(require 'ucs-normalize)
(prefer-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8-hfs)
(setq local-coding-system 'utf-8-hfs)

;;----- MacでGUIの時、optionをmeta
(if window-system (progn
                    (when (equal system-type 'darwin)
                      (setq mac-option-modifier 'meta))
                    ))

;; パッケージ管理
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
;; パッケージ情報の更新
;(package-refresh-contents)

;; インストールするパッケージ
(defvar my/favorite-packages
  '(
    go-mode
    eglot
    company
    use-package
    atom-dark-theme
    yaml-mode
    exec-path-from-shell
    ))
;; my/favorite-packagesからインストールしていないパッケージをインストール
(dolist (package my/favorite-packages)
  (unless (package-installed-p package)
    (package-install package)))

;; eglot
(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(go-mode . ("gopls")))
  (add-hook 'go-mode-hook 'eglot-ensure)
  )

(use-package company
  :config
  (global-company-mode)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)
  )

;; General Settings
(global-set-key "\C-h" 'delete-backward-char)
(setq-default indent-tabs-mode nil)       ; インデントはタブではなくスペースを使用
(setq-default show-trailing-whitespace t) ; 行末の空白をハイライト
(global-font-lock-mode t)
(require 'font-lock)
(setq transient-mark-mode t)
(setq delete-auto-save-files t)
(setq backup-inhibited t)
(define-key minibuffer-local-completion-map "\C-w" 'backward-kill-word)

;; Title
(setq frame-title-format (format "emacs@%s : %%f" (system-name)))
;; Clipboard
(setq x-select-enable-clipboard t)
;; テーマ
(load-theme 'atom-dark t)

;; 全角スペース タブ trailing-spacesを目立たせる
(setq whitespace-style
      '(tabs tab-mark spaces space-mark))
(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-display-mappings
      '((space-mark ?\x3000 [?\□])
        (tab-mark   ?\t   [?\xBB ?\t])
        ))
(use-package whitespace
  :config
  (set-face-foreground 'whitespace-space "LightSlateGray")
  (set-face-background 'whitespace-space "DarkSlateGray")
  (set-face-foreground 'whitespace-tab "LightSlateGray")
  (set-face-background 'whitespace-tab "DarkSlateGray")
  )
;; company-modeがバグるので下記設定は止める
;(global-whitespace-mode 1)

;; yaml-mode
(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
  (define-key yaml-mode-map "\C-m" 'newline-and-indent)
  )

;; go-mode
(use-package go-mode
  :init
  (add-hook 'go-mode-hook (lambda()
                            (setq tab-width 4)))
  :config
  (let ((envs '("GOROOT" "GOPATH")))
    (exec-path-from-shell-copy-envs envs))
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook' 'gofmt-before-save)
  )

