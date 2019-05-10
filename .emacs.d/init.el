;; デフォルト文字コード
(prefer-coding-system 'utf-8)
;; Title
(setq frame-title-format (format "emacs@%s : %%f" (system-name)))
;; キー
(global-set-key "\C-h" 'delete-backward-char)
(define-key minibuffer-local-completion-map "\C-w" 'backward-kill-word)
;; MacでGUIの時、optionをmeta
(if window-system (progn
                    (when (equal system-type 'darwin)
                      (setq mac-option-modifier 'meta))
                    ))
;; ファイルの設定
(setq delete-auto-save-files t)
(setq backup-inhibited t)
;; Clipboard
(setq x-select-enable-clipboard t)

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
    protobuf-mode
    ))
;; my/favorite-packagesからインストールしていないパッケージをインストール
(dolist (package my/favorite-packages)
  (unless (package-installed-p package)
    (package-install package)))

;; 濁点分離問題
(use-package ucs-normalize
  :config
  (setq file-name-coding-system 'utf-8-hfs)
  (setq local-coding-system 'utf-8-hfs))
;; テーマと色
(load-theme 'atom-dark t)

;; 全角スペース タブ trailing-spacesを目立たせる
(use-package whitespace
  :config
  (setq whitespace-style
        '(tabs tab-mark spaces space-mark trailing))
  (setq whitespace-display-mappings
        '((space-mark ?\u3000 [?\u25a1])
          (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
  (setq whitespace-space-regexp "\\(\u3000+\\)")
  (set-face-foreground 'whitespace-space "LightSlateGray")
  (set-face-background 'whitespace-space "DarkSlateGray")
  (set-face-foreground 'whitespace-tab "LightSlateGray")
  (set-face-background 'whitespace-tab "DarkSlateGray"))
(global-whitespace-mode 1)

;; eglot
(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(go-mode . ("gopls")))
  (add-hook 'go-mode-hook 'eglot-ensure))

(use-package company
  :config
  (global-company-mode)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq completion-ignore-case t)
  (setq company-dabbrev-downcase nil)
  (setq company-selection-wrap-around t))

;; yaml-mode
(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
  (define-key yaml-mode-map "\C-m" 'newline-and-indent))

;; protobuf-mode
(defconst my-protobuf-style
  '((c-basic-offset . 4)
    (indent-tabs-mode . nil)))
(use-package protobuf-mode
  :init
  (add-hook 'protobuf-mode-hook
            (lambda () (c-add-style "my-style" my-protobuf-style t))))

;; go-mode
;; global-whitespace-modeを使うときはindent-tabs-modeをnilにすること(companyが誤動作する)
(let ((envs '("GOROOT" "GOPATH")))
  (exec-path-from-shell-copy-envs envs))
(use-package go-mode
  :commands go-mode
  :defer t
  :init
  (add-hook 'go-mode-hook (lambda()
                            (setq indent-tabs-mode nil)
                            (setq c-basic-offset 4)
                            (setq tab-width 4)
                            ))
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))
