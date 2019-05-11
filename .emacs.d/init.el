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
(setq-default indent-tabs-mode nil)
;; ファイルの設定
(setq delete-auto-save-files t)
(setq backup-inhibited t)
;; Clipboard
(setq x-select-enable-clipboard t)

;; パッケージ管理サーバ
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
;; パッケージ情報の更新
;(package-refresh-contents)

;; インストールするパッケージのリスト
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

;; シェルに設定されている環境変数を引き継ぐ
(exec-path-from-shell-initialize)

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
;; M-.で定義ジャンプ、M-,でジャンプ先からもどる
;; eglot はデフォルトの Language Server として go-langserver を使うので golsp に変更する
;; 事前に go get -u golang.org/x/tools/cmd/gopls しておくこと
(use-package eglot
  :config
  (define-key eglot-mode-map (kbd "M-.") 'xref-find-definitions)
  (define-key eglot-mode-map (kbd "M-,") 'pop-tag-mark)
  (add-to-list 'eglot-server-programs '(go-mode . ("gopls")))
  (add-hook 'go-mode-hook 'eglot-ensure))

;; company-mode
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
;; global-whitespace-modeを使うときはindent-tabs-modeをnilにすること、companyが誤作動する
;; 事前に go get golang.org/x/tools/cmd/goimports しておくこと
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (protobuf-mode yaml-mode use-package go-mode exec-path-from-shell eglot company atom-dark-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
