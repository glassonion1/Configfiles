;; デフォルト文字コード
(prefer-coding-system 'utf-8)
;; キー
(global-set-key "\C-h" 'delete-backward-char)
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
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

;; パッケージ情報の更新
(package-refresh-contents)

;; インストールするパッケージのリスト
(defvar my/favorite-packages
  '(
    use-package
    exec-path-from-shell
    atom-dark-theme
    ))
;; my/favorite-packagesからインストールしていないパッケージをインストール
(dolist (package my/favorite-packages)
  (unless (package-installed-p package)
    (package-install package)))

;; シェルに設定されている環境変数を引き継ぐ
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(setq exec-path-from-shell-check-startup-files nil)

;; 濁点分離問題
(use-package ucs-normalize
  :config
  (setq file-name-coding-system 'utf-8-hfs)
  (setq local-coding-system 'utf-8-hfs))
;; テーマと色
;(load-theme 'manoj-dark t)
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

;; neotree
(use-package neotree
  :ensure t
  :init
  (setq-default neo-keymap-style 'concise)
  :config
  (setq neo-show-hidden-files t)
  (setq neo-smart-open t)
  (setq neo-create-file-auto-open t)
  (bind-key [f8] 'neotree-toggle)
  )

;; eglot
;; M-.で定義ジャンプ、M-,でジャンプ先からもどる
;; eglot はデフォルトの Language Server として go-langserver を使うので golsp に変更する
;; 事前にLSPのインストールをしておくこと
;;   go get -u golang.org/x/tools/cmd/gopls
;;   rustup component add rls rust-src
;; プロジェクトルートには.projectileを置くこと
(use-package eglot
  :ensure t
  :config
  (setq eglot-connect-timeout 3)
  (define-key eglot-mode-map (kbd "M-.") 'xref-find-definitions)
  (define-key eglot-mode-map (kbd "M-,") 'pop-tag-mark)
  (define-key eglot-mode-map (kbd "C-c C-d") 'eglot-help-at-point)
  (define-key eglot-mode-map (kbd "C-c C-r") 'eglot-code-actions)
  (add-to-list 'eglot-server-programs
               '(go-mode . ("gopls")))
  (add-hook 'go-mode-hook 'eglot-ensure)
  (add-hook 'typescript-mode-hook 'eglot-ensure)
  (add-hook 'rust-mode-hook 'eglot-ensure))

;; Bridge projectile and project together so packages that depend on project
;; like eglot work
(use-package projectile
  :ensure t)
(defun my-projectile-project-find-function (dir)
  (let ((root (projectile-project-root dir)))
    (and root (cons 'transient root))))
(projectile-mode t)
(with-eval-after-load 'project
  (add-to-list 'project-find-functions 'my-projectile-project-find-function))

;; lsp-mode
;; プロジェクトルートで M-x lsp-workspace-folder-add を実行すること
;(use-package lsp-mode
;  :ensure t ;自動インストール
;  :custom ((lsp-inhibit-message t)
;         (lsp-message-project-root-warning t)
;         (create-lockfiles nil))
;  :hook
;  (prog-major-mode . lsp-prog-major-mode-enable)
;  :config
;  (setq lsp-response-timeout 5))
;(add-hook 'hack-local-variables-hook
;          (lambda () (when (derived-mode-p 'go-mode) (lsp))))
;; company-lsp integrates company mode completion with lsp-mode.
;; completion-at-point also works out of the box but doesn't support snippets.
;(use-package company-lsp
;  :ensure t
;  :commands company-lsp)

;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  ;; Optionally enable completion-as-you-type behavior.
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-dabbrev-downcase nil)
  (setq company-selection-wrap-around t))
;; 各種メジャーモードでも C-M-iで company-modeの補完を使う
(define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete)

;; rust-mode
(use-package rust-mode
  :ensure t
  :custom rust-format-on-save t)
(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

;; go-mode
(use-package go-mode
  :ensure t
  :commands go-mode
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))

;; yaml-mode
(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
  (define-key yaml-mode-map "\C-m" 'newline-and-indent))

;; protobuf-mode
(defconst my-protobuf-style
  '((c-basic-offset . 2)
    (indent-tabs-mode . nil)))
(use-package protobuf-mode
  :ensure t
  :init
  (add-hook 'protobuf-mode-hook
            (lambda () (c-add-style "my-style" my-protobuf-style t)))
  :config
  (add-to-list 'auto-mode-alist '("\\.proto$" . protobuf-mode))
  )

;; graphql-mode
(use-package graphql-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.graphqls$" . graphql-mode))
  )

;; typescript
(use-package tide
  :ensure t)
(use-package typescript-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
  (add-hook 'typescript-mode-hook
          '(lambda ()
             (interactive)
             (tide-setup)
             (tide-hl-identifier-mode +1)
             (make-local-variable 'typescript-indent-level)
             (setq typescript-indent-level 2)
             )))

;(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
; '(package-selected-packages
;   '(package-utils jsonrpc flymake flymake-go protobuf-mode yaml-mode use-package go-mode exec-path-from-shell eglot company atom-dark-theme)))
;(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
; )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(cargo rust-mode yaml-mode use-package tide protobuf-mode projectile neotree go-mode exec-path-from-shell eglot company atom-dark-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
