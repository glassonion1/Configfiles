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
(eval-and-compile
  (customize-set-variable
   'package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("org"   . "https://orgmode.org/elpa/")))
  (package-initialize))

;; パッケージ情報の更新
;(package-refresh-contents)

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
;(setq exec-path-from-shell-check-startup-files nil)

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
;(use-package eglot
;  :ensure t
;  :config
;  (add-to-list 'eglot-server-programs
;               '(go-mode . ("gopls")))
;  :bind (:map eglot-mode-map
;              ("C-c C-d" . eglot-help-at-point)
;              ("C-c C-r" . eglot-code-actions))
;  :hook((go-mode-hook . eglot-ensure)
;        (typescript-mode-hook . eglot-ensure)
;        (rust-mode-hook . eglot-ensure))
;  )

;; Bridge projectile and project together so packages that depend on project
;; like eglot work
;(use-package projectile
;  :ensure t)
;(defun my-projectile-project-find-function (dir)
;  (let ((root (projectile-project-root dir)))
;    (and root (cons 'transient root))))
;(projectile-mode t)
;(with-eval-after-load 'project
;  (add-to-list 'project-find-functions 'my-projectile-project-find-function))

;; lsp-mode
;; プロジェクトルートで M-x lsp-workspace-folder-add を実行すること
(use-package lsp-mode
  :ensure t
  :hook
  ((rust-mode
    go-mode
    python-mode
    typescript-mode
    ) . lsp-deferred)
  :custom
  (lsp-rust-server 'rls)
  (lsp-message-project-root-warning t)
  (create-lockfiles nil)
  :commands lsp)
;; ローカル変数にLSPを適応させる
(add-hook 'hack-local-variables-hook
          (lambda () (when (derived-mode-p 'go-mode) (lsp))))
(add-hook 'hack-local-variables-hook
          (lambda () (when (derived-mode-p 'tide-mode) (lsp))))

;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
  :ensure t
  :hook
  (after-init . global-company-mode)
  :custom
  ;; Optionally enable completion-as-you-type behavior.
  (lsp-completion-provider :capf)
  (company-idle-delay 0)
  (company-minimum-prefix-length 1)
  (company-dabbrev-downcase nil)
  (company-selection-wrap-around t))
;; 各種メジャーモードで C-M-i で company-modeの補完を使う
(define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete)

;; python-black
(use-package python-black
  :ensure t)
;; python-mode
(use-package python-mode
  :ensure t
  :mode ("\\.py$" . python-mode)
  :hook
  (python-mode . python-black-on-save-mode))

;; rust-mode
(use-package rust-mode
  :ensure t
  :custom
  (rust-format-on-save t))
(use-package cargo
  :ensure t
  :hook
  (rust-mode . cargo-minor-mode))

;; go-mode
(use-package go-mode
  :ensure t
  :mode
  ("\\.go$" . go-mode)
  :hook
  (before-save . gofmt-before-save)
  :custom
  (gofmt-command "goimports"))

;; terraform-mode
(use-package terraform-mode
  :ensure t
  :mode
  ("\\.tf$" . terraform-mode)
  :hook
  (terraform-mode . terraform-format-on-save-mode))

;; yaml-mode
(use-package yaml-mode
  :ensure t
  :mode
  ("\\.ya?ml$" . yaml-mode)
  :config
  (define-key yaml-mode-map "\C-m" 'newline-and-indent))

;; protobuf-mode
(defconst my-protobuf-style
  '((c-basic-offset . 2)
    (indent-tabs-mode . nil)))
(use-package protobuf-mode
  :ensure t
  :init
  :mode
  ("\\.proto$" . protobuf-mode)
  :config
  (add-hook 'protobuf-mode-hook
            (lambda () (c-add-style "my-style" my-protobuf-style t)))
  )

;; graphql-mode
(use-package graphql-mode
  :ensure t
  :mode
  ("\\.graphqls$" . graphql-mode))

;; typescript 
(use-package typescript-mode
  :ensure t
  :hook
  (typescript-mode . subword-mode)
  :custom
  (typescript-indent-level 2))

(defun my/prettier ()
  (interactive)
  (shell-command
    (format "%s --write %s"
      (shell-quote-argument (executable-find "prettier"))
      (shell-quote-argument (expand-file-name buffer-file-name))))
  (revert-buffer t t t))

;; setup tide mode
(use-package tide
  :ensure t
  :hook
  (before-save . my/prettier))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

;; web-mode
(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.json\\'" . web-mode)
         ("\\.ts\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.js\\'" . web-mode)
         ("\\.jsx\\'" . web-mode))
  :custom
  (web-mode-attr-indent-offset nil)
  (web-mode-enable-auto-closing t)
  (web-mode-enable-auto-pairing t)
  (web-mode-auto-close-style 2)
  (web-mode-tag-auto-close-style 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-current-element-highlight t)
  (indent-tabs-mode nil)
  (tab-width 2)
  :config
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "ts" (file-name-extension buffer-file-name))
                (setup-tide-mode))))
  )


(defun solidity-mode-hook ()
  (setq c-basic-offset 2)
  )
;; solodity-mode
(use-package solidity-mode
  :ensure t
  :mode
  (("\\.sol\\'" . solidity-mode))
  :hook
  (solidity-mode . solidity-mode-hook))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("org" . "https://orgmode.org/elpa/")))
 '(package-selected-packages
   '(solidity-mode tide graphql-mode protobuf-mode yaml-mode go-mode cargo rust-mode company projectile eglot neotree atom-dark-theme exec-path-from-shell use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
