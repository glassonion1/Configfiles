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

;; ls does not support --diredの対策
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

;; ファイルの設定
(setq delete-auto-save-files t)
(setq backup-inhibited t)
;; Clipboard
(setq x-select-enable-clipboard t)

;; package-selected-packages対策
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

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

;; lsp-mode
;; プロジェクトルートで M-x lsp-workspace-folder-add を実行すること
(use-package lsp-mode
  :ensure t
  :hook
  ((rust-mode . lsp)
   (go-mode . lsp)
   (python-mode . lsp)
   ;(web-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :custom
  (lsp-rust-server 'rls)
  :commands lsp)

;; flycheck
(use-package flycheck
  :ensure t)

;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
  :ensure t
  :config
  (global-company-mode)
  ;; Optionally enable completion-as-you-type behavior.
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq completion-ignore-case t)
  (setq company-dabbrev-downcase nil)
  (setq company-selection-wrap-around t))

;; 各種メジャーモードで C-M-i で company-modeの補完を使う
(global-set-key (kbd "C-M-i") 'company-complete)
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

(defun my/prettier ()
  (interactive)
  (shell-command
    (format "%s --write %s"
      (shell-quote-argument (executable-find "prettier"))
      (shell-quote-argument (expand-file-name buffer-file-name))))
  (revert-buffer t t t))

(defun my/web-mode-tsx-hook ()
  (let ((ext (file-name-extension buffer-file-name)))
    (when (or (string-equal "ts" ext) (string-equal "tsx" ext))
      (lsp))))

;; web-mode
(use-package web-mode
  :ensure t
  :init
  (setq web-mode-enable-auto-indentation nil)
  :mode (("\\.html?\\'" . web-mode)
         ("\\.json\\'" . web-mode)
         ("\\.css\\'" . web-mode)
         ("\\.ts[x]?\\'" . web-mode)
         ("\\.js[x]?\\'" . web-mode))
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
  (electric-indent-mode -1)
  (tab-width 2)
  :config
  (add-hook 'web-mode-hook
            (lambda ()
              (add-hook 'after-save-hook 'my/prettier t t)))
  :hook
  (web-mode . my/web-mode-tsx-hook)
  )

;; docker-mode
(use-package dockerfile-mode
  :ensure t
  :mode
  ("Dockerfile\\'" . dockerfile-mode))

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
