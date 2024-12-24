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

(windmove-default-keybindings)
(setq windmove-wrap-around t)

;; ls does not support --diredの対策
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

;; ファイルの設定
(setq delete-auto-save-files t)
(setq backup-inhibited t)
;; Clipboard
(setq x-select-enable-clipboard t)

;; straight.el(パッケージマネージャ)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; インストールするパッケージのリスト
(defvar my/favorite-packages
  '(
    use-package
    exec-path-from-shell
    dracula-theme
    ))
;; my/favorite-packagesからインストールしていないパッケージをインストール
(dolist (package my/favorite-packages)
  (unless (package-installed-p package)
    (straight-use-package package)))

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
(load-theme 'dracula t)

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

(use-package rainbow-mode
  :straight t
  :init
  (add-hook 'web-mode-hook 'rainbow-mode)
  )

;; neotree
(use-package neotree
  :straight t
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
  :straight t
  :hook
  ((rust-mode . lsp)
   (go-mode . lsp)
   (python-mode . lsp)
   ;(web-mode . lsp)
   )
  :custom
  (lsp-rust-server 'rls)
  :commands lsp)

;; flycheck
(use-package flycheck
  :straight t)

(use-package corfu
  :straight (corfu :type git
                   :host github
                   :repo "minad/corfu"
                   :branch "async"
                   :files (:defaults "extensions/*"))
  :custom ((corfu-auto t)
           (corfu-auto-delay 0)
           (corfu-auto-prefix 1)
           (corfu-cycle t)
           (corfu-on-exact-match nil)
           (corfu-popupinfo-delay 0)
           (tab-always-indent 'complete))
  :init
  (global-corfu-mode +1)
  (corfu-popupinfo-mode +1)
  :config
  ;; lsp-modeでcorfuが起動するように設定する
  (with-eval-after-load 'lsp-mode
    (setq lsp-completion-provider :none)))

(use-package kind-icon
  :straight t
  :after corfu
  :custom (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Company mode is a standard completion package that works well with lsp-mode.
;(use-package company
;  :straight t
;  :config
;  (global-company-mode)
  ;; Optionally enable completion-as-you-type behavior.
;  (setq company-idle-delay 0)
;  (setq company-minimum-prefix-length 1)
;  (setq completion-ignore-case t)
;  (setq company-dabbrev-downcase nil)
;  (setq company-selection-wrap-around t))

;; 各種メジャーモードで C-M-i で company-modeの補完を使う
;(global-set-key (kbd "C-M-i") 'company-complete)
;(define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete)

;; copilotの設定
;(use-package copilot
;  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
;  :ensure t)
;(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
;(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

;; python-black
(use-package python-black
  :straight t)
;; python-mode
(use-package python-mode
  :straight t
  :mode ("\\.py$" . python-mode)
  :hook
  (python-mode . python-black-on-save-mode))

;; rust-mode
(use-package rust-mode
  :straight t
  :custom
  (rust-format-on-save t))
(use-package cargo
  :straight t
  :hook
  (rust-mode . cargo-minor-mode))

;; go-mode
(use-package go-mode
  :straight t
  :mode
  ("\\.go$" . go-mode)
  :hook
  (before-save . gofmt-before-save)
  :custom
  (gofmt-command "goimports"))

;; terraform-mode
(use-package terraform-mode
  :straight t
  :mode
  ("\\.tf$" . terraform-mode)
  :hook
  (terraform-mode . terraform-format-on-save-mode))

;; yaml-mode
(use-package yaml-mode
  :straight t
  :mode
  ("\\.ya?ml$" . yaml-mode)
  :config
  (define-key yaml-mode-map "\C-m" 'newline-and-indent))

;; protobuf-mode
(defconst my-protobuf-style
  '((c-basic-offset . 2)
    (indent-tabs-mode . nil)))
(use-package protobuf-mode
  :straight t
  :init
  :mode
  ("\\.proto$" . protobuf-mode)
  :config
  (add-hook 'protobuf-mode-hook
            (lambda () (c-add-style "my-style" my-protobuf-style t)))
  )

;; graphql-mode
(use-package graphql-mode
  :straight t
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

;; work around ts-ls bug
(advice-add 'json-parse-buffer :around
              (lambda (orig &rest rest)
                (while (re-search-forward "\\u0000" nil t)
                  (replace-match ""))
                (apply orig rest)))

;; web-mode
(use-package web-mode
  :straight t
  :init
  (setq web-mode-enable-auto-indentation nil)
  :mode (("\\.html?\\'" . web-mode)
         ("\\.json\\'" . web-mode)
         ("\\.css\\'" . web-mode)
         ("\\.ts[x]?\\'" . web-mode)
         ("\\.js[x]?\\'" . web-mode)
         ("\\.[mc]js\\'" . web-mode))
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
  :straight t
  :mode
  ("Dockerfile\\'" . dockerfile-mode))

(defun solidity-mode-hook ()
  (setq c-basic-offset 2)
  )
;; solodity-mode
(use-package solidity-mode
  :straight t
  :mode
  (("\\.sol\\'" . solidity-mode))
  :hook
  (solidity-mode . solidity-mode-hook))
