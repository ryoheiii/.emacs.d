;;; 括弧
(use-package paren
  :ensure t
  :hook
  (after-init . show-paren-mode)
  :config
  (setq show-paren-style 'mixed) ;; ウィンドウ内に収まらないときだけ括弧内も光らせる。
  )

;;; 空白
(use-package whitespace
  :ensure t
  :defer t
  :diminish
  (global-whitespace-mode whitespace-mode)
  :init
  (global-whitespace-mode t)
  :config
  (setq whitespace-style '(face       ; faceを使って視覚化する。
                           trailing   ; 行末
                           tabs       ; タブ
                           spaces     ; スペース
                           empty      ; 先頭/末尾の空行
                           space-mark ; 表示のマッピング
                           tab-mark
                           newline
                           ))

  (setq whitespace-display-mappings
        '(
          (space-mark ?\x3000 [?\u25a1])           ; 全角スペース
          (tab-mark   ?\t [?\u00BB ?\t] [?\\ ?\t]) ; タブ
          ))

  ;; スペースは全角のみを可視化
  (setq whitespace-space-regexp    "\\(\u3000+\\)")
  (setq whitespace-trailing-regexp "\\([ \u00A0]+\\)$")

  (defvar my/bg-color "Black")
  (set-face-attribute 'whitespace-trailing nil
                      :background my/bg-color
                      :foreground "DeepPink"
                      :underline t
                      )
  (set-face-attribute 'whitespace-tab nil
                      :background my/bg-color
                      :foreground "DarkMagenta"
                      :underline t
                      )
  (set-face-attribute 'whitespace-space nil
                      :background my/bg-color
                      :foreground "GreenYellow"
                      :weight 'bold
                      )
  (set-face-attribute 'whitespace-empty nil
                      :background my/bg-color
                      )
  )

(use-package grep
  :ensure t
  :bind (
         ("C-c g" . grep)
         )
  :config
  (setq grep-command-before-query "grep -nr -e ")

  (defun grep-default-command ()
    (if current-prefix-arg
        (let ((grep-command-before-target
               (concat grep-command-before-query
                       (shell-quote-argument (grep-tag-default)))))
          (cons (if buffer-file-name
                    (concat grep-command-before-target
                            " *."
                            (file-name-extension buffer-file-name))
                  (concat grep-command-before-target " ."))
                (+ (length grep-command-before-target) 1)))
      (car grep-command)))
  (setq grep-command (cons (concat grep-command-before-query " .")
                           (+ (length grep-command-before-query) 1)))
  )

;; ;; 括弧の補完
;; (use-package elec-pair
;;   :ensure t
;;   :config
;;   (electric-pair-mode +1)
;;   )

;; highlight the current line
;; (use-package hl-line
;;   :ensure t
;;   :config
;;   (global-hl-line-mode +1))

;;; バッファ名
;; ファイル名が重複していたらディレクトリ名を追加する。
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*")
  )

(use-package savehist
  :ensure t
  :defer 20
  :init
  (savehist-mode 1)
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (my-set-history "savehist") ;; my-set-history @00-auto-file-place.el
        )
  )

;; カーソルの場所を保存する
(use-package saveplace
  :ensure t
  :init
  (setq-default save-place t)
  (setq save-place-limit nil)
  (save-place-mode 1)
  :config
  (setq save-place-file (my-set-history "places"))        ;; my-set-history @00-auto-file-place.el
  (setq eshell-directory-name (my-set-history "eshell/")) ;; my-set-history @00-auto-file-place.el
  )

(use-package windmove
  :ensure t
  :config
  ;; use shift + arrow keys to switch between visible buffers
  (windmove-default-keybindings)
  )
