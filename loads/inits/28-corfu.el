;;; 28-corfu.el --- バッファ内補完の設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; Corfu ベースのバッファ内補完 UI とその拡張

;;; Code:

;;;;; [Group] Corfu - バッファ内補完 ;;;;;
;;; Corfu - 補完 UI
(use-package corfu
  :straight t
  :init (global-corfu-mode)
  :bind
  (("C-M-i" . completion-at-point)
   :map corfu-map
   ("TAB"   . corfu-insert)
   ("<tab>" . corfu-insert)
   ("C-n"   . corfu-next)
   ("C-p"   . corfu-previous)
   ("C-s"   . corfu-scroll-down)
   ("M-n"   . nil)
   ("M-p"   . nil)
   )
  :custom
  (corfu-auto t)                              ; 自動補完
  (corfu-auto-delay 0)                        ; 入力後すぐに補完を表示
  (corfu-auto-prefix 2)                       ; プレフィックス長
  (corfu-cycle t)                             ; 候補リストのループ
  (corfu-on-exact-match nil)
  (corfu-preview-current nil)                 ; 現在の候補をプレビューしない
  (corfu-preselect 'prompt)                   ; 最初の候補を選択
  (corfu-scroll-margin 2)                     ; スクロールマージン
  (corfu-separator ?\s)                       ; orderless 用のセパレータ
  (completion-ignore-case t)                  ; 大文字小文字を区別しない
  (tab-always-indent 'complete)
  :config
  ;; c-mode などの一部のモードではタブに `c-indent-line-or-region` が割り当てられているので、
  ;; 補完が出るように `indent-for-tab-command` に置き換える
  (defun my/corfu-remap-tab-command ()
    (global-set-key [remap c-indent-line-or-region] #'indent-for-tab-command))
  (add-hook 'c-mode-hook #'my/corfu-remap-tab-command)
  (add-hook 'c++-mode-hook #'my/corfu-remap-tab-command)
  (add-hook 'java-mode-hook #'my/corfu-remap-tab-command)

  ;; ミニバッファー上でverticoによる補完が行われない場合、corfu の補完が出るように
  ;; https://github.com/minad/corfu#completing-in-the-minibuffer
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

  (with-eval-after-load 'meow
    (define-key corfu-map (kbd "<escape>")
                (lambda ()
                  (interactive)
                  (corfu-quit)
                  (meow-normal-mode))))

  ;; lsp-modeでcorfuが起動するように設定する
  (with-eval-after-load 'lsp-mode
    (setq lsp-completion-provider :none))

  (with-eval-after-load 'orderless
    (defun my/orderless-for-corfu ()
      (setq-local orderless-matching-styles '(orderless-flex)))

    (add-hook 'corfu-mode-hook #'my/orderless-for-corfu))
  )

;;; Corfu-terminal - 端末 (`-nw`) で `corfu` を有効化
(use-package corfu-terminal
  :straight (:type git :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :after corfu
  :unless (display-graphic-p)
  :config
  (corfu-terminal-mode +1)
  )

;;; corfu-popupinfo - 補完候補の横に説明用のポップアップを表示
(use-package corfu-popupinfo
  :straight nil
  :after corfu
  :if (display-graphic-p)
  :hook (corfu-mode . corfu-popupinfo-mode)
  )

;;; Cape - `corfu` の補完バックエンド
(use-package cape
  :straight t
  :hook (;; ((prog-mode text-mode conf-mode lsp-completion-mode) . my/set-super-capf)
         (emacs-lisp-mode . my/set-elisp-capf)
         (after-init . my/global-capf))
  :config
  ;; 全モード共通の補完関数
  (defun my/global-capf ()
    "Set up global `completion-at-point-functions` for all modes."
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
    (advice-add 'lsp-completion-at-point :around #'cape-wrap-buster)
    (advice-add 'lsp-completion-at-point :around #'cape-wrap-nonexclusive)
    (advice-add 'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
    (add-hook 'completion-at-point-functions #'cape-dabbrev)
    (add-hook 'completion-at-point-functions #'cape-keyword)
    (add-hook 'completion-at-point-functions #'cape-file)
    )

  ;; ;; プログラミング系モードの補完
  ;; (defun my/set-super-capf ()
  ;;   "Set up `completion-at-point-functions` with `cape`."
  ;;   (setq-local completion-at-point-functions
  ;;               (list (cape-capf-super
  ;;                      )))) ;; 現状何もなし

  ;; Emacs Lisp の補完
  (defun my/set-elisp-capf ()
    "Set up `completion-at-point-functions` for `elisp-mode`."
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'cape-elisp-symbol
                       #'cape-elisp-block))))

  ;; `M-x eval-expression` (`C-x C-e` など) でも補完可能に
  (add-hook 'eval-expression-minibuffer-setup-hook #'cape-elisp-symbol)

  ;; `yas-minor-mode` 有効時のみ cape-yasnippet を補完関数に追加
  (add-hook 'yas-minor-mode-hook
            (lambda ()
              (add-to-list 'completion-at-point-functions #'cape-yasnippet t)))

  ;; dabbrevのサイズを制限
  (setq dabbrev-friend-buffer-function (lambda (other-buffer)
                                         (< (buffer-size other-buffer) (* 1024 1024))))
  )

(provide '28-corfu)
;;; 28-corfu.el ends here
