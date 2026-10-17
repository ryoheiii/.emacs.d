;;; 28-corfu.el --- バッファ内補完の設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; Corfu ベースのバッファ内補完 UI とその拡張

;;; Code:

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
   ("M-p"   . nil))
  :custom
  (corfu-auto t)                              ; 自動補完
  (corfu-auto-delay 0)                        ; 入力後すぐに補完を表示
  (corfu-auto-prefix 2)                       ; 基本値は 2 文字（27-consult.el のフックでは 1 文字）
  (corfu-cycle t)                             ; 候補リストのループ
  (corfu-on-exact-match nil)
  (corfu-preview-current nil)                 ; 現在の候補をプレビューしない
  (corfu-preselect 'prompt)                   ; プロンプトを選択（候補は C-n / C-p で選ぶ）
  (corfu-scroll-margin 2)                     ; スクロールマージン
  (corfu-separator ?\s)                       ; orderless 用のセパレータ
  (completion-ignore-case t)                  ; 大文字小文字を区別しない
  (tab-always-indent 'complete)
  ;; ミニバッファ: Vertico/Mct 使用時とパスワード入力時は Corfu を無効化
  ;; read-string 等の基本ミニバッファ (minibuffer-local-map) でも無効
  (global-corfu-minibuffer
   (lambda ()
     (not (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input)
              (eq (current-local-map) read-passwd-map)
              (eq (current-local-map) minibuffer-local-map)))))
  :config
  ;; c-mode などの一部のモードではタブに `c-indent-line-or-region` が割り当てられているので、
  ;; 補完が出るように `indent-for-tab-command` に置き換える。
  ;; c-ts-mode / c++-ts-mode は TAB を再束縛せず `tab-always-indent' がそのまま効くため、
  ;; ts モード用のフックは不要（これは cc-mode 固有の事情への対処である）。
  (defun my/corfu-remap-tab-command ()
    (keymap-global-set "<remap> <c-indent-line-or-region>" #'indent-for-tab-command))
  (add-hook 'c-mode-hook #'my/corfu-remap-tab-command)
  (add-hook 'c++-mode-hook #'my/corfu-remap-tab-command)
  (add-hook 'java-mode-hook #'my/corfu-remap-tab-command))

;;; Corfu-terminal - 標準の tty 子フレームがない Emacs で端末補完を有効化
(use-package corfu-terminal
  :straight (:type git :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :commands corfu-terminal-mode
  :init
  (unless (featurep 'tty-child-frames)
    (corfu-terminal-mode +1)))

;;; corfu-popupinfo - 補完候補の横に説明用のポップアップを表示
;; ポップアップは子フレームで描画する。Emacs 31.1 からは tty でも子フレームが使えるため
;; (`tty-child-frames')、GUI に加えて端末でも有効にする。corfu-terminal 経路では描けない。
(use-package corfu-popupinfo
  :straight nil
  :after corfu
  :if (or (display-graphic-p) (featurep 'tty-child-frames))
  :hook (corfu-mode . corfu-popupinfo-mode))

;;; Cape - `corfu` の補完バックエンド
(use-package cape
  :straight t
  :hook ((emacs-lisp-mode . my/set-elisp-capf)
         (after-init . my/global-capf))
  :config
  ;; 全モード共通の補完関数
  (defun my/global-capf ()
    "全モード共通の補完関数と Eglot の補完を設定する。"
    (unless (advice-member-p #'cape-wrap-buster 'eglot-completion-at-point)
      (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))
    (unless (advice-member-p #'cape-wrap-nonexclusive 'eglot-completion-at-point)
      (advice-add 'eglot-completion-at-point :around #'cape-wrap-nonexclusive))
    (add-hook 'completion-at-point-functions #'cape-dabbrev)
    (add-hook 'completion-at-point-functions #'cape-keyword)
    (add-hook 'completion-at-point-functions #'cape-file))

  ;; Emacs Lisp の補完
  (defun my/set-elisp-capf ()
    "Emacs Lisp バッファの補完関数を設定する。"
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'cape-elisp-symbol
                       #'cape-elisp-block))))

  ;; M-:（eval-expression）の入力欄でも Emacs Lisp のシンボルを補完する
  (add-hook 'eval-expression-minibuffer-setup-hook
            (lambda ()
              (add-hook 'completion-at-point-functions #'cape-elisp-symbol nil t)))

  ;; dabbrev が候補を探す他バッファを 1 MiB 未満に限定する。
  (setq dabbrev-friend-buffer-function (lambda (other-buffer)
                                         (< (buffer-size other-buffer) (* 1024 1024)))))

(provide '28-corfu)
;;; 28-corfu.el ends here
