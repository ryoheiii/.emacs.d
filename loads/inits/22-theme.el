;;; 22-theme.el --- テーマとモードラインの設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; カラーテーマとモードラインの設定

;;; Code:

;;;; [Group] Themes - テーマ関連 ;;;;;;
;;; color-theme-modern - モダンなカラーテーマの適用
;; (use-package color-theme-modern
;;   :straight t
;;   :config
;;   ;; 選択可能なテーマの幅を広げる
;;   ;; 参考: https://github.com/emacs-jp/replace-colorthemes/blob/master/screenshots.md
;;   (load-theme 'hober t t)
;;   (enable-theme 'hober)
;;   )

;; (use-package modus-themes
;;   :straight t
;;   :custom
;;   (modus-themes-italic-constructs nil) ;; イタリックを有効化
;;   (modus-themes-bold-constructs nil)   ;; 強調を有効化
;;   (modus-themes-mixed-fonts t)       ;; 等幅フォントとプロポーショナルフォントを混在
;;   (modus-themes-subtle-line-numbers t) ;; `display-line-numbers` の背景を控えめに
;;   (modus-themes-region '(bg-only no-extend))   ;; 選択範囲の背景を変更
;;   (modus-themes-completions
;;    '((matches . (extrabold background))
;;      (selection . (semibold accented))))
;;   :config
;;   ;; デフォルトのテーマを設定
;;   (load-theme 'modus-vivendi t) ;; ダークテーマを適用
;;   ;; (load-theme 'modus-operandi t) ;; ライトテーマを適用したい場合はこちら
;;   )
;; ;; `modus-themes-toggle` をキーバインドに割り当て
;; (global-set-key (kbd "<f6>") #'modus-themes-toggle)

;;; doom-themes - テーマ設定
(use-package doom-themes
  :straight t
  :custom
  (doom-themes-enable-bold t)   ;; 強調された文字を有効化
  (doom-themes-enable-italic t) ;; イタリックを有効化
  :custom-face
  (doom-modeline-bar ((t (:background "#6272a4"))))
  (font-lock-comment-face ((t (:foreground "#b0b0b0" :slant italic)))) ;; 薄めのグレー
  (font-lock-doc-face ((t (:foreground "#b0b0b0"))))                   ;; 薄めのグレー
  (region ((t (:background "#44475a"))))
  :config
  ;;; ロードテーマ
  ;; (load-theme 'doom-one t)
  (load-theme 'doom-dracula t)
  ;; (load-theme 'doom-gruvbox t)

  ;; 各種設定
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config)
  ;; Treemacs に Doom のスタイルを適用（GUI環境のみ）
  (when (and (display-graphic-p) (fboundp 'treemacs))
    (doom-themes-treemacs-config))
  )

(global-set-key (kbd "<f6>") #'my/toggle-doom-theme)
(defun my/toggle-doom-theme ()
  "Doomテーマを light/dark で切り替える."
  (interactive)
  (if (eq (car custom-enabled-themes) 'doom-one)
      (progn (disable-theme 'doom-one) (load-theme 'doom-nord-light t))
    (disable-theme 'doom-nord-light)
    (load-theme 'doom-one t)))

;;; doom-modeline - モードラインのテーマ設定
(use-package doom-modeline
  :straight t
  :if (display-graphic-p)
  :custom
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-icon t)
  ;; (doom-modeline-major-mode-icon nil)
  ;; (doom-modeline-minor-modes nil)
  :hook
  (after-init . doom-modeline-mode)
  :config
  ;; (line-number-mode 0)
  ;; (column-number-mode 0)
  )

;;; smart-mode-line - モードラインの外観と情報表示を最適化
(use-package smart-mode-line
  :straight t
  :defer t
  :init
  ;; sml/setup は autoload 済み → タイマーでパッケージロード + :config 実行
  (run-with-idle-timer 0.5 nil #'sml/setup)
  :config
  (setq sml/no-confirm-load-theme t
        sml/theme 'dark
        sml/shorten-directory nil) ; ディレクトリパスはフル表示
  )

;;; hide-mode-line - 特定のモードでモードラインを非表示
(use-package hide-mode-line
  :straight t
  :hook ((neotree-mode imenu-list-minor-mode minimap-mode) . hide-mode-line-mode)
  )

;;; mode-line-bell - モードラインを利用した通知システム
(use-package mode-line-bell
  :straight t
  :hook (after-init . mode-line-bell-mode)
  )

(provide '22-theme)
;;; 22-theme.el ends here
