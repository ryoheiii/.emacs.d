;;; 22-theme.el --- テーマとモードラインの設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; カラーテーマとモードラインの設定

;;; Code:

;;; カラーテーマ
;;;; doom-themes - テーマ設定
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
  ;; 起動時のテーマ
  (load-theme 'doom-dracula t)
  ;; Gnus の標準 face と doom の逆向き継承が、遅延ロード中に循環するのを防ぐ。
  ;; user テーマで設定し、F6 によるテーマ切替後も同じ空グループの色を継承する。
  (custom-theme-set-faces 'user
                         '(gnus-group-news-low-empty ((t (:inherit gnus-group-mail-1-empty)))))

  ;; NeoTree と Org にもテーマの配色を適用する。
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(keymap-global-set "<f6>" #'my/toggle-doom-theme)
(defun my/toggle-doom-theme ()
  "Doomテーマを doom-dracula と doom-nord-light の間で切り替える。"
  (interactive)
  (let ((next-theme (if (memq 'doom-dracula custom-enabled-themes)
                        'doom-nord-light
                      'doom-dracula)))
    ;; 重ね掛けを防ぐため、有効なテーマをすべて解除してから切り替える。
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme next-theme t)))

;;; モードライン
;;;; doom-modeline - モードラインのテーマ設定
(use-package doom-modeline
  :straight t
  :custom
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  ;; 総行数を「行/総行数」で表示する。旧 total-lines による mode-line-front-space への
  ;; 追記は doom-modeline の main レイアウトが参照しないため表示されなかった
  (doom-modeline-total-line-number t)
  ;; tty では Nerd Font グリフが幅計算を狂わせレイアウトが崩れるため GUI のみ有効化
  ;; (daemon 起動時は nil に倒れる。GUI クライアントで必要なら明示的に t を設定する)
  (doom-modeline-icon (display-graphic-p))
  :hook
  (after-init . doom-modeline-mode))

;;;; mode-line-invisible-mode - 特定のモードでモードラインを非表示
;; Emacs 31.1 組み込み (simple.el)。旧 hide-mode-line パッケージを置き換える
(use-package simple
  :straight nil
  :hook ((neotree-mode imenu-list-minor-mode minimap-mode) . mode-line-invisible-mode))

;;;; mode-line-bell - 警告音の代わりにモードラインで知らせる
(use-package mode-line-bell
  :straight t
  :hook (after-init . mode-line-bell-mode))

(provide '22-theme)
;;; 22-theme.el ends here
