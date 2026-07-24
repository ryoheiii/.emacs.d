;;; 22-theme.el --- テーマとモードラインの設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; カラーテーマとモードラインの設定

;;; Code:

;;;; [Group] Themes - テーマ関連 ;;;;;;
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
  )

(global-set-key (kbd "<f6>") #'my/toggle-doom-theme)
(defun my/toggle-doom-theme ()
  "Doomテーマを doom-dracula と doom-nord-light の間で切り替える。"
  (interactive)
  (let ((next-theme (if (memq 'doom-dracula custom-enabled-themes)
                        'doom-nord-light
                      'doom-dracula)))
    ;; 重ね掛けを防ぐため、有効なテーマをすべて解除してから切り替える。
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme next-theme t)))

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
  ;; GUI は doom-modeline が担当するため端末に限定し、二重初期化を防ぐ
  :unless (display-graphic-p)
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
