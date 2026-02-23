;;; 24-org.el --- Org モードの設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; Org モードとその関連パッケージの設定

;;; Code:

;;;;; [Group] Org - Org 関係 ;;;;;
;;; Org - org 設定
(use-package org
  :straight nil
  :hook ((org-mode . visual-line-mode))  ; 自動改行の有効化
  :defer t
  :custom
  (org-return-follows-link t)            ; Returnキーでリンク先を開く
  (org-hide-leading-stars t)             ; 見出しの*を非表示
  (org-startup-indented t)               ; インデント表示をデフォルトで有効化
  (org-src-fontify-natively t)           ; ソースコードをシンタックスハイライト
  (org-src-tab-acts-natively t)          ; org-babelでタブキーを言語モードに連動
  (org-edit-src-content-indentation 2)   ; org-babelのソースコードインデント
  (org-startup-folded 'content)          ; 初期表示で折りたたむ
  (org-log-done 'time)                   ; タスク完了時に時間を記録
  (org-log-into-drawer t)                ; ログを :LOGBOOK: に格納
  (org-adapt-indentation nil)            ; インデントの自動調整をオフにする
  (org-cycle-separator-lines 2)          ; 見出しの間隔
  (org-ellipsis " ▼")                   ; 折りたたみ表示の記号変更
  ;; (org-agenda-files '("~/org/agenda/"))  ; アジェンダファイルのディレクトリ
  (org-todo-keywords
   '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  )

;;; org-indent - インデントを自動調整
(use-package org-indent
  :straight nil
  :hook (org-mode . org-indent-mode)
  )

;; ;;; org-modern - 全体的なUI向上 (*) org-indent-mode と相性が悪いため一旦無効化
;; (use-package org-modern
;;   :straight t
;;   :after (org)
;;   :hook (org-mode . org-modern-mode)
;;   :config
;;   (setopt
;;    org-auto-align-tags nil
;;    org-tags-column 0
;;    org-catch-invisible-edits 'show-and-error
;;    org-special-ctrl-a/e t
;;    org-insert-heading-respect-content t
;;    org-hide-emphasis-markers t
;;    org-pretty-entities t
;;    org-agenda-tags-column 0
;;    org-agenda-block-separator ?─
;;    org-agenda-time-grid
;;    '((daily today require-timed)
;;      (800 1000 1200 1400 1600 1800 2000)
;;      " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
;;    org-agenda-current-time-string
;;    "◀── now ─────────────────────────────────────────────────")
;;   (setopt org-ellipsis "…")
;;   (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)
;;   (global-org-modern-mode)
;;   )

;; ;;; org-download - 画像のクリップボード貼り付け
;; (use-package org-download
;;   :straight t
;;   :after (org)
;;   :bind (:map org-mode-map
;;               ("C-c i" . org-download-clipboard))
;;   :custom
;;   (org-download-method 'directory)
;;   (org-download-image-dir "~/org/images/")
;;   (org-download-heading-lvl nil)
;;   (org-download-timestamp "_%Y%m%d-%H%M%S")
;;   :config
;;   (add-hook 'dired-mode-hook 'org-download-enable)
;;   )

;; ;;; org-roam - ノート管理
;; (use-package org-roam
;;   :straight t
;;   :after (org)
;;   :custom
;;   (org-roam-directory "~/org/roam")
;;   (org-roam-database-connector 'sqlite)
;;   (org-roam-db-location (expand-file-name "org-roam.db") my-db-dir)
;;   :bind (("C-c n l" . org-roam-buffer-toggle)
;;          ("C-c n f" . org-roam-node-find)
;;          ("C-c n g" . org-roam-graph)
;;          ("C-c n i" . org-roam-node-insert)
;;          ("C-c n c" . org-roam-capture)
;;          ("C-c n j" . org-roam-dailies-capture-today))
;;   :config
;;   (org-roam-db-autosync-mode)
;;   )

;; ;;; org-agenda - スケジュール管理
;; (use-package org-agenda
;;   :straight nil
;;   :after (org)
;;   :bind ("C-c a" . org-agenda)
;;   :custom
;;   (org-agenda-files (directory-files-recursively "~/org/agenda/" "\\.org$"))
;;   (org-agenda-start-on-weekday nil)
;;   (org-agenda-span 'week)
;;   (org-agenda-use-time-grid t)
;;   (org-agenda-time-grid '((daily today)
;;                           (800 1000 1200 1400 1600 1800 2000)
;;                           "......" "----------------"))
;;   )

(provide '24-org)
;;; 24-org.el ends here
