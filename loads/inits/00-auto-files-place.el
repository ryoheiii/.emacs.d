;;; Emacs の各種ディレクトリとファイルパスの設定
(defvar my-emacs-dir   (expand-file-name user-emacs-directory))       ;; /path/to/userhome/.emacs.d/
(defvar my-history-dir (expand-file-name "tmp/hist/" my-emacs-dir))   ;; /path/to/userhome/.emacs.d/tmp/hist/
(defvar my-backup-dir  (expand-file-name "tmp/backup/" my-emacs-dir)) ;; /path/to/userhome/.emacs.d/tmp/backup/

;;; パス設定ヘルパ関数
(defun my-set-history (&rest args)
  "絶対パスを生成するためのヘルパー関数。"
  (expand-file-name (apply 'concat args) my-history-dir))

;;; システムのゴミ箱ディレクトリ
(setq trash-directory (expand-file-name "~/.Trash"))

;;; バックアップ設定
(add-to-list 'backup-directory-alist (cons "." my-backup-dir))
(setq auto-save-file-name-transforms `((".*" ,my-backup-dir t)))

;;; オートセーブリスト
(setq auto-save-list-file-prefix (my-set-history "auto-save-list/.saves-"))

;;; ブックマーク設定
(setq bookmark-default-file (my-set-history "bookmark-" user-full-name))

;;; TRAMP 設定
(setq tramp-persistency-file-name (my-set-history "tramp-" user-full-name))

;;; カスタムファイル設定
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; helm-recentd
;; (setq helm-recentd-file (my-set-history "helm-recentd-" user-full-name))
