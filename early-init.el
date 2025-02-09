;;; early-init.el --- Emacs の起動前設定  -*- lexical-binding: t; -*-
;;; Commentary:
;; Emacs の起動前に実行される設定（パッケージ管理・ディレクトリ設定）

;;; Code:

;;; [Group] Debug ;;;
(setq debug-on-error t)

;;;;; [Group] Auto Files Place ;;;;;
;;; Emacs の各種ディレクトリとファイルパスの設定
(defvar my-emacs-dir   (expand-file-name user-emacs-directory))         ;; /path/to/userhome/.emacs.d/
(defvar my-history-dir (expand-file-name "tmp/hist/" my-emacs-dir))     ;; /path/to/userhome/.emacs.d/tmp/hist/
(defvar my-backup-dir  (expand-file-name "tmp/backup/" my-emacs-dir))   ;; /path/to/userhome/.emacs.d/tmp/backup/
(defvar my-package-dir (expand-file-name "tmp/package/" my-emacs-dir))  ;; /path/to/userhome/.emacs.d/tmp/package/
(defvar my-db-dir      (expand-file-name "tmp/database/" my-emacs-dir)) ;; /path/to/userhome/.emacs.d/tmp/database/
(defvar my-elisp-dir   (expand-file-name "loads/elisp/" my-emacs-dir))  ;; /path/to/userhome/.emacs.d/loads/elisp
(defvar my-custom-dir  (expand-file-name "custom/" my-emacs-dir))       ;; /path/to/userhome/.emacs.d/custom

;;; パス設定ヘルパ関数
(defun my-set-emacs (&rest args) (expand-file-name (apply 'concat args) my-emacs-dir))
(defun my-set-history (&rest args) (expand-file-name (apply 'concat args) my-history-dir))
(defun my-set-backup (&rest args) (expand-file-name (apply 'concat args) my-backup-dir))
(defun my-set-package (&rest args) (expand-file-name (apply 'concat args) my-package-dir))
(defun my-set-db (&rest args) (expand-file-name (apply 'concat args) my-db-dir))
(defun my-set-elisp (&rest args) (expand-file-name (apply 'concat args) my-elisp-dir))
(defun my-set-custom (&rest args) (expand-file-name (apply 'concat args) my-custom-dir))

;;; システムのゴミ箱ディレクトリ
(setq trash-directory (my-set-history "trash/"))

;;; カスタムファイル設定
(setq custom-file (my-set-custom "custom.el"))
(load custom-file 'noerror)

;;; バックアップ設定
(add-to-list 'backup-directory-alist (cons "." my-backup-dir))
(setq auto-save-file-name-transforms `((".*" ,my-backup-dir t)))

;;; オートセーブリスト
(setq auto-save-list-file-prefix (my-set-history "auto-save-list/.saves-" user-full-name))

;;; ブックマーク設定
(setq bookmark-default-file (my-set-history "bookmark-" user-full-name))

;;; TRAMP 設定
(setq tramp-persistency-file-name (my-set-history "tramp-" user-full-name))

;;; Transient パッケージの一時ファイル保存先
(setq transient-history-file (my-set-history "transient/history.el"))

;;; eln-cache の保存先を変更
(when (boundp 'native-comp-eln-load-path)
  (setq native-comp-eln-load-path (list (my-set-package "eln-cache/"))))

;;; emacs 起動後に ~/.emacs.d/eln-cache が存在する場合に削除
(add-hook 'emacs-startup-hook
          (lambda ()
            (let ((default-eln-cache (expand-file-name "eln-cache/" my-emacs-dir)))
              (when (file-exists-p default-eln-cache)
                (delete-directory default-eln-cache t)
                (message "Deleted unwanted default eln-cache at %s" default-eln-cache)))))



;;;;; [Group] Straight.el ;;;;;
;;; **straight.el の設定**
(setq straight-base-dir (my-set-elisp "")) ;; .last-package-update-day など

;; package.el を無効化
(setq package-enable-at-startup nil)

;;; **straight.el のインストール**
(defvar bootstrap-version)
(let ((bootstrap-file
       (my-set-elisp "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(provide 'early-init)
;;; early-init.el ends here
