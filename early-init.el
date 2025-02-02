;;;;;; [Group] Auto Files Place ;;;;;
;;; Emacs の各種ディレクトリとファイルパスの設定
(defvar my-emacs-dir   (expand-file-name user-emacs-directory))         ;; /path/to/userhome/.emacs.d/
(defvar my-history-dir (expand-file-name "tmp/hist/" my-emacs-dir))     ;; /path/to/userhome/.emacs.d/tmp/hist/
(defvar my-backup-dir  (expand-file-name "tmp/backup/" my-emacs-dir))   ;; /path/to/userhome/.emacs.d/tmp/backup/
(defvar my-package-dir (expand-file-name "tmp/package/" my-emacs-dir))  ;; /path/to/userhome/.emacs.d/tmp/package/
(defvar my-db-dir      (expand-file-name "tmp/database/" my-emacs-dir)) ;; /path/to/userhome/.emacs.d/tmp/database/
(defvar my-elisp-dir   (expand-file-name "loads/elisp/" my-emacs-dir))  ;; /path/to/userhome/.emacs.d/loads/elisp

;;; パス設定ヘルパ関数
(defun my-set-history (&rest args)
  "絶対パスを生成するためのヘルパー関数。"
  (expand-file-name (apply 'concat args) my-history-dir))

;;; システムのゴミ箱ディレクトリ
(setq trash-directory (expand-file-name "~/.Trash"))

;;; カスタムファイル設定
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
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

;;; Transient パッケージが生成する一時ファイルを保存する場所
(setq transient-history-file (expand-file-name "transient/history.el" my-history-dir))

;;; eln-cache の保存先を変更
(when (boundp 'native-comp-eln-load-path)
  (setq native-comp-eln-load-path (list (expand-file-name "eln-cache/" my-package-dir))))

;;; emacs 起動後に ~/.emacs.d/eln-cache が存在する場合に削除
(add-hook 'emacs-startup-hook
          (lambda ()
            (let ((default-eln-cache (expand-file-name "eln-cache/" my-emacs-dir)))
              (when (file-exists-p default-eln-cache)
                (delete-directory default-eln-cache t)
                (message "Deleted unwanted default eln-cache at %s" default-eln-cache)))))

;;; elpa の保存先を変更
(setq package-user-dir (expand-file-name "elpa/" my-package-dir))

;;; gnupg ディレクトリのパスを変更
(setq package-gnupghome-dir (expand-file-name "elpa/gnupg/" my-package-dir))

;;; straight 設定
(setq straight-base-dir my-elisp-dir) ;; .last-package-update-day など
