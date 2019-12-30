;; /path/to/userhome/.emacs.d/
(defvar my-emacs-dir (expand-file-name user-emacs-directory))
;; /path/to/userhome/.emacs.d/hist/
(defvar my-history-dir (concat my-emacs-dir "tmp/hist/"))
;; /path/to/userhome/.emacs.d/backup/
(defvar my-backup-dir  (concat my-emacs-dir "tmp/backup/"))

(defun my-set-history (&rest args)
  (concat my-history-dir (mapconcat 'identity args "")))

;; trash
(setq trash-directory "~/.Trash")
;; backup
(add-to-list 'backup-directory-alist (cons "." my-backup-dir))
;; auto-save
(setq auto-save-file-name-transforms `((".*" ,my-backup-dir t)))
;; auto-save-list
(setq auto-save-list-file-prefix (my-set-history "auto-save-list/.saves-"))
;; bookmark
(setq bookmark-default-file (my-set-history "bookmark-" user-full-name))
;; tramp
(setq tramp-persistency-file-name (my-set-history "tramp-" user-full-name))

;; emacsが自動的に生成する設定を別ファイルに
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; helm-recentd
;; (setq helm-recentd-file (my-set-history "helm-recentd-" user-full-name))
