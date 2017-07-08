;; /path/to/userhome/.emacs.d/
(defvar my-emacs-dir (expand-file-name user-emacs-directory))
;; /path/to/userhome/.emacs.d/hist/
(defvar my-history-dir (concat my-emacs-dir "hist/"))
;; /path/to/userhome/.emacs.d/backup/
(defvar my-backup-dir (concat my-emacs-dir "backup/"))

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
;; ;; bookmark
;; (setq bookmark-default-file (my-set-history "bookmark-" user-full-name))
;; ;; tramp
;; (setq tramp-persistency-file-name (my-set-history "tramp-" user-full-name))
;; savehist
(setq savehist-file (my-set-history "savehist"))
;; recentf
;; (setq recentf-save-file (my-set-history "recentf" "-" user-full-name))
(setq recentf-save-file (my-set-history "recentf"))
;; save-place
(setq save-place-file (my-set-history "places"))

;; 外部拡張
;; ;; save-kill
;; (setq save-kill-file-name (my-set-history "kill-ring-saved"))
;; undohist
(setq undohist-directory (my-set-history "undohist"))
;; auto-complete
(setq ac-comphist-file (my-set-history "ac-comphist.dat"))
;; multiple-cursors
(setq mc/list-file (my-set-history "mc-lists.el"))
;; ;; request
;; (setq request-storage-directory (my-set-history "request-" user-full-name))
;; ;; helm-github-stars
;; (setq helm-github-stars-cache-file (my-set-history "helm-github-stars-cache"))
;; helm-recentd
(setq helm-recentd-file (my-set-history "helm-recentd-" user-full-name))
