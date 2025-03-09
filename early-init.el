;;; early-init.el --- Emacs の起動前設定  -*- lexical-binding: t; -*-
;;; Commentary:
;; Emacs の起動前に実行される設定（パッケージ管理・ディレクトリ設定）

;;; Code:

;;;;; [Group] Debug ;;;;;
;; (setq debug-on-error t)



;;;;; [Group] Define - 定数
;;; OS判定用定数
(defconst IS-MAC (eq system-type 'darwin))
(defconst IS-LINUX (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))



;;;;; [Group] Auto Files Place - 自動生成ファイル関連制御 ;;;;;
;;; Emacs の各種ディレクトリとファイルパスの設定
(defvar my-emacs-dir    (expand-file-name user-emacs-directory))      ;; /path/to/userhome/.emacs.d/

(defvar my-loads-dir    (expand-file-name "loads/"    my-emacs-dir))  ;; /path/to/userhome/.emacs.d/loads
(defvar my-elisp-dir    (expand-file-name "elisp/"    my-loads-dir))  ;; /path/to/userhome/.emacs.d/loads/elisp
(defvar my-straight-dir (expand-file-name "straight/" my-loads-dir))  ;; /path/to/userhome/.emacs.d/loads/straight

(defvar my-custom-dir   (expand-file-name "custom/"   my-emacs-dir))  ;; /path/to/userhome/.emacs.d/custom

(defvar my-var-dir      (expand-file-name "var/"      my-emacs-dir))  ;; /path/to/userhome/.emacs.d/var
(defvar my-history-dir  (expand-file-name "hist/"     my-var-dir))    ;; /path/to/userhome/.emacs.d/var/hist/
(defvar my-backup-dir   (expand-file-name "backup/"   my-var-dir))    ;; /path/to/userhome/.emacs.d/var/backup/
(defvar my-package-dir  (expand-file-name "package/"  my-var-dir))    ;; /path/to/userhome/.emacs.d/var/package/
(defvar my-db-dir       (expand-file-name "database/" my-var-dir))    ;; /path/to/userhome/.emacs.d/var/database/

;;; パス設定ヘルパ関数
(defun my-set-emacs (&rest args) (expand-file-name (apply 'concat args) my-emacs-dir))

(defun my-set-loads (&rest args) (expand-file-name (apply 'concat args) my-loads-dir))
(defun my-set-elisp (&rest args) (expand-file-name (apply 'concat args) my-elisp-dir))
(defun my-set-straight (&rest args) (expand-file-name (apply 'concat args) my-straight-dir))

(defun my-set-custom (&rest args) (expand-file-name (apply 'concat args) my-custom-dir))

(defun my-set-history (&rest args) (expand-file-name (apply 'concat args) my-history-dir))
(defun my-set-backup (&rest args) (expand-file-name (apply 'concat args) my-backup-dir))
(defun my-set-package (&rest args) (expand-file-name (apply 'concat args) my-package-dir))
(defun my-set-db (&rest args) (expand-file-name (apply 'concat args) my-db-dir))

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

;;; emacs 起動後に ~/.emacs.d/eln-cache が生成される課題への対策
;; ディレクトリが存在する場合に削除
(add-hook 'emacs-startup-hook
          (lambda ()
            (let ((default-eln-cache (my-set-emacs "eln-cache/")))
              (when (file-exists-p default-eln-cache)
                (delete-directory default-eln-cache t)
                (message "Deleted unwanted default eln-cache at %s" default-eln-cache)))))

;;; emacs 起動後に ~/.emacs.d/snippets が生成される課題への対策
;; ;; 初期状態で yas-snippet-dirs を nil に設定し、yasnippet ロード時に明示的に設定
;; (setq yas-snippet-dirs '())
;; ディレクトリが存在する場合に削除 (Package install 時に一度生成されてしまう課題への対応)
(add-hook 'emacs-startup-hook
          (lambda ()
            (let ((default-snippets (my-set-emacs "snippets/")))
              (when (file-exists-p default-snippets)
                (delete-directory default-snippets t)
                (message "Deleted unwanted default snippets at %s" default-snippets)))))

;;; emacs 起動後に ~/.emacs.d/auto-save-list が生成される課題への対策（正常系では発生しない）
;; ディレクトリが存在する場合に削除
(add-hook 'emacs-startup-hook
          (lambda ()
            (let ((default-auto-save-list (my-set-emacs "auto-save-list/")))
              (when (file-exists-p default-auto-save-list)
                (delete-directory default-auto-save-list t)
                (message "Deleted unwanted default auto-save-list at %s" default-auto-save-list)))))



;;;;; [Group] UI Performance - 起動時の UI 最適化 ;;;;;
(setq use-file-dialog nil)           ;ファイル選択ウィンドウを使用しない
(setq inhibit-startup-buffer-menu t) ; バッファメニューの使用を抑制
(when window-system
  (scroll-bar-mode -1))              ; スクロール非表示
(menu-bar-mode -1)                   ; メニューバーを消す
(when window-system
  (tool-bar-mode -1))                ; ツールバーを消す
(blink-cursor-mode 0)                ; カーソルの点滅を止める



;;;;; [Group] Package Management - パッケージ管理 ;;;;;
;;; 'straight.el' の設定
;; ベースディレクトリの指定
(setq straight-base-dir (my-set-loads ""))
;; 'package.el' を無効化
(setq package-enable-at-startup nil)
;; 'straight.el' のインストール
(defvar bootstrap-version)
(let ((bootstrap-file
       (my-set-straight "repos/straight.el/bootstrap.el"))
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
