;;; early-init.el --- Emacs の起動前設定  -*- lexical-binding: t; -*-
;;; Commentary:
;; Emacs の起動前に実行される設定（パッケージ管理・ディレクトリ設定）

;;; Code:

;;;;; [Group] Debug ;;;;;
;; (setq debug-on-error t)

;;;;; [Group] GC - 起動時の GC 抑制 ;;;;;
;; 起動中は GC を完全に抑制し、emacs-startup-hook で適正値に復元（00-core.el）
(setq gc-cons-threshold most-positive-fixnum)

;;;;; [Group] Startup - file-name-handler 最適化 ;;;;;
;; 起動中はファイル名ハンドラを無効化し、起動完了後に復元する
;; (バッチ実行では復元フックが発火しないため対話起動に限定する)
(unless noninteractive
  (defvar my/saved-file-name-handler-alist file-name-handler-alist)
  (setq file-name-handler-alist nil)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq file-name-handler-alist
                    (delete-dups (append file-name-handler-alist
                                         my/saved-file-name-handler-alist))))
            99))

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
;; auto-save-file-name-transforms は保存先ディレクトリを作成しないため、
;; fresh 環境や --clean 後でも auto-save が失敗しないよう事前に作成する
(make-directory my-backup-dir t)
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

;;; eln-cache の保存先を変更（Emacs 29+ 公式 API）
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache (my-set-package "eln-cache/")))

;;; yasnippet のデフォルト snippets/ ディレクトリ生成を防止
;; straight.el のビルド過程で yasnippet がロードされる前に設定する必要がある
;; （use-package の :init では間に合わない — :straight が先に処理されるため）
(setq yas-snippet-dirs (list (my-set-custom "snippets")))


;;;;; [Group] UI Performance - 起動時の UI 最適化 ;;;;;
(setq frame-inhibit-implied-resize t) ; フレームの暗黙リサイズを抑制
(setq inhibit-compacting-font-caches t) ; フォントキャッシュの圧縮を抑制
(setq use-file-dialog nil)           ; ファイル選択ウィンドウを使用しない
(setq inhibit-startup-buffer-menu t) ; バッファメニューの使用を抑制
;; フレーム生成前にパラメータで UI 要素を無効化する
;; (モード関数の呼び出しより速く、daemon 起動でも GUI クライアントに正しく効く)
(push '(menu-bar-lines . 0) default-frame-alist)          ; メニューバーを消す
(push '(tool-bar-lines . 0) default-frame-alist)          ; ツールバーを消す
(push '(vertical-scroll-bars . nil) default-frame-alist)  ; スクロールバー非表示
;; モード変数も同期し、M-x での再有効化を 1 回のトグルで済むようにする
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)
(blink-cursor-mode 0)                ; カーソルの点滅を止める



;;;;; [Group] Package Management - パッケージ管理 ;;;;;
;;; 'straight.el' の設定
;; ベースディレクトリの指定
;; テストハーネスは my-straight-base-dir-override で実体の loads/ を指定する
;; (一時テストルート経由のビルドで実キャッシュに dangling symlink を作らないため)
(setq straight-base-dir (or (bound-and-true-p my-straight-base-dir-override)
                            (my-set-loads "")))
;; 'package.el' を無効化
(setq package-enable-at-startup nil)
;; パッケージ変更検出を保存時フックへ切り替える
;; 既定値 (find-at-startup find-when-checking only-once) は起動経路で find(1) を発行し、
;; loads/straight/repos 配下を同期走査するため、リポジトリ規模に比例して入力がブロックされる
;; (実測: 26,170 ファイル / 1.9 GB を 0.43〜1.94 秒。コールドキャッシュではさらに伸びる)。
;; check-on-save は before-save-hook で変更を記録する方式で、straight.el が Windows で使う既定でもある。
;; find-when-checking は残すため、M-x straight-check-all による手動の全走査は従来どおり使える。
;; bootstrap.el が本変数を見て straight-live-modifications-mode を切り替えるので、
;; 必ず bootstrap のロードより前に設定する。
(setq straight-check-for-modifications '(check-on-save find-when-checking only-once))
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
