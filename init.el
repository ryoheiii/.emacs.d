;;; Code:

;;; ロードパスの設定
;; load-path の追加関数
(defun add-to-load-path (&rest paths)
  "Add specified PATHS to the Emacs load-path."
  (dolist (path paths paths)
    (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
      (add-to-list 'load-path default-directory)
      (when (fboundp 'normal-top-level-add-subdirs-to-load-path)
        (normal-top-level-add-subdirs-to-load-path)))))

;; load-path に追加するディレクトリ
(add-to-load-path "loads/elisp/" "loads/site-elisp/")

;;; パッケージの設定
(require 'package)
(setq package-user-dir my-elisp-dir) ;; @early-init.el
;; パッケージアーカイブの設定
(setq package-archives
      '(("gnu"          . "https://elpa.gnu.org/packages/")     ; GNU の公式パッケージ
        ("melpa"        . "https://melpa.org/packages/")        ; 最大でもっとも更新が頻繁なパッケージ
        ("melpa-stable" . "https://stable.melpa.org/packages/") ; MELPA の安定版
        ("org"          . "https://orgmode.org/elpa/")          ; Org の公式パッケージ
        ))
;; パッケージアーカイブの優先度の設定（高い数値が高い優先度）
(setq package-archive-priorities
      '(("gnu"          . 4)
        ("melpa-stable" . 3)
        ("melpa"        . 2)
        ("org"          . 1)
        ))
;; パッケージシステムの初期化
(package-initialize)

;; パッケージリストの更新と use-package の確保
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; use-package の設定
(setq use-package-enable-imenu-support t)
(eval-when-compile
  (require 'use-package))

;;; straight.el の設定
(defvar straight-base-dir (expand-file-name "loads/elisp/" user-emacs-directory))
(setq straight-repository-branch "develop")

;; straight.el 自身のインストールと初期設定
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;;; Init-loader - 設定ファイルを整理し、分割された設定を読み込む
(use-package init-loader
  :ensure t
  :config
  (init-loader-load "~/.emacs.d/loads/inits/")
  (setq init-loader-show-log-after-init nil)
  )

(provide 'init)
;;; init.el ends here
