;;; init.el --- Emacs 初期設定  -*- lexical-binding: t; -*-
;;; Commentary:
;; Emacs の起動時に実行する設定

;;; Code:

;;;;; [Group] ロードパスの追加 ;;;;;
(defun add-to-load-path (&rest paths)
  "指定した PATHS を `load-path` に追加。"
  (dolist (path paths)
    (let ((default-directory (my-set-loads path)))
      (add-to-list 'load-path default-directory)
      (normal-top-level-add-subdirs-to-load-path))))
(add-to-load-path "site-elisp/") ; my-loads-dir 内のディレクトリを指定

;;;;; [Group] Use-package 設定 ;;;;;
(eval-when-compile
  (require 'straight))

;; 多数のパッケージが共通で依存する compat / seq は組み込み版へ固定する。
;; straight はレシピリポジトリにあればクローンし、組み込み版を load-path 上で
;; 覆い隠す (詳細は docs/packages.md)。最初の依存元より前に置く必要がある。
;;   compat  31.1 の組み込み stub (compat-call / compat-function を提供)
;;           >= 30 (vertico / corfu / consult / magit / copilot など)
;;   seq     2.24 (preload 済み) >= 2.24 (magit)、2.23 (ELPA 版 compat)
(dolist (pkg '(compat seq))
  (straight-override-recipe (list pkg :type 'built-in)))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t) ; 'use-package' で ':straight t' を省略可能に

;;;;; [Group] Init-loader 設定 ;;;;;
(use-package init-loader
  :straight t
  :custom
  ;; ロードエラーがあったときだけ *init log* を表示する (nil だと黙って握り潰される)
  (init-loader-show-log-after-init 'error-only)
  :config
  (init-loader-load (my-set-loads "inits/"))
  )

(provide 'init)
;;; init.el ends here
