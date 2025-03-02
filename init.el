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
      (when (fboundp 'normal-top-level-add-subdirs-to-load-path)
        (normal-top-level-add-subdirs-to-load-path)))))
(add-to-load-path "site-elisp/") ; my-loads-dir 内のディレクトリを指定

;;;;; [Group] Use-package 設定 ;;;;;
(eval-when-compile
  (require 'straight))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t) ; 'use-package' で ':straight t' を省略可能に

;;;;; [Group] Init-loader 設定 ;;;;;
(use-package init-loader
  :straight t
  :custom
  (init-loader-show-log-after-init nil)
  :config
  (init-loader-load (my-set-loads "inits/"))
  )

(provide 'init)
;;; init.el ends here
