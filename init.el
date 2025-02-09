;;; init.el --- Emacs 初期設定  -*- lexical-binding: t; -*-
;;; Commentary:
;; Emacs の起動時に実行する設定

;;; Code:

;;; `use-package` の設定
(use-package use-package
  :straight t)

;;; ロードパスの追加関数
(defun add-to-load-path (&rest paths)
  "Add specified PATHS to the Emacs load-path."
  (dolist (path paths)
    (let ((default-directory (my-set-emacs path)))
      (add-to-list 'load-path default-directory)
      (when (fboundp 'normal-top-level-add-subdirs-to-load-path)
        (normal-top-level-add-subdirs-to-load-path)))))

(add-to-load-path "loads/elisp/" "loads/site-elisp/")

;;; init-loader - 設定ファイルを整理し、分割された設定を読み込む
(use-package init-loader
  :straight t
  :custom
  (init-loader-show-log-after-init nil)
  :config
  (init-loader-load (my-set-emacs "loads/inits/"))
  )

(provide 'init)
;;; init.el ends here
