;;; init.el --- Emacs 初期設定  -*- lexical-binding: t; -*-
;;; Commentary:
;; Emacs の起動時に実行する設定

;;; Code:

;;;;; [Group] Use-package 設定 ;;;;;
(use-package use-package
  :straight t)

;;;;; [Group] ロードパスの追加 ;;;;;
(defun add-to-load-path (&rest paths)
  "Add specified PATHS to the Emacs load-path."
  (dolist (path paths)
    (let ((default-directory (my-set-emacs path)))
      (add-to-list 'load-path default-directory)
      (when (fboundp 'normal-top-level-add-subdirs-to-load-path)
        (normal-top-level-add-subdirs-to-load-path)))))

(add-to-load-path "loads/elisp/" "loads/site-elisp/")

;;;;; [Group] Init-loader 設定 ;;;;;
(use-package init-loader
  :straight t
  :custom
  (init-loader-show-log-after-init nil)
  :config
  (init-loader-load (my-set-emacs "loads/inits/"))
  )

(provide 'init)
;;; init.el ends here
