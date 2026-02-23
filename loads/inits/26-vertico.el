;;; 26-vertico.el --- ミニバッファ補完の設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; Vertico ベースのミニバッファ補完 UI とその拡張

;;; Code:

;;;;; [Group] Vertico - ミニバッファ補完 UI ;;;;;
;;; Vertico - ミニバッファ補完 UI
(use-package vertico
  :straight t
  :init
  (vertico-mode)
  (vertico-multiform-mode)
  :custom
  (vertico-cycle t)
  (vertico-count 30)
  (vertico-resize nil)
  (enable-recursive-minibuffers t)
  :config
  ;; ディレクトリを先に表示し、ファイルはアルファベット順にソート
  (defun my-vertico-sort-directories-first (files)
    "ディレクトリを先にソートし、それ以外をアルファベット順にする."
    (let ((dirs (seq-filter #'file-directory-p files))
          (nondirs (seq-remove #'file-directory-p files)))
      (append (sort dirs #'string<) (sort nondirs #'string<))))

  ;; `file` カテゴリに限定してソート適用
  (add-to-list 'vertico-multiform-categories
               '(file (vertico-sort-override-function . my-vertico-sort-directories-first)))
  )

;;; Vertico-repeat - 直前の補完を再実施
(use-package vertico-repeat
  :straight nil
  :after vertico
  :hook (minibuffer-setup . vertico-repeat-save)
  :init
  (with-eval-after-load 'meow
    (meow-leader-define-key '("z" . vertico-repeat)))
  )

;;; Vertico-directory - ディレクトリ補完
(use-package vertico-directory
  :straight nil
  :after vertico
  :bind (:map vertico-map
              ("C-l" . vertico-directory-delete-char))
  )

;;; Vertico-truncate - vertico の補完候補を横方向に切り詰める
(use-package vertico-truncate
  :straight (:host github :repo "jdtsmith/vertico-truncate")
  :after vertico
  :config
  (vertico-truncate-mode +1)
  (setq vertico-truncate-length 50)   ;; 最大表示幅 (デフォルト: 40)
  (setq vertico-truncate-suffix "…") ;; 省略記号 (デフォルト: "…")
  )

;;; Marginalia - 詳細な補完情報の表示
(use-package marginalia
  :straight t
  :after vertico
  :init (marginalia-mode)
  )

(provide '26-vertico)
;;; 26-vertico.el ends here
