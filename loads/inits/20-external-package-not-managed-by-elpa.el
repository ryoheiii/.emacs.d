;;; Stopwatch パッケージの設定
;; How to Install
;; git clone https://github.com/blue0513/stopwatch (.emacs.d/loads/site-lisp)
;; (use-package stopwatch
;;   :ensure nil
;;   )

;; straight.el 導入による "Could not find package uniquify" エラー対策。
;; ensure nil していてもEmacs の起動時に use-package がパッケージの存在を確認するためにエラーが発生する模様
(add-to-list 'load-path "~/.emacs.d/elisp/site-lisp/site-lisp/stopwatch")
(require 'stopwatch)

;;; Initchart パッケージの設定
;; https://qiita.com/yuttie/items/0f38870817c11b2166bd
;; How to Install
;; git clone https://github.com/yuttie/initchart.git
;; How to Use
;; initchart-visualize-init-sequence (create svg file)

;; (use-package initchart
;;   :ensure nil
;;   :config
;;   (initchart-record-execution-time-of load file)
;;   (initchart-record-execution-time-of require feature)
;;   )

;; straight.el 導入による "Could not find package uniquify" エラー対策。
;; ensure nil していてもEmacs の起動時に use-package がパッケージの存在を確認するためにエラーが発生する模様
(add-to-list 'load-path "~/.emacs.d/elisp/site-lisp/site-lisp/initchart")
(require 'initchart)
;; 初期化処理の実行時間を記録する
(initchart-record-execution-time-of load file)
(initchart-record-execution-time-of require feature)

;;;;; straight.el 管理系
;;; GitHub Copilot の設定
;; 警告が多発して不安定
;; (use-package copilot
;;   :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
;;   :ensure t
;;   :hook ((prog-mode . copilot-mode))  ;; プログラミング言語のバッファで Copilot を有効にする
;;   :config
;;   (setq copilot-node-executable "~/.nvm/versions/node/v21.5.0/bin/node")
;;   (define-key copilot-mode-map (kbd "C-<tab>") 'copilot-accept-completion)
;;   (define-key copilot-mode-map (kbd "C-TAB") 'copilot-accept-completion)
;;   )
