;;; stopwatch
;; How to Install
; git clone https://github.com/blue0513/stopwatch (.emacs.d/loads/site-lisp)

; (use-package stopwatch
;   :ensure nil
;   )

;; straight.el 導入による "Could not find package uniquify" エラー対策。
;; ensure nil していてもEmacs の起動時に use-package がパッケージの存在を確認するためにエラーが発生する模様
(add-to-list 'load-path "~/.emacs.d/elisp/site-lisp/site-lisp/stopwatch")
(require 'stopwatch)

;;; initchart
;; https://qiita.com/yuttie/items/0f38870817c11b2166bd
;; How to Install
; git clone https://github.com/yuttie/initchart.git
;; How to Use
; initchart-visualize-init-sequence (create svg file)

; (use-package initchart
;   :ensure nil
;   :config
;   (initchart-record-execution-time-of load file)
;   (initchart-record-execution-time-of require feature)
;   )

;; straight.el 導入による "Could not find package uniquify" エラー対策。
;; ensure nil していてもEmacs の起動時に use-package がパッケージの存在を確認するためにエラーが発生する模様
(add-to-list 'load-path "~/.emacs.d/elisp/site-lisp/site-lisp/initchart")
(require 'initchart)
(initchart-record-execution-time-of load file)
(initchart-record-execution-time-of require feature)
