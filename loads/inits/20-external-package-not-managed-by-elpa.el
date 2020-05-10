;;; stopwatch
;; How to Install
; git clone https://github.com/blue0513/stopwatch (.emacs.d/loads/site-lisp)
(use-package stopwatch)

;;; initchart
;; https://qiita.com/yuttie/items/0f38870817c11b2166bd
;; How to Install
; git clone https://github.com/yuttie/initchart.git
;; How to Use
; initchart-visualize-init-sequence (create svg file)
(use-package initchart
  :config
  (initchart-record-execution-time-of load file)
  (initchart-record-execution-time-of require feature)
  )
