;;; js2-mode
;;; JavaScript編集用のモード
;;; 2012-04-25

(autoload 'js2-mode "js2" nil t)

(add-hook 'js2-mode-hook
          (lambda ()
            ;; ASTを使わないで移動するS式を求める。
            ;; 2012-08-14
            (setq forward-sexp-function nil)))
