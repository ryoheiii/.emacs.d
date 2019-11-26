;;; flymakeより強力
; http://qiita.com/senda-akiha/items/cddb02cfdbc0c8c7bc2b

; (add-hook 'after-init-hook #'global-flycheck-mode)

;; c++11オプションのために追加
;; 19-cc-mode.elのフックでコール
(require 'flycheck)
(flycheck-define-checker c/c++
  "A C/C++ checker using g++."
  :command ("g++" "-std=c++11" "-Wall" "-Wextra" source)
  :error-patterns  ((error line-start
                           (file-name) ":" line ":" column ":" " error: " (message)
                           line-end)
                    (warning line-start
                           (file-name) ":" line ":" column ":" " warning: " (message)
                           line-end))
  :modes (c++-mode))

(flycheck-add-next-checker 'javascript-jshint
                           'javascript-gjslint)

;; キーバインド
; 設定しても動かないので00-keybind.elに記述

;(require 'flycheck)
;(setq flycheck-check-syntax-automatically '(mode-enabled save))
;; (add-hook 'ruby-mode-hook 'flycheck-mode)
;(add-hook 'enh-ruby-mode-hook 'flycheck-mode)
