;;; cc-mode
;; c-modeやc++-modeなどcc-modeベースのモード共通の設定
(add-hook
 'c-mode-common-hook
 (lambda ()
   ;; コンパイル
   (local-set-key (kbd "C-c c") 'compile)

   ;; 自動改行（auto-new-line）と
   ;; 連続する空白の一括削除（hungry-delete）を
   ;; 有効にする
   (c-toggle-auto-hungry-state 1)
   ;; (c-toggle-hungry-state 1)

   ;; 他のエディタなどがファイルを書き換えたらすぐにそれを反映する
   ;; auto-revert-modeを有効にする
   (auto-revert-mode)

   ;;; ※
   ;; インデント幅を4にする
   ;; (setq tab-width 4)
   (setq c-basic-offset 4)
   ))

;; c++-modeだけの設定
;; 2013-01-05
(add-hook
 'c++-mode-hook
 (lambda ()
   ;; 20-flycheck.elに定義
   ;; c++11オプションのために追加
   ;; todo: 150925: 研究の際，インクルードパスを通してないとエラーが出まくってうざいので，とりあえず無効に
;;;;   ;; (flycheck-select-checker 'c/c++)
 ))
