;;; 29-scoring.el --- スコアリング・履歴の設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; Prescient, Flx ベースの補完スコアリングと履歴管理

;;; Code:

(defvar-local my/input-query nil
  "補完候補のスコアリングに使う入力文字列。")

;;;;; [Group] スコアリング・履歴 ;;;;;
;;; Flx - 柔軟なスコアリング
(use-package flx
  :straight t
  :after prescient
  :config
  ;; 入力文字を抽出
  (defun my/store-input-query (string &rest _args)
    "現在の補完入力から空白を除き、スコア計算用に保存する。"
    (setq my/input-query (replace-regexp-in-string " " "" string)))
  (advice-add 'completion-all-completions :before #'my/store-input-query)

  ;; 遅延ロード先の変数を宣言する。既に設定された候補数は上書きしない。
  (defvar vertico--total nil)
  (defvar corfu--total nil)

  ;; スコアのキャッシュ
  (defvar my/flx-cache (make-hash-table :test 'equal :size 1000))

  (defun my/get-flx-score (str query)
    "STR と QUERY のスコアを返し、一致なしの nil もキャッシュする。"
    (let* ((key (cons str query))
           (cached (gethash key my/flx-cache 'missing)))
      (if (eq cached 'missing)
          ;; 一時エラーは記録せず、次回の補完で従来どおり再試行する。
          (condition-case nil
              (puthash key (car (flx-score str query flx-file-cache)) my/flx-cache)
            (error nil))
        cached)))

  (defun my/flx-tiebreaker (c1 c2)
    (let ((total (or (bound-and-true-p vertico--total)
                     (bound-and-true-p corfu--total)
                     0))
          (query-length (length my/input-query)))
      (if (and (< total 3000)
               (> query-length 2)
               (< (length c1) 100)
               (< (length c2) 100))
          (let ((score1 (my/get-flx-score c1 my/input-query))
                (score2 (my/get-flx-score c2 my/input-query)))
            (cond ((and (integerp score1) (integerp score2))
                   (cond ((> score1 score2) -1)
                         ((< score1 score2) 1)
                         (t (- (length c1) (length c2)))))
                  (t 0)))
        (- (length c1) (length c2)))))

  (setq prescient-tiebreaker #'my/flx-tiebreaker)

  (defun my/clear-flx-cache ()
    (clrhash my/flx-cache))

  ;; キャッシュを1時間ごとにクリア
  (defvar my/flx-cache-timer nil)
  (setq my/flx-cache-timer
        (run-with-timer 3600 3600 #'my/clear-flx-cache)))

;;; Prescient - 補完履歴とスコアリング
(use-package prescient
  :straight t
  :custom
  (prescient-aggressive-file-save t)
  (prescient-save-file (my-set-history "prescient-save.el"))
  :config
  ;; prescient-persist-mode は autoload なし → パッケージは即ロード、ファイル I/O のみ遅延
  (run-with-idle-timer 1 nil #'prescient-persist-mode 1))

;;; Vertico-prescient - `vertico` 用のスコアリング
(use-package vertico-prescient
  :straight t
  :after vertico
  :config
  (setq vertico-prescient-enable-filtering nil)
  (vertico-prescient-mode +1))

;;; Corfu-prescient - `corfu` 用のスコアリング
(use-package corfu-prescient
  :straight t
  :after corfu
  :config
  (with-eval-after-load 'orderless
    (setq corfu-prescient-enable-filtering nil))
  (corfu-prescient-mode +1))

(provide '29-scoring)
;;; 29-scoring.el ends here
