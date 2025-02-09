;;; 01-ui.el --- UI 設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; Emacs の基本 UI 設定

;;; Code:

;;;;;; [Group] UI Settings - インターフェース ;;;;;;
(column-number-mode t)                  ; カーソル位置の列番号表示
(line-number-mode t)                    ; カーソル位置の行番号表示
(setq frame-title-format (format "%%f - Emacs@%s" (system-name))) ; タイトルバーにフルパス表示
(fset 'yes-or-no-p 'y-or-n-p)           ; 確認ダイアログを簡略化 (yes/no → y/n)
(setq eval-expression-print-length nil) ; evalした結果を全部表示
(setq ring-bell-function 'ignore)       ; エラー音を鳴らさない

;; タブ幅・インデント設定
(setq-default tab-width 4
              indent-tabs-mode nil)

;; 表示設定
(global-font-lock-mode t)               ; ソースコードを色付け
(transient-mark-mode t)

;; 行番号表示
(if (version< emacs-version "29")
    (global-linum-mode t)               ; Emacs28 以前
  (global-display-line-numbers-mode t)) ; Emacs29 以降
(setq linum-format "%4d "
      linum-delay t)
;; nlinum.el の遅延更新
(advice-add 'linum-schedule :around
            (lambda (orig-fn &rest args)
              (run-with-idle-timer 0.2 nil #'linum-update-current)
              (apply orig-fn args)))

;; 行設定
(setq kill-whole-line t)                ; 行の先頭でC-kを一回押すだけで行全体を消去する
(setq require-final-newline nil)        ; 最終行に必ず一行挿入しない

;; 選択範囲を isearch
(advice-add 'isearch-mode :around
            (lambda (orig-fn &rest args)
              "Isearch with default text if there is a selection."
              (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
                  (let ((search-text (buffer-substring-no-properties (mark) (point))))
                    (isearch-update-ring search-text)
                    (deactivate-mark)
                    (apply orig-fn args)
                    (if (car args)
                        (isearch-repeat-forward)
                      (isearch-repeat-backward)))
                (apply orig-fn args))))

;;;;;; [Group] Font Settings - フォント設定 ;;;;;;
(when (display-graphic-p)
  ;; Ricty Diminished Discord フォントの設定
  ;; インストール: sudo apt install fonts-ricty-diminished
  (let* ((font-name "Ricty Diminished Discord")
         (font-size 15)  ;; フォントサイズ（ポイント単位）
         (font-spec (format "%s-%d" font-name font-size))
         (japanese-charsets '(japanese-jisx0208
                              japanese-jisx0212
                              katakana-jisx0201
                              unicode)))
    ;; 基本フォントの設定
    (set-face-attribute 'default nil :font font-spec)

    ;; 日本語フォントの設定（幅を統一）
    (dolist (charset japanese-charsets)
      (set-fontset-font t charset (font-spec :family font-name))))
  )

(provide '01-ui)
;;; 01-ui.el ends here
