;;; 01-ui.el --- UI 設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; Emacs の基本 UI 設定

;;; Code:

;;;;;; [Group] UI Settings - インターフェース ;;;;;;
(column-number-mode t)                  ; カーソル位置の列番号表示
(line-number-mode t)                    ; カーソル位置の行番号表示
(setq frame-title-format (format "%%f - Emacs@%s" (system-name))) ; タイトルバーにフルパス表示
(setq use-short-answers t)              ; 確認ダイアログを簡略化 (yes/no → y/n)
(setq eval-expression-print-length nil) ; evalした結果を全部表示

;; タブ幅・インデント設定
(setq-default tab-width 4
              indent-tabs-mode nil)

;; 行番号表示
(global-display-line-numbers-mode t)

;; 行設定
(setq kill-whole-line t)                ; 行の先頭でC-kを一回押すだけで行全体を消去する
(setq require-final-newline t)          ; 保存時に最終改行を自動付与(POSIX 準拠)

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
;; Ricty Diminished Discord フォントの設定
;; インストール: sudo apt install fonts-ricty-diminished
;; daemon 起動では初期化時に GUI がないため、通常起動時の即時適用に加えて
;; server-after-make-frame-hook でクライアントフレーム生成時にも適用する
(defun my/setup-fonts ()
  "GUI フレームへ基本フォントと日本語フォントセット(幅統一)を適用する。"
  (when (display-graphic-p)
    (let ((font-name "Ricty Diminished Discord")
          (font-size 15))
      ;; 基本フォントの設定
      (set-face-attribute 'default nil :font (format "%s-%d" font-name font-size))
      ;; 日本語フォントの設定(幅を統一)
      (dolist (charset '(japanese-jisx0208
                         japanese-jisx0212
                         katakana-jisx0201
                         unicode))
        (set-fontset-font t charset (font-spec :family font-name))))))
(my/setup-fonts)
(add-hook 'server-after-make-frame-hook #'my/setup-fonts)

(provide '01-ui)
;;; 01-ui.el ends here
