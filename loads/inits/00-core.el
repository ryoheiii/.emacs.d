;;; 00-core.el --- 基本設定  -*- lexical-binding: t; -*-
;;; Commentary:
;; Emacs の基本設定

;;; Code:

;;;;;; [Group] Coding System - 文字コード設定 ;;;;;;
(set-language-environment "Japanese")
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(when (eq system-type 'darwin)  ;; macOS用
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))

;;;;;; [Group] File Settings - ファイル操作関連 ;;;;;;
;; 自動保存・バックアップ設定
(setq backup-inhibited t
      delete-auto-save-files t
      make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

;; save時にmode-line を一瞬光らせる
(add-hook 'after-save-hook
          (lambda ()
            (let ((orig-fg (face-background 'mode-line)))
              (set-face-background 'mode-line "dark green")
              (run-with-idle-timer 0.1 nil
                                   (lambda (fg) (set-face-background 'mode-line fg))
                                   orig-fg))))

;; シェルスクリプト（shebangがあるファイル）の保存時に実行権を付与
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; 行末の空白を削除
(defun my-toggle-delete-trailing-whitespace ()
  "Markdown-mode のときのみ `delete-trailing-whitespace` を無効にする."
  (if (derived-mode-p 'markdown-mode)
      (remove-hook 'before-save-hook 'delete-trailing-whitespace t)
    (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)))
(add-hook 'markdown-mode-hook 'my-toggle-delete-trailing-whitespace)
(add-hook 'after-change-major-mode-hook 'my-toggle-delete-trailing-whitespace)

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

;;;;;; [Group] Line Number Settings - 行番号表示 ;;;;;;
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

;;;;;; [Group] Completion - 補完設定 ;;;;;;
(setq completion-ignore-case t
      read-file-name-completion-ignore-case t)

;;;;;; [Group] Clipboard - クリップボード設定 ;;;;;;
(setq x-select-enable-clipboard t) ;; クリップボードをシステムと共有

;;;;;; [Group] Functionality - 機能拡張 ;;;;;;
;; narrowing 禁止
(put 'narrow-to-region 'disabled nil)
;; `upcase-region` / `downcase-region` を有効化
(put 'upcase-region 'disabled nil)   ; C-x C-u -> upcase
(put 'downcase-region 'disabled nil) ; C-x C-l -> downcase
;; 現在の関数名をウィンドウ上部に表示
(which-function-mode 1)

;;;;;; [Group] Scrolling - スクロール設定 ;;;;;;
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1
      comint-scroll-show-maximum-output t)

;;;;;; [Group] Garbage Collection - GC設定 ;;;;;;
(setq gc-cons-threshold (* 128 1024 1024)
      gc-cons-percentage 0.2
      garbage-collection-messages t)
(add-hook 'focus-out-hook #'garbage-collect)

;;;;;; [Group] Misc - その他 ;;;;;;
(setq vc-follow-symlinks t
      inhibit-startup-message t
      flyspell-use-meta-tab nil
      native-comp-async-report-warnings-errors 'silent)

(provide '00-core)
;;; 00-core.el ends here
