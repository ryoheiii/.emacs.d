;;; my-snippets.el --- 個人スニペットの配置と初期化 -*- lexical-binding: t; -*-
;;; Commentary:
;; 31-editing.el から読み込む補助処理。
;; 個人スニペットの2種類の配置を解決し、利用できるときだけ有効化する。
;; このライブラリを読むだけでは yasnippet 本体をロードしない。

;;; Code:

(require 'seq)

(declare-function my-set-custom "early-init")
(declare-function yas-global-mode "yasnippet")
(defvar yas-snippet-dirs)

(defun my/yas-personal-snippet-dirs (&optional root)
  "個人スニペットの top-level ディレクトリ一覧を返す。
ROOT の既定は `custom/snippets/'。

2 つのレイアウトを支える (どちらの環境も実在する)。
  A: ROOT/<mode>/ に実体のモードディレクトリを直接置く    -> ROOT を返す
  B: ROOT/snippets を外部ディレクトリへの symlink にする  -> ROOT/snippets を返す

yasnippet は top-level dir 直下のディレクトリ名をメジャーモード名として intern する
ため、B の構成で ROOT を登録すると架空モード `snippets' が作られ、同じツリーを二重に
走査する。A と B が同時に成立する構成は yasnippet の契約では表現できないため非対応と
し、B を優先して警告する (A 側のモードディレクトリはロードしない)。"
  (let* ((root (or root (my-set-custom "snippets")))
         (linked (expand-file-name "snippets" root))
         (linked-p (file-directory-p linked))
         (root-mode-dirs
          (and (file-directory-p root)
               (seq-remove (lambda (dir)
                             (equal (file-name-nondirectory dir) "snippets"))
                           (seq-filter #'file-directory-p
                                       (directory-files root t "\\`[^.]" t))))))
    ;; `file-directory-p' は壊れた symlink に対して nil を返すため、検出しないと
    ;; リンク先の消失が無警告で握り潰される。
    (dolist (path (list root linked))
      (when (and (file-symlink-p path) (not (file-directory-p path)))
        (display-warning 'my-config
                         (format "スニペットディレクトリの symlink が壊れている: %s" path)
                         :warning)))
    (cond
     ((and linked-p root-mode-dirs)
      (display-warning
       'my-config
       (format "個人スニペットの 2 レイアウトが混在している。%s を優先し、%s 直下のモードディレクトリはロードしない"
               linked root)
       :warning)
      (list linked))
     (linked-p (list linked))
     (root-mode-dirs (list root))
     (t nil))))

(defun my/yas-resolve-snippet-dirs (dirs)
  "DIRS のうち解決できる要素だけを返し、除外した要素を `display-warning' で報告する。
`yas-snippet-dirs' (関数) は解決できない要素に対して error を送出するため、
そのままでは `yas-global-mode' ごと失敗する。パス参照ミスを黙って握り潰さない
よう、除外は必ず可視化する。"
  (let (kept dropped)
    (dolist (entry dirs)
      (let ((path (cond ((stringp entry) entry)
                        ((and (symbolp entry)
                              (boundp entry)
                              (stringp (symbol-value entry)))
                         (symbol-value entry)))))
        (if (and path (file-directory-p path))
            (push entry kept)
          (push entry dropped))))
    (dolist (entry (nreverse dropped))
      (display-warning 'my-config
                       (format "yas-snippet-dirs から解決できない要素を除外した: %S" entry)
                       :warning))
    (nreverse kept)))

(defun my/yas-setup ()
  "`yas-snippet-dirs' を解決済みの値へ確定し、可能なら `yas-global-mode' を有効にする。
有効化した場合は non-nil を返す。

全エントリが解決できず `yas-snippet-dirs' が nil になると、`yas--load-snippet-dirs'
は `(call-interactively \\='yas-load-directory)' を呼び、起動が対話プロンプトで
停止する。その場合はモードを有効化せず、警告して縮退する。"
  (setq yas-snippet-dirs (my/yas-resolve-snippet-dirs yas-snippet-dirs))
  (cond
   (yas-snippet-dirs (yas-global-mode 1) t)
   (t (display-warning
       'my-config
       "利用できるスニペットディレクトリが無いため yas-global-mode を有効化しない"
       :warning)
      nil)))

(provide 'my-snippets)
;;; my-snippets.el ends here
