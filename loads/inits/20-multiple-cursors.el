;;; multiple-cursors
(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-M-c") 'mc/edit-lines)
  (global-set-key (kbd "C-*")   'mc/mark-all-like-this)

  (global-unset-key "\C-q")
  (use-package smartrep :ensure t)
  (declare-function smartrep-define-key "smartrep")
  (smartrep-define-key global-map "C-q"
    '(("p"      . 'mc/mark-previous-like-this)
      ("n"      . 'mc/mark-next-like-this)
      ("*"      . 'mc/mark-all-like-this)
      ("d"      . 'mc/mark-all-like-this-dwim)
      ("m"      . 'mc/mark-more-like-this-extended)
      ("u"      . 'mc/unmark-next-like-this)
      ("U"      . 'mc/unmark-previous-like-this)
      ("s"      . 'mc/skip-to-next-like-this)
      ("S"      . 'mc/skip-to-previous-like-this)
      ("i"      . 'mc/insert-numbers)
      ("o"      . 'mc/sort-regions)
      ("O"      . 'mc/reverse-regions)))
  ;; smartrepによるコマンド実行中はキー入力をエコーしない
  ;; http://shakenbu.org/yanagi/d/?date=20140105
  (advice-add 'smartrep-map-internal
              :around (lambda (orig-fun &rest args)
                        (let ((echo-keystrokes 0))
                          (apply orig-fun args))))
  )

(provide '20-multiple-cursors)
;;; 20-multiple-cursors.el ends here
