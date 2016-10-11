;; undo-tree
; C-/でundo
; C-x uで樹形図表示
(when (require 'undo-tree nil t)
   (global-undo-tree-mode))
