(let [map vim.keymap.set]
  ;; vertically center the current line after certain jumps (thanks prime)
  (map :n "<C-u>" "<C-u>zz")
  (map :n "<C-d>" "<C-d>zz")
  (map :n "n" "nzz")
  (map :n "N" "Nzz")
  (map :n "*" "*zz")
  (map :n "#" "#zz")
  (map :n "G" "Gzz")
  (map :n "<C-i>" "<C-i>zz")
  (map :n "<C-o>" "<C-o>zz")
  ;; quickly delete the current buffer
  (map :n "<leader>bq" "<cmd>b#|bd#<cr>")
  ;; remove search highlighting on ESC
  (map :n "<esc>" "<cmd>noh<cr>")
  ;; windows
  (map :n "<C-l>" "<C-w><C-w>")
  (map :n "<C-w><C-d>" "<C-w>q")
  ;; format the whole file
  (map [:n :v] "<leader>bf" "gggqG")
  ;; heresy: allow saving with C-x C-s
  (map [:n :v] "<C-x><C-s>" "<cmd>w<cr>")
  (map :i "<C-x><C-s>" "<esc><cmd>w<cr>"))
