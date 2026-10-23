;; -*- lexical-binding: t; -*-

(use-package zig-mode
  :ensure t
  :vc (zig-mode :url "https://codeberg.org/ziglang/zig-mode"
                :rev "62bfbaced0222e2bfbc086fa8556adf6b3298476")
  :hook
  (zig-mode . eglot-ensure))
