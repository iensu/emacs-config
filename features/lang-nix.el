;; -*- lexical-binding: t; -*-

(use-package nix-mode
  :ensure t
  :vc (nix-mode :url "https://github.com/nixos/nix-mode"
                :rev "2c77e7e0b7540efbb20ccaee3557ef90a5dc77f0")
  :mode (("\\flake.lock\\'" . json-ts-mode)))
