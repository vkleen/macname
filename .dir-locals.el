((nil . ((eval . (setq dante-project-root (projectile-project-root)))
         (eval . (setq dante-repl-command-line `("nix-shell" "--pure" "--run" "cabal v2-repl --builddir=dante" ,(concat (projectile-project-root) "/shell.nix")))))
      ))
