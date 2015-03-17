((haskell-mode . ((haskell-indent-spaces . 4)
                  (haskell-process-type . ghci)
                  (haskell-process-path-ghci . "ghci")
                  (haskell-process-wrapper-function
                   . (lambda (args)
                       (append
                        (list "fpbuild"
                              "--docker-run-args=[\"--interactive=true\",\"--tty=false\"]"
                              "exec"
                              "--")
                        args)))
                  (haskell-process-use-ghci . t)))
 (hamlet-mode . ((hamlet/basic-offset . 4)
                 (haskell-process-use-ghci . t))))
