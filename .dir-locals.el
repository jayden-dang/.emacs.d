;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((magit-status-mode . ((magit-todos-exclude-globs . ("*.el" "*.orgcaptmpl" "html" "theme" "node_modules/**" "dist/**" "build/**" "org-themes/**" "snippets/**")))))

(setq-default eglot-workspace-configuration
    '((:gopls .
        ((staticcheck . t)
         (matcher . "CaseSensitive")))))

((nil (eglot-workspace-configuration . ((gopls . ((staticcheck . t)
              (matcher . "CaseSensitive")))))))

((nil
  (eglot-workspace-configuration
   . ((@emacs-grammarly/grammarly-languageserver
       . ((audience . "knowledgeable")))))))
