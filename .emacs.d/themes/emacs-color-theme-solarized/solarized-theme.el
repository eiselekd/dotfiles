(require 'solarized-definitions
         (locate-file "solarized-definitions.el" custom-theme-load-path
                      '("c" "")))

(message "[+] define solarized console")
(create-solarized-theme solarized
                        solarized-description (solarized-color-definitions))
