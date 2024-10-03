# `pulsic`

The package provides a global minor mode that temporarily highlights the
current line on every window state change (see `window-state-change-hook`),
except when the last command called is listed in `pulsic-exceptions`.

Similar package: pulsar.el

TOC

- [Installation](#installation)
- [Screenshots and configuration examples](#screenshots-and-configuration-examples)

## Installation

``` elisp
(use-package pulsic
  :init
  (unless (package-installed-p 'pulsic)
    (package-vc-install
     '(pulsic :vc-backend Git
              :url "https://github.com/ichernyshovvv/pulsic.el")))
  :custom
  (pulsic-iterations 1)
  (pulsic-delay 0.1)
  :config
  (pulsic-mode 1))
```

## Screenshots and configuration examples
