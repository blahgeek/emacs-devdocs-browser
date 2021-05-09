# Emacs Devdocs Browser

Browse devdocs.io documents inside Emacs!

## Features

- Browse API documentations provided by [devdocs.io](https://devdocs.io/) inside Emacs using [EWW](https://www.emacswiki.org/emacs/eww), with some custom features:
  * Highlighted code blocks
  * Better formatting
  * Extra commands like "goto other sections in current page"
- Manage (install, upgrade, uninstall, etc.) docsets
- Optionally download full content for offline usage

## Quick Start

1. Install package.

This package is not submitted to MELPA yet,
you need to manually clone this git repo and add it to `load-path`,
or use some package manager like [straight.el](https://github.com/raxod502/straight.el).

This package does not depend on other libraries.

2. Install some docs using `M-x devdocs-browser-install-doc`.
3. Browse the docs using `M-x devdocs-browser-open` or `M-x devdocs-browser-open-in`.

## FAQ

- Why browsing the doc inside Emacs instead of opening the page in the web browser?

Less navigation, consistent theme/color, consistent keybindings.

- Why using EWW instead of XWidget, [emacs-webkit](https://github.com/akirakyle/emacs-webkit) or [EAF](https://github.com/manateelazycat/emacs-application-framework/)?

    1. EWW is a builtin package and written in elisp: works across platforms, well supported, customizable.
    2. EWW is text-based: consistent theme/color, consistent keybindings, copiable and searchable, faster and use less resources.
    
- Why Devdocs.io instead of [Dash docs](https://github.com/dash-docs-el/helm-dash)?

Devdocs.io provides all API documentations converted to plain HTML content
without custom styling, scripting, headers or footers, which makes it very suitable for EWW.

## References

WIP
