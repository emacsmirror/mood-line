# mood-line

[![MELPA](https://melpa.org/packages/mood-line-badge.svg)](https://melpa.org/#/mood-line)
[![MELPA Stable](https://stable.melpa.org/packages/mood-line-badge.svg)](https://stable.melpa.org/#/mood-line)

## About

mood-line is a lightweight, drop-in replacement for the default Emacs mode line configuration.

## Features

* Clean, informative design

* Customizable, modular segment format

* Customizable glyph sets

* Lazy-loaded extensions

* Lightweight, no dependencies

## Preview

![Preview Image](.repo-assets/preview.png "Preview Image")

## Configuration

You can install mood-line directly via `package-install` from [MELPA](https://melpa.org/).
After installation, you can activate the global minor mode with `M-x mood-line-mode`.
Deactivating `mode-line-mode` will restore the default `mode-line-format`.

If you are a user of `use-package`, it is easy to configure mood-line directly in your init.el:

```elisp
(use-package mood-line

  ;; Enable mood-line
  :config
  (mood-line-mode)

  ;; Use pretty Fira Code-compatible glyphs
  :custom
  (mood-line-glyph-alist . mood-line-glyphs-fira-code))
```

### Format

mood-line uses a modular segment format, and it is easy to reconfigure:

```elisp
;; Default format:
;;   * init.el  4:32 Top                                         ELisp  ! Issues: 2
(setq mood-line-format mood-line-format-default)

;; Extended format:
;;   * init.el  4:32:52 Top                    SPCx2  LF  UTF-8  ELisp  ! Issues: 2
(setq mood-line-format mood-line-format-default-extended)

;; Custom format:
;;   * init.el : ELisp                                     Top 4:32  |  ! Issues: 2
(setq mood-line-format
      (mood-line-defformat
       ;; Left side
       (" "
        ((mood-line-segment-buffer-status) . " ")
        ((mood-line-segment-buffer-name)   . " : ")
        (mood-line-segment-major-mode))
       ;; Right side
       (((mood-line-segment-scroll)             . " ")
        ((mood-line-segment-cursor-position)    . "  ")
        ((when (mood-line-segment-checker) "|") . "  ")
        ((mood-line-segment-checker)            . "  ")
        " ")))
```

More information on the format specification is available in the documentation: \
`M-x describe-variable mood-line-format` \
`M-x describe-function mood-line-defformat`

### Glyphs

By default, mood-line will use basic ASCII character glyphs to decorate mode line segments.
If you'd like to see prettier Unicode glyphs, you can change the value of `mood-line-glyph-alist`:

```elisp
;; The default set of glyphs:
;;   * myModifiedFile.js  Replace*3                 + main  Javascript  ! Issues: 2
(setq mood-line-glyph-alist mood-line-glyphs-ascii)

;; A set of Fira Code-compatible Unicode glyphs:
;;   ● myModifiedFile.js  Replace×3                 + main  JavaScript  → Issues: 2
(setq mood-line-glyph-alist mood-line-glyphs-fira-code)

;; A set of Unicode glyphs:
;;   ● myModifiedFile.js  Replace✕3                 🞤 main  JavaScript  ⚑ Issues: 2
(setq mood-line-glyph-alist mood-line-glyphs-unicode)
```

If you'd like to supply your own glyphs, you can use the customization interface
(`M-x customize-variable mood-line-glyph-alist`) or view the documentation
(`M-x describe-variable mood-line-glyph-alist`) for more information.

You can further tweak the behavior and appearance of mood-line by viewing the customizable variables
and faces in the `mood-line` and `mood-line-faces` customization groups. (`M-x customize-group mood-line`)

## Testing

To run the included tests:

```bash
./ert-test.sh
```

## Feedback

If you experience any issues with this package, please
[open an issue](https://gitlab.com/jessieh/mood-line/issues/new)
on the issue tracker.

Suggestions for improvements and feature requests are always appreciated, as well!
