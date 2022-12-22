# mood-line

[![MELPA](https://melpa.org/packages/mood-line-badge.svg)](https://melpa.org/#/mood-line)
[![MELPA Stable](https://stable.melpa.org/packages/mood-line-badge.svg)](https://stable.melpa.org/#/mood-line)

## About

mood-line is a minimal mode-line configuration that aims to replicate some of the features of the
more advanced [doom-modeline](https://github.com/seagle0128/doom-modeline) package.

## Features

* Clean, minimal design

* Customizable glyph sets

* Anzu and multiple-cursors counters

* Encoding and EOL style indicator

* Version control status indicator

* Custom Flycheck/Flymake indicator

* Lightweight with no dependencies

## Preview

![Preview Image](https://gitlab.com/jessieh/mood-line/raw/assets/mood-line.png "Preview Image")

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

## Feedback

If you experience any issues with this package, please
[open an issue](https://gitlab.com/jessieh/mood-line/issues/new)
on the issue tracker.

Suggestions for improvements and feature requests are always appreciated, as well!
