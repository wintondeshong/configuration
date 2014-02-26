Requirements
================

- Install Emacs 24.3 +

    ```
    brew install emacs —use-git-head —cocoa —srgb
    ```

- Install Cask

    ```
    curl -fsSkL https://raw.github.com/cask/cask/master/go | python
    ```

- Install/Update Cask Dependencies

    ```
    cask install
    ```

- Update unique system paths

    - Ruby Path for enh-ruby-mode in 'custom/01ruby.el'
    - Cask Version Number for auto complete dictionary path in 'custom/03auto-complete.el'


Highlights
===================

### Documentation Tools
- Markdown Mode

### General
- Solarized Color Theme

### Hacking
- Autocomplete.el
- Ruby
    - Enh Ruby Mode
    - RVM.el
- JavaScript
    - JavaScript Mode
    - CoffeeScript Mode
- Markup
    - Haml Mode
    - Sass Mode

### Key Remappings and Shortcuts
- C-C \ C-k : kill-region
- C-x \ C-b : buffer-menu
- C-x \ C-k : kill-region
- C-w : backword-kill-word
- C-<f5> : toggle line numbers

- [f2] : comment-region
- [f3] : uncomment-region
- [f5] : indent-region

- Command-t or Alt-t: Fuzzy Search current Projectile Project
- Command-/ or Alt-/: Switch Projectile Project
- Command-Shift-f or Alt-Shift-f: Grep Projectile Project
- Command-<escape> or Alt-<escape>: Clear Projectile Project Cache

### Project Management
- Projectile
