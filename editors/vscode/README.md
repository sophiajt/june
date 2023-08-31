# Rust Syntax

This extension provides a TextMate grammar for Rust. In most cases, you won't need to install the extension, as this repository is upstreamed by VS Code (issues and PRs should be submitted here).

If you are doing a significant amount of Rust programming, the semantic highlighting provided by [Rust Analyzer][] will be superior to a textmate grammar. For example, semantic highlighting can easily distinguish enums, structs, and traits.

Rust Syntax is compatible with Rust Analyzer, but the scopes provided by this extension will not be visible while semantic highlighting is enabled. If for some reason you would like to disable semantic highlighting, you can do this in your `settings.json`:

```json
"[rust]": {
    "editor.semanticHighlighting.enabled": false
}
```

## Compatibility

Not all themes are specifically optimized for Rust.
We have tried to provide sensible default scopes that will work with most themes.
If you want to modify the colors in a particular theme, you can do so in your `settings.json`:

```json
"editor.tokenColorCustomizations": {
    "[Theme Name]": {
        "textMateRules": [
            {
                "scope": "variable.other.rust",
                "settings": {
                    "foreground": "#ffff00"
                }
            }
        ]
    }
}
```

The VS Code command `Developer: Inspect Editor Tokens and Scopes` will show you the scope stack at the current cursor position.

## Contributing

The grammar is maintained as YAML, using tasks to generate JSON on save (please don't edit the JSON grammar directly).
You can regenerate the JSON manually from the command palette using `Tasks: Run Build Task`.

[Rust Analyzer]: https://marketplace.visualstudio.com/items?itemName=matklad.rust-analyzer
