# pgne.el - an emacs mode for electrically balancing complex pairs

`pgne-mode` is a emacs minor-mode that allows inserting the closing
pair of a syntax node. This doesn't sound too dissimilar from
`electric-pair-mode`, but in contrast to `electric-pair-mode`,
`pgne-mode` allows balancing character sequences based on the
`treesit` parse state. This, in theory, allows you to balance a ruby
`def` with a corresponding `end`.

## Status

I've incrementally gotten this to "work" in nix-ts-mode, where it can
insert balanced parens and `let`/`in` as well as `if`/`then else`
pairs. I'm semi-convinced that it'll work in other modes too, but
there are some still-glaring bugs and problems to iron out.
