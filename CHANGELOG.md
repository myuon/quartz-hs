# Revision history for quartz

## 0.2.2.0 -- 2020-03-07

- Formatter Update
  - Preserves newlines between expressions in statement blocks

## 0.2.1.0 -- 2020-03-04

- Parser Rewrites
  - Rewrite Parser by hand

## 0.2.0.0 -- 2020-02-28

- Breaking change: Towards LL(1) syntax
  - Type application becomes `[]` instead of `<>`
  - Lambda abstration becomes `[tyvars](arg) => expr`

## 0.1.0.0 -- 2020-02-23

- First release
  - Most syntax/semantics are hopefully fixed
  - Incomplete support of everything
