# Hy-Vee recipe search
Command-line application that reads `.recipe` and `.recipe.md` files then opens one Firefox tab for each distinct ingredient on Hy-Vee's online store. E.g., the .recipe file

```recipe  
milk[3 cups] | I like milk
```

will open the following search:

https://www.hy-vee.com/aisles-online/search?search=milk

Prefix an ingredient with `!` to look it up in `.items.recipe-db`. A matching keyword opens its Hy-Vee product page; an unmapped keyword falls back to a normal search. For example, with this database:

```text
milk:1415789
```

the recipe ingredient `!milk` opens `https://www.hy-vee.com/aisles-online/p/1415789`. Database entries use one `keyword:item-id` pair per line.

## Build and run

Install GHC and Cabal, then run from the repository root:

```powershell
cabal run hyvee-shop -- recipes/chicken-khao-soi.recipe.md
```
