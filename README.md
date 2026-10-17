# Hy-Vee recipe search
Command-line application that reads `.recipe` and `.recipe.md` files then opens one Firefox tab for each distinct ingredient search on Hy-Vee's online store. E.g., the .recipe file 

```recipe  
milk[3 cups] | I like milk
```

will open the following search:

https://www.hy-vee.com/aisles-online/search?search=milk

## Build and run

Install GHC and Cabal, then run from the repository root:

```powershell
cabal run hyvee-shop -- recipes/chicken-khao-soi.recipe.md
```
