# Maintainer notes

## Formatting

Use [ormolu](https://hackage.haskell.org/package/ormolu) for automatic formatting.

## Data Files

The data files are not auto-generated but written script-assisted.

### `Finance.IBAN.Data`

```
> curl -O http://www.swift.com/dsp/resources/documents/IBAN_Registry.pdf
> pdftotext IBAN_REGISTRY.pdf \
  | awk -v s=0 '/^[A-Z][A-Z]/ {if (s) print} /^IBAN [sS]tructure/ {s=1} /^IBAN length/ {s=0}' | grep -v 'IBAN length' \
  > src/Finance/IBAN/Data.hs
```

and edit, correct and add missing because of inconsistencies and errors in the
document.

### `Finance.IBAN.Germany.Data`

Get latest `xlsx` from
https://www.bundesbank.de/Redaktion/DE/Standardartikel/Aufgaben/Unbarer_Zahlungsverkehr/bankleitzahlen_download.html

uncomment blz executable in `iban.cabal` (There surely is a way to not add the
dependencies to it when stating the executable).

```
> unoconv -f csv -e FilterOptions=44,34,76  blz_2014_06_09_xls.xlsx
> cabal run blz blz_2014_06_09_xls.xlsx \
    > src/Data/Finance/IBAN/Germany/Data.hs
```

And edit to taste. Should be automated.
