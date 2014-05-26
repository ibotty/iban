http://www.swift.com/dsp/resources/documents/IBAN_Registry.pdf

```
> pdftotext IBAN_REGISTRY.pdf \
  | awk -v s=0 '/^[A-Z][A-Z]/ {if (s) print} /^IBAN structure$/ {s=1} /^IBAN length$/ {s=0}' | grep -v 'IBAN length' \
  > src/Finance/IBAN/Data.hs
```

and edit
