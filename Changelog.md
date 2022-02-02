# Changelog

## Version 2.0.0

* Using attoparsec based parsers with more datails, as to why parsing failed.
  Thanks to Mihail Lazarev (@mikekeke)
* Internal IBAN and BBAN structure changed to make internal inconsistencies less likely
* Add Validity, GenValid, and Arbitrary instances for all types
* Drop IsText instances and provide a quasi-quoter.

## Version 0.1.1

 * Filter wrong characters in account number and blz in Finance.IBAN.Germany,
 * Update text dependency.

## Version 0.1

Initial version with validator for IBANs. Includes rudimentary German legacy account to IBAN functionality.
