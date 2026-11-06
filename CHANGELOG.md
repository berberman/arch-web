# arch-web

# 0.3.2

* Use template haskell to generate SPDX license data type based on `data/KNOWN-SPDX.json`
* Remove some dependency bounds
* Breaking change: `licenseId (Custom "x")` now returns `"x"` instead of `"custom: x"`. This ensures that `licenseId (parseLicense x) == x` for all `x`.

## 0.3.1

Fix typo in license

## 0.3

Migrate to SPDX License Identifiers

## 0.2

* Allow lens 5.1/5.2, aeson 2.1, servant 0.19.1+
* Adapt to Arch Linux's git migration
* Add [gnome-unstable] repo

## 0.1.1

* Add special case licenses
* Add [kde-unstable] repo

## 0.1.0

Initial release
