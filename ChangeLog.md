### 0.1.1.3

* Fix bug in float regexp being too lax in the JSON and Core schema ([#7](https://github.com/hvr/HsYAML/issues/7))

### 0.1.1.2

* Tolerate BOM at *each* `l-document-prefix` (rather than only at the first one encountered in a YAML stream)
* Workaround broken `mtl-2.2.2` bundled in GHC 8.4.1 ([#1](https://github.com/hvr/HsYAML/issues/1))
* Relax to GPL-2.0-or-later

### 0.1.1.1

* Reject (illegal) non-scalar code-points in UTF-32 streams
* Tolerate BOM at start of stream
* Disambiguate choice in `l-any-document` production regarding token separation of `c-directives-end`
* Fix `c-indentation-indicator(n)` grammar production when
  auto-detecting indentation in the presence of empty leading lines;
  also reject (illegal) auto-indent-level scalars with leading
  more-indented all-space lines
* Complete character escape rules for double-quoted scalars
* Minor optimizations

### 0.1.1.0

* `Data.YAML` module promoted from `TrustWorthy` to `Safe`
* Add `FromYAML Natural` instance
* Add `MonadFail`, `Alternative`, and `MonadPlus` instances for `Data.YAML.Parser`
* Add `Data.YAML.decodeStrict` function
* Export `Data.YAML.typeMismatch` helper function

## 0.1.0.0

* First version. Released on an unsuspecting world.
