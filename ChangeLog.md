## 0.2.0.0

This release incorporates the work from [Vijay Tadikamalla's GSOC 2019
Project](https://vijayphoenix.github.io/blog/gsoc-the-conclusion/).
Highlights of this major release include support for emitting YAML as
well as providing direct access to source locations throughout the
parsing pipeline for improved error reporting.

* Changes in `Data.YAML` module
    * YAML 1.2 Schema encoders (#21)
    * New `ToYAML` class for encoding Haskell Data-types from which YAML nodes can be constructed (#20)
        * New functions like `encodeNode`, `encodeNode'` for constructing AST.
        * New functions like `encode`, `encode1`, `encodeStrict`, `encode1Strict` for supporting typeclass-based dumping
        * Some ToYAML instances and other api
    * Modify `typeMismatch` function to show error source location in error messages (#19)
    * Provide location-aware 'failAtNode' alternative to 'fail'

* Changes in `Data.YAML.Event` module
    * Preserve and round-trip Comments at Event level(#24)
        * New  `Comment` Event to preserve comments while parsing
        * Some additional implementations to preserve and round-trip comments
    * Fix issue #22
    * New `EvPos` type for recording event and their corresponding position (#19)
    * Preserve Flow Mapping and Flow sequence (#18)
    * Features to preserve Literal/Folded ScalarStyle (#15)
        * New `Chomp` type denoting Block Chomping Indicator
        * New `IndentOfs` type denoting Block Indentation Indicator
    * New `NodeStyle` type denoting flow/block style
    * `Event(SequenceStart,MappingStart)` constructors now record `NodeStyle`
    * `Style` type renamed to `ScalarType`
    * New `writeEvents` and `writeEventsText` function
    * `Event(DocumentStart)` now records YAML directive
    * Event parser now rejects duplicate/unsupported YAML/TAG
      directives as mandated by the YAML 1.2 specification

* Move some schema related definitions from `Data.YAML` into the new `Data.YAML.Schema` module

* Make `decode`, `decode1`, `decodeStrict`, `decode1Strict`, `decodeNode`, and `decodeNode'` treat
  duplicate keys (under the respective YAML schema) in YAML mappings
  as a loader-error (controllable via new
  `schemaResolverMappingDuplicates` schema property)

* Define `Generic` and `NFData` instances for most types

* Fix `X38W` testcase (#13, #14)

---

### 0.1.1.3

* Fix bug in float regexp being too lax in the JSON and Core schema ([#7](https://github.com/hvr/HsYAML/issues/7))
* Remove dependency on `dlist`

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
