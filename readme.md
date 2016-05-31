
# Summary

Build your package against different versions of stack.

You need a `stackage-multibuild.config` file in the project directory,
and then run `stackage-multibuild`.

# Syntax

This will add `lts-6.0` to the list of snapshots to build against:

```
lts: lts-6.0
```

This will add `nightly-2016-05-30` to the list of snapshots to build against:

```
nightly: nightly-2016-05-30
```

To add the latest version, for both `lts` and `nightly`, you can use `latest`:

```
lts: latest
```

Ranges can also be used (for both `lts` and `nightly`):

```
lts-range: lts-5.0 ~ latest
```

Finally, for `lts` snapshots you can use `selected` ranges. It works like
`lts-range`, but will only use the latest snapshot of each major release.

```
lts-range-selected: lts-2.0 ~ latest
```

# Example file

```
lts-range-selected: 2.0 ~ latest
nightly: latest
```
