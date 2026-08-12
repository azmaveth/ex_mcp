# 1.0.0-rc.7 doc patches

These unified diffs update files that could not be committed through the
GitHub MCP `push_files` tool because the full file payloads exceed the
practical remote tool argument size (~25–40 KiB).

Apply from the repo root:

```bash
bash docs/patches/rc7/APPLY.sh
```

Or manually:

```bash
patch -p1 < docs/patches/rc7/CHANGELOG.md.patch
patch -p1 < docs/patches/rc7/docs__CONFIGURATION.md.patch
patch -p1 < docs/patches/rc7/docs__MCP_2026_07_28_MIGRATION_PLAN.md.patch
```

After applying, commit the three full files and remove this `docs/patches/rc7/`
directory if desired.
