# psmd-tools
PSMD Extraction and Analysis Toolkit

## Important notes for using
As of the current version, there are *very* important things to keep in mind:
* Some things might depend on SBCL instead of other Lisp implementations
* Everything assumes a specific file structure:
  * This repo's contents go into `/psmd/` by default; modify `*repo-root*` in `systems/psmd-dir.lisp` to change this location, if you wish to put this somewhere else (for instance, in your home directory)
  * The extracted ROMFS goes into `repo/romfs/` (e.g. `/psmd/romfs/banner.icn` would exist, by default)
  * The extracted *and decompressed* EXEFS goes into `repo/exefs/` (e.g. `/psmd/exefs/code.bin` would exist, by default)
* ASDF needs to be able to see the `repo/systems/` tree
