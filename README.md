# psmd-tools
PSMD Extraction and Analysis Toolkit

## Important notes for using
As of the current version, there are *very* important things to keep in mind:
* Some things might depend on SBCL instead of other Lisp implementations
* Everything assumes a specific file structure:
  * This repo's contents go into `/psmd/`
  * The extracted ROMFS goes into `/psmd/romfs/` (e.g. `/psmd/romfs/banner.icn`)
  * The extracted *and decompressed* EXEFS goes into `/psmd/exefs/` (e.g. `/psmd/exefs/code.bin`)
* ASDF needs to be able to see the `systems/` tree
