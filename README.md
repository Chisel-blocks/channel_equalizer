# Description of scala module version control principle with git
Generated by init_module-sh of dsp-tool-helpers, 20190429

## Principle of operation:
All modules version controlled with this method can be used effortlessly
and recursively as git submodules inside similar modules

The method of use is _always_ the same
1. `./init_submodules.sh` (if any)
2. Publish locally the submodules you want to use.
    (embedded to init_submodules.sh)
3. `./configure && make`

## Version strings:
In build.sbt, the version of the current module is of from
"module-<commit-hash>-SNAPHOT"
It is created with line:
`version := scala.sys.process.Process("git rev-parse --short HEAD").!!.mkString.replaceAll("\\s", "")+"-SNAPSHOT"`

Dependencies on similar submodules are defined with the
function gitSubmoduleHashSnapshotVersion
and with the dependency definitions
`libraryDependencies += "edu.berkeley.cs" %% "hbwif" % gitSubmoduleHashSnapshotVersion(modulename")`

This dependency is satisfied only if there is a locally published (sbt publishLocal) submodule
with the submodule hash of the current git submodule.

**OBS1**: Every time a submodule is updated, it must be published locally.
See init_submodules.sh for reference. Make it recursive if needed.

**OBS2**: If submodules are edited and committed, the changes are visible
at the top level ONLY if ALL the entire hierarchy of submodules from bottom
module to top are git-added, git-committed and git-pushed.
This is how submodules normally operate.

## Add your module readme here
Lorem ipsum...

