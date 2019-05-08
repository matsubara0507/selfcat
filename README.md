# selfcat

[![](https://images.microbadger.com/badges/image/matsubara0507/selfcat.svg)](https://microbadger.com/images/matsubara0507/selfcat "Get your own image badge on microbadger.com")

collect self GitHub's status

## Install

use Haskell Stack:

```
$ stack install
```

use Docker Imgae:

```
$ docker pull matsubara0507/selfcat
```

## Usage

```
$ selfcat --help
unrecognized option `--help'

selfcat [options] [input-file]
           --version      Show version
  -v       --verbose      Enable verbose mode: verbosity level "debug"
  -o PATH  --output=PATH  Directory path for outputs
```

Set GitHub Personal Token (read only public repo) in `.env`.
And exec command with example:

```
$ selfcat --output=example/outputs example/.selfcat.yaml
Success.
```
