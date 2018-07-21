# www.aligus.net

## Requirements


### Latex
e.g. MacTeX: http://www.tug.org/mactex/

### ImageMagick's convert

```bash
sudo port install ImageMagick
```

## Build

+* required to use `--recursive` to git clone because it contains git submodules

```bash
stack build
stack exec site watch
```
