# GnuCOBOL Practice
This project is for practicing COBOL. The COBOL environment is facilitated by the devcontainer

The devcontainer has COBOL related extensions already installed but feel free to improve on that.

## Instructions
- start the devcontainer(dev container extension from Microsoft required)
- write a properly formed COBOL program
- compile program using format below
```bash
cobc -x <<filename>>.cbl
```
- run compiled file
```bash
./<<filename>>
```

Please see the GnuCOBOL documentation on how to use the commands in the environment.
[GnuCOBOL Docummentation](https://gnucobol.sourceforge.io/doc/gnucobol.html)