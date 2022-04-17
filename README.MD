# Swift Import Formatter
## A Haskell script that formats all the import statements in your Swift projects.

### Please install stack to run the script. Instructions can be found here:
https://docs.haskellstack.org/en/stable/install_and_upgrade/

### Usage:
Place `import_formatter.sh` inside a script folder or at the root of the project directory. and simply run `./import_formatter.sh`

### Features:
If certain files need to be ignored due to macro imports. Please add them to the `ignoreSwiftFiles` section in the script. This also applies to subDirectories that you want to ignore. They have a separate `ignoreDirectories` section.