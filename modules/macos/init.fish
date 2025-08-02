# JAVA_HOME is dynamic depending on the version installed
set --global --export JAVA_HOME (/usr/libexec/java_home)

# Force homebrew-installed binaries to be first in PATH
fish_add_path --global --move --path "/opt/homebrew/bin" "/opt/homebrew/sbin"
