# Source VTE on the current shell
# This is useful for Tilix to work correctly
# More info: https://gnunn1.github.io/tilix-web/manual/vteconfig/
if [ $TILIX_ID ] || [ $VTE_VERSION ]; then
  source /etc/profile.d/vte.sh
fi
