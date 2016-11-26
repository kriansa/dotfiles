# Atom

Whenever you install or remove any package, you should update this plugin by:

1. Go to this plugin root dir and:
  > apm list --installed --bare | sed -e 's/@.*//' > data/packages.txt
