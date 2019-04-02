# Updating

1. __Antigen:__ is a self-contained file. Update it by fetching the latest version on their
   [repo](https://github.com/zsh-users/antigen) and put it on `plugins/zsh/antigen.zsh`.
2. __Antigen remote packages:__ run `$ antigen update`
3. __Ruby & Javascript:__ whenever a new version is out, update the file
   `ansible/roles/dev-tools/default/main.yml` with the newer versions. Then run `$
   update-node-packages && update-ruby-packages`
