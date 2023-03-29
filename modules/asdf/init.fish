function _asdf_add_path --on-event modify_path
  if test (uname) = "Linux"
    source /opt/asdf-vm/asdf.fish
  else
    source /opt/homebrew/opt/asdf/libexec/asdf.fish
  end
end
