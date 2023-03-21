function kmsg --description "Logs a message to kernel (dmesg)"
  echo $argv | sudo tee /dev/kmsg
end
