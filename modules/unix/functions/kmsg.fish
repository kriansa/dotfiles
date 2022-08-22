# Helps logging to kernel (dmesg)
function kmsg
  echo $argv | sudo tee /dev/kmsg
end
