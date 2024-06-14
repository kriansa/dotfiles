/**
 * When the window is activated, call the session D-Bus method to set the foreground process.
 */
workspace.windowActivated.connect(function (client) {
  if (!client) {
    return
  }

  // Call the session D-Bus method to set the foreground process
  callDBus(
    "com.system76.Scheduler",
    "/com/system76/Scheduler",
    "com.system76.Scheduler",
    "SetForegroundProcess",
    client.pid,
  )
})
