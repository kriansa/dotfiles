# pylint: disable=missing-function-docstring, missing-module-docstring
from types import SimpleNamespace
from logging import getLogger
from asyncio import sleep, get_running_loop, CancelledError
from pathlib import Path
from contextlib import suppress
from time import time

from wmcompanion import use, on
from wmcompanion.utils.process import cmd
from wmcompanion.modules.polybar import Polybar
from wmcompanion.modules.notifications import Notify, Urgency
from wmcompanion.events.bluetooth import BluetoothRadioStatus
from wmcompanion.events.notifications import DunstPausedStatus
from wmcompanion.events.keyboard import KbddChangeLayout
from wmcompanion.events.audio import MainVolumeLevel
from wmcompanion.events.network import WifiStatus, NetworkConnectionStatus
from wmcompanion.events.power import PowerActions, LogindIdleStatus
from wmcompanion.events.x11 import DeviceState

colors = SimpleNamespace(BAR_FG="#F2F5EA", BAR_DISABLED="#2E5460")
logger = getLogger(__name__)

@on(BluetoothRadioStatus)
@use(Polybar)
async def bluetooth_status(status: dict, polybar: Polybar):
    icon_color = colors.BAR_FG if status["enabled"] else colors.BAR_DISABLED
    await polybar("bluetooth", polybar.fmt("", color = icon_color))

@on(DunstPausedStatus)
@use(Polybar)
async def dunst_status(status: dict, polybar: Polybar):
    if status["paused"]:
        content = polybar.fmt("", color=colors.BAR_DISABLED)
    else:
        content = polybar.fmt("", color=colors.BAR_FG)

    await polybar("dunst", content)

@on(KbddChangeLayout)
@use(Polybar)
async def kbdd_layout(layout: dict, polybar: Polybar):
    layout_mappings = ["U.S.", "INT."]
    layout_id = layout["id"]

    if len(layout_mappings) >= layout_id + 1:
        output = layout_mappings[layout_id]
    else:
        output = f"Unknown: {layout}"

    await polybar("kbdd", polybar.fmt("", color=colors.BAR_FG), output)

@on(WifiStatus)
@use(Polybar)
async def wifi_status(status: dict, polybar: Polybar):
    if status["connected"]:
        color = colors.BAR_FG
        label = f"{status['strength']}%"
    elif not status["enabled"]:
        color = colors.BAR_DISABLED
        label = ""
    else:
        color = colors.BAR_FG
        label = ""

    await polybar("wlan", polybar.fmt("", color=color), label)

@on(NetworkConnectionStatus, connection_name="Wired-Network")
@use(Polybar)
async def network_status(status: dict, polybar: Polybar):
    color = colors.BAR_FG if status["connected"] else colors.BAR_DISABLED
    await polybar("eth", polybar.fmt("", color=color))

@on(MainVolumeLevel)
@use(Polybar)
async def volume_level(volume: dict, polybar: Polybar):
    async def render(polybar_module, icon_on, icon_muted, volume):
        if not volume["available"]:
            return await polybar(polybar_module, "")

        if not volume["muted"]:
            icon = icon_on
            color = colors.BAR_FG
        else:
            icon = icon_muted
            color = colors.BAR_DISABLED

        level = int(volume['level'] * 100)
        await polybar(polybar_module, polybar.fmt(f"{icon} {level}%", color=color))

    await render("mic", "", "", volume["input"])
    await render("speaker", "", "", volume["output"])

@on(PowerActions)
async def power_menu(status: dict):
    if status["event"] == PowerActions.Events.POWER_BUTTON_PRESS:
        await cmd("power-menu")

@on(PowerActions)
@use(Notify)
async def battery_warning(status: dict, notify: Notify):
    print(status)
    ignore_battery_statuses = [PowerActions.BatteryStatus.CHARGING, PowerActions.BatteryStatus.FULL]
    if status["event"] != PowerActions.Events.BATTERY_LEVEL_CHANGE or \
        status["battery-status"] in ignore_battery_statuses: return

    level = status["battery-level"]
    if level > 10:
        return

    if level > 5:
        await notify(
            f"Battery is low ({level}%)", "System will hibernate automatically at 5%",
            urgency=Urgency.NORMAL,
            dunst_stack_tag="low-battery-warn",
            icon="battery-level-10-symbolic",
        )
    else:
        await notify(
            "Hibernating in 5 seconds...",
            urgency=Urgency.CRITICAL,
            dunst_stack_tag="low-battery-warn",
            icon="battery-level-0-symbolic",
        )
        await sleep(5)
        await cmd("systemctl", "hibernate")

@on(PowerActions)
async def set_power_profile(status: dict):
    if status["event"] not in [
        PowerActions.Events.INITIAL_STATE,
        PowerActions.Events.POWER_SOURCE_SWITCH,
        PowerActions.Events.RETURN_FROM_SLEEP,
    ]: return

    if status["power-source"] == PowerActions.PowerSource.AC:
        cpu_governor = "performance"
        screen_saver = "300"
        backlight = "70"
    elif status["power-source"] == PowerActions.PowerSource.BATTERY:
        cpu_governor = "powersave"
        screen_saver = "60"
        backlight = "30"

    await cmd("sudo", "cpupower", "frequency-set", "-g", cpu_governor)

    # xset s <timeout> <cycle>
    # The meaning of these values are that timeout is how much time after idling it will trigger the
    # ScreenSaver ON, while the cycle is, after being on, how often it will trigger its cycle event,
    # originally meant for changing background patterns to avoid burn-in, but in this case it's used
    # to flag `xss-lock` that the locker can be called - otherwise, it would only call the notify
    # program. See more on xss-lock(1) and on screen-locker.
    #
    # Recommendation: Keep the second parameter the amount of time that the dimmer needs to fade out
    # completely before showing the locker - usually 5 seconds (see: screen-locker)
    await cmd("xset", "s", screen_saver, "5")
    await cmd("xbacklight", "-ctrl", "intel_backlight", "-set", backlight)

class PowerState:  # pylint: disable=too-few-public-methods
    """
    Automatically hibernates the laptop when undocked or when idling for too long on battery.
    """

    IS_IDLE = False
    ON_BATTERY = False
    NO_SCREENS = False

    @staticmethod
    async def auto_suspend():
        if hasattr(PowerState, "idle_timer") and PowerState.idle_timer:
            # On every power state change, let's cancel the idle timer so we can ensure that its
            # timer is only ever activated when on battery
            PowerState.idle_timer.cancel()
            PowerState.idle_timer = None

        if PowerState.ON_BATTERY and PowerState.NO_SCREENS:
            # Waits 5 seconds before actually suspending, in case we undock the computer but quickly
            # plug it back again
            await sleep(5)
            if PowerState.ON_BATTERY and PowerState.NO_SCREENS:
                await cmd("systemctl", "hibernate")

        if PowerState.IS_IDLE and PowerState.ON_BATTERY:
            # When idling on battery, we start a timer to automatic hibernation
            PowerState.idle_timer = get_running_loop().create_task(PowerState._start_idle_timer())

    @staticmethod
    async def _start_idle_timer():
        with suppress(CancelledError):
            await sleep(2 * 60)
            await cmd("systemctl", "hibernate")

@on(PowerActions)
async def hibernate_on_battery(status: dict):
    if status["event"] not in [
        PowerActions.Events.INITIAL_STATE,
        PowerActions.Events.POWER_SOURCE_SWITCH,
        PowerActions.Events.RETURN_FROM_SLEEP,
    ]: return

    PowerState.ON_BATTERY = status["power-source"] == PowerActions.PowerSource.BATTERY
    await PowerState.auto_suspend()

@on(DeviceState)
async def hibernate_when_undocked(status: dict):
    if status["event"] != DeviceState.ChangeEvent.SCREEN_CHANGE:
        return

    PowerState.NO_SCREENS = len(status["screens"]) == 0
    await PowerState.auto_suspend()

@on(LogindIdleStatus)
async def hibernate_when_idle(status: dict):
    PowerState.IS_IDLE = status["idle"]
    await PowerState.auto_suspend()

@on(DeviceState)
@use(Notify)
async def configure_screens(status: dict, notify: Notify):
    if status["event"] != DeviceState.ChangeEvent.SCREEN_CHANGE:
        return

    edid_aliases = {
        "7B59785F": "LAPTOP",
        "047F801B": "PRIMARY",
        "93F8B109": "PRIMARY",
        "266B2343": "PRIMARY",
        "E65018AA": "PRIMARY",
        "95BB6889": "PRIMARY",
        "40D8582C": "PRIMARY",
    }

    screens = []
    for screen in status["screens"]:
        if screen["edid_hash"] in edid_aliases:
            screen_id = edid_aliases[screen["edid_hash"]]
        else:
            screen_id = screen["edid_hash"]

        screens.append([screen["output"], screen_id])

    start_time = time()
    xrandr_time = None
    match screens:
        # Single monitor
        case [[output, _]]:
            await cmd("xrandrw", "--outputs-off", "--output", output, "--auto", "--primary")
            xrandr_time = time() - start_time

        # Configured layouts
        case [["eDP-1", "LAPTOP"], [primary_output, "PRIMARY"]]:
            await cmd(
                "xrandrw", "--outputs-off",
                "--output", primary_output, "--preferred", "--pos", "0x0", "--primary",
                "--output", "eDP-1", "--preferred", "--pos", "3440x740",
            )
            xrandr_time = time() - start_time

        # No monitors
        case []:
            await cmd("xrandrw", "--outputs-off")
            return

        # Layout not configured
        case _:
            cmdline = []
            for screen in screens:
                cmdline += ["--output", screen[0], "--auto"]

            await cmd("xrandrw", "--outputs-off", *cmdline)
            xrandr_time = time() - start_time

            await notify(
                "Monitor combination not configured",
                "Run 'Arandr' to configure it manually.",
            )

    # I don't want to restart polybar if there was no change on xrandr
    # We consider no change on xrandr when it runs "too quickly" -- subjectively
    if xrandr_time > 0.1:
        await cmd("systemctl", "reload-or-restart", "--user", "polybar")

    # Apply background
    await cmd(
        "feh", "--bg-fill", "--no-fehbg",
        Path.home().joinpath(".local/share/backgrounds/kerevel.png"),
    )

@on(DeviceState)
async def configure_inputs(status: dict):
    if status["event"] != DeviceState.ChangeEvent.INPUT_CHANGE:
        return

    # I only care about added events
    if "added" not in status["inputs"]:
        return

    for device in status["inputs"]["added"]:
        dev_id = str(device["id"])

        if device["type"] == "slave-keyboard":
            await cmd(
                "setxkbmap",
                "-model", "pc104",
                "-layout", "us,us",
                "-variant", ",alt-intl",
                "-option", "", "-option", "grp:caps_toggle"
            )
            await cmd("xset", "r", "rate", "300", "30")

        elif device["type"] == "slave-pointer":
            if device["name"] == "Razer Razer DeathAdder V2":
                await cmd("xinput", "set-prop", dev_id, "libinput Accel Speed", "-0.800000")
            elif device["name"] == "DELL0A71:00 04F3:317E Touchpad":
                await cmd("xinput", "set-prop", dev_id, "libinput Tapping Enabled", "1")
                await cmd("xinput", "set-prop", dev_id, "libinput Natural Scrolling Enabled", "1")
                await cmd("xinput", "set-prop", dev_id, "libinput Tapping Drag Lock Enabled", "1")
