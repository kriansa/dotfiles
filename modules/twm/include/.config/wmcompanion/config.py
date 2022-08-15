from types import SimpleNamespace
from asyncio import sleep
from wmcompanion import use, on
from wmcompanion.utils.subprocess import cmd, shell
from wmcompanion.modules.polybar import Polybar
from wmcompanion.modules.notifications import Notify, Urgency
from wmcompanion.events.bluetooth import BluetoothRadioStatus
from wmcompanion.events.notifications import DunstPausedStatus
from wmcompanion.events.keyboard import KbddChangeLayout
from wmcompanion.events.audio import MainVolumeLevel
from wmcompanion.events.network import WifiStatus, NetworkConnectionStatus
from wmcompanion.events.power import PowerActions

colors = SimpleNamespace(BAR_FG="#F2F5EA", BAR_DISABLED="#2E5460")

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
        elif not volume["muted"]:
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
@use(Notify)
async def power_menu(status: dict, notify: Notify):
    if status["event"] != PowerActions.Events.POWER_BUTTON_PRESS: return
    await cmd("power-menu")

@on(PowerActions)
@use(Notify)
async def battery_warning(status: dict, notify: Notify):
    ignore_battery_statuses = [PowerActions.BatteryStatus.CHARGING, PowerActions.BatteryStatus.FULL]
    if status["event"] != PowerActions.Events.BATTERY_LEVEL_CHANGE or \
        status["battery-status"] in ignore_battery_statuses: return

    level = status["battery-level"]
    if level > 10: return

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
    states = [PowerActions.Events.INITIAL_STATE, PowerActions.Events.POWER_SOURCE_SWITCH]
    if status["event"] not in states: return

    if status["power-source"] == PowerActions.PowerSource.AC:
        cpu_governor = "performance"
        screen_saver = "300"
        backlight = "70"
    elif status["power-source"] == PowerActions.PowerSource.BATTERY:
        cpu_governor = "powersave"
        screen_saver = "60"
        backlight = "30"

    await cmd("sudo", "cpupower", "frequency-set", "-g", cpu_governor)
    await cmd("xset", "s", screen_saver)
    await cmd("xbacklight", "-ctrl", "intel_backlight", "-set", backlight)

@on(PowerActions)
async def autosuspend_on_battery(status: dict):
    if status["event"] not in [
        PowerActions.Events.INITIAL_STATE,
        PowerActions.Events.POWER_SOURCE_SWITCH,
        PowerActions.Events.LID_CLOSE,
    ]: return

    on_battery = status["power-source"] == PowerActions.PowerSource.BATTERY
    lid_is_closed = status["lid-state"] == PowerActions.LidState.CLOSED
    if on_battery and lid_is_closed:
        await cmd("suspend-countdown")
