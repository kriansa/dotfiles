/*
 * Puts windows back into the custom tile they were last in when they open again.
 *
 * Only windows matched by a KWin window rule whose description contains "[tile-memory]" take
 * part. Each tagged rule remembers a single tile: the last custom tile any of its windows was in.
 * When a matching window opens, it goes into that tile only if:
 *   - the window opened on the tile's virtual desktop (window rules may have chosen it),
 *   - the monitor is connected and the tile layout there still has the same shape,
 *   - no window rule matching it forces a position or size, which would fight the tile.
 */
import QtQuick
import QtCore
import org.kde.kwin

Item {
    id: root

    readonly property string marker: "[tile-memory]"
    readonly property string rulesFile: StandardPaths.writableLocation(StandardPaths.ConfigLocation) + "/kwinrulesrc"
    readonly property string stateFile: StandardPaths.writableLocation(StandardPaths.GenericStateLocation) + "/tile-memory/state.ini"

    readonly property var ruleKeys: [
        "Description", "Enabled", "types", "wmclass", "wmclasscomplete", "wmclassmatch",
        "windowrole", "windowrolematch", "title", "titlematch", "tag", "tagmatch",
        "hastransientparent", "hastransientparentmatch", "positionrule", "sizerule",
    ]
    // Values KWin stores in kwinrulesrc for "Force" and "Force temporarily".
    readonly property var forcingRuleTypes: [2, 6]
    // Values KWin stores for "Exact match", "Substring match" and "Regular expression".
    readonly property int exactMatch: 1
    readonly property int substringMatch: 2
    readonly property int regexpMatch: 3

    Component {
        id: iniGroup
        Settings {}
    }

    Settings {
        id: memoryStore
        location: root.stateFile
        category: "memory"
    }

    // Reads some keys of one group from an INI-style file. Qt's reader treats the "[General]"
    // group as top-level keys, so the empty group name reads it. It also splits values on commas,
    // so lists are joined back into the original text.
    function readGroup(file, group, keys) {
        const settings = iniGroup.createObject(root, { location: file, category: group });
        settings.sync(); // picks up edits made since the file was last read
        const values = {};
        for (const key of keys) {
            const value = settings.value(key, null);
            if (value === null || value === undefined) {
                values[key] = null;
            } else {
                values[key] = typeof value === "string" ? value : Array.from(value).join(",");
            }
        }
        settings.destroy();
        return values;
    }

    function loadRules() {
        const order = readGroup(rulesFile, "", ["Order"]).Order;
        if (!order) {
            return [];
        }
        return order.split(",")
            .map(id => Object.assign({ id: id }, readGroup(rulesFile, id, ruleKeys)))
            .filter(rule => rule.Enabled !== "false");
    }

    // Mirrors how KWin matches rule text (window class, role, title, tag) against a window.
    function textMatches(mode, pattern, value) {
        pattern = pattern || "";
        value = value || "";
        switch (Number(mode || 0)) {
        case exactMatch:
            return value === pattern;
        case substringMatch:
            return value.includes(pattern);
        case regexpMatch:
            try {
                return new RegExp(pattern).test(value);
            } catch (error) {
                return false;
            }
        default:
            return true;
        }
    }

    // The rule stores window types as bit flags, one bit per type; no value means every type.
    // KWin counts windows without a type as normal windows.
    function typeMatches(rule, window) {
        if (rule.types === null) {
            return true;
        }
        const type = window.windowType < 0 ? 0 : window.windowType;
        return (Number(rule.types) & (1 << type)) !== 0;
    }

    // The client machine matcher is not checked: on Wayland every window runs on this machine.
    function ruleMatches(rule, window) {
        const windowClass = rule.wmclasscomplete === "true"
            ? window.resourceName + " " + window.resourceClass
            : window.resourceClass;
        const parentMatches = Number(rule.hastransientparentmatch || 0) !== exactMatch
            || !!window.transientFor === (rule.hastransientparent === "true");
        return typeMatches(rule, window)
            && textMatches(rule.wmclassmatch, rule.wmclass, windowClass)
            && textMatches(rule.windowrolematch, rule.windowrole, window.windowRole)
            && textMatches(rule.titlematch, rule.title, window.captionNormal)
            && textMatches(rule.tagmatch, rule.tag, window.tag)
            && parentMatches;
    }

    function forcesGeometry(rule) {
        return forcingRuleTypes.includes(Number(rule.positionrule))
            || forcingRuleTypes.includes(Number(rule.sizerule));
    }

    // Only main application windows take part; child dialogs are never put in tiles.
    function isCandidate(window) {
        return window.normalWindow && !window.transient;
    }

    function taggedRuleFor(window, rules) {
        const matching = rules.filter(rule => ruleMatches(rule, window));
        const tagged = matching.find(rule => (rule.Description || "").includes(marker));
        if (!tagged || matching.some(forcesGeometry)) {
            return null;
        }
        return tagged;
    }

    // A monitor's own identity survives plugging it into another port; the port name is only a
    // fallback for monitors that report no serial number.
    function screenKey(screen) {
        if (screen.serialNumber) {
            return [screen.manufacturer, screen.model, screen.serialNumber].join("|");
        }
        return "port:" + screen.name;
    }

    // The shape of a layout: split directions and how many tiles each split has, ignoring sizes,
    // so dragging a split border doesn't make remembered tiles invalid.
    function layoutShape(tile) {
        const children = Array.from(tile.tiles);
        if (children.length === 0) {
            return "*";
        }
        const direction = ["floating", "horizontal", "vertical"][tile.layoutDirection] || "unknown";
        return direction + "(" + children.map(layoutShape).join(",") + ")";
    }

    // A tile's position in its layout, as child indexes from the top, e.g. "1" or "0/2".
    function pathOf(tile) {
        const steps = [];
        for (; tile.parent; tile = tile.parent) {
            steps.unshift(Array.from(tile.parent.tiles).indexOf(tile));
        }
        return steps.join("/");
    }

    function tileAt(rootTile, path) {
        let tile = rootTile;
        for (const step of path === "" ? [] : path.split("/")) {
            tile = Array.from(tile.tiles)[Number(step)];
            if (!tile) {
                return null;
            }
        }
        return tile;
    }

    // Describes where a tile is, in terms that stay valid across sessions. Quick tiles are not
    // part of any custom layout, so they have no place.
    function placeOf(tile) {
        let rootTile = tile;
        while (rootTile.parent) {
            rootTile = rootTile.parent;
        }
        for (const screen of Workspace.screens) {
            for (const desktop of Workspace.desktops) {
                if (Workspace.rootTile(screen, desktop) === rootTile) {
                    return {
                        screen: screenKey(screen),
                        desktop: desktop.id,
                        tile: pathOf(tile),
                        layout: layoutShape(rootTile),
                    };
                }
            }
        }
        return null;
    }

    function loadMemory() {
        memoryStore.sync(); // picks up hand edits of the state file
        try {
            return JSON.parse(memoryStore.value("places", "{}"));
        } catch (error) {
            return {};
        }
    }

    function storePlace(ruleId, place) {
        const memory = loadMemory();
        if (place) {
            memory[ruleId] = place;
        } else {
            delete memory[ruleId];
        }
        memoryStore.setValue("places", JSON.stringify(memory));
        memoryStore.sync();
    }

    function restore(window, rule) {
        const place = loadMemory()[rule.id];
        if (!place) {
            return;
        }
        const screen = Array.from(Workspace.screens).find(s => screenKey(s) === place.screen);
        const desktop = Array.from(Workspace.desktops).find(d => d.id === place.desktop);
        if (!screen || !desktop) {
            return;
        }
        const onThatDesktop = window.onAllDesktops
            || Array.from(window.desktops).some(d => d.id === desktop.id);
        if (!onThatDesktop) {
            return;
        }
        const rootTile = Workspace.rootTile(screen, desktop);
        if (!rootTile || layoutShape(rootTile) !== place.layout) {
            return;
        }
        const tile = tileAt(rootTile, place.tile);
        if (tile && tile.tiles.length === 0 && tile.manage(window)) {
            console.info("tile-memory: put", window.resourceClass, "into tile", place.tile,
                         "on desktop", desktop.name);
        }
    }

    // The tagged rule of each open window, by window id. It is rebuilt whenever KWin reloads its
    // settings, which saving window rules does, so tagging a rule also applies to open windows.
    property var ruleIds: new Map()

    function windowKey(window) {
        return String(window.internalId);
    }

    function assignRules() {
        const rules = loadRules();
        ruleIds.clear();
        for (const window of Array.from(Workspace.windows)) {
            const rule = isCandidate(window) ? taggedRuleFor(window, rules) : null;
            if (rule) {
                ruleIds.set(windowKey(window), rule.id);
            }
        }
    }

    // Connected once for every main window; the window's rule is looked up when something happens.
    function watch(window) {
        const key = windowKey(window);
        // Taking a window out of its custom tile (into no tile or a quick tile) forgets the tile.
        window.tileChanged.connect(() => {
            const ruleId = ruleIds.get(key);
            if (ruleId && !window.deleted) {
                storePlace(ruleId, window.tile ? placeOf(window.tile) : null);
            }
        });
        // Editing the layout can change a tile's position or the layout's shape without the
        // window changing tiles, so the place is recorded again when the window closes. Closing
        // an untiled window keeps the memory, since other windows of the same rule may own it.
        window.closed.connect(() => {
            const ruleId = ruleIds.get(key);
            const place = window.tile ? placeOf(window.tile) : null;
            if (ruleId && place) {
                storePlace(ruleId, place);
            }
            ruleIds.delete(key);
        });
    }

    Connections {
        target: Workspace

        function onWindowAdded(window) {
            if (!root.isCandidate(window)) {
                return;
            }
            root.watch(window);
            const rule = root.taggedRuleFor(window, root.loadRules());
            if (rule) {
                root.ruleIds.set(root.windowKey(window), rule.id);
                root.restore(window, rule);
            }
        }
    }

    Connections {
        target: Options

        function onConfigChanged() {
            root.assignRules();
        }
    }

    // Windows already open when the script starts are tracked but left where they are.
    Component.onCompleted: {
        for (const window of Array.from(Workspace.windows)) {
            if (isCandidate(window)) {
                watch(window);
            }
        }
        assignRules();
    }
}
